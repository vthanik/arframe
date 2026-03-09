# ──────────────────────────────────────────────────────────────────────────────
# render-common.R — Shared backend utilities for rendering
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# 0. Span Level Count
# ══════════════════════════════════════════════════════════════════════════════

#' Count the number of unique spanner levels
#'
#' Each unique level produces one header row above the column header row.
#' Multiple spans at the same level share a single row.
#'
#' @param spans List of fr_span objects.
#' @return Integer — number of spanner rows (0 if no spans).
#' @noRd
n_spanner_levels <- function(spans) {

  if (length(spans) == 0L) return(0L)
  length(unique(vapply(spans, function(s) s$level, integer(1))))
}


# ══════════════════════════════════════════════════════════════════════════════
# 1. Cell Grid Builder
#
# Creates a flat data frame with one row per visible body cell, annotated
# with resolved styles and borders. Backends iterate this grid to emit
# format-specific output.
# ══════════════════════════════════════════════════════════════════════════════

#' Build a cell grid for the body region
#'
#' @param data Data frame (the body data, possibly a page_by subset).
#' @param columns Named list of fr_col objects (visible only).
#' @param cell_styles List of fr_cell_style objects.
#' @param page fr_page object for defaults.
#' @return A data frame with columns: row_idx, col_idx, col_name, content,
#'   align, bold, italic, underline, fg, bg, indent, font_size.
#' @noRd
build_cell_grid <- function(data, columns, cell_styles, page) {
  col_names <- names(columns)
  nr <- nrow(data)
  nc <- length(col_names)

  if (nr == 0L || nc == 0L) {
    return(vctrs::new_data_frame(list(
      row_idx = integer(0), col_idx = integer(0), col_name = character(0),
      content = character(0), align = character(0), valign = character(0),
      bold = logical(0), italic = logical(0), underline = logical(0),
      fg = character(0), bg = character(0),
      indent = numeric(0), font_size = numeric(0)
    )))
  }

  # Base grid: expand all row x col combinations
  grid <- vctrs::new_data_frame(list(
    row_idx = rep(seq_len(nr), times = nc),
    col_idx = rep(seq_len(nc), each = nr)
  ))
  grid$col_name <- col_names[grid$col_idx]

  # Fill content — column-wise vectorization (C-level lapply + unlist)
  grid$content <- unlist(lapply(col_names, function(nm) {
    x <- as.character(data[[nm]])
    x[is.na(x)] <- ""
    x
  }), use.names = FALSE)

  # Evaluate inline markup ({fr_super()}, {fr_bold()}, etc.) in cell content
  grid$content <- eval_markup_vec(grid$content)

  # Default properties from column spec
  grid$align <- vapply(grid$col_idx, function(j) {
    columns[[j]]$align %||% "left"
  }, character(1))
  grid$bold      <- FALSE
  grid$italic    <- FALSE
  grid$underline <- FALSE
  grid$fg        <- "#000000"
  grid$bg        <- NA_character_
  grid$valign    <- "top"
  grid$indent    <- 0
  grid$font_size <- page$font_size

  # Apply cell_styles in order (later wins) — body + stub regions
  for (style in cell_styles) {
    if (style$region != "body" && style$region != "stub") next
    affected <- resolve_style_mask(style, grid, col_names)
    if (!any(affected)) next

    if (!is.null(style$bold))      grid$bold[affected]      <- style$bold
    if (!is.null(style$italic))    grid$italic[affected]    <- style$italic
    if (!is.null(style$underline)) grid$underline[affected] <- style$underline
    if (!is.null(style$fg))        grid$fg[affected]        <- style$fg
    if (!is.null(style$bg))        grid$bg[affected]        <- style$bg
    if (!is.null(style$indent))    grid$indent[affected]    <- style$indent
    if (!is.null(style$font_size)) grid$font_size[affected] <- style$font_size
    if (!is.null(style$align))     grid$align[affected]     <- style$align
    if (!is.null(style$valign))    grid$valign[affected]    <- style$valign
  }

  grid
}


#' Resolve which cells a style targets
#'
#' @param style An fr_cell_style object.
#' @param grid The cell grid data frame.
#' @param col_names Character vector of visible column names.
#' @return Logical vector (length = nrow(grid)).
#' @noRd
resolve_style_mask <- function(style, grid, col_names) {
  nr_data <- max(grid$row_idx)
  # Row mask
  if (is.null(style$rows) || identical(style$rows, "all")) {
    row_mask <- rep(TRUE, nrow(grid))
  } else {
    row_mask <- grid$row_idx %in% style$rows
  }

  # Column mask
  if (style$type == "row" || is.null(style$cols) || identical(style$cols, "all")) {
    col_mask <- rep(TRUE, nrow(grid))
  } else if (is.character(style$cols)) {
    col_mask <- grid$col_name %in% style$cols
  } else if (is.numeric(style$cols)) {
    col_mask <- grid$col_idx %in% style$cols
  } else {
    col_mask <- rep(TRUE, nrow(grid))
  }

  # Stub region targets first column only
  if (identical(style$region, "stub")) {
    col_mask <- grid$col_idx == 1L
  }

  row_mask & col_mask
}


# ── Shared: apply cell_style overrides to a grid ────────────────────────────
#' Apply cell_style properties to a grid
#' @param grid A data frame with col_idx, col_name, and style columns.
#' @param cell_styles List of fr_cell_style objects.
#' @param region Character. Region to match ("body", "header", "stub").
#' @param col_names Character vector of visible column names.
#' @param header_row_idx Integer or NULL. For header grids, the row index.
#' @noRd
apply_styles_to_grid <- function(grid, cell_styles, region, col_names,
                                  header_row_idx = NULL) {
  for (style in cell_styles) {
    if (style$region != region) next

    # Row mask
    if (!is.null(header_row_idx)) {
      # Header: match against header row index
      if (!is.null(style$rows) && !identical(style$rows, "all")) {
        if (!(header_row_idx %in% style$rows)) next
      }
      col_mask <- if (is.null(style$cols) || identical(style$cols, "all") ||
                      identical(style$type, "row")) {
        rep(TRUE, nrow(grid))
      } else if (is.character(style$cols)) {
        grid$col_name %in% style$cols
      } else if (is.numeric(style$cols)) {
        grid$col_idx %in% style$cols
      } else {
        rep(TRUE, nrow(grid))
      }
      if (!any(col_mask)) next
      affected <- col_mask
    } else {
      # Body: use resolve_style_mask
      affected <- resolve_style_mask(style, grid, col_names)
      if (!any(affected)) next
    }

    if (!is.null(style$bold))      grid$bold[affected]      <- style$bold
    if (!is.null(style$italic))    grid$italic[affected]    <- style$italic
    if (!is.null(style$underline)) grid$underline[affected] <- style$underline
    if (!is.null(style$fg))        grid$fg[affected]        <- style$fg
    if (!is.null(style$bg))        grid$bg[affected]        <- style$bg
    if (!is.null(style$font_size)) grid$font_size[affected] <- style$font_size
    if (!is.null(style$align))     grid$align[affected]     <- style$align
    if (!is.null(style$indent) && "indent" %in% names(grid))
      grid$indent[affected] <- style$indent
  }
  grid
}


# ══════════════════════════════════════════════════════════════════════════════
# 1b. Header Cell Grid Builder
#
# Like build_cell_grid() but for the column header row. Applies header-
# region styles from spec$cell_styles so users can override default bold,
# colors, etc. via fr_style(region = "header", ...).
# ══════════════════════════════════════════════════════════════════════════════

#' Build a cell grid for the column header row
#'
#' @param columns Named list of fr_col objects (visible only).
#' @param cell_styles List of fr_cell_style objects.
#' @param page fr_page object for defaults.
#' @param header_row_idx Integer. 1-based header row index (after spanners).
#' @param header_cfg fr_header object with header-level defaults.
#' @return A data frame with one row per header cell.
#' @noRd
build_header_cell_grid <- function(columns, cell_styles, page, header_row_idx,
                                   default_valign = "bottom",
                                   header_cfg = NULL) {
  col_names <- names(columns)
  nc <- length(col_names)

  # Use header_cfg for defaults when available
  default_bold <- if (!is.null(header_cfg$bold)) header_cfg$bold else FALSE
  default_fg   <- if (!is.null(header_cfg$fg))   header_cfg$fg   else "#000000"
  default_bg   <- if (!is.null(header_cfg$bg))   header_cfg$bg   else NA_character_
  default_fs   <- if (!is.null(header_cfg$font_size)) header_cfg$font_size else page$font_size

  grid <- vctrs::new_data_frame(list(
    col_idx   = seq_len(nc),
    col_name  = col_names,
    align     = vapply(columns, function(c) c$header_align %||% c$align %||% "left", character(1)),
    valign    = rep(default_valign, nc),
    bold      = rep(default_bold, nc),
    italic    = rep(FALSE, nc),
    underline = rep(FALSE, nc),
    fg        = rep(default_fg, nc),
    bg        = rep(default_bg, nc),
    font_size = rep(default_fs, nc)
  ))

  for (style in cell_styles) {
    if (style$region != "header") next

    # Row mask: match against header row index
    if (!is.null(style$rows) && !identical(style$rows, "all")) {
      if (!(header_row_idx %in% style$rows)) next
    }

    # Column mask
    if (is.null(style$cols) || identical(style$cols, "all") ||
        identical(style$type, "row")) {
      col_mask <- rep(TRUE, nc)
    } else if (is.character(style$cols)) {
      col_mask <- grid$col_name %in% style$cols
    } else if (is.numeric(style$cols)) {
      col_mask <- grid$col_idx %in% style$cols
    } else {
      col_mask <- rep(TRUE, nc)
    }
    if (!any(col_mask)) next

    if (!is.null(style$bold))      grid$bold[col_mask]      <- style$bold
    if (!is.null(style$italic))    grid$italic[col_mask]    <- style$italic
    if (!is.null(style$underline)) grid$underline[col_mask] <- style$underline
    if (!is.null(style$fg))        grid$fg[col_mask]        <- style$fg
    if (!is.null(style$bg))        grid$bg[col_mask]        <- style$bg
    if (!is.null(style$font_size)) grid$font_size[col_mask] <- style$font_size
    if (!is.null(style$align))     grid$align[col_mask]     <- style$align
    if (!is.null(style$valign))    grid$valign[col_mask]    <- style$valign
  }

  grid
}


# ══════════════════════════════════════════════════════════════════════════════
# 1c. Row Height Extraction
#
# Extracts per-row height overrides from fr_row_style objects in cell_styles.
# ══════════════════════════════════════════════════════════════════════════════

#' Build row height vector from cell_styles
#'
#' @param nr Integer. Number of body rows.
#' @param cell_styles List of fr_cell_style objects.
#' @return Numeric vector of length nr (NA = auto height).
#' @noRd
build_row_heights <- function(nr, cell_styles) {
  heights <- rep(NA_real_, nr)

  for (style in cell_styles) {
    if (style$type != "row" || is.null(style$height)) next
    if (is.null(style$rows) || identical(style$rows, "all")) {
      heights[] <- style$height
    } else {
      idx <- style$rows[style$rows <= nr]
      heights[idx] <- style$height
    }
  }

  heights
}


# ══════════════════════════════════════════════════════════════════════════════
# 1d. Decimal Tab Position Computation
#
# For decimal-aligned columns, compute the optimal RTF \tqdec tab stop
# position by scanning actual cell content. The decimal point (or right
# edge for integers) is centered at the tab stop.
# ══════════════════════════════════════════════════════════════════════════════

#' Split a string at the decimal alignment point
#'
#' Splits at the first `.`, falling back to the first ` `, falling back to
#' the first `-` (not at position 1, to preserve leading negatives), falling
#' back to treating the whole string as the "before" part. Used by RTF
#' tab-stop computation, LaTeX makebox computation, and LaTeX body row
#' rendering.
#'
#' @param string A trimmed, non-empty character string.
#' @return A list with `before` and `after` (both character scalars).
#' @noRd
split_at_decimal <- function(string) {
  dot_pos <- regexpr(".", string, fixed = TRUE)
  sp_pos  <- regexpr(" ", string, fixed = TRUE)

  # When both dot and space exist and dot comes AFTER space, the dot is inside
  # a parenthetical (e.g., "28 (62.2%)") — prefer space as the split point
  if (dot_pos > 0L && sp_pos > 0L && dot_pos > sp_pos) {
    return(list(
      before = substr(string, 1L, sp_pos - 1L),
      after  = substr(string, sp_pos, nchar(string))
    ))
  }
  # Dot before space (or no space): standard decimal split
  if (dot_pos > 0L) {
    return(list(
      before = substr(string, 1L, dot_pos - 1L),
      after  = substr(string, dot_pos, nchar(string))
    ))
  }
  # Space only
  if (sp_pos > 0L) {
    return(list(
      before = substr(string, 1L, sp_pos - 1L),
      after  = substr(string, sp_pos, nchar(string))
    ))
  }
  # Dash (not leading) — for ranges like "41-82"
  if (nchar(string) > 1L) {
    dash_pos <- regexpr("-", substring(string, 2L), fixed = TRUE)
    if (dash_pos > 0L) {
      pos <- dash_pos + 1L
      return(list(
        before = substr(string, 1L, pos - 1L),
        after  = substr(string, pos, nchar(string))
      ))
    }
  }
  list(before = string, after = "")
}


#' Compute max before-decimal width in twips per column
#'
#' For each column with `align = "decimal"`, scan all cell values to find
#' the widest "before decimal" part using AFM font metrics. Used by the RTF
#' backend (sub-cell split) and LaTeX backend (`\\makebox` alignment).
#'
#' @param data Data frame of body values.
#' @param columns List of fr_col objects (visible columns only).
#' @param cell_grid The built cell grid (has align per cell).
#' @param font_family Font family name.
#' @param font_size Font size in points.
#' @return Named integer vector (one per column, NA = not decimal).
#' @noRd
compute_decimal_before_twips <- function(data, columns, cell_grid, font_family, font_size) {
  col_names <- names(columns)
  nc <- length(col_names)

  result <- rep(NA_integer_, nc)
  names(result) <- col_names

  for (j in seq_len(nc)) {
    col_mask <- cell_grid$col_idx == j
    if (!any(cell_grid$align[col_mask] == "decimal")) next

    vals <- data[[col_names[j]]]
    if (is.null(vals) || length(vals) == 0L) next

    vals_str <- as.character(vals)
    vals_str[is.na(vals_str)] <- ""
    vals_str <- unique(trimws(vals_str))
    vals_str <- vals_str[nzchar(vals_str)]

    max_before_twips <- 0
    if (length(vals_str) > 0L) {
      befores <- vapply(vals_str, function(v) split_at_decimal(v)$before,
                         character(1), USE.NAMES = FALSE)
      max_before_twips <- max(measure_text_width_twips(
        befores, font_family, font_size
      ))
    }

    result[j] <- as.integer(max_before_twips)
  }

  result
}


#' Compute max before-decimal width in points for LaTeX decimal alignment
#'
#' Delegates to [compute_decimal_before_twips()] and converts to points
#' (twips / 20). Used by the LaTeX backend to position `\makebox` alignment.
#'
#' @inheritParams compute_decimal_before_twips
#' @return Named numeric vector (one per column, NA = not decimal), in points.
#' @noRd
compute_decimal_before_pt <- function(data, columns, cell_grid, font_family, font_size) {
  compute_decimal_before_twips(data, columns, cell_grid, font_family, font_size) / 20
}


# ══════════════════════════════════════════════════════════════════════════════
# 1e. Blank-After Row Insertion
#
# Inserts empty rows into the data frame at group boundaries defined by
# the blank_after column(s). Called during finalize_spec() so all backends
# benefit.
# ══════════════════════════════════════════════════════════════════════════════

#' Insert blank rows after group boundaries
#'
#' @param data Data frame.
#' @param blank_cols Character vector of column names from spec$body$blank_after.
#' @return A list with `data` (data frame with blanks inserted) and
#'   `insert_positions` (integer vector of original row indices after which
#'   blanks were inserted — i.e., the boundary rows in the ORIGINAL data).
#' @noRd
insert_blank_after <- function(data, blank_cols) {
  empty <- list(data = data, insert_positions = integer(0))
  if (nrow(data) <= 1L || length(blank_cols) == 0L) return(empty)

  blank_cols <- intersect(blank_cols, names(data))
  if (length(blank_cols) == 0L) return(empty)

  keys <- inject(paste(!!!data[blank_cols], sep = "\x1f"))

  # Find rows where the next row has a different key (group boundary)
  boundaries <- which(keys[-length(keys)] != keys[-1L])
  if (length(boundaries) == 0L) return(empty)

  # Build a blank row template (all empty strings)
  blank_row <- vctrs::vec_init(data, 1L)
  blank_row[1L, ] <- ""

  # Build result by interleaving data chunks and blank rows
  result <- vector("list", 2L * length(boundaries) + 1L)
  prev <- 1L
  for (k in seq_along(boundaries)) {
    result[[2L * k - 1L]] <- vctrs::vec_slice(data, prev:boundaries[k])
    result[[2L * k]] <- blank_row
    prev <- boundaries[k] + 1L
  }
  result[[2L * length(boundaries) + 1L]] <- vctrs::vec_slice(data, prev:nrow(data))

  list(data = vctrs::vec_rbind(!!!result), insert_positions = boundaries)
}


#' Remap style row indices after blank row insertion
#'
#' When `insert_blank_after()` inserts blank rows, numeric row indices in
#' `cell_styles` become stale. This function shifts each style's row indices
#' to account for the inserted blanks.
#'
#' @param cell_styles List of fr_cell_style objects.
#' @param insert_positions Integer vector — original row indices after which
#'   blanks were inserted (the boundary rows from `insert_blank_after()`).
#' @return Modified list of fr_cell_style objects with shifted row indices.
#' @noRd
remap_style_indices <- function(cell_styles, insert_positions) {
  for (i in seq_along(cell_styles)) {
    rows <- cell_styles[[i]]$rows
    if (is.null(rows) || identical(rows, "all") || !is.numeric(rows)) next
    cell_styles[[i]]$rows <- vapply(rows, function(r) {
      as.integer(r + sum(insert_positions < r))
    }, integer(1), USE.NAMES = FALSE)
  }
  cell_styles
}


# ══════════════════════════════════════════════════════════════════════════════
# 1e. Indent-By Style Injection
#
# Translates fr_rows(indent_by = ..., group_by = ...) into cell_styles
# so that detail rows get a left indent in the rendered output.
# ══════════════════════════════════════════════════════════════════════════════

#' Apply indent_by as cell styles
#'
#' When `group_by` is set, only non-group-header rows are indented.
#' When `indent_by` is used alone, all rows are indented.
#'
#' The indent width is exactly 2 character widths in the current page font,
#' computed via AFM font metrics for consistent rendering across RTF and PDF.
#'
#' @param spec An fr_spec object (pre-finalization).
#' @return Modified fr_spec with additional cell_styles.
#' @noRd
apply_indent_by <- function(spec) {
  indent_cols <- spec$body$indent_by
  if (length(indent_cols) == 0L) return(spec)

  # Only apply to columns that exist in data
  indent_cols <- intersect(indent_cols, names(spec$data))
  if (length(indent_cols) == 0L) return(spec)

  nr <- nrow(spec$data)
  if (nr == 0L) return(spec)

  # Calculate indent: 2 space-character widths in the current page font
  indent_twips <- measure_text_width_twips(
    "  ", spec$page$font_family, spec$page$font_size
  )
  indent_inches <- twips_to_inches(indent_twips)

  group_cols <- spec$body$group_by
  if (length(group_cols) > 0L) {
    # With group_by + indent_by: indent only detail (non-header, non-blank)
    # rows. No auto-bold — users control bold via fr_styles(fr_row_style(...)).
    # Note: this runs AFTER insert_blank_after(), so blank rows may be present.
    group_cols <- intersect(group_cols, names(spec$data))
    if (length(group_cols) > 0L) {
      # Identify blank rows (all cells empty — inserted by blank_after)
      is_blank <- rowSums(spec$data != "") == 0L

      # Identify group header rows: first non-blank row of each group
      keys <- inject(paste(!!!spec$data[group_cols], sep = "\x1f"))
      is_header <- c(TRUE, keys[-length(keys)] != keys[-1L])

      # Detail rows: not a header and not blank
      detail_rows <- which(!is_header & !is_blank)

      if (length(detail_rows) > 0L) {
        indent_style <- new_fr_cell_style(
          region = "body", type = "col",
          rows = detail_rows, cols = indent_cols,
          indent = indent_inches
        )
        spec$cell_styles <- c(spec$cell_styles, list(indent_style))
      }
    }
  } else {
    # Without group_by: indent all rows in indent_by columns
    indent_style <- new_fr_cell_style(
      region = "body", type = "col",
      rows = "all", cols = indent_cols,
      indent = indent_inches
    )
    spec$cell_styles <- c(spec$cell_styles, list(indent_style))
  }

  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# 1c. Keep-Together Mask
#
# Builds a logical vector indicating which rows need \trkeep (RTF) or
# equivalent keep-with-next property. All rows in a group_by group
# get TRUE so Word/renderer keeps them on the same page.
# ══════════════════════════════════════════════════════════════════════════════

#' Build keep-together mask for body rows
#'
#' Marks rows that should stay with the next row (keep-with-next) to prevent
#' page breaks within `group_by` groups. Uses orphan/widow minimums for
#' intelligent splitting of large groups.
#'
#' **Behavior**:
#' - Small groups (`<= orphan_min + widow_min` non-blank rows): all rows glued
#'   together — no page split within the group.
#' - Large groups: header + first `orphan_min - 1` rows glued (prevents
#'   orphaned header at page bottom); last `widow_min` rows glued (prevents
#'   widowed tail on next page). The middle is free to split.
#'
#' @param data Data frame (body data for this section, post-blank-insertion).
#' @param keep_cols Character vector of column names from spec$body$group_by.
#' @param orphan_min Integer. Minimum rows to keep at the bottom of a page
#'   when a group must split. Default 3.
#' @param widow_min Integer. Minimum rows to carry to the next page when a
#'   group must split. Default 3.
#' @return Logical vector of length nrow(data), TRUE = keep with next row.
#' @noRd
build_keep_mask <- function(data, keep_cols, orphan_min = 3L, widow_min = 3L) {
  nr <- nrow(data)
  if (nr <= 1L || length(keep_cols) == 0L) return(rep(FALSE, nr))

  # Columns may not be visible but must exist in data
  keep_cols <- intersect(keep_cols, names(data))
  if (length(keep_cols) == 0L) return(rep(FALSE, nr))

  # Skip blank rows (inserted by blank_after)
  is_blank <- rowSums(data != "") == 0L

  # Build group key per row
  keys <- inject(paste(!!!data[keep_cols], sep = "\x1f"))

  mask <- rep(FALSE, nr)

  # Identify group header rows: first non-blank row where key changes
  is_header <- !is_blank & c(TRUE, keys[-1L] != keys[-length(keys)])
  header_positions <- which(is_header)

  for (idx in seq_along(header_positions)) {
    h <- header_positions[idx]
    # Find group end (last row before next header or end of data)
    if (idx < length(header_positions)) {
      group_end <- header_positions[idx + 1L] - 1L
    } else {
      group_end <- nr
    }
    # Skip trailing blanks
    while (group_end > h && is_blank[group_end]) group_end <- group_end - 1L

    # Non-blank rows in this group
    non_blank <- which(!is_blank[h:group_end]) + h - 1L
    group_size <- length(non_blank)

    if (group_size <= 1L) next

    if (group_size <= orphan_min + widow_min) {
      # Small group: keep entirely together (mark all but last for keep-with-next)
      mask[non_blank[-length(non_blank)]] <- TRUE
    } else {
      # Large group: enforce orphan_min at top, widow_min at bottom
      # Top: header + first (orphan_min - 1) children stay together
      top_keep <- non_blank[seq_len(orphan_min - 1L)]
      mask[top_keep] <- TRUE
      # Bottom: last (widow_min) rows stay together
      bottom_keep <- non_blank[(group_size - widow_min + 1L):(group_size - 1L)]
      mask[bottom_keep] <- TRUE
    }
  }

  mask
}


# ══════════════════════════════════════════════════════════════════════════════
# 2. Border Resolution
#
# Converts fr_rule objects into per-cell border annotations.
# Returns a list of border matrices (top, bottom, left, right) where each
# entry is either NULL (no border) or a list(width, linestyle, fg).
# ══════════════════════════════════════════════════════════════════════════════

#' Resolve rules into per-cell border structures
#'
#' @param rules List of fr_rule / fr_rule_box / fr_vline_spec objects.
#' @param nrow_body Integer. Number of body rows.
#' @param ncol Integer. Number of visible columns.
#' @param nrow_header Integer. Number of header rows (including spanners).
#' @return A list with `header` and `body` sublists, each containing
#'   `top`, `bottom`, `left`, `right` matrices.
#' @noRd
resolve_borders <- function(rules, nrow_body, ncol, nrow_header = 1L) {
  # Initialize border matrices: each cell gets NULL or a border spec
  # Header borders
  h_top    <- matrix(list(NULL), nrow = nrow_header, ncol = ncol)
  h_bottom <- matrix(list(NULL), nrow = nrow_header, ncol = ncol)
  h_left   <- matrix(list(NULL), nrow = nrow_header, ncol = ncol)
  h_right  <- matrix(list(NULL), nrow = nrow_header, ncol = ncol)

  # Body borders
  b_top    <- matrix(list(NULL), nrow = max(1L, nrow_body), ncol = ncol)
  b_bottom <- matrix(list(NULL), nrow = max(1L, nrow_body), ncol = ncol)
  b_left   <- matrix(list(NULL), nrow = max(1L, nrow_body), ncol = ncol)
  b_right  <- matrix(list(NULL), nrow = max(1L, nrow_body), ncol = ncol)

  border_spec <- function(width, linestyle, fg) {
    list(width = width, linestyle = linestyle, fg = fg)
  }

  for (rule in rules) {
    if (inherits(rule, "fr_rule_box")) {
      # Box: all four outer sides
      bs <- border_spec(fr_env$rtf_box_border_wd, "solid", "#000000")
      # Top of header
      for (j in seq_len(ncol)) h_top[1L, j] <- list(bs)
      # Bottom of last body row
      if (nrow_body > 0L) {
        for (j in seq_len(ncol)) b_bottom[nrow_body, j] <- list(bs)
      }
      # Left and right edges
      for (i in seq_len(nrow_header)) {
        h_left[i, 1L]    <- list(bs)
        h_right[i, ncol] <- list(bs)
      }
      for (i in seq_len(max(1L, nrow_body))) {
        b_left[i, 1L]    <- list(bs)
        b_right[i, ncol] <- list(bs)
      }
      next
    }

    if (inherits(rule, "fr_vline_spec")) {
      bs <- border_spec(rule$width, rule$linestyle, rule$fg)
      preset <- rule$preset

      # Determine which column gaps get rules
      if (!is.null(rule$cols)) {
        gaps <- rule$cols
      } else if (preset == "all") {
        gaps <- seq(0L, ncol)
      } else if (preset == "inner") {
        gaps <- seq_len(ncol - 1L)
      } else if (preset == "box") {
        gaps <- c(0L, ncol)
      } else {
        next
      }

      for (g in gaps) {
        if (g == 0L) {
          # Left edge
          for (i in seq_len(nrow_header)) h_left[i, 1L] <- list(bs)
          for (i in seq_len(max(1L, nrow_body))) b_left[i, 1L] <- list(bs)
        } else if (g == ncol) {
          # Right edge
          for (i in seq_len(nrow_header)) h_right[i, ncol] <- list(bs)
          for (i in seq_len(max(1L, nrow_body))) b_right[i, ncol] <- list(bs)
        } else {
          # Between columns g and g+1
          for (i in seq_len(nrow_header)) {
            h_right[i, g]      <- list(bs)
            h_left[i, g + 1L]  <- list(bs)
          }
          for (i in seq_len(max(1L, nrow_body))) {
            b_right[i, g]      <- list(bs)
            b_left[i, g + 1L]  <- list(bs)
          }
        }
      }
      next
    }

    if (!inherits(rule, "fr_rule")) next
    if (rule$direction != "horizontal") next

    bs <- border_spec(rule$width, rule$linestyle, rule$fg)

    if (rule$region == "header") {
      if (rule$side == "above") {
        for (j in seq_len(ncol)) h_top[1L, j] <- list(bs)
      } else {
        # below header = bottom of last header row
        for (j in seq_len(ncol)) h_bottom[nrow_header, j] <- list(bs)
      }
    } else if (rule$region == "body") {
      if (rule$side == "below") {
        if (nrow_body > 0L) {
          if (is.null(rule$rows)) {
            # Below last body row
            for (j in seq_len(ncol)) b_bottom[nrow_body, j] <- list(bs)
          } else if (identical(rule$rows, "all")) {
            for (i in seq_len(nrow_body)) {
              for (j in seq_len(ncol)) b_bottom[i, j] <- list(bs)
            }
          } else {
            for (i in rule$rows) {
              if (i <= nrow_body) {
                for (j in seq_len(ncol)) b_bottom[i, j] <- list(bs)
              }
            }
          }
        }
      } else {
        # above body = top of first body row
        if (nrow_body > 0L) {
          for (j in seq_len(ncol)) b_top[1L, j] <- list(bs)
        }
      }
    }
  }

  list(
    header = list(top = h_top, bottom = h_bottom, left = h_left, right = h_right),
    body   = list(top = b_top, bottom = b_bottom, left = b_left, right = b_right)
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# 3. Sentinel Resolver Factories
# ══════════════════════════════════════════════════════════════════════════════

#' Resolve a sentinel token to RTF markup
#'
#' Formatting types (BOLD, ITALIC, etc.) recursively escape their content
#' via `rtf_escape_and_resolve()` so that non-ASCII characters (em dash,
#' dagger, etc.) and nested sentinels are properly converted to RTF escape
#' codes. Without this, raw UTF-8 bytes would leak into the ANSI RTF file.
#' @noRd
rtf_sentinel_resolver <- function(type, content) {
  switch(toupper(type),
    "SUPER"     = paste0("{\\super ", rtf_escape_and_resolve(content), "}"),
    "SUB"       = paste0("{\\sub ", rtf_escape_and_resolve(content), "}"),
    "BOLD"      = paste0("{\\b ", rtf_escape_and_resolve(content), "}"),
    "ITALIC"    = paste0("{\\i ", rtf_escape_and_resolve(content), "}"),
    "UNDERLINE" = paste0("{\\ul ", rtf_escape_and_resolve(content), "}"),
    "NEWLINE"   = "\\line ",
    "UNICODE"   = rtf_encode_unicode_char(content),
    content
  )
}


#' Resolve a sentinel token to LaTeX markup
#'
#' Formatting types recursively escape their content via
#' `latex_escape_and_resolve()` so that LaTeX specials (`%`, `&`, `#`,
#' etc.) and nested sentinels are properly handled.
#' @noRd
latex_sentinel_resolver <- function(type, content) {
  switch(toupper(type),
    "SUPER"     = paste0("\\textsuperscript{", latex_escape_and_resolve(content), "}"),
    "SUB"       = paste0("\\textsubscript{", latex_escape_and_resolve(content), "}"),
    "BOLD"      = paste0("\\textbf{", latex_escape_and_resolve(content), "}"),
    "ITALIC"    = paste0("\\textit{", latex_escape_and_resolve(content), "}"),
    "UNDERLINE" = paste0("\\underline{", latex_escape_and_resolve(content), "}"),
    "NEWLINE"   = "\\\\",
    "UNICODE"   = latex_encode_unicode_char(content),
    content
  )
}


#' Encode a single Unicode character for LaTeX
#' @noRd
latex_encode_unicode_char <- function(char) {
  # Check known LaTeX map first
  mapped <- fr_env$latex_unicode[char]
  if (!is.na(mapped)) return(mapped)
  # XeLaTeX handles Unicode natively — pass through

  char
}


#' Escape text for LaTeX output
#'
#' Escapes LaTeX special characters and converts known Unicode
#' characters to LaTeX commands.
#'
#' @param text Character vector.
#' @return Character vector with LaTeX-safe text.
#' @noRd
latex_escape <- function(text) {
  if (length(text) == 0L) return(character(0))
  # Order matters: backslash first
  specials <- fr_env$latex_specials
  for (i in seq_along(specials)) {
    text <- stringi::stri_replace_all_fixed(text, names(specials)[i],
                                             specials[i])
  }

  # Convert known Unicode characters to LaTeX commands
  vapply(text, function(t) {
    chars <- strsplit(t, "")[[1L]]
    if (length(chars) == 0L) return("")
    result <- vapply(chars, function(ch) {
      cp <- utf8ToInt(ch)
      if (length(cp) == 1L && cp > 127L) {
        mapped <- fr_env$latex_unicode[ch]
        if (!is.na(mapped)) return(mapped)
      }
      ch
    }, character(1), USE.NAMES = FALSE)
    paste0(result, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}


#' Escape AND resolve sentinels for LaTeX
#'
#' Splits text around sentinel markers, escapes non-sentinel parts,
#' then resolves sentinels to LaTeX commands.
#'
#' @param text Character scalar.
#' @return Character scalar with LaTeX-safe text and resolved sentinels.
#' @noRd
latex_escape_and_resolve <- function(text) {
  if (!has_sentinel(text)) return(latex_escape(text))

  pattern <- fr_env$sentinel_pattern
  m <- gregexpr(pattern, text, perl = TRUE)
  sentinels <- regmatches(text, m)[[1L]]
  non_sentinels <- regmatches(text, m, invert = TRUE)[[1L]]

  parts <- character(0)
  for (i in seq_along(non_sentinels)) {
    if (nzchar(non_sentinels[i])) {
      parts <- c(parts, latex_escape(non_sentinels[i]))
    }
    if (i <= length(sentinels)) {
      tok_parts <- regmatches(sentinels[i],
                              regexec(pattern, sentinels[i], perl = TRUE))[[1L]]
      resolved <- latex_sentinel_resolver(tok_parts[[2L]], tok_parts[[3L]])
      parts <- c(parts, resolved)
    }
  }

  paste0(parts, collapse = "")
}


#' Encode a single Unicode character as RTF
#' @noRd
rtf_encode_unicode_char <- function(char) {
  # Check known RTF shorthand map first
  shorthand <- fr_env$rtf_unicode[char]
  if (!is.na(shorthand)) return(shorthand)

  # General: \uN? where N = signed 16-bit decimal codepoint
  cp <- utf8ToInt(char)
  if (length(cp) == 0L) return("")
  # RTF uses signed 16-bit; codepoints > 32767 need negative representation
  if (cp > 32767L) cp <- cp - 65536L
  paste0("\\u", cp, "?")
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. Text Escaping
# ══════════════════════════════════════════════════════════════════════════════

#' Escape text for RTF output
#'
#' Escapes RTF special characters (\\, {, }) and converts non-ASCII
#' characters to RTF Unicode escapes.
#'
#' @param text Character vector.
#' @return Character vector with RTF-safe text.
#' @noRd
rtf_escape <- function(text) {
  if (length(text) == 0L) return(character(0))
  # Must escape backslash first, then braces
  text <- stringi::stri_replace_all_fixed(text, "\\", "\\\\")
  text <- stringi::stri_replace_all_fixed(text, "{", "\\{")
  text <- stringi::stri_replace_all_fixed(text, "}", "\\}")

  # Convert non-ASCII characters to \uN?
  vapply(text, function(t) {
    chars <- strsplit(t, "")[[1L]]
    if (length(chars) == 0L) return("")
    result <- vapply(chars, function(ch) {
      cp <- utf8ToInt(ch)
      if (length(cp) == 1L && cp > 127L) {
        rtf_encode_unicode_char(ch)
      } else {
        ch
      }
    }, character(1), USE.NAMES = FALSE)
    paste0(result, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}


#' Escape AND resolve sentinels for RTF
#'
#' Splits text around sentinel markers, escapes non-sentinel parts,
#' then resolves sentinels to RTF markup.
#'
#' @param text Character scalar.
#' @return Character scalar with RTF-safe text and resolved sentinels.
#' @noRd
rtf_escape_and_resolve <- function(text) {
  if (!has_sentinel(text)) return(rtf_escape(text))

  # Split around sentinels: find all sentinel tokens

  pattern <- fr_env$sentinel_pattern
  # Use regmatches to extract sentinels and non-sentinel parts
  m <- gregexpr(pattern, text, perl = TRUE)
  sentinels <- regmatches(text, m)[[1L]]
  non_sentinels <- regmatches(text, m, invert = TRUE)[[1L]]

  # Escape non-sentinel parts, resolve sentinel parts
  parts <- character(0)
  for (i in seq_along(non_sentinels)) {
    if (nzchar(non_sentinels[i])) {
      parts <- c(parts, rtf_escape(non_sentinels[i]))
    }
    if (i <= length(sentinels)) {
      # Parse and resolve the sentinel
      tok_parts <- regmatches(sentinels[i],
                              regexec(pattern, sentinels[i], perl = TRUE))[[1L]]
      resolved <- rtf_sentinel_resolver(tok_parts[[2L]], tok_parts[[3L]])
      parts <- c(parts, resolved)
    }
  }

  paste0(parts, collapse = "")
}


# ══════════════════════════════════════════════════════════════════════════════
# 5. Color Table Builder
# ══════════════════════════════════════════════════════════════════════════════

#' Collect all unique colors from rules and cell_styles
#'
#' @param spec An fr_spec object.
#' @return Character vector of unique hex color strings.
#' @noRd
collect_colors <- function(spec) {
  colors <- "#000000"  # Always include black

  # From rules
  for (rule in spec$rules) {
    if (!is.null(rule$fg)) colors <- c(colors, rule$fg)
  }

  # From header styling
  if (!is.null(spec$header$bg)) colors <- c(colors, spec$header$bg)
  if (!is.null(spec$header$fg)) colors <- c(colors, spec$header$fg)

  # From cell_styles
  for (style in spec$cell_styles) {
    if (!is.null(style$fg)) colors <- c(colors, style$fg)
    if (!is.null(style$bg)) colors <- c(colors, style$bg)
  }

  unique(colors)
}


#' Build RTF color table string and return index map
#'
#' @param colors Character vector of hex color strings.
#' @return A list with `rtf` (the {\colortbl...} string) and `index`
#'   (named integer vector mapping hex -> 1-based color index).
#' @noRd
build_rtf_colortbl <- function(colors) {
  colors <- unique(colors)
  # RTF colortbl: index 0 = auto (;), then each color
  entries <- vapply(colors, function(hex) {
    rgb <- hex_to_rgb(hex)
    paste0("\\red", rgb[["r"]], "\\green", rgb[["g"]], "\\blue", rgb[["b"]], ";")
  }, character(1), USE.NAMES = FALSE)

  rtf <- paste0("{\\colortbl;", paste0(entries, collapse = ""), "}")

  # Index map: RTF color indices are 1-based (0 = auto)
  index <- seq_along(colors)
  names(index) <- colors

  list(rtf = rtf, index = index)
}
