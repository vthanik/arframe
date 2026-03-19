# ──────────────────────────────────────────────────────────────────────────────
# render-common.R — Shared backend utilities for rendering
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# 0. Shared Micro-Helpers
# ══════════════════════════════════════════════════════════════════════════════

#' Build group keys from data columns
#' @param data Data frame.
#' @param cols Character vector of column names.
#' @return Character vector of length nrow(data), one composite key per row.
#' @noRd
build_group_keys <- function(data, cols) {
  inject(paste(!!!data[cols], sep = fr_env$group_sep))
}


#' Detect blank rows (all cells empty string)
#' @param data Data frame.
#' @return Logical vector of length nrow(data).
#' @noRd
detect_blank_rows <- function(data) {
  rowSums(data != "") == 0L
}


# ══════════════════════════════════════════════════════════════════════════════
# 0a. Span Level Count
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
  if (length(spans) == 0L) {
    return(0L)
  }
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
      row_idx = integer(0),
      col_idx = integer(0),
      col_name = character(0),
      content = character(0),
      align = character(0),
      valign = character(0),
      bold = logical(0),
      italic = logical(0),
      underline = logical(0),
      fg = character(0),
      bg = character(0),
      indent = numeric(0),
      font_size = numeric(0)
    )))
  }

  # Base grid: expand all row x col combinations
  grid <- vctrs::new_data_frame(list(
    row_idx = rep(seq_len(nr), times = nc),
    col_idx = rep(seq_len(nc), each = nr)
  ))
  grid$col_name <- col_names[grid$col_idx]

  # Fill content — column-wise vectorization (C-level lapply + unlist)
  grid$content <- unlist(
    lapply(col_names, function(nm) {
      x <- as.character(data[[nm]])
      x[is.na(x)] <- ""
      x
    }),
    use.names = FALSE
  )

  # Evaluate inline markup ({fr_super()}, {fr_bold()}, etc.) in cell content
  grid$content <- eval_markup_vec(grid$content)

  # Default properties from column spec — vectorize by column, then broadcast
  col_aligns <- vapply(columns, function(c) c$align %||% "left", character(1))
  grid$align <- col_aligns[grid$col_idx]
  grid$bold <- FALSE
  grid$italic <- FALSE
  grid$underline <- FALSE
  grid$fg <- "#000000"
  grid$bg <- NA_character_
  grid$valign <- "top"
  grid$indent <- 0
  grid$font_size <- page$font_size

  # Apply cell_styles in order (later wins) — body + stub regions
  for (style in cell_styles) {
    if (style$region != "body" && style$region != "stub") {
      next
    }
    affected <- resolve_style_mask(style, grid, col_names)
    if (!any(affected)) {
      next
    }

    if (!is.null(style$bold)) {
      grid$bold[affected] <- style$bold
    }
    if (!is.null(style$italic)) {
      grid$italic[affected] <- style$italic
    }
    if (!is.null(style$underline)) {
      grid$underline[affected] <- style$underline
    }
    if (!is.null(style$fg)) {
      grid$fg[affected] <- style$fg
    }
    if (!is.null(style$bg)) {
      grid$bg[affected] <- style$bg
    }
    if (!is.null(style$indent)) {
      grid$indent[affected] <- style$indent
    }
    if (!is.null(style$font_size)) {
      grid$font_size[affected] <- style$font_size
    }
    if (!is.null(style$align)) {
      grid$align[affected] <- style$align
    }
    if (!is.null(style$valign)) grid$valign[affected] <- style$valign
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
  if (
    style$type == "row" || is.null(style$cols) || identical(style$cols, "all")
  ) {
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
apply_styles_to_grid <- function(
  grid,
  cell_styles,
  region,
  col_names,
  header_row_idx = NULL
) {
  for (style in cell_styles) {
    if (style$region != region) {
      next
    }

    # Row mask
    if (!is.null(header_row_idx)) {
      # Header: match against header row index
      if (!is.null(style$rows) && !identical(style$rows, "all")) {
        if (!(header_row_idx %in% style$rows)) next
      }
      col_mask <- if (
        is.null(style$cols) ||
          identical(style$cols, "all") ||
          identical(style$type, "row")
      ) {
        rep(TRUE, nrow(grid))
      } else if (is.character(style$cols)) {
        grid$col_name %in% style$cols
      } else if (is.numeric(style$cols)) {
        grid$col_idx %in% style$cols
      } else {
        rep(TRUE, nrow(grid))
      }
      if (!any(col_mask)) {
        next
      }
      affected <- col_mask
    } else {
      # Body: use resolve_style_mask
      affected <- resolve_style_mask(style, grid, col_names)
      if (!any(affected)) next
    }

    if (!is.null(style$bold)) {
      grid$bold[affected] <- style$bold
    }
    if (!is.null(style$italic)) {
      grid$italic[affected] <- style$italic
    }
    if (!is.null(style$underline)) {
      grid$underline[affected] <- style$underline
    }
    if (!is.null(style$fg)) {
      grid$fg[affected] <- style$fg
    }
    if (!is.null(style$bg)) {
      grid$bg[affected] <- style$bg
    }
    if (!is.null(style$font_size)) {
      grid$font_size[affected] <- style$font_size
    }
    if (!is.null(style$align)) {
      grid$align[affected] <- style$align
    }
    if (!is.null(style$valign) && "valign" %in% names(grid)) {
      grid$valign[affected] <- style$valign
    }
    if (!is.null(style$indent) && "indent" %in% names(grid)) {
      grid$indent[affected] <- style$indent
    }
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
build_header_cell_grid <- function(
  columns,
  cell_styles,
  page,
  header_row_idx,
  default_valign = "bottom",
  header_cfg = NULL
) {
  col_names <- names(columns)
  nc <- length(col_names)

  # Use header_cfg for defaults when available
  default_bold <- if (!is.null(header_cfg$bold)) header_cfg$bold else FALSE
  default_fg <- if (!is.null(header_cfg$fg)) header_cfg$fg else "#000000"
  default_bg <- if (!is.null(header_cfg$bg)) header_cfg$bg else NA_character_
  default_fs <- if (!is.null(header_cfg$font_size)) {
    header_cfg$font_size
  } else {
    page$font_size
  }

  grid <- vctrs::new_data_frame(list(
    col_idx = seq_len(nc),
    col_name = col_names,
    align = vapply(
      columns,
      function(c) c$header_align %||% c$align %||% "left",
      character(1)
    ),
    valign = rep(default_valign, nc),
    bold = rep(default_bold, nc),
    italic = rep(FALSE, nc),
    underline = rep(FALSE, nc),
    fg = rep(default_fg, nc),
    bg = rep(default_bg, nc),
    font_size = rep(default_fs, nc)
  ))

  grid <- apply_styles_to_grid(
    grid,
    cell_styles,
    region = "header",
    col_names = col_names,
    header_row_idx = header_row_idx
  )

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
    if (style$type != "row" || is.null(style$height)) {
      next
    }
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
# 1d. Group Header Row Injection
#
# When group_label is set, inserts a header row at each group boundary.
# The group value appears in the target column; all other columns are empty.
# Returns updated data + row indices of injected headers (for auto-bold).
# ══════════════════════════════════════════════════════════════════════════════

#' Inject group header rows into the data frame
#'
#' For each unique group (defined by `group_cols`), inserts a new row at the
#' start of the group where the `label_col` receives the group value and all
#' other columns are empty. Returns the expanded data and the row indices of
#' the injected header rows (1-based in the new data frame).
#'
#' @param data Data frame.
#' @param group_cols Character vector of column names (from `group_by`).
#' @param label_col Character scalar — column to receive the group value.
#' @return List with `data` (expanded) and `header_rows` (integer vector).
#' @noRd
inject_group_headers <- function(data, group_cols, label_col) {
  nr <- nrow(data)
  if (nr == 0L || length(group_cols) == 0L || is.null(label_col)) {
    return(list(data = data, header_rows = integer(0)))
  }

  keys <- build_group_keys(data, group_cols)
  # Find first row of each group
  boundaries <- which(c(TRUE, keys[-length(keys)] != keys[-1L]))

  if (length(boundaries) == 0L) {
    return(list(data = data, header_rows = integer(0)))
  }

  # Build empty header row template (matches insert_blank_after pattern)
  hdr_template <- vctrs::vec_init(data, 1L)
  hdr_template[1L, ] <- ""

  sep <- fr_env$group_label_sep

  # Chunk-based interleaving: header row before each group, then the group data
  n_boundaries <- length(boundaries)
  ends <- c(boundaries[-1L] - 1L, nr)
  result <- vector("list", 2L * n_boundaries)
  header_positions <- integer(n_boundaries)
  cumulative_rows <- 0L

  for (k in seq_len(n_boundaries)) {
    # Build header row with group label
    hdr <- hdr_template
    row_idx <- boundaries[k]
    if (length(group_cols) == 1L) {
      hdr[[label_col]] <- as.character(data[[group_cols]][row_idx])
    } else {
      hdr[[label_col]] <- paste(
        vapply(
          group_cols,
          function(gc) as.character(data[[gc]][row_idx]),
          character(1)
        ),
        collapse = sep
      )
    }
    # Preserve group_cols values so blank_after/keepn still work
    for (gc in group_cols) {
      hdr[[gc]] <- as.character(data[[gc]][row_idx])
    }

    result[[2L * k - 1L]] <- hdr
    cumulative_rows <- cumulative_rows + 1L
    header_positions[k] <- cumulative_rows

    chunk <- vctrs::vec_slice(data, boundaries[k]:ends[k])
    result[[2L * k]] <- chunk
    cumulative_rows <- cumulative_rows + nrow(chunk)
  }

  list(
    data = vctrs::vec_rbind(!!!result),
    header_rows = header_positions
  )
}


#' Remap style row indices after group header injection
#'
#' When `inject_group_headers()` inserts header rows, existing numeric row
#' indices in `cell_styles` become stale. Each original row i shifts down by
#' the number of header rows injected at or before position i.
#'
#' @param cell_styles List of fr_cell_style objects.
#' @param header_rows Integer vector — row positions of injected headers
#'   in the NEW data frame.
#' @param new_nrow Integer — total rows in the new data frame.
#' @return Modified list of fr_cell_style objects with shifted row indices.
#' @noRd
remap_style_indices_for_injected <- function(cell_styles, header_rows) {
  n_headers <- length(header_rows)
  if (n_headers == 0L) {
    return(cell_styles)
  }

  sorted_headers <- sort(header_rows)
  # The j-th header is at new position sorted_headers[j], so the original
  # boundary it was inserted before = sorted_headers[j] - j + 1
  orig_boundaries <- sorted_headers - seq_along(sorted_headers) + 1L

  for (i in seq_along(cell_styles)) {
    rows <- cell_styles[[i]]$rows
    if (is.null(rows) || identical(rows, "all") || !is.numeric(rows)) {
      next
    }
    # Original row k -> k + number of headers whose boundary <= k
    cell_styles[[i]]$rows <- as.integer(
      rows + findInterval(rows, orig_boundaries)
    )
  }
  cell_styles
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
  if (nrow(data) <= 1L || length(blank_cols) == 0L) {
    return(empty)
  }

  blank_cols <- intersect(blank_cols, names(data))
  if (length(blank_cols) == 0L) {
    return(empty)
  }

  keys <- build_group_keys(data, blank_cols)

  # Find rows where the next row has a different key (group boundary)
  boundaries <- which(keys[-length(keys)] != keys[-1L])
  if (length(boundaries) == 0L) {
    return(empty)
  }

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
  result[[2L * length(boundaries) + 1L]] <- vctrs::vec_slice(
    data,
    prev:nrow(data)
  )

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
  sorted_pos <- sort(insert_positions)
  for (i in seq_along(cell_styles)) {
    rows <- cell_styles[[i]]$rows
    if (is.null(rows) || identical(rows, "all") || !is.numeric(rows)) {
      next
    }
    cell_styles[[i]]$rows <- as.integer(
      rows + findInterval(rows - 1L, sorted_pos)
    )
  }
  cell_styles
}


# ══════════════════════════════════════════════════════════════════════════════
# 1f. Indent-By Style Injection
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
  indent_spec <- spec$body$indent_by
  if (length(indent_spec) == 0L) {
    return(spec)
  }

  nr <- nrow(spec$data)
  if (nr == 0L) {
    return(spec)
  }

  # Calculate base indent: 2 space-character widths in the current page font
  indent_twips <- measure_text_width_twips(
    "  ",
    spec$page$font_family,
    spec$page$font_size
  )
  base_indent <- twips_to_inches(indent_twips)

  # Multi-level form: list(key = "row_type", col = "term", levels = c(...))
  if (is.list(indent_spec)) {
    return(apply_indent_by_levels(spec, indent_spec, base_indent))
  }

  # Simple form: character vector of column names
  indent_cols <- intersect(indent_spec, names(spec$data))
  if (length(indent_cols) == 0L) {
    return(spec)
  }

  group_cols <- spec$body$group_by
  if (length(group_cols) > 0L) {
    # With group_by + indent_by: indent only detail (non-header, non-blank)
    # rows. No auto-bold — users control bold via fr_styles(fr_row_style(...)).
    # Note: this runs AFTER insert_blank_after(), so blank rows may be present.
    group_cols <- intersect(group_cols, names(spec$data))
    if (length(group_cols) > 0L) {
      # Identify blank rows (all cells empty — inserted by blank_after)
      is_blank <- detect_blank_rows(spec$data)

      # Identify group header rows: first non-blank row of each group
      keys <- build_group_keys(spec$data, group_cols)
      is_header <- c(TRUE, keys[-length(keys)] != keys[-1L])

      # Detail rows: not a header and not blank
      detail_rows <- which(!is_header & !is_blank)

      if (length(detail_rows) > 0L) {
        indent_style <- new_fr_cell_style(
          region = "body",
          type = "col",
          rows = detail_rows,
          cols = indent_cols,
          indent = base_indent
        )
        spec$cell_styles <- c(spec$cell_styles, list(indent_style))
      }
    }
  } else {
    # Without group_by: indent all rows in indent_by columns
    indent_style <- new_fr_cell_style(
      region = "body",
      type = "col",
      rows = "all",
      cols = indent_cols,
      indent = base_indent
    )
    spec$cell_styles <- c(spec$cell_styles, list(indent_style))
  }

  spec
}


#' Apply multi-level indent from a named list spec
#'
#' Each unique indent level (from `indent_spec$levels`) creates one cell_style
#' targeting the rows whose key column matches that level's name.
#'
#' @param spec An fr_spec object.
#' @param indent_spec List with `key`, `col`, `levels`.
#' @param base_indent Numeric. One indent unit in inches.
#' @return Modified fr_spec.
#' @noRd
apply_indent_by_levels <- function(spec, indent_spec, base_indent) {
  key_col <- indent_spec$key
  target_cols <- intersect(indent_spec$col, names(spec$data))
  levels_map <- indent_spec$levels

  if (length(target_cols) == 0L || !key_col %in% names(spec$data)) {
    return(spec)
  }

  key_values <- as.character(spec$data[[key_col]])
  is_blank <- detect_blank_rows(spec$data)

  # Group rows by indent level, create one cell_style per level
  for (level_name in names(levels_map)) {
    level_mult <- levels_map[[level_name]]
    if (level_mult == 0) {
      next # No indent for level 0
    }
    matched_rows <- which(key_values == level_name & !is_blank)
    if (length(matched_rows) > 0L) {
      indent_style <- new_fr_cell_style(
        region = "body",
        type = "col",
        rows = matched_rows,
        cols = target_cols,
        indent = base_indent * level_mult
      )
      spec$cell_styles <- c(spec$cell_styles, list(indent_style))
    }
  }

  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# 1g. Leading-Space → Paragraph Indent
#
# Detects leading spaces in cell data, measures their width using AFM font
# metrics, strips the spaces, and creates cell_styles with the corresponding
# indent. Both RTF (\li) and LaTeX (\leftskip) consume grid$indent for
# paragraph-level indent that applies to ALL lines including wrapped text.
# ══════════════════════════════════════════════════════════════════════════════

#' Convert leading spaces to paragraph-level indent
#'
#' For columns with `spaces = "indent"` (the default), detects leading spaces
#' in each cell, measures their width via AFM font metrics, strips them from
#' the data, and injects cell_styles with the equivalent indent in inches.
#'
#' Skips decimal-aligned columns (own spacing engine) and hidden columns.
#'
#' @param spec An fr_spec object (post-blank-row insertion, pre-indent_by).
#' @return Modified fr_spec with stripped data and new cell_styles.
#' @noRd
apply_leading_indent <- function(spec) {
  default_spaces <- spec$columns_meta$spaces %||% "indent"
  nr <- nrow(spec$data)
  if (nr == 0L) {
    return(spec)
  }

  decimal_cols <- names(spec$decimal_geometry %||% list())

  # Pre-measure single space width (font is constant across columns)
  space_twips <- measure_text_width_twips(
    " ",
    spec$page$font_family,
    spec$page$font_size
  )

  for (nm in names(spec$columns)) {
    col <- spec$columns[[nm]]
    col_spaces <- col$spaces %||% default_spaces
    if (col_spaces != "indent") {
      next
    }
    if (nm %in% decimal_cols) {
      next
    }
    if (!nm %in% names(spec$data)) {
      next
    }
    if (isFALSE(col$visible)) {
      next
    }

    vals <- as.character(spec$data[[nm]])
    vals[is.na(vals)] <- ""
    # Cache stripped result to avoid double regex
    stripped <- sub("^ +", "", vals)
    n_lead <- nchar(vals) - nchar(stripped)
    has_lead <- n_lead > 0L

    if (!any(has_lead)) {
      next
    }

    # Strip leading spaces from data
    spec$data[[nm]][has_lead] <- stripped[has_lead]

    # Group rows by indent level, create one cell_style per level
    unique_levels <- sort(unique(n_lead[has_lead]))
    new_styles <- vector("list", length(unique_levels))
    for (k in seq_along(unique_levels)) {
      n <- unique_levels[k]
      rows <- which(n_lead == n)
      indent_inches <- twips_to_inches(space_twips * n)
      new_styles[[k]] <- new_fr_cell_style(
        region = "body",
        type = "cell",
        rows = rows,
        cols = nm,
        indent = indent_inches
      )
    }
    spec$cell_styles <- c(spec$cell_styles, new_styles)
  }
  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# 1h. Keep-Together Mask
#
# Builds a logical vector driving RTF-native \keepn + \trkeep pagination.
# Rows marked TRUE emit \keepn (paragraph-level) + \trkeep (row-level), which
# tells Word to keep each such row on the same page as the next row.
# Groups that fit on one page get a full keepn chain; only groups larger than
# the page use orphan/widow edge protection for the top and bottom rows.
# Disabled entirely when group_keep = FALSE (visual-only grouping).
# ══════════════════════════════════════════════════════════════════════════════

#' Build keep-together mask for body rows
#'
#' Marks rows that should stay with the next row (keep-with-next) to prevent
#' page breaks within `group_by` groups. The mask drives RTF `\keepn` +
#' `\trkeep` control words — Word keeps rows with `\keepn` on the same page
#' as the next row, so the entire chain of TRUE rows travels together.
#'
#' **Behavior**:
#' - Groups that fit on one page (`group_size <= page_rows`): all rows glued
#'   together via full keepn chain — no page split within the group.
#' - Groups larger than one page: header + first `orphan_min - 1` rows glued
#'   (prevents orphaned header at page bottom); last `widow_min` rows glued
#'   (prevents widowed tail on next page). The middle rows are free to split.
#'
#' @param data Data frame (body data for this section, post-blank-insertion).
#' @param keep_cols Character vector of column names from spec$body$group_by.
#' @param orphan_min Integer. Minimum rows to keep at the bottom of a page
#'   when a group must split. Default 3.
#' @param widow_min Integer. Minimum rows to carry to the next page when a
#'   group must split. Default 3.
#' @param page_rows Integer (or Inf). Estimated number of body rows that fit
#'   on one page, computed from `calculate_page_budget()` / `row_height_twips()`.
#'   Groups with `group_size <= page_rows` get a full keepn chain; only
#'   oversized groups use orphan/widow edge protection. Default `Inf` (always
#'   full keepn, i.e. pre-`page_rows` behavior).
#' @return Logical vector of length nrow(data), TRUE = keep with next row.
#' @noRd
build_keep_mask <- function(
  data,
  keep_cols,
  orphan_min = 3L,
  widow_min = 3L,
  page_rows = Inf
) {
  nr <- nrow(data)
  if (nr <= 1L || length(keep_cols) == 0L) {
    return(rep(FALSE, nr))
  }

  # Columns may not be visible but must exist in data
  keep_cols <- intersect(keep_cols, names(data))
  if (length(keep_cols) == 0L) {
    return(rep(FALSE, nr))
  }

  # Skip blank rows (inserted by blank_after)
  is_blank <- detect_blank_rows(data)

  # Build group key per row
  keys <- build_group_keys(data, keep_cols)

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
    while (group_end > h && is_blank[group_end]) {
      group_end <- group_end - 1L
    }

    # Non-blank rows in this group
    non_blank <- which(!is_blank[h:group_end]) + h - 1L
    group_size <- length(non_blank)

    if (group_size <= 1L) {
      next
    }

    if (group_size <= page_rows) {
      # Group fits on one page: keep entirely together
      mask[non_blank[-length(non_blank)]] <- TRUE
    } else {
      # Group larger than one page: orphan/widow edge protection only
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
  h_top <- matrix(list(NULL), nrow = nrow_header, ncol = ncol)
  h_bottom <- matrix(list(NULL), nrow = nrow_header, ncol = ncol)
  h_left <- matrix(list(NULL), nrow = nrow_header, ncol = ncol)
  h_right <- matrix(list(NULL), nrow = nrow_header, ncol = ncol)

  # Empty table: return header-only borders (no body rows to process)
  if (nrow_body == 0L) {
    empty_body <- list(
      top = matrix(list(NULL), nrow = 0L, ncol = ncol),
      bottom = matrix(list(NULL), nrow = 0L, ncol = ncol),
      left = matrix(list(NULL), nrow = 0L, ncol = ncol),
      right = matrix(list(NULL), nrow = 0L, ncol = ncol)
    )
    return(list(
      header = list(
        top = h_top,
        bottom = h_bottom,
        left = h_left,
        right = h_right
      ),
      body = empty_body
    ))
  }

  # Body borders
  b_top <- matrix(list(NULL), nrow = nrow_body, ncol = ncol)
  b_bottom <- matrix(list(NULL), nrow = nrow_body, ncol = ncol)
  b_left <- matrix(list(NULL), nrow = nrow_body, ncol = ncol)
  b_right <- matrix(list(NULL), nrow = nrow_body, ncol = ncol)

  border_spec <- function(width, linestyle, fg) {
    list(width = width, linestyle = linestyle, fg = fg)
  }

  for (rule in rules) {
    if (inherits(rule, "fr_rule_box")) {
      # Box: all four outer sides
      bs <- border_spec(fr_env$rtf_box_border_wd, "solid", "#000000")
      # Top of header
      h_top[1L, ] <- list(bs)
      # Bottom of last body row
      if (nrow_body > 0L) {
        b_bottom[nrow_body, ] <- list(bs)
      }
      # Left and right edges
      h_left[seq_len(nrow_header), 1L] <- list(bs)
      h_right[seq_len(nrow_header), ncol] <- list(bs)
      b_left[seq_len(nrow_body), 1L] <- list(bs)
      b_right[seq_len(nrow_body), ncol] <- list(bs)
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

      h_rows <- seq_len(nrow_header)
      b_rows <- seq_len(nrow_body)
      for (g in gaps) {
        if (g == 0L) {
          # Left edge
          h_left[h_rows, 1L] <- list(bs)
          b_left[b_rows, 1L] <- list(bs)
        } else if (g == ncol) {
          # Right edge
          h_right[h_rows, ncol] <- list(bs)
          b_right[b_rows, ncol] <- list(bs)
        } else {
          # Between columns g and g+1
          h_right[h_rows, g] <- list(bs)
          h_left[h_rows, g + 1L] <- list(bs)
          b_right[b_rows, g] <- list(bs)
          b_left[b_rows, g + 1L] <- list(bs)
        }
      }
      next
    }

    if (!inherits(rule, "fr_rule")) {
      next
    }
    if (rule$direction != "horizontal") {
      next
    }

    bs <- border_spec(rule$width, rule$linestyle, rule$fg)

    if (rule$region == "header") {
      if (rule$side == "above") {
        h_top[1L, ] <- list(bs)
      } else {
        # below header = bottom of last header row
        h_bottom[nrow_header, ] <- list(bs)
      }
    } else if (rule$region == "body") {
      if (rule$side == "below") {
        if (nrow_body > 0L) {
          if (is.null(rule$rows)) {
            # Below last body row
            b_bottom[nrow_body, ] <- list(bs)
          } else if (identical(rule$rows, "all")) {
            b_bottom[seq_len(nrow_body), ] <- list(bs)
          } else {
            valid_rows <- rule$rows[rule$rows <= nrow_body]
            if (length(valid_rows) > 0L) {
              b_bottom[valid_rows, ] <- list(bs)
            }
          }
        }
      } else {
        # above body = top of first body row
        if (nrow_body > 0L) {
          b_top[1L, ] <- list(bs)
        }
      }
    }
  }

  list(
    header = list(
      top = h_top,
      bottom = h_bottom,
      left = h_left,
      right = h_right
    ),
    body = list(top = b_top, bottom = b_bottom, left = b_left, right = b_right)
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
  switch(
    toupper(type),
    "SUPER" = paste0("{\\super ", rtf_escape_and_resolve(content), "}"),
    "SUB" = paste0("{\\sub ", rtf_escape_and_resolve(content), "}"),
    "BOLD" = paste0("{\\b ", rtf_escape_and_resolve(content), "}"),
    "ITALIC" = paste0("{\\i ", rtf_escape_and_resolve(content), "}"),
    "UNDERLINE" = paste0("{\\ul ", rtf_escape_and_resolve(content), "}"),
    "NEWLINE" = "\\line ",
    "UNICODE" = rtf_encode_unicode_char(content),
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
  switch(
    toupper(type),
    "SUPER" = paste0(
      "\\textsuperscript{",
      latex_escape_and_resolve(content),
      "}"
    ),
    "SUB" = paste0("\\textsubscript{", latex_escape_and_resolve(content), "}"),
    "BOLD" = paste0("\\textbf{", latex_escape_and_resolve(content), "}"),
    "ITALIC" = paste0("\\textit{", latex_escape_and_resolve(content), "}"),
    "UNDERLINE" = paste0(
      "\\underline{",
      latex_escape_and_resolve(content),
      "}"
    ),
    "NEWLINE" = "\\\\",
    "UNICODE" = latex_encode_unicode_char(content),
    content
  )
}


#' Encode a single Unicode character for LaTeX
#' @noRd
latex_encode_unicode_char <- function(char) {
  # Check known LaTeX map first
  mapped <- fr_env$latex_unicode[char]
  if (!is.na(mapped)) {
    return(mapped)
  }
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
  if (length(text) == 0L) {
    return(character(0))
  }
  # Order matters: backslash first
  specials <- fr_env$latex_specials
  text <- stringi::stri_replace_all_fixed(
    text,
    names(specials),
    specials,
    vectorize_all = FALSE
  )

  # Short-circuit: skip Unicode processing for all-ASCII elements
  has_non_ascii <- stringi::stri_detect_regex(text, "[^\\x00-\\x7F]")
  if (!any(has_non_ascii)) {
    return(text)
  }

  # Only process elements with non-ASCII characters
  non_ascii_idx <- which(has_non_ascii)
  non_ascii_text <- text[non_ascii_idx]

  # Extract unique non-ASCII chars, build replacement map from known map
  all_chars <- stringi::stri_extract_all_regex(non_ascii_text, "[^\\x00-\\x7F]")
  unique_chars <- unique(unlist(all_chars, use.names = FALSE))
  replacements <- fr_env$latex_unicode[unique_chars]

  # Only replace chars that have known LaTeX mappings
  known <- !is.na(replacements)
  if (any(known)) {
    text[non_ascii_idx] <- stringi::stri_replace_all_fixed(
      non_ascii_text,
      unique_chars[known],
      replacements[known],
      vectorize_all = FALSE
    )
  }
  text
}


#' Escape and resolve sentinels in text (generic)
#'
#' Splits text around sentinel markers, escapes non-sentinel parts via
#' `escape_fn`, then resolves sentinel tokens via `resolver_fn`. Both
#' `rtf_escape_and_resolve()` and `latex_escape_and_resolve()` delegate here.
#'
#' @param text Character scalar.
#' @param escape_fn Function to escape plain text portions.
#' @param resolver_fn Function to resolve sentinel tokens.
#' @return Character scalar with escaped text and resolved sentinels.
#' @noRd
escape_and_resolve <- function(text, escape_fn, resolver_fn) {
  if (!has_sentinel(text)) {
    return(escape_fn(text))
  }

  pattern <- fr_env$sentinel_pattern
  m <- gregexpr(pattern, text, perl = TRUE)
  sentinels <- regmatches(text, m)[[1L]]
  non_sentinels <- regmatches(text, m, invert = TRUE)[[1L]]

  parts <- character(0)
  for (i in seq_along(non_sentinels)) {
    if (nzchar(non_sentinels[i])) {
      parts <- c(parts, escape_fn(non_sentinels[i]))
    }
    if (i <= length(sentinels)) {
      tok_parts <- regmatches(
        sentinels[i],
        regexec(pattern, sentinels[i], perl = TRUE)
      )[[1L]]
      if (length(tok_parts) >= 3L) {
        resolved <- resolver_fn(tok_parts[[2L]], tok_parts[[3L]])
        parts <- c(parts, resolved)
      } else {
        # Malformed sentinel: pass through unresolved
        parts <- c(parts, escape_fn(sentinels[i]))
      }
    }
  }

  paste0(parts, collapse = "")
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
  escape_and_resolve(text, latex_escape, latex_sentinel_resolver)
}


#' Encode a single Unicode character as RTF
#' @noRd
rtf_encode_unicode_char <- function(char) {
  # Check known RTF shorthand map first
  shorthand <- fr_env$rtf_unicode[char]
  if (!is.na(shorthand)) {
    return(shorthand)
  }

  # General: \uN? where N = signed 16-bit decimal codepoint
  cp <- utf8ToInt(char)
  if (length(cp) == 0L) {
    return("")
  }
  # RTF uses signed 16-bit; codepoints > 32767 need negative representation
  if (cp > 32767L) {
    cp <- cp - 65536L
  }
  paste0("\\u", cp, "?")
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. Text Escaping
# ══════════════════════════════════════════════════════════════════════════════

#' Replace \\n with RTF \\line, preserving leading spaces
#'
#' In RTF, `\\line` is a control word whose first trailing space is consumed as
#' a delimiter. Any leading spaces on subsequent lines must be converted to
#' non-breaking spaces (`\\~`) to survive rendering.
#'
#' @param text Character vector (already RTF-escaped).
#' @return Character vector with `\\n` replaced by `\\line` + preserved spaces.
#' @noRd
newline_to_rtf_line <- function(text) {
  vapply(
    text,
    function(t) {
      if (!grepl("\n", t, fixed = TRUE)) {
        return(t)
      }
      parts <- strsplit(t, "\n", fixed = TRUE)[[1L]]
      # First part stays as-is; subsequent parts get leading spaces → \~
      for (i in seq_along(parts)[-1L]) {
        stripped <- sub("^ +", "", parts[[i]])
        n_spaces <- nchar(parts[[i]]) - nchar(stripped)
        if (n_spaces > 0L) {
          parts[[i]] <- paste0(strrep("\\~", n_spaces), stripped)
        }
      }
      paste0(parts, collapse = "\\line ")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Replace \\n with LaTeX \\\\, preserving leading spaces
#'
#' LaTeX collapses multiple spaces into one. Leading spaces on subsequent
#' lines are converted to `~` (non-breaking space) to preserve indentation.
#'
#' @param text Character vector (already LaTeX-escaped).
#' @return Character vector with `\\n` replaced by `\\\\` + preserved spaces.
#' @noRd
newline_to_latex_break <- function(text) {
  vapply(
    text,
    function(t) {
      if (!grepl("\n", t, fixed = TRUE)) {
        return(t)
      }
      parts <- strsplit(t, "\n", fixed = TRUE)[[1L]]
      for (i in seq_along(parts)[-1L]) {
        stripped <- sub("^ +", "", parts[[i]])
        n_spaces <- nchar(parts[[i]]) - nchar(stripped)
        if (n_spaces > 0L) {
          parts[[i]] <- paste0(strrep("~", n_spaces), stripped)
        }
      }
      paste0(parts, collapse = " \\\\ ")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Escape text for RTF output
#'
#' Escapes RTF special characters (\\, {, }) and converts non-ASCII
#' characters to RTF Unicode escapes.
#'
#' @param text Character vector.
#' @return Character vector with RTF-safe text.
#' @noRd
rtf_escape <- function(text) {
  if (length(text) == 0L) {
    return(character(0))
  }
  # Must escape backslash first, then braces
  text <- stringi::stri_replace_all_fixed(text, "\\", "\\\\")
  text <- stringi::stri_replace_all_fixed(text, "{", "\\{")
  text <- stringi::stri_replace_all_fixed(text, "}", "\\}")

  # Short-circuit: skip Unicode processing for all-ASCII elements
  has_non_ascii <- stringi::stri_detect_regex(text, "[^\\x00-\\x7F]")
  if (!any(has_non_ascii)) {
    return(text)
  }

  # Only process elements with non-ASCII characters
  non_ascii_idx <- which(has_non_ascii)
  non_ascii_text <- text[non_ascii_idx]

  # Extract unique non-ASCII chars, build replacement map
  all_chars <- stringi::stri_extract_all_regex(non_ascii_text, "[^\\x00-\\x7F]")
  unique_chars <- unique(unlist(all_chars, use.names = FALSE))
  replacements <- vapply(
    unique_chars,
    rtf_encode_unicode_char,
    character(1),
    USE.NAMES = FALSE
  )

  # Vectorized replace: one pass per unique non-ASCII char
  text[non_ascii_idx] <- stringi::stri_replace_all_fixed(
    non_ascii_text,
    unique_chars,
    replacements,
    vectorize_all = FALSE
  )
  text
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
  escape_and_resolve(text, rtf_escape, rtf_sentinel_resolver)
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
  colors <- "#000000" # Always include black

  # From rules
  for (rule in spec$rules) {
    if (!is.null(rule$fg)) colors <- c(colors, rule$fg)
  }

  # From header styling
  if (!is.null(spec$header$bg)) {
    colors <- c(colors, spec$header$bg)
  }
  if (!is.null(spec$header$fg)) {
    colors <- c(colors, spec$header$fg)
  }

  # From cell_styles
  for (style in spec$cell_styles) {
    if (!is.null(style$fg)) {
      colors <- c(colors, style$fg)
    }
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
  entries <- vapply(
    colors,
    function(hex) {
      rgb <- hex_to_rgb(hex)
      paste0(
        "\\red",
        rgb[["r"]],
        "\\green",
        rgb[["g"]],
        "\\blue",
        rgb[["b"]],
        ";"
      )
    },
    character(1),
    USE.NAMES = FALSE
  )

  rtf <- paste0("{\\colortbl;", paste0(entries, collapse = ""), "}")

  # Index map: RTF color indices are 1-based (0 = auto)
  index <- seq_along(colors)
  names(index) <- colors

  list(rtf = rtf, index = index)
}
