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
  rowSums(data != "" & !is.na(data)) == 0L
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

# ── Style Registry ───────────────────────────────────────────────────────────
# First-principles: most cells share 1–3 unique style combinations.
# Stores unique style records once; tracks which style applies to each cell via
# a single integer index. Style application = 1 write per cell instead of 9.
#
# new_style_registry()  — create empty registry
# register_style()      — intern a record, return its integer id (deduplicates)
# .materialize_style()  — expand style_ids back to per-property columns
# ─────────────────────────────────────────────────────────────────────────────

new_style_registry <- function() {
  env <- new.env(parent = emptyenv())
  env$records <- list()
  env$n <- 0L
  env$hash_to_id <- new.env(hash = TRUE, parent = emptyenv())
  # Pre-built column vectors: indexed by style ID for O(1) materialization
  env$v_align <- character(0)
  env$v_valign <- character(0)
  env$v_bold <- logical(0)
  env$v_italic <- logical(0)
  env$v_underline <- logical(0)
  env$v_color <- character(0)
  env$v_background <- character(0)
  env$v_indent <- numeric(0)
  env$v_font_size <- numeric(0)
  env
}

register_style <- function(reg, rec) {
  # Hash: concatenate all field values with a separator unlikely to appear
  key <- paste(
    rec$align, rec$valign, rec$bold, rec$italic, rec$underline,
    rec$color, rec$background %||% "NA", rec$indent, rec$font_size,
    sep = "\x1f"
  )
  existing <- reg$hash_to_id[[key]]
  if (!is.null(existing)) {
    return(existing)
  }
  id <- reg$n + 1L
  reg$n <- id
  reg$records[[id]] <- rec
  reg$hash_to_id[[key]] <- id
  # Append to column vectors for fast batch materialization
  reg$v_align[id] <- rec$align
  reg$v_valign[id] <- rec$valign
  reg$v_bold[id] <- rec$bold
  reg$v_italic[id] <- rec$italic
  reg$v_underline[id] <- rec$underline
  reg$v_color[id] <- rec$color
  reg$v_background[id] <- rec$background %||% NA_character_
  reg$v_indent[id] <- rec$indent
  reg$v_font_size[id] <- rec$font_size
  id
}

# ── Border Registry helpers (reuse new_style_registry pattern) ──────────────

.register_border <- function(reg, bs) {
  key <- paste(bs$width, bs$linestyle, bs$fg, sep = "\x1f")
  existing <- reg$hash_to_id[[key]]
  if (!is.null(existing)) return(existing)
  id <- reg$n + 1L
  reg$n <- id
  reg$records[[id]] <- bs
  reg$hash_to_id[[key]] <- id
  id
}

# Materialize an integer border matrix back to list(NULL)/list(spec) format.
.materialize_border_mat <- function(mat, reg) {
  result <- matrix(list(NULL), nrow = nrow(mat), ncol = ncol(mat))
  nonzero <- which(mat != 0L)
  if (length(nonzero) > 0L) {
    result[nonzero] <- reg$records[mat[nonzero]]
  }
  result
}

# Expand style_ids + registry back to one column per property.
# Uses pre-built column vectors (set in register_style) for vector indexing
# instead of 9 vapply calls — 9 vector subsets vs 9×n list element extractions.
.materialize_style <- function(style_ids, registry) {
  list(
    align = registry$v_align[style_ids],
    valign = registry$v_valign[style_ids],
    bold = registry$v_bold[style_ids],
    italic = registry$v_italic[style_ids],
    underline = registry$v_underline[style_ids],
    color = registry$v_color[style_ids],
    background = registry$v_background[style_ids],
    indent = registry$v_indent[style_ids],
    font_size = registry$v_font_size[style_ids]
  )
}


#' Build a cell grid for the body region
#'
#' @param data Data frame (the body data, possibly a page_by subset).
#' @param columns Named list of fr_col objects (visible only).
#' @param cell_styles List of fr_cell_style objects.
#' @param page fr_page object for defaults.
#' @return A data frame with columns: row_idx, col_idx, col_name, content,
#'   align, bold, italic, underline, color, background, indent, font_size.
#' @noRd
build_cell_grid <- function(data, columns, cell_styles, page) {
  col_names <- names(columns)
  nr <- vctrs::vec_size(data)
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
      color = character(0),
      background = character(0),
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
  # Stable row IDs for style targeting (survives row injection)
  row_ids_src <- data$.__row_id__ %||% paste0("r", seq_len(nr))
  grid$row_id <- rep(row_ids_src, times = nc)

  # Fill content — column-wise vectorization (C-level lapply + unlist)
  grid$content <- unlist(
    lapply(col_names, function(nm) {
      x <- as.character(data[[nm]])
      na_to_empty(x)
    }),
    use.names = FALSE
  )

  # Evaluate inline markup ({fr_super()}, {fr_bold()}, etc.) in cell content
  grid$content <- eval_markup_vec(grid$content)

  # ── Style registry: one integer index per cell instead of 9 property cols ──
  # Register one default style record per column (alignment differs per column).
  # All other defaults are page-level constants shared across columns.
  col_aligns <- vapply(columns, function(c) c$align %||% "left", character(1))
  fs <- page$font_size

  reg <- new_style_registry()

  # Pre-register one record per unique column alignment (usually 1-3 unique).
  # style_ids[k] = ID of the style record currently assigned to cell k.
  col_default_ids <- vapply(
    col_aligns,
    function(al) {
      register_style(reg, list(
        align = al, valign = "top", bold = FALSE, italic = FALSE,
        underline = FALSE, color = "#000000", background = NA_character_,
        indent = 0, font_size = fs
      ))
    },
    integer(1)
  )
  # Broadcast per-column defaults across all rows (column-major order)
  style_ids <- rep(col_default_ids, each = nr)

  # Apply cell_styles in order (later wins) — body + stub regions
  # Key: each style iteration = 1 vector write on style_ids (not 9 writes on
  # 9 separate property columns). Fewer copy-on-modify triggers = faster.
  for (style in cell_styles) {
    if (style$region != "body" && style$region != "stub") {
      next
    }
    affected <- which(resolve_style_mask(style, grid, col_names))
    if (length(affected) == 0L) {
      next
    }

    # Batch by current style id: cells with the same current style produce the
    # same merged record, so we register each merged record only once.
    curr_ids <- unique(style_ids[affected])
    for (cid in curr_ids) {
      cells <- affected[style_ids[affected] == cid]
      cur <- reg$records[[cid]]
      new_rec <- list(
        align = style$align %||% cur$align,
        valign = style$valign %||% cur$valign,
        bold = style$bold %||% cur$bold,
        italic = style$italic %||% cur$italic,
        underline = style$underline %||% cur$underline,
        color = style$color %||% cur$color,
        background = style$background %||% cur$background,
        indent = style$indent %||% cur$indent,
        font_size = style$font_size %||% cur$font_size
      )
      new_id <- register_style(reg, new_rec)
      style_ids[cells] <- new_id
    }
  }

  # Materialize: expand style_ids back to per-property columns for backends
  props <- .materialize_style(style_ids, reg)
  grid$align <- props$align
  grid$valign <- props$valign
  grid$bold <- props$bold
  grid$italic <- props$italic
  grid$underline <- props$underline
  grid$color <- props$color
  grid$background <- props$background
  grid$indent <- props$indent
  grid$font_size <- props$font_size

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
  # Row mask: prefer stable row_ids over integer row indices
  if (!is.null(style$row_ids) && length(style$row_ids) > 0L) {
    row_mask <- grid$row_id %in% style$row_ids
  } else if (is.null(style$rows) || identical(style$rows, "all")) {
    row_mask <- rep(TRUE, vctrs::vec_size(grid))
  } else {
    row_mask <- grid$row_idx %in% style$rows
  }

  # Column mask
  if (
    style$type == "row" || is.null(style$cols) || identical(style$cols, "all")
  ) {
    col_mask <- rep(TRUE, vctrs::vec_size(grid))
  } else if (is.character(style$cols)) {
    col_mask <- grid$col_name %in% style$cols
  } else if (is.numeric(style$cols)) {
    col_mask <- grid$col_idx %in% style$cols
  } else {
    cli::cli_warn(
      c(
        "Malformed {.arg cols} selector in style definition.",
        "x" = "{.arg cols} has unexpected type {.obj_type_friendly {style$cols}}.",
        "i" = "Expected {.cls character}, {.cls numeric}, {.val all}, or {.val NULL}.",
        "i" = "Falling back to targeting all columns."
      ),
      call = caller_env()
    )
    col_mask <- rep(TRUE, vctrs::vec_size(grid))
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
      # Header: row_ids don't apply to header grids (body-only concept).
      # Match against header row index only.
      if (!is.null(style$row_ids)) next
      if (!is.null(style$rows) && !identical(style$rows, "all")) {
        if (!(header_row_idx %in% style$rows)) next
      }
      col_mask <- if (
        is.null(style$cols) ||
          identical(style$cols, "all") ||
          identical(style$type, "row")
      ) {
        rep(TRUE, vctrs::vec_size(grid))
      } else if (is.character(style$cols)) {
        grid$col_name %in% style$cols
      } else if (is.numeric(style$cols)) {
        grid$col_idx %in% style$cols
      } else {
        rep(TRUE, vctrs::vec_size(grid))
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
    if (!is.null(style$color)) {
      grid$color[affected] <- style$color
    }
    if (!is.null(style$background)) {
      grid$background[affected] <- style$background
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
  default_fg <- if (!is.null(header_cfg$color)) header_cfg$color else "#000000"
  default_bg <- if (!is.null(header_cfg$background)) {
    header_cfg$background
  } else {
    NA_character_
  }
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
    color = rep(default_fg, nc),
    background = rep(default_bg, nc),
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
#' @param preserve_cols Character vector of additional column names whose
#'   values should be copied from the source row to the injected header row
#'   (e.g. `page_by` columns, so that `prepare_pages()` can split correctly).
#' @return List with `data` (expanded) and `header_rows` (integer vector).
#' @noRd
inject_group_headers <- function(
  data,
  group_cols,
  label_col,
  preserve_cols = NULL
) {
  nr <- vctrs::vec_size(data)
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

  # Columns to copy from source row: group_cols + preserve_cols (e.g. page_by)
  copy_cols <- unique(c(group_cols, intersect(preserve_cols, names(data))))

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
    # Preserve group_cols + page_by values so blank_after/keepn/page split work
    for (gc in copy_cols) {
      hdr[[gc]] <- as.character(data[[gc]][row_idx])
    }

    hdr$.__row_id__ <- paste0("gh_", k)
    result[[2L * k - 1L]] <- hdr
    cumulative_rows <- cumulative_rows + 1L
    header_positions[k] <- cumulative_rows

    chunk <- vctrs::vec_slice(data, boundaries[k]:ends[k])
    result[[2L * k]] <- chunk
    cumulative_rows <- cumulative_rows + vctrs::vec_size(chunk)
  }

  list(
    data = vctrs::vec_rbind(!!!result),
    header_rows = header_positions
  )
}


# remap_style_indices_for_injected() — DELETED
# Styles now reference stable row IDs (.__row_id__) stamped at fr_table() time.
# Integer index remapping is no longer needed after inject_group_headers().
# Row IDs survive injection: injected header rows get "gh_k" IDs, blank rows
# get "blank_k" IDs. resolve_style_mask() matches on grid$row_id directly.


# ══════════════════════════════════════════════════════════════════════════════
# 1d-ii. Group Header Styling Helpers
# ══════════════════════════════════════════════════════════════════════════════

#' Identify group header row positions in the finalized data
#'
#' Returns a list with `$all` (all header rows) and `$by_level` (named list
#' mapping level names to row positions). Works for both label-group and
#' leaf-hierarchy paths.
#'
#' @param spec The spec after header injection.
#' @param gl_header_rows Integer vector from `inject_group_headers()$header_rows`.
#'   May be `integer(0)` if no label-group injection occurred.
#' @return List with `all` (integer) and `by_level` (named list of integer).
#' @noRd
identify_group_header_rows <- function(spec, gl_header_rows) {
  all_ids <- character(0)
  by_level <- list()
  row_id_col <- spec$data$.__row_id__

  # Path 1: label groups — headers injected by inject_group_headers()
  if (length(gl_header_rows) > 0L) {
    all_ids <- row_id_col[gl_header_rows]
  }

  # Path 2: leaf hierarchies — identified by __row_level__ column
  leaf_col <- spec$body$group_leaf
  if (!is.null(leaf_col) && "__row_level__" %in% names(spec$data)) {
    row_levels <- spec$data[["__row_level__"]]
    non_leaf_mask <- row_levels != leaf_col
    all_ids <- row_id_col[non_leaf_mask]

    # Split by level for per-level targeting
    unique_levels <- unique(row_levels[non_leaf_mask])
    for (lvl in unique_levels) {
      by_level[[lvl]] <- row_id_col[row_levels == lvl]
    }
  }

  list(all = all_ids, by_level = by_level)
}


#' Build fr_cell_style objects from a group_style specification
#'
#' @param group_style Named list (flat or per-level) from `spec$body$group_style`.
#' @param positions List from `identify_group_header_rows()`.
#' @return List of `fr_cell_style` objects.
#' @noRd
build_group_styles <- function(group_style, positions) {
  style_keys <- c(
    "bold",
    "italic",
    "underline",
    "color",
    "background",
    "font_size",
    "align",
    "valign"
  )

  is_flat <- all(names(group_style) %in% style_keys)

  if (is_flat) {
    # Single style for all group header rows
    return(list(make_row_style_from_list(group_style, positions$all)))
  }

  # Per-level: each named element targets rows at that level
  styles <- list()
  for (lvl in names(group_style)) {
    rows <- positions$by_level[[lvl]]
    if (length(rows) > 0L) {
      styles <- c(
        styles,
        list(make_row_style_from_list(group_style[[lvl]], rows))
      )
    }
  }
  styles
}


#' Create an fr_cell_style (row type) from a named property list
#' @noRd
make_row_style_from_list <- function(props, row_ids) {
  style <- new_fr_row_style(
    bold = props$bold,
    italic = props$italic,
    underline = props$underline,
    color = props$color,
    background = props$background,
    font_size = props$font_size,
    align = props$align,
    valign = props$valign
  )
  style$row_ids <- row_ids
  style
}


#' Resolve deferred "group_headers" selectors in cell_styles
#'
#' Replaces `rows = "group_headers"` or `rows = "group_headers:<level>"` with
#' actual integer row positions from `positions`.
#'
#' @param cell_styles List of `fr_cell_style` objects.
#' @param positions List from `identify_group_header_rows()`.
#' @return Updated list of `fr_cell_style` objects.
#' @noRd
resolve_deferred_group_selectors <- function(cell_styles, positions) {
  for (i in seq_along(cell_styles)) {
    rows <- cell_styles[[i]]$rows
    if (!is_group_header_selector(rows)) {
      next
    }

    # Store resolved IDs in row_ids; clear the string sentinel from rows
    if (rows == "group_headers") {
      cell_styles[[i]]$row_ids <- positions$all
    } else {
      level <- sub("^group_headers:", "", rows)
      cell_styles[[i]]$row_ids <- positions$by_level[[level]] %||% character(0)
    }
    cell_styles[[i]]$rows <- NULL
  }
  cell_styles
}


#' Merge page_by style objects into a single resolved style
#'
#' Later styles override earlier ones for the same property.
#'
#' @param styles List of style objects from `spec$page_by_styles`.
#' @return Named list with resolved `bold`, `italic`, `underline`, `color`,
#'   `background`, `font_size`, `align` values.
#' @noRd
resolve_page_by_style <- function(styles) {
  result <- list(
    bold = NULL,
    italic = NULL,
    underline = NULL,
    color = NULL,
    background = NULL,
    font_size = NULL,
    align = NULL
  )
  for (s in styles) {
    for (prop in names(result)) {
      if (!is.null(s[[prop]])) result[[prop]] <- s[[prop]]
    }
  }
  result
}


#' Build inline CSS style string for page_by labels
#'
#' @param styles List of page_by style objects.
#' @return Character string like ` style="font-weight: bold; ..."` or `""`.
#' @noRd
build_page_by_inline_css <- function(styles) {
  s <- resolve_page_by_style(styles)
  props <- character(0)
  if (isTRUE(s$bold)) {
    props <- c(props, "font-weight: bold")
  }
  if (isTRUE(s$italic)) {
    props <- c(props, "font-style: italic")
  }
  if (isTRUE(s$underline)) {
    props <- c(props, "text-decoration: underline")
  }
  if (!is.null(s$color)) {
    props <- c(props, paste0("color: ", s$color))
  }
  if (!is.null(s$background)) {
    props <- c(props, paste0("background-color: ", s$background))
  }
  if (!is.null(s$font_size)) {
    props <- c(props, paste0("font-size: ", s$font_size, "pt"))
  }
  if (!is.null(s$align)) {
    props <- c(props, paste0("text-align: ", s$align))
  }
  if (length(props) == 0L) {
    return("")
  }
  paste0(" style=\"", paste0(props, collapse = "; "), "\"")
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
#' @param preserve_cols Character vector of additional column names whose
#'   values should be copied from the preceding row to the blank row
#'   (e.g. `page_by` columns, so that `prepare_pages()` can split correctly).
#' @return A list with `data` (data frame with blanks inserted) and
#'   `insert_positions` (integer vector of original row indices after which
#'   blanks were inserted — i.e., the boundary rows in the ORIGINAL data).
#' @noRd
insert_blank_after <- function(data, blank_cols, preserve_cols = NULL) {
  empty <- list(data = data, insert_positions = integer(0))
  if (vctrs::vec_size(data) <= 1L || length(blank_cols) == 0L) {
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

  # Columns to copy from the boundary row (e.g. page_by columns)
  copy_cols <- intersect(preserve_cols, names(data))

  # Build result by interleaving data chunks and blank rows
  result <- vector("list", 2L * length(boundaries) + 1L)
  prev <- 1L
  for (k in seq_along(boundaries)) {
    result[[2L * k - 1L]] <- vctrs::vec_slice(data, prev:boundaries[k])
    brow <- blank_row
    for (gc in copy_cols) {
      brow[[gc]] <- as.character(data[[gc]][boundaries[k]])
    }
    brow$.__row_id__ <- paste0("blank_", k)
    result[[2L * k]] <- brow
    prev <- boundaries[k] + 1L
  }
  result[[2L * length(boundaries) + 1L]] <- vctrs::vec_slice(
    data,
    prev:vctrs::vec_size(data)
  )

  list(data = vctrs::vec_rbind(!!!result), insert_positions = boundaries)
}


# remap_style_indices() — DELETED
# Styles now reference stable row IDs (.__row_id__). Integer index remapping
# is no longer needed after insert_blank_after(). Blank rows get "blank_k" IDs.


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

  nr <- vctrs::vec_size(spec$data)
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
          row_ids = spec$data$.__row_id__[detail_rows],
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
        row_ids = spec$data$.__row_id__[matched_rows],
        cols = target_cols,
        indent = base_indent * level_mult
      )
      spec$cell_styles <- c(spec$cell_styles, list(indent_style))
    }
  }

  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# 1f-b. Multi-Level Hierarchy Collapse
#
# Collapses multiple hierarchy columns into a single display column with
# injected header rows for non-leaf levels and auto-set indentation.
# ══════════════════════════════════════════════════════════════════════════════

#' Collapse multi-level hierarchy columns into a single display column
#'
#' When `group_by = list(cols = c("phase", "visit", "pct"), leaf = "pct")`,
#' this function:
#' 1. Creates `__display__` and `__row_level__` columns
#' 2. Injects header-only rows for non-leaf levels at group boundaries
#' 3. Auto-hides source hierarchy columns
#' 4. Rewrites `indent_by` to use the new columns
#'
#' Must run BEFORE `finalize_columns()` so the new columns exist for width
#' estimation and source columns can be hidden before visibility resolution.
#'
#' @param spec fr_spec object.
#' @return Modified fr_spec, or unchanged if not in hierarchy mode.
#' @noRd
collapse_hierarchy <- function(spec) {
  hierarchy_cols <- spec$body$group_hierarchy_cols
  leaf <- spec$body$group_leaf

  if (is.null(hierarchy_cols) || is.null(leaf)) {
    return(spec)
  }

  data <- spec$data
  nr <- vctrs::vec_size(data)
  if (nr == 0L) {
    return(spec)
  }

  # Non-leaf levels (top to bottom)
  non_leaf <- setdiff(hierarchy_cols, leaf)
  # Build depth map: first col = depth 0, second = depth 1, etc.
  depth_map <- stats::setNames(
    seq_along(hierarchy_cols) - 1L,
    hierarchy_cols
  )

  # Build composite keys for each non-leaf level to detect boundaries
  # For level k, the key is paste(cols[1], ..., cols[k])
  page_by_cols <- spec$body$page_by

  # Pre-compute column values as character (once, not per-row)
  non_leaf_vals <- lapply(
    stats::setNames(non_leaf, non_leaf),
    function(lvl) as.character(data[[lvl]])
  )
  leaf_values <- as.character(data[[leaf]])

  # Detect boundaries for each non-leaf level (vectorised).
  # A boundary at level k occurs when the value changes from the previous row,

  # OR when any higher level (lower depth) also has a boundary.
  boundary_list <- vector("list", length(non_leaf))
  names(boundary_list) <- non_leaf
  for (k in seq_along(non_leaf)) {
    lvl <- non_leaf[k]
    vals <- non_leaf_vals[[lvl]]
    changed <- c(
      TRUE,
      vals[-1L] != vals[-nr] | is.na(vals[-1L]) != is.na(vals[-nr])
    )
    # Propagate: if a parent level changed, child levels also emit headers
    if (k > 1L) {
      parent_boundaries <- boundary_list[[non_leaf[k - 1L]]]
      changed <- changed | parent_boundaries
    }
    boundary_list[[lvl]] <- changed
  }

  # Count total header rows to pre-allocate
  n_headers <- sum(vapply(boundary_list, sum, integer(1)))

  # Add __display__ and __row_level__ to data so types match header rows
  data[["__display__"]] <- leaf_values
  data[["__row_level__"]] <- leaf

  # Build header rows in batch
  # For each boundary, record: which data row it precedes, level name, display value
  hdr_row_idx <- integer(n_headers)
  hdr_display <- character(n_headers)
  hdr_level <- character(n_headers)
  h <- 0L
  for (lvl in non_leaf) {
    boundaries <- which(boundary_list[[lvl]])
    for (b in boundaries) {
      h <- h + 1L
      hdr_row_idx[h] <- b
      hdr_display[h] <- non_leaf_vals[[lvl]][b]
      hdr_level[h] <- lvl
    }
  }

  # Sort headers by (data_row, depth) so they interleave correctly
  hdr_depth <- depth_map[hdr_level]
  hdr_order <- order(hdr_row_idx, hdr_depth)
  hdr_row_idx <- hdr_row_idx[hdr_order]
  hdr_display <- hdr_display[hdr_order]
  hdr_level <- hdr_level[hdr_order]

  # Build header data frame in one shot
  hdr_template <- vctrs::vec_init(data, 1L)
  for (cn in names(hdr_template)) {
    if (is.character(hdr_template[[cn]])) {
      hdr_template[[cn]] <- ""
    }
  }

  hdr_block <- vctrs::vec_slice(hdr_template, rep(1L, n_headers))
  hdr_block[["__display__"]] <- hdr_display
  hdr_block[["__row_level__"]] <- hdr_level
  # Assign stable row IDs to injected hierarchy header rows
  if (".__row_id__" %in% names(hdr_block)) {
    hdr_block[[".__row_id__"]] <- paste0("ch_", seq_len(n_headers))
  }

  # Preserve columns on header rows (page_by + hierarchy cols)
  preserve_cols <- unique(c(
    intersect(page_by_cols, names(data)),
    hierarchy_cols
  ))
  for (pc in preserve_cols) {
    hdr_block[[pc]] <- as.character(data[[pc]][hdr_row_idx])
  }

  # Interleave: build an index that places headers before their data rows.
  # Each data row gets position (row_number * 2); headers get (row_number * 2 - 1).
  # Then sort to get final order.
  data_positions <- seq_len(nr) * 2L
  hdr_positions <- hdr_row_idx * 2L - 1L

  new_data <- vctrs::vec_rbind(data, hdr_block)
  final_order <- order(c(data_positions, hdr_positions), method = "radix")
  new_data <- vctrs::vec_slice(new_data, final_order)
  rownames(new_data) <- NULL

  spec$data <- new_data

  # Auto-hide source hierarchy columns
  for (col in hierarchy_cols) {
    if (!is.null(spec$columns[[col]])) {
      # Only auto-hide if user hasn't explicitly set visible
      if (is.null(spec$columns[[col]]$visible)) {
        spec$columns[[col]]$visible <- FALSE
      }
    }
  }

  # Store columns to auto-hide during finalize_columns
  # (for hierarchy columns that don't have column specs yet)
  spec$body$.auto_hide_cols <- hierarchy_cols

  # Auto-set indent_by to use __row_level__ and __display__
  levels_vec <- stats::setNames(
    as.numeric(depth_map[hierarchy_cols]),
    hierarchy_cols
  )
  spec$body$indent_by <- list(
    key = "__row_level__",
    col = "__display__",
    levels = levels_vec
  )

  # Set group_by to the top-level column for group boundary detection
  # (used by blank_after, group_keep, etc.)
  spec$body$group_by <- hierarchy_cols[1L]

  # Clear hierarchy fields so collapse doesn't run again
  spec$body$group_hierarchy_cols <- NULL

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
#' For columns with `space_mode = "indent"` (the default), detects leading spaces
#' in each cell, measures their width via AFM font metrics, strips them from
#' the data, and injects cell_styles with the equivalent indent in inches.
#'
#' Skips decimal-aligned columns (own spacing engine) and hidden columns.
#'
#' @param spec An fr_spec object (post-blank-row insertion, pre-indent_by).
#' @return Modified fr_spec with stripped data and new cell_styles.
#' @noRd
apply_leading_indent <- function(spec) {
  default_spaces <- spec$columns_meta$space_mode %||% "indent"
  nr <- vctrs::vec_size(spec$data)
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
    col_spaces <- col$space_mode %||% default_spaces
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

    vals <- na_to_empty(as.character(spec$data[[nm]]))
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
    row_id_col <- spec$data$.__row_id__
    for (k in seq_along(unique_levels)) {
      n <- unique_levels[k]
      rows <- which(n_lead == n)
      indent_inches <- twips_to_inches(space_twips * n)
      new_styles[[k]] <- new_fr_cell_style(
        region = "body",
        type = "cell",
        row_ids = row_id_col[rows],
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
#'   on one page, computed from `compute_page_budget()` / `row_height_twips()`.
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
  nr <- vctrs::vec_size(data)
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
#' @param rules List of fr_rule / fr_vline_spec objects (fr_rule_box is a
#'   subclass of fr_vline_spec with box_mode = "full").
#' @param nrow_body Integer. Number of body rows.
#' @param ncol Integer. Number of visible columns.
#' @param nrow_header Integer. Number of header rows (including spanners).
#' @return A list with `header` and `body` sublists, each containing
#'   `top`, `bottom`, `left`, `right` matrices.
#' @noRd
resolve_borders <- function(rules, nrow_body, ncol, nrow_header = 1L) {
  # ── Border registry: integer matrices replace 8 x (nrow*ncol) list(NULL) ──
  # 0L = no border; positive integers index into breg$records.
  # Eliminates one list allocation per cell (e.g. 50-row × 8-col = 3200 fewer
  # list(NULL) objects). Materialized back to list matrices before returning.
  breg <- new_style_registry()

  .bid <- function(bs) .register_border(breg, bs)
  .mmat <- function(m) .materialize_border_mat(m, breg)

  # Header integer border matrices
  h_top <- matrix(0L, nrow = nrow_header, ncol = ncol)
  h_bottom <- matrix(0L, nrow = nrow_header, ncol = ncol)
  h_left <- matrix(0L, nrow = nrow_header, ncol = ncol)
  h_right <- matrix(0L, nrow = nrow_header, ncol = ncol)

  # Empty table: return header-only borders (no body rows to process)
  if (nrow_body == 0L) {
    return(list(
      header = list(
        top = .mmat(h_top), bottom = .mmat(h_bottom),
        left = .mmat(h_left), right = .mmat(h_right)
      ),
      body = list(
        top = matrix(list(NULL), nrow = 0L, ncol = ncol),
        bottom = matrix(list(NULL), nrow = 0L, ncol = ncol),
        left = matrix(list(NULL), nrow = 0L, ncol = ncol),
        right = matrix(list(NULL), nrow = 0L, ncol = ncol)
      )
    ))
  }

  # Body integer border matrices
  b_top <- matrix(0L, nrow = nrow_body, ncol = ncol)
  b_bottom <- matrix(0L, nrow = nrow_body, ncol = ncol)
  b_left <- matrix(0L, nrow = nrow_body, ncol = ncol)
  b_right <- matrix(0L, nrow = nrow_body, ncol = ncol)

  border_spec <- function(width, linestyle, fg) {
    list(width = width, linestyle = linestyle, fg = fg)
  }

  for (rule in rules) {
    if (inherits(rule, "fr_vline_spec") || inherits(rule, "fr_rule_box")) {
      # Unified handling for fr_vline_spec and fr_rule_box (which is a
      # subclass of fr_vline_spec). Use rule fields with defaults for
      # backward compat with bare fr_rule_box objects.
      bw <- rule$width %||% fr_env$rtf_box_border_wd
      bls <- rule$linestyle %||% "solid"
      bfg <- rule$fg %||% "#000000"
      bs_id <- .bid(border_spec(bw, bls, bfg))
      preset <- rule$preset
      is_fullbox <- inherits(rule, "fr_rule_box") ||
        identical(rule$box_mode, "full")

      # Full-box adds top and bottom horizontal borders
      if (is_fullbox) {
        h_top[1L, ] <- bs_id
        if (nrow_body > 0L) {
          b_bottom[nrow_body, ] <- bs_id
        }
      }

      # Determine which column gaps get vertical rules
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
      # Vectorized gap assignment: handle edge cases separately, inner in batch
      if (0L %in% gaps) {
        h_left[h_rows, 1L] <- bs_id
        b_left[b_rows, 1L] <- bs_id
      }
      if (ncol %in% gaps) {
        h_right[h_rows, ncol] <- bs_id
        b_right[b_rows, ncol] <- bs_id
      }
      inner_gaps <- gaps[gaps > 0L & gaps < ncol]
      if (length(inner_gaps) > 0L) {
        h_right[h_rows, inner_gaps] <- bs_id
        h_left[h_rows, inner_gaps + 1L] <- bs_id
        b_right[b_rows, inner_gaps] <- bs_id
        b_left[b_rows, inner_gaps + 1L] <- bs_id
      }
      next
    }

    if (!inherits(rule, "fr_rule")) {
      next
    }
    if (rule$direction != "horizontal") {
      next
    }

    bs_id <- .bid(border_spec(rule$width, rule$linestyle, rule$fg))

    if (rule$region == "header") {
      if (rule$side == "above") {
        h_top[1L, ] <- bs_id
      } else {
        # below header = bottom of last header row
        h_bottom[nrow_header, ] <- bs_id
      }
    } else if (rule$region == "body") {
      if (rule$side == "below") {
        if (nrow_body > 0L) {
          if (is.null(rule$rows)) {
            # Below last body row
            b_bottom[nrow_body, ] <- bs_id
          } else if (identical(rule$rows, "all")) {
            b_bottom[seq_len(nrow_body), ] <- bs_id
          } else {
            valid_rows <- rule$rows[rule$rows <= nrow_body]
            if (length(valid_rows) > 0L) {
              b_bottom[valid_rows, ] <- bs_id
            }
          }
        }
      } else {
        # above body = top of first body row
        if (nrow_body > 0L) {
          b_top[1L, ] <- bs_id
        }
      }
    }
  }

  # Materialize integer matrices → list-of-NULL/border-spec matrices
  list(
    header = list(
      top = .mmat(h_top), bottom = .mmat(h_bottom),
      left = .mmat(h_left), right = .mmat(h_right)
    ),
    body = list(
      top = .mmat(b_top), bottom = .mmat(b_bottom),
      left = .mmat(b_left), right = .mmat(b_right)
    )
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

  # Collect parts in a list (O(n)) instead of growing a vector (O(n²))
  n_parts <- length(non_sentinels) + length(sentinels)
  parts <- vector("list", n_parts)
  idx <- 0L
  for (i in seq_along(non_sentinels)) {
    if (nzchar(non_sentinels[i])) {
      idx <- idx + 1L
      parts[[idx]] <- escape_fn(non_sentinels[i])
    }
    if (i <= length(sentinels)) {
      tok_parts <- regmatches(
        sentinels[i],
        regexec(pattern, sentinels[i], perl = TRUE)
      )[[1L]]
      idx <- idx + 1L
      if (length(tok_parts) >= 3L) {
        parts[[idx]] <- resolver_fn(tok_parts[[2L]], tok_parts[[3L]])
      } else {
        cli::cli_warn(
          "Malformed sentinel token encountered and escaped: {.val {sentinels[i]}}."
        )
        parts[[idx]] <- escape_fn(sentinels[i])
      }
    }
  }

  paste0(unlist(parts[seq_len(idx)]), collapse = "")
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
  if (!is.null(spec$header$background)) {
    colors <- c(colors, spec$header$background)
  }
  if (!is.null(spec$header$color)) {
    colors <- c(colors, spec$header$color)
  }

  # From cell_styles
  for (style in spec$cell_styles) {
    if (!is.null(style$color)) {
      colors <- c(colors, style$color)
    }
    if (!is.null(style$background)) colors <- c(colors, style$background)
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
