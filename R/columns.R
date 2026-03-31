# ------------------------------------------------------------------------------
# columns.R -- Column width estimation and distribution
# ------------------------------------------------------------------------------

# ==============================================================================
# 1. Column Initialisation
#
# Auto-generates fr_col objects for data frame columns that haven't been
# explicitly configured via fr_cols(). Uses fr_col() from classes.R --
# NEVER duplicates the class construction.
# ==============================================================================

#' Build default columns for unconfigured data frame columns
#'
#' @param data Data frame.
#' @param configured Named list of fr_col objects already set by user.
#' @param default_width Numeric, "auto", "equal", or NULL. Global width
#'   default from fr_cols .width. NULL means auto.
#' @param width_mode Character. "fixed", "auto", or "equal".
#' @param default_align Character. Global alignment default. NULL means
#'   auto-detect from column type.
#' @param label_fn Function or NULL. Applied to auto-generated labels
#'   (column names not explicitly labelled).
#' @param labels Named list of column name to fully formatted label string.
#'   Overrides all other label sources for matching columns.
#' @param page fr_page object (for font metrics and printable area).
#' @return Named list of fr_col objects for all columns.
#' @noRd
build_default_columns <- function(
  data,
  configured = list(),
  default_width = NULL,
  width_mode = "fixed",
  default_align = NULL,
  label_fn = NULL,
  labels = NULL,
  page = new_fr_page()
) {
  col_names <- names(data)
  result <- vector("list", length(col_names))
  names(result) <- col_names

  # For fixed mode, resolve the fallback width
  # For percent mode, default_width is an fr_pct -- assign directly
  fallback_width <- if (width_mode == "fixed") {
    (default_width %||% 1.5)
  } else if (width_mode == "percent") {
    default_width
  } else {
    NULL
  }

  for (nm in col_names) {
    auto_align <- default_align %||%
      (if (is.numeric(data[[nm]])) "right" else "left")

    if (nm %in% names(configured)) {
      col_def <- configured[[nm]]
      col_def$id <- nm
      if (is.null(col_def$align)) col_def$align <- auto_align
    } else {
      # Auto-generate label: use label_fn if provided, else column name
      auto_label <- if (!is.null(label_fn)) label_fn(nm) else nm
      col_def <- fr_col(label = auto_label, align = auto_align)
      col_def$id <- nm
    }

    # .labels wins: fully formatted label overrides all other sources
    if (!is.null(labels) && nm %in% names(labels)) {
      col_def$label <- as.character(labels[[nm]])
    }

    # Resolve width = "auto" at the per-column level
    if (
      identical(col_def$width, "auto") ||
        (is.null(col_def$width) && width_mode %in% c("auto", "fit"))
    ) {
      col_def$width <- compute_col_width(data, nm, col_def$label, page)
      col_def$width_auto <- TRUE
    }

    # Fill NULL width from fallback (fixed or percent mode)
    if (is.null(col_def$width) && width_mode %in% c("fixed", "percent")) {
      col_def$width <- fallback_width
    }

    result[[nm]] <- col_def
  }

  # -- Post-pass: width distribution --------------------------------------

  if (width_mode == "auto") {
    # Distribution deferred to finalize_spec() which has the final page context
    # (e.g., col_split may be set after fr_cols via fr_page)
  } else if (width_mode == "fit") {
    result <- distribute_fit_widths(result, page)
  } else if (width_mode == "equal") {
    result <- distribute_equal_widths(result, page)
  }

  result
}


# ==============================================================================
# 2. Text Width Measurement (AFM-based)
# ==============================================================================

#' Measure text width in twips using AFM font metrics
#'
#' Sums per-character widths from pre-parsed AFM data to produce accurate
#' text width measurements for proportional fonts. Vectorized over `text`.
#'
#' @param text Character vector. Strings to measure.
#' @param font_family Character. Font family type ("modern"/"swiss"/"roman")
#'   or a font name (e.g. "Courier New", "Arial").
#' @param font_size_pt Numeric. Font size in points.
#' @param bold Logical. Use bold variant metrics.
#' @param italic Logical. Use italic variant metrics.
#' @return Numeric vector. Width in twips for each element of `text`.
#' @noRd
measure_text_width_twips <- function(
  text,
  font_family = "Helvetica",
  font_size_pt = 10,
  bold = FALSE,
  italic = FALSE
) {
  afm_name <- resolve_afm_name(font_family, bold = bold, italic = italic)
  char_widths <- afm_metrics[[afm_name]]

  # Build byte-indexed lookup table (256 entries, one per possible byte value).
  # AFM names are single-byte Latin-1 characters (0x20-0xFB). For text
  # measurement, we use charToRaw() for fast byte-level indexing on ASCII
  # content, but fall back to per-codepoint lookup for multi-byte UTF-8
  # characters (em dashes, arrows, etc.) that charToRaw() splits into
  # multiple bytes.
  default_w <- unname(char_widths[" "])
  if (is.na(default_w)) {
    default_w <- 500L
  }
  lut <- rep(default_w, 256L)
  byte_idx <- as.integer(charToRaw(paste0(names(char_widths), collapse = "")))
  lut[byte_idx] <- unname(char_widths)

  # Conversion factor: AFM units (1/1000 em) -> twips
  # 1 pt = 20 twips; AFM width at font_size_pt = width/1000 * font_size_pt pt
  scale <- font_size_pt / 1000 * 20

  vapply(
    text,
    function(t) {
      if (is.na(t) || !nzchar(t)) {
        return(0)
      }
      # Check for non-ASCII: any byte > 127 that's part of a multi-byte
      # UTF-8 sequence would produce wrong widths via charToRaw() because
      # it splits a single character into 2-4 bytes. Use utf8ToInt() for
      # those characters instead.
      raw_bytes <- charToRaw(t)
      if (all(raw_bytes <= as.raw(0x7F))) {
        # Pure ASCII fast path: byte == codepoint, direct LUT lookup
        return(sum(lut[as.integer(raw_bytes)]) * scale)
      }
      # Mixed content: measure per-codepoint
      cps <- utf8ToInt(t)
      total <- 0
      for (cp in cps) {
        if (cp <= 255L) {
          total <- total + lut[cp]
        } else {
          # Non-Latin-1 codepoint: use default (space) width
          total <- total + default_w
        }
      }
      total * scale
    },
    numeric(1),
    USE.NAMES = FALSE
  )
}


# ==============================================================================
# 3. Width Estimation
# ==============================================================================

#' Estimate column width from content and header text
#'
#' Uses the page font metrics to measure the widest cell value and
#' the header label, then converts to inches with padding.
#'
#' @param data Data frame.
#' @param col_name Column name to measure.
#' @param label Resolved display label (may contain sentinels).
#' @param page fr_page object for font metrics.
#' @return Numeric. Estimated width in inches.
#' @noRd
compute_col_width <- function(data, col_name, label, page) {
  font_family <- page$font_family
  font_size <- page$font_size

  # Content width: measure widest cell value using AFM metrics
  col_values <- as.character(data[[col_name]])
  if (length(col_values) == 0L || all(is.na(col_values))) {
    max_content_twips <- 0
    cli::cli_warn(c(
      "!" = "Column {.val {col_name}} is all NA -- width estimated from label only (min 0.5in)."
    ))
  } else {
    col_values <- na_to_empty(col_values)
    col_values <- unique(col_values)
    max_content_twips <- max(measure_text_width_twips(
      col_values,
      font_family,
      font_size
    ))
  }

  # Label width: widest line (multi-line aware, strip sentinels)
  label_plain <- label_to_plain(label)
  label_lines <- strsplit(label_plain, "\n", fixed = TRUE)[[1L]]
  # strsplit("", ...) returns c("") not character(0), so check for empty strings
  if (length(label_lines) == 0L || all(!nzchar(label_lines))) {
    label_lines <- col_name
  }
  max_label_twips <- max(measure_text_width_twips(
    label_lines,
    font_family,
    font_size,
    bold = TRUE
  ))

  # Use the wider of content or label + 2 chars padding
  padding_twips <- measure_text_width_twips("  ", font_family, font_size)
  max_twips <- max(max_content_twips, max_label_twips) + padding_twips

  # Convert to inches
  width_inches <- twips_to_inches(max_twips)

  # Clamp: minimum 0.5in, maximum 5.0in
  max(0.5, min(5.0, width_inches))
}


# ==============================================================================
# 3. Width Distribution
# ==============================================================================

#' Separate fixed-width columns from auto-estimated columns
#'
#' Classifies visible columns by `width_auto` flag. Columns with
#' `width_auto = TRUE` (auto-estimated) are scalable; all others
#' (explicit `fr_col(width=)`) are fixed.
#'
#' @param columns Named list of fr_col objects.
#' @param visible_names Character vector of visible column names.
#' @return List with `$fixed_sum` (numeric) and `$auto_names` (character).
#' @noRd
separate_fixed_auto_cols <- function(columns, visible_names) {
  is_auto <- vapply(
    visible_names,
    function(nm) isTRUE(columns[[nm]]$width_auto),
    logical(1),
    USE.NAMES = FALSE
  )
  auto_names <- visible_names[is_auto]
  fixed_sum <- sum(vapply(
    visible_names[!is_auto],
    function(nm) columns[[nm]]$width,
    numeric(1),
    USE.NAMES = FALSE
  ))
  list(fixed_sum = fixed_sum, auto_names = auto_names)
}


#' Scale auto-estimated columns to target width
#'
#' Common scaling logic for distribute_auto/fit/equal_widths.
#' Only modifies columns in `auto_names`; fixed columns are untouched.
#'
#' @param columns Named list of fr_col objects.
#' @param auto_names Character vector of auto-estimated column names.
#' @param remaining Numeric. Target total width for auto columns.
#' @return Modified columns list with scaled widths.
#' @noRd
scale_auto_columns <- function(columns, auto_names, remaining) {
  if (length(auto_names) == 0L) {
    return(columns)
  }
  auto_total <- sum(vapply(
    columns[auto_names],
    function(c) c$width,
    numeric(1)
  ))
  if (auto_total <= 0 || remaining <= 0) {
    if (auto_total <= 0) {
      cli::cli_warn(
        c(
          "Auto-width columns have zero estimated width.",
          "i" = "Columns {.val {auto_names}} may render with no visible width.",
          "i" = "Set explicit widths via {.fn fr_cols} if needed."
        ),
        call = caller_env()
      )
    }
    return(columns)
  }

  scale_factor <- remaining / auto_total
  for (nm in auto_names) {
    columns[[nm]]$width <- columns[[nm]]$width * scale_factor
  }
  columns
}


#' Distribute auto-calculated widths to fit the printable page
#'
#' After per-column auto-estimation, scale all auto-width columns
#' proportionally so the total fits the printable page width.
#' Fixed-width columns (explicit numeric in fr_col) are preserved.
#'
#' @param columns Named list of fr_col objects (all widths resolved).
#' @param page fr_page object.
#' @return Named list of fr_col objects with adjusted widths.
#' @noRd
distribute_auto_widths <- function(columns, page) {
  printable <- printable_area_inches(page)[["width"]]
  visible <- visible_columns(columns)
  if (length(visible) == 0L) {
    return(columns)
  }

  total <- sum(vapply(visible, function(col) col$width, numeric(1)))
  if (total <= 0 || total <= printable) {
    return(columns)
  }

  parts <- separate_fixed_auto_cols(columns, names(visible))
  if (length(parts$auto_names) == 0L) {
    return(columns)
  }

  scale_auto_columns(columns, parts$auto_names, printable - parts$fixed_sum)
}


#' Distribute fit widths: scale auto-calculated widths to fill page exactly
#'
#' Like "auto", but always scales (up or down) so the total width
#' exactly matches the printable page width, preserving the ratio
#' between columns.
#'
#' @param columns Named list of fr_col objects (all widths resolved).
#' @param page fr_page object.
#' @return Named list of fr_col objects with adjusted widths.
#' @noRd
distribute_fit_widths <- function(columns, page) {
  printable <- printable_area_inches(page)[["width"]]
  visible <- visible_columns(columns)
  if (length(visible) == 0L) {
    return(columns)
  }

  parts <- separate_fixed_auto_cols(columns, names(visible))
  if (length(parts$auto_names) == 0L) {
    return(columns)
  }

  scale_auto_columns(columns, parts$auto_names, printable - parts$fixed_sum)
}


#' Distribute equal widths across unset columns
#'
#' Columns with an explicit numeric width in fr_col keep their size.
#' Remaining printable space is divided equally among columns whose
#' width was not explicitly set (NULL width).
#'
#' @param columns Named list of fr_col objects.
#' @param page fr_page object.
#' @return Named list of fr_col objects with resolved widths.
#' @noRd
distribute_equal_widths <- function(columns, page) {
  printable <- printable_area_inches(page)[["width"]]
  visible <- visible_columns(columns)
  if (length(visible) == 0L) {
    return(columns)
  }

  # Separate explicit-width vs unset columns (NULL width)
  vis_names <- names(visible)
  has_width <- vapply(
    vis_names,
    function(nm) !is.null(columns[[nm]]$width),
    logical(1),
    USE.NAMES = FALSE
  )
  unfixed_names <- vis_names[!has_width]
  fixed_sum <- sum(vapply(
    vis_names[has_width],
    function(nm) columns[[nm]]$width,
    numeric(1),
    USE.NAMES = FALSE
  ))

  if (length(unfixed_names) == 0L) {
    return(columns)
  }

  remaining <- max(0.5 * length(unfixed_names), printable - fixed_sum)
  equal_width <- remaining / length(unfixed_names)

  for (nm in unfixed_names) {
    columns[[nm]]$width <- equal_width
  }
  columns
}


# ==============================================================================
# 4. Printable Area Calculation
# ==============================================================================

#' Calculate printable area in inches
#'
#' @param page fr_page object.
#' @return Named numeric: `width` and `height` in inches.
#' @noRd
printable_area_inches <- function(page) {
  dims <- paper_dims_twips(page$paper, page$orientation)
  c(
    width = twips_to_inches(dims[["width"]]) -
      page$margins$left -
      page$margins$right,
    height = twips_to_inches(dims[["height"]]) -
      page$margins$top -
      page$margins$bottom
  )
}


#' Calculate printable area in twips
#' @noRd
printable_area_twips <- function(page) {
  area <- printable_area_inches(page)
  c(
    width = inches_to_twips(area[["width"]]),
    height = inches_to_twips(area[["height"]])
  )
}
