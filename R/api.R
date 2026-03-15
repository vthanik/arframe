# ─────────────────────────────────────────────────────────────────────────────
# api.R — Pipeline entry point: fr_table
#
# Pipeline contract (all verbs follow this):
#   - First argument is always an fr_spec (validated with check_fr_spec)
#   - Returns a modified fr_spec — the pipeline is immutable
#   - Side effects happen only at fr_render()
#
# See also: api-content.R (fr_titles, fr_footnotes),
#           api-theme.R (fr_theme, fr_theme_get, fr_theme_reset),
#           api-cols.R (fr_cols), api-header.R (fr_header),
#           api-spans.R (fr_spans), api-rows.R (fr_rows),
#           api-page.R (fr_page, fr_pagehead, fr_pagefoot),
#           api-rules.R (fr_hlines, fr_vlines, fr_grid),
#           api-style.R (fr_style, fr_row_style, fr_col_style, fr_styles).
# ─────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# fr_table — Pipeline entry point
# ══════════════════════════════════════════════════════════════════════════════

#' Start a arframe Table Pipeline
#'
#' @description
#'
#' `fr_table()` is the entry point for every **arframe** pipeline. It wraps a
#' presentation-ready data frame in an `fr_spec` object, which you then
#' configure with the `fr_*()` verbs and finally render with `fr_render()`.
#'
#' The data frame should already contain the rows and columns you want to
#' display — arframe does not summarise or reshape data. Use packages such as
#' **gt**, **Tplyr**, **rtables**, **tidytlg**, or **tfrmt** to summarise your
#' data, then hand the summary data frame off to `fr_table()`.
#'
#' @param data A data frame (or tibble). Must already be presentation-ready:
#'   each row maps to one table row, each column to one table column. No
#'   summarisation is performed by arframe.
#'
#' @return An `fr_spec` object. Pass it to any `fr_*()` verb via `|>`.
#'
#' @section Pipeline overview:
#' ```
#' data |>
#'   fr_table()                        # start
#'   |> fr_titles(...)                 # titles above the table
#'   |> fr_footnotes(...)              # footnotes below
#'   |> fr_cols(col = fr_col(...), .n = ...) # columns / N-counts
#'   |> fr_header(bold = TRUE, align = .)   # header presentation
#'   |> fr_spans(...)                  # spanning headers
#'   |> fr_rows(page_by = ...)         # pagination / grouping
#'   |> fr_page(orientation = ...)     # page layout
#'   |> fr_pagehead(left = ...)        # running header
#'   |> fr_pagefoot(right = ...)       # running footer
#'   |> fr_hlines("header")         # horizontal rules
#'   |> fr_vlines("box")               # vertical rules
#'   |> fr_styles(fr_row_style(...))   # cell / row / column styling
#'   |> fr_render("output.rtf")        # render to file
#' ```
#'
#' @section Tips:
#' * Any `fr_*()` verb can be called multiple times; most **replace** (not
#'   append) the previous setting. Exceptions: `fr_spans()` and `fr_styles()`
#'   **append** on repeated calls.
#' * Column order in the rendered table matches the column order of `data`.
#'   Reorder columns in `data` before calling `fr_table()` if needed.
#' * Invisible columns (e.g. grouping keys) can be hidden via
#'   `fr_cols(col = fr_col(visible = FALSE))` rather than dropping them
#'   from `data`.
#'
#' @examples
#' ## ── Minimal: one-step pipeline ───────────────────────────────────────────
#'
#' tbl_demog |> fr_table()
#'
#' ## ── Typical regulatory pipeline ─────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles(
#'     "Table 14.1.1 Summary of Demographics and Baseline Characteristics",
#'     "Full Analysis Set"
#'   ) |>
#'   fr_footnotes("[a] Percentages based on the number of subjects in each arm.") |>
#'   fr_hlines("header") |>
#'   fr_page(orientation = "landscape", font_size = 9)
#'
#' ## ── AE by SOC/PT with pagination ─────────────────────────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_titles("Table 14.3.2 Adverse Events by System Organ Class and Preferred Term",
#'             "Safety Analysis Set") |>
#'   fr_rows(page_by = "soc", group_by = "soc") |>
#'   fr_hlines("header")
#'
#' ## ── Store spec, reuse, branch ────────────────────────────────────────────
#'
#' base_spec <- tbl_demog |>
#'   fr_table() |>
#'   fr_page(orientation = "landscape", font_size = 9)
#'
#' # Branch A: portrait with smaller font
#' spec_a <- base_spec |> fr_page(orientation = "portrait", font_size = 8)
#'
#' # Branch B: add titles and render
#' spec_b <- base_spec |>
#'   fr_titles("Table 14.1.1 Demographics") |>
#'   fr_hlines("booktabs")
#'
#' ## ── Complete 10-verb pipeline ─────────────────────────────────────────────
#' # Demonstrates every major pipeline verb in a single specification.
#' # In study programs, hlines/header/pagehead/pagefoot go in fr_theme().
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles(
#'     "Table 14.1.1 Summary of Demographics and Baseline Characteristics",
#'     "Full Analysis Set"
#'   ) |>
#'   fr_footnotes(
#'     "[a] Percentages based on subjects in each arm.",
#'     "Source: ADSL"
#'   ) |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     placebo        = fr_col("Placebo\n(N=45)", width = 1.5),
#'     zom_50mg       = fr_col("Zomerane 50mg\n(N=45)", width = 1.5),
#'     zom_100mg      = fr_col("Zomerane 100mg\n(N=45)", width = 1.5),
#'     total          = fr_col("Total\n(N=135)", width = 1.5),
#'     group          = fr_col(visible = FALSE)
#'   ) |>
#'   fr_header(bold = TRUE, bg = "#D9E2F3") |>
#'   fr_spans("Treatment Arm" = c("placebo", "zom_50mg", "zom_100mg")) |>
#'   fr_rows(group_by = "group", indent_by = "group") |>
#'   fr_hlines("header") |>
#'   fr_vlines("inner") |>
#'   fr_styles(
#'     fr_row_style(rows = 1, bold = TRUE)
#'   ) |>
#'   fr_page(orientation = "landscape", font_size = 9) |>
#'   fr_pagehead(left = "Study TFRM-2024-001", right = "Page {thepage} of {total_pages}") |>
#'   fr_pagefoot(left = "{program}", right = "{datetime}") |>
#'   fr_spacing(titles_after = 1, footnotes_before = 1)
#'
#' ## ── fr_table() vs fr_listing() on the same data ──────────────────────────
#' # fr_table() uses auto-detected defaults; fr_listing() sets listing defaults.
#'
#' ae_subset <- adae[1:20, c("USUBJID", "AEDECOD", "AESEV", "ASTDT")]
#'
#' # Table view: auto-detected alignment, 9pt, no split
#' spec_tbl <- ae_subset |>
#'   fr_table() |>
#'   fr_titles("Table View of AE Records")
#' spec_tbl$page$font_size    # 9 (default)
#'
#' # Listing view: left-aligned, 8pt, split = TRUE, wrap = TRUE
#' spec_lst <- ae_subset |>
#'   fr_listing() |>
#'   fr_titles("Listing View of AE Records")
#' spec_lst$page$font_size          # 8 (listing default)
#' spec_lst$columns_meta$split      # TRUE (listing default)
#'
#' ## ── Hidden column used for grouping ──────────────────────────────────────
#' # The "group" column drives indent_by but is not displayed.
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     group = fr_col(visible = FALSE)
#'   ) |>
#'   fr_rows(indent_by = "group") |>
#'   fr_titles("Table 14.1.1 Demographics (grouped, hidden key)")
#'
#' @seealso [fr_titles()], [fr_footnotes()], [fr_cols()], [fr_header()],
#'   [fr_spans()], [fr_rows()], [fr_page()], [fr_hlines()], [fr_vlines()],
#'   [fr_styles()], [fr_config()], [fr_render()]
#'
#' @export
fr_table <- function(data) {
  if (!is.data.frame(data)) {
    cli_abort(
      c(
        "{.arg data} must be a data frame.",
        "x" = "You supplied {.obj_type_friendly {data}}.",
        "i" = "Example: {.code my_df |> fr_table()}"
      ),
      call = caller_env()
    )
  }
  spec <- new_fr_spec(data)
  spec <- apply_config(spec)
  spec <- apply_fr_theme(spec)
  spec
}


#' Start a arframe Listing Pipeline
#'
#' @description
#'
#' Entry point for clinical listings — long-format data displays that show
#' individual records (e.g., adverse event listings, concomitant medication
#' listings, lab data listings). Listings differ from summary tables in that
#' every row is a data record, not a summary statistic.
#'
#' `fr_listing()` creates an `fr_spec` with listing-appropriate defaults:
#' landscape orientation, smaller font (8pt), left-aligned columns,
#' header-only rules, and column splitting enabled.
#'
#' @param data A data frame. Each row maps to one listing row.
#'
#' @return An `fr_spec` object with `type = "listing"` and listing defaults.
#'
#' @section Listing defaults (vs `fr_table()`):
#' | Setting | `fr_table()` | `fr_listing()` |
#' |---------|-------------|----------------|
#' | Orientation | landscape | landscape |
#' | Font size | 9pt | 8pt |
#' | Default align | auto-detect | left |
#' | hlines | none | header |
#' | split | NULL | TRUE |
#' | wrap | FALSE | TRUE |
#'
#' @section Listing-specific features:
#' * **`sort_by`**: Pass to [fr_rows()] to sort data before rendering
#' * **`repeat_cols`**: Pass to [fr_rows()] to suppress repeated values
#' * **`wrap`**: Auto-enabled for long text columns
#'
#' @examples
#' ## ── Minimal listing ──────────────────────────────────────────────────────
#'
#' adae |> fr_listing()
#'
#' ## ── Full listing pipeline ─────────────────────────────────────────────────
#'
#' adae |>
#'   fr_listing() |>
#'   fr_cols(
#'     USUBJID = fr_col("Subject ID", width = 1.2),
#'     AEDECOD = fr_col("Preferred Term"),
#'     AESEV   = fr_col("Severity", width = 1.0)
#'   ) |>
#'   fr_rows(sort_by = c("USUBJID", "ASTDT"),
#'           repeat_cols = "USUBJID") |>
#'   fr_titles("Listing 16.2.7 Adverse Events") |>
#'   fr_footnotes("Source: ADAE")
#'
#' ## ── Listing with wrap = TRUE for long text ───────────────────────────────
#' # wrap is TRUE by default in fr_listing(), but can also be set via fr_rows().
#'
#' adcm |>
#'   fr_listing() |>
#'   fr_cols(
#'     USUBJID = fr_col("Subject", width = 1.2),
#'     CMDECOD = fr_col("Medication", width = 2.0),
#'     CMCAT   = fr_col("Category", width = 1.5),
#'     CMSTDT  = fr_col("Start Date", width = 1.0),
#'     CMENDT  = fr_col("End Date", width = 1.0)
#'   ) |>
#'   fr_rows(wrap = TRUE) |>
#'   fr_titles("Listing 16.2.4 Concomitant Medications") |>
#'   fr_footnotes("Source: ADCM")
#'
#' ## ── Listing with page_by pagination ──────────────────────────────────────
#' # Each treatment arm starts on a new page.
#'
#' adae |>
#'   fr_listing() |>
#'   fr_cols(
#'     USUBJID  = fr_col("Subject ID", width = 1.2),
#'     AEBODSYS = fr_col("System Organ Class", width = 2.0),
#'     AEDECOD  = fr_col("Preferred Term", width = 1.5),
#'     AESEV    = fr_col("Severity", width = 0.8),
#'     TRTA     = fr_col(visible = FALSE)
#'   ) |>
#'   fr_rows(page_by = "TRTA") |>
#'   fr_titles("Listing 16.2.7.1 Adverse Events by Treatment Arm")
#'
#' ## ── sort_by + repeat_cols together ────────────────────────────────────────
#' # Sort by subject and start day, suppress repeated subject IDs.
#'
#' adae |>
#'   fr_listing() |>
#'   fr_cols(
#'     USUBJID = fr_col("Subject ID", width = 1.3),
#'     AEDECOD = fr_col("Preferred Term", width = 2.0),
#'     AESEV   = fr_col("Severity", width = 0.8),
#'     ASTDY   = fr_col("Study Day", width = 0.8, align = "right"),
#'     ADURN   = fr_col("Duration (days)", width = 1.0, align = "right")
#'   ) |>
#'   fr_rows(
#'     sort_by = c("USUBJID", "ASTDY"),
#'     repeat_cols = "USUBJID"
#'   ) |>
#'   fr_titles("Listing 16.2.7.2 Adverse Events Sorted by Subject and Study Day") |>
#'   fr_footnotes("Repeated subject IDs are suppressed for readability.")
#'
#' @seealso [fr_table()] for summary tables, [fr_rows()] for `sort_by` and
#'   `repeat_cols`, [fr_render()] to produce output.
#'
#' @export
fr_listing <- function(data) {
  if (!is.data.frame(data)) {
    cli_abort(
      c(
        "{.arg data} must be a data frame.",
        "x" = "You supplied {.obj_type_friendly {data}}.",
        "i" = "Example: {.code my_df |> fr_listing()}"
      ),
      call = caller_env()
    )
  }

  spec <- new_fr_spec(data, type = "listing")
  spec <- apply_config(spec)

  # Listing defaults applied BEFORE theme so theme can override them
  spec$page$font_size <- 8
  spec$columns_meta$split <- TRUE
  spec$body$wrap <- TRUE

  # Default to left alignment for all columns
  for (nm in names(spec$data)) {
    if (is.null(spec$columns[[nm]])) {
      col_def <- fr_col(label = nm, align = "left")
      col_def$id <- nm
      spec$columns[[nm]] <- col_def
    }
  }

  # Default header rule (before theme so theme can override)
  spec <- fr_hlines(spec, "header")

  # Theme applied last — can override all listing defaults
  spec <- apply_fr_theme(spec)

  spec
}


#' Start a arframe Figure Pipeline
#'
#' @description
#'
#' Entry point for figures — wraps a plot object (ggplot2 or base R) with
#' the same titling, footnoting, and page chrome as tables. The plot is
#' embedded in the rendered output with consistent regulatory formatting.
#'
#' @param plot A plot object or a list of plot objects. Supported types:
#'   * `ggplot` object (from ggplot2)
#'   * `recordedplot` object (from `recordPlot()`)
#'   * A `list` of the above for multi-page figures (one plot per page)
#' @param width Numeric. Plot width in inches within the page. Default `NULL`
#'   uses the full printable width.
#' @param height Numeric. Plot height in inches within the page. Default `NULL`
#'   uses the remaining height after titles and footnotes.
#' @param meta A data frame of per-page metadata for multi-page figures.
#'   Must have exactly one row per plot. Column names become tokens available
#'   in [fr_titles()] and [fr_footnotes()] via `{column_name}` syntax,
#'   resolved dynamically for each page. Ignored for single-plot figures.
#'
#' @return An `fr_spec` object with `type = "figure"`.
#'
#' @section Supported verbs:
#' Figures support: [fr_titles()], [fr_footnotes()], [fr_page()],
#' [fr_pagehead()], [fr_pagefoot()], [fr_spacing()], [fr_render()].
#'
#' Column/header/row/style/span/rule verbs are no-ops on figure specs.
#'
#' @section Render strategy:
#' * **PDF**: Plot saved as temporary PDF, included via `\\includegraphics`
#'   in LaTeX with titles/footnotes as DeclareTblrTemplate.
#' * **RTF**: Plot saved as temporary PNG, embedded via `\\pict` RTF
#'   picture group with titles as paragraphs.
#'
#' @examples
#' # fr_figure() requires a ggplot or recordedplot object
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(adtte, ggplot2::aes(x = AVAL, y = TRTA)) +
#'     ggplot2::geom_point()
#'
#'   spec <- p |>
#'     fr_figure() |>
#'     fr_titles("Figure 14.2.1 Time-to-Event") |>
#'     fr_footnotes("Source: ADTTE") |>
#'     fr_page(orientation = "landscape")
#'   spec
#' }
#'
#' ## ── Explicit width and height ─────────────────────────────────────────────
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(adsl, ggplot2::aes(x = AGE, fill = TRT01A)) +
#'     ggplot2::geom_histogram(binwidth = 5, position = "dodge") +
#'     ggplot2::labs(x = "Age (years)", y = "Count", fill = "Treatment")
#'
#'   spec <- p |>
#'     fr_figure(width = 7, height = 4.5) |>
#'     fr_titles(
#'       "Figure 14.1.2 Distribution of Age by Treatment Arm",
#'       "Full Analysis Set"
#'     ) |>
#'     fr_footnotes("Source: ADSL") |>
#'     fr_page(orientation = "landscape")
#'   spec
#' }
#'
#' ## ── Base R plot with recordPlot() ─────────────────────────────────────────
#' # Capture a base R plot as a recordedplot object.
#' # Use a temporary PDF device to avoid creating Rplots.pdf.
#' tmp_pdf <- tempfile(fileext = ".pdf")
#' pdf(tmp_pdf)
#' old_par <- par(mar = c(5, 4, 2, 1))
#' plot(adsl$AGE, adsl$BMIBL,
#'      xlab = "Age (years)", ylab = "BMI (kg/m2)",
#'      pch = 19, col = "#4472C4", cex = 0.7,
#'      main = "")
#' p_base <- recordPlot()
#' par(old_par)
#' dev.off()
#' unlink(tmp_pdf)
#'
#' spec <- p_base |>
#'   fr_figure(width = 6, height = 4) |>
#'   fr_titles("Figure 14.1.3 Age vs BMI at Baseline") |>
#'   fr_footnotes("Source: ADSL")
#' spec
#'
#' @section Multi-page figures:
#' Pass a list of plots and an optional `meta` data frame to create multi-page
#' figures. Each meta column becomes a token in titles and footnotes, resolved
#' per page:
#'
#' ```
#' plots <- list(p_adults, p_pediatrics, p_geriatrics)
#' page_meta <- data.frame(subgroup = c("Adults", "Pediatrics", "Geriatrics"))
#'
#' plots |>
#'   fr_figure(meta = page_meta) |>
#'   fr_titles("Figure 14.1.1 Kaplan-Meier Curve", "Subgroup: {subgroup}") |>
#'   fr_render("km_multi.rtf")
#' ```
#'
#' @seealso [fr_table()] for summary tables, [fr_listing()] for listings.
#'
#' @export
fr_figure <- function(plot, width = NULL, height = NULL, meta = NULL) {
  call <- caller_env()

  # ── Multi-page: list of plots ──────────────────────────────────────────────
  if (
    is.list(plot) &&
      !inherits(plot, "ggplot") &&
      !inherits(plot, "recordedplot")
  ) {
    if (length(plot) == 0L) {
      cli_abort(
        c(
          "{.arg plot} list must contain at least one plot.",
          "i" = "Pass a single plot or a non-empty list of plots."
        ),
        call = call
      )
    }

    # Validate each element
    is_plot <- function(x) inherits(x, "ggplot") || inherits(x, "recordedplot")
    for (i in seq_along(plot)) {
      if (!is_plot(plot[[i]])) {
        p <- plot[[i]]
        cli_abort(
          c(
            "Element {i} of {.arg plot} must be a {.cls ggplot} or {.cls recordedplot} object.",
            "x" = "You supplied {.obj_type_friendly {p}}.",
            "i" = "Every element of the plot list must be a plot object."
          ),
          call = call
        )
      }
    }

    # Validate meta
    if (!is.null(meta)) {
      if (!is.data.frame(meta)) {
        cli_abort(
          c(
            "{.arg meta} must be a data frame.",
            "x" = "You supplied {.obj_type_friendly {meta}}."
          ),
          call = call
        )
      }
      if (nrow(meta) != length(plot)) {
        cli_abort(
          c(
            "{.arg meta} must have one row per plot.",
            "x" = "{.arg plot} has {length(plot)} element{?s} but {.arg meta} has {nrow(meta)} row{?s}."
          ),
          call = call
        )
      }
    }

    if (!is.null(width)) {
      check_positive_num(width, arg = "width", call = call)
    }
    if (!is.null(height)) {
      check_positive_num(height, arg = "height", call = call)
    }

    spec <- new_fr_spec(
      data = vctrs::new_data_frame(),
      type = "figure",
      plot = plot[[1L]],
      plots = plot,
      figure_meta = meta
    )
    spec$figure_width <- width
    spec$figure_height <- height
    spec <- apply_config(spec)
    spec <- apply_fr_theme(spec)
    return(spec)
  }

  # ── Single plot ────────────────────────────────────────────────────────────
  is_plot <- function(x) inherits(x, "ggplot") || inherits(x, "recordedplot")

  if (!is_plot(plot)) {
    cli_abort(
      c(
        "{.arg plot} must be a {.cls ggplot} or {.cls recordedplot} object.",
        "x" = "You supplied {.obj_type_friendly {plot}}.",
        "i" = "Use {.fn ggplot2::ggplot} or {.fn recordPlot} to create plots."
      ),
      call = call
    )
  }

  if (!is.null(width)) {
    check_positive_num(width, arg = "width", call = call)
  }
  if (!is.null(height)) {
    check_positive_num(height, arg = "height", call = call)
  }

  if (!is.null(meta)) {
    cli::cli_warn(
      c(
        "{.arg meta} is ignored for single-plot figures.",
        "i" = "Pass a list of plots to use per-page metadata."
      )
    )
  }

  # Create spec with empty data frame
  spec <- new_fr_spec(
    data = vctrs::new_data_frame(),
    type = "figure",
    plot = plot
  )
  spec$figure_width <- width
  spec$figure_height <- height
  spec <- apply_config(spec)
  spec <- apply_fr_theme(spec)

  spec
}
