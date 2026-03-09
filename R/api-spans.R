# ──────────────────────────────────────────────────────────────────────────────
# api-spans.R — Spanning header verb: fr_spans
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# fr_spans — Add spanning column headers
# ══════════════════════════════════════════════════════════════════════════════

#' Add Spanning Column Headers
#'
#' @description
#'
#' Adds one or more spanning headers — labels that sit above a group of
#' columns in an additional header row. Each named argument creates one span:
#' the argument **name** is the span label; the **value** is a character
#' vector of column names covered by the span.
#'
#' Calling `fr_spans()` **appends** spans to any already defined. Call it
#' multiple times with different `.level` values to build multi-level spanning
#' headers.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param ... Named character vectors. The name is the span label; the value
#'   is a character vector of data column names to span.
#' @param .level Integer. Vertical level of the span row (`1L` = immediately
#'   above the column header row; higher numbers are further above). Default
#'   `1L`.
#' @param .hline Logical. Whether to draw a thin horizontal line below the
#'   spanned columns. Default `TRUE`. Set to `FALSE` to suppress the border.
#'
#' @return A modified `fr_spec`. Spans are appended to `spec$header$spans`.
#'
#' @section Regulatory conventions:
#' Standard pharma house styles recommend:
#' * Use spanning headers **only when necessary**. Avoid them for simple
#'   two-arm tables where the column label already identifies the treatment
#'   group.
#' * When used, the span label typically carries the **treatment arm name**
#'   or **timepoint period**, while the individual column labels carry the
#'   dose level and N (e.g. `"10 mg\n(N=45)"`).
#' * Common patterns:
#'   - Single-level span grouping dose arms under a compound name:
#'     `"Zomerane" = c("low_dose", "high_dose")`
#'   - Two-level span: dose arms at level 1, compound at level 2.
#'   - Timepoint spans: `"Week 12" = c("w12_n", "w12_pct")`.
#'
#' @section Multi-level spanning headers:
#' Build from **inner to outer** (lowest level first):
#' ```r
#' spec |>
#'   fr_spans("Zomerane" = c("low_n", "high_n"), .level = 1L) |>
#'   fr_spans("Active Treatment" = c("low_n", "high_n"), .level = 2L)
#' ```
#' Level 1 spans sit directly above the column labels; level 2 spans sit
#' above level 1.
#'
#' @section Tips:
#' * Span labels support `{fr_*()}` inline markup (e.g. `"{fr_bold('Treatment')}"`).
#' * A column can appear in at most one span per level. Overlapping spans at
#'   the same level produce undefined layout behaviour — avoid this.
#' * Columns not covered by any span at a given level display an empty cell
#'   in that span row.
#' * The `.hline` border helps visually separate the span from the column
#'   labels below it. Set `.hline = FALSE` when spanning is purely
#'   decorative (e.g. grouping timepoints without a visible divider).
#' * Span alignment inherits from [fr_header()] `align` if set, otherwise
#'   defaults to center.
#'
#' @examples
#' ## ── Single-level treatment group span ────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic",    width = 2.5),
#'     zom_50mg       = fr_col("50 mg",             width = 1.5, align = "right"),
#'     zom_100mg      = fr_col("100 mg",            width = 1.5, align = "right"),
#'     placebo        = fr_col("Placebo",            width = 1.5, align = "right"),
#'     total          = fr_col("Total",              width = 1.5, align = "right")
#'   ) |>
#'   fr_header(
#'     n = c(zom_50mg = 45, zom_100mg = 45, placebo = 45, total = 135),
#'     format = "{name}\n(N={n})"
#'   ) |>
#'   fr_spans(
#'     "Zomerane" = c("zom_50mg", "zom_100mg")
#'   )
#'
#' ## ── Two-level spanning header ─────────────────────────────────────────────
#'
#' # Build a small example with dose-level columns
#' dose_data <- data.frame(
#'   param = "Weight", low_n = "43", low_pct = "31.9%",
#'   high_n = "47", high_pct = "34.8%"
#' )
#' dose_data |>
#'   fr_table() |>
#'   fr_spans("10 mg"    = c("low_n",  "low_pct"),  .level = 1L) |>
#'   fr_spans("25 mg"    = c("high_n", "high_pct"), .level = 1L) |>
#'   fr_spans("Zomerane" = c("low_n",  "low_pct",
#'                            "high_n", "high_pct"), .level = 2L)
#'
#' ## ── Markup in span label ─────────────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_spans(
#'     "{fr_bold('Zomerane')}" = c("zom_50mg", "zom_100mg")
#'   )
#'
#' ## ── Span without bottom border (.hline = FALSE) ────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_spans(
#'     "Zomerane" = c("zom_50mg", "zom_100mg"),
#'     .hline = FALSE
#'   )
#'
#' ## ── Single-column span (acts as a group label above one column) ────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_spans(
#'     "Zomerane" = c("zom_50mg", "zom_100mg"),
#'     "Reference" = "placebo"
#'   )
#'
#' ## ── Span + fr_header bold interaction ──────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(bold = TRUE) |>
#'   fr_spans(
#'     "Zomerane" = c("zom_50mg", "zom_100mg")
#'   )
#'
#' @seealso [fr_cols()] for individual column labels, [fr_col()] for the
#'   column spec constructor, [fr_header()] for header-level styling that
#'   applies to spans and column labels.
#'
#' @export
fr_spans <- function(spec, ..., .level = 1L, .hline = TRUE) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  dots <- list(...)
  if (length(dots) == 0L) return(spec)

  if (is.null(names(dots)) || any(names(dots) == "")) {
    cli_abort(
      c("All arguments to {.fn fr_spans} must be named.",
        "i" = 'The name is the span label: {.code fr_spans(spec, "Treatment" = c("col1", "col2"))}.'),
      call = call
    )
  }

  new_spans <- lapply(names(dots), function(label) {
    cols <- dots[[label]]
    if (!is.character(cols) || length(cols) == 0L) {
      cli_abort(
        c("Span {.val {label}}: value must be a non-empty character vector of column names.",
          "x" = "You supplied {.obj_type_friendly {cols}}."),
        call = call
      )
    }
    bad <- setdiff(cols, names(spec$data))
    if (length(bad) > 0L) {
      cli_abort(
        "Span {.val {label}}: column{?s} not found in the data: {.val {bad}}.",
        call = call
      )
    }
    new_fr_span(label, cols, level = .level, hline = .hline)
  })

  spec$header$spans <- c(spec$header$spans, new_spans)
  spec
}
