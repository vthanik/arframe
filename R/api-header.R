# ──────────────────────────────────────────────────────────────────────────────
# api-header.R — Column header presentation verb: fr_header
# ──────────────────────────────────────────────────────────────────────────────

#' Configure Column Header Presentation
#'
#' @description
#'
#' The single owner of all header **presentation** settings: alignment,
#' vertical alignment, bold, background/foreground colour, and font size.
#' These defaults apply to **all** column header cells unless overridden by
#' per-column `fr_col(header_align = ...)` or
#' `fr_style(region = "header", ...)`.
#'
#' [fr_cols()] owns column **structure** (labels, widths, body alignment,
#' visibility, N-counts, spanning groups); `fr_header()` owns header
#' **presentation** only. N-count labels and format are set exclusively
#' via [fr_cols()] `.n` / `.n_format` and [fr_col()] `n` parameters.
#'
#' Calling `fr_header()` again **replaces** the previous header config
#' (except spans, which are managed by [fr_spans()]).
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param align Scalar horizontal alignment for all header cells. One of
#'   `"left"`, `"center"`, `"right"`, or `NULL` (inherit from column body
#'   alignment). For per-column header alignment, use
#'   `fr_col(header_align = ...)` instead.
#'
#' @param valign Default vertical alignment. One of `"top"`, `"middle"`,
#'   `"bottom"` (default). Relevant when header rows have unequal height.
#' @param bold Logical or `NULL`. Whether header cells are bold. Default
#'   `NULL` inherits the built-in default (FALSE). Set `TRUE` explicitly
#'   if you want bold headers.
#' @param background Background colour for header cells: hex string (`"#003366"`) or
#'   CSS named colour (`"steelblue"`, `"lightgray"`, etc.). `NULL` inherits.
#' @param color Foreground (text) colour for header cells: hex string or CSS
#'   named colour. `NULL` inherits.
#' @param font_size Font size in points for header cells. `NULL` inherits
#'   from page font size.
#' @param repeat_on_page Logical. Whether to repeat the column header on
#'   every page. Default `TRUE` (standard for regulatory tables).
#' @return A modified `fr_spec`. Header config stored in `spec$header`.
#'
#' @section Priority chain for header alignment:
#' ```
#' config header.align < fr_header(align) < fr_col(header_align) < fr_style(region="header", align)
#' ```
#'
#' @section Regulatory conventions:
#' Many pharma TFL outputs use **bold, centered** column headers with
#' `valign = "bottom"` so that short labels (e.g. "Characteristic") align
#' at the bottom when adjacent columns have multi-line labels (e.g.
#' `"Placebo\\n(N=45)"`). By default, headers are **not bold** and inherit
#' alignment from the column's `align` setting — use
#' `fr_header(bold = TRUE, align = "center")` to opt in.
#'
#' @examples
#' ## ── Center all headers with bottom valign ────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(align = "center", valign = "bottom", bold = TRUE)
#'
#' ## ── Per-column header alignment via fr_col ────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", header_align = "left"),
#'     zom_50mg       = fr_col("Zomerane 50 mg", header_align = "center"),
#'     placebo        = fr_col("Placebo",         header_align = "center"),
#'     total          = fr_col("Total",           header_align = "right")
#'   ) |>
#'   fr_header(bold = TRUE)
#'
#' ## ── Header background colour (hex or CSS named colour) ──────────────────
#'
#' # Hex colour
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(background = "#E0E0E0", bold = TRUE)
#'
#' # CSS named colour (148 available: lavender, aliceblue, gainsboro, etc.)
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(background = "lavender", color = "midnightblue", bold = TRUE)
#'
#' ## ── N-counts now on fr_cols(), not fr_header() ────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     zom_50mg       = fr_col("Zomerane 50 mg", n = 45),
#'     placebo        = fr_col("Placebo",         n = 45),
#'     .n_format = "{label}\n(N={n})"
#'   ) |>
#'   fr_header(bold = TRUE, align = "center")
#'
#' @section Precedence:
#' Header alignment priority (last wins):
#'
#' `_arframe.yml` < `fr_theme(header=)` < `fr_header(align=)` <
#' `fr_col(header_align=)` < `fr_style(region="header", align=)`
#'
#' Header font size priority:
#'
#' `fr_page(font_size=)` < `fr_header(font_size=)` <
#' `fr_style(region="header", font_size=)`
#'
#' @seealso [fr_cols()] for column structure and N-counts, [fr_spans()] for
#'   spanning headers, [fr_col()] for per-column `header_align` overrides,
#'   [fr_style()] with `region = "header"` for cell-level overrides,
#'   [fr_config()] for setting header defaults via `_arframe.yml`.
#'
#' @export
fr_header <- function(
  spec,
  align = NULL,
  valign = NULL,
  bold = NULL,
  background = NULL,
  color = NULL,
  font_size = NULL,
  repeat_on_page = NULL
) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  # ── Resolve align: scalar string only ─────────────────────────────────────

  if (!is.null(align)) {
    if (is.list(align)) {
      cli_abort(
        c(
          "{.arg align} must be a scalar string, not a list.",
          "i" = "For per-column header alignment, use {.code fr_col(header_align = ...)} instead."
        ),
        call = call
      )
    }
    align <- match_arg_fr(align, fr_env$valid_aligns, call = call)
  }

  if (!is.null(valign)) {
    valign <- match_arg_fr(valign, fr_env$valid_valigns, call = call)
  }
  if (!is.null(bold)) {
    check_scalar_lgl(bold, arg = "bold", call = call)
  }
  if (!is.null(font_size)) {
    check_positive_num(font_size, arg = "font_size", call = call)
  }
  if (!is.null(background)) {
    background <- resolve_color(background, call = call)
  }
  if (!is.null(color)) {
    color <- resolve_color(color, call = call)
  }
  if (!is.null(repeat_on_page)) {
    check_scalar_lgl(repeat_on_page, arg = "repeat_on_page", call = call)
  }

  # Preserve existing spans — fr_header replaces everything else
  old <- spec$header

  spec$header <- new_fr_header(
    spans = old$spans,
    repeat_on_page = if (!missing(repeat_on_page)) {
      repeat_on_page
    } else {
      old$repeat_on_page
    },
    valign = if (!missing(valign)) valign else old$valign,
    align = if (!missing(align)) align else old$align,
    bold = if (!missing(bold)) bold else old$bold,
    background = if (!missing(background)) background else old$background,
    color = if (!missing(color)) color else old$color,
    font_size = if (!missing(font_size)) font_size else old$font_size,
    span_gap = old$span_gap
  )

  spec
}
