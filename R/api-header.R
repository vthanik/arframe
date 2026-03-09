# ──────────────────────────────────────────────────────────────────────────────
# api-header.R — Column header presentation verb: fr_header
# ──────────────────────────────────────────────────────────────────────────────


#' Configure Column Header Presentation
#'
#' @description
#'
#' The single owner of all header presentation settings: alignment, vertical
#' alignment, N-count label formatting, bold, background/foreground colour,
#' and font size. These defaults apply to **all** column header cells unless
#' overridden by per-column `fr_col(header_align = ...)` or
#' `fr_style(region = "header", ...)`.
#'
#' [fr_cols()] owns column **structure** (labels, widths, body alignment,
#' visibility); `fr_header()` owns header **presentation**. There is no
#' overlap — N-count formatting, header alignment, and header valign live
#' exclusively here.
#'
#' N-count label formatting is **deferred** — the `n` and `format` values
#' are stored on the spec and resolved during rendering. This makes
#' `fr_header()` and [fr_cols()] order-independent (ggplot2 pattern).
#'
#' Calling `fr_header()` again **replaces** the previous header config
#' (except spans, which are managed by [fr_spans()]).
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param align Default horizontal alignment for all header cells. One of
#'   `"left"`, `"center"`, `"right"`, or `NULL` (inherit from column).
#'   Overridden per-column by `fr_col(header_align = ...)`.
#' @param valign Default vertical alignment. One of `"top"`, `"middle"`,
#'   `"bottom"` (default). Relevant when header rows have unequal height.
#' @param n Subject counts for N-count column header labels. Four forms:
#'
#'   * **Named numeric vector** — global N, same for all page groups.
#'     Names must match data column names.
#'     Example: `c(placebo = 45, zom_50mg = 44)`.
#'
#'   * **Named list of named numeric vectors** — per-group N. List names
#'     must match `page_by` group values. Each element is a named numeric
#'     vector keyed by column name.
#'     Example: `list("Blood Pressure" = c(placebo = 42, zom_50mg = 40),
#'                     "Heart Rate" = c(placebo = 45, zom_50mg = 44))`.
#'
#'   * **Function** — called once per page group to compute N dynamically.
#'     Must accept exactly two arguments and return a named numeric vector
#'     keyed by column name:
#'
#'     \describe{
#'       \item{`group_data`}{A data frame: the subset of `n_data` (or
#'         `spec$data` if `n_data` is `NULL`) filtered to the current
#'         `page_by` group. For example, if `page_by = "param"` and the
#'         current page is `"Systolic BP (mmHg)"`, then `group_data`
#'         contains only rows where `PARAM == "Systolic BP (mmHg)"`.
#'         When there is no `page_by`, `group_data` is the full dataset.}
#'       \item{`group_label`}{A character string: the display label for
#'         the current `page_by` group (e.g. `"Systolic BP (mmHg)"`).
#'         `NULL` when no `page_by` is set. Useful for conditional
#'         counting logic that varies by group — for example, excluding
#'         baseline visits for certain parameters, or using a different
#'         subject-ID column per group.}
#'     }
#'
#'     Most functions only need `group_data` (the subset already does the
#'     filtering). `group_label` is available for edge cases where the
#'     counting logic itself must differ by group. Both arguments are
#'     always passed by tlframe, so the function must accept both even if
#'     it ignores `group_label`.
#'
#'     Example: `function(d, gl) c(placebo = length(unique(d$USUBJID[d$TRT01A == "Placebo"])))`.
#'
#'   * **`"auto"`** — auto-compute unique subjects per column per group.
#'     Requires `n_subject` (subject ID column name). Uses `n_data` if
#'     provided, otherwise counts from `spec$data`. For each display column
#'     name present in the source data, counts unique `n_subject` values
#'     where the column is non-missing. Best suited for **wide-format**
#'     source data where column names match the display table. For
#'     long-format CDISC data (e.g., `TRTA` column), use the function
#'     form instead.
#'
#' @param format A [glue][glue::glue]-style format string for N-count
#'   labels. Available tokens: `{name}` (column label) and `{n}` (count).
#'   Default `NULL` (no N-count formatting).
#'   Example: `"{name}\n(N={n})"`.
#' @param n_subject Character scalar. Column name containing subject
#'   identifiers (e.g. `"USUBJID"`). Required when `n = "auto"`.
#'   The column must exist in `n_data` (or `spec$data` if `n_data` is
#'   `NULL`).
#' @param n_data Optional data frame for N-count computation. When
#'   provided, the `"auto"` and function forms of `n` operate on this
#'   dataset instead of `spec$data`. Useful when the display table is
#'   pre-summarized but N-counts must come from record-level source data
#'   (e.g. ADSL, ADVS). The data frame must contain the `page_by`
#'   column(s) if per-group counts are needed.
#' @param bold Logical or `NULL`. Whether header cells are bold. Default
#'   `NULL` inherits the built-in default (FALSE). Set `TRUE` explicitly
#'   if you want bold headers.
#' @param bg Background colour for header cells: hex string (`"#003366"`) or
#'   CSS named colour (`"steelblue"`, `"lightgray"`, etc.). `NULL` inherits.
#' @param fg Foreground (text) colour for header cells: hex string or CSS
#'   named colour. `NULL` inherits.
#' @param font_size Font size in points for header cells. `NULL` inherits
#'   from page font size.
#' @param repeat_on_page Logical. Whether to repeat the column header on
#'   every page. Default `TRUE` (standard for regulatory tables).
#' @param span_gap Logical or `NULL`. Whether to insert narrow gap columns
#'   between adjacent spanning headers at the same level. Gap columns create
#'   a clean visual break between span groups in both RTF and PDF output,
#'   without relying on border trimming. Default `TRUE`. Set `FALSE` to
#'   disable gap insertion and produce continuous span hlines.
#' @return A modified `fr_spec`. Header config stored in `spec$header`.
#'
#' @section Priority chain for header alignment:
#' ```
#' config header.align < fr_header(align) < fr_col(header_align) < fr_style(region="header", align)
#' ```
#'
#' @section N-count label resolution:
#' N-count labels are resolved at different stages depending on the form of
#' `n`:
#'
#' * **Named numeric vector** (global) — resolved once during
#'   `finalize_spec()`. All page groups share the same labels.
#' * **Named list, function, or `"auto"`** — resolved per page group in
#'   the render loop. Each group gets its own header labels via a label
#'   override map, without cloning the column specs.
#'
#' The `{name}` token in `format` expands to the column's resolved label
#' (from `fr_col(label = ...)` or auto-generated). This means
#' `fr_header()` and `fr_cols()` can be called in any order.
#'
#' @section Regulatory conventions:
#' Many pharma TFL outputs use **bold, centered** column headers with
#' `valign = "bottom"` so that short labels (e.g. "Characteristic") align
#' at the bottom when adjacent columns have multi-line labels (e.g.
#' "Placebo\\n(N=45)"). The `{name}\\n(N={n})` format is the standard ICH/CDISC
#' convention for treatment-arm headers. By default, headers are **not bold**
#' and inherit alignment from the column's `align` setting — use
#' `fr_header(bold = TRUE, align = "center")` to opt in.
#'
#' @examples
#' ## ── Center all headers with bottom valign ────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(align = "center", valign = "bottom", bold = TRUE)
#'
#' ## ── Global N-count (same for all groups) ─────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     zom_50mg       = fr_col("Zomerane 50 mg", align = "right"),
#'     placebo        = fr_col("Placebo", align = "right")
#'   ) |>
#'   fr_header(
#'     n = c(zom_50mg = 45, placebo = 45),
#'     format = "{name}\n(N={n})"
#'   )
#'
#' ## ── Per-group N-count (different N per page_by group) ────────────────────
#'
#' # Hardcode N per parameter — each page_by group shows its own N
#' tbl_vs |>
#'   fr_table() |>
#'   fr_rows(page_by = "param") |>
#'   fr_header(
#'     n = list(
#'       "Systolic BP (mmHg)"  = c(placebo = 45, zom_50mg = 45, zom_100mg = 39, total = 129),
#'       "Heart Rate (bpm)"    = c(placebo = 45, zom_50mg = 38, zom_100mg = 40, total = 123),
#'       "Temperature (C)"     = c(placebo = 42, zom_50mg = 39, zom_100mg = 37, total = 118)
#'     ),
#'     format = "{name}\n(N={n})"
#'   )
#'
#' ## ── Function N-count (dynamic from record-level source) ──────────────────
#'
#' # Best approach for long-format source data (CDISC ADaM pattern):
#' # the function receives the per-group subset of n_data and computes
#' # N per treatment arm dynamically.
#' #
#' # group_data = rows of advs where PARAM matches the current page_by value
#' # group_label = the page_by label string (unused here — counting logic
#' #               is the same for all parameters, only the data subset changes)
#' tbl_vs |>
#'   fr_table() |>
#'   fr_rows(page_by = "param") |>
#'   fr_header(
#'     n = function(group_data, group_label) {
#'       d <- group_data
#'       c(placebo   = length(unique(d$USUBJID[d$TRTA == "Placebo"])),
#'         zom_50mg  = length(unique(d$USUBJID[d$TRTA == "Zomerane 50mg"])),
#'         zom_100mg = length(unique(d$USUBJID[d$TRTA == "Zomerane 100mg"])),
#'         total     = length(unique(d$USUBJID)))
#'     },
#'     n_data = advs,
#'     format = "{name}\n(N={n})"
#'   )
#'
#' ## ── Function N-count using group_label for conditional logic ──────────────
#'
#' # group_label is useful when counting logic varies by parameter.
#' # Here, Temperature uses a stricter filter (non-missing + in-range).
#' tbl_vs |>
#'   fr_table() |>
#'   fr_rows(page_by = "param") |>
#'   fr_header(
#'     n = function(group_data, group_label) {
#'       d <- group_data
#'       if (group_label == "Temperature (C)") {
#'         # Stricter: only subjects with in-range temperature
#'         d <- d[d$AVAL >= 35 & d$AVAL <= 42, ]
#'       }
#'       c(placebo   = length(unique(d$USUBJID[d$TRTA == "Placebo"])),
#'         zom_50mg  = length(unique(d$USUBJID[d$TRTA == "Zomerane 50mg"])),
#'         zom_100mg = length(unique(d$USUBJID[d$TRTA == "Zomerane 100mg"])),
#'         total     = length(unique(d$USUBJID)))
#'     },
#'     n_data = advs,
#'     format = "{name}\n(N={n})"
#'   )
#'
#' ## ── Header background colour (hex or CSS named colour) ──────────────────
#'
#' # Hex colour
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(bg = "#E0E0E0", bold = TRUE)
#'
#' # CSS named colour (148 available: lavender, aliceblue, gainsboro, etc.)
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(bg = "lavender", fg = "midnightblue", bold = TRUE)
#'
#' ## ── Order-independent: fr_header first, then fr_cols ─────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(n = c(zom_50mg = 45), format = "{name}\n(N={n})") |>
#'   fr_cols(zom_50mg = fr_col("Zom 50mg", align = "right"))
#'
#' @seealso [fr_cols()] for column structure, [fr_spans()] for spanning
#'   headers, [fr_col()] for per-column `header_align` overrides,
#'   [fr_style()] with `region = "header"` for cell-level overrides,
#'   [fr_config()] for setting header defaults via `_tlframe.yml`.
#'
#' @export
fr_header <- function(spec, align = NULL, valign = NULL,
                      n = NULL, format = NULL,
                      n_subject = NULL, n_data = NULL,
                      bold = NULL, bg = NULL, fg = NULL,
                      font_size = NULL, repeat_on_page = NULL,
                      span_gap = NULL) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  if (!is.null(align))  align  <- match_arg_fr(align,  fr_env$valid_aligns,  call = call)
  if (!is.null(valign)) valign <- match_arg_fr(valign, fr_env$valid_valigns, call = call)
  if (!is.null(bold))   check_scalar_lgl(bold, arg = "bold", call = call)
  if (!is.null(font_size)) check_positive_num(font_size, arg = "font_size", call = call)
  if (!is.null(bg)) bg <- resolve_color(bg, call = call)
  if (!is.null(fg)) fg <- resolve_color(fg, call = call)
  if (!is.null(repeat_on_page)) check_scalar_lgl(repeat_on_page, arg = "repeat_on_page", call = call)
  if (!is.null(span_gap)) check_scalar_lgl(span_gap, arg = "span_gap", call = call)

  # Validate N-count parameters
  validate_n_param(n = n, n_subject = n_subject, n_data = n_data,
                    format = format, call = call)

  # Preserve existing spans — fr_header replaces everything else
  old_spans <- spec$header$spans

  spec$header <- new_fr_header(
    spans          = old_spans,
    repeat_on_page = repeat_on_page %||% spec$header$repeat_on_page,
    valign         = valign    %||% spec$header$valign,
    align          = align     %||% spec$header$align,
    bold           = bold      %||% spec$header$bold,
    bg             = bg        %||% spec$header$bg,
    fg             = fg        %||% spec$header$fg,
    font_size      = font_size %||% spec$header$font_size,
    n              = n         %||% spec$header$n,
    format         = format    %||% spec$header$format,
    n_subject      = n_subject %||% spec$header$n_subject,
    n_data         = n_data    %||% spec$header$n_data,
    span_gap       = span_gap  %||% spec$header$span_gap
  )

  spec
}
