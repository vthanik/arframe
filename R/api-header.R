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
#' @param align Horizontal alignment for header cells. Accepts two forms:
#'
#'   * **Scalar string** — `"left"`, `"center"`, `"right"`, or `NULL`
#'     (inherit from column). Applied to all header cells uniformly.
#'
#'   * **Named list + tidyselect** — per-column alignment using column
#'     names and tidyselect helpers. Names are alignment values, values
#'     are column selections:
#'     ```r
#'     fr_header(spec, align = list(
#'       center = c(starts_with("zom"), "placebo"),
#'       left   = "characteristic",
#'       right  = "total"
#'     ))
#'     ```
#'     Unmatched columns keep `fr_col(header_align = ...)` or fall back
#'     to the body `align` setting.
#'
#'   **Precedence** (highest wins):
#'   1. `fr_col(header_align = "right")` — per-column override
#'   2. `fr_header(align = list(...))` — tidyselect targeting
#'   3. `fr_header(align = "center")` — blanket scalar
#'   4. Column body `align` — inherited default
#'
#' @param valign Default vertical alignment. One of `"top"`, `"middle"`,
#'   `"bottom"` (default). Relevant when header rows have unequal height.
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
#' ## ── Tidyselect alignment (per-column targeting) ──────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_header(bold = TRUE, align = list(
#'     left   = "characteristic",
#'     center = c(starts_with("zom"), "placebo", "total")
#'   ))
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
#' @seealso [fr_cols()] for column structure and N-counts, [fr_spans()] for
#'   spanning headers, [fr_col()] for per-column `header_align` overrides,
#'   [fr_style()] with `region = "header"` for cell-level overrides,
#'   [fr_config()] for setting header defaults via `_tlframe.yml`.
#'
#' @export
fr_header <- function(spec, align = NULL, valign = NULL,
                      bold = NULL, bg = NULL, fg = NULL,
                      font_size = NULL, repeat_on_page = NULL) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  # ── Resolve align: scalar string or named list (tidyselect) ─────────────

  align_expr <- rlang::enexpr(align)
  align_map <- NULL

  if (!is.null(align_expr)) {
    # Detect named list form: align = list(center = ..., left = ...)
    # Must capture unevaluated to preserve tidyselect helpers like starts_with()
    is_list_call <- rlang::is_call(align_expr, "list")

    if (is_list_call) {
      # Extract the named elements from the unevaluated list(...) call
      align_args <- rlang::call_args(align_expr)
      align_names <- names(align_args)

      valid_aligns <- fr_env$valid_aligns
      bad_names <- setdiff(align_names, valid_aligns)
      if (length(bad_names) > 0L) {
        cli_abort(
          c("{.arg align} list names must be valid alignments: {.val {valid_aligns}}.",
            "x" = "Invalid name{?s}: {.val {bad_names}}."),
          call = call
        )
      }

      # Resolve each group via tidyselect
      align_map <- list()
      col_names <- names(spec$columns) %||% names(spec$data)
      col_proxy <- set_names(seq_along(col_names), col_names)
      for (i in seq_along(align_args)) {
        a <- align_names[[i]]
        sel_expr <- align_args[[i]]
        sel <- tryCatch(
          tidyselect::eval_select(sel_expr,
                                  data = col_proxy,
                                  error_call = call),
          error = function(e) {
            cli_abort(
              c("Failed to resolve {.arg align} tidyselect for {.val {a}}.",
                "x" = conditionMessage(e)),
              call = call
            )
          }
        )
        for (nm in names(sel)) {
          align_map[[nm]] <- a
        }
      }
      align <- NULL
    } else {
      # Scalar string form — evaluate the expression now
      align <- rlang::eval_tidy(align_expr)
      if (!is.null(align)) {
        align <- match_arg_fr(align, fr_env$valid_aligns, call = call)
      }
    }
  } else {
    align <- NULL
  }

  if (!is.null(valign)) valign <- match_arg_fr(valign, fr_env$valid_valigns, call = call)
  if (!is.null(bold))   check_scalar_lgl(bold, arg = "bold", call = call)
  if (!is.null(font_size)) check_positive_num(font_size, arg = "font_size", call = call)
  if (!is.null(bg)) bg <- resolve_color(bg, call = call)
  if (!is.null(fg)) fg <- resolve_color(fg, call = call)
  if (!is.null(repeat_on_page)) check_scalar_lgl(repeat_on_page, arg = "repeat_on_page", call = call)

  # Preserve existing spans — fr_header replaces everything else
  old_spans <- spec$header$spans

  spec$header <- new_fr_header(
    spans          = old_spans,
    repeat_on_page = repeat_on_page %||% spec$header$repeat_on_page,
    valign         = valign    %||% spec$header$valign,
    align          = align     %||% spec$header$align,
    align_map      = align_map %||% spec$header$align_map,
    bold           = bold      %||% spec$header$bold,
    bg             = bg        %||% spec$header$bg,
    fg             = fg        %||% spec$header$fg,
    font_size      = font_size %||% spec$header$font_size,
    span_gap       = spec$header$span_gap
  )

  spec
}
