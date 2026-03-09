# ──────────────────────────────────────────────────────────────────────────────
# api-rows.R — Row grouping and pagination verb: fr_rows
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# fr_rows — Row grouping and pagination control
# ══════════════════════════════════════════════════════════════════════════════

#' Configure Row Grouping and Pagination
#'
#' @description
#'
#' Controls how data rows are grouped, paginated, indented, and spaced.
#' Calling `fr_rows()` again **replaces** the previous row configuration.
#'
#' tlframe uses a two-level greedy pagination algorithm (identical for RTF and
#' PDF output):
#' * **`page_by`** forces a page break whenever the named column(s) change
#'   value.
#' * **`group_by`** keeps rows with the same value in the named column(s)
#'   on the same page via `\trkeep` (RTF). If a group is too large for a
#'   single page, it breaks normally and a "(continued)" header row repeats
#'   at the top of the next page.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param page_by Character vector of column name(s). A new page begins
#'   whenever any of these columns change value. The column value is rendered
#'   as a group label above the column headers, and the column is
#'   **automatically hidden** from the table body at render time. Use for
#'   multi-SOC tables that break by System Organ Class.
#' @param group_by Character vector of column name(s). Rows sharing the same
#'   value are visually grouped (indented detail rows, blank-after spacing)
#'   **and** kept together on the same page when possible. When a group spans
#'   pages, a "(continued)" header row repeats at the top of the next page.
#' @param indent_by Character vector of column name(s). Rows are indented
#'   relative to their parent group level, creating a hierarchy. Typically
#'   used for PT rows under SOC.
#' @param blank_after Character vector of column name(s). A blank row is
#'   inserted after each group boundary in these columns.
#' @param page_by_bold Logical. Whether the page-by label is rendered in
#'   bold. Default `FALSE`. Set `TRUE` to make the label stand out above
#'   the column headers.
#' @param page_by_align Horizontal alignment for the page-by label. One of
#'   `"left"` (default), `"center"`, `"right"`, or `"decimal"`.
#'
#' @return A modified `fr_spec`. Row config stored in `spec$body`.
#'
#' @section Regulatory conventions — AE tables:
#' Standard pharma AE tables (MedDRA SOC/PT hierarchy) follow these row
#' conventions:
#' * **Sort order**: SOC rows sorted by **descending incidence** across all
#'   arms; within each SOC, PT rows sorted by descending incidence
#'   (alphabetical as tiebreaker). This is the standard regulatory sort order.
#' * **"TOTAL SUBJECTS WITH AN EVENT"** is always the first summary row,
#'   above the first SOC. Pre-compute this in your data frame.
#' * **Subject counting**: a subject is counted once per SOC and once per
#'   PT, even with multiple events. Document this in `fr_footnotes()`.
#' * **Page breaks by SOC**: each System Organ Class starts a new page
#'   (`page_by = "soc"`). PT rows stay with their SOC header via `group_by`.
#' * **Indentation**: PT rows are indented under the SOC header
#'   (`indent_by = "pt_label"`). Indent: 0.1667 in (2 spaces / 240 twips).
#'
#' @section Regulatory conventions — descriptive statistics tables:
#' For demographics and continuous endpoint tables, the standard summary
#' statistic row order is:
#' `n, Mean, SD (or SE), Median, Q1/Q3, Min/Max`
#' Pre-sort your long-form data to match this order. Use `blank_after` to
#' insert visual separation between characteristic blocks (e.g. between Age
#' and Sex sections).
#'
#' @section Orphan and widow control:
#' The pagination engine respects the `orphan_min` and `widow_min` settings
#' in [fr_page()]. Orphan control prevents a lone row being stranded at the
#' bottom of a page; widow control prevents a lone row starting a new page.
#' These apply within `group_by` groups: if placing a group would leave
#' fewer than `orphan_min` rows before the page break, the group is moved to
#' the next page instead.
#'
#' @section Tips:
#' * `page_by` columns are **automatically hidden** at render time — their
#'   values appear as group labels in the section header, so they don't need
#'   to appear in the table body. Override with
#'   `fr_cols(col = fr_col(visible = TRUE))` if needed (set *after*
#'   `fr_rows()`).
#' * `group_by`, `indent_by`, etc. columns are structural — hide them via
#'   `fr_cols(col = fr_col(visible = FALSE))` if they shouldn't appear.
#' * Use `group_by` (not `page_by`) when you want rows to travel together
#'   but not necessarily start on a new page.
#' * `blank_after` is the simplest way to add visual group separation without
#'   bold headers.
#'
#' @examples
#' ## ── AE table paginated by System Organ Class ─────────────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_rows(page_by = "soc")
#'
#' ## ── Demographics table: blank row after each group ────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_rows(blank_after = "group")
#'
#' ## ── Indented hierarchy (SOC bold header + indented PT rows) ──────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_rows(
#'     group_by  = "soc",
#'     indent_by = "pt"
#'   )
#'
#' ## ── Combined: page_by + group_by + indent ──────────────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_cols(soc = fr_col(visible = FALSE),
#'           row_type = fr_col(visible = FALSE)) |>
#'   fr_rows(
#'     page_by   = "soc",
#'     group_by  = "soc",
#'     indent_by = "pt"
#'   )
#'
#' ## ── sort_by: order a listing by subject and start date ────────────────
#'
#' adae[1:20, c("USUBJID", "AEBODSYS", "AEDECOD", "ASTDT", "AESEV")] |>
#'   fr_table() |>
#'   fr_rows(sort_by = c("USUBJID", "ASTDT"))
#'
#' ## ── repeat_cols: suppress repeated subject IDs in a listing ───────────
#'
#' adae[1:20, c("USUBJID", "AEBODSYS", "AEDECOD", "AESEV")] |>
#'   fr_table() |>
#'   fr_rows(
#'     sort_by     = c("USUBJID", "AEBODSYS"),
#'     repeat_cols = "USUBJID"
#'   )
#'
#' ## ── wrap = TRUE: enable text wrapping for long verbatim terms ─────────
#'
#' adae[1:10, c("USUBJID", "AEBODSYS", "AEDECOD", "AEOUT")] |>
#'   fr_table() |>
#'   fr_rows(wrap = TRUE)
#'
#' ## ── Combined: sort_by + repeat_cols on adverse event listing ──────────
#'
#' adae[1:30, c("USUBJID", "AEBODSYS", "AEDECOD", "AESEV", "ASTDT")] |>
#'   fr_table() |>
#'   fr_rows(
#'     sort_by     = c("USUBJID", "AEBODSYS", "AEDECOD"),
#'     repeat_cols = c("USUBJID", "AEBODSYS"),
#'     wrap        = TRUE
#'   )
#'
#' @seealso [fr_page()] to set `orphan_min` / `widow_min`,
#'   [fr_cols()] to hide structural columns from display.
#'
#' @param sort_by Character vector of column name(s). Sorts the data by these
#'   columns before rendering. For listings, this controls the display order
#'   (e.g., `sort_by = c("USUBJID", "ASTDT")`). Sorting is applied in
#'   `finalize_spec()`.
#' @param repeat_cols Character vector of column name(s). Suppresses repeated
#'   consecutive values in these columns — only the first occurrence in each
#'   run is displayed. Standard for listings where subject ID appears once per
#'   block. Suppression is applied in `finalize_spec()`.
#' @param wrap Logical. When `TRUE`, enables text wrapping in body cells.
#'   Default `FALSE`. For listings with long text fields (e.g., verbatim terms,
#'   medical history), wrapping prevents cell content from overflowing.
#'
#' @export
fr_rows <- function(spec, page_by = NULL,
                    group_by = NULL, indent_by = NULL, blank_after = NULL,
                    page_by_bold = FALSE, page_by_align = "left",
                    sort_by = NULL, repeat_cols = NULL, wrap = FALSE) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  validate_cols <- function(x, arg) {
    if (is.null(x)) return(character(0))
    if (!is.character(x)) {
      cli_abort(c("{.arg {arg}} must be a character vector of column names.",
                  "x" = "You supplied {.obj_type_friendly {x}}."),
                call = call)
    }
    bad <- setdiff(x, names(spec$data))
    if (length(bad) > 0L) {
      cli_abort("{.arg {arg}}: column{?s} not found in the data: {.val {bad}}.",
                call = call)
    }
    x
  }

  check_scalar_lgl(page_by_bold, arg = "page_by_bold", call = call)
  check_scalar_lgl(wrap, arg = "wrap", call = call)
  page_by_align <- match_arg_fr(page_by_align, fr_env$valid_aligns, call = call)

  spec$body <- new_fr_body(
    page_by       = validate_cols(page_by,       "page_by"),
    group_by      = validate_cols(group_by,      "group_by"),
    indent_by     = validate_cols(indent_by,     "indent_by"),
    blank_after   = validate_cols(blank_after,   "blank_after"),
    page_by_bold  = page_by_bold,
    page_by_align = page_by_align,
    sort_by       = validate_cols(sort_by,       "sort_by"),
    repeat_cols   = validate_cols(repeat_cols,   "repeat_cols"),
    wrap          = wrap
  )
  spec
}
