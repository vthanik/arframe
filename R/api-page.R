# ──────────────────────────────────────────────────────────────────────────────
# api-page.R — Page layout verbs: fr_page, fr_pagehead, fr_pagefoot
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# fr_page — Set page layout options
# ══════════════════════════════════════════════════════════════════════════════

#' Set Page Layout Options
#'
#' @description
#'
#' Sets page-level properties: paper size, orientation, margins, font, and
#' pagination controls. Only the arguments you **explicitly supply** are
#' changed — all others retain their current values. This allows incremental
#' updates anywhere in a pipeline.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param orientation `"landscape"` (default, 11 × 8.5 in for Letter) or
#'   `"portrait"` (8.5 × 11 in).
#' @param paper `"letter"` (default, 8.5 × 11 in), `"a4"` (210 × 297 mm),
#'   or `"legal"` (8.5 × 14 in).
#' @param margins Margin(s) in inches. Accepts:
#'   * Scalar: `1` — all four sides equal.
#'   * Length 2: `c(vertical, horizontal)` — top/bottom vs left/right.
#'   * Length 4: `c(top, right, bottom, left)` — CSS order.
#'   * Named list: `list(top=1, bottom=1, left=0.75, right=0.75)`.
#' @param font_family Font family name. Defaults to the OS monospace font
#'   (`"Courier New"` on Windows/macOS, `"Latin Modern Mono"` on Linux).
#'   Regulatory submissions typically use Courier New 9pt.
#'   Set the `TLFRAME_FONT_DIR` environment variable to a directory
#'   containing `.ttf`/`.otf` files to make custom fonts available for
#'   PDF rendering without system-wide installation. Ideal for Docker,
#'   CI, or project-local fonts. See `vignette("automation")` for examples.
#' @param font_size Font size in points. Default `9`. Typical pharma range:
#'   7–10 pt.
#' @param orphan_min Minimum body rows to keep at the **bottom** of a page
#'   before forcing a page break. Default `3L`. Set to `1L` to disable.
#' @param widow_min Minimum body rows to carry to the **top** of the next page.
#'   Default `3L`. Set to `1L` to disable.
#' @param continuation Character scalar appended to the column header block on
#'   continuation pages, e.g. `"(continued)"`. `NULL` (default) disables.
#' @param col_gap Inter-column padding in **points** (total gap between
#'   adjacent columns). Default `4` (2 pt left + 2 pt right per cell).
#'   Increase to `6` or `8` if long text in adjacent columns runs together.
#'   Set to `0` for zero padding (cells flush, legacy behaviour).
#'   Applied symmetrically as half the value on each side of every cell
#'   in both RTF and PDF output.
#' @param tokens Named list of custom `{token}` values for use in page
#'   headers/footers. E.g. `list(study = "TFRM-2024-001", pop = "FAS")`.
#'   Use these tokens in [fr_pagehead()] and [fr_pagefoot()] strings.
#'
#' @return A modified `fr_spec`. Page settings stored in `spec$page`.
#'
#' @section Regulatory conventions:
#' **FDA/US submissions (eCTD):** landscape Letter (8.5 × 11 in),
#' Courier New 9 pt, 1 in margins on all sides. This is the most common
#' regulatory RTF standard and the package default.
#'
#' **EMA/European submissions:** A4 paper (210 × 297 mm) is acceptable;
#' landscape A4 is common for wide safety tables.
#'
#' **Database cutoff date** is a mandatory element in every CSR table.
#' The standard practice is to define it as a custom token:
#' ```r
#' fr_page(tokens = list(cutoff = "31DEC2024")) |>
#' fr_pagehead(left  = "Study: {study}",
#'             right = "Database Cutoff: {cutoff}")
#' ```
#'
#' **Continuation label:** multi-page tables must indicate continuation.
#' Regulatory convention is to append `"(continued)"` to the column header
#' block on all pages after the first:
#' ```r
#' fr_page(continuation = "(continued)")
#' ```
#'
#' @section Tips:
#' * `tokens` are the clean way to inject study-level metadata (study number,
#'   cutoff date, population) into running headers without repeating the
#'   value in every `fr_pagehead()` call.
#' * `orphan_min = 3` means: if fewer than 3 rows would remain at the bottom
#'   of a page before a `group_by` group, move the group to the next page.
#'
#' @examples
#' ## ── Standard pharma RTF setup ────────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_page(
#'     orientation = "landscape",
#'     paper       = "letter",
#'     font_family = "Courier New",
#'     font_size   = 9,
#'     margins     = 1
#'   )
#'
#' ## ── A4 portrait for European submissions ─────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_page(orientation = "portrait", paper = "a4", font_size = 9)
#'
#' ## ── Narrow margins to fit wide table ─────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_page(margins = c(1, 0.5))   # top/bottom = 1in, left/right = 0.5in
#'
#' ## ── Custom tokens for running header ─────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_page(tokens = list(study = "TFRM-2024-001", pop = "FAS")) |>
#'   fr_pagehead(left  = "Study: {study}  Population: {pop}",
#'               right = "Page {thepage} of {total_pages}")
#'
#' ## ── Continuation label on multi-page tables ──────────────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_page(continuation = "(continued)")
#'
#' ## ── Tight orphan/widow control ────────────────────────────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_page(orphan_min = 2L, widow_min = 2L)
#'
#' ## ── Wider inter-column padding ─────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_page(col_gap = 8)   # 4 pt each side (generous spacing)
#'
#' ## ── Legal paper size (8.5 x 14 in) ─────────────────────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_page(paper = "legal", orientation = "landscape", margins = 1)
#'
#' ## ── Custom fonts via TLFRAME_FONT_DIR (Docker/CI) ────────────────────
#'
#' # Set TLFRAME_FONT_DIR to a directory of .ttf/.otf files;
#' # XeLaTeX discovers them by name --- no system install needed.
#' # Sys.setenv(TLFRAME_FONT_DIR = "/path/to/fonts")
#'
#' @seealso [fr_pagehead()], [fr_pagefoot()] for running headers/footers,
#'   [fr_rows()] for `page_by` / `group_by` row pagination,
#'   [fr_cols()] with `.split` for column splitting.
#'
#' @export
fr_page <- function(spec, orientation = NULL, paper = NULL, margins = NULL,
                    font_family = NULL, font_size = NULL, orphan_min = NULL,
                    widow_min = NULL, continuation = NULL, col_gap = NULL,
                    tokens = NULL) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  old <- spec$page

  spec$page <- new_fr_page(
    orientation  = orientation  %||% old$orientation,
    paper        = paper        %||% old$paper,
    margins      = margins      %||% old$margins,
    font_family  = font_family  %||% old$font_family,
    font_size    = font_size    %||% old$font_size,
    orphan_min   = orphan_min   %||% old$orphan_min,
    widow_min    = widow_min    %||% old$widow_min,
    continuation = if (!is.null(continuation)) continuation else old$continuation,
    col_gap      = col_gap      %||% old$col_gap,
    tokens       = if (!is.null(tokens)) tokens else old$tokens,
    call         = call
  )
  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_pagehead / fr_pagefoot — Running page header and footer
# ══════════════════════════════════════════════════════════════════════════════

#' Set Running Page Header
#'
#' @description
#'
#' Sets the running header printed at the top of every page (or every page
#' after the first). The header is divided into three independent zones:
#' `left`, `center`, and `right`.
#'
#' Supports `{token}` placeholders, evaluated at render time:
#' * `{thepage}` — current page number.
#' * `{total_pages}` — total page count.
#' * Custom tokens defined via `fr_page(tokens = list(...))`.
#'
#' Calling `fr_pagehead()` again **replaces** the previous header.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param left Character scalar, character vector, or `NULL`. Left zone text.
#'   Vectors are collapsed with newlines to produce multi-line output.
#' @param center Character scalar, character vector, or `NULL`. Center zone text.
#' @param right Character scalar, character vector, or `NULL`. Right zone text.
#'   All three zones support token placeholders (e.g. \code{thepage})
#'   evaluated at render time.
#' @param font_size Font size for the header text in points. `NULL` inherits
#'   from the page font size (`fr_page(font_size = ...)`).
#' @param bold Logical. Whether header text is bold. Default `FALSE`.
#'
#' @return A modified `fr_spec`. Header stored in `spec$pagehead`.
#'
#' @section Regulatory conventions:
#' ICH E3 specifies a **3-part running header** on every table page:
#'
#' * **Left zone**: Study/protocol identifier.
#' * **Center zone**: Compound name, document type, or left blank.
#' * **Right zone**: `"Page X of Y"`.
#'
#' The **database cutoff date** is a mandatory audit trail element. Standard
#' table shells show it on the right of the running header on every page:
#' ```
#' Study/Protocol: TFRM-2024-001          Database Cutoff Date: 31DEC2024
#' ```
#' Implement this with:
#' ```r
#' fr_page(tokens = list(study = "TFRM-2024-001", cutoff = "31DEC2024")) |>
#' fr_pagehead(
#'   left  = "Study/Protocol: {study}",
#'   right = "Database Cutoff Date: {cutoff}"
#' )
#' ```
#' Date format in RTF submissions is `DDMONYYYY` (e.g. `"31DEC2024"`), not
#' ISO 8601, to match SAS ODS date conventions.
#'
#' @examples
#' ## ── Standard: Study/Protocol + Database Cutoff Date ──────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_page(tokens = list(study  = "TFRM-2024-001",
#'                         cutoff = "31DEC2024")) |>
#'   fr_pagehead(
#'     left  = "Study/Protocol: {study}",
#'     right = "Database Cutoff Date: {cutoff}"
#'   )
#'
#' ## ── 3-part header: study | CONFIDENTIAL | Page X of Y ────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_page(tokens = list(study = "TFRM-2024-001")) |>
#'   fr_pagehead(
#'     left   = "Study/Protocol: {study}",
#'     center = "CONFIDENTIAL",
#'     right  = "Page {thepage} of {total_pages}"
#'   )
#'
#' ## ── Multi-token header with compound and population ───────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_page(tokens = list(
#'     study    = "TFRM-2024-001",
#'     compound = "Zomerane",
#'     pop      = "FAS"
#'   )) |>
#'   fr_pagehead(
#'     left   = "{study}  {compound}",
#'     center = "Population: {pop}",
#'     right  = "Page {thepage} of {total_pages}"
#'   )
#'
#' ## ── Smaller header font size ──────────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_pagehead(
#'     left      = "TFRM-2024-001",
#'     right     = "Page {thepage} of {total_pages}",
#'     font_size = 7
#'   )
#'
#' ## ── Combined pagehead + pagefoot in one pipeline ──────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_page(tokens = list(study = "TFRM-2024-001", cutoff = "31DEC2024")) |>
#'   fr_pagehead(
#'     left  = "Study: {study}",
#'     right = "Cutoff: {cutoff}"
#'   ) |>
#'   fr_pagefoot(
#'     left  = "{program}",
#'     right = "{datetime}"
#'   )
#'
#' @section Spacing:
#' By default, **no gap** is inserted after the page header. Add one with
#' [fr_spacing()]:
#' ```r
#' spec |> fr_spacing(pagehead_after = 1L)   # one blank line
#' spec |> fr_spacing(pagehead_after = 2L)   # two blank lines
#' ```
#' This can also be set in `_tlframe.yml`:
#' ```yaml
#' spacing:
#'   pagehead_after: 1
#' ```
#'
#' @seealso [fr_pagefoot()] for the footer, [fr_page()] for custom token
#'   definitions, [fr_titles()] for above-table title lines, [fr_spacing()]
#'   for gap control.
#'
#' @export
fr_pagehead <- function(spec, left = NULL, center = NULL, right = NULL,
                        font_size = NULL, bold = FALSE) {
  call <- caller_env()
  check_fr_spec(spec, call = call)
  check_scalar_lgl(bold, arg = "bold", call = call)
  if (!is.null(font_size)) check_positive_num(font_size, arg = "font_size",
                                               call = call)
  # Collapse character vectors into newline-separated scalars
  left   <- collapse_chrome_text(left)
  center <- collapse_chrome_text(center)
  right  <- collapse_chrome_text(right)

  spec$pagehead <- new_fr_pagechrome(left      = left,
                                      center    = center,
                                      right     = right,
                                      font_size = font_size,
                                      bold      = bold)
  spec
}


#' Set Running Page Footer
#'
#' @description
#'
#' Sets the running footer printed at the bottom of every page. The footer
#' uses the same three-zone (`left`, `center`, `right`) system as [fr_pagehead()].
#'
#' Supports `{token}` placeholders, evaluated at render time:
#' * `{program}` — R script filename (from `rstudioapi` or `sys.call`).
#' * `{datetime}` — run datetime in `DDMONYYYY HH:MM:SS` format
#'   (e.g. `"05MAR2026 14:24:25"`).
#' * `{thepage}`, `{total_pages}`, and custom tokens are also supported.
#'
#' Calling `fr_pagefoot()` again **replaces** the previous footer.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param left Character scalar or `NULL`. Left zone text.
#' @param center Character scalar or `NULL`. Center zone text.
#' @param right Character scalar or `NULL`. Right zone text.
#'   All three zones support token placeholders evaluated at render time.
#' @param font_size Font size in points. `NULL` inherits from page.
#' @param bold Logical. Default `FALSE`.
#'
#' @return A modified `fr_spec`. Footer stored in `spec$pagefoot`.
#'
#' @section Regulatory conventions:
#' Standard pharma table shells specify the following footer content on every
#' table page:
#'
#' * **Left zone**: Program path or filename. Use the `{program}` token,
#'   which resolves to the R script filename at render time.
#' * **Center zone**: Confidentiality label if needed.
#' * **Right zone**: Data source name, data extract date, and run datetime.
#'
#' Full submission-standard footer:
#' ```r
#' fr_page(tokens = list(datasource = "ADSL", extract = "01JAN2025")) |>
#' fr_pagefoot(
#'   left  = "{program}",
#'   right = "Data Source: {datasource}  Extract: {extract}  Run: {datetime}"
#' )
#' ```
#'
#' The `{datetime}` token resolves to `DDMONYYYY HH:MM:SS` at render time
#' (e.g. `"05MAR2026 14:24:25"`).
#'
#' @examples
#' ## ── Program path + run datetime (most common footer pattern) ─────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_pagefoot(
#'     left  = "{program}",
#'     right = "{datetime}"
#'   )
#'
#' ## ── Full submission-standard footer: program + data source + runtime ──────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_page(tokens = list(datasource = "ADSL", extract = "01JAN2025")) |>
#'   fr_pagefoot(
#'     left  = "{program}",
#'     right = "Data Source: {datasource}  Extract: {extract}  Run: {datetime}"
#'   )
#'
#' ## ── Program + confidentiality + datetime (3-zone) ─────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_pagefoot(
#'     left   = "{program}",
#'     center = "CONFIDENTIAL",
#'     right  = "{datetime}"
#'   )
#'
#' ## ── Minimal: page number only in footer ───────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_pagefoot(right = "Page {thepage} of {total_pages}")
#'
#' ## ── Full page chrome: pagehead + pagefoot together ────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_page(tokens = list(study = "TFRM-2024-001", cutoff = "31DEC2024")) |>
#'   fr_pagehead(
#'     left  = "Study: {study}",
#'     right = "Cutoff: {cutoff}"
#'   ) |>
#'   fr_pagefoot(
#'     left  = "{program}",
#'     right = "{datetime}"
#'   )
#'
#' @seealso [fr_pagehead()] for the running header, [fr_page()] for custom
#'   token definitions.
#'
#' @export
fr_pagefoot <- function(spec, left = NULL, center = NULL, right = NULL,
                        font_size = NULL, bold = FALSE) {
  call <- caller_env()
  check_fr_spec(spec, call = call)
  check_scalar_lgl(bold, arg = "bold", call = call)
  if (!is.null(font_size)) check_positive_num(font_size, arg = "font_size",
                                               call = call)
  # Collapse character vectors into newline-separated scalars
  left   <- collapse_chrome_text(left)
  center <- collapse_chrome_text(center)
  right  <- collapse_chrome_text(right)

  spec$pagefoot <- new_fr_pagechrome(left      = left,
                                      center    = center,
                                      right     = right,
                                      font_size = font_size,
                                      bold      = bold)
  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# ── Helper: collapse character vectors for pagehead/pagefoot zones ───────────

#' Collapse multi-element character vectors to newline-separated scalar
#'
#' Allows `fr_pagehead(left = c("Line 1", "Line 2"))` by joining with `"\n"`.
#' Validates type and provides clear dplyr-style error on misuse.
#' @noRd
collapse_chrome_text <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is.null(x)) return(NULL)
  if (!is.character(x)) {
    cli_abort(
      c("{.arg {arg}} must be a character string or character vector.",
        "x" = "You supplied a {.cls {class(x)}} value.",
        "i" = "Example: {.code fr_pagehead({arg} = \"Study: TFRM-2024-001\")}"),
      call = call
    )
  }
  if (length(x) <= 1L) return(x)
  paste0(x, collapse = "\n")
}


# fr_spacing — Section spacing control
# ══════════════════════════════════════════════════════════════════════════════

#' Control Spacing Between Table Sections
#'
#' @description
#'
#' Sets the number of blank lines inserted between table sections. These gaps
#' provide visual separation between the running header, titles, group label,
#' column headers, body rows, footnotes, and running footer.
#'
#' The five controllable junctions in the table anatomy:
#'
#' ```
#' [Page Header]
#'   --- pagehead_after ---
#' Titles
#'   --- titles_after ---
#' Page-by Label
#'   --- page_by_after ---
#' Column Headers / Body Rows
#'   --- footnotes_before ---
#' Footnotes
#'   --- pagefoot_before ---
#' [Page Footer]
#' ```
#'
#' Calling `fr_spacing()` **replaces** only the specified values. Unspecified
#' arguments keep their current value.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param titles_after Integer. Blank lines between the last title and the
#'   column header rule (or page-by label if `page_by` is set). Default `1L`.
#'   Set to `0L` for no gap.
#' @param footnotes_before Integer. Blank lines between the last body row
#'   (or bottom rule) and the first footnote. Default `1L`. Set to `0L`
#'   for no gap.
#' @param pagehead_after Integer. Blank lines between the running page
#'   header and the first title. Default `0L` (no gap). Set to `1L` for
#'   one blank line. Only takes effect when a page header is set via
#'   [fr_pagehead()].
#' @param pagefoot_before Integer. Blank lines between the last footnote
#'   and the running page footer. Default `0L`. Set to `1L` or more to
#'   add breathing room above the footer. Only takes effect when a page
#'   footer is set via [fr_pagefoot()].
#' @param page_by_after Integer. Blank lines between the page-by label
#'   and the column header row. Default `1L`. Only takes effect when
#'   `page_by` is set via [fr_rows()]. Set to `0L` for no gap.
#'
#' @return A modified `fr_spec`. Spacing stored in `spec$spacing`.
#'
#' @section Pharma conventions:
#' Standard pharma submission tables (ICH E3, FDA/EMA) typically use one blank
#' line after titles and before footnotes. The package defaults reflect a
#' minimal layout:
#' * `titles_after = 1L` — one blank line after titles
#' * `footnotes_before = 1L` — one blank line before footnotes
#' * `pagehead_after = 0L` — no gap (user decides)
#' * `pagefoot_before = 0L` — no gap (user decides)
#' * `page_by_after = 1L` — one blank line after page-by label
#'
#' @section YAML configuration:
#' All five spacing values can be set in `_tlframe.yml`:
#' ```yaml
#' spacing:
#'   titles_after: 1
#'   footnotes_before: 1
#'   pagehead_after: 0
#'   pagefoot_before: 0
#'   page_by_after: 1
#' ```
#'
#' @examples
#' ## ── Default spacing (1 blank line at each gap) ───────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles("Table 14.1.1 Demographics", "Safety Population") |>
#'   fr_footnotes("[a] Percentages based on N in column header.")
#'
#' ## ── No gap after titles (compact layout) ─────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_spacing(titles_after = 0L) |>
#'   fr_titles("Table 14.1.1 Demographics")
#'
#' ## ── Two blank lines before footnotes ─────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_spacing(footnotes_before = 2L) |>
#'   fr_footnotes("[a] Percentages based on N in column header.")
#'
#' ## ── Space after group label (page_by) ────────────────────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_rows(page_by = "soc") |>
#'   fr_spacing(page_by_after = 1L)
#'
#' ## ── Space before page footer ──────────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_pagefoot(left = "{program}", right = "{datetime}") |>
#'   fr_spacing(pagefoot_before = 1L)
#'
#' ## ── All five spacing parameters in one call ──────────────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_titles("Table 14.3.1 Adverse Events by SOC") |>
#'   fr_footnotes("[a] MedDRA v26.1.") |>
#'   fr_rows(page_by = "soc") |>
#'   fr_pagehead(left = "TFRM-2024-001") |>
#'   fr_pagefoot(left = "{program}") |>
#'   fr_spacing(
#'     titles_after     = 1L,
#'     footnotes_before = 1L,
#'     pagehead_after   = 1L,
#'     pagefoot_before  = 1L,
#'     page_by_after    = 1L
#'   )
#'
#' ## ── Tight layout: zero spacing everywhere ────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles("Table 14.1.1 Demographics") |>
#'   fr_footnotes("[a] N = number of subjects.") |>
#'   fr_spacing(
#'     titles_after     = 0L,
#'     footnotes_before = 0L,
#'     pagehead_after   = 0L,
#'     pagefoot_before  = 0L,
#'     page_by_after    = 0L
#'   )
#'
#' @seealso [fr_titles()], [fr_footnotes()], [fr_pagehead()], [fr_pagefoot()],
#'   [fr_rows()].
#'
#' @export
fr_spacing <- function(spec, titles_after = NULL, footnotes_before = NULL,
                       pagehead_after = NULL, pagefoot_before = NULL,
                       page_by_after = NULL) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  if (!is.null(titles_after)) {
    spec$spacing$titles_after <- check_non_negative_int(titles_after,
                                                         arg = "titles_after",
                                                         call = call)
  }
  if (!is.null(footnotes_before)) {
    spec$spacing$footnotes_before <- check_non_negative_int(footnotes_before,
                                                             arg = "footnotes_before",
                                                             call = call)
  }
  if (!is.null(pagehead_after)) {
    spec$spacing$pagehead_after <- check_non_negative_int(pagehead_after,
                                                           arg = "pagehead_after",
                                                           call = call)
  }
  if (!is.null(pagefoot_before)) {
    spec$spacing$pagefoot_before <- check_non_negative_int(pagefoot_before,
                                                             arg = "pagefoot_before",
                                                             call = call)
  }
  if (!is.null(page_by_after)) {
    spec$spacing$page_by_after <- check_non_negative_int(page_by_after,
                                                           arg = "page_by_after",
                                                           call = call)
  }

  spec
}
