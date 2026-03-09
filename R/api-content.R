# ──────────────────────────────────────────────────────────────────────────────
# api-content.R — Content metadata verbs: fr_titles, fr_footnotes
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# fr_titles — Set table titles
# ══════════════════════════════════════════════════════════════════════════════

#' Set Table Titles
#'
#' @description
#'
#' Sets one or more title lines displayed above the table body. Each argument
#' in `...` produces one title line in the rendered output. Calling
#' `fr_titles()` again **replaces** all previously set titles.
#'
#' Titles support inline `{fr_*()}` markup — see [fr_bold()], [fr_italic()],
#' [fr_super()], [fr_unicode()].
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param ... One or more title lines. Each may be:
#'   * A **character scalar** — plain text, optionally with `{fr_*()}` markup.
#'   * A **list**. The first element is the text content (can be unnamed).
#'     Optionally include `align`, `bold`, `font_size` to override per-line styling.
#' @param .align Default alignment for all title lines. One of `"left"`,
#'   `"center"` (default), or `"right"`. Can be overridden per line via the
#'   list form.
#' @param .bold Default bold for all title lines. `FALSE` by default.
#' @param .font_size Default font size in points. `NULL` inherits the page
#'   font size set by [fr_page()].
#'
#' @return A modified `fr_spec`. Titles are stored in `spec$meta$titles`.
#'
#' @section Regulatory conventions:
#' ICH E3 §10 specifies that each table should carry:
#' * **Line 1** — Table number and title (e.g. `"Table 14.1.1 Summary of
#'   Demographics and Baseline Characteristics"`).
#' * **Line 2** — Population qualifier (e.g. `"Full Analysis Set"` or
#'   `"Safety Population"`).
#' * **Line 3** — Study identifier or further qualifier (optional, e.g.
#'   `"Study TFRM-2024-001"` or `"Database Cutoff: 31DEC2024"`).
#'
#' **Standard table numbering scheme (ICH E3 / FDA/EMA submissions):**
#' | Section | Content |
#' |---------|---------|
#' | 14.1.x | Demographics, disposition, baseline characteristics |
#' | 14.2.x | Efficacy results (including PK/PD) |
#' | 14.3.1.x | Adverse events & dosing/exposure |
#' | 14.3.2.x | Deaths, serious & significant adverse events |
#' | 14.3.4.x | Abnormal laboratory values |
#' | 14.3.5.x | Vital signs, ECG, physical findings |
#' | 16.2.x | Subject data listings |
#'
#' In TFL outputs, titles are typically **left-aligned**, 9 pt, and
#' not bold. Use `"center"` alignment for PDF or journal output.
#'
#' @section Spacing:
#' By default, **one blank line** is inserted after the last title and before
#' the column header. Control this with [fr_spacing()]:
#' ```r
#' spec |> fr_spacing(titles_after = 0L)   # no gap
#' spec |> fr_spacing(titles_after = 2L)   # two blank lines
#' ```
#' This can also be set in `_tlframe.yml`:
#' ```yaml
#' spacing:
#'   titles_after: 1
#' ```
#'
#' @section Tips:
#' * Pass 0 arguments (`fr_titles(spec)`) to clear all titles.
#' * Markup in titles: use `"{fr_bold('Table 14.1.1')}"` to bold just the
#'   table number. The glue `{...}` syntax is evaluated at spec-build time.
#' * `fr_titles()` always **replaces** — chain multiple calls if you want to
#'   progressively build a pipeline, but only the last call takes effect.
#'
#' @examples
#' ## ── Standard two-line ICH E3 title ──────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles(
#'     "Table 14.1.1 Summary of Demographics and Baseline Characteristics",
#'     "Full Analysis Set"
#'   )
#'
#' ## ── Three-line title with study identifier ───────────────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_titles(
#'     "Table 14.3.2 Adverse Events by System Organ Class and Preferred Term",
#'     "Safety Analysis Set",
#'     "Study TFRM-2024-001"
#'   )
#'
#' ## ── Inline markup in a title ─────────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles(
#'     "{fr_bold('Table 14.1.1')} Summary of Demographics",
#'     "Full Analysis Set"
#'   )
#'
#' ## ── Per-line styling with list form ──────────────────────────────────────
#'
#' # List form for per-line styling overrides
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles(
#'     list("Table 14.1.1 Summary of Demographics", bold = TRUE, font_size = 10),
#'     list("Full Analysis Set", align = "left", font_size = 9)
#'   )
#'
#' ## ── Left-aligned titles (common in pharma RTF) ───────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles(
#'     "Table 14.1.1 Summary of Demographics",
#'     "Full Analysis Set",
#'     .align = "left"
#'   )
#'
#' ## ── Clear all titles ─────────────────────────────────────────────────────
#'
#' spec <- tbl_demog |> fr_table() |> fr_titles("Old Title")
#' spec <- spec |> fr_titles()  # removes all titles
#'
#' @seealso [fr_footnotes()] for footnotes below the table,
#'   [fr_pagehead()] for running page headers, [fr_spacing()] for gap
#'   control, [fr_bold()] and [fr_italic()] for inline markup.
#'
#' @export
fr_titles <- function(spec, ..., .align = "center", .bold = FALSE,
                      .font_size = NULL) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  .align <- match_arg_fr(.align, c("left", "center", "right"), call = call)

  dots <- list(...)
  if (length(dots) == 0L) {
    spec$meta$titles <- list()
    return(spec)
  }

  entries <- lapply(dots, function(x) {
    if (is.character(x)) {
      check_scalar_chr(x, arg = "title", call = call)
      new_title_entry(
        content   = normalise_text(x, env = call),
        align     = .align,
        bold      = .bold,
        font_size = .font_size
      )
    } else if (is.list(x)) {
      content <- x[["content"]] %||% x[[1L]]
      check_scalar_chr(content, arg = "title content", call = call)
      new_title_entry(
        content   = normalise_text(content, env = call),
        align     = x[["align"]]     %||% .align,
        bold      = x[["bold"]]      %||% .bold,
        font_size = x[["font_size"]] %||% .font_size
      )
    } else {
      cli_abort(c("Each title must be a character scalar or a named list.",
                  "x" = "You supplied {.obj_type_friendly {x}}."),
                call = call)
    }
  })

  spec$meta$titles <- entries
  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_footnotes — Set table footnotes
# ══════════════════════════════════════════════════════════════════════════════

#' Set Table Footnotes
#'
#' @description
#'
#' Sets one or more footnote lines displayed below the table body. Each
#' argument in `...` produces one footnote line. Calling `fr_footnotes()`
#' again **replaces** all previously set footnotes.
#'
#' Footnotes support inline `{fr_*()}` markup, including [fr_super()] for
#' superscript footnote labels.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param ... One or more footnotes. Each may be:
#'   * A **character scalar** — the footnote text.
#'   * A **list**. The first element is the footnote text (can be unnamed).
#'     Optionally include `align`, `placement`, `font_size` to override formatting.
#' @param .align Default alignment. `"left"` (default) is standard for
#'   regulatory submissions. Override per line via the list form.
#' @param .placement When to print footnotes. `"every"` (default) repeats
#'   footnotes on every page; `"last"` prints them only on the final page.
#'   Override per footnote via the list form.
#' @param .font_size Default font size in points. `NULL` inherits from page.
#' @param .separator Logical. Whether to draw a horizontal separator rule
#'   above the footnote block. Default `FALSE`.
#'
#' @return A modified `fr_spec`. Footnotes are stored in `spec$meta$footnotes`.
#'
#' @section Regulatory conventions:
#' ICH E3 and common pharma submission standards specify:
#'
#' **Labelling:**
#' * Footnote labels use lower-case letters in square brackets: `[a]`, `[b]`,
#'   `[c]`. Do **not** use numeric labels — `[1]`, `[2]` are reserved for
#'   literature references.
#' * Do **not** begin the footnote section with the word "Note" or "Notes:".
#'
#' **Layout:**
#' * Each footnote on its own line, left-aligned, same or smaller font than
#'   the table body.
#' * RTF/SAS submissions: limit to **8 footnotes** per table for full
#'   compatibility with standard TLG macro footnote slots.
#'
#' **Ordering:**
#' * `[a]`, `[b]`, … content footnotes first (in the order symbols appear
#'   reading left-to-right, top-to-bottom through the table).
#' * Abbreviation expansion line last (e.g. `"AE = Adverse Event; FAS = Full
#'   Analysis Set; SD = Standard Deviation."`).
#' * Source / data-cut note on the **final page only**
#'   (`placement = "last"`).
#'
#' **Common regulatory footnote patterns:**
#' * *Percentage denominator*: `"[a] Percentages are based on N in the
#'   column header."` — required whenever `n (%)` columns appear.
#' * *MedDRA coding*: `"[a] Coded using MedDRA Version XX.X."` — for AE,
#'   medical history, and prior/concomitant medication tables.
#' * *Subject counting*: `"[a] A subject is counted only once for multiple
#'   events within the same System Organ Class or Preferred Term."` — AE
#'   tables where events > subjects is possible.
#' * *Dagger symbol*: use `{fr_dagger()}` and `{fr_ddagger()}` markup for
#'   treatment-related or serious AE markers.
#'
#' @section Spacing:
#' By default, **one blank line** is inserted between the last body row and
#' the first footnote. Control this with [fr_spacing()]:
#' ```r
#' spec |> fr_spacing(footnotes_before = 0L)   # no gap
#' spec |> fr_spacing(footnotes_before = 2L)   # two blank lines
#' ```
#' This can also be set in `_tlframe.yml`:
#' ```yaml
#' spacing:
#'   footnotes_before: 1
#' ```
#'
#' @section Tips:
#' * Use `fr_super()` for superscript footnote markers inside cell values:
#'   `"{fr_super('a')} Value with footnote"`. The matching footnote line
#'   should begin with `"[a] ..."`.
#' * Set `.separator = FALSE` when the table ends with a bottom rule that
#'   already visually separates the footnotes.
#' * `placement = "last"` for the source note prevents it repeating on every
#'   continuation page. **Note:** this only works in PDF output. RTF has no
#'   mechanism for last-page-only footer content (`{\footer}` repeats on
#'   every page). In RTF, all footnotes appear on every page regardless of
#'   placement. A warning is emitted when rendering to RTF with
#'   `placement = "last"` footnotes.
#'
#' @examples
#' ## ── Demographics table: standard footnote set ─────────────────────────────
#'
#' # Correct order: [a][b] content footnotes, abbreviations last, source final page
#' tbl_demog |>
#'   fr_table() |>
#'   fr_footnotes(
#'     "[a] Percentages are based on N in the column header.",
#'     "[b] P-value from two-sample t-test (continuous) or chi-squared (categorical).",
#'     "FAS = Full Analysis Set; SD = Standard Deviation.",
#'     list("Source: ADSL; data cut 31DEC2024.", placement = "last")
#'   )
#'
#' ## ── AE table: MedDRA + subject-counting + source footnotes ───────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_footnotes(
#'     "[a] Coded using MedDRA Version 27.1.",
#'     paste0("[b] A subject is counted only once for multiple events within ",
#'            "the same System Organ Class or Preferred Term."),
#'     "[c] Percentages are based on N in the column header.",
#'     paste0("AE = Adverse Event; PT = Preferred Term; ",
#'            "SOC = System Organ Class; TEAE = Treatment-Emergent AE."),
#'     list("Source: ADAE; data cut 31DEC2024.", placement = "last")
#'   )
#'
#' ## ── Source note on last page only ────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_footnotes(
#'     "[a] Percentages based on N in column header.",
#'     list("Source: ADSL; data cut 31DEC2024.", placement = "last")
#'   )
#'
#' ## ── Inline markup: dagger and double-dagger symbols ──────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_footnotes(
#'     "{fr_dagger()} Treatment-related adverse events.",
#'     "{fr_ddagger()} Serious adverse events."
#'   )
#'
#' ## ── Per-line font size override ───────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_footnotes(
#'     "[a] Percentages based on N in column header.",
#'     list("Source: ADSL.", font_size = 7)
#'   )
#'
#' ## ── No separator rule ────────────────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_hlines("hsides") |>
#'   fr_footnotes(
#'     "[a] Percentages based on N in column header.",
#'     .separator = FALSE
#'   )
#'
#' ## ── Right-aligned footnotes ───────────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_footnotes(
#'     "[a] Percentages based on N in column header.",
#'     .align = "right"
#'   )
#'
#' ## ── Clear all footnotes ───────────────────────────────────────────────────
#'
#' spec <- tbl_demog |> fr_table() |> fr_footnotes("[a] Old footnote.")
#' spec <- spec |> fr_footnotes()  # removes all footnotes
#'
#' @seealso [fr_titles()] for titles above the table,
#'   [fr_pagefoot()] for running page footers, [fr_spacing()] for gap
#'   control, [fr_dagger()], [fr_ddagger()], [fr_super()] for inline
#'   markup symbols.
#'
#' @export
fr_footnotes <- function(spec, ..., .align = "left", .placement = "every",
                         .font_size = NULL, .separator = FALSE) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  .align     <- match_arg_fr(.align,     c("left", "center", "right"), call = call)
  .placement <- match_arg_fr(.placement, c("every", "last"),           call = call)
  check_scalar_lgl(.separator, arg = ".separator", call = call)

  dots <- list(...)
  if (length(dots) == 0L) {
    spec$meta$footnotes         <- list()
    spec$meta$footnote_separator <- .separator
    return(spec)
  }

  entries <- lapply(dots, function(x) {
    if (is.character(x)) {
      check_scalar_chr(x, arg = "footnote", call = call)
      new_footnote_entry(
        content   = normalise_text(x, env = call),
        align     = .align,
        placement = .placement,
        font_size = .font_size
      )
    } else if (is.list(x)) {
      content <- x[["content"]] %||% x[[1L]]
      check_scalar_chr(content, arg = "footnote content", call = call)
      new_footnote_entry(
        content   = normalise_text(content, env = call),
        align     = x[["align"]]     %||% .align,
        placement = x[["placement"]] %||% .placement,
        font_size = x[["font_size"]] %||% .font_size
      )
    } else {
      cli_abort(c("Each footnote must be a character scalar or a named list.",
                  "x" = "You supplied {.obj_type_friendly {x}}."),
                call = call)
    }
  })

  spec$meta$footnotes          <- entries
  spec$meta$footnote_separator <- .separator
  spec
}
