# ──────────────────────────────────────────────────────────────────────────────
# api-theme.R — Study-level theme management
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# fr_theme — Study-level defaults (ggplot2 theme pattern)
# ══════════════════════════════════════════════════════════════════════════════

#' Set or Update Study-Level Table Theme
#'
#' @description
#'
#' `fr_theme()` sets study-level defaults that are automatically applied to
#' **every** subsequent `fr_table()` call. Define font size, page layout,
#' tokens, running header/footer, and line rules once at the top of your
#' program; all tables inherit these settings without repeating them.
#'
#' @aliases fr_theme_set
#'
#' @details
#' This follows the same pattern as `ggplot2` theme management. Calling
#' `fr_theme(...)` (or its alias `fr_theme_set(...)`) **merges** new settings
#' into the current global theme without discarding keys you did not mention.
#' This is analogous to `ggplot2::theme_update()`.
#'
#' Use [fr_theme_reset()] to clear all settings and return to built-in
#' defaults, or [fr_theme_get()] to inspect the current state.
#'
#' @param orientation `"landscape"` or `"portrait"`. `NULL` leaves unchanged.
#' @param paper `"letter"`, `"a4"`, or `"legal"`. `NULL` leaves unchanged.
#' @param margins Margin(s) in inches (same formats as [fr_page()]). `NULL`
#'   leaves unchanged.
#' @param col_gap Inter-column padding in **points** (integer). `NULL` leaves
#'   unchanged. See [fr_page()] for details.
#' @param font_family Font family name. `NULL` leaves unchanged.
#' @param font_size Font size in points. `NULL` leaves unchanged.
#' @param spaces How to handle leading spaces in cell data. One of
#'   `"indent"` (convert to paragraph-level indent) or `"preserve"`
#'   (keep literal spaces). `NULL` leaves unchanged. See [fr_cols()]
#'   `.spaces` parameter for details.
#' @param split Logical. `TRUE` to enable column splitting for wide tables,
#'   `FALSE` to disable. `NULL` leaves unchanged. See [fr_cols()] `.split`
#'   parameter for details.
#' @param stub Character vector of column names to mark as stub columns
#'   (repeated in every panel during column splitting). `NULL` leaves
#'   unchanged. See [fr_col()] `stub` parameter for details.
#' @param pagehead Named list with `left`, `center`, `right`, `font_size`,
#'   `bold` elements — same as [fr_pagehead()] arguments. `NULL` leaves
#'   unchanged.
#' @param pagefoot Named list with `left`, `center`, `right`, `font_size`,
#'   `bold` elements — same as [fr_pagefoot()] arguments. `NULL` leaves
#'   unchanged.
#' @param tokens Named list of `{token}` values. `NULL` leaves unchanged.
#' @param hlines Horizontal rule preset string (e.g. `"header"`). `NULL`
#'   leaves unchanged.
#' @param vlines Vertical rule preset string (e.g. `"box"`). `NULL` leaves
#'   unchanged.
#' @param spacing Named list with `titles_after`, `footnotes_before`,
#'   `pagehead_after`, `pagefoot_before`, `page_by_after` (integer
#'   blank lines). `NULL` leaves unchanged. See [fr_spacing()].
#' @param n_format A [glue][glue::glue]-style format string for N-count
#'   labels. Applied to all tables that use `fr_col(n = ...)` or
#'   `fr_cols(.n = ...)` without an explicit `.n_format`. Available
#'   tokens: `{label}` and `{n}`. `NULL` leaves unchanged.
#' @param continuation Character scalar appended to column headers on
#'   continuation pages (e.g. `"(continued)"`). `NULL` leaves unchanged.
#'   See [fr_page()] for details.
#' @param page_by_bold Logical. Whether `page_by` group labels are bold.
#'   `NULL` leaves unchanged. See [fr_rows()] for details.
#' @param page_by_align Alignment for `page_by` group labels. One of
#'   `"left"`, `"center"`, `"right"`, or `"decimal"`. `NULL` leaves unchanged.
#'   See [fr_rows()] for details.
#' @param page_by_visible Logical. Whether `page_by` group labels are
#'   displayed. `NULL` leaves unchanged. See [fr_rows()] for details.
#' @param group_bold Logical. Whether group header rows (from `group_label`)
#'   are bold. `NULL` leaves unchanged. See [fr_rows()] for details.
#' @param group_keep Logical. Whether `group_by` groups are kept together
#'   on the same page. `NULL` leaves unchanged. See [fr_rows()] for details.
#' @param header Named list of header defaults. Supports all [fr_header()]
#'   parameters: `bold`, `align`, `valign`, `bg`, `fg`, `font_size`,
#'   `repeat_on_page`, plus `span_gap` (logical, insert gap columns between
#'   adjacent spans, default `TRUE`). `NULL` leaves unchanged.
#' @param footnote_separator Logical. Whether to draw a separator above the
#'   footnote block. `NULL` leaves unchanged.
#'
#' @return Invisibly `NULL`.
#'
#' @section How the theme is applied:
#' When `fr_table()` is called, it automatically applies the stored theme to
#' the new `fr_spec`. You can still override any setting per-table by calling
#' the corresponding verb after `fr_table()`. Per-table overrides always win.
#'
#' Theme settings are stored in the package's internal environment and persist
#' for the duration of the R session. Call [fr_theme_reset()] to clear all
#' settings and [fr_theme_get()] to inspect the current theme.
#'
#' @examples
#' ## ── Define study theme at the top of your program ────────────────────────
#'
#' fr_theme(
#'   orientation = "landscape",
#'   paper       = "letter",
#'   font_size   = 9,
#'   tokens      = list(study = "TFRM-2024-001", cutoff = "31DEC2024"),
#'   pagehead    = list(
#'     left  = "Study: {study}",
#'     right = "Database Cutoff: {cutoff}"
#'   ),
#'   pagefoot    = list(
#'     left  = "{program}",
#'     right = "{datetime}"
#'   ),
#'   hlines = "header",
#'   vlines = "box"
#' )
#'
#' # All subsequent fr_table() calls inherit the theme — no need to repeat
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles("Table 14.1.1 Demographics", "Full Analysis Set")
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_titles("Table 14.3.2 Adverse Events", "Safety Analysis Set")
#'
#' ## ── Set section spacing (blank lines between table parts) ─────────────────
#'
#' fr_theme(spacing = list(titles_after = 1L, footnotes_before = 1L))
#'
#' ## ── Merge additional settings (does not discard existing ones) ────────────
#'
#' fr_theme(font_size = 9, hlines = "header")
#' fr_theme(vlines = "box")          # adds vlines; font_size and hlines kept
#' fr_theme_get()                    # inspect
#'
#' ## ── Override a theme setting for one specific table ───────────────────────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_page(orientation = "portrait")   # overrides the landscape theme default
#'
#' ## ── Theme with spacing configuration ──────────────────────────────────────
#'
#' fr_theme_reset()
#' fr_theme(
#'   font_size = 9,
#'   spacing   = list(titles_after = 2L, footnotes_before = 2L,
#'                    pagehead_after = 1L)
#' )
#' fr_theme_get()$spacing   # check the spacing settings
#'
#' ## ── Theme with leading-space handling ────────────────────────────────────
#'
#' fr_theme_reset()
#' fr_theme(spaces = "indent")   # default: leading spaces → paragraph indent
#' fr_theme_get()$spaces         # "indent"
#'
#' fr_theme(spaces = "preserve") # keep leading spaces as literal characters
#' fr_theme_get()$spaces         # "preserve"
#'
#' ## ── Theme with footnote_separator ────────────────────────────────────────
#'
#' fr_theme_reset()
#' fr_theme(footnote_separator = FALSE, hlines = "header")
#' fr_theme_get()$footnote_separator   # FALSE
#'
#' ## ── Theme auto-applied: inspect spec after fr_table() ────────────────────
#'
#' fr_theme_reset()
#' fr_theme(font_size = 8, orientation = "portrait", hlines = "header")
#' spec <- tbl_demog |> fr_table()
#' spec$page$font_size      # 8 (inherited from theme)
#' spec$page$orientation    # "portrait" (inherited from theme)
#'
#' ## ── Reset all theme settings ──────────────────────────────────────────────
#'
#' fr_theme_reset()
#'
#' @seealso [fr_theme_get()] to inspect, [fr_theme_reset()] to clear,
#'   [fr_page()] for per-table page layout, [fr_pagehead()] and [fr_pagefoot()]
#'   for running headers and footers, [fr_spacing()] for per-table gap control.
#'
#' @export
fr_theme <- function(
  orientation = NULL,
  paper = NULL,
  margins = NULL,
  col_gap = NULL,
  font_family = NULL,
  font_size = NULL,
  spaces = NULL,
  split = NULL,
  stub = NULL,
  pagehead = NULL,
  pagefoot = NULL,
  tokens = NULL,
  hlines = NULL,
  vlines = NULL,
  spacing = NULL,
  n_format = NULL,
  continuation = NULL,
  page_by_bold = NULL,
  page_by_align = NULL,
  page_by_visible = NULL,
  group_bold = NULL,
  group_keep = NULL,
  header = NULL,
  footnote_separator = NULL
) {
  call <- caller_env()

  if (!is.null(font_size)) {
    check_positive_num(font_size, arg = "font_size", call = call)
  }
  if (!is.null(col_gap)) {
    check_non_negative_int(col_gap, arg = "col_gap", call = call)
  }
  if (!is.null(footnote_separator)) {
    check_scalar_lgl(
      footnote_separator,
      arg = "footnote_separator",
      call = call
    )
  }
  if (!is.null(n_format)) {
    check_scalar_chr(n_format, arg = "n_format", call = call)
  }
  if (!is.null(spaces)) {
    spaces <- match_arg_fr(spaces, fr_env$valid_spaces, call = call)
  }
  if (!is.null(split)) {
    check_scalar_lgl(split, arg = "split", call = call)
  }
  if (!is.null(stub)) {
    if (!is.character(stub)) {
      cli_abort(
        c(
          "{.arg stub} must be a character vector of column names.",
          "x" = "You supplied {.obj_type_friendly {stub}}."
        ),
        call = call
      )
    }
  }
  if (!is.null(header)) {
    if (!is.list(header)) {
      cli_abort(
        c(
          "{.arg header} must be a list.",
          "x" = "You supplied {.obj_type_friendly {header}}."
        ),
        call = call
      )
    }
    if (!is.null(header$span_gap)) {
      check_scalar_lgl(header$span_gap, arg = "header$span_gap", call = call)
    }
  }
  if (!is.null(continuation)) {
    check_scalar_chr(continuation, arg = "continuation", call = call)
  }
  if (!is.null(page_by_bold)) {
    check_scalar_lgl(page_by_bold, arg = "page_by_bold", call = call)
  }
  if (!is.null(page_by_align)) {
    page_by_align <- match_arg_fr(
      page_by_align,
      fr_env$valid_aligns,
      call = call
    )
  }
  if (!is.null(page_by_visible)) {
    check_scalar_lgl(page_by_visible, arg = "page_by_visible", call = call)
  }
  if (!is.null(group_bold)) {
    check_scalar_lgl(group_bold, arg = "group_bold", call = call)
  }
  if (!is.null(group_keep)) {
    check_scalar_lgl(group_keep, arg = "group_keep", call = call)
  }
  if (!is.null(hlines)) {
    hlines <- match_arg_fr(hlines, names(fr_env$hline_presets), call = call)
  }
  if (!is.null(vlines)) {
    vlines <- match_arg_fr(
      vlines,
      c("box", "all", "inner", "void"),
      call = call
    )
  }

  theme <- fr_env$theme %||% list()

  set_if <- function(key, value) {
    if (!is.null(value)) theme[[key]] <<- value
  }

  set_if("orientation", orientation)
  set_if("paper", paper)
  set_if("margins", margins)
  set_if("col_gap", col_gap)
  set_if("spaces", spaces)
  set_if("split", split)
  set_if("stub", stub)
  set_if("font_family", font_family)
  set_if("font_size", font_size)
  set_if("pagehead", pagehead)
  set_if("pagefoot", pagefoot)
  set_if("tokens", tokens)
  set_if("hlines", hlines)
  set_if("vlines", vlines)
  set_if("spacing", spacing)
  set_if("header", header)
  set_if("n_format", n_format)
  set_if("continuation", continuation)
  set_if("page_by_bold", page_by_bold)
  set_if("page_by_align", page_by_align)
  set_if("page_by_visible", page_by_visible)
  set_if("group_bold", group_bold)
  set_if("group_keep", group_keep)
  set_if("footnote_separator", footnote_separator)

  fr_env$theme <- theme
  invisible(NULL)
}

#' @export
fr_theme_set <- fr_theme


#' Get the Current Study-Level Table Theme
#'
#' @description
#'
#' Returns the current study-level theme as a named list. Use this to inspect
#' what defaults have been set via [fr_theme()].
#'
#' @return A named list of current theme settings, or an empty list if no
#'   theme has been set. Possible keys (all optional):
#'   * `orientation` — `"landscape"` or `"portrait"`
#'   * `paper` — `"letter"`, `"a4"`, `"legal"`
#'   * `font_family` — e.g. `"Times New Roman"`
#'   * `font_size` — numeric (points)
#'   * `margins` — numeric vector (inches)
#'   * `tokens` — named list of token values
#'   * `pagehead` — list with `left`, `center`, `right`, `font_size`, `bold`
#'   * `pagefoot` — list with `left`, `center`, `right`, `font_size`, `bold`
#'   * `hlines` — preset name (e.g. `"header"`)
#'   * `vlines` — preset name (e.g. `"box"`)
#'   * `spacing` — list with `titles_after`, `footnotes_before`,
#'     `pagehead_after`, `pagefoot_before`, `page_by_after` (integer
#'     blank lines)
#'   * `col_gap` — integer (points)
#'   * `split` — logical (`TRUE`/`FALSE`) column splitting
#'   * `stub` — character vector (stub column names)
#'   * `page_by_bold` — logical; whether page-by labels are bold
#'   * `page_by_align` — character; alignment of page-by labels
#'   * `page_by_visible` — logical; whether page-by labels are displayed
#'   * `group_bold` — logical; whether group_label header rows are bold
#'   * `group_keep` — logical; whether group_by groups are kept together
#'   * `footnote_separator` — logical
#'
#' @examples
#' fr_theme(font_size = 9, hlines = "header", orientation = "landscape")
#' fr_theme_get()
#' # $font_size
#' # [1] 9
#' # $hlines
#' # [1] "header"
#' # $orientation
#' # [1] "landscape"
#'
#' # Programmatic access
#' theme <- fr_theme_get()
#' theme$font_size   # 9
#'
#' ## ── Empty state after reset ───────────────────────────────────────────────
#'
#' fr_theme_reset()
#' fr_theme_get()     # list() — no theme settings active
#' length(fr_theme_get()) == 0L   # TRUE
#'
#' @seealso [fr_theme()] to set, [fr_theme_reset()] to clear,
#'   [fr_config_get()] for YAML config inspection.
#'
#' @export
fr_theme_get <- function() {
  as.list(fr_env$theme %||% list())
}


#' Reset the Study-Level Table Theme
#'
#' @description
#'
#' Clears all study-level defaults previously set by [fr_theme()]. After
#' calling `fr_theme_reset()`, [fr_table()] returns to its built-in
#' defaults (or YAML config defaults if [fr_config()] was loaded).
#'
#' Call this between test runs, at the top of scripts, or when switching
#' between studies in the same R session.
#'
#' @return Invisibly `NULL`.
#'
#' @examples
#' ## ── Before / after effect ─────────────────────────────────────────────────
#'
#' fr_theme(font_size = 9, hlines = "header", orientation = "landscape")
#' fr_theme_get()$font_size        # 9
#' fr_theme_get()$hlines           # "header"
#'
#' fr_theme_reset()                # clear everything
#' fr_theme_get()                  # list() — empty
#' length(fr_theme_get()) == 0L    # TRUE
#'
#' ## ── Reset does not affect YAML config ─────────────────────────────────────
#'
#' # fr_theme_reset() only clears the session theme.
#' # To also clear YAML config, call fr_config_reset() separately.
#' fr_theme(font_size = 8)
#' fr_theme_reset()
#' fr_theme_get()   # list()
#'
#' @seealso [fr_theme()] to set, [fr_theme_get()] to inspect,
#'   [fr_config_reset()] to also clear YAML config.
#'
#' @export
fr_theme_reset <- function() {
  fr_env$theme <- list()
  invisible(NULL)
}
