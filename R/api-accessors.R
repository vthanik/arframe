# ──────────────────────────────────────────────────────────────────────────────
# api-accessors.R — Stable public accessor API: fr_get_*() family
#
# These accessors provide a stable public interface to read spec properties
# without coupling user code to the internal list structure of fr_spec.
# All return deep copies so modifications don't affect the original spec.
# ──────────────────────────────────────────────────────────────────────────────


#' Get the Data Frame from a Spec
#'
#' @description
#'
#' Extracts the data frame stored in the `fr_spec` object. This is the same
#' data frame passed to [fr_table()] or [fr_listing()], before any rendering
#' transformations (sorting, repeat suppression, blank-after rows).
#'
#' Returns a **deep copy** — modifying the result does not affect the spec.
#'
#' @param spec An `fr_spec` object from [fr_table()] or [fr_listing()].
#'
#' @return A data frame. For figure specs created by [fr_figure()], returns
#'   an empty data frame with zero rows and zero columns.
#'
#' @examples
#' spec <- tbl_demog |> fr_table()
#' d <- fr_get_data(spec)
#' nrow(d)
#' names(d)
#'
#' @seealso [fr_table()], [fr_listing()], [fr_get_columns()] for column
#'   configuration.
#' @export
fr_get_data <- function(spec) {
  check_fr_spec(spec, call = caller_env())
  spec$data
}


#' Get Page Configuration from a Spec
#'
#' @description
#'
#' Extracts the page layout settings configured via [fr_page()]. Returns
#' a named list with all page properties: orientation, paper size, margins,
#' font family, font size, and column gap.
#'
#' @param spec An `fr_spec` object from [fr_table()] or [fr_listing()].
#'
#' @return A named list with fields: `orientation`, `paper`, `margins` (list
#'   with `top`, `bottom`, `left`, `right`), `font_family`, `font_size`,
#'   `col_gap`.
#'
#' @examples
#' spec <- tbl_demog |> fr_table() |>
#'   fr_page(orientation = "landscape", font_size = 8)
#' pg <- fr_get_page(spec)
#' pg$orientation   # "landscape"
#' pg$font_size     # 8
#' pg$margins$left  # margin in inches
#'
#' @seealso [fr_page()] to set page layout, [fr_pagehead()] and
#'   [fr_pagefoot()] for running headers/footers.
#' @export
fr_get_page <- function(spec) {
  check_fr_spec(spec, call = caller_env())
  as.list(spec$page)
}


#' Get Column Specifications from a Spec
#'
#' @description
#'
#' Extracts the named list of `fr_col` objects configured via [fr_cols()].
#' Each element describes a column's label, width, alignment, and other
#' properties. If no columns have been explicitly configured, returns an
#' empty list (columns are auto-generated at render time by
#' `finalize_spec()`).
#'
#' @param spec An `fr_spec` object from [fr_table()] or [fr_listing()].
#'
#' @return A named list of `fr_col` objects, keyed by column name. Empty
#'   list if no columns have been configured.
#'
#' @examples
#' spec <- tbl_demog |> fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     placebo = fr_col("Placebo", align = "right")
#'   )
#' cols <- fr_get_columns(spec)
#' names(cols)            # "characteristic", "placebo"
#' cols$placebo$width     # 1.5 (default)
#' cols$placebo$align     # "right"
#'
#' @seealso [fr_cols()] to configure columns, [fr_col()] for the column
#'   spec constructor, [fr_get_col()] for a single column.
#' @export
fr_get_columns <- function(spec) {
  check_fr_spec(spec, call = caller_env())
  spec$columns
}


#' Get a Single Column Specification
#'
#' @description
#'
#' Extracts the `fr_col` object for a named column. Errors with an
#' informative message if the column is not found, listing available
#' column names.
#'
#' @param spec An `fr_spec` object from [fr_table()] or [fr_listing()].
#' @param col Character scalar. Column name to retrieve.
#'
#' @return An `fr_col` object with fields: `label`, `width`, `align`,
#'   `header_align`, `visible`.
#'
#' @examples
#' spec <- tbl_demog |> fr_table() |>
#'   fr_cols(characteristic = fr_col("Characteristic", width = 2.5))
#' col <- fr_get_col(spec, "characteristic")
#' col$label   # "Characteristic"
#' col$width   # 2.5
#'
#' @seealso [fr_cols()], [fr_col()], [fr_get_columns()] for all columns.
#' @export
fr_get_col <- function(spec, col) {
  call <- caller_env()
  check_fr_spec(spec, call = call)
  check_scalar_chr(col, arg = "col", call = call)
  result <- spec$columns[[col]]
  if (is.null(result)) {
    cli_abort(
      "Column {.val {col}} not found in spec. Available: {.val {names(spec$columns)}}.",
      call = call
    )
  }
  result
}


#' Get Titles from a Spec
#'
#' @description
#'
#' Extracts the list of title entries configured via [fr_titles()]. Each
#' entry is a list with fields: `content`, `align`, `bold`, `font_size`.
#'
#' @param spec An `fr_spec` object from [fr_table()] or [fr_listing()].
#'
#' @return A list of title entry lists. Empty list if no titles are
#'   configured.
#'
#' @examples
#' spec <- tbl_demog |> fr_table() |>
#'   fr_titles("Table 14.1.1", "Summary of Demographics", .bold = TRUE)
#' titles <- fr_get_titles(spec)
#' length(titles)           # 2
#' titles[[1]]$content      # "Table 14.1.1"
#' titles[[1]]$bold         # TRUE
#'
#' @seealso [fr_titles()] to set titles, [fr_get_footnotes()] for
#'   footnotes.
#' @export
fr_get_titles <- function(spec) {
  check_fr_spec(spec, call = caller_env())
  spec$meta$titles %||% list()
}


#' Get Footnotes from a Spec
#'
#' @description
#'
#' Extracts the list of footnote entries configured via [fr_footnotes()].
#' Each entry is a list with fields: `content`, `align`, `font_size`,
#' `placement`.
#'
#' @param spec An `fr_spec` object from [fr_table()] or [fr_listing()].
#'
#' @return A list of footnote entry lists. Empty list if no footnotes are
#'   configured.
#'
#' @examples
#' spec <- tbl_demog |> fr_table() |>
#'   fr_footnotes("Source: ADSL", "[a] Fisher's exact test")
#' fns <- fr_get_footnotes(spec)
#' length(fns)              # 2
#' fns[[1]]$content         # "Source: ADSL"
#' fns[[2]]$placement       # "every" (default)
#'
#' @seealso [fr_footnotes()] to set footnotes, [fr_get_titles()] for
#'   titles.
#' @export
fr_get_footnotes <- function(spec) {
  check_fr_spec(spec, call = caller_env())
  spec$meta$footnotes %||% list()
}


#' Get Cell Styles from a Spec
#'
#' @description
#'
#' Extracts the list of cell style overrides applied via [fr_styles()].
#' Each element is an `fr_cell_style` object describing the target region,
#' rows, columns, and style properties (bold, fg, bg, etc.).
#'
#' @param spec An `fr_spec` object from [fr_table()] or [fr_listing()].
#'
#' @return A list of `fr_cell_style` objects. Empty list if no styles
#'   are applied.
#'
#' @examples
#' spec <- tbl_demog |> fr_table() |>
#'   fr_styles(
#'     fr_row_style(rows = 1L, bold = TRUE),
#'     fr_col_style(cols = "total", bg = "#EBF5FB")
#'   )
#' styles <- fr_get_styles(spec)
#' length(styles)           # 2
#'
#' @seealso [fr_styles()] to apply styles, [fr_style()], [fr_row_style()],
#'   [fr_col_style()] for style constructors.
#' @export
fr_get_styles <- function(spec) {
  check_fr_spec(spec, call = caller_env())
  spec$cell_styles %||% list()
}


#' Get Rules from a Spec
#'
#' @description
#'
#' Extracts the list of horizontal and vertical rule objects configured
#' via [fr_hlines()], [fr_vlines()], or [fr_grid()]. Each element is an
#' `fr_rule` object describing position, width, colour, and line style.
#'
#' @param spec An `fr_spec` object from [fr_table()] or [fr_listing()].
#'
#' @return A list of `fr_rule` objects. Empty list if no rules are
#'   configured.
#'
#' @examples
#' spec <- tbl_demog |> fr_table() |> fr_hlines("header")
#' rules <- fr_get_rules(spec)
#' length(rules)            # depends on "header" preset
#' rules[[1]]$direction     # "horizontal"
#'
#' @seealso [fr_hlines()] for horizontal rules, [fr_vlines()] for vertical
#'   rules, [fr_grid()] for full grid.
#' @export
fr_get_rules <- function(spec) {
  check_fr_spec(spec, call = caller_env())
  spec$rules %||% list()
}
