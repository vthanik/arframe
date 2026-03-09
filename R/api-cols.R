# ──────────────────────────────────────────────────────────────────────────────
# api-cols.R — Column configuration verbs: fr_cols, fr_select
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols — Configure column display
# ══════════════════════════════════════════════════════════════════════════════

#' Configure Column Display
#'
#' @description
#'
#' Configures the display label, width, alignment, and visibility of one or
#' more table columns. `fr_cols()` owns column **structure**; [fr_header()]
#' owns header **presentation** (alignment, valign, N-counts, bold, colours).
#'
#' Columns not explicitly named here receive auto-generated defaults: the
#' column name as the label (optionally transformed by `.label_fn`),
#' alignment inferred from the R column type (numeric → `"right"`,
#' everything else → `"left"`), and width from `.width`.
#'
#' Calling `fr_cols()` again **replaces** the entire column configuration.
#'
#' For N-count header labels (e.g. `"Placebo\\n(N=45)"`), header alignment,
#' or header vertical alignment, use [fr_header()] which is
#' order-independent with `fr_cols()`.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param ... Column specifications. Can be provided in two ways:
#'   * **Named arguments**: The argument name must match a column name in the data
#'     frame. The value can be an [fr_col()] object or a character scalar (label).
#'   * **Formulas (tidyselect)**: Use `lhs ~ rhs` where `lhs` is a set of columns
#'     selected via [tidyselect::language] (e.g., `starts_with("col")`), and `rhs`
#'     is an [fr_col()] object or character scalar applied to all selected columns.
#'     Powered by the `tidyselect` package.
#'
#'   Any data column not listed receives auto-generated defaults.
#' @param .list Named list or character vector of pre-built labels. Allows you
#'   to construct column labels externally (e.g., using `sprintf()`) and pass
#'   them in programmatically. Example: `list(zom_50mg = "Zomerane 50 mg")`.
#' @param .width Default column width applied to all columns that do not have
#'   an explicit width set. Accepts:
#'   * **Numeric** — width in inches (e.g. `1.5`). Default when `NULL`.
#'   * **`"auto"`** — auto-calculate widths from content and header text
#'     using the page font metrics. The layout engine measures the widest
#'     cell value and the header label for each column, adds padding, and
#'     scales **down** if the total exceeds the printable page width.
#'     Columns that already fit are left as-is.
#'   * **`"fit"`** — like `"auto"`, but always scales (up **or** down) so
#'     the total width exactly fills the printable page width, preserving
#'     the content-based ratio between columns. Use this when you want
#'     every table to span the full page width without manual tuning.
#'   * **`"equal"`** — distribute the printable page width equally among
#'     all columns that do not have a fixed width. Columns with an
#'     explicit numeric `width` in [fr_col()] keep their size; the
#'     remaining space is divided equally.
#'   * **Percentage string** — e.g. `"25%"`. Sets every column's default
#'     width as a fraction of the printable page width. Must be between
#'     `"0%"` (exclusive) and `"100%"` (inclusive). Columns with an explicit
#'     `width` in [fr_col()] are not affected.
#'   * **`NULL`** (default) — same as `"auto"`. Columns auto-size from
#'     content and header text.
#' @param .align Default alignment applied to all columns that do not have an
#'   explicit alignment set. One of `"left"`, `"center"`, `"right"`,
#'   `"decimal"`. `NULL` auto-detects: `"right"` for numeric columns,
#'   `"left"` for everything else.
#' @param .label_fn A function (or rlang-style lambda) applied to
#'   auto-generated column labels. Only affects columns whose labels were
#'   **not** explicitly set via `...` or `.list`. Receives
#'   the column name as input, returns the display label.
#'
#'   Common transforms:
#'   * `~ gsub("_", " ", .x)` — replace underscores with spaces.
#'   * `~ tools::toTitleCase(gsub("_", " ", .x))` — title case.
#'   * `toupper` — all caps.
#'
#' @param .split Logical or `NULL`. Column splitting for wide tables that
#'   exceed the printable page width:
#'   * `NULL` (default) — no splitting. All columns on one page.
#'   * `TRUE` — split across multiple panels, with stub columns repeated
#'     in each panel. Panel width behaviour follows `.width`:
#'     - `.width = "auto"` → panels keep natural column widths.
#'     - `.width = "fit"` → panels scale to fill the page width.
#'     - `.width = "equal"` → unfixed columns share remaining space equally.
#'   * `FALSE` — explicitly no splitting.
#'
#'   Stub columns (repeated in every panel) are designated via
#'   `fr_col(stub = TRUE)`. When `.split` is enabled but no columns have
#'   `stub = TRUE`, stubs are auto-inferred from `group_by`/`indent_by`
#'   columns or the first column.
#'
#' @return A modified `fr_spec`. Column specs stored in `spec$columns`,
#'   split settings in `spec$columns_meta`.
#'
#' @section Regulatory conventions — column ordering:
#' Standard pharma house styles and FDA/EMA submissions require:
#' * **Active treatment arm(s) left**, in order of dose escalation (lowest
#'   to highest dose, left to right).
#' * **Comparator / placebo arm(s) rightmost** among the study arm columns.
#' * **Total column** (if included): optional; covers **active arms only**
#'   (does not pool active + placebo). Placed immediately after the
#'   rightmost active arm column, before the placebo column.
#'
#' Example column order for a 2-dose + placebo study:
#' ```text
#' | Characteristic | Drug 10 mg | Drug 25 mg | Total (Active) | Placebo |
#' ```
#'
#' @section Label resolution order:
#' Column labels are resolved in this priority (highest wins):
#' 1. [fr_header()] `n` / `format` — dynamic label with N counts.
#' 2. Explicit `fr_col(label = ...)` in `...` arguments.
#' 3. `.list` — programmatic label map.
#' 4. `.label_fn` — transform function applied to the column name.
#' 5. Column name — the raw data frame column name as-is.
#'
#' @section Tips:
#' * Column order in the rendered table matches the **data frame column
#'   order**, not the order you list them in `fr_cols()`. To reorder
#'   columns, reorder the data frame first.
#' * Use `visible = FALSE` to suppress a column from rendering while keeping
#'   it available as a grouping key for [fr_rows()].
#' * `align = "decimal"` aligns decimal points in numeric columns.
#' * Width is in inches. Landscape Letter with 1 in margins gives 9 in
#'   printable width. A common pharma layout: stub column 2.5 in + 4–5 data
#'   columns at 1.3–1.5 in each.
#' * `.width = "auto"` is the fastest way to get a working table — the
#'   layout engine calculates widths from content. Use fixed widths only
#'   when you need exact control.
#' * `.label_fn` transforms **only** auto-generated labels (column names
#'   that were not explicitly relabelled). This lets you set a baseline
#'   transform and override specific columns as needed.
#'
#' @examples
#' ## ── Relabel + set widths + right-align (most common pattern) ─────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic",          width = 2.5),
#'     zom_50mg       = fr_col("Zomerane 50 mg",          width = 1.5, align = "right"),
#'     zom_100mg      = fr_col("Zomerane 100 mg",         width = 1.5, align = "right"),
#'     placebo        = fr_col("Placebo",                  width = 1.5, align = "right"),
#'     total          = fr_col("All Subjects",             width = 1.5, align = "right")
#'   ) |>
#'   fr_header(
#'     n = c(zom_50mg = 45, zom_100mg = 45, placebo = 45, total = 135),
#'     format = "{name}\n(N={n})"
#'   )
#'
#' ## ── Auto-width: let the engine calculate ─────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic"),
#'     .width = "auto"
#'   )
#'
#' ## ── Percentage widths: responsive to page size ───────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = "30%"),
#'     zom_50mg       = fr_col("Zomerane 50mg",  width = "17.5%", align = "right"),
#'     zom_100mg      = fr_col("Zomerane 100mg", width = "17.5%", align = "right"),
#'     placebo        = fr_col("Placebo",         width = "17.5%", align = "right"),
#'     total          = fr_col("Total",           width = "17.5%", align = "right")
#'   )
#'
#' ## ── Fit mode: fill the full page width proportionally ────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic"),
#'     .width = "fit"
#'   )
#'
#' ## ── Equal-width distribution ─────────────────────────────────────────────
#'
#' # Stub column fixed at 2.5in; remaining columns share the rest equally
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     .width = "equal"
#'   )
#'
#' ## ── Label transform: underscores to spaces + title case ──────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(.label_fn = ~ tools::toTitleCase(gsub("_", " ", .x)))
#'
#' ## ── Tidyselect formula (apply config to multiple columns) ────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     c(zom_50mg, zom_100mg) ~ fr_col(width = 1.5, align = "right"),
#'     starts_with("t") ~ fr_col(width = 1.5, align = "right")
#'   )
#'
#' ## ── Pre-formatted labels via .list ───────────────────────────────────────
#'
#' labels_vec <- c(placebo = "Placebo", zom_50mg = "Zom 50mg")
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(.list = labels_vec)
#'
#' ## ── N-count formatting via fr_header ────────────────────────────────────
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
#' ## ── Combine auto-width + label transform ─────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic"),
#'     .width = "auto",
#'     .label_fn = ~ tools::toTitleCase(gsub("_", " ", .x))
#'   ) |>
#'   fr_header(
#'     n = c(zom_50mg = 45, zom_100mg = 45, placebo = 45, total = 135),
#'     format = "{name}\n(N={n})"
#'   )
#'
#' ## ── Column splitting: split + fit to fill page ─────────────────────────
#'
#' tbl_vs |>
#'   fr_table() |>
#'   fr_cols(
#'     param     = fr_col("Parameter", stub = TRUE),
#'     statistic = fr_col("Statistic", stub = TRUE),
#'     .split = TRUE,
#'     .width = "fit"
#'   )
#'
#' ## ── Column splitting with natural widths (no stretching) ────────────────
#'
#' tbl_vs |>
#'   fr_table() |>
#'   fr_cols(.split = TRUE)
#'
#' @seealso [fr_col()] for the column spec constructor, [fr_header()] for
#'   N-count labels and header styling, [fr_spans()] for spanning group
#'   headers, [fr_rows()] for pagination.
#'
#' @export
fr_cols <- function(spec, ..., .list = NULL, .width = NULL, .align = NULL,
                       .label_fn = NULL, .split = NULL) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  # Validate .split: NULL, TRUE, or FALSE
  if (!is.null(.split)) {
    check_scalar_lgl(.split, arg = ".split", call = call)
    spec$columns_meta$split <- .split
  }

  # Validate .width: NULL, numeric, "auto", "equal", "fit", or "N%"
  # NULL defaults to "auto" (auto-size from content)
  if (is.null(.width)) {
    width_mode <- "auto"
    .width <- "auto"
  } else if (is.character(.width)) {
    pct <- parse_pct_width(.width, arg = ".width", call = call)
    if (!is.null(pct)) {
      .width <- pct
      width_mode <- "percent"
    } else {
      .width <- match_arg_fr(.width, c("auto", "equal", "fit"), call = call)
      width_mode <- .width
    }
  } else {
    check_positive_num(.width, arg = ".width", call = call)
    width_mode <- "fixed"
  }
  if (!is.null(.align)) .align <- match_arg_fr(.align, fr_env$valid_aligns, call = call)

  # Validate .label_fn
  if (!is.null(.label_fn)) {
    .label_fn <- rlang::as_function(.label_fn, call = call)
  }

  # Validate .list — named list/vector of pre-built label strings
  if (!is.null(.list)) {
    if ((!is.list(.list) && !is.character(.list)) || is.null(names(.list))) {
      cli_abort(
        c("{.arg .list} must be a named list or named character vector.",
          "i" = "Each name is a data column name; each value is the label string.",
          "i" = 'Example: {.code list(col1 = "Drug 50 mg\\n(N=45)")}'),
        call = call
      )
    }
    bad_labels <- setdiff(names(.list), names(spec$data))
    if (length(bad_labels) > 0L) {
      cli_abort(
        c("{.arg .list} column{?s} not found in the data: {.val {bad_labels}}.",
          "i" = "Available columns: {.val {names(spec$data)}}."),
        call = call
      )
    }
    labels_vec <- as.list(.list)
  } else {
    labels_vec <- list()
  }

  dots <- rlang::list2(...)
  expanded_dots <- list()

  if (length(dots) > 0L) {
    for (i in seq_along(dots)) {
      nm <- names(dots)[i]
      val <- dots[[i]]

      # Tidyselect formula (lhs ~ rhs)
      if (rlang::is_formula(val)) {
        lhs <- rlang::f_lhs(val)
        rhs <- rlang::f_rhs(val)

        # Evaluate selector against dataframe to get matched column indices/names
        sel <- tidyselect::eval_select(rlang::expr(!!lhs), spec$data)

        # Evaluate the config (rhs) in its environment
        rhs_eval <- rlang::eval_tidy(rhs, env = rlang::f_env(val))

        for (colnm in names(sel)) {
          expanded_dots[[colnm]] <- rhs_eval
        }
      } else {
        # Named argument
        if (is.null(nm) || nm == "") {
          cli_abort(
            c("All arguments to {.fn fr_cols} must be named or formula column selectors.",
              "i" = "Example named: {.code characteristic = fr_col(...)}",
              "i" = "Example formula: {.code starts_with('col') ~ fr_col(...)}"),
            call = call
          )
        }
        if (!nm %in% names(spec$data)) {
          cli_abort(
            c("Column {.val {nm}} not found in the data.",
              "i" = "Available columns: {.val {names(spec$data)}}."),
            call = call
          )
        }
        expanded_dots[[nm]] <- val
      }
    }
  }

  # Normalise expanded_dots configs
  configured_cols <- list()
  for (nm in names(expanded_dots)) {
    val <- expanded_dots[[nm]]
    if (is.character(val)) {
      check_scalar_chr(val, arg = nm, call = call)
      configured_cols[[nm]] <- structure(list(id = "", label = val, width = NULL, align = NULL, header_align = NULL, visible = NULL),
                class = "fr_col")
    } else {
      check_fr_col(val, arg = nm, call = call)
      configured_cols[[nm]] <- val
    }
  }

  spec$columns <- build_default_columns(spec$data,
                                         configured     = configured_cols,
                                         default_width  = .width,
                                         width_mode     = width_mode,
                                         default_align  = .align,
                                         label_fn       = .label_fn,
                                         labels         = labels_vec,
                                         page           = spec$page)

  spec$columns_meta$width_mode <- width_mode
  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_select — Lightweight column selector helper
# ══════════════════════════════════════════════════════════════════════════════

#' Select Columns by Name Pattern
#'
#' @description
#'
#' A lightweight column selection helper that returns a character vector of
#' column names from an `fr_spec`, suitable for passing to the `cols`
#' argument of [fr_col_style()], [fr_style()], [fr_spans()], or any verb
#' that accepts column names.
#'
#' `fr_select()` uses base R [grep()] — no tidyselect dependency. For
#' `tidyselect`-style helpers (`starts_with()`, `ends_with()`, etc.),
#' use the formula interface in [fr_cols()] instead.
#'
#' Use `fr_select()` when you need a **reusable** column vector — for
#' example, to pass the same set of columns to both `fr_spans()` and
#' `fr_col_style()`. For one-off column targeting, inline the names
#' directly in `cols = c("a", "b")`.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param pattern A regular expression (passed to [grep()]). Selects all
#'   column names in `spec$data` that match. `NULL` returns all columns
#'   (unless `cols` is also supplied).
#' @param cols A character vector of exact column names to include.
#'   When both `cols` and `pattern` are supplied, their results are
#'   **unioned** in data-frame column order.
#' @param exclude Character vector of column names to **remove** from the
#'   result. Applied after pattern/cols matching.
#'
#' @return A character vector of column names in data-frame column order.
#'
#' @section Tips:
#' * `fr_select(spec)` with no arguments returns **all** column names —
#'   useful as a starting point for exclusion-based selection.
#' * Combine `pattern` and `exclude` for expressive selections:
#'   `fr_select(spec, pattern = "^zom_", exclude = "zom_total")`.
#' * The result is always in data-frame column order, regardless of the
#'   order you list names in `cols`.
#'
#' @examples
#' spec <- tbl_demog |> fr_table()
#'
#' ## ── All columns ──────────────────────────────────────────────────────────
#'
#' fr_select(spec)
#'
#' ## ── Regex: columns starting with "zom_" ──────────────────────────────────
#'
#' fr_select(spec, pattern = "^zom_")
#'
#' ## ── Exact names ──────────────────────────────────────────────────────────
#'
#' fr_select(spec, cols = c("zom_50mg", "zom_100mg"))
#'
#' ## ── Exclude a column ─────────────────────────────────────────────────────
#'
#' fr_select(spec, exclude = "characteristic")
#'
#' ## ── Use result in fr_col_style ────────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_styles(
#'     fr_col_style(cols = fr_select(spec, pattern = "^zom_"), align = "right")
#'   )
#'
#' @seealso [fr_cols()] for column configuration, [fr_col_style()] for
#'   column styling.
#'
#' @export
fr_select <- function(spec, pattern = NULL, cols = NULL, exclude = NULL) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  all_cols <- names(spec$data)

  # Start with an empty selection set
  selected <- character(0)

  if (is.null(pattern) && is.null(cols)) {
    selected <- all_cols
  } else {
    if (!is.null(pattern)) {
      if (!is.character(pattern) || length(pattern) != 1L) {
        cli_abort(c("{.arg pattern} must be a single character regex string.",
                    "x" = "You supplied {.obj_type_friendly {pattern}}."),
                  call = call)
      }
      selected <- c(selected, grep(pattern, all_cols, value = TRUE))
    }
    if (!is.null(cols)) {
      if (!is.character(cols)) {
        cli_abort(c("{.arg cols} must be a character vector of column names.",
                    "x" = "You supplied {.obj_type_friendly {cols}}."),
                  call = call)
      }
      bad <- setdiff(cols, all_cols)
      if (length(bad) > 0L) {
        cli_abort(
          c("{.arg cols}: column{?s} not found in the data: {.val {bad}}.",
            "i" = "Available columns: {.val {all_cols}}."),
          call = call
        )
      }
      selected <- c(selected, cols)
    }
  }

  # Remove duplicates while preserving data-frame column order
  selected <- all_cols[all_cols %in% unique(selected)]

  # Apply exclusions
  if (!is.null(exclude)) {
    if (!is.character(exclude)) {
      cli_abort(c("{.arg exclude} must be a character vector of column names.",
                  "x" = "You supplied {.obj_type_friendly {exclude}}."),
                call = call)
    }
    selected <- setdiff(selected, exclude)
  }

  selected
}
