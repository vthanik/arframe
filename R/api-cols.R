# ──────────────────────────────────────────────────────────────────────────────
# api-cols.R — Column configuration verbs: fr_cols
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols — Configure column display
# ══════════════════════════════════════════════════════════════════════════════

#' Configure Column Display
#'
#' @description
#'
#' Configures the display label, width, alignment, visibility, N-counts, and
#' spanning groups for table columns. `fr_cols()` is the **single source of
#' truth** for all column structure; [fr_header()] owns header
#' **presentation** only (bold, colours, font size).
#'
#' Columns not explicitly named here receive auto-generated defaults: the
#' column name as the label (optionally transformed by `.label_fn`),
#' alignment inferred from the R column type (numeric → `"right"`,
#' everything else → `"left"`), and width from `.width`.
#'
#' Calling `fr_cols()` again **replaces** the entire column configuration.
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
#' @param .spaces How to handle leading spaces in cell data for all columns
#'   that do not have an explicit `spaces` set in [fr_col()]. One of:
#'   * `"indent"` (default) — convert leading spaces to paragraph-level
#'     indent. The indent width is measured from the page font metrics,
#'     so it renders correctly in both proportional and monospace fonts.
#'   * `"preserve"` — keep leading spaces as literal characters.
#'   * `NULL` — same as `"indent"`.
#' @param .n Bulk N-counts applied across columns and spanning groups.
#'   Names are matched **case-insensitively** using a two-step lookup:
#'   first by **column display label**, then by **data column name** as
#'   fallback. This means you can use either form:
#'   * `c("Placebo" = 45)` — matches by label
#'   * `c(placebo = 45)` — matches by column name (no label repetition)
#'
#'   Accepts:
#'
#'   * **Named numeric vector** — names = display labels or column names,
#'     matched case-insensitively to column labels first, then column
#'     names. Also matches spanning group names.
#'     Example: `c(placebo = 45, zom_50mg = 44)`.
#'
#'   * **Data frame (2-column)** — column 1 = display labels,
#'     column 2 = counts. Same N on every page.
#'     Example: `data.frame(trt = c("Placebo", "Zom 50mg"), n = c(45, 44))`.
#'
#'   * **Data frame (3-column)** — column 1 = `page_by` group values,
#'     column 2 = display labels, column 3 = counts. Different N per
#'     `page_by` group.
#'
#'   * **Named list** — keys = `page_by` group values, values = named
#'     numeric vectors (names = display labels).
#'     Example: `list("Systolic BP" = c("Placebo" = 42, "Zom" = 40))`.
#'
#'   **Auto-routing**: when a label matches a `group` name (spanning
#'   header), N goes on the span. When it matches a column label, N goes
#'   on the column. Group matches take priority (no double-apply).
#'
#'   Per-column `fr_col(n = ...)` always takes highest priority.
#'
#' @param .n_format A [glue][glue::glue]-style format string for N-count
#'   labels. Available tokens: `{label}` (column display label) and
#'   `{n}` (count). Default `NULL` inherits from config YAML
#'   `columns.n_format` or theme. Example: `"{label}\\n(N={n})"`.
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
#' 1. N-count formatting (`fr_col(n=)` or `.n` + `.n_format`) — dynamic
#'    label with N counts, applied at render time.
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
#' ## ── Per-column N-counts (80% case) ───────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     zom_50mg       = fr_col("Zomerane 50 mg",  n = 45),
#'     zom_100mg      = fr_col("Zomerane 100 mg", n = 45),
#'     placebo        = fr_col("Placebo",          n = 45),
#'     total          = fr_col("Total",            n = 135),
#'     .n_format = "{label}\n(N={n})"
#'   ) |>
#'   fr_header(bold = TRUE, align = "center")
#'
#' ## ── Bulk N by column name (no label repetition) ────────────────────────
#'
#' # .n keys match column names — labels only defined once in fr_col()
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     placebo        = fr_col("Placebo"),
#'     zom_50mg       = fr_col("Zomerane 50 mg"),
#'     zom_100mg      = fr_col("Zomerane 100 mg"),
#'     total          = fr_col("Total"),
#'     .n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135),
#'     .n_format = "{label}\n(N={n})"
#'   )
#'
#' ## ── Bulk N from a data frame ─────────────────────────────────────────────
#'
#' adsl_n <- data.frame(
#'   trt = c("Placebo", "Zomerane 50 mg", "Zomerane 100 mg", "Total"),
#'   n   = c(45L, 45L, 45L, 135L)
#' )
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     placebo        = fr_col("Placebo"),
#'     zom_50mg       = fr_col("Zomerane 50 mg"),
#'     zom_100mg      = fr_col("Zomerane 100 mg"),
#'     total          = fr_col("Total"),
#'     .n = adsl_n,
#'     .n_format = "{label}\n(N={n})"
#'   )
#'
#' ## ── Spanning groups via group= ───────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     zom_50mg       = fr_col("50 mg",  group = "Zomerane"),
#'     zom_100mg      = fr_col("100 mg", group = "Zomerane"),
#'     placebo        = fr_col("Placebo"),
#'     total          = fr_col("Total")
#'   )
#'
#' ## ── Bulk N auto-routes to spans and columns ──────────────────────────────
#'
#' n_df <- data.frame(
#'   trt = c("Zomerane", "Placebo", "Total"),
#'   n   = c(90L, 45L, 135L)
#' )
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     zom_50mg       = fr_col("50 mg",  group = "Zomerane"),
#'     zom_100mg      = fr_col("100 mg", group = "Zomerane"),
#'     placebo        = fr_col("Placebo"),
#'     total          = fr_col("Total"),
#'     .n = n_df,
#'     .n_format = "{label}\n(N={n})"
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
#' ## ── Leading spaces → paragraph-level indent (default) ──────────────────
#'
#' # tbl_demog has leading spaces ("  Mean (SD)", "  <65", etc.)
#' # Default .spaces = "indent" converts them to real paragraph indent
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     .spaces = "indent"
#'   )
#'
#' ## ── Preserve leading spaces as literal characters ─────────────────────
#'
#' # Use "preserve" for pre-formatted content with exact spacing
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     .spaces = "preserve"
#'   )
#'
#' ## ── Per-column override: preserve one column, indent the rest ─────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5, spaces = "preserve"),
#'     .spaces = "indent"
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
#'   header presentation (bold, colours, alignment), [fr_spans()] for
#'   advanced multi-level spanning headers, [fr_rows()] for pagination.
#'
#' @export
fr_cols <- function(spec, ..., .list = NULL, .width = NULL, .align = NULL,
                       .label_fn = NULL, .spaces = NULL, .split = NULL,
                       .n = NULL, .n_format = NULL) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  # Validate .split: NULL, TRUE, or FALSE
  if (!is.null(.split)) {
    check_scalar_lgl(.split, arg = ".split", call = call)
    spec$columns_meta$split <- .split
  }

  # Validate .spaces: NULL, "indent", or "preserve"
  if (!is.null(.spaces)) {
    .spaces <- match_arg_fr(.spaces, fr_env$valid_spaces, call = call)
    spec$columns_meta$spaces <- .spaces
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
    validate_cols_exist(names(.list), names(spec$data), arg = ".list",
                        call = call)
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

  # Validate and store N-count parameters
  if (!is.null(.n)) {
    validate_n_param(n = .n, format = .n_format, call = call)
    spec$columns_meta$n <- .n
  }
  if (!is.null(.n_format)) {
    check_scalar_chr(.n_format, arg = ".n_format", call = call)
    spec$columns_meta$n_format <- .n_format
  }

  spec
}


