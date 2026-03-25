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
#' Calling `fr_rows()` again **merges** with the previous row configuration:
#' only the arguments you explicitly supply are changed.
#'
#' arframe uses a two-level greedy pagination algorithm (identical for RTF and
#' PDF output):
#' * **`page_by`** forces a page break whenever the named column(s) change
#'   value.
#' * **`group_by`** keeps rows with the same value in the named column(s)
#'   on the same page via `\trkeep` (RTF). If a group is too large for a
#'   single page, it breaks normally and a "(continued)" header row repeats
#'   at the top of the next page.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param page_by Page break specification. Accepts two forms:
#'
#'   **Simple (character)**: Character vector of column name(s). A new page
#'   begins whenever any of these columns change value. The column value is
#'   rendered as a group label above the column headers, and the column is
#'   **automatically hidden** from the table body at render time.
#'
#'   **List form**: A named list with elements:
#'   * `cols` — character vector of column name(s) (required)
#'   * `visible` — logical; whether the page-by label is displayed above the
#'     column headers (default `TRUE`). Set `FALSE` to get page breaks at group
#'     boundaries without a visible label.
#'
#'   Style page-by labels (bold, alignment, colour) via [fr_styles()] with
#'   `rows = "page_by"`, or set defaults via [fr_theme()].
#'
#' @param group_by Group specification. Accepts two forms:
#'
#'   **Simple (character)**: Character vector of column name(s). Rows sharing
#'   the same value are visually grouped (kept together on the same page when
#'   possible). When a group spans pages, a "(continued)" header row repeats
#'   at the top of the next page.
#'
#'   **List form**: A named list with elements:
#'   * `cols` — character vector of column name(s) (required). When multiple
#'     columns are supplied, groups are defined by the **combination** of all
#'     column values (e.g., `cols = c("PARAMCD", "direction")` groups by
#'     each unique PARAMCD + direction pair).
#'   * `label` — character scalar; column name into which group header values
#'     are injected. A **header row** is inserted at the start of each group,
#'     containing the value from the first `cols` column. When `label` is set
#'     and `indent_by` is not, `indent_by` is **automatically inferred** from
#'     `label`, indenting all detail rows under the bold group header.
#'   * `leaf` — character scalar; the lowest-level column in a multi-level
#'     hierarchy. Must be one of the `cols` values. When set, arframe
#'     collapses all hierarchy columns into a single `__display__` column
#'     and adds a `__row_level__` column containing the source column name
#'     for each row (e.g., `"soc"` or `"pt"`). Use `__row_level__` with
#'     [fr_rows_matches()] to style specific levels independently — for
#'     example, bold only the SOC header rows without affecting PT rows.
#'     Indentation is auto-set from the hierarchy depth. Source columns are
#'     auto-hidden.
#'
#'   `group_by` does **not** affect decimal alignment — values align globally
#'   across the entire column regardless of group boundaries. Only `page_by`
#'   creates separate alignment contexts (since pages are physically separate).
#'
#'   Style group header rows (bold, etc.) via [fr_styles()] by targeting
#'   group header rows with [fr_rows_matches()]. For single-level `label`
#'   groups, match on an empty stat column (e.g.,
#'   `fr_rows_matches("total", "")`). For multi-level `leaf` hierarchies,
#'   match on the `__row_level__` column (e.g.,
#'   `fr_rows_matches("__row_level__", "soc")`).
#'
#' @param indent_by Row indentation specification. Accepts two forms:
#'
#'   **Simple (single level)**: A character vector of column name(s). All
#'   detail rows (non-header, non-blank) in these columns receive one indent
#'   level. Typically used with `group_by` for SOC/PT tables:
#'   `indent_by = "pt"`.
#'
#'   **Multi-level (SAS-style)**: A named list with three elements:
#'   * `key` — column name containing row type markers (e.g., `"row_type"`)
#'   * `col` — column name(s) to apply indent to (e.g., `"term"`)
#'   * `levels` — named numeric vector mapping key values to indent
#'     multipliers (e.g., `c(soc = 0, hlt = 1, pt = 2)`)
#'
#'   Each indent level = 2 space-character widths (~0.17 in at 9pt).
#'   Rows whose key value is not in `levels` receive no indent.
#' @param blank_after Character vector of column name(s). A blank row is
#'   inserted after each group boundary in these columns.
#' @param group_keep Logical. Whether `group_by` groups are kept together
#'   on the same page via RTF `\keepn` / LaTeX keep-with-next. Default
#'   `TRUE`. Set `FALSE` for visual-only grouping (blank_after, indent)
#'   without page-keeping — useful for long groups where you want the
#'   renderer to break freely.
#' @param group_style Style properties for group header rows (injected by
#'   `group_by` with `label` or `leaf`). Accepts two forms:
#'
#'   **Uniform style** (all group headers): A named list of style properties:
#'   `bold`, `italic`, `underline`, `color`, `background`, `font_size`,
#'   `align`. Example: `group_style = list(bold = TRUE, background = "#F0F0F0")`.
#'
#'   **Per-level style** (leaf hierarchies only): A named list where names
#'   match hierarchy column names and values are style property lists.
#'   Example: `group_style = list(soc = list(bold = TRUE), hlt = list(italic = TRUE))`.
#'   Levels not listed receive no special styling.
#'
#'   `group_style` has the lowest precedence — explicit [fr_styles()] calls
#'   override it. Set study-wide defaults via [fr_theme()] or `_arframe.yml`.
#'
#'   Alternatively, use `rows = "group_headers"` in [fr_row_style()] or
#'   [fr_style()] for full styling control via [fr_styles()].
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
#' * Style page-by labels and group header rows via [fr_styles()] for full
#'   control over bold, alignment, and other formatting.
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
#' ## ── page_by with hidden label (page breaks only, no visible label) ───────
#'
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_rows(page_by = list(cols = "soc", visible = FALSE))
#'
#' ## ── Multi-level indent (SOC / HLT / PT hierarchy) ─────────────────────
#' ## Uses a named list: key column determines indent level per row
#'
#' # Given data with columns: soc, term, row_type, placebo, ...
#' # where row_type is "soc", "hlt", or "pt"
#' spec <- data.frame(
#'   soc = c("GI disorders", "GI disorders", "GI disorders"),
#'   term = c("GI disorders", "GI signs", "Nausea"),
#'   row_type = c("soc", "hlt", "pt"),
#'   result = c("72 (53.3)", "54 (40.0)", "24 (17.8)"),
#'   stringsAsFactors = FALSE
#' ) |>
#'   fr_table() |>
#'   fr_cols(soc = fr_col(visible = FALSE),
#'           row_type = fr_col(visible = FALSE)) |>
#'   fr_rows(
#'     group_by  = "soc",
#'     indent_by = list(
#'       key    = "row_type",
#'       col    = "term",
#'       levels = c(soc = 0, hlt = 1, pt = 2)
#'     )
#'   )
#'
#' ## ── Multi-level hierarchy with leaf + bold SOC only ─────────────────
#'
#' data.frame(
#'   soc    = c("GI disorders", "GI disorders", "GI disorders",
#'              "Nervous system", "Nervous system"),
#'   pt     = c("Nausea", "Vomiting", "Diarrhoea",
#'              "Headache", "Dizziness"),
#'   total  = c("24 (17.8)", "18 (13.3)", "12 ( 8.9)",
#'              "30 (22.2)", "15 (11.1)"),
#'   stringsAsFactors = FALSE
#' ) |>
#'   fr_table() |>
#'   fr_rows(
#'     group_by    = list(cols = c("soc", "pt"), leaf = "pt"),
#'     blank_after = "soc",
#'     group_style = list(soc = list(bold = TRUE))
#'   )
#'
#' ## ── group_by list form: auto-inject group headers ────────────────────
#' ## label injects header rows from group_by values into the named column.
#' ## indent_by is auto-inferred — no need to specify both.
#' ##
#' ## Result:
#' ##   stat           value
#' ##   Sex                       <- header row (from group column)
#' ##     Female       27 (60.0)  <- indented detail
#' ##     Male         18 (40.0)  <- indented detail
#' ##
#' ##   Age                       <- header row
#' ##     Mean (SD)    75.0 (6.8) <- indented detail
#' ##     Median       74.0
#' ##     Min, Max     65, 88
#'
#' data.frame(
#'   group = c("Sex", "Sex", "Age", "Age", "Age"),
#'   stat  = c("Female", "Male", "Mean (SD)", "Median", "Min, Max"),
#'   value = c("27 (60.0)", "18 (40.0)", "75.0 (6.8)", "74.0", "65, 88"),
#'   stringsAsFactors = FALSE
#' ) |>
#'   fr_table() |>
#'   fr_cols(group = fr_col(visible = FALSE)) |>
#'   fr_rows(
#'     group_by = list(cols = "group", label = "stat")
#'   )
#'
#' ## ── group_by with label + bold group headers ──────────────────────
#'
#' data.frame(
#'   PARAMCD    = c("ALB", "ALB", "ALT", "ALT"),
#'   stat_label = c("High", "Low", "High", "Low"),
#'   total      = c("6 ( 2.4)", "35 (13.8)", "30 (11.8)", "6 ( 2.4)"),
#'   placebo    = c("4 ( 4.7)", "17 (19.8)", "9 (10.5)", "1 ( 1.2)"),
#'   stringsAsFactors = FALSE
#' ) |>
#'   fr_table() |>
#'   fr_cols(PARAMCD = fr_col(visible = FALSE)) |>
#'   fr_rows(
#'     group_by    = list(cols = "PARAMCD", label = "stat_label"),
#'     blank_after = "PARAMCD",
#'     group_style = list(bold = TRUE)
#'   )
#'
#' ## ── Per-level styling (leaf hierarchies) ─────────────────────────────
#'
#' data.frame(
#'   soc   = c("GI disorders", "GI disorders",
#'             "Nervous system", "Nervous system"),
#'   pt    = c("Nausea", "Vomiting", "Headache", "Dizziness"),
#'   total = c("24 (17.8)", "18 (13.3)", "30 (22.2)", "15 (11.1)"),
#'   stringsAsFactors = FALSE
#' ) |>
#'   fr_table() |>
#'   fr_rows(
#'     group_by    = list(cols = c("soc", "pt"), leaf = "pt"),
#'     blank_after = "soc",
#'     group_style = list(soc = list(bold = TRUE, background = "#F0F0F0"))
#'   )
#'
#' ## ── sort_by: order a listing by subject and start date ────────────────
#'
#' adae[1:20, c("USUBJID", "AEBODSYS", "AEDECOD", "ASTDT", "AESEV")] |>
#'   fr_table() |>
#'   fr_rows(sort_by = c("USUBJID", "ASTDT"))
#'
#' ## ── suppress: suppress repeated subject IDs in a listing ───────────
#'
#' adae[1:20, c("USUBJID", "AEBODSYS", "AEDECOD", "AESEV")] |>
#'   fr_table() |>
#'   fr_rows(
#'     sort_by     = c("USUBJID", "AEBODSYS"),
#'     suppress = "USUBJID"
#'   )
#'
#' ## ── wrap = TRUE: enable text wrapping for long verbatim terms ─────────
#'
#' adae[1:10, c("USUBJID", "AEBODSYS", "AEDECOD", "AEOUT")] |>
#'   fr_table() |>
#'   fr_rows(wrap = TRUE)
#'
#' ## ── Combined: sort_by + suppress on adverse event listing ──────────
#'
#' adae[1:30, c("USUBJID", "AEBODSYS", "AEDECOD", "AESEV", "ASTDT")] |>
#'   fr_table() |>
#'   fr_rows(
#'     sort_by     = c("USUBJID", "AEBODSYS", "AEDECOD"),
#'     suppress = c("USUBJID", "AEBODSYS"),
#'     wrap        = TRUE
#'   )
#'
#' @param sort_by Character vector of column name(s). Sorts the data by these
#'   columns before rendering. For listings, this controls the display order
#'   (e.g., `sort_by = c("USUBJID", "ASTDT")`). Sorting is applied in
#'   `finalize_spec()`.
#' @param suppress Character vector of column name(s). Suppresses repeated
#'   consecutive values in these columns — only the first occurrence in each
#'   run is displayed. Standard for listings where subject ID appears once per
#'   block. Suppression is applied in `finalize_spec()`.
#' @param wrap Logical. When `TRUE`, enables text wrapping in body cells.
#'   Default `FALSE`. For listings with long text fields (e.g., verbatim terms,
#'   medical history), wrapping prevents cell content from overflowing.
#'
#' @seealso [fr_page()] to set `orphan_min` / `widow_min`,
#'   [fr_cols()] to hide structural columns from display.
#'
#' @export
fr_rows <- function(
  spec,
  page_by = NULL,
  group_by = NULL,
  indent_by = NULL,
  blank_after = NULL,
  group_keep = TRUE,
  group_style = NULL,
  sort_by = NULL,
  suppress = NULL,
  wrap = FALSE
) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  validate_cols <- function(x, arg) {
    if (is.null(x)) {
      return(NULL)
    }
    if (!is.character(x)) {
      cli_abort(
        c(
          "{.arg {arg}} must be a character vector of column names.",
          "x" = "You supplied {.obj_type_friendly {x}}."
        ),
        call = call
      )
    }
    validate_cols_exist(x, names(spec$data), arg = arg, call = call)
  }

  validate_indent_by <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    # Simple form: character vector of column names
    if (is.character(x)) {
      return(validate_cols(x, "indent_by"))
    }
    # Multi-level form: named list with key, col, levels
    if (is.list(x)) {
      required <- c("key", "col", "levels")
      missing_elem <- setdiff(required, names(x))
      if (length(missing_elem) > 0L) {
        cli_abort(
          c(
            "{.arg indent_by} list must contain: {.val {required}}.",
            "x" = "Missing: {.val {missing_elem}}.",
            "i" = paste0(
              "Example: {.code indent_by = list(key = \"row_type\", ",
              "col = \"term\", levels = c(soc = 0, hlt = 1, pt = 2))}"
            )
          ),
          call = call
        )
      }
      validate_cols_exist(
        x$key,
        names(spec$data),
        arg = "indent_by$key",
        call = call
      )
      validate_cols_exist(
        x$col,
        names(spec$data),
        arg = "indent_by$col",
        call = call
      )
      if (!is.numeric(x$levels) || is.null(names(x$levels))) {
        cli_abort(
          c(
            "{.arg indent_by$levels} must be a named numeric vector.",
            "x" = "You supplied {.obj_type_friendly {x$levels}}.",
            "i" = "Example: {.code c(soc = 0, hlt = 1, pt = 2)}"
          ),
          call = call
        )
      }
      return(x)
    }
    cli_abort(
      c(
        "{.arg indent_by} must be a character vector or a named list.",
        "x" = "You supplied {.obj_type_friendly {x}}.",
        "i" = paste0(
          "Simple: {.code indent_by = \"pt\"}. ",
          "Multi-level: {.code indent_by = list(key = \"row_type\", ",
          "col = \"term\", levels = c(hlt = 1, pt = 2))}"
        )
      ),
      call = call
    )
  }

  # ── Normalise page_by: string or list(cols, visible) ────────────────────
  page_by_cols <- NULL
  page_by_visible <- NULL
  if (!missing(page_by) && !is.null(page_by)) {
    if (is.character(page_by)) {
      page_by_cols <- validate_cols(page_by, "page_by")
      page_by_visible <- TRUE
    } else if (is.list(page_by) && "cols" %in% names(page_by)) {
      page_by_cols <- validate_cols(page_by$cols, "page_by$cols")
      page_by_visible <- page_by$visible %||% TRUE
      check_scalar_lgl(page_by_visible, arg = "page_by$visible", call = call)
    } else {
      cli_abort(
        c(
          "{.arg page_by} must be a character vector or a list with a {.val cols} element.",
          "x" = "You supplied {.obj_type_friendly {page_by}}.",
          "i" = paste0(
            "Simple: {.code page_by = \"PARAM\"}. ",
            "List: {.code page_by = list(cols = \"PARAM\", visible = FALSE)}"
          )
        ),
        call = call
      )
    }
  }

  # ── Normalise group_by: string or list(cols, label, leaf) ───────────────
  group_by_cols <- NULL
  group_label <- NULL
  group_leaf <- NULL
  if (!missing(group_by) && !is.null(group_by)) {
    if (is.character(group_by)) {
      group_by_cols <- validate_cols(group_by, "group_by")
    } else if (is.list(group_by) && "cols" %in% names(group_by)) {
      group_by_cols <- validate_cols(group_by$cols, "group_by$cols")

      # Validate label
      if (!is.null(group_by$label)) {
        check_scalar_chr(group_by$label, arg = "group_by$label", call = call)
        validate_cols_exist(
          group_by$label,
          names(spec$data),
          arg = "group_by$label",
          call = call
        )
        group_label <- group_by$label
      }

      # Validate leaf (placeholder for Fix 3)
      if (!is.null(group_by$leaf)) {
        check_scalar_chr(group_by$leaf, arg = "group_by$leaf", call = call)
        if (!group_by$leaf %in% group_by$cols) {
          cli_abort(
            c(
              "{.arg group_by$leaf} must be one of the {.arg group_by$cols} values.",
              "x" = "{.val {group_by$leaf}} is not in {.val {group_by$cols}}."
            ),
            call = call
          )
        }
        group_leaf <- group_by$leaf
      }
    } else {
      cli_abort(
        c(
          "{.arg group_by} must be a character vector or a list with a {.val cols} element.",
          "x" = "You supplied {.obj_type_friendly {group_by}}.",
          "i" = paste0(
            "Simple: {.code group_by = \"soc\"}. ",
            "List: {.code group_by = list(cols = \"variable\", label = \"stat_label\")}"
          )
        ),
        call = call
      )
    }
  }

  # Warn if multi-level indent_by is set without group_by
  effective_gb <- group_by_cols %||% spec$body$group_by
  if (!missing(indent_by) && is.list(indent_by)) {
    if (is.null(effective_gb) || length(effective_gb) == 0L) {
      cli::cli_warn(
        c(
          "Multi-level {.arg indent_by} typically requires {.arg group_by}.",
          "i" = "Without {.arg group_by}, indent levels apply but rows are not grouped."
        ),
        call = call
      )
    }
  }

  if (!missing(group_keep)) {
    check_scalar_lgl(group_keep, arg = "group_keep", call = call)
  }
  if (!is.null(group_style)) {
    group_style <- validate_group_style(group_style, call = call)
  }
  if (!missing(wrap)) {
    check_scalar_lgl(wrap, arg = "wrap", call = call)
  }

  # Validate page_by and group_by don't share the same column
  effective_page_by <- page_by_cols %||% spec$body$page_by
  effective_group_by <- group_by_cols %||% spec$body$group_by
  if (
    !is.null(effective_page_by) &&
      !is.null(effective_group_by) &&
      length(intersect(effective_page_by, effective_group_by)) > 0L
  ) {
    shared <- intersect(effective_page_by, effective_group_by)
    cli::cli_warn(
      c(
        "{.arg page_by} and {.arg group_by} share column{?s}: {.val {shared}}.",
        "i" = "{.arg group_by} grouping is applied within each {.arg page_by} page."
      ),
      call = call
    )
  }

  old <- spec$body

  # Resolve group_label: from list-form group_by or from prior call
  effective_gl <- group_label %||% old$group_label

  # Auto-infer indent_by from group_label when not explicitly set.
  # group_label inserts header rows — detail rows should be indented under them.
  effective_indent <- validate_indent_by(indent_by) %||% old$indent_by
  indent_not_set <- is.null(effective_indent) ||
    (is.character(effective_indent) && length(effective_indent) == 0L)
  if (indent_not_set && !is.null(effective_gl)) {
    effective_indent <- effective_gl
  }

  spec$body <- new_fr_body(
    page_by = page_by_cols %||% old$page_by,
    group_by = group_by_cols %||% old$group_by,
    indent_by = effective_indent,
    blank_after = validate_cols(blank_after, "blank_after") %||%
      old$blank_after,
    page_by_visible = if (!is.null(page_by_visible)) {
      page_by_visible
    } else {
      old$page_by_visible
    },
    group_label = effective_gl,
    group_keep = if (!missing(group_keep)) {
      group_keep
    } else {
      old$group_keep
    },
    group_style = group_style %||% old$group_style,
    group_leaf = group_leaf %||% old$group_leaf,
    group_hierarchy_cols = if (
      !is.null(group_leaf) && length(group_by_cols) > 1L
    ) {
      group_by_cols
    } else {
      old$group_hierarchy_cols
    },
    sort_by = validate_cols(sort_by, "sort_by") %||% old$sort_by,
    suppress = validate_cols(suppress, "suppress") %||%
      old$suppress,
    wrap = if (!missing(wrap)) wrap else old$wrap
  )

  # Collapse hierarchy immediately so __display__ and __row_level__ columns

  # exist in spec$data for downstream fr_cols() calls.
  spec <- collapse_hierarchy(spec)

  spec
}
