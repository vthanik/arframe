# ──────────────────────────────────────────────────────────────────────────────
# classes.R — S3 Intermediate Representation (IR) classes
#
# NAMING CONVENTION (Advanced R, Hadley Wickham §13.3):
#
#   new_fr_*()  — Internal low-level constructors. Minimal validation, not
#                 exported. Called by pipeline verbs in api.R. Analogous to
#                 vctrs::new_vctr(), ggplot2::new_scale().
#
#   fr_col()    — User-facing helper. Exported, full validation, nice errors.
#                 Called directly by users inside fr_cols().
#
# Rule of thumb:
#   - If the user types it directly → no new_ prefix, export it
#   - If only our code calls it     → new_ prefix, @noRd
#
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# fr_page — Page layout specification (internal constructor)
#
# Users configure page layout via the fr_page() pipeline verb in api.R,
# which calls new_fr_page() internally. Not exported.
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
new_fr_page <- function(
  orientation = "landscape",
  paper = "letter",
  margins = 1,
  font_family = NULL,
  font_size = 9,
  orphan_min = fr_env$default_orphan_min,
  widow_min = fr_env$default_widow_min,
  continuation = NULL,
  col_gap = 4L,
  tokens = list(),
  call = caller_env()
) {
  orientation <- match_arg_fr(
    orientation,
    c("landscape", "portrait"),
    call = call
  )
  paper <- match_arg_fr(paper, c("letter", "a4", "legal"), call = call)

  # ── Margin normalisation (ggplot2 / CSS convention) ───────────────────────
  # Length 1: all sides equal       → margins = 1
  # Length 2: c(vertical, horizontal) → margins = c(1, 0.75)
  # Length 4: c(top, right, bottom, left) → CSS order (t, r, b, l)
  # Named list: list(top=, bottom=, left=, right=) → explicit
  margins <- normalise_margins(margins, call = call)

  check_positive_num(font_size, arg = "font_size", call = call)
  if (font_size < 4 || font_size > 72) {
    cli_abort(
      c(
        "{.arg font_size} must be between 4 and 72.",
        "x" = "You supplied {.val {font_size}}."
      ),
      call = call
    )
  }

  if (is.null(font_family)) {
    font_family <- os_default_fonts()$serif
  }

  # Validate continuation: NULL or character scalar
  # Default NULL — no continuation text. LaTeX longtable has no default
  # continuation text; \endhead is empty by default and the user must
  # explicitly write e.g. "\multicolumn{N}{c}{Table N -- continued}"
  # in the \endhead block. We follow the same philosophy: opt-in only.
  if (!is.null(continuation)) {
    check_scalar_chr(continuation, arg = "continuation", call = call)
  }

  # Validate col_gap (inter-column padding in points)
  check_non_negative_int(col_gap, arg = "col_gap", call = call)

  # Validate tokens (checks for built-in override attempt)
  validate_user_tokens(tokens, call = call)

  structure(
    list(
      orientation = orientation,
      paper = paper,
      margins = margins,
      font_family = font_family,
      font_size = font_size,
      orphan_min = vec_cast(orphan_min, integer()),
      widow_min = vec_cast(widow_min, integer()),
      continuation = continuation,
      col_gap = vec_cast(col_gap, integer()),
      tokens = as.list(tokens)
    ),
    class = "fr_page"
  )
}


# ── Margin normalisation helper ─────────────────────────────────────────────
#
# ggplot2 source (R/margins.R):
#   margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
#   margin_auto(t = 0, r = t, b = t, l = r, unit = "pt")
#   Mnemonic: "trouble" → t, r, b, l
#
# CSS shorthand:
#   margin: 1in;                        → all sides
#   margin: 1in 0.75in;                 → vertical horizontal
#   margin: 1in 0.75in 1in 0.75in;     → top right bottom left
#
# We accept:
#   Scalar numeric  → all four sides equal
#   Length-2 numeric → c(vertical, horizontal) = c(top_bottom, left_right)
#   Length-4 numeric → c(top, right, bottom, left)  (CSS t-r-b-l order)
#   Named list       → list(top =, bottom =, left =, right =)
# ─────────────────────────────────────────────────────────────────────────────

#' @noRd
normalise_margins <- function(margins, call = caller_env()) {
  # Named list form (our original API)
  if (is.list(margins)) {
    req <- c("top", "bottom", "left", "right")
    if (!all(req %in% names(margins))) {
      cli_abort(
        c(
          "{.arg margins} as a list must have names: {.val {req}}.",
          "x" = "You supplied names: {.val {names(margins)}}.",
          "i" = "Or pass a numeric: {.code 1} (all sides), {.code c(1, 0.75)} (vert, horiz), or {.code c(1, 0.75, 1, 0.75)} (t, r, b, l)."
        ),
        call = call
      )
    }
    for (side in req) {
      if (!is.numeric(margins[[side]]) || margins[[side]] < 0) {
        cli_abort(
          c(
            "{.arg margins${side}} must be a non-negative number.",
            "x" = "You supplied {.obj_type_friendly {margins[[side]]}}."
          ),
          call = call
        )
      }
    }
    return(margins)
  }

  # Numeric shorthand
  if (!is.numeric(margins)) {
    cli_abort(
      c(
        "{.arg margins} must be a numeric vector or a named list.",
        "x" = "You supplied {.obj_type_friendly {margins}}.",
        "i" = "Examples: {.code 1}, {.code c(1, 0.75)}, {.code c(1, 0.75, 1, 0.75)}, or {.code list(top = 1, bottom = 1, left = 0.75, right = 0.75)}."
      ),
      call = call
    )
  }

  if (any(margins < 0)) {
    cli_abort(
      c(
        "{.arg margins} values must be non-negative.",
        "x" = "Negative value{?s}: {.val {margins[margins < 0]}}."
      ),
      call = call
    )
  }

  margins <- unname(margins)
  n <- length(margins)
  result <- switch(
    as.character(n),
    "1" = list(
      top = margins[1],
      bottom = margins[1],
      left = margins[1],
      right = margins[1]
    ),
    "2" = list(
      top = margins[1],
      bottom = margins[1],
      left = margins[2],
      right = margins[2]
    ),
    "4" = list(
      top = margins[1],
      right = margins[2],
      bottom = margins[3],
      left = margins[4]
    ),
    cli_abort(
      c(
        "{.arg margins} must have length 1, 2, or 4.",
        "x" = "You supplied length {.val {n}}.",
        "i" = "{.code 1} = all sides, {.code c(vert, horiz)}, {.code c(top, right, bottom, left)}."
      ),
      call = call
    )
  )

  result
}


# ── Token validation ────────────────────────────────────────────────────────

#' Validate user tokens: error if overriding built-in tokens
#' @noRd
validate_user_tokens <- function(tokens, call = caller_env()) {
  if (length(tokens) == 0L) {
    return(invisible(NULL))
  }

  if (!is.list(tokens) || is.null(names(tokens))) {
    cli_abort(
      c(
        "{.arg tokens} must be a named list (e.g., {.code list(study = \"ABC-001\")}).",
        "x" = "You supplied {.obj_type_friendly {tokens}}."
      ),
      call = call
    )
  }

  # thepage and total_pages are controlled by the pagination engine and
  # cannot be overridden — they change on every page
  builtin_readonly <- c("thepage", "total_pages")
  conflicts <- intersect(names(tokens), builtin_readonly)
  if (length(conflicts) > 0L) {
    cli_abort(
      c(
        "Cannot override built-in token{?s}: {.val {conflicts}}.",
        "i" = "{.val thepage} and {.val total_pages} are set automatically by the pagination engine.",
        "i" = "You can set {.val program} and {.val datetime} via {.arg tokens}."
      ),
      call = call
    )
  }

  invisible(NULL)
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_pct — Percentage width marker (internal)
#
# Wraps a numeric fraction (0–1) to distinguish percentage-based widths from
# absolute inch widths. Resolved to absolute inches in finalize_columns().
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
fr_pct <- function(x) structure(x, class = "fr_pct")

#' @noRd
is_fr_pct <- function(x) inherits(x, "fr_pct")


# ══════════════════════════════════════════════════════════════════════════════
# fr_col — Column definition (USER-FACING, exported)
#
# Called directly by users inside fr_cols():
#   fr_cols(param = fr_col("Parameter", width = 2.5))
# ══════════════════════════════════════════════════════════════════════════════

#' Define a Column Specification
#'
#' @description
#'
#' Defines the display properties of a single table column: its label, width,
#' alignment, visibility, N-count, and spanning group. Used inside [fr_cols()]
#' as a named argument to configure individual columns. Columns not explicitly
#' configured receive auto-generated defaults from the `.width`, `.align`, and
#' `.label_fn` arguments of [fr_cols()].
#'
#' `fr_col()` is the **single source of truth** for column structure. Labels,
#' widths, alignment, N-counts, and spanning groups are all defined here.
#' [fr_header()] is purely for header *presentation* (bold, colours, font).
#'
#' @param label Character scalar. Display label shown in the column header.
#'   Supports `{fr_*()}` inline markup (e.g. `"{fr_super('a')}"` for
#'   superscript footnote markers). Default `""` inherits the data frame
#'   column name as the label, optionally transformed by the `.label_fn`
#'   argument of [fr_cols()].
#' @param width Column width. Accepts:
#'   * **Numeric** — fixed width in inches (e.g. `2.5`). Use for stub
#'     columns or when you need exact control.
#'   * **`"auto"`** — auto-calculate from content and header widths using
#'     the page font metrics. The layout engine measures the widest cell
#'     value and the header label, adds padding, and converts to inches.
#'     Columns are then proportionally scaled to fit the printable page
#'     width. This is the fastest way to get a working table.
#'   * **Percentage string** — e.g. `"20%"`. Sets the column width as a
#'     fraction of the printable page width. Resolved to absolute inches
#'     at render time. Must be between `"0%"` (exclusive) and `"100%"`
#'     (inclusive). Useful for responsive layouts that adapt to different
#'     page sizes or orientations.
#'   * **`NULL`** (default) — inherits from the `.width` argument of
#'     [fr_cols()]. If `.width` is also `NULL`, the package default
#'     of 1.5 in is used.
#' @param align Column alignment. One of:
#'   * `"left"` — left-aligned. Standard for text and stub columns.
#'   * `"center"` — centered. Rare in regulatory tables; used for
#'     binary indicators (Yes/No) or short categorical values.
#'   * `"right"` — right-aligned. Standard for numeric columns (counts,
#'     percentages, p-values).
#'   * `"decimal"` — decimal-point alignment. Aligns the decimal point
#'     (or last digit for integers) across all rows in the column.
#'     Standard for continuous summary statistics (mean, SD, median).
#'   * `NULL` (default) — auto-detects from the R column type:
#'     numeric/integer → `"right"`, everything else → `"left"`.
#' @param header_align Horizontal alignment for the column header cell.
#'   One of `"left"`, `"center"`, `"right"`, `"decimal"`, or `NULL`
#'   (default). When `NULL`, the header inherits alignment from `align`.
#'   Use this to center treatment-arm headers while keeping body cells
#'   right-aligned. Can also be set uniformly via
#'   `fr_header(align = ...)`; per-column values here take priority.
#'   `fr_style(region = "header", align = ...)` overrides both.
#' @param visible Logical or `NULL`. Controls whether the column appears
#'   in the rendered output.
#'   * `NULL` (default): the system decides — columns used as
#'     [fr_rows()] `page_by` keys are auto-hidden; all others are visible.
#'   * `TRUE`: force the column visible, even if it is a `page_by` key.
#'   * `FALSE`: hide the column. Useful for structural columns (grouping
#'     keys, sort-order columns, row-type flags) that should remain in the
#'     data for pagination and styling logic but not appear in output.
#' @param stub Logical. Whether this column is a **stub column** —
#'   repeated on every panel when `fr_cols(.split = TRUE)` splits the
#'   table across multiple column pages. Default `FALSE`.
#'   Stub columns are typically row-label or parameter columns that
#'   provide context for each panel. When `.split` is enabled but no
#'   columns have `stub = TRUE`, stubs are auto-inferred from
#'   `group_by`/`indent_by` columns or the first column.
#' @param space_mode How to handle leading spaces in cell data. One of:
#'   * `"indent"` — convert leading spaces to paragraph-level indent
#'     (RTF `\li`, LaTeX `\leftskip`). All lines (including wrapped)
#'     maintain the same indent. This is the correct approach for
#'     proportional fonts and pharma SOC/PT hierarchies.
#'   * `"preserve"` — keep leading spaces as literal characters. Use
#'     for pre-formatted content where exact spacing must be retained.
#'   * `NULL` (default) — inherits from the `.space_mode` argument of
#'     [fr_cols()], which defaults to `"indent"`.
#' @param n Per-column subject count. A non-negative integer scalar
#'   (e.g. `n = 45`). Formatted into the column label at render time
#'   using the `.n_format` template from [fr_cols()]. Takes **highest
#'   priority** in N-count resolution — overrides bulk `.n` from
#'   [fr_cols()]. Use for the common case where you know each column's
#'   N at definition time. `NULL` (default) means no per-column N.
#' @param group Character scalar. Assigns this column to a **spanning
#'   header group**. All columns sharing the same `group` value get an
#'   auto-generated span at level 1, ordered by first column appearance.
#'   This replaces [fr_spans()] for the 90\% case — define your column
#'   structure and grouping in one place.
#'
#'   Rules:
#'   * Single-column groups create single-column sub-headers.
#'   * Explicit [fr_spans()] at the same label overrides the auto-span.
#'   * [fr_spans()] at `.level = 2L` adds higher-level spans above.
#'   * `NULL` (default) means no spanning group.
#'
#' @return An S3 object of class `fr_col` with components `id`, `label`,
#'   `width`, `align`, `header_align`, `visible`, `stub`, `space_mode`, `n`,
#'   and `group`.
#'
#' @section Width guidelines:
#' Landscape Letter paper (11 × 8.5 in) with 1 in margins gives **9 in**
#' of printable width. A common pharma layout:
#'
#' | Column type | Typical width |
#' |-------------|---------------|
#' | Stub / row label | 2.0–3.0 in |
#' | Treatment arm (n, %) | 1.2–1.5 in |
#' | P-value | 0.8–1.0 in |
#' | Total column | 1.2–1.5 in |
#'
#' Use `width = "auto"` to let the layout engine calculate these from
#' content, or set fixed widths for exact control.
#'
#' @section Alignment conventions:
#' Standard pharma house styles (Roche, Novartis, Pfizer TFL guides):
#' * **Stub / parameter column**: left-aligned.
#' * **Count and percentage columns**: right-aligned or decimal-aligned.
#' * **P-value columns**: right-aligned or decimal-aligned.
#' * **Category columns** (Yes/No, Male/Female): centered or left-aligned.
#'
#' @section N-count precedence:
#' When multiple N-count sources are present, the highest-priority
#' source wins (no double-apply):
#'
#' | Priority | Source |
#' |----------|--------|
#' | 1 (highest) | `fr_col(n = 45)` — per-column scalar |
#' | 2 | `fr_cols(.n = c("Placebo" = 45))` — bulk named vector |
#' | 3 | `fr_cols(.n = data.frame(...))` — data frame form |
#' | 4 (lowest) | `fr_cols(.n = list(...))` — named list form |
#'
#' @section Spanning groups:
#' The `group` parameter provides inline spanning — no separate
#' [fr_spans()] call needed. Columns with the same `group` value are
#' grouped under a single spanning header:
#'
#' ```r
#' fr_cols(
#'   stat      = fr_col("Statistic", width = 1.5),
#'   pbo_base  = fr_col("Baseline", group = "Placebo"),
#'   pbo_val   = fr_col("Value",    group = "Placebo"),
#'   drg_base  = fr_col("Baseline", group = "Zomerane"),
#'   drg_val   = fr_col("Value",    group = "Zomerane")
#' )
#' ```
#'
#' @examples
#' ## ── Fixed width with explicit alignment ──────────────────────────────────
#'
#' fr_col("Parameter", width = 2.5, align = "left")
#'
#' ## ── Percentage width: 25% of printable area ──────────────────────────────
#'
#' fr_col("Parameter", width = "25%")
#'
#' ## ── Centered header over right-aligned body ──────────────────────────────
#'
#' fr_col("Zomerane 50mg", width = 1.5, align = "right", header_align = "center")
#'
#' ## ── Per-column N-count ───────────────────────────────────────────────────
#'
#' fr_col("Placebo", n = 45)
#'
#' ## ── N-count + spanning group ─────────────────────────────────────────────
#'
#' fr_col("Baseline", group = "Placebo")
#'
#' ## ── Percentage width in a pipeline ───────────────────────────────────────
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
#' ## ── Auto-width: let the engine measure content ───────────────────────────
#'
#' fr_col("Parameter", width = "auto")
#'
#' ## ── Markup in label: superscript unit ────────────────────────────────────
#'
#' fr_col("BMI (kg/m{fr_super('2')})", width = 1.5, align = "decimal")
#'
#' ## ── Decimal alignment for continuous statistics ──────────────────────────
#'
#' fr_col("Mean (SD)", width = 1.5, align = "decimal")
#'
#' ## ── Hidden structural column (grouping key for fr_rows) ─────────────────
#'
#' fr_col(visible = FALSE)
#'
#' ## ── Stub column (repeats in every panel during column splitting) ────────
#'
#' fr_col("Parameter", width = 2.5, stub = TRUE)
#'
#' ## ── Spanning group: columns grouped under one header ─────────────────────
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
#' ## ── Per-column N-counts in a pipeline ────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_cols(
#'     characteristic = fr_col("Characteristic", width = 2.5),
#'     zom_50mg       = fr_col("Zomerane 50 mg", n = 45),
#'     placebo        = fr_col("Placebo",         n = 45),
#'     .n_format = "{label}\n(N={n})"
#'   )
#'
#' ## ── Space mode: preserve leading spaces as literal characters ──────────
#'
#' fr_col("Statistic", space_mode = "preserve")
#'
#' ## ── Space mode: explicit indent mode (default, but can be stated) ─────
#'
#' fr_col("Characteristic", width = 2.5, space_mode = "indent")
#'
#' @section Precedence:
#' Body alignment: `fr_cols(.align=)` < `fr_col(align=)` <
#' `fr_col_style(align=)` < `fr_style(align=)`
#'
#' Header alignment: `fr_header(align=)` < `fr_col(header_align=)` <
#' `fr_style(region="header", align=)`
#'
#' @seealso [fr_cols()] to apply column specs to a table (including
#'   `.n`, `.n_format`, and `.split` for N-counts, formatting, and
#'   column splitting),
#'   [fr_header()] for header presentation (bold, colours, alignment),
#'   [fr_spans()] for advanced multi-level spanning headers,
#'   [fr_super()], [fr_bold()], [fr_italic()] for inline markup in labels.
#'
#' @export
fr_col <- function(
  label = "",
  width = NULL,
  align = NULL,
  header_align = NULL,
  visible = NULL,
  stub = FALSE,
  space_mode = NULL,
  n = NULL,
  group = NULL
) {
  check_scalar_chr(label, arg = "label")
  if (!is.null(width)) {
    if (is.character(width)) {
      pct <- parse_pct_width(width, arg = "width")
      if (!is.null(pct)) {
        width <- pct
      } else {
        width <- match_arg_fr(width, "auto")
      }
    } else {
      check_positive_num(width, arg = "width")
    }
  }
  if (!is.null(align)) {
    align <- match_arg_fr(align, fr_env$valid_aligns)
  }
  if (!is.null(header_align)) {
    header_align <- match_arg_fr(header_align, fr_env$valid_aligns)
  }
  if (!is.null(visible)) {
    check_scalar_lgl(visible, arg = "visible")
  }
  check_scalar_lgl(stub, arg = "stub")
  if (!is.null(space_mode)) {
    space_mode <- match_arg_fr(space_mode, fr_env$valid_space_modes)
  }
  if (!is.null(n)) {
    n <- check_non_negative_int(n, arg = "n")
  }
  if (!is.null(group)) {
    check_scalar_chr(group, arg = "group")
  }

  structure(
    list(
      id = "",
      label = label,
      width = width,
      align = align,
      header_align = header_align,
      visible = visible,
      stub = stub,
      space_mode = space_mode,
      n = n,
      group = group
    ),
    class = "fr_col"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_span — Spanning header (internal)
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
new_fr_span <- function(label, columns, level = 1L, hline = TRUE) {
  check_scalar_chr(label, arg = "label")

  structure(
    list(
      label = label,
      columns = vec_cast(columns, character()),
      level = vec_cast(level, integer()),
      hline = vec_cast(hline, logical())
    ),
    class = "fr_span"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_header — Column header configuration (internal)
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
new_fr_header <- function(
  spans = list(),
  repeat_on_page = TRUE,
  valign = "bottom",
  align = NULL,
  bold = NULL,
  background = NULL,
  color = NULL,
  font_size = NULL,
  span_gap = TRUE
) {
  structure(
    list(
      spans = spans,
      repeat_on_page = repeat_on_page,
      valign = valign,
      align = align,
      bold = bold,
      background = background,
      color = color,
      font_size = font_size,
      span_gap = span_gap
    ),
    class = "fr_header"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_body — Row grouping and pagination control (internal)
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
new_fr_body <- function(
  page_by = character(0),
  group_by = character(0),
  indent_by = character(0),
  blank_after = character(0),
  page_by_visible = TRUE,
  group_label = NULL,
  group_keep = TRUE,
  group_style = NULL,
  group_leaf = NULL,
  group_hierarchy_cols = NULL,
  sort_by = character(0),
  suppress = character(0),
  wrap = FALSE
) {
  structure(
    list(
      page_by = vec_cast(page_by, character()),
      group_by = vec_cast(group_by, character()),
      indent_by = if (is.list(indent_by)) {
        indent_by
      } else {
        vec_cast(indent_by, character())
      },
      blank_after = vec_cast(blank_after, character()),
      page_by_visible = page_by_visible,
      group_label = group_label,
      group_keep = group_keep,
      group_style = group_style,
      group_leaf = group_leaf,
      group_hierarchy_cols = group_hierarchy_cols,
      sort_by = vec_cast(sort_by, character()),
      suppress = vec_cast(suppress, character()),
      wrap = wrap
    ),
    class = "fr_body"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_rule — A single horizontal or vertical rule (internal)
#
# FIELDS:
#
#   direction   "horizontal" | "vertical"
#
#   region      "header"   — column header row(s) + spanner row(s)
#               "body"     — data rows
#               "spanners" — spanner rows only
#
#   side        "above" | "below"  (horizontal rules only)
#
#   rows        NULL = side-based positioning
#               "all" = rule below every row in the region
#               integer vector = specific row index(es)
#
#   cols        NULL = all columns
#               "all" = all columns
#               integer vector = specific column index(es)
#
#   width       Rule thickness in points. Resolved from named shorthand or
#               numeric by resolve_line_width().
#
#   linestyle   "solid" | "dashed" | "dotted" | "double" | "dashdot"
#               CSS + SAS ODS vocabulary.
#
#   fg          Rule colour, hex string. Default "#000000".
#
#   leftpos / rightpos   (horizontal rules only)
#               Fraction 0–1 controlling partial rule extent.
#               NULL = full width. Controls partial horizontal extent.
#
#   abovepos / belowpos  (vertical rules only)
#               Fraction 0–1 controlling partial rule extent.
#               NULL = full height. Controls partial vertical extent.
#
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
new_fr_rule <- function(
  direction = "horizontal",
  region = "header",
  side = "below",
  rows = NULL,
  cols = NULL,
  width = 0.5,
  linestyle = "solid",
  fg = "#000000",
  leftpos = NULL,
  rightpos = NULL,
  abovepos = NULL,
  belowpos = NULL,
  call = caller_env()
) {
  direction <- match_arg_fr(direction, c("horizontal", "vertical"), call = call)
  region <- match_arg_fr(region, c("header", "body", "spanners"), call = call)
  side <- match_arg_fr(side, c("above", "below"), call = call)

  # ── Validate rows ───────────────────────────────────────────────────────
  if (!is.null(rows)) {
    if (is.character(rows)) {
      rows <- match_arg_fr(rows, "all", call = call)
    } else if (is.numeric(rows)) {
      if (any(rows < 1L) || any(rows != as.integer(rows))) {
        cli_abort(
          c(
            "{.arg rows} must be positive integers or {.val all}.",
            "x" = "You supplied {.val {rows}}."
          ),
          call = call
        )
      }
      rows <- as.integer(rows)
    } else {
      cli_abort(
        c(
          "{.arg rows} must be {.val all}, an integer vector, or {.code NULL}.",
          "x" = "You supplied {.obj_type_friendly {rows}}."
        ),
        call = call
      )
    }
  }

  # ── Validate cols ───────────────────────────────────────────────────────
  if (!is.null(cols)) {
    if (is.character(cols)) {
      cols <- match_arg_fr(cols, "all", call = call)
    } else if (is.numeric(cols)) {
      if (any(cols < 1L) || any(cols != as.integer(cols))) {
        cli_abort(
          c(
            "{.arg cols} must be positive integers or {.val all}.",
            "x" = "You supplied {.val {cols}}."
          ),
          call = call
        )
      }
      cols <- as.integer(cols)
    } else {
      cli_abort(
        c(
          "{.arg cols} must be {.val all}, an integer vector, or {.code NULL}.",
          "x" = "You supplied {.obj_type_friendly {cols}}."
        ),
        call = call
      )
    }
  }

  linestyle <- match_arg_fr(linestyle, fr_env$valid_linestyles, call = call)
  width <- resolve_line_width(width, call = call)

  if (!is.null(fg)) {
    fg <- resolve_color(fg, call = call)
  }

  # ── Validate partial line fractions ────────────────────────────────────
  validate_fraction <- function(x, arg) {
    if (is.null(x)) {
      return(NULL)
    }
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x > 1) {
      cli_abort(
        c(
          "{.arg {arg}} must be a number in [0, 1] or {.code NULL}.",
          "x" = "You supplied {.val {x}}."
        ),
        call = call
      )
    }
    x
  }
  leftpos <- validate_fraction(leftpos, "leftpos")
  rightpos <- validate_fraction(rightpos, "rightpos")
  abovepos <- validate_fraction(abovepos, "abovepos")
  belowpos <- validate_fraction(belowpos, "belowpos")

  structure(
    list(
      direction = direction,
      region = region,
      side = side,
      rows = rows,
      cols = cols,
      width = width,
      linestyle = linestyle,
      fg = fg %||% "#000000",
      leftpos = leftpos,
      rightpos = rightpos,
      abovepos = abovepos,
      belowpos = belowpos
    ),
    class = c(
      if (direction == "horizontal") "fr_rule_hline" else "fr_rule_vline",
      "fr_rule"
    )
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_cell_style — Cell-level style override (internal)
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
new_fr_cell_style <- function(
  type = "cell",
  region = "body",
  rows = NULL,
  cols = NULL,
  bold = NULL,
  italic = NULL,
  underline = NULL,
  color = NULL,
  background = NULL,
  font = NULL,
  font_size = NULL,
  align = NULL,
  valign = NULL,
  indent = NULL,
  colspan = NULL,
  rowspan = NULL,
  height = NULL
) {
  structure(
    list(
      type = type,
      region = region,
      rows = rows,
      cols = cols,
      bold = bold,
      italic = italic,
      underline = underline,
      color = if (!is.null(color)) resolve_color(color) else NULL,
      background = if (!is.null(background)) {
        resolve_color(background)
      } else {
        NULL
      },
      font = font,
      font_size = font_size,
      align = align,
      valign = valign,
      indent = indent,
      colspan = colspan,
      rowspan = rowspan,
      height = height
    ),
    class = "fr_cell_style"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_row_style / fr_col_style — Row- and column-level style overrides (internal)
#
# These are thin wrappers over new_fr_cell_style() that set type = "row" or
# type = "col" and restrict the available fields accordingly.
#
# Backend key mapping:
#   type = "row"  → row{i} = {background, color, font, halign, valign, ht, ...}
#   type = "col"  → column{j} = {background, color, font, halign, wd}
#
# RTF mapping:
#   type = "row"  → per-row: \trrh (height), \clcbpat (background), \cf (color), \b\i\ul
#   type = "col"  → per-column: applied to every cell in the column
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
new_fr_row_style <- function(
  rows = NULL,
  bold = NULL,
  italic = NULL,
  underline = NULL,
  color = NULL,
  background = NULL,
  font_size = NULL,
  align = NULL,
  valign = NULL,
  height = NULL
) {
  new_fr_cell_style(
    type = "row",
    region = "body",
    rows = rows,
    cols = NULL,
    bold = bold,
    italic = italic,
    underline = underline,
    color = color,
    background = background,
    font_size = font_size,
    align = align,
    valign = valign,
    height = height
  )
}


#' @noRd
new_fr_col_style <- function(
  cols = NULL,
  bold = NULL,
  italic = NULL,
  underline = NULL,
  color = NULL,
  background = NULL,
  font_size = NULL,
  align = NULL,
  valign = NULL
) {
  new_fr_cell_style(
    type = "col",
    region = "body",
    rows = NULL,
    cols = cols,
    bold = bold,
    italic = italic,
    underline = underline,
    color = color,
    background = background,
    font_size = font_size,
    align = align,
    valign = valign
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_meta — Titles and footnotes (internal)
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
new_fr_meta <- function(
  titles = list(),
  footnotes = list(),
  footnote_separator = FALSE
) {
  structure(
    list(
      titles = as.list(titles),
      footnotes = as.list(footnotes),
      footnote_separator = footnote_separator
    ),
    class = "fr_meta"
  )
}


#' Create a title entry
#'
#' Each title line is stored with its own alignment and styling.
#'
#' @param content Character. Title text (supports `{fr_*()}` markup).
#' @param align Character. `"left"`, `"center"`, or `"right"`.
#' @param bold Logical.
#' @param font_size Numeric or NULL (inherit from page).
#' @return A named list (lightweight data container, not an S3 class).
#' @noRd
new_title_entry <- function(
  content,
  align = "center",
  bold = FALSE,
  font_size = NULL
) {
  structure(
    list(
      content = content,
      align = match_arg_fr(align, c("left", "center", "right")),
      bold = bold,
      font_size = font_size
    ),
    class = "fr_title_entry"
  )
}

#' @export
print.fr_title_entry <- function(x, ...) {
  style <- if (x$bold) " [bold]" else ""
  size <- if (!is.null(x$font_size)) paste0(" ", x$font_size, "pt") else ""
  cat(sprintf(
    "<fr_title_entry> (%s%s%s) %s\n",
    x$align,
    style,
    size,
    label_to_plain(x$content)
  ))
  invisible(x)
}


#' Create a footnote entry
#'
#' @param content Character.
#' @param align Character.
#' @param placement Character. `"every"` (default) or `"last"`.
#' @param font_size Numeric or NULL.
#' @return A named list.
#' @noRd
new_footnote_entry <- function(
  content,
  align = "left",
  placement = "every",
  font_size = NULL
) {
  structure(
    list(
      content = content,
      align = match_arg_fr(align, c("left", "center", "right")),
      placement = match_arg_fr(placement, c("every", "last")),
      font_size = font_size
    ),
    class = "fr_footnote_entry"
  )
}

#' @export
print.fr_footnote_entry <- function(x, ...) {
  place <- if (x$placement == "last") " [last]" else ""
  size <- if (!is.null(x$font_size)) paste0(" ", x$font_size, "pt") else ""
  cat(sprintf(
    "<fr_footnote_entry> (%s%s%s) %s\n",
    x$align,
    place,
    size,
    label_to_plain(x$content)
  ))
  invisible(x)
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_pagechrome — Page header/footer definition (internal)
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
new_fr_pagechrome <- function(
  left = NULL,
  center = NULL,
  right = NULL,
  font_size = NULL,
  bold = NULL
) {
  structure(
    list(
      left = left,
      center = center,
      right = right,
      font_size = font_size,
      bold = bold
    ),
    class = "fr_pagechrome"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_spec — Root specification object (internal)
#
# Created by fr_table() in api.R, modified by pipeline verbs,
# consumed by fr_render().
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
new_fr_spec <- function(
  data,
  meta = new_fr_meta(),
  columns = list(),
  columns_meta = list(
    split = FALSE,
    width_mode = "auto",
    space_mode = "indent",
    n_format = fr_env$default_n_format
  ),
  header = new_fr_header(),
  body = new_fr_body(),
  rules = list(),
  cell_styles = list(),
  page_by_styles = list(),
  page = new_fr_page(),
  pagehead = NULL,
  pagefoot = NULL,
  spacing = list(
    titles_after = 1L,
    footnotes_before = 1L,
    pagehead_after = 0L,
    pagefoot_before = 0L,
    page_by_after = 1L
  ),
  type = "table",
  plot = NULL,
  plots = NULL,
  figure_meta = NULL,
  call = caller_env()
) {
  if (!is.data.frame(data)) {
    cli_abort(
      c(
        "{.arg data} must be a data frame.",
        "x" = "You supplied {.obj_type_friendly {data}}.",
        "i" = "Example: {.code my_df |> fr_table()}"
      ),
      call = call
    )
  }

  structure(
    list(
      data = data,
      meta = meta,
      columns = columns,
      columns_meta = columns_meta,
      header = header,
      body = body,
      rules = rules,
      cell_styles = cell_styles,
      page_by_styles = page_by_styles,
      page = page,
      pagehead = pagehead,
      pagefoot = pagefoot,
      spacing = spacing,
      type = type,
      plot = plot,
      plots = plots,
      figure_meta = figure_meta
    ),
    class = "fr_spec"
  )
}


#' @export
print.fr_spec <- function(x, ..., compact = FALSE) {
  # Auto-preview in Viewer panel (RStudio, Positron, or any IDE with viewer)
  viewer <- getOption("viewer")
  if (interactive() && !is.null(viewer)) {
    tryCatch(
      {
        tmp <- tempfile(fileext = ".html")
        x$.viewer <- TRUE
        fr_render(x, tmp)
        if (requireNamespace("htmltools", quietly = TRUE)) {
          html_content <- paste0(readLines(tmp, warn = FALSE), collapse = "\n")
          widget <- htmltools::browsable(htmltools::HTML(html_content))
          print(widget)
        } else {
          viewer(tmp)
        }
      },
      error = function(e) {
        cli::cli_warn(c(
          "!" = "HTML preview failed.",
          "i" = conditionMessage(e)
        ), call = caller_env())
        NULL
      }
    )
    return(invisible(x))
  }

  if (isTRUE(compact)) {
    nr <- nrow(x$data)
    nc <- length(x$columns)
    orient <- x$page$orientation %||% "landscape"
    n_titles <- length(x$meta$titles %||% list())
    n_fn <- length(x$meta$footnotes %||% list())
    cat(sprintf("<fr_spec> %d rows x %d columns [%s]", nr, nc, orient))
    if (n_titles > 0L || n_fn > 0L) {
      parts <- character(0)
      if (n_titles > 0L) {
        parts <- c(parts, sprintf("%d title(s)", n_titles))
      }
      if (n_fn > 0L) {
        parts <- c(parts, sprintf("%d footnote(s)", n_fn))
      }
      cat(" | ", paste(parts, collapse = ", "))
    }
    cat("\n")
    return(invisible(x))
  }

  # Rich tree view
  type_label <- x$type %||% "Table"
  type_label <- paste0(
    toupper(substr(type_label, 1, 1)),
    substring(type_label, 2)
  )
  cli::cli_h3("fr_spec: {type_label}")

  # Data summary (skip for figures)
  nr <- nrow(x$data)
  nc_data <- ncol(x$data)
  nc_spec <- length(x$columns)
  if (!identical(x$type, "figure")) {
    cli::cli_text("Data: {nr} row{?s} x {nc_data} column{?s}")
  }

  # Figure info
  if (identical(x$type, "figure")) {
    n_plots <- if (!is.null(x$plots)) length(x$plots) else 1L
    cli::cli_text("Plot{?s}: {n_plots} page{?s}")
    if (!is.null(x$figure_meta)) {
      meta_cols <- paste(names(x$figure_meta), collapse = ", ")
      cli::cli_text("Meta: {meta_cols}")
    }
  }

  # Page config
  orient <- x$page$orientation %||% "landscape"
  paper <- x$page$paper %||% "letter"
  fs <- x$page$font_size %||% 9
  font <- x$page$font_family %||% "Times New Roman"
  cg <- x$page$col_gap %||% 4L
  cg_str <- if (cg != 4L) paste0(", col_gap=", cg, "pt") else ""
  cli::cli_text("Page: {orient} {paper}, {fs}pt {font}{cg_str}")

  # Titles
  titles <- x$meta$titles %||% list()
  if (length(titles) > 0L) {
    cli::cli_text("Titles ({length(titles)}):")
    for (i in seq_along(titles)) {
      t <- titles[[i]]
      txt <- label_to_plain(t$content)
      if (nchar(txt) > 60L) {
        txt <- paste0(substr(txt, 1, 57), "...")
      }
      cli::cli_text("  {i}. [{t$align}] {.val {txt}}")
    }
  }

  # Columns
  if (nc_spec > 0L) {
    vis_cols <- visible_columns(x$columns)
    cli::cli_text("Columns ({length(vis_cols)} visible of {nc_spec}):")
    show_n <- min(length(vis_cols), 8L)
    nms <- names(vis_cols)
    for (i in seq_len(show_n)) {
      col <- vis_cols[[i]]
      lbl <- label_to_plain(col$label %||% nms[i])
      if (nchar(lbl) > 20L) {
        lbl <- paste0(substr(lbl, 1, 17), "...")
      }
      w <- if (is_fr_pct(col$width)) {
        sprintf("%.0f%%", unclass(col$width) * 100)
      } else if (is.numeric(col$width)) {
        sprintf("%.2fin", col$width)
      } else {
        "auto"
      }
      a <- col$align %||% "left"
      cli::cli_text("  {nms[i]}  {.val {lbl}}  {w}  {a}")
    }
    if (length(vis_cols) > show_n) {
      cli::cli_text("  ... and {length(vis_cols) - show_n} more")
    }
  }

  # Header
  h <- x$header
  if (!is.null(h)) {
    parts <- character(0)
    if (isTRUE(h$bold)) {
      parts <- c(parts, "bold")
    }
    if (!is.null(h$valign)) {
      parts <- c(parts, paste0("valign=", h$valign))
    }
    if (!is.null(h$align)) {
      parts <- c(parts, paste0("align=", h$align))
    }
    if (length(parts) > 0L) {
      cli::cli_text("Header: {paste(parts, collapse = ', ')}")
    }
  }

  # Rows config
  b <- x$body
  if (!is.null(b)) {
    parts <- character(0)
    if (length(b$page_by) > 0L) {
      pb_str <- paste0("page_by=", paste(b$page_by, collapse = ","))
      if (!isTRUE(b$page_by_visible)) {
        pb_str <- paste0(pb_str, " (visible=FALSE)")
      }
      parts <- c(parts, pb_str)
    }
    if (length(b$group_by) > 0L) {
      gb_str <- paste0("group_by=", paste(b$group_by, collapse = ","))
      if (!is.null(b$group_label)) {
        gb_str <- paste0(gb_str, " (label=", b$group_label, ")")
      }
      parts <- c(parts, gb_str)
    }
    if (length(b$sort_by) > 0L) {
      parts <- c(parts, paste0("sort_by=", paste(b$sort_by, collapse = ",")))
    }
    if (length(b$indent_by) > 0L) {
      parts <- c(
        parts,
        paste0("indent_by=", paste(b$indent_by, collapse = ","))
      )
    }
    if (isTRUE(b$suppress)) {
      parts <- c(parts, "suppress")
    }
    if (isTRUE(b$wrap)) {
      parts <- c(parts, "wrap")
    }
    if (length(parts) > 0L) {
      cli::cli_text("Rows: {paste(parts, collapse = ', ')}")
    }
  }

  # Rules
  if (length(x$rules) > 0L) {
    n_h <- sum(vapply(
      x$rules,
      function(r) inherits(r, "fr_rule_hline"),
      logical(1)
    ))
    n_v <- sum(vapply(
      x$rules,
      function(r) inherits(r, "fr_rule_vline"),
      logical(1)
    ))
    parts <- character(0)
    if (n_h > 0L) {
      parts <- c(parts, paste0(n_h, " hline(s)"))
    }
    if (n_v > 0L) {
      parts <- c(parts, paste0(n_v, " vline(s)"))
    }
    cli::cli_text("Rules: {paste(parts, collapse = ', ')}")
  }

  # Spans
  spans <- x$header$spans %||% list()
  if (length(spans) > 0L) {
    cli::cli_text("Spans: {length(spans)}")
  }

  # Styles
  if (length(x$cell_styles) > 0L) {
    cli::cli_text("Styles: {length(x$cell_styles)} override{?s}")
  }

  # Footnotes
  footnotes <- x$meta$footnotes %||% list()
  if (length(footnotes) > 0L) {
    cli::cli_text("Footnotes ({length(footnotes)}):")
    for (i in seq_along(footnotes)) {
      fn <- footnotes[[i]]
      txt <- label_to_plain(fn$content)
      if (nchar(txt) > 60L) {
        txt <- paste0(substr(txt, 1, 57), "...")
      }
      place <- if (fn$placement == "last") " [last]" else ""
      cli::cli_text("  {i}. [{fn$align}{place}] {.val {txt}}")
    }
  }

  # Plot (figure)
  if (!is.null(x$plot)) {
    cli::cli_text("Plot: {class(x$plot)[1L]}")
  }

  invisible(x)
}


#' Format an fr_spec as a compact one-liner
#'
#' Returns a concise string summary useful for logging, debugging, or
#' `sprintf()` interpolation.
#'
#' @param x An `fr_spec` object.
#' @param ... Ignored.
#' @return Character scalar describing the spec.
#'
#' @examples
#' spec <- tbl_demog |> fr_table()
#' format(spec)
#' sprintf("Processing: %s", format(spec))
#'
#' @export
format.fr_spec <- function(x, ...) {
  nr <- nrow(x$data)
  nc <- ncol(x$data)
  type <- x$type %||% "table"
  n_styles <- length(x$cell_styles)
  n_titles <- length(x$meta$titles)
  sprintf(
    "<fr_spec> %s: %d rows x %d cols, %d style(s), %d title(s)",
    type, nr, nc, n_styles, n_titles
  )
}


#' @export
summary.fr_spec <- function(object, ...) {
  print(object, ...)
}


#' Render fr_spec as inline HTML in knitr documents
#'
#' Enables automatic rendering of `fr_spec` objects in R Markdown, Quarto,
#' and pkgdown vignettes — just like gt tables render inline.
#'
#' @param x An `fr_spec` object.
#' @param ... Ignored.
#' @return An htmltools tag object, rendered by knitr natively.
#' @exportS3Method knitr::knit_print
knit_print.fr_spec <- function(x, ...) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(invisible(x))
  }

  # Finalize + render to HTML fragment (not a full document)
  if (identical(x$type, "figure")) {
    # Figures need the full render path — use temp file, then strip wrapper
    tmp <- tempfile(fileext = ".html")
    on.exit(unlink(tmp), add = TRUE)
    x$.viewer <- TRUE
    x$.knitr <- TRUE
    fr_render(x, tmp)
    html <- paste0(readLines(tmp, warn = FALSE), collapse = "\n")
    # Strip the full document wrapper — extract body content
    html <- sub(".*<body>\\s*", "", html)
    html <- sub("\\s*</body>.*", "", html)
    # Wrap in htmltools tag (gt-style) for pkgdown compatibility
    uid <- paste0(
      "arframe-fig-",
      format(
        as.integer(Sys.time()) * 1000 + sample(999, 1),
        scientific = FALSE
      )
    )
    fig_tag <- htmltools::tags$div(
      id = uid,
      style = htmltools::css(
        `max-width` = "100%",
        `overflow-x` = "auto"
      ),
      htmltools::HTML(html)
    )
    return(knitr::knit_print(fig_tag))
  }

  spec <- finalize_spec(x)
  page_groups <- prepare_pages(spec)
  col_panels <- compute_col_panels(spec)

  if (isTRUE(spec$columns_meta$split) && length(col_panels) > 1L) {
    wm <- spec$columns_meta$width_mode
    if (identical(wm, "fit")) {
      spec <- fit_panel_widths(spec, col_panels)
    } else if (identical(wm, "equal")) {
      spec <- equal_panel_widths(spec, col_panels)
    }
  }

  total_sections <- length(col_panels) * length(page_groups)
  section_idx <- 0L
  sections <- vector("list", total_sections)

  for (group_idx in seq_along(page_groups)) {
    group <- page_groups[[group_idx]]
    for (panel_idx in seq_along(col_panels)) {
      panel_cols <- col_panels[[panel_idx]]
      vis_columns <- spec$columns[intersect(panel_cols, names(spec$columns))]
      section_idx <- section_idx + 1L
      is_last <- (group_idx == length(page_groups) &&
        panel_idx == length(col_panels))

      nrow_header <- 1L + n_spanner_levels(spec$header$spans)
      borders <- resolve_borders(
        spec$rules,
        nrow(group$data),
        length(vis_columns),
        nrow_header
      )

      if (!is.null(group$label_overrides) || !is.null(group$span_overrides)) {
        label_overrides <- group$label_overrides
        span_overrides <- group$span_overrides
      } else {
        resolved <- resolve_group_labels(spec, group$data, group$group_label)
        if (is.list(resolved)) {
          label_overrides <- resolved$columns
          span_overrides <- resolved$spans
        } else {
          label_overrides <- resolved
          span_overrides <- NULL
        }
      }

      cell_grid <- build_cell_grid(
        group$data,
        vis_columns,
        spec$cell_styles,
        spec$page
      )

      token_map <- build_token_map(
        page_num = section_idx,
        total_pages = total_sections,
        spec = spec
      )

      sections[[section_idx]] <- html_section(
        spec,
        group,
        vis_columns,
        cell_grid,
        borders,
        label_overrides,
        span_overrides,
        token_map,
        panel_idx,
        is_last
      )
    }
  }

  body <- paste0(sections, collapse = "\n")
  # Returns an htmltools tag object (like gt) — knitr handles it natively
  html_tag <- html_fragment(body, spec)
  knitr::knit_print(html_tag)
}


#' @export
print.fr_col <- function(x, ...) {
  label <- x$label %||% x$id %||% ""
  width <- if (is_fr_pct(x$width)) {
    sprintf("%.0f%%", unclass(x$width) * 100)
  } else if (is.numeric(x$width)) {
    sprintf("%.2fin", x$width)
  } else {
    "auto"
  }
  align <- x$align %||% "left"
  stub <- if (isTRUE(x$stub)) " stub" else ""
  n_tag <- if (!is.null(x$n)) sprintf(" N=%d", x$n) else ""
  grp_tag <- if (!is.null(x$group)) sprintf(" group=%s", x$group) else ""
  cat(sprintf(
    "<fr_col> \"%s\" [%s, %s%s%s%s]\n",
    label,
    width,
    align,
    stub,
    n_tag,
    grp_tag
  ))
  invisible(x)
}


# ══════════════════════════════════════════════════════════════════════════════
# Validators
# ══════════════════════════════════════════════════════════════════════════════

#' Test if an Object is an fr_spec
#'
#' @param x An object to test.
#' @return `TRUE` if `x` inherits from `"fr_spec"`, `FALSE` otherwise.
#'
#' @examples
#' spec <- tbl_demog |> fr_table()
#' is.fr_spec(spec)     # TRUE
#' is.fr_spec(mtcars)   # FALSE
#'
#' @export
is.fr_spec <- function(x) inherits(x, "fr_spec")


#' Test if an Object is an fr_col
#'
#' @param x An object to test.
#' @return `TRUE` if `x` inherits from `"fr_col"`, `FALSE` otherwise.
#'
#' @examples
#' col <- fr_col(label = "Treatment", align = "center")
#' is.fr_col(col)       # TRUE
#' is.fr_col("text")    # FALSE
#'
#' @export
is.fr_col <- function(x) inherits(x, "fr_col")


#' Validate that an object is an fr_spec
#' @noRd
check_fr_spec <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!inherits(x, "fr_spec")) {
    cli_abort(
      c(
        "{.arg {arg}} must be an {.cls fr_spec} object (created by {.fn fr_table}).",
        "x" = "You supplied {.obj_type_friendly {x}}."
      ),
      arg = arg,
      call = call
    )
  }
  invisible(x)
}

#' Validate that an object is an fr_col
#' @noRd
check_fr_col <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!inherits(x, "fr_col")) {
    cli_abort(
      c(
        "{.arg {arg}} must be an {.cls fr_col} object (created by {.fn fr_col}).",
        "x" = "You supplied {.obj_type_friendly {x}}."
      ),
      arg = arg,
      call = call
    )
  }
  invisible(x)
}
