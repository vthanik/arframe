# Define a Column Specification

Defines the display properties of a single table column: its label,
width, alignment, visibility, N-count, and spanning group. Used inside
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md) as
a named argument to configure individual columns. Columns not explicitly
configured receive auto-generated defaults from the `.width`, `.align`,
and `.label_fn` arguments of
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md).

`fr_col()` is the **single source of truth** for column structure.
Labels, widths, alignment, N-counts, and spanning groups are all defined
here.
[`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md)
is purely for header *presentation* (bold, colours, font).

## Usage

``` r
fr_col(
  label = "",
  width = NULL,
  align = NULL,
  header_align = NULL,
  visible = NULL,
  stub = FALSE,
  space_mode = NULL,
  n = NULL,
  group = NULL
)
```

## Arguments

- label:

  Character scalar. Display label shown in the column header. Supports
  `{fr_*()}` inline markup (e.g. `"{fr_super('a')}"` for superscript
  footnote markers). Default `""` inherits the data frame column name as
  the label, optionally transformed by the `.label_fn` argument of
  [`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md).

- width:

  Column width. Accepts:

  - **Numeric** — fixed width in inches (e.g. `2.5`). Use for stub
    columns or when you need exact control.

  - **`"auto"`** — auto-calculate from content and header widths using
    the page font metrics. The layout engine measures the widest cell
    value and the header label, adds padding, and converts to inches.
    Columns are then proportionally scaled to fit the printable page
    width. This is the fastest way to get a working table.

  - **Percentage string** — e.g. `"20%"`. Sets the column width as a
    fraction of the printable page width. Resolved to absolute inches at
    render time. Must be between `"0%"` (exclusive) and `"100%"`
    (inclusive). Useful for responsive layouts that adapt to different
    page sizes or orientations.

  - **`NULL`** (default) — inherits from the `.width` argument of
    [`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md).
    If `.width` is also `NULL`, the package default of 1.5 in is used.

- align:

  Column alignment. One of:

  - `"left"` — left-aligned. Standard for text and stub columns.

  - `"center"` — centered. Rare in regulatory tables; used for binary
    indicators (Yes/No) or short categorical values.

  - `"right"` — right-aligned. Standard for numeric columns (counts,
    percentages, p-values).

  - `"decimal"` — decimal-point alignment. Aligns the decimal point (or
    last digit for integers) across all rows in the column. Standard for
    continuous summary statistics (mean, SD, median).

  - `NULL` (default) — auto-detects from the R column type:
    numeric/integer → `"right"`, everything else → `"left"`.

- header_align:

  Horizontal alignment for the column header cell. One of `"left"`,
  `"center"`, `"right"`, `"decimal"`, or `NULL` (default). When `NULL`,
  the header inherits alignment from `align`. Use this to center
  treatment-arm headers while keeping body cells right-aligned. Can also
  be set uniformly via `fr_header(align = ...)`; per-column values here
  take priority. `fr_style(region = "header", align = ...)` overrides
  both.

- visible:

  Logical or `NULL`. Controls whether the column appears in the rendered
  output.

  - `NULL` (default): the system decides — columns used as
    [`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
    `page_by` keys are auto-hidden; all others are visible.

  - `TRUE`: force the column visible, even if it is a `page_by` key.

  - `FALSE`: hide the column. Useful for structural columns (grouping
    keys, sort-order columns, row-type flags) that should remain in the
    data for pagination and styling logic but not appear in output.

- stub:

  Logical. Whether this column is a **stub column** — repeated on every
  panel when `fr_cols(.split = TRUE)` splits the table across multiple
  column pages. Default `FALSE`. Stub columns are typically row-label or
  parameter columns that provide context for each panel. When `.split`
  is enabled but no columns have `stub = TRUE`, stubs are auto-inferred
  from `group_by`/`indent_by` columns or the first column.

- space_mode:

  How to handle leading spaces in cell data. One of:

  - `"indent"` — convert leading spaces to paragraph-level indent (RTF
    `\li`, LaTeX `\leftskip`). All lines (including wrapped) maintain
    the same indent. This is the correct approach for proportional fonts
    and pharma SOC/PT hierarchies.

  - `"preserve"` — keep leading spaces as literal characters. Use for
    pre-formatted content where exact spacing must be retained.

  - `NULL` (default) — inherits from the `.space_mode` argument of
    [`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md),
    which defaults to `"indent"`.

- n:

  Per-column subject count. A non-negative integer scalar (e.g.
  `n = 45`). Formatted into the column label at render time using the
  `.n_format` template from
  [`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md).
  Takes **highest priority** in N-count resolution — overrides bulk `.n`
  from
  [`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md).
  Use for the common case where you know each column's N at definition
  time. `NULL` (default) means no per-column N.

- group:

  Character scalar. Assigns this column to a **spanning header group**.
  All columns sharing the same `group` value get an auto-generated span
  at level 1, ordered by first column appearance. This replaces
  [`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
  for the 90\\ structure and grouping in one place.

  Rules:

  - Single-column groups create single-column sub-headers.

  - Explicit
    [`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
    at the same label overrides the auto-span.

  - [`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
    at `.level = 2L` adds higher-level spans above.

  - `NULL` (default) means no spanning group.

## Value

An S3 object of class `fr_col` with components `id`, `label`, `width`,
`align`, `header_align`, `visible`, `stub`, `space_mode`, `n`, and
`group`.

## Width guidelines

Landscape Letter paper (11 × 8.5 in) with 1 in margins gives **9 in** of
printable width. A common pharma layout:

|                      |               |
|----------------------|---------------|
| Column type          | Typical width |
| Stub / row label     | 2.0–3.0 in    |
| Treatment arm (n, %) | 1.2–1.5 in    |
| P-value              | 0.8–1.0 in    |
| Total column         | 1.2–1.5 in    |

Use `width = "auto"` to let the layout engine calculate these from
content, or set fixed widths for exact control.

## Alignment conventions

Standard pharma house styles (Roche, Novartis, Pfizer TFL guides):

- **Stub / parameter column**: left-aligned.

- **Count and percentage columns**: right-aligned or decimal-aligned.

- **P-value columns**: right-aligned or decimal-aligned.

- **Category columns** (Yes/No, Male/Female): centered or left-aligned.

## N-count precedence

When multiple N-count sources are present, the highest-priority source
wins (no double-apply):

|             |                                                       |
|-------------|-------------------------------------------------------|
| Priority    | Source                                                |
| 1 (highest) | `fr_col(n = 45)` — per-column scalar                  |
| 2           | `fr_cols(.n = c("Placebo" = 45))` — bulk named vector |
| 3           | `fr_cols(.n = data.frame(...))` — data frame form     |
| 4 (lowest)  | `fr_cols(.n = list(...))` — named list form           |

## Spanning groups

The `group` parameter provides inline spanning — no separate
[`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
call needed. Columns with the same `group` value are grouped under a
single spanning header:

    fr_cols(
      stat      = fr_col("Statistic", width = 1.5),
      pbo_base  = fr_col("Baseline", group = "Placebo"),
      pbo_val   = fr_col("Value",    group = "Placebo"),
      drg_base  = fr_col("Baseline", group = "Zomerane"),
      drg_val   = fr_col("Value",    group = "Zomerane")
    )

## Precedence

Body alignment: `fr_cols(.align=)` \< `fr_col(align=)` \<
`fr_col_style(align=)` \< `fr_style(align=)`

Header alignment: `fr_header(align=)` \< `fr_col(header_align=)` \<
`fr_style(region="header", align=)`

## See also

[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md) to
apply column specs to a table (including `.n`, `.n_format`, and `.split`
for N-counts, formatting, and column splitting),
[`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md)
for header presentation (bold, colours, alignment),
[`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
for advanced multi-level spanning headers,
[`fr_super()`](https://vthanik.github.io/arframe/reference/fr_super.md),
[`fr_bold()`](https://vthanik.github.io/arframe/reference/fr_bold.md),
[`fr_italic()`](https://vthanik.github.io/arframe/reference/fr_italic.md)
for inline markup in labels.

## Examples

``` r
## ── Fixed width with explicit alignment ──────────────────────────────────

fr_col("Parameter", width = 2.5, align = "left")
#> <fr_col> "Parameter" [2.50in, left]

## ── Percentage width: 25% of printable area ──────────────────────────────

fr_col("Parameter", width = "25%")
#> <fr_col> "Parameter" [25%, left]

## ── Centered header over right-aligned body ──────────────────────────────

fr_col("Zomerane 50mg", width = 1.5, align = "right", header_align = "center")
#> <fr_col> "Zomerane 50mg" [1.50in, right]

## ── Per-column N-count ───────────────────────────────────────────────────

fr_col("Placebo", n = 45)
#> <fr_col> "Placebo" [auto, left N=45]

## ── N-count + spanning group ─────────────────────────────────────────────

fr_col("Baseline", group = "Placebo")
#> <fr_col> "Baseline" [auto, left group=Placebo]

## ── Percentage width in a pipeline ───────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = "30%"),
    zom_50mg       = fr_col("Zomerane 50mg",  width = "17.5%", align = "right"),
    zom_100mg      = fr_col("Zomerane 100mg", width = "17.5%", align = "right"),
    placebo        = fr_col("Placebo",         width = "17.5%", align = "right"),
    total          = fr_col("Total",           width = "17.5%", align = "right")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 30% left
#> placebo "Placebo" 18% right
#> zom_50mg "Zomerane 50mg" 18% right
#> zom_100mg "Zomerane 100mg" 18% right
#> total "Total" 18% right
#> group "group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Auto-width: let the engine measure content ───────────────────────────

fr_col("Parameter", width = "auto")
#> <fr_col> "Parameter" [auto, left]

## ── Markup in label: superscript unit ────────────────────────────────────

fr_col("BMI (kg/m{fr_super('2')})", width = 1.5, align = "decimal")
#> <fr_col> "BMI (kg/m{fr_super('2')})" [1.50in, decimal]

## ── Decimal alignment for continuous statistics ──────────────────────────

fr_col("Mean (SD)", width = 1.5, align = "decimal")
#> <fr_col> "Mean (SD)" [1.50in, decimal]

## ── Hidden structural column (grouping key for fr_rows) ─────────────────

fr_col(visible = FALSE)
#> <fr_col> "" [auto, left]

## ── Stub column (repeats in every panel during column splitting) ────────

fr_col("Parameter", width = 2.5, stub = TRUE)
#> <fr_col> "Parameter" [2.50in, left stub]

## ── Spanning group: columns grouped under one header ─────────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    zom_50mg       = fr_col("50 mg",  group = "Zomerane"),
    zom_100mg      = fr_col("100 mg", group = "Zomerane"),
    placebo        = fr_col("Placebo"),
    total          = fr_col("Total")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 2.50in left
#> placebo "Placebo" 0.61in left
#> zom_50mg "50 mg" 0.61in left
#> zom_100mg "100 mg" 0.61in left
#> total "Total" 0.61in left
#> group "group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Per-column N-counts in a pipeline ────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    zom_50mg       = fr_col("Zomerane 50 mg", n = 45),
    placebo        = fr_col("Placebo",         n = 45),
    .n_format = "{label}\n(N={n})"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 2.50in left
#> placebo "Placebo" 0.61in left
#> zom_50mg "Zomerane 50 mg" 0.97in left
#> zom_100mg "zom_100mg" 0.70in left
#> total "total" 0.61in left
#> group "group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Space mode: preserve leading spaces as literal characters ──────────

fr_col("Statistic", space_mode = "preserve")
#> <fr_col> "Statistic" [auto, left]

## ── Space mode: explicit indent mode (default, but can be stated) ─────

fr_col("Characteristic", width = 2.5, space_mode = "indent")
#> <fr_col> "Characteristic" [2.50in, left]
```
