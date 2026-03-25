# Configure Column Display

Configures the display label, width, alignment, visibility, N-counts,
and spanning groups for table columns. `fr_cols()` is the **single
source of truth** for all column structure;
[`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md)
owns header **presentation** only (bold, colours, font size).

Columns not explicitly named here receive auto-generated defaults: the
column name as the label (optionally transformed by `.label_fn`),
alignment inferred from the R column type (numeric → `"right"`,
everything else → `"left"`), and width from `.width`.

Calling `fr_cols()` again **replaces** the entire column configuration.

## Usage

``` r
fr_cols(
  spec,
  ...,
  .list = NULL,
  .width = NULL,
  .align = NULL,
  .label_fn = NULL,
  .space_mode = NULL,
  .split = NULL,
  .n = NULL,
  .n_format = NULL
)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- ...:

  Column specifications. Can be provided in two ways:

  - **Named arguments**: The argument name must match a column name in
    the data frame. The value can be an
    [`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md)
    object or a character scalar (label).

  - **Formulas (tidyselect)**: Use `lhs ~ rhs` where `lhs` is a set of
    columns selected via
    [tidyselect::language](https://tidyselect.r-lib.org/reference/language.html)
    (e.g., `starts_with("col")`), and `rhs` is an
    [`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md)
    object or character scalar applied to all selected columns. Powered
    by the `tidyselect` package.

  Any data column not listed receives auto-generated defaults.

- .list:

  Named list or character vector of pre-built labels. Allows you to
  construct column labels externally (e.g., using
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html)) and pass them in
  programmatically. Example: `list(zom_50mg = "Zomerane 50 mg")`.

- .width:

  Default column width applied to all columns that do not have an
  explicit width set. Accepts:

  - **Numeric** — width in inches (e.g. `1.5`). Default when `NULL`.

  - **`"auto"`** — auto-calculate widths from content and header text
    using the page font metrics. The layout engine measures the widest
    cell value and the header label for each column, adds padding, and
    scales **down** if the total exceeds the printable page width.
    Columns that already fit are left as-is.

  - **`"fit"`** — like `"auto"`, but always scales (up **or** down) so
    the total width exactly fills the printable page width, preserving
    the content-based ratio between columns. Use this when you want
    every table to span the full page width without manual tuning.

  - **`"equal"`** — distribute the printable page width equally among
    all columns that do not have a fixed width. Columns with an explicit
    numeric `width` in
    [`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md)
    keep their size; the remaining space is divided equally.

  - **Percentage string** — e.g. `"25%"`. Sets every column's default
    width as a fraction of the printable page width. Must be between
    `"0%"` (exclusive) and `"100%"` (inclusive). Columns with an
    explicit `width` in
    [`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md)
    are not affected.

  - **`NULL`** (default) — same as `"auto"`. Columns auto-size from
    content and header text.

- .align:

  Default alignment applied to all columns that do not have an explicit
  alignment set. One of `"left"`, `"center"`, `"right"`, `"decimal"`.
  `NULL` auto-detects: `"right"` for numeric columns, `"left"` for
  everything else.

- .label_fn:

  A function (or rlang-style lambda) applied to auto-generated column
  labels. Only affects columns whose labels were **not** explicitly set
  via `...` or `.list`. Receives the column name as input, returns the
  display label.

  Common transforms:

  - `~ gsub("_", " ", .x)` — replace underscores with spaces.

  - `~ tools::toTitleCase(gsub("_", " ", .x))` — title case.

  - `toupper` — all caps.

- .space_mode:

  How to handle leading spaces in cell data for all columns that do not
  have an explicit `space_mode` set in
  [`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md).
  One of:

  - `"indent"` (default) — convert leading spaces to paragraph-level
    indent. The indent width is measured from the page font metrics, so
    it renders correctly in both proportional and monospace fonts.

  - `"preserve"` — keep leading spaces as literal characters.

  - `NULL` — same as `"indent"`.

- .split:

  Logical or `NULL`. Column splitting for wide tables that exceed the
  printable page width:

  - `NULL` (default) — no splitting. All columns on one page.

  - `TRUE` — split across multiple panels, with stub columns repeated in
    each panel. Panel width behaviour follows `.width`:

    - `.width = "auto"` → panels keep natural column widths.

    - `.width = "fit"` → panels scale to fill the page width.

    - `.width = "equal"` → unfixed columns share remaining space
      equally.

  - `FALSE` — explicitly no splitting.

  Stub columns (repeated in every panel) are designated via
  `fr_col(stub = TRUE)`. When `.split` is enabled but no columns have
  `stub = TRUE`, stubs are auto-inferred from `group_by`/`indent_by`
  columns or the first column.

- .n:

  Bulk N-counts applied across columns and spanning groups. Names are
  matched **case-insensitively** using a two-step lookup: first by
  **column display label**, then by **data column name** as fallback.
  This means you can use either form:

  - `c("Placebo" = 45)` — matches by label

  - `c(placebo = 45)` — matches by column name (no label repetition)

  Accepts:

  - **Named numeric vector** — names = display labels or column names,
    matched case-insensitively to column labels first, then column
    names. Also matches spanning group names. Example:
    `c(placebo = 45, zom_50mg = 44)`.

  - **Data frame (2-column)** — column 1 = display labels, column 2 =
    counts. Same N on every page. Example:
    `data.frame(trt = c("Placebo", "Zom 50mg"), n = c(45, 44))`.

  - **Data frame (3-column)** — column 1 = `page_by` group values,
    column 2 = display labels, column 3 = counts. Different N per
    `page_by` group.

  - **Named list** — keys = `page_by` group values, values = named
    numeric vectors (names = display labels). Example:
    `list("Systolic BP" = c("Placebo" = 42, "Zom" = 40))`.

  **Auto-routing**: when a label matches a `group` name (spanning
  header), N goes on the span. When it matches a column label, N goes on
  the column. Group matches take priority (no double-apply).

  Per-column `fr_col(n = ...)` always takes highest priority.

- .n_format:

  A [glue](https://glue.tidyverse.org/reference/glue.html)-style format
  string for N-count labels. Available tokens: `{label}` (column display
  label) and `{n}` (count). Default `NULL` inherits from config
  (default: `"{label}\\n(N={n})"`). Override at project level via
  `columns.n_format` in `_arframe.yml`, session level via
  `fr_theme(n_format = ...)`, or per-table here.

## Value

A modified `fr_spec`. Column specs stored in `spec$columns`, split
settings in `spec$columns_meta`.

## Regulatory conventions — column ordering

Standard pharma house styles and FDA/EMA submissions require:

- **Active treatment arm(s) left**, in order of dose escalation (lowest
  to highest dose, left to right).

- **Comparator / placebo arm(s) rightmost** among the study arm columns.

- **Total column** (if included): optional; covers **active arms only**
  (does not pool active + placebo). Placed immediately after the
  rightmost active arm column, before the placebo column.

Example column order for a 2-dose + placebo study:

## Label resolution order

Column labels are resolved in this priority (highest wins):

1.  N-count formatting (`fr_col(n=)` or `.n` + `.n_format`) — dynamic
    label with N counts, applied at render time.

2.  Explicit `fr_col(label = ...)` in `...` arguments.

3.  `.list` — programmatic label map.

4.  `.label_fn` — transform function applied to the column name.

5.  Column name — the raw data frame column name as-is.

## Tips

- Column order in the rendered table matches the **data frame column
  order**, not the order you list them in `fr_cols()`. To reorder
  columns, reorder the data frame first.

- Use `visible = FALSE` to suppress a column from rendering while
  keeping it available as a grouping key for
  [`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md).

- `align = "decimal"` aligns decimal points in numeric columns.

- Width is in inches. Landscape Letter with 1 in margins gives 9 in
  printable width. A common pharma layout: stub column 2.5 in + 4–5 data
  columns at 1.3–1.5 in each.

- `.width = "auto"` is the fastest way to get a working table — the
  layout engine calculates widths from content. Use fixed widths only
  when you need exact control.

- `.label_fn` transforms **only** auto-generated labels (column names
  that were not explicitly relabelled). This lets you set a baseline
  transform and override specific columns as needed.

## Parameter Precedence

Settings resolve from four tiers (lowest to highest priority): package
defaults \< `_arframe.yml` \<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
\< this function. Only parameters you explicitly supply override
previous tiers.

## See also

[`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md) for
the column spec constructor,
[`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md)
for header presentation (bold, colours, alignment),
[`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
for advanced multi-level spanning headers,
[`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
for pagination.

## Examples

``` r
## ── Per-column N-counts (80% case) ───────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    zom_50mg       = fr_col("Zomerane 50 mg",  n = 45),
    zom_100mg      = fr_col("Zomerane 100 mg", n = 45),
    placebo        = fr_col("Placebo",          n = 45),
    total          = fr_col("Total",            n = 135),
    .n_format = "{label}\n(N={n})"
  ) |>
  fr_header(bold = TRUE, align = "center")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 2.50in left
#> placebo "Placebo" 0.61in left
#> zom_50mg "Zomerane 50 mg" 0.97in left
#> zom_100mg "Zomerane 100 mg" 1.03in left
#> total "Total" 0.61in left
#> group "group" 0.62in left
#> Header: bold, valign=bottom, align=center
#> Rules: 1 hline(s)

## ── Bulk N by column name (no label repetition) ────────────────────────

# .n keys match column names — labels only defined once in fr_col()
tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    placebo        = fr_col("Placebo"),
    zom_50mg       = fr_col("Zomerane 50 mg"),
    zom_100mg      = fr_col("Zomerane 100 mg"),
    total          = fr_col("Total"),
    .n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135),
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
#> zom_100mg "Zomerane 100 mg" 1.03in left
#> total "Total" 0.61in left
#> group "group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Bulk N from a data frame ─────────────────────────────────────────────

adsl_n <- data.frame(
  trt = c("Placebo", "Zomerane 50 mg", "Zomerane 100 mg", "Total"),
  n   = c(45L, 45L, 45L, 135L)
)
tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    placebo        = fr_col("Placebo"),
    zom_50mg       = fr_col("Zomerane 50 mg"),
    zom_100mg      = fr_col("Zomerane 100 mg"),
    total          = fr_col("Total"),
    .n = adsl_n,
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
#> zom_100mg "Zomerane 100 mg" 1.03in left
#> total "Total" 0.61in left
#> group "group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Spanning groups via group= ───────────────────────────────────────────

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

## ── Bulk N auto-routes to spans and columns ──────────────────────────────

n_df <- data.frame(
  trt = c("Zomerane", "Placebo", "Total"),
  n   = c(90L, 45L, 135L)
)
tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    zom_50mg       = fr_col("50 mg",  group = "Zomerane"),
    zom_100mg      = fr_col("100 mg", group = "Zomerane"),
    placebo        = fr_col("Placebo"),
    total          = fr_col("Total"),
    .n = n_df,
    .n_format = "{label}\n(N={n})"
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

## ── Auto-width: let the engine calculate ─────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic"),
    .width = "auto"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 1.85in left
#> placebo "placebo" 0.61in left
#> zom_50mg "zom_50mg" 0.64in left
#> zom_100mg "zom_100mg" 0.70in left
#> total "total" 0.61in left
#> group "group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Percentage widths: responsive to page size ───────────────────────────

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

## ── Fit mode: fill the full page width proportionally ────────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic"),
    .width = "fit"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 3.48in left
#> placebo "placebo" 1.16in left
#> zom_50mg "zom_50mg" 1.20in left
#> zom_100mg "zom_100mg" 1.32in left
#> total "total" 1.16in left
#> group "group" 1.18in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Equal-width distribution ─────────────────────────────────────────────

# Stub column fixed at 2.5in; remaining columns share the rest equally
tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    .width = "equal"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 2.50in left
#> placebo "placebo" 1.40in left
#> zom_50mg "zom_50mg" 1.40in left
#> zom_100mg "zom_100mg" 1.40in left
#> total "total" 1.40in left
#> group "group" 1.40in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Label transform: underscores to spaces + title case ──────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(.label_fn = ~ tools::toTitleCase(gsub("_", " ", .x)))
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 1.85in left
#> placebo "Placebo" 0.61in left
#> zom_50mg "Zom 50mg" 0.64in left
#> zom_100mg "Zom 100mg" 0.70in left
#> total "Total" 0.61in left
#> group "Group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Tidyselect formula (apply config to multiple columns) ────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    c(zom_50mg, zom_100mg) ~ fr_col(width = 1.5, align = "right"),
    starts_with("t") ~ fr_col(width = 1.5, align = "right")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 2.50in left
#> placebo "placebo" 0.61in left
#> zom_50mg "" 1.50in right
#> zom_100mg "" 1.50in right
#> total "" 1.50in right
#> group "group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Pre-formatted labels via .list ───────────────────────────────────────

labels_vec <- c(placebo = "Placebo", zom_50mg = "Zom 50mg")
tbl_demog |>
  fr_table() |>
  fr_cols(.list = labels_vec)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "characteristic" 1.85in left
#> placebo "Placebo" 0.61in left
#> zom_50mg "Zom 50mg" 0.64in left
#> zom_100mg "zom_100mg" 0.70in left
#> total "total" 0.61in left
#> group "group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Leading spaces → paragraph-level indent (default) ──────────────────

# tbl_demog has leading spaces ("  Mean (SD)", "  <65", etc.)
# Default .space_mode = "indent" converts them to real paragraph indent
tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    .space_mode = "indent"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 2.50in left
#> placebo "placebo" 0.61in left
#> zom_50mg "zom_50mg" 0.64in left
#> zom_100mg "zom_100mg" 0.70in left
#> total "total" 0.61in left
#> group "group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Preserve leading spaces as literal characters ─────────────────────

# Use "preserve" for pre-formatted content with exact spacing
tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    .space_mode = "preserve"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 2.50in left
#> placebo "placebo" 0.61in left
#> zom_50mg "zom_50mg" 0.64in left
#> zom_100mg "zom_100mg" 0.70in left
#> total "total" 0.61in left
#> group "group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Per-column override: preserve one column, indent the rest ─────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5, space_mode = "preserve"),
    .space_mode = "indent"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 2.50in left
#> placebo "placebo" 0.61in left
#> zom_50mg "zom_50mg" 0.64in left
#> zom_100mg "zom_100mg" 0.70in left
#> total "total" 0.61in left
#> group "group" 0.62in left
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Column splitting: split + fit to fill page ─────────────────────────

tbl_vs |>
  fr_table() |>
  fr_cols(
    param     = fr_col("Parameter", stub = TRUE),
    statistic = fr_col("Statistic", stub = TRUE),
    .split = TRUE,
    .width = "fit"
  )
#> 
#> ── fr_spec: Table 
#> Data: 60 rows x 12 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (12 visible of 12):
#> param "Parameter" 1.05in left
#> timepoint "timepoint" 0.52in left
#> statistic "Statistic" 0.56in left
#> placebo_base "placebo_base" 0.70in left
#> placebo_value "placebo_value" 0.74in left
#> placebo_chg "placebo_chg" 0.65in left
#> zom_50mg_base "zom_50mg_base" 0.85in left
#> zom_50mg_value "zom_50mg_value" 0.89in left
#> ... and 4 more
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Column splitting with natural widths (no stretching) ────────────────

tbl_vs |>
  fr_table() |>
  fr_cols(.split = TRUE)
#> 
#> ── fr_spec: Table 
#> Data: 60 rows x 12 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (12 visible of 12):
#> param "param" 1.16in left
#> timepoint "timepoint" 0.58in left
#> statistic "statistic" 0.62in left
#> placebo_base "placebo_base" 0.77in left
#> placebo_value "placebo_value" 0.82in left
#> placebo_chg "placebo_chg" 0.72in left
#> zom_50mg_base "zom_50mg_base" 0.94in left
#> zom_50mg_value "zom_50mg_value" 0.99in left
#> ... and 4 more
#> Header: valign=bottom
#> Rules: 1 hline(s)
```
