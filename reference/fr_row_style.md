# Define a Row Style Override

Creates a row-level style object for use in
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).
Row styles apply uniformly across all cells in the targeted rows.

## Usage

``` r
fr_row_style(
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
)
```

## Arguments

- rows:

  Integer vector of body row positions, `"all"`, `NULL` (all body rows),
  or a special selector. Row indices are 1-based. Multiple indices are
  supported: `rows = c(1L, 3L, 5L)`.

  **Group header selectors** (resolved at render time):

  - `"group_headers"` — all group header rows (from `group_by` with
    `label` or `leaf`).

  - `"group_headers:<level>"` — only headers at a specific hierarchy
    level (e.g., `"group_headers:soc"`). For `leaf` hierarchies only.

  **Page_by selector:**

  - `"page_by"` — page_by section labels (rendered above column headers
    for each page group). Page_by labels are plain text by default
    across all backends (RTF, HTML, PDF). Supports: `bold`, `italic`,
    `underline`, `color`, `background`, `font_size`, `align`. Does not
    support `height` or `valign` (page_by labels are section headers,
    not table rows).

- bold, italic, underline:

  Logical or `NULL` to inherit.

- color:

  Foreground (text) colour, or `NULL`.

- background:

  Background (fill) colour, or `NULL`.

- font_size:

  Font size in points, or `NULL`.

- align:

  Horizontal alignment: `"left"`, `"center"`, `"right"`, `"decimal"`, or
  `NULL`. See
  [`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md)
  **Alignment model**.

- valign:

  Vertical alignment: `"top"`, `"middle"`, `"bottom"`, or `NULL`. See
  [`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md)
  **Alignment model**.

- height:

  Row height in inches, or `NULL` (auto).

## Value

An `fr_cell_style` object with `type = "row"` for use in
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).

## Style precedence

When multiple styles target the same cell, narrower scopes win:

    fr_col_style  <  fr_row_style  <  fr_style (cell)

Within the same scope level, later styles (later in the
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
call or in later
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
calls) override earlier ones. `fr_row_style()` overrides
[`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md)
for the same property because row styles are narrower than column
styles.

## Tips

- Use `fr_row_style(rows = "all", background = "#F5F5F5")` for a subtle
  background on all body rows (zebra striping requires alternating
  calls: rows of odd/even index).

- Row height in regulatory tables is normally controlled by
  `fr_page(font_size = ...)` and the layout engine. Set `height` only
  when you need a specific row to be taller (e.g. a summary row).

- Use
  [`fr_rows_matches()`](https://vthanik.github.io/arframe/reference/fr_rows_matches.md)
  instead of hard-coded row indices for content-based row targeting
  (e.g. bold every "Total" row).

## See also

[`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md)
for column-level styling,
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md)
for cell-level styling,
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
to apply to a spec.

## Examples

``` r
## ── Standalone: store and reuse ─────────────────────────────────────────
bold_first <- fr_row_style(rows = 1L, bold = TRUE)
bold_first  # inspect
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 1
#> 
#> $cols
#> NULL
#> 
#> $bold
#> [1] TRUE
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $color
#> NULL
#> 
#> $background
#> NULL
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> NULL
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Bold the first body row (e.g. total row) ─────────────────────────────

fr_row_style(rows = 1L, bold = TRUE)
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 1
#> 
#> $cols
#> NULL
#> 
#> $bold
#> [1] TRUE
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $color
#> NULL
#> 
#> $background
#> NULL
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> NULL
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Light grey background on all body rows ────────────────────────────────

fr_row_style(rows = "all", background = "#F5F5F5")
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] "all"
#> 
#> $cols
#> NULL
#> 
#> $bold
#> NULL
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $color
#> NULL
#> 
#> $background
#> [1] "#F5F5F5"
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> NULL
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Highlight multiple specific rows ──────────────────────────────────────

# Pass a vector of row indices to style multiple disparate rows at once:
fr_row_style(rows = c(2L, 4L, 6L), background = "#E8F4FD")
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 2 4 6
#> 
#> $cols
#> NULL
#> 
#> $bold
#> NULL
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $color
#> NULL
#> 
#> $background
#> [1] "#E8F4FD"
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> NULL
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Highlight last row (totals) in a different colour ─────────────────────

fr_row_style(rows = nrow(tbl_demog), background = "#FFF3CD", bold = TRUE)
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 28
#> 
#> $cols
#> NULL
#> 
#> $bold
#> [1] TRUE
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $color
#> NULL
#> 
#> $background
#> [1] "#FFF3CD"
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> NULL
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Increase height of header-adjacent row ────────────────────────────────

fr_row_style(rows = 1L, height = 0.3)
#> $type
#> [1] "row"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 1
#> 
#> $cols
#> NULL
#> 
#> $bold
#> NULL
#> 
#> $italic
#> NULL
#> 
#> $underline
#> NULL
#> 
#> $color
#> NULL
#> 
#> $background
#> NULL
#> 
#> $font
#> NULL
#> 
#> $font_size
#> NULL
#> 
#> $align
#> NULL
#> 
#> $valign
#> NULL
#> 
#> $indent
#> NULL
#> 
#> $colspan
#> NULL
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> [1] 0.3
#> 
#> attr(,"class")
#> [1] "fr_cell_style"

## ── Full pipeline with row styles ─────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_hlines("header") |>
  fr_styles(
    fr_row_style(rows = "all", background = "#FAFAFA"),
    fr_style(region = "header", bold = TRUE, background = "#E0E0E0")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)
#> Styles: 2 overrides

## ── Bold all group header rows ────────────────────────────────────────

data.frame(
  variable = c("Sex", "Sex", "Age", "Age"),
  stat = c("Female", "Male", "Mean (SD)", "Median"),
  value = c("27 (60.0)", "18 (40.0)", "75.0 (6.8)", "74.0"),
  stringsAsFactors = FALSE
) |>
  fr_table() |>
  fr_cols(variable = fr_col(visible = FALSE)) |>
  fr_rows(group_by = list(cols = "variable", label = "stat")) |>
  fr_styles(
    fr_row_style(rows = "group_headers", bold = TRUE, background = "#E8E8E8")
  )
#> 
#> ── fr_spec: Table 
#> Data: 4 rows x 3 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (2 visible of 3):
#> stat "stat" 0.62in left
#> value "value" 0.55in left
#> Header: valign=bottom
#> Rows: group_by=variable (label=stat), indent_by=stat
#> Styles: 1 override

## ── Bold page_by labels ───────────────────────────────────────────────

tbl_vs[tbl_vs$timepoint == "Week 24", ] |>
  fr_table() |>
  fr_cols(param = fr_col(visible = FALSE),
          timepoint = fr_col(visible = FALSE)) |>
  fr_rows(page_by = "param") |>
  fr_styles(
    fr_row_style(rows = "page_by", bold = TRUE)
  )
#> 
#> ── fr_spec: Table 
#> Data: 20 rows x 12 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (10 visible of 12):
#> statistic "statistic" 0.62in left
#> placebo_base "placebo_base" 0.77in left
#> placebo_value "placebo_value" 0.82in left
#> placebo_chg "placebo_chg" 0.72in left
#> zom_50mg_base "zom_50mg_base" 0.94in left
#> zom_50mg_value "zom_50mg_value" 0.99in left
#> zom_50mg_chg "zom_50mg_chg" 0.89in left
#> zom_100mg_base "zom_100mg_base" 1.00in left
#> ... and 2 more
#> Header: valign=bottom
#> Rows: page_by=param
```
