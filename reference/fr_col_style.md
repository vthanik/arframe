# Define a Column Style Override

Creates a column-level style object for use in
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).
Column styles apply uniformly to all body cells in the targeted columns.

## Usage

``` r
fr_col_style(
  cols = NULL,
  bold = NULL,
  italic = NULL,
  underline = NULL,
  color = NULL,
  background = NULL,
  font_size = NULL,
  align = NULL,
  valign = NULL
)
```

## Arguments

- cols:

  Column selection. Accepts any of:

  - A character vector of column names:
    `cols = c("zom_50mg", "placebo")`

  - A tidyselect expression: `cols = starts_with("zom_")`

  - `NULL` (default) — targets all columns.

  Tidyselect expressions are captured unevaluated and resolved when the
  style is applied via
  [`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).
  Supported helpers:
  [`tidyselect::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`tidyselect::ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`tidyselect::contains()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`tidyselect::matches()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html),
  [`tidyselect::where()`](https://tidyselect.r-lib.org/reference/where.html),
  and more.

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

## Value

An `fr_cell_style` object with `type = "col"` for use in
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).

## Style precedence

Column styles are the **broadest** scope and are overridden by both row
styles and cell styles:

    fr_col_style  <  fr_row_style  <  fr_style (cell)

Column **alignment** set via `fr_col(align = ...)` in
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md) is
the base default. `fr_col_style(align = ...)` overrides it for body
cells. `fr_row_style(align = ...)` overrides both. Finally,
`fr_style(region = "body", align = ...)` targeting specific cells wins.

## Tips

- Column alignment is usually set in
  [`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
  via `fr_col(align = ...)`. Use `fr_col_style()` only when you need to
  override alignment for a subset of columns without reconfiguring the
  full column spec.

- Use `fr_col_style(cols = "total", background = "#EBF5FB")` to give the
  Total column a distinct background — a common regulatory convention.

- Column styles apply only to **body** cells. To style header cells, use
  `fr_style(region = "header", cols = ...)` or
  [`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md).

## See also

[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md)
for row-level styling,
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md)
for cell-level styling,
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
to apply to a spec,
[`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md) for
column-level alignment via `align`.

## Examples

``` r
## ── Standalone: store and reuse ─────────────────────────────────────────
total_highlight <- fr_col_style(cols = "total", background = "aliceblue")
total_highlight  # inspect
#> $type
#> [1] "col"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> NULL
#> 
#> $cols
#> [1] "total"
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
#> [1] "#F0F8FF"
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

## ── Total column with blue tint ──────────────────────────────────────────

fr_col_style(cols = "total", background = "#EBF5FB")
#> $type
#> [1] "col"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> NULL
#> 
#> $cols
#> [1] "total"
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
#> [1] "#EBF5FB"
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

## ── Bold the row-label stub column ───────────────────────────────────────

fr_col_style(cols = "characteristic", bold = TRUE)
#> $type
#> [1] "col"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> NULL
#> 
#> $cols
#> [1] "characteristic"
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

## ── Right-align all numeric data columns ─────────────────────────────────

fr_col_style(cols = c("zom_50mg", "zom_100mg", "placebo", "total"),
             align = "right")
#> $type
#> [1] "col"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> NULL
#> 
#> $cols
#> [1] "zom_50mg"  "zom_100mg" "placebo"   "total"    
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
#> [1] "right"
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

## ── Multiple columns in one call ──────────────────────────────────────────

fr_col_style(cols = c("placebo", "zom_50mg", "zom_100mg"), italic = TRUE)
#> $type
#> [1] "col"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> NULL
#> 
#> $cols
#> [1] "placebo"   "zom_50mg"  "zom_100mg"
#> 
#> $bold
#> NULL
#> 
#> $italic
#> [1] TRUE
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

## ── Tidyselect: columns starting with "zom_" ──────────────────────────────

fr_col_style(cols = starts_with("zom_"), align = "center")
#> $type
#> [1] "col"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> NULL
#> 
#> $cols
#> <quosure>
#> expr: ^starts_with("zom_")
#> env:  0x62d5540fe028
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
#> [1] "center"
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

## ── Tidyselect: all columns containing "mg" ──────────────────────────────

fr_col_style(cols = contains("mg"), background = "#F5F5F5")
#> $type
#> [1] "col"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> NULL
#> 
#> $cols
#> <quosure>
#> expr: ^contains("mg")
#> env:  0x62d5540fe028
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

## ── Foreground + background combined ─────────────────────────────────────

fr_col_style(cols = "total", color = "#003366", background = "#E8F4FD")
#> $type
#> [1] "col"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> NULL
#> 
#> $cols
#> [1] "total"
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
#> [1] "#003366"
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

## ── Full pipeline with column styles ─────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_hlines("header") |>
  fr_vlines("box") |>
  fr_styles(
    fr_style(region = "header", bold = TRUE),
    fr_col_style(cols = "total", background = "#EBF5FB")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)
#> Styles: 2 overrides
```
