# Define a Cell Style Override

Creates a cell-level style object for use in
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).
Targets individual cells by row and column position (or by region
keyword). Only properties you explicitly set override the base style;
`NULL` means "inherit from the base style".

For row-level or column-level styling, prefer
[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md)
and
[`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md)
which have a simpler interface for uniform row/column formatting.

## Usage

``` r
fr_style(
  region = "body",
  rows = NULL,
  cols = NULL,
  bold = NULL,
  italic = NULL,
  underline = NULL,
  color = NULL,
  background = NULL,
  font_size = NULL,
  align = NULL,
  valign = NULL,
  indent = NULL,
  colspan = NULL,
  rowspan = NULL
)
```

## Arguments

- region:

  Region to target. One of:

  - `"body"` (default) — data body rows.

  - `"header"` — column header row(s) and spanner row(s).

  - `"stub"` — the first (row-label) column across all body rows. For
    **multiple stub columns**, use `region = "body"` with
    `cols = c("lab_name", "visit")` instead.

- rows:

  Integer vector of row positions, `"all"` (all rows in the region), or
  `NULL` (all rows). Row indices are 1-based relative to the region.
  Multiple indices are supported: `rows = c(1L, 3L, 5L)`.

- cols:

  Column selection. Accepts any of:

  - A character vector of column names:
    `cols = c("zom_50mg", "placebo")`

  - A tidyselect expression: `cols = starts_with("zom_")`

  - `NULL` (default) — targets all columns.

  Tidyselect expressions are captured unevaluated and resolved when the
  style is applied via
  [`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).
  This means you can create style objects with tidyselect expressions
  before the data is available. Supported helpers:
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

  Foreground (text) colour: hex string (`"#003366"`) or any of the 148
  CSS named colours (`"navy"`, `"steelblue"`, `"tomato"`, etc.). `NULL`
  to inherit.

- background:

  Background (fill) colour: hex string or CSS named colour. `NULL` to
  inherit.

- font_size:

  Font size in points, or `NULL` to inherit.

- align:

  Horizontal alignment override: `"left"`, `"center"`, `"right"`,
  `"decimal"`, or `NULL`. Controls how text flows within the cell
  (left-to-right positioning). In RTF this maps to `\ql`, `\qc`, `\qr`;
  in LaTeX to `l`, `c`, `r` column types. See **Alignment model** below.

- valign:

  Vertical alignment override: `"top"`, `"middle"`, `"bottom"`, or
  `NULL`. Controls where content sits within a cell when the row is
  taller than the content (e.g., due to multi-line labels in adjacent
  cells). In RTF this maps to `\clvertalt`, `\clvertalc`, `\clvertalb`;
  in LaTeX to `p` (top), `m` (middle), `b` (bottom). See **Alignment
  model** below.

- indent:

  Indentation in inches, or `NULL`.

- colspan:

  Integer. Number of columns the cell spans (horizontal merge). `NULL` =
  no span.

- rowspan:

  Integer. Number of rows the cell spans (vertical merge). `NULL` = no
  span.

## Value

An `fr_cell_style` object for use in
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).

## Alignment model

arframe separates alignment into two orthogonal axes, following the same
model as RTF and LaTeX/tabularray:

|  |  |  |  |
|----|----|----|----|
| Parameter | Axis | Controls | Values |
| `align` | Horizontal | Left-to-right text positioning | `"left"`, `"center"`, `"right"`, `"decimal"` |
| `valign` | Vertical | Top-to-bottom content positioning | `"top"`, `"middle"`, `"bottom"` |

**`align`** (horizontal) is a **paragraph-level** property — it controls
how each line of text is positioned within the cell width. Most cells
need this.

**`valign`** (vertical) is a **cell-level** property — it controls where
the content block sits when the cell is taller than needed. This matters
only when rows have unequal content height (e.g., a single-line label
next to a multi-line label). For single-line body rows, `valign` has no
visible effect.

They combine freely: a cell can be `align = "right", valign = "bottom"`
(text right-aligned, sitting at the bottom of a tall row).

## See also

[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md)
for row-level styling,
[`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md)
for column-level styling,
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
to apply styles to a spec.

## Examples

``` r
## ── Style objects are standalone — create, inspect, reuse ──────────────

# Create style objects independently
header_bold <- fr_style(region = "header", bold = TRUE)
total_background    <- fr_style(cols = "total", background = "aliceblue")

# Inspect the object
str(header_bold)
#> List of 17
#>  $ type      : chr "cell"
#>  $ region    : chr "header"
#>  $ rows      : NULL
#>  $ cols      : NULL
#>  $ bold      : logi TRUE
#>  $ italic    : NULL
#>  $ underline : NULL
#>  $ color     : NULL
#>  $ background: NULL
#>  $ font      : NULL
#>  $ font_size : NULL
#>  $ align     : NULL
#>  $ valign    : NULL
#>  $ indent    : NULL
#>  $ colspan   : NULL
#>  $ rowspan   : NULL
#>  $ height    : NULL
#>  - attr(*, "class")= chr "fr_cell_style"

# Apply to any spec via fr_styles()
tbl_demog |>
  fr_table() |>
  fr_styles(header_bold, total_background)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 2 overrides

# Reuse the same styles across different tables
tbl_disp |>
  fr_table() |>
  fr_styles(header_bold, total_background)
#> 
#> ── fr_spec: Table 
#> Data: 8 rows x 5 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 2 overrides

## ── Bold the entire column header ────────────────────────────────────────

fr_style(region = "header", bold = TRUE)
#> $type
#> [1] "cell"
#> 
#> $region
#> [1] "header"
#> 
#> $rows
#> NULL
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

## ── Red text for specific body rows ──────────────────────────────────────

fr_style(region = "body", rows = c(1L, 3L), color = "#CC0000")
#> $type
#> [1] "cell"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 1 3
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
#> [1] "#CC0000"
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

## ── CSS named colour (148 colours available) ────────────────────────────
fr_style(cols = "total", background = "aliceblue")
#> $type
#> [1] "cell"
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
fr_style(region = "body", rows = 3L, color = "crimson", bold = TRUE)
#> $type
#> [1] "cell"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> [1] 3
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
#> [1] "#DC143C"
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

## ── Highlight the Total column header ────────────────────────────────────

fr_style(region = "header", cols = "total", background = "#D0E4FF", bold = TRUE)
#> $type
#> [1] "cell"
#> 
#> $region
#> [1] "header"
#> 
#> $rows
#> NULL
#> 
#> $cols
#> [1] "total"
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
#> [1] "#D0E4FF"
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

## ── Stub column: bold row labels ─────────────────────────────────────────

fr_style(region = "stub", bold = TRUE)
#> $type
#> [1] "cell"
#> 
#> $region
#> [1] "stub"
#> 
#> $rows
#> NULL
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

## ── Tidyselect: style columns by pattern ───────────────────────────────────

# Use tidyselect helpers instead of hard-coding column names:
fr_style(cols = starts_with("zom_"), background = "#F5F5F5")
#> $type
#> [1] "cell"
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
#> env:  0x62d5580f6a18
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
fr_style(cols = contains("mg"), italic = TRUE)
#> $type
#> [1] "cell"
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
#> env:  0x62d5580f6a18
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

## ── Multi-stub layout (e.g. multiple row-label columns) ──────────────────

# The "stub" region targets exactly one column (the first dataset column).
# When your table has multiple row-label columns (like System Organ Class
# followed by Preferred Term), target them by name using `region = "body"`:
fr_style(region = "body", cols = c("soc", "pt"), align = "left", bold = TRUE)
#> $type
#> [1] "cell"
#> 
#> $region
#> [1] "body"
#> 
#> $rows
#> NULL
#> 
#> $cols
#> [1] "soc" "pt" 
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
#> [1] "left"
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

## ── Merged spanning title cell ────────────────────────────────────────────

fr_style(region = "header", rows = 1L, cols = "characteristic",
         colspan = 5L, bold = TRUE, background = "#F0F0F0")
#> $type
#> [1] "cell"
#> 
#> $region
#> [1] "header"
#> 
#> $rows
#> [1] 1
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
#> [1] "#F0F0F0"
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
#> [1] 5
#> 
#> $rowspan
#> NULL
#> 
#> $height
#> NULL
#> 
#> attr(,"class")
#> [1] "fr_cell_style"
```
