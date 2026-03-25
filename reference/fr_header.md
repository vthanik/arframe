# Configure Column Header Presentation

The single owner of all header **presentation** settings: alignment,
vertical alignment, bold, background/foreground colour, and font size.
These defaults apply to **all** column header cells unless overridden by
per-column `fr_col(header_align = ...)` or
`fr_style(region = "header", ...)`.

[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
owns column **structure** (labels, widths, body alignment, visibility,
N-counts, spanning groups); `fr_header()` owns header **presentation**
only. N-count labels and format are set exclusively via
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
`.n` / `.n_format` and
[`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md) `n`
parameters.

Calling `fr_header()` again **replaces** the previous header config
(except spans, which are managed by
[`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)).

## Usage

``` r
fr_header(
  spec,
  align = NULL,
  valign = NULL,
  bold = NULL,
  background = NULL,
  color = NULL,
  font_size = NULL,
  repeat_on_page = NULL
)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- align:

  Scalar horizontal alignment for all header cells. One of `"left"`,
  `"center"`, `"right"`, or `NULL` (inherit from column body alignment).
  For per-column header alignment, use `fr_col(header_align = ...)`
  instead.

- valign:

  Default vertical alignment. One of `"top"`, `"middle"`, `"bottom"`
  (default). Relevant when header rows have unequal height.

- bold:

  Logical or `NULL`. Whether header cells are bold. Default `NULL`
  inherits the built-in default (FALSE). Set `TRUE` explicitly if you
  want bold headers.

- background:

  Background colour for header cells: hex string (`"#003366"`) or CSS
  named colour (`"steelblue"`, `"lightgray"`, etc.). `NULL` inherits.

- color:

  Foreground (text) colour for header cells: hex string or CSS named
  colour. `NULL` inherits.

- font_size:

  Font size in points for header cells. `NULL` inherits from page font
  size.

- repeat_on_page:

  Logical. Whether to repeat the column header on every page. Default
  `TRUE` (standard for regulatory tables).

## Value

A modified `fr_spec`. Header config stored in `spec$header`.

## Priority chain for header alignment

    config header.align < fr_header(align) < fr_col(header_align) < fr_style(region="header", align)

## Regulatory conventions

Many pharma TFL outputs use **bold, centered** column headers with
`valign = "bottom"` so that short labels (e.g. "Characteristic") align
at the bottom when adjacent columns have multi-line labels (e.g.
`"Placebo\\n(N=45)"`). By default, headers are **not bold** and inherit
alignment from the column's `align` setting — use
`fr_header(bold = TRUE, align = "center")` to opt in.

## Parameter Precedence

Settings resolve from four tiers (lowest to highest priority): package
defaults \< `_arframe.yml` \<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
\< this function. Only parameters you explicitly supply override
previous tiers.

## Precedence

Header alignment priority (last wins):

`_arframe.yml` \< `fr_theme(header=)` \< `fr_header(align=)` \<
`fr_col(header_align=)` \< `fr_style(region="header", align=)`

Header font size priority:

`fr_page(font_size=)` \< `fr_header(font_size=)` \<
`fr_style(region="header", font_size=)`

## See also

[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
for column structure and N-counts,
[`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
for spanning headers,
[`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md) for
per-column `header_align` overrides,
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md)
with `region = "header"` for cell-level overrides,
[`fr_config()`](https://vthanik.github.io/arframe/reference/fr_config.md)
for setting header defaults via `_arframe.yml`.

## Examples

``` r
## ── Center all headers with bottom valign ────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_header(align = "center", valign = "bottom", bold = TRUE)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: bold, valign=bottom, align=center

## ── Per-column header alignment via fr_col ────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", header_align = "left"),
    zom_50mg       = fr_col("Zomerane 50 mg", header_align = "center"),
    placebo        = fr_col("Placebo",         header_align = "center"),
    total          = fr_col("Total",           header_align = "right")
  ) |>
  fr_header(bold = TRUE)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 1.85in left
#> placebo "Placebo" 0.61in left
#> zom_50mg "Zomerane 50 mg" 0.97in left
#> zom_100mg "zom_100mg" 0.70in left
#> total "Total" 0.61in left
#> group "group" 0.62in left
#> Header: bold, valign=bottom

## ── Header background colour (hex or CSS named colour) ──────────────────

# Hex colour
tbl_demog |>
  fr_table() |>
  fr_header(background = "#E0E0E0", bold = TRUE)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: bold, valign=bottom

# CSS named colour (148 available: lavender, aliceblue, gainsboro, etc.)
tbl_demog |>
  fr_table() |>
  fr_header(background = "lavender", color = "midnightblue", bold = TRUE)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: bold, valign=bottom

## ── N-counts now on fr_cols(), not fr_header() ────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    zom_50mg       = fr_col("Zomerane 50 mg", n = 45),
    placebo        = fr_col("Placebo",         n = 45),
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
#> zom_100mg "zom_100mg" 0.70in left
#> total "total" 0.61in left
#> group "group" 0.62in left
#> Header: bold, valign=bottom, align=center
```
