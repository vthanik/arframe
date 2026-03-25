# Apply Style Overrides to a Table

Appends one or more style override objects — created by
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md),
[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md),
or
[`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md)
— to the table specification. Multiple calls to `fr_styles()`
**accumulate**: styles are applied in order, with later styles
overriding earlier ones where they target the same cells.

## Usage

``` r
fr_styles(spec, ...)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- ...:

  One or more style objects created by
  [`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md),
  [`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md),
  or
  [`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md).

## Value

A modified `fr_spec`. Styles appended to `spec$cell_styles`.

## Style application order

Styles are applied in the order they are passed. The precedence (from
lowest to highest) is:

1.  **Column styles** (`fr_col_style`) — widest scope, applied first.

2.  **Row styles** (`fr_row_style`) — override column styles.

3.  **Cell styles** (`fr_style`) — narrowest scope, applied last.

Within the same type, later styles override earlier ones for the same
cell properties. This means you can set a broad default and then
selectively override specific cells.

## Tips

- It is idiomatic to pass all styles in a single `fr_styles()` call,
  ordering them from broad to specific.

- `fr_styles()` **appends** — call
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
  again to start fresh and discard accumulated styles.

- The number of styles has negligible performance impact — the render
  engine applies them in a single pass over the cell grid.

## Precedence

When multiple styles target the same cell, later styles in the
`fr_styles()` call override earlier ones. Scope precedence:

[`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md)
\<
[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md)
\<
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md)
(cell-level)

## See also

[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md),
[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md),
[`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md)
for the style constructors.

## Examples

``` r
## ── Create reusable style objects, then apply ─────────────────────────────
header_style <- fr_style(region = "header", bold = TRUE, background = "lavender")
total_col    <- fr_col_style(cols = "total", background = "aliceblue")
bold_totals  <- fr_row_style(
  rows = fr_rows_matches("characteristic", "Total"),
  bold = TRUE
)

tbl_demog |>
  fr_table() |>
  fr_styles(header_style, total_col, bold_totals)
#> Warning: `fr_rows_matches()`: no rows matched in column "characteristic".
#> ℹ Selector: value Total.
#> ℹ Styles targeting these rows will have no effect.
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 3 overrides

## ── Bold header + highlighted Total column ────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_hlines("header") |>
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

## ── Zebra striping (alternating row shading) ─────────────────────────────

n <- nrow(tbl_demog)
odd_rows  <- seq(1, n, by = 2)
even_rows <- seq(2, n, by = 2)

tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_row_style(rows = odd_rows,  background = "#FFFFFF"),
    fr_row_style(rows = even_rows, background = "#F5F5F5")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 2 overrides

## ── Multi-layer: col → row → cell ────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_styles(
    # Broad: light background for all data columns
    fr_col_style(cols = c("zom_50mg", "zom_100mg", "placebo"), background = "#FAFAFA"),
    # Mid: bold entire header
    fr_style(region = "header", bold = TRUE),
    # Narrow: red text for high-risk row
    fr_style(region = "body", rows = 3L, color = "#CC0000", bold = TRUE)
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 3 overrides

## ── Stub column bold + header background ─────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_hlines("header") |>
  fr_vlines("box") |>
  fr_styles(
    fr_style(region = "header", bold = TRUE, background = "#E8E8E8"),
    fr_style(region = "stub",   bold = TRUE)
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)
#> Styles: 2 overrides

## ── Accumulating styles across multiple calls ─────────────────────────────

spec <- tbl_demog |>
  fr_table() |>
  fr_styles(fr_style(region = "header", bold = TRUE))

# Later: add Total column highlight (accumulates, does not replace)
spec <- spec |>
  fr_styles(fr_col_style(cols = "total", background = "#EBF5FB"))
```
