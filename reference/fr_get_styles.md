# Get Cell Styles from a Spec

Extracts the list of cell style overrides applied via
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).
Each element is an `fr_cell_style` object describing the target region,
rows, columns, and style properties (bold, color, background, etc.).

## Usage

``` r
fr_get_styles(spec)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
  or
  [`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md).

## Value

A list of `fr_cell_style` objects. Empty list if no styles are applied.

## See also

[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
to apply styles,
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md),
[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md),
[`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md)
for style constructors.

## Examples

``` r
spec <- tbl_demog |> fr_table() |>
  fr_styles(
    fr_row_style(rows = 1L, bold = TRUE),
    fr_col_style(cols = "total", background = "#EBF5FB")
  )
styles <- fr_get_styles(spec)
length(styles)           # 2
#> [1] 2
```
