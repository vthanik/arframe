# Get a Single Column Specification

Extracts the `fr_col` object for a named column. Errors with an
informative message if the column is not found, listing available column
names.

## Usage

``` r
fr_get_col(spec, col)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
  or
  [`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md).

- col:

  Character scalar. Column name to retrieve.

## Value

An `fr_col` object with fields: `label`, `width`, `align`,
`header_align`, `visible`.

## See also

[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md),
[`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md),
[`fr_get_columns()`](https://vthanik.github.io/arframe/reference/fr_get_columns.md)
for all columns.

## Examples

``` r
spec <- tbl_demog |> fr_table() |>
  fr_cols(characteristic = fr_col("Characteristic", width = 2.5))
col <- fr_get_col(spec, "characteristic")
col$label   # "Characteristic"
#> [1] "Characteristic"
col$width   # 2.5
#> [1] 2.5
```
