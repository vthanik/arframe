# Get Column Specifications from a Spec

Extracts the named list of `fr_col` objects configured via
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md).
Each element describes a column's label, width, alignment, and other
properties. If no columns have been explicitly configured, returns an
empty list (columns are auto-generated at render time by
`finalize_spec()`).

## Usage

``` r
fr_get_columns(spec)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
  or
  [`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md).

## Value

A named list of `fr_col` objects, keyed by column name. Empty list if no
columns have been configured.

## See also

[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md) to
configure columns,
[`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md) for
the column spec constructor,
[`fr_get_col()`](https://vthanik.github.io/arframe/reference/fr_get_col.md)
for a single column.

## Examples

``` r
spec <- tbl_demog |> fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    placebo = fr_col("Placebo", align = "right")
  )
cols <- fr_get_columns(spec)
names(cols)            # "characteristic", "placebo"
#> [1] "characteristic" "placebo"        "zom_50mg"       "zom_100mg"     
#> [5] "total"          "group"         
cols$placebo$width     # 1.5 (default)
#> [1] 0.6145
cols$placebo$align     # "right"
#> [1] "right"
```
