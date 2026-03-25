# Get the Data Frame from a Spec

Extracts the data frame stored in the `fr_spec` object. This is the same
data frame passed to
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
or
[`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md),
before any rendering transformations (sorting, repeat suppression,
blank-after rows).

Modifying the returned data frame does not affect the spec (R
copy-on-modify semantics protect the original).

## Usage

``` r
fr_get_data(spec)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
  or
  [`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md).

## Value

A data frame. For figure specs created by
[`fr_figure()`](https://vthanik.github.io/arframe/reference/fr_figure.md),
returns an empty data frame with zero rows and zero columns.

## See also

[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md),
[`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md),
[`fr_get_columns()`](https://vthanik.github.io/arframe/reference/fr_get_columns.md)
for column configuration.

## Examples

``` r
spec <- tbl_demog |> fr_table()
d <- fr_get_data(spec)
nrow(d)
#> [1] 28
names(d)
#> [1] "characteristic" "placebo"        "zom_50mg"       "zom_100mg"     
#> [5] "total"          "group"         
```
