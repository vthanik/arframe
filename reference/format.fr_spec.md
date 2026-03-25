# Format an fr_spec as a compact one-liner

Returns a concise string summary useful for logging, debugging, or
[`sprintf()`](https://rdrr.io/r/base/sprintf.html) interpolation.

## Usage

``` r
# S3 method for class 'fr_spec'
format(x, ...)
```

## Arguments

- x:

  An `fr_spec` object.

- ...:

  Ignored.

## Value

Character scalar describing the spec.

## Examples

``` r
spec <- tbl_demog |> fr_table()
format(spec)
#> [1] "<fr_spec> table: 28 rows x 6 cols, 0 style(s), 0 title(s)"
sprintf("Processing: %s", format(spec))
#> [1] "Processing: <fr_spec> table: 28 rows x 6 cols, 0 style(s), 0 title(s)"
```
