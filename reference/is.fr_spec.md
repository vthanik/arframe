# Test if an Object is an fr_spec

Test if an Object is an fr_spec

## Usage

``` r
is.fr_spec(x)
```

## Arguments

- x:

  An object to test.

## Value

`TRUE` if `x` inherits from `"fr_spec"`, `FALSE` otherwise.

## Examples

``` r
spec <- tbl_demog |> fr_table()
is.fr_spec(spec)     # TRUE
#> [1] TRUE
is.fr_spec(mtcars)   # FALSE
#> [1] FALSE
```
