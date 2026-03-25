# Test if an Object is an fr_col

Test if an Object is an fr_col

## Usage

``` r
is.fr_col(x)
```

## Arguments

- x:

  An object to test.

## Value

`TRUE` if `x` inherits from `"fr_col"`, `FALSE` otherwise.

## Examples

``` r
col <- fr_col(label = "Treatment", align = "center")
is.fr_col(col)       # TRUE
#> [1] TRUE
is.fr_col("text")    # FALSE
#> [1] FALSE
```
