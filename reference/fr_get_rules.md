# Get Rules from a Spec

Extracts the list of horizontal and vertical rule objects configured via
[`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md),
[`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md),
or
[`fr_grid()`](https://vthanik.github.io/arframe/reference/fr_grid.md).
Each element is an `fr_rule` object describing position, width, colour,
and line style.

## Usage

``` r
fr_get_rules(spec)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
  or
  [`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md).

## Value

A list of `fr_rule` objects. Empty list if no rules are configured.

## See also

[`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md)
for horizontal rules,
[`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md)
for vertical rules,
[`fr_grid()`](https://vthanik.github.io/arframe/reference/fr_grid.md)
for full grid.

## Examples

``` r
spec <- tbl_demog |> fr_table() |> fr_hlines("header")
rules <- fr_get_rules(spec)
length(rules)            # depends on "header" preset
#> [1] 1
rules[[1]]$direction     # "horizontal"
#> [1] "horizontal"
```
