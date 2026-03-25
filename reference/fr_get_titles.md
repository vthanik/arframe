# Get Titles from a Spec

Extracts the list of title entries configured via
[`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md).
Each entry is a list with fields: `content`, `align`, `bold`,
`font_size`.

## Usage

``` r
fr_get_titles(spec)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
  or
  [`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md).

## Value

A list of title entry lists. Empty list if no titles are configured.

## See also

[`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md)
to set titles,
[`fr_get_footnotes()`](https://vthanik.github.io/arframe/reference/fr_get_footnotes.md)
for footnotes.

## Examples

``` r
spec <- tbl_demog |> fr_table() |>
  fr_titles("Table 14.1.1", "Summary of Demographics", .bold = TRUE)
titles <- fr_get_titles(spec)
length(titles)           # 2
#> [1] 2
titles[[1]]$content      # "Table 14.1.1"
#> [1] "Table 14.1.1"
titles[[1]]$bold         # TRUE
#> [1] TRUE
```
