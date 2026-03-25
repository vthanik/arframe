# Get Footnotes from a Spec

Extracts the list of footnote entries configured via
[`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md).
Each entry is a list with fields: `content`, `align`, `font_size`,
`placement`.

## Usage

``` r
fr_get_footnotes(spec)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
  or
  [`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md).

## Value

A list of footnote entry lists. Empty list if no footnotes are
configured.

## See also

[`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md)
to set footnotes,
[`fr_get_titles()`](https://vthanik.github.io/arframe/reference/fr_get_titles.md)
for titles.

## Examples

``` r
spec <- tbl_demog |> fr_table() |>
  fr_footnotes("Source: ADSL", "[a] Fisher's exact test")
fns <- fr_get_footnotes(spec)
length(fns)              # 2
#> [1] 2
fns[[1]]$content         # "Source: ADSL"
#> [1] "Source: ADSL"
fns[[2]]$placement       # "every" (default)
#> [1] "every"
```
