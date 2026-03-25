# Get Page Configuration from a Spec

Extracts the page layout settings configured via
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md).
Returns a named list with all page properties: orientation, paper size,
margins, font family, font size, and column gap.

## Usage

``` r
fr_get_page(spec)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
  or
  [`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md).

## Value

A named list with fields: `orientation`, `paper`, `margins` (list with
`top`, `bottom`, `left`, `right`), `font_family`, `font_size`,
`col_gap`.

## See also

[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md) to
set page layout,
[`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md)
and
[`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md)
for running headers/footers.

## Examples

``` r
spec <- tbl_demog |> fr_table() |>
  fr_page(orientation = "landscape", font_size = 8)
pg <- fr_get_page(spec)
pg$orientation   # "landscape"
#> [1] "landscape"
pg$font_size     # 8
#> [1] 8
pg$margins$left  # margin in inches
#> [1] 1
```
