# Double Dagger Symbol

Inserts the double dagger symbol (U+2021). Shorthand for
`fr_unicode(0x2021)`.

In regulatory tables the double dagger marks **serious adverse events**
or acts as a secondary marker after the single dagger. See
[`fr_dagger()`](https://vthanik.github.io/arframe/reference/fr_dagger.md)
for the full symbol convention table.

## Usage

``` r
fr_ddagger()
```

## Value

An `fr_markup` object containing the double dagger character.

## See also

[`fr_dagger()`](https://vthanik.github.io/arframe/reference/fr_dagger.md)
for the single dagger and symbol conventions,
[`fr_unicode()`](https://vthanik.github.io/arframe/reference/fr_unicode.md)
for arbitrary Unicode characters.

## Examples

``` r
# Standalone double dagger symbol
fr_ddagger()
#> <fr_markup> ‡

# In footnotes:
spec <- tbl_demog |> fr_table()
spec |> fr_footnotes(
  "{fr_dagger()} Treatment-related adverse event.",
  "{fr_ddagger()} Serious adverse event."
)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (2):
#> 1. [left] "† Treatment-related adverse event."
#> 2. [left] "‡ Serious adverse event."
```
