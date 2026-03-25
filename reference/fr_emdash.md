# Em Dash

Inserts an em dash (U+2014, "—"). Shorthand for `fr_unicode(0x2014)`.

The em dash is used in regulatory table titles and footnotes to separate
clauses, e.g. "Safety Population — All Randomized Subjects". Using
`fr_emdash()` instead of a literal "—" character guarantees correct
rendering in both RTF and PDF output regardless of file encoding.

## Usage

``` r
fr_emdash()
```

## Value

An `fr_markup` object containing the em dash character.

## See also

[`fr_endash()`](https://vthanik.github.io/arframe/reference/fr_endash.md)
for the shorter en dash,
[`fr_unicode()`](https://vthanik.github.io/arframe/reference/fr_unicode.md)
for arbitrary Unicode characters.

## Examples

``` r
# In a title:
spec <- tbl_demog |> fr_table()
spec |> fr_titles("Safety Population {fr_emdash()} FAS")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (1):
#> 1. [center] "Safety Population — FAS"
#> Header: valign=bottom

# In a footnote:
spec |> fr_footnotes("Source: ADSL {fr_emdash()} Safety Population")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "Source: ADSL — Safety Population"
```
