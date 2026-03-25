# Dagger Symbol

Inserts the dagger symbol (U+2020). Shorthand for `fr_unicode(0x2020)`.

In regulatory tables the dagger is used to mark treatment-related
adverse events or to indicate a secondary footnote series when the
`[a]`/`[b]` series is exhausted.

## Usage

``` r
fr_dagger()
```

## Value

An `fr_markup` object containing the dagger character.

## Symbol conventions in pharma TLFs

|  |  |  |
|----|----|----|
| Symbol | Markup | Common use |
| `[a]`, `[b]` | superscript | Primary footnote markers |
| dagger | `fr_dagger()` | Treatment-related AE |
| double dagger | [`fr_ddagger()`](https://vthanik.github.io/arframe/reference/fr_ddagger.md) | Serious AE |
| asterisk | `*` | Significance indicator |

## See also

[`fr_ddagger()`](https://vthanik.github.io/arframe/reference/fr_ddagger.md)
for the double dagger,
[`fr_unicode()`](https://vthanik.github.io/arframe/reference/fr_unicode.md)
for arbitrary Unicode characters,
[`fr_super()`](https://vthanik.github.io/arframe/reference/fr_super.md)
for superscript footnote markers.

## Examples

``` r
# Standalone dagger symbol
fr_dagger()
#> <fr_markup> †

# In a footnote:
spec <- tbl_demog |> fr_table()
spec |> fr_footnotes("{fr_dagger()} Treatment-related adverse event.")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "† Treatment-related adverse event."

# Combined dagger and double dagger:
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
