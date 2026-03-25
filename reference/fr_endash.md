# En Dash

Inserts an en dash (U+2013, "–"). Shorthand for `fr_unicode(0x2013)`.

The en dash is used in regulatory tables for numeric ranges (e.g. "18–65
years"), date ranges, and confidence intervals. Using `fr_endash()`
instead of a literal "–" character guarantees correct rendering in both
RTF and PDF output.

## Usage

``` r
fr_endash()
```

## Value

An `fr_markup` object containing the en dash character.

## See also

[`fr_emdash()`](https://vthanik.github.io/arframe/reference/fr_emdash.md)
for the longer em dash,
[`fr_unicode()`](https://vthanik.github.io/arframe/reference/fr_unicode.md)
for arbitrary Unicode characters.

## Examples

``` r
# Numeric range in a footnote:
spec <- tbl_demog |> fr_table()
spec |> fr_footnotes("[a] Age range: 18{fr_endash()}65 years.")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "[a] Age range: 18–65 years."

# In a column label:
fr_col("95% CI{fr_newline()}(Lower{fr_endash()}Upper)", width = 1.5)
#> <fr_col> "95% CI{fr_newline()}(Lower{fr_endash()}Upper)" [1.50in, left]
```
