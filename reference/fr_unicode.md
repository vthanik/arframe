# Unicode Character by Codepoint

Inserts a Unicode character by its numeric codepoint. At render time the
backend converts to the appropriate escape sequence (RTF `\uN` or LaTeX
command). Use this for symbols not available on the keyboard or for
cross-platform portability.

For the most commonly used symbols in clinical tables, prefer the
dedicated helpers:
[`fr_dagger()`](https://vthanik.github.io/arframe/reference/fr_dagger.md),
[`fr_ddagger()`](https://vthanik.github.io/arframe/reference/fr_ddagger.md).

## Usage

``` r
fr_unicode(codepoint)
```

## Arguments

- codepoint:

  Integer. Unicode codepoint in hex (e.g. `0x00B1` for ±) or decimal
  (e.g. `177L`). Must be a single value.

## Value

An `fr_markup` object containing the UTF-8 character.

## Common codepoints for clinical tables

|  |  |  |
|----|----|----|
| Symbol | Hex | Description |
| ± | `0x00B1` | Plus-minus (lab ranges) |
| ≤ | `0x2264` | Less-than-or-equal |
| ≥ | `0x2265` | Greater-than-or-equal |
| × | `0x00D7` | Multiplication (dosing) |
| ° | `0x00B0` | Degree (temperature) |
| α | `0x03B1` | Alpha (significance level) |
| β | `0x03B2` | Beta (type II error) |
| μ | `0x03BC` | Mu (mean) |
| † | `0x2020` | Dagger (use [`fr_dagger()`](https://vthanik.github.io/arframe/reference/fr_dagger.md)) |
| ‡ | `0x2021` | Double dagger (use [`fr_ddagger()`](https://vthanik.github.io/arframe/reference/fr_ddagger.md)) |

## See also

[`fr_dagger()`](https://vthanik.github.io/arframe/reference/fr_dagger.md)
and
[`fr_ddagger()`](https://vthanik.github.io/arframe/reference/fr_ddagger.md)
for common symbol shortcuts,
[`fr_super()`](https://vthanik.github.io/arframe/reference/fr_super.md)
for superscript notation.

## Examples

``` r
# Plus-minus symbol in a label
fr_col("Mean {fr_unicode(0x00B1)} SD", width = 1.5)
#> <fr_col> "Mean {fr_unicode(0x00B1)} SD" [1.50in, left]

# Degree symbol
fr_col("Temperature ({fr_unicode(0x00B0)}C)", width = 1.5)
#> <fr_col> "Temperature ({fr_unicode(0x00B0)}C)" [1.50in, left]

# In a footnote:
spec <- tbl_demog |> fr_table()
spec |> fr_footnotes("[a] P {fr_unicode(0x2264)} 0.05 considered significant.")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "[a] P ≤ 0.05 considered significant."
```
