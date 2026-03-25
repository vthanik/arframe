# Subscript Markup

Renders content as subscript in the final output. Use inside glue-string
expressions in titles, footnotes, column labels, and cell values. Common
in clinical statistics for chemical formulas (H₂O), statistical notation
(beta-1), and indexed variables (x_i).

## Usage

``` r
fr_sub(x)
```

## Arguments

- x:

  Character or numeric. Content to render as subscript. Coerced to
  character internally.

## Value

An `fr_markup` object. When interpolated inside a glue string, produces
a sentinel token resolved at render time.

## Rendering

In RTF output, subscript uses `\sub` (half the font size, lowered). In
future LaTeX output, it maps to `\textsubscript{}`. The subscript text
renders at roughly half the surrounding font size.

## See also

[`fr_super()`](https://vthanik.github.io/arframe/reference/fr_super.md)
for superscript,
[`fr_unicode()`](https://vthanik.github.io/arframe/reference/fr_unicode.md)
for arbitrary Unicode characters.

## Examples

``` r
# Chemical formula: H₂O
fr_col("H{fr_sub(2)}O", width = 1.0)
#> <fr_col> "H{fr_sub(2)}O" [1.00in, left]

# Statistical subscript: x_i
fr_col("x{fr_sub('i')}", width = 1.0)
#> <fr_col> "x{fr_sub('i')}" [1.00in, left]

# In a footnote:
spec <- tbl_demog |> fr_table()
spec |> fr_footnotes("{fr_sub('n')} = number of subjects with data")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "n = number of subjects with data"
```
