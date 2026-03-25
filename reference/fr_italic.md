# Italic Markup

Renders a text span as italic in the final output. Use inside
glue-string expressions to italicize a portion of a title, footnote,
column label, or cell value.

Common in regulatory tables for:

- **P-value annotations**: `"{fr_italic('P')}-value"` renders as
  *P*-value.

- **Statistical method notes**: Fisher's exact, Cochran-Mantel-Haenszel.

- **Latin abbreviations**: *vs.*, *et al.*, *in vitro*.

## Usage

``` r
fr_italic(x)
```

## Arguments

- x:

  Character. Text to render in italic.

## Value

An `fr_markup` object. When interpolated inside a glue string, produces
a sentinel token resolved at render time.

## Rendering

In RTF output, italic uses `\i` ... `\i0` control words. In future LaTeX
output, it maps to `\textit{}`.

## See also

[`fr_bold()`](https://vthanik.github.io/arframe/reference/fr_bold.md)
for bold,
[`fr_underline()`](https://vthanik.github.io/arframe/reference/fr_underline.md)
for underline.

## Examples

``` r
# Italic P-value annotation
fr_italic("P")
#> <fr_markup> _P_

# In footnotes:
spec <- tbl_demog |> fr_table()
spec |> fr_footnotes("[a] {fr_italic('P')}-value from Fisher's exact test.")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "[a] P-value from Fisher's exact test."
spec |> fr_footnotes("Comparison {fr_italic('vs.')} placebo.")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "Comparison vs. placebo."
```
