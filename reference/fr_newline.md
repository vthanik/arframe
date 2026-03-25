# Line Break Within a Text Element

Inserts a line break within a single title line, footnote line, or
column label. Use this when you need a multi-line label that is
logically one element (e.g. a column header that wraps to two lines).

In most cases you can use a literal `"\\n"` in your string instead.
`fr_newline()` is provided for use inside glue expressions where a
literal newline would be awkward or ambiguous.

## Usage

``` r
fr_newline()
```

## Value

An `fr_markup` object that resolves to a line break at render time.

## Rendering

In RTF output, the newline resolves to `\\line` (a soft line break
within the same paragraph). In future LaTeX output, it maps to `\\\\`.
The layout engine accounts for newlines when calculating row height.

## See also

[`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md) for
column labels,
[`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md)
for title lines.

## Examples

``` r
# Multi-line column header via newline markup
fr_col("Treatment{fr_newline()}Arm", width = 1.5)
#> <fr_col> "Treatment{fr_newline()}Arm" [1.50in, left]

# Equivalent using literal newline (simpler — preferred)
fr_col("Treatment\nArm", width = 1.5)
#> <fr_col> "Treatment
#> Arm" [1.50in, left]
```
