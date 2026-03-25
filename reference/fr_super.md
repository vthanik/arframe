# Superscript Markup

Renders content as superscript in the final output. Use inside
glue-string expressions in titles, footnotes, column labels, and cell
values. At render time the backend converts the markup to the
appropriate format (RTF control words or LaTeX `\textsuperscript{}`).

The most common use in regulatory tables is for footnote markers:
`"[a]{fr_super('a')}"` in a cell value, paired with
`"[a] Percentages based on N."` in
[`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md).

## Usage

``` r
fr_super(x)
```

## Arguments

- x:

  Character or numeric. Content to render as superscript. Coerced to
  character internally.

## Value

An `fr_markup` object. When interpolated inside a glue string (e.g.
`"{fr_super(1)}"`), produces a sentinel token (`\x01SUPER:1\x02`) that
is resolved at render time.

## See also

[`fr_sub()`](https://vthanik.github.io/arframe/reference/fr_sub.md) for
subscript,
[`fr_bold()`](https://vthanik.github.io/arframe/reference/fr_bold.md)
and
[`fr_italic()`](https://vthanik.github.io/arframe/reference/fr_italic.md)
for font style,
[`fr_dagger()`](https://vthanik.github.io/arframe/reference/fr_dagger.md)
for the † symbol.

## Examples

``` r
# Footnote marker
fr_super("a")
#> <fr_markup> ^{a}

# Unit with exponent: kg/m²
fr_col("BMI kg/m{fr_super(2)}", width = 1.5)
#> <fr_col> "BMI kg/m{fr_super(2)}" [1.50in, left]

# Standalone usage (inspect the sentinel)
format(fr_super(1))
#> [1] "\001SUPER:1\002"

# In a pipeline:
spec <- tbl_demog |> fr_table()
n <- 1
spec |> fr_footnotes("{fr_super(n)} Pearson chi-square test")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "1 Pearson chi-square test"
```
