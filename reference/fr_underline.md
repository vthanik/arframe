# Underline Markup

Renders a text span as underlined in the final output. Use inside
glue-string expressions. Underline is less common in regulatory tables
but is sometimes used for:

- **Confidentiality marks**: "CONFIDENTIAL" in title headers.

- **Hyperlink-style emphasis** in electronic submissions.

For most emphasis needs, prefer
[`fr_bold()`](https://vthanik.github.io/arframe/reference/fr_bold.md) —
it is the standard in pharma TFL outputs.

## Usage

``` r
fr_underline(x)
```

## Arguments

- x:

  Character. Text to underline.

## Value

An `fr_markup` object. When interpolated inside a glue string, produces
a sentinel token resolved at render time.

## Rendering

In RTF output, underline uses `\ul` ... `\ulnone` control words. In
future LaTeX output, it maps to `\underline{}`.

## See also

[`fr_bold()`](https://vthanik.github.io/arframe/reference/fr_bold.md)
for bold,
[`fr_italic()`](https://vthanik.github.io/arframe/reference/fr_italic.md)
for italic.

## Examples

``` r
# Confidentiality marker
fr_underline("CONFIDENTIAL")
#> <fr_markup> __CONFIDENTIAL__

# In a title:
spec <- tbl_demog |> fr_table()
spec |> fr_titles("{fr_underline('CONFIDENTIAL')} - Do Not Distribute")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (1):
#> 1. [center] "CONFIDENTIAL - Do Not Distribute"
#> Header: valign=bottom
```
