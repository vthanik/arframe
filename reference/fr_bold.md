# Bold Markup

Renders a text span as bold in the final output. Use inside glue-string
expressions to apply bold formatting to a **portion** of a title,
footnote, column label, or cell value — without bolding the entire line.

**When to use `fr_bold()` vs `bold = TRUE`:**

- `fr_bold("text")` — inline markup for partial bold within a string.

- `fr_row_style(bold = TRUE)` — bold the entire row uniformly.

- `fr_header(bold = TRUE)` — bold all column headers.

## Usage

``` r
fr_bold(x)
```

## Arguments

- x:

  Character. Text to render in bold.

## Value

An `fr_markup` object. When interpolated inside a glue string, produces
a sentinel token resolved at render time.

## Rendering

In RTF output, bold uses `\b` ... `\b0` control words. In future LaTeX
output, it maps to `\textbf{}`. The bold weight matches the surrounding
font family.

## See also

[`fr_italic()`](https://vthanik.github.io/arframe/reference/fr_italic.md)
for italic,
[`fr_underline()`](https://vthanik.github.io/arframe/reference/fr_underline.md)
for underline,
[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md)
for bolding entire rows,
[`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md)
for bolding all column headers.

## Examples

``` r
# Bold a label keyword
fr_col("{fr_bold('Total')} Subjects", width = 1.5)
#> <fr_col> "{fr_bold('Total')} Subjects" [1.50in, left]

# Bold just the table number in a title:
spec <- tbl_demog |> fr_table()
spec |> fr_titles("{fr_bold('Table 14.1.1')} Summary of Demographics")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (1):
#> 1. [center] "Table 14.1.1 Summary of Demographics"
#> Header: valign=bottom
#> Rules: 1 hline(s)
```
