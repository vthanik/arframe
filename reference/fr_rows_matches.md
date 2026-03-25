# Select Rows by Column Value or Pattern

Creates a row selector object for use in the `rows` argument of
[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md)
or
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md).
At style-application time (inside
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)),
the selector is evaluated against the table data frame to produce the
matching row positions.

This avoids hard-coding integer row numbers when styling rows by their
content — for example, bolding every "Total" row or colouring every
p-value row red. The selector is data-driven: if rows are reordered or
filtered, the styles automatically track the correct rows.

## Usage

``` r
fr_rows_matches(col, value = NULL, pattern = NULL, ignore.case = FALSE)
```

## Arguments

- col:

  Character scalar. Name of the data column to match against. Must exist
  in the data frame passed to
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- value:

  Scalar. Exact value to match (using `==`). Mutually exclusive with
  `pattern`. Supports any atomic type (character, numeric, logical).

- pattern:

  Character scalar. A regular expression passed to
  [`grep()`](https://rdrr.io/r/base/grep.html) (Perl-compatible).
  Mutually exclusive with `value`. The regex is matched against the
  character representation of the column values.

- ignore.case:

  Logical. Whether `pattern` matching is case-insensitive. Default
  `FALSE`. Ignored when `value` is used.

## Value

An `fr_rows_selector` object for use in
[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md)
or
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md).

## Common patterns

|                            |                                              |
|----------------------------|----------------------------------------------|
| Pattern                    | Matches                                      |
| `value = "Total"`          | Exact string "Total"                         |
| `pattern = "^Total"`       | Starts with "Total"                          |
| `pattern = "p[- .]?value"` | "p-value", "p value", "p.value"              |
| `pattern = "^\\\\s"`       | Rows starting with whitespace (indented)     |
| `pattern = "^[A-Z]"`       | Rows starting with uppercase (group headers) |

## See also

[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md)
for row styling,
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md)
for cell styling,
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
to apply styles to a spec.

## Examples

``` r
## ── Bold every "Total" row ────────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_row_style(rows = fr_rows_matches("characteristic", "Total"), bold = TRUE)
  )
#> Warning: `fr_rows_matches()`: no rows matched in column "characteristic".
#> ℹ Selector: value Total.
#> ℹ Styles targeting these rows will have no effect.
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 1 override

## ── Red text for p-value rows (regex) ────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_row_style(
      rows = fr_rows_matches("characteristic", pattern = "^p[- .]?value",
                              ignore.case = TRUE),
      color = "#CC0000", italic = TRUE
    )
  )
#> Warning: `fr_rows_matches()`: no rows matched in column "characteristic".
#> ℹ Selector: pattern ^p[- .]?value.
#> ℹ Styles targeting these rows will have no effect.
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 1 override

## ── Combine with cell styles ──────────────────────────────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_styles(
    # Shade every SOC header row
    fr_row_style(rows = fr_rows_matches("row_type", "soc"), background = "#F0F0F0", bold = TRUE),
    # Red text for all PT rows with a p-value pattern
    fr_row_style(rows = fr_rows_matches("row_type", "pt"), color = "#333333")
  )
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 2 overrides

## ── Alternation pattern: match "Total" or "Subtotal" ────────────────────

fr_rows_matches("characteristic", pattern = "Total|Subtotal")
#> $col
#> [1] "characteristic"
#> 
#> $value
#> NULL
#> 
#> $pattern
#> [1] "Total|Subtotal"
#> 
#> $ignore.case
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "fr_rows_selector"

## ── Character class pattern: rows starting with uppercase letter ───────

fr_rows_matches("characteristic", pattern = "^[A-Z]")
#> $col
#> [1] "characteristic"
#> 
#> $value
#> NULL
#> 
#> $pattern
#> [1] "^[A-Z]"
#> 
#> $ignore.case
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "fr_rows_selector"

## ── Combined with fr_style in a pipeline ──────────────────────────────────

tbl_disp |>
  fr_table() |>
  fr_styles(
    fr_row_style(
      rows = fr_rows_matches("category", pattern = "Completed|Discontinued"),
      bold = TRUE, background = "#F0F0F0"
    )
  )
#> 
#> ── fr_spec: Table 
#> Data: 8 rows x 5 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 1 override
```
