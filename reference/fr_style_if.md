# Create a Conditional Style Override

Creates a data-driven style that is evaluated against cell values at
application time (inside
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)).
Rows matching the condition receive the specified style properties. This
avoids hard-coding row indices when styling based on content.

## Usage

``` r
fr_style_if(
  condition,
  cols = NULL,
  apply_to = "cell",
  bold = NULL,
  italic = NULL,
  underline = NULL,
  color = NULL,
  background = NULL,
  font_size = NULL,
  align = NULL,
  valign = NULL
)
```

## Arguments

- condition:

  A one-sided formula or function that returns a logical vector. The
  formula uses `.x` as the pronoun for the column values (converted via
  [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html)):

  - `~ .x == "Total"` — match exact text

  - `~ as.numeric(.x) < 0.05` — numeric comparison

  - `~ grepl("^N=", .x)` — pattern matching

  When `cols = NULL`, `.x` receives `seq_len(nrow(data))` (row indices),
  useful for zebra striping: `~ (.x %% 2) == 0`.

- cols:

  Column selection for the condition. Accepts any of:

  - A character vector of column names:
    `cols = c("zom_50mg", "placebo")`

  - A tidyselect expression: `cols = starts_with("zom_")`

  - `NULL` (default) — row-index-based conditions (e.g., zebra
    striping).

  The condition is applied to the values of each selected column
  independently. Tidyselect expressions are resolved when the style is
  applied via
  [`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).

- apply_to:

  Character scalar. Controls scope of matched rows:

  - `"cell"` (default): style only the cells where the condition is
    TRUE.

  - `"row"`: style **all** columns in rows where the condition is TRUE.

- bold, italic, underline:

  Logical or `NULL` to leave unchanged.

- color:

  Foreground (text) colour: hex string or CSS named colour, or `NULL`.

- background:

  Background (fill) colour: hex string or CSS named colour, or `NULL`.

- font_size:

  Font size in points, or `NULL`.

- align:

  Horizontal alignment (`"left"`, `"center"`, `"right"`, `"decimal"`),
  or `NULL`.

- valign:

  Vertical alignment (`"top"`, `"middle"`, `"bottom"`), or `NULL`. See
  [`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md)
  **Alignment model**.

## Value

An `fr_conditional_style` object for use in
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md).

## See also

[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md),
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md),
[`fr_rows_matches()`](https://vthanik.github.io/arframe/reference/fr_rows_matches.md)

## Examples

``` r
## ── Bold "Total" rows ─────────────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_style_if(
      cols = "characteristic",
      condition = ~ .x == "Total",
      apply_to = "row",
      bold = TRUE
    )
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Zebra striping ────────────────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_style_if(
      condition = ~ (.x %% 2) == 0,
      background = "#F5F5F5",
      apply_to = "row"
    )
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 1 override

## ── Numeric condition: highlight small p-values ──────────────────────────

# Create a table with a p-value column
pval_data <- data.frame(
  characteristic = c("Age", "Sex", "Weight"),
  treatment = c("50 (23.5)", "30 (14.1)", "45 (21.1)"),
  placebo   = c("55 (25.8)", "28 (13.1)", "52 (24.4)"),
  pvalue    = c("0.042", "0.310", "0.003"),
  stringsAsFactors = FALSE
)

pval_data |>
  fr_table() |>
  fr_styles(
    fr_style_if(
      cols = "pvalue",
      condition = ~ as.numeric(.x) < 0.05,
      apply_to = "row",
      bold = TRUE, color = "#CC0000"
    )
  )
#> 
#> ── fr_spec: Table 
#> Data: 3 rows x 4 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 1 override

## ── Pattern matching with grepl ────────────────────────────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_styles(
    fr_style_if(
      cols = "soc",
      condition = ~ grepl("SKIN|GASTROINTESTINAL", .x, ignore.case = TRUE),
      apply_to = "row",
      background = "#FFF3CD"
    )
  )
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 1 override

## ── apply_to = "cell": colour only the matching cell ───────────────────────

tbl_disp |>
  fr_table() |>
  fr_styles(
    fr_style_if(
      cols = c("placebo", "zom_50mg", "zom_100mg"),
      condition = ~ grepl("0", .x),
      apply_to = "cell",
      color = "#999999", italic = TRUE
    )
  )
#> 
#> ── fr_spec: Table 
#> Data: 8 rows x 5 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 3 overrides

## ── Function form (not formula) for the condition ──────────────────────────

is_total <- function(x) x == "Total"

tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_style_if(
      cols = "characteristic",
      condition = is_total,
      apply_to = "row",
      bold = TRUE, background = "#E8E8E8"
    )
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Multiple cols: evaluate condition on several columns ───────────────────

tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_style_if(
      cols = c("placebo", "zom_50mg", "zom_100mg", "total"),
      condition = ~ grepl("^0", .x),
      apply_to = "cell",
      color = "#999999"
    )
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Styles: 2 overrides
```
