# Add Spanning Column Headers

Adds one or more spanning headers — labels that sit above a group of
columns in an additional header row. Each named argument creates one
span: the argument **name** is the span label; the **value** is a
character vector of column names covered by the span.

Calling `fr_spans()` **appends** spans to any already defined. Call it
multiple times with different `.level` values to build multi-level
spanning headers.

## Usage

``` r
fr_spans(spec, ..., .level = 1L, .hline = TRUE, .gap = NULL)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- ...:

  Named column selections. The name is the span label; the value is
  either a character vector of column names or a tidyselect expression:

  - Character vector: `"Treatment" = c("col1", "col2")`

  - Tidyselect: `"Treatment" = starts_with("zom_")`

  Supported helpers:
  [`tidyselect::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`tidyselect::ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`tidyselect::contains()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`tidyselect::matches()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  and more.

- .level:

  Integer. Vertical level of the span row (`1L` = immediately above the
  column header row; higher numbers are further above). Default `1L`.

- .hline:

  Logical. Whether to draw a thin horizontal line below the spanned
  columns. Default `TRUE`. Set to `FALSE` to suppress the border.

- .gap:

  Logical or `NULL`. Whether to insert narrow gap columns between
  adjacent spanning headers at the same level. Gap columns create a
  clean visual break between span groups in both RTF and PDF output,
  without relying on border trimming. `NULL` (default) inherits the
  current setting (default `TRUE`). Set `FALSE` to disable gap insertion
  and produce continuous span hlines.

## Value

A modified `fr_spec`. Spans are appended to `spec$header$spans`.

## Regulatory conventions

Standard pharma house styles recommend:

- Use spanning headers **only when necessary**. Avoid them for simple
  two-arm tables where the column label already identifies the treatment
  group.

- When used, the span label typically carries the **treatment arm name**
  or **timepoint period**, while the individual column labels carry the
  dose level and N (e.g. `"10 mg\n(N=45)"`).

- Common patterns:

  - Single-level span grouping dose arms under a compound name:
    `"Zomerane" = c("low_dose", "high_dose")`

  - Two-level span: dose arms at level 1, compound at level 2.

  - Timepoint spans: `"Week 12" = c("w12_n", "w12_pct")`.

## Multi-level spanning headers

Build from **inner to outer** (lowest level first):

    spec |>
      fr_spans("Zomerane" = c("low_n", "high_n"), .level = 1L) |>
      fr_spans("Active Treatment" = c("low_n", "high_n"), .level = 2L)

Level 1 spans sit directly above the column labels; level 2 spans sit
above level 1.

## Tips

- Span labels support `{fr_*()}` inline markup (e.g.
  `"{fr_bold('Treatment')}"`).

- A column can appear in at most one span per level. Overlapping spans
  at the same level produce undefined layout behaviour — avoid this.

- Columns not covered by any span at a given level display an empty cell
  in that span row.

- The `.hline` border helps visually separate the span from the column
  labels below it. Set `.hline = FALSE` when spanning is purely
  decorative (e.g. grouping timepoints without a visible divider).

- Span alignment inherits from
  [`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md)
  `align` if set, otherwise defaults to center.

## See also

[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
for individual column labels,
[`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md) for
the column spec constructor,
[`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md)
for header-level styling that applies to spans and column labels.

## Examples

``` r
## ── Single-level treatment group span ────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("Characteristic",    width = 2.5),
    zom_50mg       = fr_col("50 mg",             width = 1.5, align = "right"),
    zom_100mg      = fr_col("100 mg",            width = 1.5, align = "right"),
    placebo        = fr_col("Placebo",            width = 1.5, align = "right"),
    total          = fr_col("Total",              width = 1.5, align = "right"),
    .n = c(zom_50mg = 45, zom_100mg = 45, placebo = 45, total = 135),
    .n_format = "{label}\n(N={n})"
  ) |>
  fr_spans(
    "Zomerane" = c("zom_50mg", "zom_100mg")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (6 visible of 6):
#> characteristic "Characteristic" 2.50in left
#> placebo "Placebo" 1.50in right
#> zom_50mg "50 mg" 1.50in right
#> zom_100mg "100 mg" 1.50in right
#> total "Total" 1.50in right
#> group "group" 0.62in left
#> Header: valign=bottom
#> Spans: 1

## ── Two-level spanning header ─────────────────────────────────────────────

# Build a small example with dose-level columns
dose_data <- data.frame(
  param = "Weight", low_n = "43", low_pct = "31.9%",
  high_n = "47", high_pct = "34.8%"
)
dose_data |>
  fr_table() |>
  fr_spans("10 mg"    = c("low_n",  "low_pct"),  .level = 1L) |>
  fr_spans("25 mg"    = c("high_n", "high_pct"), .level = 1L) |>
  fr_spans("Zomerane" = c("low_n",  "low_pct",
                           "high_n", "high_pct"), .level = 2L)
#> 
#> ── fr_spec: Table 
#> Data: 1 row x 5 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Spans: 3

## ── Markup in span label ─────────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_spans(
    "{fr_bold('Zomerane')}" = c("zom_50mg", "zom_100mg")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Spans: 1

## ── Span without bottom border (.hline = FALSE) ────────────────────────

tbl_demog |>
  fr_table() |>
  fr_spans(
    "Zomerane" = c("zom_50mg", "zom_100mg"),
    .hline = FALSE
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Spans: 1

## ── Single-column span (acts as a group label above one column) ────────

tbl_demog |>
  fr_table() |>
  fr_spans(
    "Zomerane" = c("zom_50mg", "zom_100mg"),
    "Reference" = "placebo"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Spans: 2

## ── Tidyselect: span columns by pattern ────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_spans(
    "Zomerane" = starts_with("zom_")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Spans: 1

## ── Span + fr_header bold interaction ──────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_header(bold = TRUE) |>
  fr_spans(
    "Zomerane" = c("zom_50mg", "zom_100mg")
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: bold, valign=bottom
#> Spans: 1
```
