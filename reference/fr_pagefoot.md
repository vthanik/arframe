# Set Running Page Footer

Sets the running footer printed at the bottom of every page. The footer
uses the same three-zone (`left`, `center`, `right`) system as
[`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md).

Supports `{token}` placeholders, evaluated at render time:

- `{program}` — R script filename (from `rstudioapi` or `sys.call`).

- `{datetime}` — run datetime in `DDMONYYYY HH:MM:SS` format (e.g.
  `"05MAR2026 14:24:25"`).

- `{thepage}`, `{total_pages}`, and custom tokens are also supported.

Calling `fr_pagefoot()` again **merges** with the previous footer: only
the arguments you explicitly supply are changed.

## Usage

``` r
fr_pagefoot(
  spec,
  left = NULL,
  center = NULL,
  right = NULL,
  font_size = NULL,
  bold = NULL
)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- left:

  Character scalar or `NULL`. Left zone text.

- center:

  Character scalar or `NULL`. Center zone text.

- right:

  Character scalar or `NULL`. Right zone text. All three zones support
  token placeholders evaluated at render time.

- font_size:

  Font size in points. `NULL` inherits from page.

- bold:

  Logical. Default `NULL` (not bold).

## Value

A modified `fr_spec`. Footer stored in `spec$pagefoot`.

## Regulatory conventions

Standard pharma table shells specify the following footer content on
every table page:

- **Left zone**: Program path or filename. Use the `{program}` token,
  which resolves to the R script filename at render time.

- **Center zone**: Confidentiality label if needed.

- **Right zone**: Data source name, data extract date, and run datetime.

Full submission-standard footer:

    fr_page(tokens = list(datasource = "ADSL", extract = "01JAN2025")) |>
    fr_pagefoot(
      left  = "{program}",
      right = "Data Source: {datasource}  Extract: {extract}  Run: {datetime}"
    )

The `{datetime}` token resolves to `DDMONYYYY HH:MM:SS` at render time
(e.g. `"05MAR2026 14:24:25"`).

## Parameter Precedence

Settings resolve from four tiers (lowest to highest priority): package
defaults \< `_arframe.yml` \<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
\< this function. Only parameters you explicitly supply override
previous tiers.

## See also

[`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md)
for the running header,
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)
for custom token definitions.

## Examples

``` r
## ── Program path + run datetime (most common footer pattern) ─────────────

tbl_demog |>
  fr_table() |>
  fr_pagefoot(
    left  = "{program}",
    right = "{datetime}"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Full submission-standard footer: program + data source + runtime ──────

tbl_demog |>
  fr_table() |>
  fr_page(tokens = list(datasource = "ADSL", extract = "01JAN2025")) |>
  fr_pagefoot(
    left  = "{program}",
    right = "Data Source: {datasource}  Extract: {extract}  Run: {datetime}"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Program + confidentiality + datetime (3-zone) ─────────────────────────

tbl_demog |>
  fr_table() |>
  fr_pagefoot(
    left   = "{program}",
    center = "CONFIDENTIAL",
    right  = "{datetime}"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Minimal: page number only in footer ───────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_pagefoot(right = "Page {thepage} of {total_pages}")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Full page chrome: pagehead + pagefoot together ────────────────────────

tbl_demog |>
  fr_table() |>
  fr_page(tokens = list(study = "TFRM-2024-001", cutoff = "31DEC2024")) |>
  fr_pagehead(
    left  = "Study: {study}",
    right = "Cutoff: {cutoff}"
  ) |>
  fr_pagefoot(
    left  = "{program}",
    right = "{datetime}"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
```
