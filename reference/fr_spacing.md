# Control Spacing Between Table Sections

Sets the number of blank lines inserted between table sections. These
gaps provide visual separation between the running header, titles, group
label, column headers, body rows, footnotes, and running footer.

The five controllable junctions in the table anatomy:

    [Page Header]
      --- pagehead_after ---
    Titles
      --- titles_after ---
    Page-by Label
      --- page_by_after ---
    Column Headers / Body Rows
      --- footnotes_before ---
    Footnotes
      --- pagefoot_before ---
    [Page Footer]

Calling `fr_spacing()` **replaces** only the specified values.
Unspecified arguments keep their current value.

## Usage

``` r
fr_spacing(
  spec,
  titles_after = NULL,
  footnotes_before = NULL,
  pagehead_after = NULL,
  pagefoot_before = NULL,
  page_by_after = NULL
)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- titles_after:

  Integer. Blank lines between the last title and the column header rule
  (or page-by label if `page_by` is set). Default `1L`. Set to `0L` for
  no gap.

- footnotes_before:

  Integer. Blank lines between the last body row (or bottom rule) and
  the first footnote. Default `1L`. Set to `0L` for no gap.

- pagehead_after:

  Integer. Blank lines between the running page header and the first
  title. Default `0L` (no gap). Set to `1L` for one blank line. Only
  takes effect when a page header is set via
  [`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md).

- pagefoot_before:

  Integer. Blank lines between the last footnote and the running page
  footer. Default `0L`. Set to `1L` or more to add breathing room above
  the footer. Only takes effect when a page footer is set via
  [`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md).

- page_by_after:

  Integer. Blank lines between the page-by label and the column header
  row. Default `1L`. Only takes effect when `page_by` is set via
  [`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md).
  Set to `0L` for no gap.

## Value

A modified `fr_spec`. Spacing stored in `spec$spacing`.

## Pharma conventions

Standard pharma submission tables (ICH E3, FDA/EMA) typically use one
blank line after titles and before footnotes. The package defaults
reflect a minimal layout:

- `titles_after = 1L` — one blank line after titles

- `footnotes_before = 1L` — one blank line before footnotes

- `pagehead_after = 0L` — no gap (user decides)

- `pagefoot_before = 0L` — no gap (user decides)

- `page_by_after = 1L` — one blank line after page-by label

## YAML configuration

All five spacing values can be set in `_arframe.yml`:

    spacing:
      titles_after: 1
      footnotes_before: 1
      pagehead_after: 0
      pagefoot_before: 0
      page_by_after: 1

## Parameter Precedence

Settings resolve from four tiers (lowest to highest priority): package
defaults \< `_arframe.yml` \<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
\< this function. Only parameters you explicitly supply override
previous tiers.

## See also

[`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md),
[`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md),
[`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md),
[`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md),
[`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md).

## Examples

``` r
## ── Default spacing (1 blank line at each gap) ───────────────────────────

tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1 Demographics", "Safety Population") |>
  fr_footnotes("[a] Percentages based on N in column header.")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Table 14.1.1 Demographics"
#> 2. [center] "Safety Population"
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "[a] Percentages based on N in column header."

## ── No gap after titles (compact layout) ─────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_spacing(titles_after = 0L) |>
  fr_titles("Table 14.1.1 Demographics")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (1):
#> 1. [center] "Table 14.1.1 Demographics"
#> Header: valign=bottom

## ── Two blank lines before footnotes ─────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_spacing(footnotes_before = 2L) |>
  fr_footnotes("[a] Percentages based on N in column header.")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "[a] Percentages based on N in column header."

## ── Space after group label (page_by) ────────────────────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_rows(page_by = "soc") |>
  fr_spacing(page_by_after = 1L)
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rows: page_by=soc

## ── Space before page footer ──────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_pagefoot(left = "{program}", right = "{datetime}") |>
  fr_spacing(pagefoot_before = 1L)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── All five spacing parameters in one call ──────────────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_titles("Table 14.3.1 Adverse Events by SOC") |>
  fr_footnotes("[a] MedDRA v26.1.") |>
  fr_rows(page_by = "soc") |>
  fr_pagehead(left = "TFRM-2024-001") |>
  fr_pagefoot(left = "{program}") |>
  fr_spacing(
    titles_after     = 1L,
    footnotes_before = 1L,
    pagehead_after   = 1L,
    pagefoot_before  = 1L,
    page_by_after    = 1L
  )
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (1):
#> 1. [center] "Table 14.3.1 Adverse Events by SOC"
#> Header: valign=bottom
#> Rows: page_by=soc
#> Footnotes (1):
#> 1. [left] "[a] MedDRA v26.1."

## ── Tight layout: zero spacing everywhere ────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1 Demographics") |>
  fr_footnotes("[a] N = number of subjects.") |>
  fr_spacing(
    titles_after     = 0L,
    footnotes_before = 0L,
    pagehead_after   = 0L,
    pagefoot_before  = 0L,
    page_by_after    = 0L
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (1):
#> 1. [center] "Table 14.1.1 Demographics"
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "[a] N = number of subjects."
```
