# Set Running Page Header

Sets the running header printed at the top of every page (or every page
after the first). The header is divided into three independent zones:
`left`, `center`, and `right`.

Supports `{token}` placeholders, evaluated at render time:

- `{thepage}` — current page number.

- `{total_pages}` — total page count.

- Custom tokens defined via `fr_page(tokens = list(...))`.

Calling `fr_pagehead()` again **merges** with the previous header: only
the arguments you explicitly supply are changed.

## Usage

``` r
fr_pagehead(
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

  Character scalar, character vector, or `NULL`. Left zone text. Vectors
  are collapsed with newlines to produce multi-line output.

- center:

  Character scalar, character vector, or `NULL`. Center zone text.

- right:

  Character scalar, character vector, or `NULL`. Right zone text. All
  three zones support token placeholders (e.g. `thepage`) evaluated at
  render time.

- font_size:

  Font size for the header text in points. `NULL` inherits from the page
  font size (`fr_page(font_size = ...)`).

- bold:

  Logical. Whether header text is bold. Default `NULL` (not bold).

## Value

A modified `fr_spec`. Header stored in `spec$pagehead`.

## Regulatory conventions

ICH E3 specifies a **3-part running header** on every table page:

- **Left zone**: Study/protocol identifier.

- **Center zone**: Compound name, document type, or left blank.

- **Right zone**: `"Page X of Y"`.

The **database cutoff date** is a mandatory audit trail element.
Standard table shells show it on the right of the running header on
every page:

    Study/Protocol: TFRM-2024-001          Database Cutoff Date: 31DEC2024

Implement this with:

    fr_page(tokens = list(study = "TFRM-2024-001", cutoff = "31DEC2024")) |>
    fr_pagehead(
      left  = "Study/Protocol: {study}",
      right = "Database Cutoff Date: {cutoff}"
    )

Date format in RTF submissions is `DDMONYYYY` (e.g. `"31DEC2024"`), not
ISO 8601, to match SAS ODS date conventions.

## Parameter Precedence

Settings resolve from four tiers (lowest to highest priority): package
defaults \< `_arframe.yml` \<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
\< this function. Only parameters you explicitly supply override
previous tiers.

## Spacing

By default, **no gap** is inserted after the page header. Add one with
[`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md):

    spec |> fr_spacing(pagehead_after = 1L)   # one blank line
    spec |> fr_spacing(pagehead_after = 2L)   # two blank lines

This can also be set in `_arframe.yml`:

    spacing:
      pagehead_after: 1

## See also

[`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md)
for the footer,
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)
for custom token definitions,
[`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md)
for above-table title lines,
[`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md)
for gap control.

## Examples

``` r
## ── Standard: Study/Protocol + Database Cutoff Date ──────────────────────

tbl_demog |>
  fr_table() |>
  fr_page(tokens = list(study  = "TFRM-2024-001",
                        cutoff = "31DEC2024")) |>
  fr_pagehead(
    left  = "Study/Protocol: {study}",
    right = "Database Cutoff Date: {cutoff}"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── 3-part header: study | CONFIDENTIAL | Page X of Y ────────────────────

tbl_demog |>
  fr_table() |>
  fr_page(tokens = list(study = "TFRM-2024-001")) |>
  fr_pagehead(
    left   = "Study/Protocol: {study}",
    center = "CONFIDENTIAL",
    right  = "Page {thepage} of {total_pages}"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Multi-token header with compound and population ───────────────────────

tbl_demog |>
  fr_table() |>
  fr_page(tokens = list(
    study    = "TFRM-2024-001",
    compound = "Zomerane",
    pop      = "FAS"
  )) |>
  fr_pagehead(
    left   = "{study}  {compound}",
    center = "Population: {pop}",
    right  = "Page {thepage} of {total_pages}"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Smaller header font size ──────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_pagehead(
    left      = "TFRM-2024-001",
    right     = "Page {thepage} of {total_pages}",
    font_size = 7
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Combined pagehead + pagefoot in one pipeline ──────────────────────────

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
