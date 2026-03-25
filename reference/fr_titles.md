# Set Table Titles

Sets one or more title lines displayed above the table body. Each
argument in `...` produces one title line in the rendered output.
Calling `fr_titles()` again **replaces** all previously set titles.

Titles support inline `{fr_*()}` markup — see
[`fr_bold()`](https://vthanik.github.io/arframe/reference/fr_bold.md),
[`fr_italic()`](https://vthanik.github.io/arframe/reference/fr_italic.md),
[`fr_super()`](https://vthanik.github.io/arframe/reference/fr_super.md),
[`fr_unicode()`](https://vthanik.github.io/arframe/reference/fr_unicode.md).

## Usage

``` r
fr_titles(spec, ..., .align = "center", .bold = FALSE, .font_size = NULL)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- ...:

  One or more title lines. Each may be:

  - A **character scalar** — plain text, optionally with `{fr_*()}`
    markup.

  - A **list**. The first element is the text content (can be unnamed).
    Optionally include `align`, `bold`, `font_size` to override per-line
    styling.

- .align:

  Default alignment for all title lines. One of `"left"`, `"center"`
  (default), or `"right"`. Can be overridden per line via the list form.

- .bold:

  Default bold for all title lines. `FALSE` by default.

- .font_size:

  Default font size in points. `NULL` inherits the page font size set by
  [`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md).

## Value

A modified `fr_spec`. Titles are stored in `spec$meta$titles`.

## Regulatory conventions

ICH E3 §10 specifies that each table should carry:

- **Line 1** — Table number and title (e.g.
  `"Table 14.1.1 Summary of Demographics and Baseline Characteristics"`).

- **Line 2** — Population qualifier (e.g. `"Full Analysis Set"` or
  `"Safety Population"`).

- **Line 3** — Study identifier or further qualifier (optional, e.g.
  `"Study TFRM-2024-001"` or `"Database Cutoff: 31DEC2024"`).

**Standard table numbering scheme (ICH E3 / FDA/EMA submissions):**

|          |                                                     |
|----------|-----------------------------------------------------|
| Section  | Content                                             |
| 14.1.x   | Demographics, disposition, baseline characteristics |
| 14.2.x   | Efficacy results (including PK/PD)                  |
| 14.3.1.x | Adverse events & dosing/exposure                    |
| 14.3.2.x | Deaths, serious & significant adverse events        |
| 14.3.4.x | Abnormal laboratory values                          |
| 14.3.5.x | Vital signs, ECG, physical findings                 |
| 16.2.x   | Subject data listings                               |

In TFL outputs, titles are typically **left-aligned**, 9 pt, and not
bold. Use `"center"` alignment for PDF or journal output.

## Spacing

By default, **one blank line** is inserted after the last title and
before the column header. Control this with
[`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md):

    spec |> fr_spacing(titles_after = 0L)   # no gap
    spec |> fr_spacing(titles_after = 2L)   # two blank lines

This can also be set in `_arframe.yml`:

    spacing:
      titles_after: 1

## Tips

- Pass 0 arguments (`fr_titles(spec)`) to clear all titles.

- Markup in titles: use `"{fr_bold('Table 14.1.1')}"` to bold just the
  table number. The glue `{...}` syntax is evaluated at spec-build time.

- `fr_titles()` always **replaces** — chain multiple calls if you want
  to progressively build a pipeline, but only the last call takes
  effect.

## Parameter Precedence

Settings resolve from four tiers (lowest to highest priority): package
defaults \< `_arframe.yml` \<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
\< this function. Only parameters you explicitly supply override
previous tiers.

## See also

[`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md)
for footnotes below the table,
[`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md)
for running page headers,
[`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md)
for gap control,
[`fr_bold()`](https://vthanik.github.io/arframe/reference/fr_bold.md)
and
[`fr_italic()`](https://vthanik.github.io/arframe/reference/fr_italic.md)
for inline markup.

## Examples

``` r
## ── Standard two-line ICH E3 title ──────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1 Summary of Demographics and Baseline Characteristics",
    "Full Analysis Set"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Table 14.1.1 Summary of Demographics and Baseline Charact..."
#> 2. [center] "Full Analysis Set"
#> Header: valign=bottom

## ── Three-line title with study identifier ───────────────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_titles(
    "Table 14.3.2 Adverse Events by System Organ Class and Preferred Term",
    "Safety Analysis Set",
    "Study TFRM-2024-001"
  )
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (3):
#> 1. [center] "Table 14.3.2 Adverse Events by System Organ Class and Pre..."
#> 2. [center] "Safety Analysis Set"
#> 3. [center] "Study TFRM-2024-001"
#> Header: valign=bottom

## ── Inline markup in a title ─────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_titles(
    "{fr_bold('Table 14.1.1')} Summary of Demographics",
    "Full Analysis Set"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Table 14.1.1 Summary of Demographics"
#> 2. [center] "Full Analysis Set"
#> Header: valign=bottom

## ── Per-line styling with list form ──────────────────────────────────────

# List form for per-line styling overrides
tbl_demog |>
  fr_table() |>
  fr_titles(
    list("Table 14.1.1 Summary of Demographics", bold = TRUE, font_size = 10),
    list("Full Analysis Set", align = "left", font_size = 9)
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Table 14.1.1 Summary of Demographics"
#> 2. [left] "Full Analysis Set"
#> Header: valign=bottom

## ── Left-aligned titles (common in pharma RTF) ───────────────────────────

tbl_demog |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1 Summary of Demographics",
    "Full Analysis Set",
    .align = "left"
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [left] "Table 14.1.1 Summary of Demographics"
#> 2. [left] "Full Analysis Set"
#> Header: valign=bottom

## ── Clear all titles ─────────────────────────────────────────────────────

spec <- tbl_demog |> fr_table() |> fr_titles("Old Title")
spec <- spec |> fr_titles()  # removes all titles
```
