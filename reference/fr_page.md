# Set Page Layout Options

Sets page-level properties: paper size, orientation, margins, font, and
pagination controls. Only the arguments you **explicitly supply** are
changed — all others retain their current values. This allows
incremental updates anywhere in a pipeline.

## Usage

``` r
fr_page(
  spec,
  orientation = NULL,
  paper = NULL,
  margins = NULL,
  font_family = NULL,
  font_size = NULL,
  orphan_min = NULL,
  widow_min = NULL,
  continuation = NULL,
  col_gap = NULL,
  tokens = NULL
)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- orientation:

  `"landscape"` (default, 11 × 8.5 in for Letter) or `"portrait"` (8.5 ×
  11 in).

- paper:

  `"letter"` (default, 8.5 × 11 in), `"a4"` (210 × 297 mm), or `"legal"`
  (8.5 × 14 in).

- margins:

  Margin(s) in inches. Accepts:

  - Scalar: `1` — all four sides equal.

  - Length 2: `c(vertical, horizontal)` — top/bottom vs left/right.

  - Length 4: `c(top, right, bottom, left)` — CSS order.

  - Named list: `list(top=1, bottom=1, left=0.75, right=0.75)`.

- font_family:

  Font family name. Default: `"Times New Roman"` (FDA recommended for
  submission documents).

  ### Font Resolution Order

  arframe resolves fonts in this priority:

  1.  **FDA-recommended** — Times New Roman (serif), Calibri/Arial
      (sans-serif), Courier New (monospace)

  2.  **`ARFRAME_FONT_DIR`** — custom fonts from the directory specified
      by this environment variable (`.ttf`/`.otf` files). Ideal for
      Docker, CI, or project-local fonts.

  3.  **Adobe open-source** (SIL OFL, free) — Source Serif 4 (serif),
      Source Sans 3 (sans-serif), Source Code Pro (monospace). Install
      from Google Fonts or <https://github.com/adobe-fonts>.

  4.  **CSS generic** — `serif`, `sans-serif`, `monospace` (HTML only;
      RTF/PDF viewers substitute the closest available font).

  Three font families are supported:

  - **Serif** (roman): `"Times New Roman"`, `"Source Serif 4"`,
    `"Georgia"`, etc.

  - **Sans-serif** (swiss): `"Calibri"`, `"Arial"`, `"Source Sans 3"`,
    etc.

  - **Monospace** (modern): `"Courier New"`, `"Source Code Pro"`,
    `"Consolas"`, etc.

- font_size:

  Font size in points. Default `9`. Typical pharma range: 7–10 pt.

- orphan_min:

  Minimum body rows to keep at the **bottom** of a page before forcing a
  page break. Default `3L`. Set to `1L` to disable.

- widow_min:

  Minimum body rows to carry to the **top** of the next page. Default
  `3L`. Set to `1L` to disable.

- continuation:

  Character scalar appended to the column header block on continuation
  pages, e.g. `"(continued)"`. `NULL` (default) disables.

- col_gap:

  Inter-column padding in **points** (total gap between adjacent
  columns). Default `4` (2 pt left + 2 pt right per cell). Increase to
  `6` or `8` if long text in adjacent columns runs together. Set to `0`
  for zero padding (cells flush, legacy behaviour). Applied
  symmetrically as half the value on each side of every cell in both RTF
  and PDF output.

- tokens:

  Named list of custom `{token}` values for use in page headers/footers.
  E.g. `list(study = "TFRM-2024-001", pop = "FAS")`. Use these tokens in
  [`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md)
  and
  [`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md)
  strings.

## Value

A modified `fr_spec`. Page settings stored in `spec$page`.

## Regulatory conventions

**FDA/US submissions (eCTD):** landscape Letter (8.5 × 11 in), Times New
Roman 9 pt, 1 in margins on all sides. This is the most common
regulatory RTF standard and the package default.

**EMA/European submissions:** A4 paper (210 × 297 mm) is acceptable;
landscape A4 is common for wide safety tables.

**Database cutoff date** is a mandatory element in every CSR table. The
standard practice is to define it as a custom token:

    fr_page(tokens = list(cutoff = "31DEC2024")) |>
    fr_pagehead(left  = "Study: {study}",
                right = "Database Cutoff: {cutoff}")

**Continuation label:** multi-page tables must indicate continuation.
Regulatory convention is to append `"(continued)"` to the column header
block on all pages after the first:

    fr_page(continuation = "(continued)")

## Tips

- `tokens` are the clean way to inject study-level metadata (study
  number, cutoff date, population) into running headers without
  repeating the value in every
  [`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md)
  call.

- `orphan_min = 3` means: if fewer than 3 rows would remain at the
  bottom of a page before a `group_by` group, move the group to the next
  page.

## Parameter Precedence

Settings resolve from four tiers (lowest to highest priority): package
defaults \< `_arframe.yml` \<
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
\< this function. Only parameters you explicitly supply override
previous tiers.

## Precedence

`font_size` applies to all table text unless overridden:

`fr_page(font_size=)` \< `fr_header(font_size=)` \<
`fr_pagehead(font_size=)` / `fr_pagefoot(font_size=)` \<
`fr_style(font_size=)`

## See also

[`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md),
[`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md)
for running headers/footers,
[`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
for `page_by` / `group_by` row pagination,
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
with `.split` for column splitting.

## Examples

``` r
## ── Standard pharma RTF setup ────────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_page(
    orientation = "landscape",
    paper       = "letter",
    font_family = "Times New Roman",
    font_size   = 9,
    margins     = 1
  )
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── A4 portrait for European submissions ─────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_page(orientation = "portrait", paper = "a4", font_size = 9)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: portrait a4, 9pt Times New Roman
#> Header: valign=bottom

## ── Narrow margins to fit wide table ─────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_page(margins = c(1, 0.5))   # top/bottom = 1in, left/right = 0.5in
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Custom tokens for running header ─────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_page(tokens = list(study = "TFRM-2024-001", pop = "FAS")) |>
  fr_pagehead(left  = "Study: {study}  Population: {pop}",
              right = "Page {thepage} of {total_pages}")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Continuation label on multi-page tables ──────────────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_page(continuation = "(continued)")
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Tight orphan/widow control ────────────────────────────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_page(orphan_min = 2L, widow_min = 2L)
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Wider inter-column padding ─────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_page(col_gap = 8)   # 4 pt each side (generous spacing)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman, col_gap=8pt
#> Header: valign=bottom

## ── Legal paper size (8.5 x 14 in) ─────────────────────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_page(paper = "legal", orientation = "landscape", margins = 1)
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape legal, 9pt Times New Roman
#> Header: valign=bottom

## ── Custom fonts via ARFRAME_FONT_DIR (Docker/CI) ────────────────────

# Set ARFRAME_FONT_DIR to a directory of .ttf/.otf files;
# XeLaTeX discovers them by name --- no system install needed.
# Sys.setenv(ARFRAME_FONT_DIR = "/path/to/fonts")
```
