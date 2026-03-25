# Set or Update Study-Level Table Theme

`fr_theme()` sets study-level defaults that are automatically applied to
**every** subsequent
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
call. Define font size, page layout, tokens, running header/footer, and
line rules once at the top of your program; all tables inherit these
settings without repeating them.

## Usage

``` r
fr_theme(
  orientation = NULL,
  paper = NULL,
  margins = NULL,
  col_gap = NULL,
  font_family = NULL,
  font_size = NULL,
  space_mode = NULL,
  split = NULL,
  stub = NULL,
  pagehead = NULL,
  pagefoot = NULL,
  tokens = NULL,
  hlines = NULL,
  vlines = NULL,
  spacing = NULL,
  n_format = NULL,
  continuation = NULL,
  group_keep = NULL,
  group_style = NULL,
  page_by_style = NULL,
  header = NULL,
  footnote_separator = NULL
)

fr_theme_set(
  orientation = NULL,
  paper = NULL,
  margins = NULL,
  col_gap = NULL,
  font_family = NULL,
  font_size = NULL,
  space_mode = NULL,
  split = NULL,
  stub = NULL,
  pagehead = NULL,
  pagefoot = NULL,
  tokens = NULL,
  hlines = NULL,
  vlines = NULL,
  spacing = NULL,
  n_format = NULL,
  continuation = NULL,
  group_keep = NULL,
  group_style = NULL,
  page_by_style = NULL,
  header = NULL,
  footnote_separator = NULL
)
```

## Arguments

- orientation:

  `"landscape"` or `"portrait"`. `NULL` leaves unchanged.

- paper:

  `"letter"`, `"a4"`, or `"legal"`. `NULL` leaves unchanged.

- margins:

  Margin(s) in inches (same formats as
  [`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)).
  `NULL` leaves unchanged.

- col_gap:

  Inter-column padding in **points** (integer). `NULL` leaves unchanged.
  See
  [`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)
  for details.

- font_family:

  Font family name. `NULL` leaves unchanged.

- font_size:

  Font size in points. `NULL` leaves unchanged.

- space_mode:

  How to handle leading spaces in cell data. One of `"indent"` (convert
  to paragraph-level indent) or `"preserve"` (keep literal spaces).
  `NULL` leaves unchanged. See
  [`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
  `.space_mode` parameter for details.

- split:

  Logical. `TRUE` to enable column splitting for wide tables, `FALSE` to
  disable. `NULL` leaves unchanged. See
  [`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
  `.split` parameter for details.

- stub:

  Character vector of column names to mark as stub columns (repeated in
  every panel during column splitting). `NULL` leaves unchanged. See
  [`fr_col()`](https://vthanik.github.io/arframe/reference/fr_col.md)
  `stub` parameter for details.

- pagehead:

  Named list with `left`, `center`, `right`, `font_size`, `bold`
  elements — same as
  [`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md)
  arguments. `NULL` leaves unchanged.

- pagefoot:

  Named list with `left`, `center`, `right`, `font_size`, `bold`
  elements — same as
  [`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md)
  arguments. `NULL` leaves unchanged.

- tokens:

  Named list of `{token}` values. `NULL` leaves unchanged.

- hlines:

  Horizontal rule preset string (e.g. `"header"`). `NULL` leaves
  unchanged.

- vlines:

  Vertical rule preset string (e.g. `"box"`). `NULL` leaves unchanged.

- spacing:

  Named list with `titles_after`, `footnotes_before`, `pagehead_after`,
  `pagefoot_before`, `page_by_after` (integer blank lines). `NULL`
  leaves unchanged. See
  [`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md).

- n_format:

  A [glue](https://glue.tidyverse.org/reference/glue.html)-style format
  string for N-count labels. Applied to all tables that use
  `fr_col(n = ...)` or `fr_cols(.n = ...)` without an explicit
  `.n_format`. Available tokens: `{label}` and `{n}`. `NULL` leaves
  unchanged.

- continuation:

  Character scalar appended to column headers on continuation pages
  (e.g. `"(continued)"`). `NULL` leaves unchanged. See
  [`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)
  for details.

- group_keep:

  Logical. Whether `group_by` groups are kept together on the same page.
  `NULL` leaves unchanged. See
  [`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
  for details.

- group_style:

  Named list of style properties for group header rows. Applied to all
  tables that use `group_by` with `label` or `leaf`. `NULL` leaves
  unchanged. See
  [`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
  for details and examples.

- page_by_style:

  Named list of style properties for page_by section labels. Applied to
  all tables that use `fr_rows(page_by = ...)`. Supports: `bold`,
  `italic`, `underline`, `color`, `background`, `font_size`, `align`.
  `NULL` leaves unchanged. Page_by labels are plain text by default.
  Example: `page_by_style = list(bold = TRUE, align = "left")`.

- header:

  Named list of header defaults. Supports all
  [`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md)
  parameters: `bold`, `align`, `valign`, `background`, `color`,
  `font_size`, `repeat_on_page`, plus `span_gap` (logical, insert gap
  columns between adjacent spans, default `TRUE`). `NULL` leaves
  unchanged.

- footnote_separator:

  Logical. Whether to draw a separator above the footnote block. `NULL`
  leaves unchanged.

## Value

Invisibly `NULL`.

## Details

This follows the same pattern as `ggplot2` theme management. Calling
`fr_theme(...)` (or its alias `fr_theme_set(...)`) **merges** new
settings into the current global theme without discarding keys you did
not mention. This is analogous to
[`ggplot2::theme_update()`](https://ggplot2.tidyverse.org/reference/get_theme.html).

Use
[`fr_theme_reset()`](https://vthanik.github.io/arframe/reference/fr_theme_reset.md)
to clear all settings and return to built-in defaults, or
[`fr_theme_get()`](https://vthanik.github.io/arframe/reference/fr_theme_get.md)
to inspect the current state.

## How the theme is applied

When
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
is called, it automatically applies the stored theme to the new
`fr_spec`. You can still override any setting per-table by calling the
corresponding verb after
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).
Per-table overrides always win.

Theme settings are stored in the package's internal environment and
persist for the duration of the R session. Call
[`fr_theme_reset()`](https://vthanik.github.io/arframe/reference/fr_theme_reset.md)
to clear all settings and
[`fr_theme_get()`](https://vthanik.github.io/arframe/reference/fr_theme_get.md)
to inspect the current theme.

## See also

[`fr_theme_get()`](https://vthanik.github.io/arframe/reference/fr_theme_get.md)
to inspect,
[`fr_theme_reset()`](https://vthanik.github.io/arframe/reference/fr_theme_reset.md)
to clear,
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)
for per-table page layout,
[`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md)
and
[`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md)
for running headers and footers,
[`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md)
for per-table gap control.

## Examples

``` r
## ── Define study theme at the top of your program ────────────────────────

fr_theme(
  orientation = "landscape",
  paper       = "letter",
  font_size   = 9,
  tokens      = list(study = "TFRM-2024-001", cutoff = "31DEC2024"),
  pagehead    = list(
    left  = "Study: {study}",
    right = "Database Cutoff: {cutoff}"
  ),
  pagefoot    = list(
    left  = "{program}",
    right = "{datetime}"
  ),
  hlines = "header",
  vlines = "box"
)

# All subsequent fr_table() calls inherit the theme — no need to repeat
tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1 Demographics", "Full Analysis Set")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Table 14.1.1 Demographics"
#> 2. [center] "Full Analysis Set"
#> Header: valign=bottom
#> Rules: 1 hline(s)

tbl_ae_soc |>
  fr_table() |>
  fr_titles("Table 14.3.2 Adverse Events", "Safety Analysis Set")
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Table 14.3.2 Adverse Events"
#> 2. [center] "Safety Analysis Set"
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Set section spacing (blank lines between table parts) ─────────────────

fr_theme(spacing = list(titles_after = 1L, footnotes_before = 1L))

## ── Merge additional settings (does not discard existing ones) ────────────

fr_theme(font_size = 9, hlines = "header")
fr_theme(vlines = "box")          # adds vlines; font_size and hlines kept
fr_theme_get()                    # inspect
#> $orientation
#> [1] "landscape"
#> 
#> $paper
#> [1] "letter"
#> 
#> $font_size
#> [1] 9
#> 
#> $pagehead
#> $pagehead$left
#> [1] "Study: {study}"
#> 
#> $pagehead$right
#> [1] "Database Cutoff: {cutoff}"
#> 
#> 
#> $pagefoot
#> $pagefoot$left
#> [1] "{program}"
#> 
#> $pagefoot$right
#> [1] "{datetime}"
#> 
#> 
#> $tokens
#> $tokens$study
#> [1] "TFRM-2024-001"
#> 
#> $tokens$cutoff
#> [1] "31DEC2024"
#> 
#> 
#> $hlines
#> [1] "header"
#> 
#> $vlines
#> [1] "box"
#> 
#> $spacing
#> $spacing$titles_after
#> [1] 1
#> 
#> $spacing$footnotes_before
#> [1] 1
#> 
#> 

## ── Override a theme setting for one specific table ───────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_page(orientation = "portrait")   # overrides the landscape theme default
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: portrait letter, 9pt Times New Roman
#> Header: valign=bottom
#> Rules: 1 hline(s)

## ── Theme with spacing configuration ──────────────────────────────────────

fr_theme_reset()
fr_theme(
  font_size = 9,
  spacing   = list(titles_after = 2L, footnotes_before = 2L,
                   pagehead_after = 1L)
)
fr_theme_get()$spacing   # check the spacing settings
#> $titles_after
#> [1] 2
#> 
#> $footnotes_before
#> [1] 2
#> 
#> $pagehead_after
#> [1] 1
#> 

## ── Theme with leading-space handling ────────────────────────────────────

fr_theme_reset()
fr_theme(space_mode = "indent")   # default: leading spaces → paragraph indent
fr_theme_get()$space_mode         # "indent"
#> [1] "indent"

fr_theme(space_mode = "preserve") # keep leading spaces as literal characters
fr_theme_get()$space_mode         # "preserve"
#> [1] "preserve"

## ── Theme with footnote_separator ────────────────────────────────────────

fr_theme_reset()
fr_theme(footnote_separator = FALSE, hlines = "header")
fr_theme_get()$footnote_separator   # FALSE
#> [1] FALSE

## ── Theme auto-applied: inspect spec after fr_table() ────────────────────

fr_theme_reset()
fr_theme(font_size = 8, orientation = "portrait", hlines = "header")
spec <- tbl_demog |> fr_table()
spec$page$font_size      # 8 (inherited from theme)
#> [1] 8
spec$page$orientation    # "portrait" (inherited from theme)
#> [1] "portrait"

## ── Reset all theme settings ──────────────────────────────────────────────

fr_theme_reset()
```
