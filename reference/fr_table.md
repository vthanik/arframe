# Start a arframe Table Pipeline

`fr_table()` is the entry point for every **arframe** pipeline. It wraps
a presentation-ready data frame in an `fr_spec` object, which you then
configure with the `fr_*()` verbs and finally render with
[`fr_render()`](https://vthanik.github.io/arframe/reference/fr_render.md).

The data frame should already contain the rows and columns you want to
display — arframe does not summarise or reshape data. Use packages such
as **gt**, **Tplyr**, **rtables**, **tidytlg**, or **tfrmt** to
summarise your data, then hand the summary data frame off to
`fr_table()`.

## Usage

``` r
fr_table(data)
```

## Arguments

- data:

  A data frame (or tibble). Must already be presentation-ready: each row
  maps to one table row, each column to one table column. No
  summarisation is performed by arframe.

## Value

An `fr_spec` object. Pass it to any `fr_*()` verb via `|>`.

## Pipeline overview

    data |>
      fr_table()                        # start
      |> fr_titles(...)                 # titles above the table
      |> fr_footnotes(...)              # footnotes below
      |> fr_cols(col = fr_col(...), .n = ...) # columns / N-counts
      |> fr_header(bold = TRUE, align = .)   # header presentation
      |> fr_spans(...)                  # spanning headers
      |> fr_rows(page_by = ...)         # pagination / grouping
      |> fr_page(orientation = ...)     # page layout
      |> fr_pagehead(left = ...)        # running header
      |> fr_pagefoot(right = ...)       # running footer
      |> fr_hlines("header")         # horizontal rules
      |> fr_vlines("box")               # vertical rules
      |> fr_styles(fr_row_style(...))   # cell / row / column styling
      |> fr_render("output.rtf")        # render to file

## Tips

- Any `fr_*()` verb can be called multiple times; most **replace** (not
  append) the previous setting. Exceptions:
  [`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
  and
  [`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
  **append** on repeated calls.

- Column order in the rendered table matches the column order of `data`.
  Reorder columns in `data` before calling `fr_table()` if needed.

- Invisible columns (e.g. grouping keys) can be hidden via
  `fr_cols(col = fr_col(visible = FALSE))` rather than dropping them
  from `data`.

## See also

[`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md),
[`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md),
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md),
[`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md),
[`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md),
[`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md),
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md),
[`fr_hlines()`](https://vthanik.github.io/arframe/reference/fr_hlines.md),
[`fr_vlines()`](https://vthanik.github.io/arframe/reference/fr_vlines.md),
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md),
[`fr_config()`](https://vthanik.github.io/arframe/reference/fr_config.md),
[`fr_render()`](https://vthanik.github.io/arframe/reference/fr_render.md)

## Examples

``` r
## ── Minimal: one-step pipeline ───────────────────────────────────────────

tbl_demog |> fr_table()
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Header: valign=bottom

## ── Typical regulatory pipeline ─────────────────────────────────────────

tbl_demog |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1 Summary of Demographics and Baseline Characteristics",
    "Full Analysis Set"
  ) |>
  fr_footnotes("[a] Percentages based on the number of subjects in each arm.") |>
  fr_hlines("header") |>
  fr_page(orientation = "landscape", font_size = 9)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Table 14.1.1 Summary of Demographics and Baseline Charact..."
#> 2. [center] "Full Analysis Set"
#> Header: valign=bottom
#> Rules: 1 hline(s)
#> Footnotes (1):
#> 1. [left] "[a] Percentages based on the number of subjects in each arm."

## ── AE by SOC/PT with pagination ─────────────────────────────────────────

tbl_ae_soc |>
  fr_table() |>
  fr_titles("Table 14.3.2 Adverse Events by System Organ Class and Preferred Term",
            "Safety Analysis Set") |>
  fr_rows(page_by = "soc", group_by = "soc") |>
  fr_hlines("header")
#> Warning: `page_by` and `group_by` share column: "soc".
#> ℹ `group_by` grouping is applied within each `page_by` page.
#> 
#> ── fr_spec: Table 
#> Data: 96 rows x 7 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Table 14.3.2 Adverse Events by System Organ Class and Pre..."
#> 2. [center] "Safety Analysis Set"
#> Header: valign=bottom
#> Rows: page_by=soc, group_by=soc
#> Rules: 1 hline(s)

## ── Store spec, reuse, branch ────────────────────────────────────────────

base_spec <- tbl_demog |>
  fr_table() |>
  fr_page(orientation = "landscape", font_size = 9)

# Branch A: portrait with smaller font
spec_a <- base_spec |> fr_page(orientation = "portrait", font_size = 8)

# Branch B: add titles and render
spec_b <- base_spec |>
  fr_titles("Table 14.1.1 Demographics") |>
  fr_hlines("booktabs")

## ── Complete 10-verb pipeline ─────────────────────────────────────────────
# Demonstrates every major pipeline verb in a single specification.
# In study programs, hlines/header/pagehead/pagefoot go in fr_theme().

tbl_demog |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1 Summary of Demographics and Baseline Characteristics",
    "Full Analysis Set"
  ) |>
  fr_footnotes(
    "[a] Percentages based on subjects in each arm.",
    "Source: ADSL"
  ) |>
  fr_cols(
    characteristic = fr_col("Characteristic", width = 2.5),
    placebo        = fr_col("Placebo\n(N=45)", width = 1.5),
    zom_50mg       = fr_col("Zomerane 50mg\n(N=45)", width = 1.5),
    zom_100mg      = fr_col("Zomerane 100mg\n(N=45)", width = 1.5),
    total          = fr_col("Total\n(N=135)", width = 1.5),
    group          = fr_col(visible = FALSE)
  ) |>
  fr_header(bold = TRUE, background = "#D9E2F3") |>
  fr_spans("Treatment Arm" = c("placebo", "zom_50mg", "zom_100mg")) |>
  fr_rows(group_by = "group", blank_after = "group", indent_by = "group") |>
  fr_hlines("header") |>
  fr_vlines("inner") |>
  fr_styles(
    fr_row_style(rows = 1, bold = TRUE)
  ) |>
  fr_page(orientation = "landscape", font_size = 9) |>
  fr_pagehead(left = "Study TFRM-2024-001", right = "Page {thepage} of {total_pages}") |>
  fr_pagefoot(left = "{program}", right = "{datetime}") |>
  fr_spacing(titles_after = 1, footnotes_before = 1)
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Table 14.1.1 Summary of Demographics and Baseline Charact..."
#> 2. [center] "Full Analysis Set"
#> Columns (5 visible of 6):
#> characteristic "Characteristic" 2.50in left
#> placebo "Placebo (N=45)" 1.50in left
#> zom_50mg "Zomerane 50mg (N=45)" 1.50in left
#> zom_100mg "Zomerane 100mg (N..." 1.50in left
#> total "Total (N=135)" 1.50in left
#> Header: bold, valign=bottom
#> Rows: group_by=group, indent_by=group
#> Rules: 1 hline(s)
#> Spans: 1
#> Styles: 1 override
#> Footnotes (2):
#> 1. [left] "[a] Percentages based on subjects in each arm."
#> 2. [left] "Source: ADSL"

## ── fr_table() vs fr_listing() on the same data ──────────────────────────
# fr_table() uses auto-detected defaults; fr_listing() sets listing defaults.

ae_subset <- adae[1:20, c("USUBJID", "AEDECOD", "AESEV", "ASTDT")]

# Table view: auto-detected alignment, 9pt, no split
spec_tbl <- ae_subset |>
  fr_table() |>
  fr_titles("Table View of AE Records")
spec_tbl$page$font_size    # 9 (default)
#> [1] 9

# Listing view: left-aligned, 8pt, split = TRUE, wrap = TRUE
spec_lst <- ae_subset |>
  fr_listing() |>
  fr_titles("Listing View of AE Records")
spec_lst$page$font_size          # 8 (listing default)
#> [1] 8
spec_lst$columns_meta$split      # TRUE (listing default)
#> [1] TRUE

## ── Hidden column used for grouping ──────────────────────────────────────
# The "group" column drives indent_by but is not displayed.

tbl_demog |>
  fr_table() |>
  fr_cols(
    group = fr_col(visible = FALSE)
  ) |>
  fr_rows(indent_by = "group") |>
  fr_titles("Table 14.1.1 Demographics (grouped, hidden key)")
#> 
#> ── fr_spec: Table 
#> Data: 28 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Titles (1):
#> 1. [center] "Table 14.1.1 Demographics (grouped, hidden key)"
#> Columns (5 visible of 6):
#> characteristic "characteristic" 1.85in left
#> placebo "placebo" 0.61in left
#> zom_50mg "zom_50mg" 0.64in left
#> zom_100mg "zom_100mg" 0.70in left
#> total "total" 0.61in left
#> Header: valign=bottom
#> Rows: indent_by=group
```
