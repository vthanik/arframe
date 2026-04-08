
# arframe <img src="man/figures/logo.png" align="right" height="139" alt="" />

> Regulatory-grade clinical tables, listings, and figures in RTF, PDF, and HTML from a single specification.

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/vthanik/arframe)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**arframe** is an R package for pharmaceutical submissions. You describe the output once; arframe renders RTF, PDF, and HTML — change the file extension, the output adapts.

```r
tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1", "Demographics and Baseline Characteristics") |>
  fr_cols(
    characteristic = fr_col("", width = 2.5),
    placebo        = fr_col("Placebo", align = "decimal"),
    zom_50mg       = fr_col("Zomerane 50mg", align = "decimal"),
    zom_100mg      = fr_col("Zomerane 100mg", align = "decimal"),
    total          = fr_col("Total", align = "decimal"),
    group          = fr_col(visible = FALSE),
    .n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)
  ) |>
  fr_hlines("header") |>
  fr_render("Table_14_1_1.rtf")  # Change to .pdf — same output, different format
```

## Why arframe?

The pharmaverse has excellent tools for data derivation (admiral), analysis (Tplyr, rtables, tern), and summarisation (gtsummary, tfrmt). But no single package closes the **last mile**: taking a finished analysis table and producing submission-grade RTF, PDF, and HTML from the same specification.

- **r2rtf** produces RTF only. Zero PDF.
- **tfrmt + docorator** produces good PDF. RTF is documented as experimental/lossy.
- **gt / gtsummary** produces HTML-first output. RTF page headers are not field-code-based.
- **None** produce all three formats from one spec with running page headers, decimal alignment, and study-wide font control.

arframe is a native rendering engine — it writes RTF 1.9.1 control words and tabularray/LaTeX directly. There is no intermediate gt or pandoc layer. Both RTF and PDF are first-class outputs, produced from the same `fr_spec` without a format-specific pipeline.

| Capability | arframe |
|---|---|
| Native RTF rendering | Direct RTF 1.9.1 — no intermediate HTML or gt layer |
| Native PDF rendering | Direct tabularray/XeLaTeX — no RMarkdown/Quarto pipeline |
| HTML preview | Self-contained HTML with paper simulation and viewer integration |
| **Single spec → RTF + PDF + HTML** | Same `fr_spec` renders to `.rtf`, `.pdf`, or `.html` |
| **Font family** (Courier New, Times New Roman) | `fr_page(font_family = "Courier New")` — first-class parameter via fontspec |
| **Automatic decimal alignment** | AFM font metrics for 15 stat types — no manual markers |
| **Auto column width** | Physical width from AFM metrics for 12 font variants |
| Group-aware pagination | Keeps SOC groups together, repeats headers, adds continuation text |
| Page headers/footers | 3-slot layout with live field tokens: `{thepage}`, `{total_pages}`, `{program}` |
| **Study-wide config** | `_arframe.yml` auto-discovered — one file sets font, margins, headers for all programs |
| Inline markup | `fr_super()`, `fr_bold()`, `fr_italic()`, `fr_dagger()` in any cell |
| Dependencies | 7 imports — no dplyr, tidyr, purrr, or gt |

## Installation

```r
# Install from GitHub
# install.packages("pak")
pak::pak("vthanik/arframe")
```

## Quick start

arframe ships with synthetic CDISC ADaM datasets so every example runs out of the box.

### A table in 3 lines

```r
library(arframe)

tbl_demog |> fr_table() |> fr_render(tempfile(fileext = ".rtf"))
```

### A production demographics table

```r
# Set shared formatting once — inherited by all tables
fr_theme(
  font_size   = 9,
  font_family = "Times New Roman",
  orientation = "landscape",
  hlines      = "header",
  header      = list(bold = TRUE, align = "center"),
  n_format    = "{label}\n(N={n})",
  pagehead    = list(left = "TFRM-2024-001", right = "CONFIDENTIAL"),
  pagefoot    = list(left = "{program}",
                     right = "Page {thepage} of {total_pages}")
)

n_itt <- c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)

spec <- tbl_demog |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1",
    "Demographics and Baseline Characteristics",
    "Intent-to-Treat Population"
  ) |>
  fr_cols(
    .width = "fit",
    characteristic = fr_col("", width = 2.5),
    placebo        = fr_col("Placebo", align = "decimal"),
    zom_50mg       = fr_col("Zomerane 50mg", align = "decimal"),
    zom_100mg      = fr_col("Zomerane 100mg", align = "decimal"),
    total          = fr_col("Total", align = "decimal"),
    group          = fr_col(visible = FALSE),
    .n = n_itt
  ) |>
  fr_rows(group_by = "group", blank_after = "group") |>
  fr_footnotes("Percentages based on number of subjects per treatment group.")

# Render both formats from the same spec
fr_render(spec, tempfile(fileext = ".rtf"))
fr_render(spec, tempfile(fileext = ".pdf"))
```

### An AE table with SOC/PT hierarchy

```r
tbl_ae_soc |>
  fr_table() |>
  fr_titles(
    "Table 14.3.1",
    "Treatment-Emergent Adverse Events by SOC and Preferred Term",
    "Safety Population"
  ) |>
  fr_page(continuation = "(continued)") |>
  fr_cols(
    soc       = fr_col(visible = FALSE),
    pt        = fr_col("System Organ Class\n  Preferred Term", width = 3.5),
    row_type  = fr_col(visible = FALSE),
    placebo   = fr_col("Placebo", align = "decimal"),
    zom_50mg  = fr_col("Zomerane\n50mg", align = "decimal"),
    zom_100mg = fr_col("Zomerane\n100mg", align = "decimal"),
    total     = fr_col("Total", align = "decimal"),
    .n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)
  ) |>
  fr_rows(group_by = "soc", indent_by = "pt") |>
  fr_styles(
    fr_row_style(rows = fr_rows_matches("row_type", value = "soc"), bold = TRUE)
  ) |>
  fr_render(tempfile(fileext = ".rtf"))
```

## Pipeline verbs

Every verb takes an `fr_spec`, returns a modified `fr_spec`. Verb order doesn't matter — arframe resolves everything at render time.

| Verb | Purpose |
|---|---|
| `fr_table()` / `fr_listing()` / `fr_figure()` | Entry points — create spec from data or plot (figures support a list of plots with per-page metadata tokens) |
| `fr_cols()` + `fr_col()` | Column labels, widths, alignment, visibility, N-counts |
| `fr_header()` | Header styling (bold, align, valign, colors) |
| `fr_titles()` | Table titles (1-4 lines, with inline markup) |
| `fr_footnotes()` | Footnotes with placement control (`"every"` or `"last"`) |
| `fr_rows()` | `group_by` (with list form for `label`), `group_keep`, `page_by` (with list form for `visible`), `indent_by` (multi-level), `blank_after`, `sort_by`, `suppress`, `wrap` |
| `fr_page()` | Paper size, orientation, margins, font, continuation text |
| `fr_pagehead()` / `fr_pagefoot()` | Running headers/footers with token substitution |
| `fr_hlines()` / `fr_vlines()` / `fr_grid()` | Horizontal/vertical rules (8 presets) |
| `fr_spans()` | Spanning column headers |
| `fr_styles()` | Cell/row/column conditional formatting |
| `fr_spacing()` | Gaps between structural sections |
| `fr_render()` | Render to RTF, PDF, HTML, or LaTeX |

## Key features

### Multi-format rendering

```r
fr_render(spec, "output.rtf")   # Native RTF
fr_render(spec, "output.pdf")   # Native PDF via XeLaTeX
fr_render(spec, "output.html")  # Self-contained HTML preview
fr_render(spec, "output.tex")   # LaTeX source
```

### Interactive preview

```r
# In RStudio/Positron — just print the spec to see it in the Viewer panel
spec
# In Rmd/Quarto/pkgdown — tables render inline automatically via knit_print
```

### Automatic N-counts in headers

```r
spec |> fr_cols(
  .n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135),
  .n_format = "{label}\n(N={n})"
)
```

### Page tokens

```r
spec |>
  fr_pagehead(left = "Protocol TFRM-2024-001", right = "CONFIDENTIAL") |>
  fr_pagefoot(left = "{program}", right = "Page {thepage} of {total_pages}")
```

### Inline markup

```r
# Works in titles, footnotes, and data cells
spec |> fr_titles("Table 14.1.1{fr_super('a')}", "Demographics")
spec |> fr_footnotes("{fr_super('a')} Post-hoc analysis")
```

### Four-tier defaults

```
+----------------------------------------------------------------------+
| 4. Per-table verbs         fr_page(font_size = 10)          <- wins  |
| 3. Session theme           fr_theme(font_size = 9)                   |
| 2. Project config          _arframe.yml                              |
| 1. Package defaults        inst/defaults/_arframe.yml       <- lowest|
+----------------------------------------------------------------------+
```

### Recipes: reusable pipeline fragments

```r
company_layout <- fr_recipe(
  fr_page(orientation = "landscape", font_size = 9),
  fr_pagehead(left = "TFRM-2024-001", right = "CONFIDENTIAL"),
  fr_pagefoot(left = "{program}", right = "Page {thepage} of {total_pages}"),
  fr_hlines("header")
)

tbl_demog |> fr_table() |> fr_apply(company_layout) |> fr_titles("Table 14.1.1", "Demographics")
```

### Validation and QC

```r
fr_validate(spec)                        # Pre-render checks
fr_get_titles(spec)                      # Programmatic inspection
fr_style_explain(spec, row = 1, col = "total")  # Debug style cascade
```

## Built-in datasets

| Dataset | Description |
|---|---|
| `adsl`, `adae`, `adtte`, `adcm`, `advs` | Synthetic CDISC ADaM datasets (135 subjects) |
| `tbl_demog` | Demographics summary (Table 14.1.1) |
| `tbl_ae_soc` | AE by SOC/PT (Table 14.3.1.2) |
| `tbl_ae_summary` | Overall AE summary (Table 14.3.1.1) |
| `tbl_disp` | Subject disposition (Table 14.1.3) |
| `tbl_tte` | Time-to-event summary (Table 14.2.1) |
| `tbl_cm` | Concomitant medications (Table 14.4.1) |
| `tbl_vs` | Vital signs (Table 14.3.6) |

## Architecture

arframe writes output directly — no intermediate gt or huxtable layer:

```
fr_spec → finalize_spec() → RTF backend  → .rtf file
                            ├→ LaTeX backend → .tex → XeLaTeX → .pdf
                            └→ HTML backend  → .html file
```

- **RTF**: Writes RTF 1.9.1 control words directly. Cell-level formatting, decimal alignment, `\trkeep` group protection, R-side pagination.
- **PDF**: Generates tabularray LaTeX, compiles with XeLaTeX. Falls back to Adobe open-source fonts (Source Serif 4, Source Sans 3, Source Code Pro — SIL OFL) on Linux/Docker without Microsoft fonts. Set `ARFRAME_FONT_DIR` to a directory of `.ttf`/`.otf` files for project-local fonts without system-wide installation.
- **HTML**: Self-contained document with paper simulation (orientation-aware page dimensions, margins). Auto-preview in RStudio/Positron viewer via `print()`. Inline rendering in Rmd/Quarto via `knit_print()`.
- **Font metrics**: Real Adobe Font Metrics (AFM) for 12 font variants — accurate column width estimation without rendering.

## Documentation

- **[Get Started](https://vthanik.github.io/arframe/articles/arframe.html)** — Your first table to production-ready output
- **[Columns & Headers](https://vthanik.github.io/arframe/articles/columns-and-headers.html)** — Widths, alignment, N-counts, decimal alignment
- **[Titles & Footnotes](https://vthanik.github.io/arframe/articles/titles-and-footnotes.html)** — Titles, footnotes, inline markup
- **[Rows & Page Layout](https://vthanik.github.io/arframe/articles/rows-and-pages.html)** — Grouping, pagination, page chrome
- **[Rules, Spans & Styles](https://vthanik.github.io/arframe/articles/styling.html)** — Borders, spanning headers, conditional formatting
- **[Table Cookbook](https://vthanik.github.io/arframe/articles/table-cookbook.html)** — 8 complete ICH E3 table programs
- **[Listings & Figures](https://vthanik.github.io/arframe/articles/listings-and-figures.html)** — Patient listings and embedded figures
- **[Automation & Batch](https://vthanik.github.io/arframe/articles/automation.html)** — Themes, recipes, YAML config, batch rendering
- **[Architecture](https://vthanik.github.io/arframe/articles/architecture.html)** — Internal design and backend interface

## Related packages

arframe sits at the **output layer** of the pharmaverse — it receives whatever the analysis layer produces and renders it to files. It does not replace upstream packages; it completes them.

| Package | Role | Relationship to arframe |
|---|---|---|
| [admiral](https://pharmaverse.github.io/admiral/) | ADaM dataset creation | Upstream — arframe renders admiral outputs |
| [Tplyr](https://atorus-research.github.io/Tplyr/) / [rtables](https://insightsengineering.github.io/rtables/) | Table summarisation | Upstream — arframe renders their wide-format output |
| [cards / cardx](https://insightsengineering.github.io/cards/) | ARD creation | Upstream — `fr_wide_ard()` converts ARD to arframe input |
| [gtsummary](https://www.danieldsjoberg.com/gtsummary/) | High-level summaries | Upstream — arframe renders the same data gtsummary summarises |
| [tfrmt](https://gsk-biostatistics.github.io/tfrmt/) | ARD display spec + mock shells | Complementary — tfrmt for SAP-phase shell review; arframe for production output |
| [r2rtf](https://merck.github.io/r2rtf/) | RTF-only rendering | Parallel — r2rtf is RTF-specialist; arframe adds PDF + HTML from the same spec |
| [docorator](https://gsk-biostatistics.github.io/docorator/) | gt-based PDF decoration | Parallel — docorator wraps gt objects; arframe is a native engine without a gt layer |

## License

MIT
