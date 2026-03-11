
# tlframe <img src="man/figures/logo.png" align="right" height="139" alt="" />

> Regulatory-grade clinical tables, listings, and figures in RTF and PDF from a single specification.

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/vthanik/tlframe)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**tlframe** is an R package for pharmaceutical submissions. You describe the output once; tlframe renders both RTF and PDF — change the file extension, the output adapts.

```r
tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1", "Demographics and Baseline Characteristics") |>
  fr_cols(label = c(stat = "Statistic", placebo = "Placebo", low = "Low Dose",
                    high = "High Dose", total = "Total")) |>
  fr_header(bold = TRUE, bg = "#003366", fg = "white") |>
  fr_pagehead(left = "Protocol TFRM-2024-001", right = "CONFIDENTIAL") |>
  fr_pagefoot(left = "{program}", right = "Page {thepage} of {total_pages}") |>
  fr_footnotes("Percentages based on N per treatment arm.") |>
  fr_hlines("header") |>
  fr_render("Table_14_1_1.rtf")  # Change to .pdf — same output, different format
```

## Why tlframe?

The pharmaverse has excellent tools for data derivation (admiral), analysis (Tplyr, rtables, tern), and table decoration (docorator, r2rtf). **tlframe occupies a different position**: it is a native rendering engine that writes RTF control words directly (from the RTF 1.9.1 spec) and generates LaTeX/tabularray natively, so both formats are first-class outputs.

| Capability | tlframe |
|---|---|
| Native RTF rendering | Direct RTF 1.9.1 — no intermediate HTML or gt layer |
| Native PDF rendering | Direct tabularray/XeLaTeX — no RMarkdown/Quarto pipeline |
| Single spec, both formats | Same `fr_spec` object renders to `.rtf` or `.pdf` |
| Group-aware pagination | Keeps groups together, repeats headers, adds continuation text |
| Decimal alignment | 10-type stat display engine (n/%, mean (SD), CI, p-values, ranges) |
| Page headers/footers | 3-slot layout with tokens: `{thepage}`, `{total_pages}`, `{program}` |
| Inline markup | `fr_super()`, `fr_bold()`, `fr_italic()`, `fr_dagger()` in any cell |
| Dependencies | 7 imports — no dplyr, tidyr, purrr, or gt |

## Installation

```r
# Install from GitHub
# install.packages("pak")
pak::pak("vthanik/tlframe")
```

## Quick start

tlframe ships with synthetic CDISC ADaM datasets so every example runs out of the box.

### A table in 3 lines

```r
library(tlframe)

tbl_demog |> fr_table() |> fr_render(tempfile(fileext = ".rtf"))
```

### A submission-ready demographics table

```r
spec <- tbl_demog |>
  fr_table(group_by = "group") |>
  fr_titles("Table 14.1.1", "Demographics and Baseline Characteristics",
            "All Randomized Subjects") |>
  fr_cols(label = c(stat = "Statistic", placebo = "Placebo (N=45)",
                    low = "Low Dose (N=45)", high = "High Dose (N=45)",
                    total = "Total (N=135)"),
          align = c(stat = "left"),
          width = c(stat = 2.0)) |>
  fr_header(bold = TRUE) |>
  fr_hlines("header") |>
  fr_rows(indent_by = c("stat" = 0.25)) |>
  fr_pagehead(left = "Protocol TFRM-2024-001",
              center = "CONFIDENTIAL",
              right = "Study TFRM-2024-001") |>
  fr_pagefoot(left = "{program}",
              right = "Page {thepage} of {total_pages}") |>
  fr_footnotes("Percentages based on N per treatment arm.",
               "Continuous variables summarized as Mean (SD).")

# Render both formats from the same spec
fr_render(spec, tempfile(fileext = ".rtf"))
fr_render(spec, tempfile(fileext = ".pdf"))
```

### An AE table with SOC/PT grouping

```r
tbl_ae_soc |>
  fr_table(group_by = "soc", page_by = "soc") |>
  fr_titles("Table 14.3.1",
            "Adverse Events by System Organ Class and Preferred Term") |>
  fr_rows(indent_by = c("pt" = 0.25), blank_after = "soc") |>
  fr_cols(.n = c("Placebo" = 45, "Low Dose" = 45, "High Dose" = 45),
          .n_format = "{label}\n(N={n})") |>
  fr_header(bold = TRUE) |>
  fr_hlines("header") |>
  fr_render(tempfile(fileext = ".rtf"))
```

## Pipeline verbs

Every verb takes an `fr_spec`, returns a modified `fr_spec`. Side effects only at `fr_render()`.

| Verb | Purpose |
|---|---|
| `fr_table()` | Entry point — creates spec from data |
| `fr_cols()` | Column labels, widths, alignment, selection |
| `fr_header()` | Header styling, alignment |
| `fr_titles()` | Table titles (1-4 lines) |
| `fr_footnotes()` | Footnotes with optional separator |
| `fr_rows()` | Row grouping, indentation, blank rows |
| `fr_page()` | Paper size, orientation, margins, font |
| `fr_pagehead()` | 3-slot page header with tokens |
| `fr_pagefoot()` | 3-slot page footer with tokens |
| `fr_hlines()` / `fr_vlines()` | Horizontal/vertical rules |
| `fr_spans()` | Spanning column headers |
| `fr_style()` | Cell-level conditional formatting |
| `fr_spacing()` | Gaps between titles, headers, footnotes |
| `fr_render()` | Render to RTF, PDF, or LaTeX |

## Key features

### Dual-format rendering

```r
fr_render(spec, "output.rtf")  # Native RTF
fr_render(spec, "output.pdf")  # Native PDF via XeLaTeX
fr_render(spec, "output.tex")  # LaTeX source
```

### Automatic N-counts in headers

```r
spec |> fr_cols(.n = c("Placebo" = 45, "Low Dose" = 45, "High Dose" = 45),
               .n_format = "{label}\n(N={n})")
```

### Page tokens

```r
spec |> fr_pagehead(left = "Protocol XYZ", right = "CONFIDENTIAL") |>
        fr_pagefoot(left = "{program}", right = "Page {thepage} of {total_pages}")
```

### Inline markup

```r
# Works in titles, footnotes, and data cells
spec |> fr_titles("Table 14.1.1{fr_super('a')}", "Demographics")
spec |> fr_footnotes("{fr_super('a')} Post-hoc analysis")
```

### YAML configuration

```yaml
# _tlframe.yml — company-wide defaults
page:
  orientation: landscape
  font_family: Courier
  font_size: 9
header:
  bold: true
hlines: header
```

```r
fr_config("_tlframe.yml")  # Apply to all tables in session
```

## Built-in datasets

| Dataset | Description |
|---|---|
| `adsl`, `adae`, `adtte`, `adcm`, `advs` | Synthetic CDISC ADaM datasets (135 subjects) |
| `tbl_demog` | Demographics summary (ICH E3 Table 14.1.1) |
| `tbl_ae_soc` | AE by SOC/PT (ICH E3 Table 14.3.1) |
| `tbl_disp` | Subject disposition (ICH E3 Table 14.1.4) |
| `tbl_tte` | Time-to-event summary (ICH E3 Table 14.2.1) |
| `tbl_cm` | Concomitant medications (ICH E3 Table 14.4.1) |
| `tbl_vs` | Vital signs (ICH E3 Table 14.3.6) |

## Architecture

tlframe writes output directly — no intermediate HTML, gt, or huxtable layer:

```
fr_spec ──► finalize_spec() ──► RTF backend ──► .rtf file
                               └► LaTeX backend ──► .tex ──► XeLaTeX ──► .pdf
```

- **RTF**: Writes RTF 1.9.1 control words directly. Cell-level formatting, decimal alignment tabs, `\trkeep` group protection.
- **PDF**: Generates tabularray LaTeX, compiles with XeLaTeX. Falls back to Latin Modern fonts (built into tinytex) on Linux/Docker without Microsoft fonts.
- **Font metrics**: Real Adobe Font Metrics (AFM) for 12 font variants — accurate column width estimation without rendering.

## Related packages

tlframe is designed to complement the pharmaverse ecosystem:

- **[admiral](https://pharmaverse.github.io/admiral/)** — ADaM dataset creation
- **[Tplyr](https://atorus-research.github.io/Tplyr/)** / **[rtables](https://insightsengineering.github.io/rtables/)** — Analysis table summarization (upstream of tlframe)
- **[r2rtf](https://merck.github.io/r2rtf/)** — RTF-focused table rendering
- **[docorator](https://gsk-biostatistics.github.io/docorator/)** — gt-based document decoration

## License

MIT
