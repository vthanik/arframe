# Render a Table to File

The terminal verb in every **arframe** pipeline. Takes a fully
configured `fr_spec` object and writes the rendered table to a file.
Supports RTF, LaTeX, and PDF output.

Row pagination within the RTF output is delegated to the
Word/LibreOffice rendering engine via native RTF properties (`\trhdr`
for repeating headers). The LaTeX backend uses tabularray's `longtblr`
environment with `rowhead` for repeating headers. R handles `page_by`
group splitting (section/page breaks between groups) and `col_split`
(column panel calculation for wide tables).

## Usage

``` r
fr_render(spec, path, format = NULL, ...)
```

## Arguments

- spec:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md),
  configured with `fr_*()` pipeline verbs.

- path:

  Character scalar. Output file path. The file extension determines the
  format unless `format` is specified:

  - `.rtf` — Rich Text Format.

  - `.html` / `.htm` — Self-contained HTML preview.

  - `.tex` — LaTeX source (tabularray/XeLaTeX).

  - `.pdf` — PDF via XeLaTeX compilation (requires XeLaTeX on PATH or
    **tinytex**). First-time setup: call
    [`fr_install_latex_deps()`](https://vthanik.github.io/arframe/reference/fr_install_latex_deps.md)
    to install required LaTeX packages (notably **tabularray**). For
    custom fonts without system-wide installation, set the
    `ARFRAME_FONT_DIR` environment variable (see
    [`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)).

- format:

  Character scalar or `NULL`. Output format: `"rtf"`, `"html"`,
  `"latex"`, or `"pdf"`. If `NULL` (default), detected from the file
  extension of `path`.

- ...:

  Reserved for future backend-specific options.

## Value

Invisibly returns `path` (the output file path).

## Page-by groups

When `fr_rows(page_by = "col_name")` is set, the data is split by the
unique values of that column. Each group is rendered as a separate
section with its own table, separated by `\sect` (section break) in RTF.
The page-by column value is printed as a bold label above each group's
table.

## Column splitting

When `fr_cols(.split = TRUE)` is set and the total column width exceeds
the printable page area, columns are automatically split into panels.
Stub columns (set via `fr_col(stub = TRUE)`) are repeated in every
panel. Each panel is rendered as a separate section.

## See also

[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
to start a pipeline,
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)
for page layout,
[`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
for pagination control.

## Examples

``` r
# Render to a temporary RTF file
out <- file.path(tempdir(), "demog.rtf")
tbl_demog |>
  fr_table() |>
  fr_render(out)
unlink(out)

# Full pipeline (in study programs, hlines/pagehead/pagefoot go in fr_theme())
out <- file.path(tempdir(), "demog_full.rtf")
tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1", "Summary of Demographics") |>
  fr_footnotes("Source: ADSL", "Program: t_demog.R") |>
  fr_cols(.width = "auto") |>
  fr_hlines("header") |>
  fr_page(orientation = "landscape") |>
  fr_pagehead(left = "{program}", right = "{datetime}") |>
  fr_pagefoot(center = "Page {thepage} of {total_pages}") |>
  fr_render(out)
unlink(out)

# Multiple format rendering in sequence (RTF then LaTeX source)
rtf_out <- file.path(tempdir(), "ae_soc.rtf")
tex_out <- file.path(tempdir(), "ae_soc.tex")
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_titles("Table 14.3.1", "Adverse Events by SOC") |>
  fr_hlines("header") |>
  fr_page(orientation = "landscape")
fr_render(spec, rtf_out)
fr_render(spec, tex_out)
unlink(c(rtf_out, tex_out))

# Listing rendering — individual AE records
out <- file.path(tempdir(), "ae_listing.rtf")
adae |>
  fr_listing() |>
  fr_cols(
    USUBJID = fr_col("Subject ID", width = 1.2),
    AEDECOD = fr_col("Preferred Term"),
    AESEV   = fr_col("Severity", width = 1.0)
  ) |>
  fr_rows(sort_by = c("USUBJID", "ASTDT"),
          suppress = "USUBJID") |>
  fr_titles("Listing 16.2.7", "Adverse Events") |>
  fr_footnotes("Source: ADAE") |>
  fr_render(out)
unlink(out)

# Figure rendering (requires ggplot2)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(adsl, ggplot2::aes(x = AGE)) +
    ggplot2::geom_histogram(binwidth = 5) +
    ggplot2::labs(title = NULL, x = "Age (years)", y = "Count")
  out <- file.path(tempdir(), "age_dist.rtf")
  p |>
    fr_figure() |>
    fr_titles("Figure 14.1.1", "Distribution of Age") |>
    fr_footnotes("Source: ADSL") |>
    fr_page(orientation = "landscape") |>
    fr_render(out)
  unlink(out)
}
```
