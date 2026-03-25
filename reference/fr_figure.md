# Start a arframe Figure Pipeline

Entry point for figures — wraps a plot object (ggplot2 or base R) with
the same titling, footnoting, and page chrome as tables. The plot is
embedded in the rendered output with consistent regulatory formatting.

## Usage

``` r
fr_figure(plot, width = NULL, height = NULL, meta = NULL)
```

## Arguments

- plot:

  A plot object or a list of plot objects. Supported types:

  - `ggplot` object (from ggplot2)

  - `recordedplot` object (from
    [`recordPlot()`](https://rdrr.io/r/grDevices/recordplot.html))

  - A `list` of the above for multi-page figures (one plot per page)

- width:

  Numeric. Plot width in inches within the page. Default `NULL` uses the
  full printable width.

- height:

  Numeric. Plot height in inches within the page. Default `NULL` uses
  the remaining height after titles and footnotes.

- meta:

  A data frame of per-page metadata for multi-page figures. Must have
  exactly one row per plot. Column names become tokens available in
  [`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md)
  and
  [`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md)
  via `{column_name}` syntax, resolved dynamically for each page.
  Ignored for single-plot figures.

## Value

An `fr_spec` object with `type = "figure"`.

## Supported verbs

Figures support:
[`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md),
[`fr_footnotes()`](https://vthanik.github.io/arframe/reference/fr_footnotes.md),
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md),
[`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md),
[`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md),
[`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md),
[`fr_render()`](https://vthanik.github.io/arframe/reference/fr_render.md).

Column/header/row/style/span/rule verbs are no-ops on figure specs.

## Render strategy

- **PDF**: Plot saved as temporary PDF, included via `\\includegraphics`
  in LaTeX with titles/footnotes as DeclareTblrTemplate.

- **RTF**: Plot saved as temporary PNG, embedded via `\\pict` RTF
  picture group with titles as paragraphs.

## Multi-page figures

Pass a list of plots and an optional `meta` data frame to create
multi-page figures. Each meta column becomes a token in titles and
footnotes, resolved per page:

    plots <- list(p_adults, p_pediatrics, p_geriatrics)
    page_meta <- data.frame(subgroup = c("Adults", "Pediatrics", "Geriatrics"))

    plots |>
      fr_figure(meta = page_meta) |>
      fr_titles("Figure 14.1.1 Kaplan-Meier Curve", "Subgroup: {subgroup}") |>
      fr_render("km_multi.rtf")

## See also

[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
for summary tables,
[`fr_listing()`](https://vthanik.github.io/arframe/reference/fr_listing.md)
for listings.

## Examples

``` r
# fr_figure() requires a ggplot or recordedplot object
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(adtte, ggplot2::aes(x = AVAL, y = TRTA)) +
    ggplot2::geom_point()

  spec <- p |>
    fr_figure() |>
    fr_titles("Figure 14.2.1 Time-to-Event") |>
    fr_footnotes("Source: ADTTE") |>
    fr_page(orientation = "landscape")
  spec
}
#> 
#> ── fr_spec: Figure 
#> Plot: 1 page
#> Page: landscape letter, 9pt Times New Roman
#> Titles (1):
#> 1. [center] "Figure 14.2.1 Time-to-Event"
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "Source: ADTTE"
#> Plot: ggplot2::ggplot

## ── Explicit width and height ─────────────────────────────────────────────
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(adsl, ggplot2::aes(x = AGE, fill = TRT01A)) +
    ggplot2::geom_histogram(binwidth = 5, position = "dodge") +
    ggplot2::labs(x = "Age (years)", y = "Count", fill = "Treatment")

  spec <- p |>
    fr_figure(width = 7, height = 4.5) |>
    fr_titles(
      "Figure 14.1.2 Distribution of Age by Treatment Arm",
      "Full Analysis Set"
    ) |>
    fr_footnotes("Source: ADSL") |>
    fr_page(orientation = "landscape")
  spec
}
#> 
#> ── fr_spec: Figure 
#> Plot: 1 page
#> Page: landscape letter, 9pt Times New Roman
#> Titles (2):
#> 1. [center] "Figure 14.1.2 Distribution of Age by Treatment Arm"
#> 2. [center] "Full Analysis Set"
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "Source: ADSL"
#> Plot: ggplot2::ggplot

## ── Base R plot with recordPlot() ─────────────────────────────────────────
# Capture a base R plot as a recordedplot object.
# Use a temporary PDF device to avoid creating Rplots.pdf.
tmp_pdf <- tempfile(fileext = ".pdf")
pdf(tmp_pdf)
old_par <- par(mar = c(5, 4, 2, 1))
plot(adsl$AGE, adsl$BMIBL,
     xlab = "Age (years)", ylab = "BMI (kg/m2)",
     pch = 19, col = "#4472C4", cex = 0.7,
     main = "")
p_base <- recordPlot()
par(old_par)
dev.off()
#> agg_record_2eadf476ebf79 
#>                        2 
unlink(tmp_pdf)

spec <- p_base |>
  fr_figure(width = 6, height = 4) |>
  fr_titles("Figure 14.1.3 Age vs BMI at Baseline") |>
  fr_footnotes("Source: ADSL")
spec
#> 
#> ── fr_spec: Figure 
#> Plot: 1 page
#> Page: landscape letter, 9pt Times New Roman
#> Titles (1):
#> 1. [center] "Figure 14.1.3 Age vs BMI at Baseline"
#> Header: valign=bottom
#> Footnotes (1):
#> 1. [left] "Source: ADSL"
#> Plot: recordedplot
```
