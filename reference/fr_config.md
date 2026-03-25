# Load Configuration from a YAML File

Loads study-level configuration from a `_arframe.yml` file. If no file
path is given, auto-discovers the nearest `_arframe.yml` by searching up
the directory tree from the current working directory.

The loaded config is stored internally and automatically applied to
every subsequent
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
call. Config values are the **lowest priority** — they are overridden by
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
(session-level) and per-table verbs like
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md) or
[`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md).

**arframe is the first pharma TFL package with file-driven theming.**
Place a single `_arframe.yml` at your project root and every table in
the study inherits the same fonts, margins, headers, footers, and rules
without any R code.

## Usage

``` r
fr_config(file = NULL)
```

## Arguments

- file:

  Character scalar or `NULL`. Path to a `_arframe.yml` file. `NULL`
  (default) triggers auto-discovery: searches up the directory tree from
  [`getwd()`](https://rdrr.io/r/base/getwd.html) until a `_arframe.yml`
  is found, falling back to the package-bundled defaults
  (`inst/defaults/_arframe.yml`).

## Value

Invisibly returns the parsed config as a named list.

## Four-tier precedence

Settings are resolved from lowest to highest priority:

    Package defaults  < _arframe.yml  < fr_theme()  < per-table verbs

Config from `_arframe.yml` overrides package defaults.
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
overrides config. Per-table verbs (e.g. `fr_page(font_size = 10)`)
override everything.

## YAML file structure

    # _arframe.yml — place at project root
    page:
      paper: letter
      orientation: landscape
      margins: [1.0, 0.75, 1.0, 0.75]   # top, right, bottom, left
      font_family: "Times New Roman"
      font_size: 9
      col_gap: 4

    columns:
      split: false           # column splitting for wide tables
      space_mode: indent     # indent | preserve — leading space handling

    header:
      align: ~               # inherit from column (user decides)
      valign: bottom
      bold: false

    pagehead:
      left: "{company}"
      center: "{study_id}"
      right: "Page {thepage} of {total_pages}"
      font_size: 8

    pagefoot:
      left: "{program}"
      right: "{datetime}"
      font_size: 8

    rules:
      hlines: header         # header | booktabs | open | hsides | box | void
      vlines: void           # box | all | inner | void

    footnotes:
      separator: true
      font_size: 8

    spacing:                 # blank lines between table sections
      titles_after: 1        # after titles, before column header
      footnotes_before: 1    # after body, before footnotes
      pagehead_after: 0      # after page header, before titles
      pagefoot_before: 0     # after footnotes, before page footer
      page_by_after: 1       # after page_by label, before col headers

    tokens:                  # custom token values for pagehead/pagefoot
      company: "Pharma Corp"
      study_id: "TFRM-2024-001"

## Auto-discovery

When `file = NULL`, `fr_config()` searches for `_arframe.yml` starting
from the current working directory and walking up parent directories.
This mirrors the behaviour of `.Rprofile`, `_quarto.yml`, and
`_pkgdown.yml`. A typical project layout:

    study-root/
      _arframe.yml         <-- found here
      R/
        t_demog.R
      output/
        tbl_14_1_1.rtf

## See also

[`fr_config_get()`](https://vthanik.github.io/arframe/reference/fr_config_get.md)
to inspect,
[`fr_config_reset()`](https://vthanik.github.io/arframe/reference/fr_config_reset.md)
to clear,
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
for session-level overrides,
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)
for per-table page layout.

## Examples

``` r
# Load the built-in package defaults config
default_cfg <- system.file("defaults/_arframe.yml", package = "arframe")
fr_config(default_cfg)

# Inspect what was loaded
fr_config_get()
#> $page
#> $page$paper
#> [1] "letter"
#> 
#> $page$orientation
#> [1] "landscape"
#> 
#> $page$margins
#> [1] 1.00 0.75 1.00 0.75
#> 
#> $page$font_family
#> [1] "Times New Roman"
#> 
#> $page$font_size
#> [1] 9
#> 
#> $page$continuation
#> NULL
#> 
#> $page$col_gap
#> [1] 4
#> 
#> 
#> $columns
#> $columns$split
#> [1] FALSE
#> 
#> $columns$space_mode
#> [1] "indent"
#> 
#> $columns$n_format
#> [1] "{label}\n(N={n})"
#> 
#> 
#> $header
#> $header$align
#> NULL
#> 
#> $header$valign
#> [1] "bottom"
#> 
#> $header$bold
#> [1] FALSE
#> 
#> $header$span_gap
#> [1] TRUE
#> 
#> 
#> $pagehead
#> $pagehead$left
#> NULL
#> 
#> $pagehead$center
#> NULL
#> 
#> $pagehead$right
#> NULL
#> 
#> $pagehead$font_size
#> NULL
#> 
#> $pagehead$bold
#> [1] FALSE
#> 
#> 
#> $pagefoot
#> $pagefoot$left
#> [1] "{program}"
#> 
#> $pagefoot$center
#> NULL
#> 
#> $pagefoot$right
#> [1] "{datetime}"
#> 
#> $pagefoot$font_size
#> NULL
#> 
#> $pagefoot$bold
#> [1] FALSE
#> 
#> 
#> $rules
#> $rules$hlines
#> [1] "header"
#> 
#> $rules$vlines
#> [1] "void"
#> 
#> 
#> $footnotes
#> $footnotes$separator
#> [1] FALSE
#> 
#> $footnotes$font_size
#> NULL
#> 
#> $footnotes$placement
#> [1] "every"
#> 
#> 
#> $spacing
#> $spacing$titles_after
#> [1] 1
#> 
#> $spacing$footnotes_before
#> [1] 1
#> 
#> $spacing$pagehead_after
#> [1] 0
#> 
#> $spacing$pagefoot_before
#> [1] 0
#> 
#> $spacing$page_by_after
#> [1] 1
#> 
#> 
#> $titles
#> $titles$align
#> [1] "center"
#> 
#> $titles$bold
#> [1] FALSE
#> 
#> $titles$font_size
#> NULL
#> 
#> 
#> $tokens
#> named list()
#> 

# Create a temporary custom config and load it
yml <- file.path(tempdir(), "_arframe.yml")
writeLines(c(
  "page:",
  "  orientation: landscape",
  "  font_size: 8",
  "  font_family: 'Times New Roman'",
  "header:",
  "  bold: true",
  "tokens:",
  "  company: 'Pharma Corp'"
), yml)
fr_config(yml)
fr_config_get()$page$font_size    # 8
#> [1] 8
fr_config_get()$tokens$company    # "Pharma Corp"
#> [1] "Pharma Corp"

# Clean up
fr_config_reset()
unlink(yml)
```
