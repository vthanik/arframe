# Automation & Batch

*“I can make one table. How do I make 50?”*

## Four-tier defaults

arframe resolves settings from four tiers, lowest to highest priority:

    +----------------------------------------------------------------------+
    | 4. Per-table verbs         fr_page(font_size = 10)          <- wins  |
    | 3. Session theme           fr_theme(font_size = 9)                   |
    | 2. Project config          _arframe.yml                              |
    | 1. Package defaults        inst/defaults/_arframe.yml       <- lowest|
    +----------------------------------------------------------------------+

| Tier | Scope | Set by | Survives restart |
|----|----|----|----|
| Package defaults | All users | Package author | Always |
| `_arframe.yml` | Project/study | Lead programmer | Yes (file) |
| [`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md) | R session | Programmer | No |
| Per-table verbs | One table | Programmer | No |

> **SAS:** Tier 2 = `autoexec.sas` + `PROC TEMPLATE`. Tier 3 =
> `OPTIONS`. Tier 4 = inline `STYLE()=`.

## YAML configuration

Place `_arframe.yml` at your project root. Every table inherits these
settings:

``` yaml
page:
  paper: letter
  orientation: landscape
  margins: [1.0, 0.75, 1.0, 0.75]
  font_family: "Times New Roman"
  font_size: 9
  continuation: "(continued)"

header:
  bold: false
  valign: bottom
  n_format: "{label}\n(N={n})"

pagehead:
  left: "Protocol TFRM-2024-001"
  right: "CONFIDENTIAL"

pagefoot:
  left: "{program}"
  right: "Page {thepage} of {total_pages}"

rules:
  hlines: header

footnotes:
  separator: false

spacing:
  titles_after: 1
  footnotes_before: 1

tokens:
  study: "TFRM-2024-001"
  cutoff: "15MAR2025"
```

``` r
fr_config()
cfg <- fr_config_get()
cfg$page$font_size
#> [1] 9
fr_config_reset()
```

Custom tokens defined in YAML resolve in
[`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md)
and
[`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md).

## Session theme

[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
sets session-level defaults. Call it once at the top of your program:

``` r
fr_theme(
  font_size   = 9,
  font_family = "Times New Roman",
  orientation = "landscape",
  hlines      = "header",
  pagehead    = list(left = "TFRM-2024-001", right = "CONFIDENTIAL"),
  pagefoot    = list(left = "{program}",
                     right = "Page {thepage} of {total_pages}")
)
```

Every table now inherits the theme:

``` r
spec1 <- tbl_demog |> fr_table() |> fr_titles("Table 14.1.1", "Demographics")
spec2 <- tbl_disp |> fr_table() |> fr_titles("Table 14.1.4", "Disposition")
fr_get_page(spec1)$font_size
#> [1] 9
fr_get_page(spec2)$font_size
#> [1] 9
```

Override per-table:

``` r
spec3 <- tbl_ae_soc |>
  fr_table() |>
  fr_page(orientation = "portrait")
fr_get_page(spec3)$orientation
#> [1] "portrait"
fr_get_page(spec3)$font_size
#> [1] 9
```

``` r
fr_theme_reset()
```

## Recipes

Recipes capture verb chains as portable, composable objects:

``` r
company_layout <- fr_recipe(
  fr_page(orientation = "landscape", font_size = 9),
  fr_pagehead(left = "TFRM-2024-001", right = "CONFIDENTIAL"),
  fr_pagefoot(left = "{program}",
              right = "Page {thepage} of {total_pages}"),
  fr_hlines("header")
)
company_layout
#> 
#> ── fr_recipe (4 verbs)
#> 1. fr_page(orientation="landscape", font_size=9)
#> 2. fr_pagehead(left="TFRM-2024-001", right="CONFIDENTIAL")
#> 3. fr_pagefoot(left="{program}", right="Page {thepage} of {tot...)
#> 4. fr_hlines("header")
```

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_apply(company_layout) |>
  fr_titles("Table 14.1.1", "Demographics")
fr_get_page(spec)$font_size
#> [1] 9
```

### Composing recipes

[`c()`](https://rdrr.io/r/base/c.html) merges recipes. Later recipes
override for replacing verbs; appending verbs (`fr_spans`, `fr_styles`)
accumulate:

``` r
ae_style <- fr_recipe(
  fr_header(bold = TRUE, align = "center"),
  fr_page(continuation = "(continued)")
)
ae_combined <- c(company_layout, ae_style)
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_apply(ae_combined) |>
  fr_titles("Table 14.3.1", "AE by SOC/PT")
fr_get_page(spec)$continuation
#> [1] "(continued)"
```

### Three-layer recipe pattern

``` r
# ── Population N-counts (define once) ──
n_itt <- c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)

# Layer 1: Company standard
company <- fr_recipe(
  fr_page(orientation = "landscape", font_family = "Times New Roman", font_size = 9),
  fr_pagehead(right = "CONFIDENTIAL"),
  fr_pagefoot(left = "{program}", right = "Page {thepage} of {total_pages}"),
  fr_hlines("header"),
  fr_header(bold = TRUE, align = "center")
)

# Layer 2: Study-specific
study <- fr_recipe(
  fr_pagehead(left = "Protocol TFRM-2024-001"),
  fr_page(tokens = list(study = "TFRM-2024-001", cutoff = "15MAR2025"))
)

# Layer 3: Table-type (only what varies by table type)
demog_recipe <- fr_recipe(
  fr_cols(.n = n_itt, .n_format = "{label}\n(N={n})"),
  fr_footnotes("Percentages based on N per treatment arm.")
)

# Usage: only titles and columns are unique per table
tbl_demog |>
  fr_table() |>
  fr_apply(c(company, study, demog_recipe)) |>
  fr_titles("Table 14.1.1", "Demographics") |>
  fr_cols(group = fr_col(visible = FALSE)) |>
  fr_render("output/Table_14_1_1.rtf")
```

## Batch rendering

Combine a metadata table with recipes and a loop:

``` r
metadata <- data.frame(
  table_id  = c("14.1.1", "14.1.4", "14.3.1", "14.2.1"),
  title1    = c("Demographics", "Subject Disposition",
                "AE by SOC/PT", "Time to Study Withdrawal"),
  title2    = c("ITT Population", "All Randomized",
                "Safety Population", "ITT Population"),
  data_name = c("tbl_demog", "tbl_disp", "tbl_ae_soc", "tbl_tte"),
  recipe    = c("demog", "demog", "ae", "tte"),
  stringsAsFactors = FALSE
)
metadata
#>   table_id                   title1            title2  data_name recipe
#> 1   14.1.1             Demographics    ITT Population  tbl_demog  demog
#> 2   14.1.4      Subject Disposition    All Randomized   tbl_disp  demog
#> 3   14.3.1             AE by SOC/PT Safety Population tbl_ae_soc     ae
#> 4   14.2.1 Time to Study Withdrawal    ITT Population    tbl_tte    tte
```

``` r
for (i in seq_len(nrow(metadata))) {
  row <- metadata[i, ]
  data <- get(row$data_name)
  recipe <- recipes[[row$recipe]]

  data |>
    fr_table() |>
    fr_apply(c(company, study, recipe)) |>
    fr_titles(paste("Table", row$table_id), row$title1, row$title2) |>
    fr_render(sprintf("output/Table_%s.rtf", gsub("\\.", "_", row$table_id)))
}
```

> **SAS:** This replaces the macro library + `%include` pattern. The
> metadata CSV is your macro call dataset, recipes are your macros, and
> the loop is your `%DO` block.

## RTF vs PDF

| Feature           | RTF                | PDF                     |
|-------------------|--------------------|-------------------------|
| Engine            | Direct RTF 1.9.1   | tabularray + XeLaTeX    |
| Footnote `"last"` | Repeats every page | Works (lastfoot)        |
| Page numbers      | `\chpgn` fields    | `\thepage` / `\pageref` |
| Decimal alignment | `\ql\li<offset>`   | `\hspace{pt}` + `~`     |
| Fonts             | Referenced by name | Embedded via fontspec   |
| Group pagination  | R-side pagination  | `longtblr`              |

**Linux font fallback:** On systems without Microsoft fonts, PDF
rendering automatically falls back to Adobe open-source fonts — Source
Serif 4, Source Sans 3, and Source Code Pro (SIL Open Font License). No
extra font installation needed.

**Custom font directory:** Set the `ARFRAME_FONT_DIR` environment
variable to a directory containing `.ttf` or `.otf` files. arframe
passes this to XeLaTeX via `OSFONTDIR`, making fonts discoverable by
family name — no system-wide installation required. This is ideal for
Docker containers, CI pipelines, or project-local font directories:

``` r
# Shell: export ARFRAME_FONT_DIR=/path/to/project/fonts
# R:
Sys.setenv(ARFRAME_FONT_DIR = "/path/to/project/fonts")
spec |> fr_page(font_family = "Courier New") |> fr_render("output.pdf")
```

``` r
install.packages("tinytex")
tinytex::install_tinytex()
```

## Validation and QC

### Pre-render validation

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1", "Demographics") |>
  fr_cols(group = fr_col(visible = FALSE))
fr_validate(spec)
```

[`fr_validate()`](https://vthanik.github.io/arframe/reference/fr_validate.md)
checks columns, widths, styles, spans, fonts, and row references. Use
`strict = TRUE` in CI pipelines.

### Accessor-based QC

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1", "Demographics") |>
  fr_footnotes("Source: ADSL.") |>
  fr_cols(group = fr_col(visible = FALSE))

stopifnot(length(fr_get_titles(spec)) == 2)
stopifnot(grepl("ADSL", fr_get_footnotes(spec)[[1]]$content))
stopifnot(fr_get_page(spec)$orientation == "landscape")
```

### Double-programming workflow

``` r
# Programmer 1
spec1 <- tbl_demog |> fr_table() |>
  fr_titles("Table 14.1.1", "Demographics") |>
  fr_render("output/t_14_1_1_prod.rtf")

# Programmer 2 (QC)
spec2 <- tbl_demog |> fr_table() |>
  fr_titles("Table 14.1.1", "Demographics") |>
  fr_render("output/t_14_1_1_qc.rtf")

# Same data + same verbs = identical output
```

## CI/CD and Docker

``` r
# render_all.R
library(arframe)
fr_config("_arframe.yml")
metadata <- read.csv("metadata.csv", stringsAsFactors = FALSE)
for (i in seq_len(nrow(metadata))) {
  row <- metadata[i, ]
  data <- get(row$data_name)
  recipe <- readRDS(sprintf("recipes/%s.rds", row$recipe))
  data |>
    fr_table() |>
    fr_apply(recipe) |>
    fr_titles(paste("Table", row$table_id), row$title1) |>
    fr_render(sprintf("output/Table_%s.rtf", gsub("\\.", "_", row$table_id)))
}
```

``` dockerfile
FROM rocker/r-ver:4.3.3
RUN apt-get update && apt-get install -y libxml2-dev libcurl4-openssl-dev
RUN R -e "install.packages(c('cli','glue','rlang','tidyselect','vctrs','yaml','stringi'))"
RUN R -e "install.packages('tinytex'); tinytex::install_tinytex()"
# Adobe open-source fonts serve as fallback --- no Microsoft fonts needed

# Optional: bundle project-specific fonts (e.g., Times New Roman)
COPY fonts/ /opt/fonts/
ENV ARFRAME_FONT_DIR=/opt/fonts
```
