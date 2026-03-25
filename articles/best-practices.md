# Best Practices & Pharmaverse Integration

This guide covers how to use arframe at its fullest — from project setup
to batch rendering to pharmaverse integration with the cards package.

## Project setup with `_arframe.yml`

Place `_arframe.yml` at your project root. Every table program in the
directory (and subdirectories) inherits these settings automatically —
no
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
call needed:

``` yaml
# _arframe.yml
page:
  paper: letter
  orientation: landscape
  margins: [1.0, 0.75, 1.0, 0.75]
  font_family: "Times New Roman"
  font_size: 9
  continuation: "(continued)"

header:
  bold: true
  align: center
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

arframe discovers the file by walking up from the working directory
(like `.Rprofile`). You never need to specify the path.

### When to use what

| Setting | Where | Why |
|----|----|----|
| Font, orientation, margins | `_arframe.yml` | Same for every table in the study |
| Header bold/center, N-format | `_arframe.yml` | Same for every table |
| Page headers/footers | `_arframe.yml` | Same for every table |
| Footnote separator | `_arframe.yml` | Same for every table |
| Session overrides | [`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md) | Quick experiments, temporary changes |
| Per-table overrides | [`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md), [`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md), etc. | Only what’s unique to one table |

The four-tier precedence:

    Package defaults < _arframe.yml < fr_theme() < per-table verbs

## Study-wide defaults with `fr_theme()`

When you don’t have a YAML file (quick scripts, interactive work), set
session defaults with
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md):

``` r
fr_theme(
  font_size   = 9,
  font_family = "Times New Roman",
  orientation = "landscape",
  hlines      = "header",
  header      = list(bold = TRUE, align = "center"),
  n_format    = "{label}\n(N={n})",
  footnote_separator = FALSE,
  pagehead    = list(left = "TFRM-2024-001", right = "CONFIDENTIAL"),
  pagefoot    = list(left = "{program}",
                     right = "Page {thepage} of {total_pages}")
)
```

Now every table inherits these:

``` r
spec1 <- tbl_demog |> fr_table() |>
  fr_titles("Table 14.1.1", "Demographics")
spec2 <- tbl_disp |> fr_table() |>
  fr_titles("Table 14.1.4", "Disposition")
fr_get_page(spec1)$font_size
#> [1] 9
fr_get_page(spec2)$font_size
#> [1] 9
```

## Recipes: reusable verb chains

Recipes capture verb chains as portable objects:

``` r
# Company standard
company <- fr_recipe(
  fr_page(orientation = "landscape", font_size = 9),
  fr_pagehead(left = "TFRM-2024-001", right = "CONFIDENTIAL"),
  fr_pagefoot(left = "{program}",
              right = "Page {thepage} of {total_pages}"),
  fr_hlines("header"),
  fr_header(bold = TRUE, align = "center")
)

# AE tables need continuation text
ae_recipe <- fr_recipe(
  fr_page(continuation = "(continued)")
)

# Compose with c()
ae_combined <- c(company, ae_recipe)
```

Apply to any table:

``` r
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_apply(ae_combined) |>
  fr_titles("Table 14.3.1", "AE by SOC/PT")
fr_get_page(spec)$continuation
#> [1] "(continued)"
```

### Three-layer recipe pattern

``` r
n_itt <- c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)

# Layer 1: Company standard (shared across all studies)
company <- fr_recipe(
  fr_page(orientation = "landscape", font_family = "Times New Roman",
          font_size = 9),
  fr_pagehead(right = "CONFIDENTIAL"),
  fr_pagefoot(left = "{program}",
              right = "Page {thepage} of {total_pages}"),
  fr_hlines("header"),
  fr_header(bold = TRUE, align = "center")
)

# Layer 2: Study-specific
study <- fr_recipe(
  fr_pagehead(left = "Protocol TFRM-2024-001"),
  fr_page(tokens = list(study = "TFRM-2024-001", cutoff = "15MAR2025"))
)

# Layer 3: Table-type (only what varies)
demog_recipe <- fr_recipe(
  fr_cols(.n = n_itt, .n_format = "{label}\n(N={n})"),
  fr_footnotes("Percentages based on N per treatment arm.")
)

# One table: only titles and columns are unique
tbl_demog |>
  fr_table() |>
  fr_apply(c(company, study, demog_recipe)) |>
  fr_titles("Table 14.1.1", "Demographics") |>
  fr_cols(group = fr_col(visible = FALSE)) |>
  fr_render("output/Table_14_1_1.rtf")
```

## Single table workflow

The minimal pipeline:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1", "Demographics") |>
  fr_hlines("header")
```

Build up progressively:

``` r
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
    placebo   = fr_col("Placebo", align = "decimal"),
    zom_50mg  = fr_col("Zomerane 50mg", align = "decimal"),
    zom_100mg = fr_col("Zomerane 100mg", align = "decimal"),
    total     = fr_col("Total", align = "decimal"),
    group     = fr_col(visible = FALSE),
    .n = c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)
  ) |>
  fr_header(bold = TRUE, align = "center") |>
  fr_rows(group_by = "group", blank_after = "group") |>
  fr_hlines("header") |>
  fr_footnotes("Percentages based on number of subjects per treatment group.")
```

Validate before rendering:

``` r
fr_validate(spec)
```

## Batch rendering

Combine a metadata table with recipes and a loop:

``` r
metadata <- data.frame(
  table_id  = c("14.1.1", "14.1.4", "14.3.1"),
  title1    = c("Demographics", "Subject Disposition", "AE by SOC/PT"),
  title2    = c("ITT Population", "All Randomized", "Safety Population"),
  data_name = c("tbl_demog", "tbl_disp", "tbl_ae_soc"),
  recipe    = c("demog", "demog", "ae"),
  stringsAsFactors = FALSE
)

recipes <- list(demog = demog_recipe, ae = ae_recipe)

for (i in seq_len(nrow(metadata))) {
  row <- metadata[i, ]
  data <- get(row$data_name)
  recipe <- recipes[[row$recipe]]

  data |>
    fr_table() |>
    fr_apply(c(company, study, recipe)) |>
    fr_titles(paste("Table", row$table_id), row$title1, row$title2) |>
    fr_render(sprintf("output/Table_%s.rtf",
                      gsub("\\.", "_", row$table_id)))
}
```

## Pharmaverse integration: cards + fr_wide_ard()

The [cards](https://insightsengineering.github.io/cards/) package
produces Analysis Results Data (ARD).
[`fr_wide_ard()`](https://vthanik.github.io/arframe/reference/fr_wide_ard.md)
converts ARD output to a wide data frame ready for
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

### Demographics (continuous + categorical)

``` r
library(cards)

demog_ard <- ard_stack(
  data = adsl_saf,
  .by = "TRT01A",
  ard_continuous(variables = "AGE"),
  ard_categorical(variables = c("AGEGR1", "SEX", "RACE")),
  .overall = TRUE
)

demog_wide <- fr_wide_ard(
  demog_ard,
  statistic = list(
    continuous = c(
      "n"         = "{N}",
      "Mean (SD)" = "{mean} ({sd})",
      "Median"    = "{median}",
      "Q1, Q3"    = "{p25}, {p75}",
      "Min, Max"  = "{min}, {max}"
    ),
    categorical = "{n} ({p}%)"
  ),
  decimals = c(mean = 1, sd = 2, median = 1, p25 = 1, p75 = 1, p = 1),
  label = c(
    AGE    = "Age (years)",
    AGEGR1 = "Age Group, n (%)",
    SEX    = "Sex, n (%)",
    RACE   = "Race, n (%)"
  )
)

demog_wide |>
  fr_table() |>
  fr_cols(
    variable   = fr_col(visible = FALSE),
    stat_label = fr_col("", width = 2.5),
    .align = "decimal"
  ) |>
  fr_rows(
    group_by = list(cols = "variable", label = "stat_label"),
    group_style = list(bold = TRUE)
  )
```

### AE overall (dichotomous flags)

``` r
ae_ard <- ard_stack(
  data = ae_subj,
  .by = "TRT01A",
  ard_dichotomous(
    variables = c(any_teae, any_sae, any_death),
    value = list(any_teae = TRUE, any_sae = TRUE, any_death = TRUE)
  ),
  .overall = TRUE
)

fr_wide_ard(ae_ard, statistic = "{n} ({p}%)", decimals = c(p = 1))
```

### Hierarchical SOC/PT

``` r
ae_ard <- ard_stack_hierarchical(
  data        = adae_teae,
  variables   = c(AEBODSYS, AEDECOD),
  by          = TRT01A,
  denominator = adsl_saf,
  id          = USUBJID,
  overall     = TRUE,
  over_variables = TRUE
) |>
  sort_ard_hierarchical(sort = "descending")

ae_wide <- fr_wide_ard(
  ae_ard,
  statistic = "{n} ({p}%)",
  decimals  = c(p = 1),
  label     = c(
    "..ard_hierarchical_overall.." = "TOTAL SUBJECTS WITH AN EVENT"
  )
)
```

The output contains `soc`, `pt`, `row_type` columns for hierarchical
grouping with
[`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md).

### Per-variable format overrides

``` r
fr_wide_ard(
  demog_ard,
  statistic = list(
    AGE = c(
      "n"         = "{N}",
      "Mean (SD)" = "{mean} ({sd})"
    ),
    SEX = "{n} ({p}%)",
    .default = "{n} ({p}%)"
  ),
  decimals = list(
    AGE = c(mean = 1, sd = 2),
    .default = c(p = 1)
  )
)
```

### N-count extraction

``` r
result <- fr_wide_ard(
  demog_ard,
  statistic = "{n} ({p}%)",
  big_n = "N"
)

# N-counts extracted to attribute
result |>
  fr_table() |>
  fr_cols(.n = attr(result, "n_counts"), .n_format = "{label}\n(N={n})")
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

### Accessor-based QC

``` r
stopifnot(length(fr_get_titles(spec)) == 2)
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

## Custom stat types for decimal alignment

Register custom formats when your data uses stat patterns not covered by
the 18 built-in types:

``` r
fr_register_stat_type(
  name    = "ratio_ci",
  pattern = "^-?\\d+\\.?\\d*\\s*\\(-?\\d+\\.?\\d*,\\s*-?\\d+\\.?\\d*\\)$",
  family  = "compound",
  richness = 4L
)
```

After registration, `align = "decimal"` automatically detects and aligns
the new format alongside standard types.
