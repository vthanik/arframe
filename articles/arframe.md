# Get Started with arframe

arframe is a regulatory frame for TFLs — submission-ready tables,
listings, and figures in RTF, PDF, and HTML from a single specification.
You describe the output once; arframe renders it. Change the file
extension, the output adapts. No dplyr, no gt, no RMarkdown — just data
in, document out.

## Your first table

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_titles("Table 14.1.1", "Demographics and Baseline Characteristics") |>
  fr_hlines("header")
spec
```

Table 14.1.1

Demographics and Baseline Characteristics

| characteristic | placebo | zom_50mg | zom_100mg | total | group |
|:---|:---|:---|:---|:---|:---|
| Subjects, n | 45 | 45 | 45 | 135 | n |
| Age (years) |  |  |  |  | age_cont |
| Mean (SD) | 75.0 (6.75) | 73.1 (8.43) | 75.3 (7.09) | 74.4 (7.46) | age_cont |
| Median | 74.0 | 74.0 | 73.0 | 74.0 | age_cont |
| Min, Max | 65.0, 88.0 | 55.0, 88.0 | 55.0, 88.0 | 55.0, 88.0 | age_cont |
| Age Group, n (%) |  |  |  |  | age_cat |
| \<65 | 0 | 7 (15.6) | 1 (2.2) | 8 (5.9) | age_cat |
| 65-80 | 36 (80.0) | 29 (64.4) | 31 (68.9) | 96 (71.1) | age_cat |
| \>80 | 9 (20.0) | 9 (20.0) | 13 (28.9) | 31 (23.0) | age_cat |
| Sex, n (%) |  |  |  |  | sex |
| Female | 27 (60.0) | 28 (62.2) | 20 (44.4) | 75 (55.6) | sex |
| Male | 18 (40.0) | 17 (37.8) | 25 (55.6) | 60 (44.4) | sex |
| Race, n (%) |  |  |  |  | race |
| White | 38 (84.4) | 32 (71.1) | 35 (77.8) | 105 (77.8) | race |
| Black or African American | 1 (2.2) | 5 (11.1) | 5 (11.1) | 11 (8.1) | race |
| Asian | 6 (13.3) | 6 (13.3) | 5 (11.1) | 17 (12.6) | race |
| American Indian or Alaska Native | 0 | 2 (4.4) | 0 | 2 (1.5) | race |
| BMI (kg/m2) |  |  |  |  | bmi |
| Mean (SD) | 27.1 (4.76) | 26.5 (4.60) | 24.9 (5.81) | 26.1 (5.13) | bmi |
| Median | 27.8 | 25.9 | 24.6 | 25.8 | bmi |
| Min, Max | 18.7, 42.3 | 17.3, 36.5 | 14.2, 44.5 | 14.2, 44.5 | bmi |
| MMSE Score at Baseline |  |  |  |  | mmse |
| Mean (SD) | 19.9 (3.51) | 19.8 (3.36) | 19.5 (3.55) | 19.7 (3.45) | mmse |
| Median | 20.0 | 20.0 | 19.0 | 20.0 | mmse |
| Min, Max | 13.0, 26.0 | 13.0, 26.0 | 13.0, 26.0 | 13.0, 26.0 | mmse |
| Study Completion, n (%) |  |  |  |  | completion |
| Completed | 44 (97.8) | 41 (91.1) | 37 (82.2) | 122 (90.4) | completion |
| Discontinued | 1 (2.2) | 4 (8.9) | 8 (17.8) | 13 (9.6) | completion |

/tmp/RtmpgVlH1h/callr-scr-2eadf2ce2dd3 25MAR2026 21:13:30

That’s a complete, renderable table. To produce a file:

``` r
spec |> fr_render("Table_14_1_1.rtf")
spec |> fr_render("Table_14_1_1.pdf")
spec |> fr_render("Table_14_1_1.html")
```

Same spec, any format. The file extension controls the output.

## Production-ready table

Real pharma tables need column labels, N-counts, footnotes, and page
chrome. Start by setting shared formatting **once** with
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md)
— every table in the session inherits it:

``` r
# ── Set once per study program (or use _arframe.yml) ──
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

n_itt <- c(placebo = 45, zom_50mg = 45, zom_100mg = 45, total = 135)
```

Now each table only specifies what is **unique** to it:

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
    placebo        = fr_col("Placebo", align = "decimal"),
    zom_50mg       = fr_col("Zomerane 50mg", align = "decimal"),
    zom_100mg      = fr_col("Zomerane 100mg", align = "decimal"),
    total          = fr_col("Total", align = "decimal"),
    group          = fr_col(visible = FALSE),
    .n = n_itt
  ) |>
  fr_rows(group_by = "group", blank_after = "group") |>
  fr_footnotes("Percentages based on number of subjects per treatment group.")
fr_validate(spec)
```

TFRM-2024-001 CONFIDENTIAL

Table 14.1.1

Demographics and Baseline Characteristics

Intent-to-Treat Population

[TABLE]

/tmp/RtmpgVlH1h/callr-scr-2eadf2ce2dd3 Page 1 of 1

Notice what is **not** in the table program: header styling, hlines,
page headers/footers, N-format, footnote separator — the theme handles
all of it. This is how production study programs work. See [Automation &
Batch](https://vthanik.github.io/arframe/articles/automation.md) for the
full pattern.

## Table anatomy

Every pharma table has the same structural regions. Here’s how they map
to arframe verbs:

    +------------------------------------------------------------------------+
    | Protocol TFRM-2024-001                                    CONFIDENTIAL |<- fr_pagehead()
    +------------------------------------------------------------------------+
    |                                                                        |
    |                           Table 14.1.1                                 |<- fr_titles()
    |               Demographics and Baseline Characteristics                |
    |                                                                        |
    | ---------------------------------------------------------------------- |
    |                  Placebo    Zomerane 50mg   Zomerane 100mg    Total    |<- fr_cols() +
    |                   (N=45)      (N=45)          (N=45)        (N=135)    |   fr_cols(.n=)
    | ====================================================================== |<- fr_hlines()
    | Age (years)                                                            |
    |   Mean (SD)    68.2 (7.1)   67.8 (6.9)     68.0 (7.3)    68.0 (7.1)    |<- data rows
    |                                                                        |
    | Sex, n (%)                                                             |<- fr_rows(blank_after=)
    |   Male         26 (57.8)    24 (53.3)      25 (55.6)     75 (55.6)     |
    | ---------------------------------------------------------------------- |
    | Percentages based on number of subjects per treatment group.           |<- fr_footnotes()
    +------------------------------------------------------------------------+
    | t_demog.R                                                 Page 1 of 1  |<- fr_pagefoot()
    +------------------------------------------------------------------------+

10 verbs, 10 regions. Learn the verbs, build any table.

## The pipeline

Every verb takes a spec, returns a spec. Verb order doesn’t matter —
arframe resolves everything (widths, N-count labels, page breaks) at
render time.

``` r
data |>
  fr_table()    |>   # create spec
  fr_titles()   |>   # add titles
  fr_cols()     |>   # configure columns, N-counts
  fr_header()   |>   # style the header row
  fr_rows()     |>   # row grouping, pagination
  fr_hlines()   |>   # horizontal rules
  fr_styles()   |>   # cell-level formatting
  fr_render("out.rtf")  # produce output
```

## What’s next

| I want to… | Read |
|----|----|
| Configure columns, widths, decimal alignment | [Columns & Headers](https://vthanik.github.io/arframe/articles/columns-and-headers.md) |
| Add N-counts to column headers | [Columns & Headers](https://vthanik.github.io/arframe/articles/columns-and-headers.md) |
| Write titles, footnotes, superscripts | [Titles & Footnotes](https://vthanik.github.io/arframe/articles/titles-and-footnotes.md) |
| Group rows, paginate, indent, group_label | [Rows & Page Layout](https://vthanik.github.io/arframe/articles/rows-and-pages.md) |
| Add borders, spanning headers, cell colors | [Rules, Spans & Styles](https://vthanik.github.io/arframe/articles/styling.md) |
| Copy a complete table recipe | [Table Cookbook](https://vthanik.github.io/arframe/articles/table-cookbook.md) |
| Create listings or embed figures | [Listings & Figures](https://vthanik.github.io/arframe/articles/listings-and-figures.md) |
| Set up study-wide defaults, batch render | [Automation & Batch](https://vthanik.github.io/arframe/articles/automation.md) |
| Understand the internals | [Architecture](https://vthanik.github.io/arframe/articles/architecture.md) |
