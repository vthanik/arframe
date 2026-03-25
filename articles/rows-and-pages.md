# Rows & Page Layout

[`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
controls how data flows across pages.
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md),
[`fr_pagehead()`](https://vthanik.github.io/arframe/reference/fr_pagehead.md),
and
[`fr_pagefoot()`](https://vthanik.github.io/arframe/reference/fr_pagefoot.md)
control the physical page.

## `page_by`: separate pages per parameter

``` r
vs_wk24 <- tbl_vs[tbl_vs$timepoint == "Week 24",
                   c("param", "statistic", "placebo_value",
                     "zom_50mg_value", "zom_100mg_value")]
spec <- vs_wk24 |>
  fr_table() |>
  fr_cols(param = fr_col(visible = FALSE)) |>
  fr_rows(page_by = "param")
```

Each unique value of `param` gets its own page section. The label
appears as a header above each section.

### Styling page_by labels

Page_by labels are plain text by default. Style them with
`rows = "page_by"`:

``` r
spec <- vs_wk24 |>
  fr_table() |>
  fr_cols(param = fr_col(visible = FALSE)) |>
  fr_rows(page_by = "param") |>
  fr_styles(
    fr_row_style(rows = "page_by", bold = TRUE)
  )
```

Set study-wide defaults via
[`fr_theme()`](https://vthanik.github.io/arframe/reference/fr_theme.md):

``` r
fr_theme(page_by_style = list(bold = TRUE, align = "left"))
```

> **SAS:** `BY param;` in PROC REPORT with `#BYVAL` in titles.

## `group_by` and `blank_after`

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(group = fr_col(visible = FALSE)) |>
  fr_rows(group_by = "group", blank_after = "group")
```

`group_by` keeps groups together during pagination. `blank_after`
inserts an empty row after each group.

## `indent_by`: SOC/PT hierarchies

``` r
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_cols(
    soc      = fr_col(visible = FALSE),
    pt       = fr_col("SOC / Preferred Term", width = 3.0),
    row_type = fr_col(visible = FALSE)
  ) |>
  fr_rows(group_by = "soc", indent_by = "pt")
```

| SOC / Preferred Term | placebo | zom_50mg | zom_100mg | total |
|:---|:---|:---|:---|:---|
| SUBJECTS WITH \>=1 TEAE | 44 (97.8) | 44 (97.8) | 45 (100.0) | 133 (98.5) |
| Gastrointestinal disorders | 17 (37.8) | 28 (62.2) | 27 (60.0) | 72 (53.3) |
| Nausea | 5 (11.1) | 10 (22.2) | 9 (20.0) | 24 (17.8) |
| Vomiting | 1 (2.2) | 6 (13.3) | 11 (24.4) | 18 (13.3) |
| Diarrhoea | 2 (4.4) | 7 (15.6) | 8 (17.8) | 17 (12.6) |
| Flatulence | 5 (11.1) | 5 (11.1) | 3 (6.7) | 13 (9.6) |
| Abdominal pain upper | 4 (8.9) | 2 (4.4) | 5 (11.1) | 11 (8.1) |
| Dyspepsia | 1 (2.2) | 4 (8.9) | 2 (4.4) | 7 (5.2) |
| Constipation | 1 (2.2) | 2 (4.4) | 3 (6.7) | 6 (4.4) |
| Nervous system disorders | 14 (31.1) | 24 (53.3) | 26 (57.8) | 64 (47.4) |
| Headache | 6 (13.3) | 7 (15.6) | 7 (15.6) | 20 (14.8) |
| Dizziness | 2 (4.4) | 9 (20.0) | 9 (20.0) | 20 (14.8) |
| Tremor | 3 (6.7) | 7 (15.6) | 2 (4.4) | 12 (8.9) |
| Somnolence | 2 (4.4) | 2 (4.4) | 8 (17.8) | 12 (8.9) |
| Insomnia | 3 (6.7) | 3 (6.7) | 4 (8.9) | 10 (7.4) |
| Paraesthesia | 1 (2.2) | 3 (6.7) | 2 (4.4) | 6 (4.4) |
| Infections and infestations | 21 (46.7) | 15 (33.3) | 15 (33.3) | 51 (37.8) |
| Nasopharyngitis | 11 (24.4) | 6 (13.3) | 4 (8.9) | 21 (15.6) |
| Urinary tract infection | 4 (8.9) | 5 (11.1) | 4 (8.9) | 13 (9.6) |
| Upper respiratory tract infection | 3 (6.7) | 4 (8.9) | 5 (11.1) | 12 (8.9) |
| Influenza | 5 (11.1) | 1 (2.2) | 2 (4.4) | 8 (5.9) |
| Bronchitis | 1 (2.2) | 2 (4.4) | 4 (8.9) | 7 (5.2) |
| General disorders and administration site conditions | 16 (35.6) | 14 (31.1) | 15 (33.3) | 45 (33.3) |
| Fatigue | 10 (22.2) | 6 (13.3) | 4 (8.9) | 20 (14.8) |
| Asthenia | 3 (6.7) | 4 (8.9) | 9 (20.0) | 16 (11.9) |
| Pyrexia | 2 (4.4) | 3 (6.7) | 4 (8.9) | 9 (6.7) |
| Peripheral oedema | 1 (2.2) | 3 (6.7) | 4 (8.9) | 8 (5.9) |
| Vascular disorders | 11 (24.4) | 20 (44.4) | 11 (24.4) | 42 (31.1) |
| Hypertension | 6 (13.3) | 8 (17.8) | 2 (4.4) | 16 (11.9) |
| Hot flush | 3 (6.7) | 4 (8.9) | 4 (8.9) | 11 (8.1) |
| Hypotension | 1 (2.2) | 4 (8.9) | 5 (11.1) | 10 (7.4) |
| Flushing | 3 (6.7) | 5 (11.1) | 0 | 8 (5.9) |
| Respiratory, thoracic and mediastinal disorders | 11 (24.4) | 13 (28.9) | 17 (37.8) | 41 (30.4) |
| Rhinorrhoea | 7 (15.6) | 3 (6.7) | 5 (11.1) | 15 (11.1) |
| Cough | 2 (4.4) | 5 (11.1) | 5 (11.1) | 12 (8.9) |
| Epistaxis | 2 (4.4) | 2 (4.4) | 4 (8.9) | 8 (5.9) |
| Dyspnoea | 0 | 3 (6.7) | 4 (8.9) | 7 (5.2) |
| Oropharyngeal pain | 1 (2.2) | 2 (4.4) | 2 (4.4) | 5 (3.7) |
| Musculoskeletal and connective tissue disorders | 10 (22.2) | 13 (28.9) | 16 (35.6) | 39 (28.9) |
| Back pain | 5 (11.1) | 5 (11.1) | 10 (22.2) | 20 (14.8) |
| Myalgia | 3 (6.7) | 6 (13.3) | 2 (4.4) | 11 (8.1) |
| Arthralgia | 2 (4.4) | 3 (6.7) | 4 (8.9) | 9 (6.7) |
| Pain in extremity | 3 (6.7) | 0 | 2 (4.4) | 5 (3.7) |
| Psychiatric disorders | 15 (33.3) | 12 (26.7) | 11 (24.4) | 38 (28.1) |
| Anxiety | 6 (13.3) | 4 (8.9) | 4 (8.9) | 14 (10.4) |
| Confusional state | 4 (8.9) | 3 (6.7) | 2 (4.4) | 9 (6.7) |
| Agitation | 3 (6.7) | 2 (4.4) | 4 (8.9) | 9 (6.7) |
| Depression | 2 (4.4) | 4 (8.9) | 2 (4.4) | 8 (5.9) |
| Hallucination | 1 (2.2) | 1 (2.2) | 0 | 2 (1.5) |
| Cardiac disorders | 13 (28.9) | 11 (24.4) | 12 (26.7) | 36 (26.7) |
| Palpitations | 5 (11.1) | 4 (8.9) | 1 (2.2) | 10 (7.4) |
| Bradycardia | 3 (6.7) | 2 (4.4) | 4 (8.9) | 9 (6.7) |
| Tachycardia | 1 (2.2) | 4 (8.9) | 3 (6.7) | 8 (5.9) |
| Extrasystoles | 3 (6.7) | 1 (2.2) | 3 (6.7) | 7 (5.2) |
| Atrial fibrillation | 2 (4.4) | 1 (2.2) | 4 (8.9) | 7 (5.2) |
| Metabolism and nutrition disorders | 12 (26.7) | 11 (24.4) | 12 (26.7) | 35 (25.9) |
| Dehydration | 5 (11.1) | 4 (8.9) | 5 (11.1) | 14 (10.4) |
| Decreased appetite | 2 (4.4) | 5 (11.1) | 3 (6.7) | 10 (7.4) |
| Weight decreased | 4 (8.9) | 2 (4.4) | 1 (2.2) | 7 (5.2) |
| Hypokalaemia | 3 (6.7) | 1 (2.2) | 3 (6.7) | 7 (5.2) |
| Skin and subcutaneous tissue disorders | 8 (17.8) | 10 (22.2) | 17 (37.8) | 35 (25.9) |
| Rash | 3 (6.7) | 3 (6.7) | 9 (20.0) | 15 (11.1) |
| Hyperhidrosis | 1 (2.2) | 1 (2.2) | 9 (20.0) | 11 (8.1) |
| Pruritus | 2 (4.4) | 6 (13.3) | 1 (2.2) | 9 (6.7) |
| Dermatitis | 2 (4.4) | 2 (4.4) | 1 (2.2) | 5 (3.7) |
| Dry skin | 1 (2.2) | 0 | 2 (4.4) | 3 (2.2) |
| Investigations | 6 (13.3) | 10 (22.2) | 10 (22.2) | 26 (19.3) |
| Weight increased | 2 (4.4) | 5 (11.1) | 5 (11.1) | 12 (8.9) |
| Blood creatinine increased | 4 (8.9) | 2 (4.4) | 2 (4.4) | 8 (5.9) |
| Alanine aminotransferase increased | 0 | 3 (6.7) | 4 (8.9) | 7 (5.2) |
| Renal and urinary disorders | 7 (15.6) | 10 (22.2) | 8 (17.8) | 25 (18.5) |
| Incontinence | 4 (8.9) | 6 (13.3) | 0 | 10 (7.4) |
| Nocturia | 2 (4.4) | 3 (6.7) | 3 (6.7) | 8 (5.9) |
| Dysuria | 0 | 1 (2.2) | 4 (8.9) | 5 (3.7) |
| Pollakiuria | 1 (2.2) | 2 (4.4) | 1 (2.2) | 4 (3.0) |
| Reproductive system and breast disorders | 8 (17.8) | 8 (17.8) | 7 (15.6) | 23 (17.0) |
| Erectile dysfunction | 3 (6.7) | 4 (8.9) | 3 (6.7) | 10 (7.4) |
| Menstrual disorder | 2 (4.4) | 5 (11.1) | 2 (4.4) | 9 (6.7) |
| Gynaecomastia | 4 (8.9) | 1 (2.2) | 3 (6.7) | 8 (5.9) |
| Eye disorders | 7 (15.6) | 9 (20.0) | 6 (13.3) | 22 (16.3) |
| Conjunctivitis | 2 (4.4) | 6 (13.3) | 2 (4.4) | 10 (7.4) |
| Dry eye | 3 (6.7) | 1 (2.2) | 3 (6.7) | 7 (5.2) |
| Vision blurred | 2 (4.4) | 2 (4.4) | 1 (2.2) | 5 (3.7) |
| Lacrimation increased | 0 | 3 (6.7) | 1 (2.2) | 4 (3.0) |
| Ear and labyrinth disorders | 6 (13.3) | 8 (17.8) | 8 (17.8) | 22 (16.3) |
| Tinnitus | 1 (2.2) | 5 (11.1) | 6 (13.3) | 12 (8.9) |
| Vertigo | 4 (8.9) | 2 (4.4) | 2 (4.4) | 8 (5.9) |
| Ear pain | 1 (2.2) | 1 (2.2) | 0 | 2 (1.5) |
| Blood and lymphatic system disorders | 4 (8.9) | 7 (15.6) | 7 (15.6) | 18 (13.3) |
| Anaemia | 2 (4.4) | 5 (11.1) | 3 (6.7) | 10 (7.4) |
| Leukopenia | 1 (2.2) | 3 (6.7) | 1 (2.2) | 5 (3.7) |
| Thrombocytopenia | 1 (2.2) | 0 | 3 (6.7) | 4 (3.0) |
| Hepatobiliary disorders | 5 (11.1) | 5 (11.1) | 8 (17.8) | 18 (13.3) |
| Hepatic enzyme increased | 2 (4.4) | 2 (4.4) | 3 (6.7) | 7 (5.2) |
| Cholelithiasis | 2 (4.4) | 2 (4.4) | 3 (6.7) | 7 (5.2) |
| Hepatic steatosis | 1 (2.2) | 1 (2.2) | 3 (6.7) | 5 (3.7) |

/tmp/RtmpgVlH1h/callr-scr-2eadf75221ac7 25MAR2026 21:13:56

`indent_by` indents the named column’s values by 2 characters beneath
the group header. SOC terms appear flush-left; PTs are indented.

### Multi-level indent

For deeper hierarchies (SOC / HLT / PT), pass a named list with a key
column that determines indent level per row:

``` r
ae_hierarchy <- data.frame(
  soc      = rep("Gastrointestinal disorders", 5),
  term     = c("Gastrointestinal disorders", "GI signs and symptoms",
               "Nausea", "Vomiting", "Diarrhoea"),
  row_type = c("soc", "hlt", "pt", "pt", "pt"),
  total    = c("72 (53.3)", "54 (40.0)", "24 (17.8)", "18 (13.3)", "12 (8.9)"),
  stringsAsFactors = FALSE
)

spec <- ae_hierarchy |>
  fr_table() |>
  fr_cols(
    soc      = fr_col(visible = FALSE),
    row_type = fr_col(visible = FALSE),
    term     = fr_col("SOC / HLT / Preferred Term", width = 3.5),
    total    = fr_col("Total\nn (%)")
  ) |>
  fr_rows(
    group_by  = "soc",
    indent_by = list(
      key    = "row_type",
      col    = "term",
      levels = c(soc = 0, hlt = 1, pt = 2)
    )
  )
```

Each level adds 2 space-character widths (~0.17 in at 9pt). SOC rows get
0 indent, HLT rows get 1 level, PT rows get 2 levels.

## `group_label`: auto-inject group headers

When group names and display values live in **separate columns** (e.g.,
demographics where `"Sex"` is the group and `"Male"` / `"Female"` are
the statistics), `group_label` auto-injects the group value as a header
row in the target display column:

``` r
demog_long <- data.frame(
  variable = c("Sex", "Sex", "Age (years)", "Age (years)", "Age (years)"),
  stat     = c("Female", "Male", "Mean (SD)", "Median", "Min, Max"),
  value    = c("27 (60.0)", "18 (40.0)", "75.0 (6.8)", "74.0", "65, 88"),
  stringsAsFactors = FALSE
)

spec <- demog_long |>
  fr_table() |>
  fr_cols(variable = fr_col(visible = FALSE)) |>
  fr_rows(
    group_by = list(cols = "variable", label = "stat")
  )
```

This inserts `"Sex"` and `"Age (years)"` as header rows in the `stat`
column at each group boundary. Detail rows are automatically indented
underneath (when `indent_by` is not set, it is inferred from `label`).

### Styling group headers

Bold group headers with `group_style`:

``` r
spec <- demog_long |>
  fr_table() |>
  fr_cols(variable = fr_col(visible = FALSE)) |>
  fr_rows(
    group_by = list(cols = "variable", label = "stat"),
    group_style = list(bold = TRUE)
  )
```

For full control, use
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
with `rows = "group_headers"`:

``` r
spec <- demog_long |>
  fr_table() |>
  fr_cols(variable = fr_col(visible = FALSE)) |>
  fr_rows(group_by = list(cols = "variable", label = "stat")) |>
  fr_styles(
    fr_row_style(rows = "group_headers", bold = TRUE, background = "#F0F0F0")
  )
```

Both approaches work for `leaf` hierarchies too. Target specific levels
with per-level `group_style` or `rows = "group_headers:soc"`:

``` r
fr_rows(
  group_by = list(cols = c("soc", "pt"), leaf = "pt"),
  group_style = list(soc = list(bold = TRUE))
)
```

## `group_keep`: visual-only grouping

By default, `group_by` keeps groups together on the same page. Set
`group_keep = FALSE` for visual-only grouping (indent, group headers)
without page-keeping — useful for long groups where you want the
renderer to break freely:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(group = fr_col(visible = FALSE)) |>
  fr_rows(group_by = "group", group_keep = FALSE)
```

## `wrap`: text wrapping in body cells

For listings with long text fields, set `wrap = TRUE` to enable text
wrapping in body cells. Without wrapping, long values overflow the cell
boundary; with it, the cell grows vertically to fit:

``` r
adae[1:10, c("USUBJID", "AEBODSYS", "AEDECOD", "AEOUT")] |>
  fr_table() |>
  fr_rows(wrap = TRUE)
```

`wrap` pairs naturally with `suppress` on listings where the first
column (e.g., subject ID) should appear only once per block:

``` r
adae[1:30, c("USUBJID", "AEBODSYS", "AEDECOD", "AESEV", "ASTDT")] |>
  fr_table() |>
  fr_rows(
    sort_by     = c("USUBJID", "AEBODSYS", "AEDECOD"),
    suppress = c("USUBJID", "AEBODSYS"),
    wrap        = TRUE
  )
```

## Hidden page breaks

Use the list form of `page_by` with `visible = FALSE` to get page breaks
at group boundaries without a visible label above the headers:

``` r
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_cols(soc = fr_col(visible = FALSE),
          row_type = fr_col(visible = FALSE)) |>
  fr_rows(page_by = list(cols = "soc", visible = FALSE))
```

## Combining row features

``` r
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_cols(
    soc      = fr_col(visible = FALSE),
    pt       = fr_col("SOC / PT", width = 3.0),
    row_type = fr_col(visible = FALSE),
    placebo  = fr_col("Placebo"),
    zom_50mg = fr_col("Zomerane 50mg"),
    zom_100mg = fr_col("Zomerane 100mg"),
    total    = fr_col("Total")
  ) |>
  fr_rows(group_by = "soc", indent_by = "pt")
```

## `sort_by` and `suppress`

For listings, `sort_by` controls row order and `suppress` suppresses
repeated values:

``` r
ae_list <- adae[1:20, c("USUBJID", "ARM", "AEDECOD", "AESEV")]
spec <- ae_list |>
  fr_listing() |>
  fr_rows(sort_by = c("ARM", "USUBJID"),
          suppress = "ARM")
```

`suppress` only prints the value when it changes (like `NOREPEAT` in
SAS).

## Page configuration

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_page(
    orientation = "landscape",
    paper       = "letter",
    font_family = "Times New Roman",
    font_size   = 9,
    margins     = c(top = 1.0, right = 0.75, bottom = 1.0, left = 0.75)
  )
pg <- fr_get_page(spec)
pg$orientation
#> [1] "landscape"
```

These are the package defaults — you only need
[`fr_page()`](https://vthanik.github.io/arframe/reference/fr_page.md)
when overriding.

### Orphan and widow control

`orphan_min` and `widow_min` prevent isolated rows at page boundaries.
`orphan_min` (default `3L`) is the minimum number of body rows that must
remain at the bottom of a page before a group; if fewer would remain,
the entire group moves to the next page. `widow_min` (default `3L`) is
the minimum number of rows that must carry over to the top of the next
page:

``` r
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_rows(group_by = "soc") |>
  fr_page(orphan_min = 2L, widow_min = 2L)
```

Set either to `1L` to disable the corresponding control. These settings
only take effect when `group_by` is active.

> **Custom fonts for PDF:** Set `ARFRAME_FONT_DIR` to a directory of
> `.ttf`/`.otf` files and XeLaTeX discovers them by name — no
> system-wide installation needed. See
> [`vignette("automation")`](https://vthanik.github.io/arframe/articles/automation.md)
> for Docker/CI examples.

### Margin formats

| Format                              | Meaning                           |
|-------------------------------------|-----------------------------------|
| `margins = 1.0`                     | All four margins = 1 inch         |
| `margins = c(1.0, 0.75)`            | Top/bottom = 1, left/right = 0.75 |
| `margins = c(1.0, 0.75, 1.0, 0.75)` | Top, right, bottom, left          |
| `margins = list(top = 1, ...)`      | Named list                        |

## Running headers and footers

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_pagehead(
    left  = "Protocol TFRM-2024-001",
    right = "CONFIDENTIAL"
  ) |>
  fr_pagefoot(
    left   = "{program}",
    center = "{datetime}",
    right  = "Page {thepage} of {total_pages}"
  )
```

Protocol TFRM-2024-001 CONFIDENTIAL

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

/tmp/RtmpgVlH1h/callr-scr-2eadf75221ac7 25MAR2026 21:13:56 Page 1 of 1

### Built-in tokens

| Token           | Value               |
|-----------------|---------------------|
| `{thepage}`     | Current page number |
| `{total_pages}` | Total page count    |
| `{program}`     | Source file name    |
| `{datetime}`    | Render timestamp    |

### Custom tokens

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_page(tokens = list(study = "TFRM-2024-001", cutoff = "15MAR2025")) |>
  fr_pagefoot(left = "Study: {study}", right = "Cutoff: {cutoff}")
```

> **SAS:** `TITLE j=l "Protocol..." j=r "CONFIDENTIAL";` and
> `FOOTNOTE j=l "&_SASPROGRAMFILE" j=r "Page ^{thispage} of ^{lastpage}";`

## ICH E3 compliance mapping

| ICH E3 Requirement | arframe Feature |
|----|----|
| Study ID on every page | `fr_pagehead(left = "Study TFRM-2024-001")` |
| Table number and title | `fr_titles("Table 14.1.1", "Demographics...")` |
| Population label | Third title line |
| Treatment arm N-counts | `fr_cols(.n = ..., .n_format = ...)` |
| Page numbering | `fr_pagefoot(right = "Page {thepage} of {total_pages}")` |
| Program name | `fr_pagefoot(left = "{program}")` |
| Continuation label | `fr_page(continuation = "(continued)")` |
| Footnotes/abbreviations | `fr_footnotes(...)` |

## Continuation text

For multi-page tables, `continuation` appends text to titles on page 2+:

``` r
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_titles("Table 14.3.1", "TEAEs by SOC and Preferred Term") |>
  fr_page(continuation = "(continued)")
```

## Section spacing

[`fr_spacing()`](https://vthanik.github.io/arframe/reference/fr_spacing.md)
controls blank lines between structural sections:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_spacing(
    titles_after     = 1,
    footnotes_before = 1,
    pagehead_after   = 0,
    pagefoot_before  = 0,
    page_by_after    = 1
  )
```
