# Rules, Spans & Styles

## Horizontal rules

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_hlines("header")
```

### Presets

| Preset | Description |
|----|----|
| `"header"` | Single rule below the column header only (pharma standard, ICH E3) |
| `"open"` | Rule above header + rule below header; no bottom border |
| `"hsides"` | Rule at the top (above header) and bottom (below body) only |
| `"above"` | Single rule above the column header only |
| `"below"` | Single rule below the last body row only |
| `"booktabs"` | Thick top (1pt) + thin mid (0.5pt) + thick bottom (1pt) (publication style) |
| `"box"` | Full outer border on all four sides |
| `"void"` | No horizontal rules; clears all previously set rules |

### Custom styling

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_hlines("open", width = "thick", color = "#003366", linestyle = "dashed")
```

Width options: `"hairline"` (0.25pt), `"thin"` (0.5pt), `"medium"`
(1pt), `"thick"` (1.5pt), or a numeric value in points.

> **SAS:**
> `STYLE(header)=[borderbottomstyle=solid borderbottomwidth=1pt];`

## Vertical rules and grids

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_hlines("header") |>
  fr_vlines("inner")
```

Vertical presets: `"box"` (outer edges only), `"all"` (every column +
outer), `"inner"` (between columns only), `"void"` (none).

[`fr_grid()`](https://vthanik.github.io/arframe/reference/fr_grid.md) is
shorthand for combined horizontal + vertical rules:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_grid("box/all", width = "thin", color = "#999999")
```

## Spanning headers

[`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
groups columns under a shared label:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("", width = 2.5),
    placebo   = fr_col("Placebo"),
    zom_50mg  = fr_col("Zomerane 50mg"),
    zom_100mg = fr_col("Zomerane 100mg"),
    total     = fr_col("Total"),
    group     = fr_col(visible = FALSE)
  ) |>
  fr_spans("Active Treatment" = c("zom_50mg", "zom_100mg"))
```

|  |  | Active Treatment |  |  |
|:---|:---|:---|:---|:---|
|  | Placebo | Zomerane 50mg | Zomerane 100mg | Total |
| Subjects, n | 45 | 45 | 45 | 135 |
| Age (years) |  |  |  |  |
| Mean (SD) | 75.0 (6.75) | 73.1 (8.43) | 75.3 (7.09) | 74.4 (7.46) |
| Median | 74.0 | 74.0 | 73.0 | 74.0 |
| Min, Max | 65.0, 88.0 | 55.0, 88.0 | 55.0, 88.0 | 55.0, 88.0 |
| Age Group, n (%) |  |  |  |  |
| \<65 | 0 | 7 (15.6) | 1 (2.2) | 8 (5.9) |
| 65-80 | 36 (80.0) | 29 (64.4) | 31 (68.9) | 96 (71.1) |
| \>80 | 9 (20.0) | 9 (20.0) | 13 (28.9) | 31 (23.0) |
| Sex, n (%) |  |  |  |  |
| Female | 27 (60.0) | 28 (62.2) | 20 (44.4) | 75 (55.6) |
| Male | 18 (40.0) | 17 (37.8) | 25 (55.6) | 60 (44.4) |
| Race, n (%) |  |  |  |  |
| White | 38 (84.4) | 32 (71.1) | 35 (77.8) | 105 (77.8) |
| Black or African American | 1 (2.2) | 5 (11.1) | 5 (11.1) | 11 (8.1) |
| Asian | 6 (13.3) | 6 (13.3) | 5 (11.1) | 17 (12.6) |
| American Indian or Alaska Native | 0 | 2 (4.4) | 0 | 2 (1.5) |
| BMI (kg/m2) |  |  |  |  |
| Mean (SD) | 27.1 (4.76) | 26.5 (4.60) | 24.9 (5.81) | 26.1 (5.13) |
| Median | 27.8 | 25.9 | 24.6 | 25.8 |
| Min, Max | 18.7, 42.3 | 17.3, 36.5 | 14.2, 44.5 | 14.2, 44.5 |
| MMSE Score at Baseline |  |  |  |  |
| Mean (SD) | 19.9 (3.51) | 19.8 (3.36) | 19.5 (3.55) | 19.7 (3.45) |
| Median | 20.0 | 20.0 | 19.0 | 20.0 |
| Min, Max | 13.0, 26.0 | 13.0, 26.0 | 13.0, 26.0 | 13.0, 26.0 |
| Study Completion, n (%) |  |  |  |  |
| Completed | 44 (97.8) | 41 (91.1) | 37 (82.2) | 122 (90.4) |
| Discontinued | 1 (2.2) | 4 (8.9) | 8 (17.8) | 13 (9.6) |

/tmp/RtmpgVlH1h/callr-scr-2eadf718dde40 25MAR2026 21:14:00

Unlike most verbs,
[`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
**appends** on repeated calls.

### Multi-level spans

Build from inner to outer (lowest level first):

``` r
spec |>
  fr_spans("10 mg"  = c("low_n", "low_pct"),  .level = 1) |>
  fr_spans("25 mg"  = c("high_n", "high_pct"), .level = 1) |>
  fr_spans("Zomerane" = c("low_n", "low_pct",
                           "high_n", "high_pct"), .level = 2)
```

Level 1 sits above column labels; level 2 sits above level 1.

### Tidyselect spans

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_spans("Zomerane" = starts_with("zom_"))
```

### Gap columns

By default, a narrow gap separates adjacent spans at the same level.
Disable with `.gap = FALSE`:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_spans(
    "Zomerane" = c("zom_50mg", "zom_100mg"),
    "Reference" = "placebo",
    .gap = FALSE
  )
```

## Cell styling

Apply visual overrides to rows, columns, or individual cells:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(
    characteristic = fr_col("", width = 2.5),
    placebo   = fr_col("Placebo"),
    zom_50mg  = fr_col("Zomerane 50mg"),
    zom_100mg = fr_col("Zomerane 100mg"),
    total     = fr_col("Total"),
    group     = fr_col(visible = FALSE)
  ) |>
  fr_hlines("header") |>
  fr_styles(
    fr_row_style(rows = 1, bold = TRUE),
    fr_col_style(cols = "total", background = "#EBF5FB"),
    fr_style(rows = 3, cols = "placebo", color = "#CC0000")
  )
```

|  | Placebo | Zomerane 50mg | Zomerane 100mg | Total |
|:---|:---|:---|:---|:---|
| Subjects, n | 45 | 45 | 45 | 135 |
| Age (years) |  |  |  |  |
| Mean (SD) | 75.0 (6.75) | 73.1 (8.43) | 75.3 (7.09) | 74.4 (7.46) |
| Median | 74.0 | 74.0 | 73.0 | 74.0 |
| Min, Max | 65.0, 88.0 | 55.0, 88.0 | 55.0, 88.0 | 55.0, 88.0 |
| Age Group, n (%) |  |  |  |  |
| \<65 | 0 | 7 (15.6) | 1 (2.2) | 8 (5.9) |
| 65-80 | 36 (80.0) | 29 (64.4) | 31 (68.9) | 96 (71.1) |
| \>80 | 9 (20.0) | 9 (20.0) | 13 (28.9) | 31 (23.0) |
| Sex, n (%) |  |  |  |  |
| Female | 27 (60.0) | 28 (62.2) | 20 (44.4) | 75 (55.6) |
| Male | 18 (40.0) | 17 (37.8) | 25 (55.6) | 60 (44.4) |
| Race, n (%) |  |  |  |  |
| White | 38 (84.4) | 32 (71.1) | 35 (77.8) | 105 (77.8) |
| Black or African American | 1 (2.2) | 5 (11.1) | 5 (11.1) | 11 (8.1) |
| Asian | 6 (13.3) | 6 (13.3) | 5 (11.1) | 17 (12.6) |
| American Indian or Alaska Native | 0 | 2 (4.4) | 0 | 2 (1.5) |
| BMI (kg/m2) |  |  |  |  |
| Mean (SD) | 27.1 (4.76) | 26.5 (4.60) | 24.9 (5.81) | 26.1 (5.13) |
| Median | 27.8 | 25.9 | 24.6 | 25.8 |
| Min, Max | 18.7, 42.3 | 17.3, 36.5 | 14.2, 44.5 | 14.2, 44.5 |
| MMSE Score at Baseline |  |  |  |  |
| Mean (SD) | 19.9 (3.51) | 19.8 (3.36) | 19.5 (3.55) | 19.7 (3.45) |
| Median | 20.0 | 20.0 | 19.0 | 20.0 |
| Min, Max | 13.0, 26.0 | 13.0, 26.0 | 13.0, 26.0 | 13.0, 26.0 |
| Study Completion, n (%) |  |  |  |  |
| Completed | 44 (97.8) | 41 (91.1) | 37 (82.2) | 122 (90.4) |
| Discontinued | 1 (2.2) | 4 (8.9) | 8 (17.8) | 13 (9.6) |

/tmp/RtmpgVlH1h/callr-scr-2eadf718dde40 25MAR2026 21:14:00

### Style constructors

| Constructor | Scope | Key args |
|----|----|----|
| [`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md) | Entire row(s) | `rows`, `bold`, `background`, `color`, `align`, `valign`, `height`, … |
| `fr_row_style(rows = "group_headers")` | Group header rows | `bold`, `background`, `color`, … |
| `fr_row_style(rows = "page_by")` | Page_by section labels | `bold`, `align`, `color`, `font_size`, … |
| [`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md) | Entire column(s) | `cols`, `bold`, `background`, `color`, `align`, `valign`, … |
| [`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md) | Row-column intersection | `region`, `rows`, `cols`, `bold`, `background`, `color`, `align`, `valign`, `colspan`, … |
| [`fr_style_if()`](https://vthanik.github.io/arframe/reference/fr_style_if.md) | Data-driven (conditional) | `condition`, `cols`, `apply_to`, `bold`, `background`, `color`, `align`, … |

[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md)
targets a specific `region`: `"body"` (default), `"header"`, or
`"stub"`. `valign` controls vertical placement within the cell: `"top"`,
`"middle"`, or `"bottom"`.

Like
[`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md),
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
**appends** on repeated calls.

### Group header styling

When
[`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
creates group headers (via `group_by` with `label` or `leaf`), style
them with `group_style` on
[`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md) or
`rows = "group_headers"` in
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md):

``` r
demog_long <- data.frame(
  variable = c("Sex", "Sex", "Age", "Age"),
  stat     = c("Female", "Male", "Mean (SD)", "Median"),
  value    = c("27 (60.0)", "18 (40.0)", "75.0 (6.8)", "74.0"),
  stringsAsFactors = FALSE
)

# Quick path: group_style on fr_rows()
spec <- demog_long |>
  fr_table() |>
  fr_cols(variable = fr_col(visible = FALSE)) |>
  fr_rows(
    group_by = list(cols = "variable", label = "stat"),
    group_style = list(bold = TRUE)
  )
```

``` r
# Full path: fr_styles() with group_headers selector
spec <- demog_long |>
  fr_table() |>
  fr_cols(variable = fr_col(visible = FALSE)) |>
  fr_rows(group_by = list(cols = "variable", label = "stat")) |>
  fr_styles(
    fr_row_style(rows = "group_headers", bold = TRUE, background = "#F0F0F0")
  )
```

For leaf hierarchies, target specific levels with
`rows = "group_headers:soc"`:

``` r
spec |>
  fr_styles(
    fr_row_style(rows = "group_headers:soc", bold = TRUE),
    fr_row_style(rows = "group_headers:hlt", italic = TRUE)
  )
```

### Page_by label styling

Page_by labels (`fr_rows(page_by = "param")`) are plain text by default
across all backends. Style them with `rows = "page_by"`:

``` r
spec <- tbl_vs[tbl_vs$timepoint == "Week 24", ] |>
  fr_table() |>
  fr_cols(param = fr_col(visible = FALSE),
          timepoint = fr_col(visible = FALSE)) |>
  fr_rows(page_by = "param") |>
  fr_styles(
    fr_row_style(rows = "page_by", bold = TRUE)
  )
```

Supported properties: `bold`, `italic`, `underline`, `color`,
`background`, `font_size`, `align`. Consistent across RTF, HTML, and
PDF.

### Content-based row selection

[`fr_rows_matches()`](https://vthanik.github.io/arframe/reference/fr_rows_matches.md)
selects rows by data values instead of row numbers:

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
  fr_styles(
    fr_row_style(
      rows = fr_rows_matches("row_type", value = "soc"), bold = TRUE
    ),
    fr_row_style(
      rows = fr_rows_matches("row_type", value = "total"),
      bold = TRUE, background = "#D5E8D4"
    )
  )
```

| SOC / PT | Placebo | Zomerane 50mg | Zomerane 100mg | Total |
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

/tmp/RtmpgVlH1h/callr-scr-2eadf718dde40 25MAR2026 21:14:00

Regex patterns work too:

``` r
spec <- tbl_tte |>
  fr_table() |>
  fr_styles(
    fr_row_style(
      rows = fr_rows_matches("statistic", pattern = "^[A-Z]"),
      bold = TRUE
    )
  )
```

### Header region styling

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_style(region = "header", bold = TRUE, background = "#003366",
             color = "#FFFFFF", align = "center")
  )
```

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

/tmp/RtmpgVlH1h/callr-scr-2eadf718dde40 25MAR2026 21:14:00

### Zebra striping

``` r
spec <- tbl_disp |>
  fr_table() |>
  fr_styles(
    fr_row_style(rows = seq(1, nrow(tbl_disp), 2), background = "#F5F5F5")
  )
```

| category           | placebo   | zom_50mg  | zom_100mg | total      |
|:-------------------|:----------|:----------|:----------|:-----------|
| Randomized         | 45        | 45        | 45        | 135        |
| Completed          | 44 (97.8) | 41 (91.1) | 37 (82.2) | 122 (90.4) |
| Discontinued       | 1 (2.2)   | 4 (8.9)   | 8 (17.8)  | 13 (9.6)   |
| Adverse Event      | 1 (2.2)   | 2 (4.4)   | 3 (6.7)   | 6 (4.4)    |
| Withdrew Consent   | 0         | 1 (2.2)   | 3 (6.7)   | 4 (3.0)    |
| Lost to Follow-up  | 0         | 0         | 0         | 0          |
| Lack of Efficacy   | 0         | 1 (2.2)   | 2 (4.4)   | 3 (2.2)    |
| Physician Decision | 0         | 0         | 0         | 0          |

/tmp/RtmpgVlH1h/callr-scr-2eadf718dde40 25MAR2026 21:14:00

### Conditional styling with `fr_style_if()`

[`fr_style_if()`](https://vthanik.github.io/arframe/reference/fr_style_if.md)
applies styles based on cell values rather than row numbers. The
condition is a one-sided formula using `.x` as the pronoun, or a plain
function. It is evaluated at render time against the actual data, so it
works correctly even when row positions are unknown in advance.

**Bold “Randomized” row** — match exact text in a column:

``` r
spec <- tbl_disp |>
  fr_table() |>
  fr_hlines("header") |>
  fr_styles(
    fr_style_if(
      cols = "category",
      condition = ~ .x == "Randomized",
      apply_to = "row",
      bold = TRUE, background = "#E8E8E8"
    )
  )
```

**Zebra striping without hard-coded indices** — when `cols = NULL`, `.x`
receives row indices:

``` r
spec <- tbl_disp |>
  fr_table() |>
  fr_hlines("header") |>
  fr_styles(
    fr_style_if(
      condition = ~ (.x %% 2) == 0,
      apply_to = "row",
      background = "#F5F5F5"
    )
  )
```

**Highlight significant p-values** — numeric comparison on a character
column:

``` r
pval_data <- data.frame(
  characteristic = c("Age", "Sex", "Weight"),
  treatment = c("50 (23.5)", "30 (14.1)", "45 (21.1)"),
  placebo   = c("55 (25.8)", "28 (13.1)", "52 (24.4)"),
  pvalue    = c("0.042", "0.310", "0.003"),
  stringsAsFactors = FALSE
)

spec <- pval_data |>
  fr_table() |>
  fr_hlines("header") |>
  fr_styles(
    fr_style_if(
      cols = "pvalue",
      condition = ~ as.numeric(.x) < 0.05,
      apply_to = "row",
      bold = TRUE, color = "#CC0000"
    )
  )
```

**Colour only the matching cells** — use `apply_to = "cell"` (the
default) instead of `"row"`:

``` r
spec <- tbl_ae_soc |>
  fr_table() |>
  fr_hlines("header") |>
  fr_styles(
    fr_style_if(
      cols = "soc",
      condition = ~ grepl("SKIN|GASTROINTESTINAL", .x, ignore.case = TRUE),
      apply_to = "row",
      background = "#FFF3CD"
    )
  )
```

The `condition` argument also accepts a plain function instead of a
formula:

``` r
is_randomized <- function(x) x == "Randomized"

tbl_disp |>
  fr_table() |>
  fr_styles(
    fr_style_if(
      cols = "category",
      condition = is_randomized,
      apply_to = "row",
      bold = TRUE, background = "#E8E8E8"
    )
  )
```

> **SAS:**
> `COMPUTE category; IF category = "Randomized" THEN CALL DEFINE(_ROW_, "STYLE", "STYLE=[FONT_WEIGHT=BOLD BACKGROUND=GRAY]"); ENDCOMP;`

[`fr_style_if()`](https://vthanik.github.io/arframe/reference/fr_style_if.md)
objects are passed to
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
alongside
[`fr_row_style()`](https://vthanik.github.io/arframe/reference/fr_row_style.md),
[`fr_col_style()`](https://vthanik.github.io/arframe/reference/fr_col_style.md),
and
[`fr_style()`](https://vthanik.github.io/arframe/reference/fr_style.md).
All four constructor types can be mixed in a single
[`fr_styles()`](https://vthanik.github.io/arframe/reference/fr_styles.md)
call.

### Style precedence

When multiple styles target the same cell, later styles win. Order from
broadest to most specific:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(group = fr_col(visible = FALSE)) |>
  fr_styles(
    fr_col_style(cols = "total", background = "#F0F4F8"),       # broadest
    fr_row_style(rows = 1, bold = TRUE),                # narrower
    fr_style(rows = 1, cols = "total",
             background = "#003366", color = "#FFFFFF")             # most specific
  )
```

### Style debugging

[`fr_style_explain()`](https://vthanik.github.io/arframe/reference/fr_style_explain.md)
shows the resolution chain for a specific cell:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_styles(
    fr_row_style(rows = 1, bold = TRUE),
    fr_col_style(cols = "total", background = "#EBF5FB")
  )
fr_style_explain(spec, row = 1, col = "total")
#> 
#> ── Style explain: row 1, col "total"
#> Content: "135"
#> 2 matching styles:
#> [1] row: bold=TRUE
#> [2] col: background="#EBF5FB"
#> 
#> Final: bold=TRUE, italic=FALSE, color=#000000, background=#EBF5FB, align=left,
#> valign=top, indent=0
```
