# Titles & Footnotes

## Titles

Each argument to
[`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md)
is one title line, centered by default:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1",
    "Demographics and Baseline Characteristics",
    "Intent-to-Treat Population"
  )
length(fr_get_titles(spec))
#> [1] 3
```

Calling
[`fr_titles()`](https://vthanik.github.io/arframe/reference/fr_titles.md)
again **replaces** all previous titles.

> **SAS:** `TITLE1 "Table 14.1.1"; TITLE2 "Demographics...";`

### Styled titles

Pass a named list to control alignment and bold per line:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_titles(
    list("Sponsor: Acme Pharma", align = "left"),
    list("Protocol: TFRM-2024-001", align = "left"),
    "Table 14.1.1",
    list("Demographics and Baseline Characteristics", bold = TRUE),
    "Intent-to-Treat Population"
  )
length(fr_get_titles(spec))
#> [1] 5
```

Sponsor: Acme Pharma

Protocol: TFRM-2024-001

Table 14.1.1

Demographics and Baseline Characteristics

Intent-to-Treat Population

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

/tmp/RtmpgVlH1h/callr-scr-2eadf1eb9819b 25MAR2026 21:14:09

List fields: `align` (`"left"`, `"center"`, `"right"`), `bold`
(logical), `font_size` (numeric).

### Default styling

`.align` and `.bold` set defaults for all lines:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1",
    "Demographics and Baseline Characteristics",
    .bold = TRUE, .align = "center"
  )
fr_get_titles(spec)[[1]]$bold
#> [1] TRUE
```

## Footnotes

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_footnotes(
    "Percentages based on number of subjects per treatment group.",
    "MMSE = Mini-Mental State Examination.",
    .separator = FALSE
  )
length(fr_get_footnotes(spec))
#> [1] 2
```

| Option | Default | Description |
|----|----|----|
| `.separator` | `FALSE` | Draw a horizontal separator rule above footnotes |
| `.placement` | `"every"` | `"last"` = last page only (PDF only; RTF repeats all) |
| `.align` | `"left"` | Footnote text alignment |

> **SAS:** `FOOTNOTE1 "Percentages...";`

## Inline markup

Use `{fr_*()}` expressions inside any text string — titles, footnotes,
column labels, data cells:

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_footnotes(
    "{fr_super('a')} Fisher's exact test.",
    "{fr_super('b')} Cochran-Mantel-Haenszel test.",
    "BMI = Body Mass Index (kg/m{fr_super(2)})."
  )
fr_get_footnotes(spec)[[3]]$content
#> [1] "BMI = Body Mass Index (kg/m\001SUPER:2\002)."
```

### Markup reference

| Function | Output | Use case |
|----|----|----|
| `fr_super(x)` | Superscript | Footnote markers, units (`"m{fr_super(2)}"`) |
| `fr_sub(x)` | Subscript | Chemical formulas (`"H{fr_sub(2)}O"`) |
| `fr_bold(x)` | **Bold** | Inline emphasis (`"{fr_bold('Total')} Subjects"`) |
| `fr_italic(x)` | *Italic* | P-value annotations (`"{fr_italic('P')}-value"`) |
| `fr_underline(x)` | Underline | Highlighting (`"{fr_underline('CONFIDENTIAL')}"`) |
| [`fr_newline()`](https://vthanik.github.io/arframe/reference/fr_newline.md) | Line break | Multi-line cells (`"Treatment{fr_newline()}Arm"`) |
| [`fr_dagger()`](https://vthanik.github.io/arframe/reference/fr_dagger.md) | Dagger (U+2020) | Treatment-related AE markers |
| [`fr_ddagger()`](https://vthanik.github.io/arframe/reference/fr_ddagger.md) | Double dagger (U+2021) | Serious AE markers |
| [`fr_emdash()`](https://vthanik.github.io/arframe/reference/fr_emdash.md) | Em dash (U+2014) | Title clauses (`"Safety {fr_emdash()} FAS"`) |
| [`fr_endash()`](https://vthanik.github.io/arframe/reference/fr_endash.md) | En dash (U+2013) | Numeric ranges (`"18{fr_endash()}65 years"`) |
| `fr_unicode(code)` | Unicode char | Plus-minus (`fr_unicode(0x00B1)`), degree, etc. |

### Markup in column labels

``` r
spec <- tbl_demog |>
  fr_table() |>
  fr_cols(
    placebo = fr_col("{fr_bold('Placebo')}")
  )
```

### Markup in data cells

Markup works anywhere a string appears, including the data itself:

``` r
d <- data.frame(stat = "P{fr_super('a')}-value", val = "<0.001",
                stringsAsFactors = FALSE)
spec <- d |>
  fr_table() |>
  fr_cols(
    stat = fr_col("Statistic", width = 2),
    val  = fr_col("Result", width = 1.5)
  )
```

The sentinel tokens survive
[`paste()`](https://rdrr.io/r/base/paste.html),
[`sprintf()`](https://rdrr.io/r/base/sprintf.html), and `glue()` — they
are resolved per-backend at render time.
