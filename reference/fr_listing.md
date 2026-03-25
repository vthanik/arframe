# Start a arframe Listing Pipeline

Entry point for clinical listings — long-format data displays that show
individual records (e.g., adverse event listings, concomitant medication
listings, lab data listings). Listings differ from summary tables in
that every row is a data record, not a summary statistic.

`fr_listing()` creates an `fr_spec` with listing-appropriate defaults:
landscape orientation, smaller font (8pt), left-aligned columns,
header-only rules, and column splitting enabled.

## Usage

``` r
fr_listing(data)
```

## Arguments

- data:

  A data frame. Each row maps to one listing row.

## Value

An `fr_spec` object with `type = "listing"` and listing defaults.

## Listing defaults (vs [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md))

|  |  |  |
|----|----|----|
| Setting | [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md) | `fr_listing()` |
| Orientation | landscape | landscape |
| Font size | 9pt | 8pt |
| Default align | auto-detect | left |
| hlines | none | header |
| split | NULL | TRUE |
| wrap | FALSE | TRUE |

## Listing-specific features

- **`sort_by`**: Pass to
  [`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
  to sort data before rendering

- **`suppress`**: Pass to
  [`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
  to suppress repeated values

- **`wrap`**: Auto-enabled for long text columns

## See also

[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
for summary tables,
[`fr_rows()`](https://vthanik.github.io/arframe/reference/fr_rows.md)
for `sort_by` and `suppress`,
[`fr_render()`](https://vthanik.github.io/arframe/reference/fr_render.md)
to produce output.

## Examples

``` r
## ── Minimal listing ──────────────────────────────────────────────────────

adae |> fr_listing()
#> 
#> ── fr_spec: Listing 
#> Data: 758 rows x 27 columns
#> Page: landscape letter, 8pt Times New Roman
#> Columns (27 visible of 27):
#> STUDYID "STUDYID" auto left
#> USUBJID "USUBJID" auto left
#> ARM "ARM" auto left
#> TRTA "TRTA" auto left
#> TRTAN "TRTAN" auto left
#> AGE "AGE" auto left
#> SEX "SEX" auto left
#> RACE "RACE" auto left
#> ... and 19 more
#> Header: valign=bottom
#> Rows: wrap
#> Rules: 1 hline(s)

## ── Full listing pipeline ─────────────────────────────────────────────────

adae |>
  fr_listing() |>
  fr_cols(
    USUBJID = fr_col("Subject ID", width = 1.2),
    AEDECOD = fr_col("Preferred Term"),
    AESEV   = fr_col("Severity", width = 1.0)
  ) |>
  fr_rows(sort_by = c("USUBJID", "ASTDT"),
          suppress = "USUBJID") |>
  fr_titles("Listing 16.2.7 Adverse Events") |>
  fr_footnotes("Source: ADAE")
#> 
#> ── fr_spec: Listing 
#> Data: 758 rows x 27 columns
#> Page: landscape letter, 8pt Times New Roman
#> Titles (1):
#> 1. [center] "Listing 16.2.7 Adverse Events"
#> Columns (27 visible of 27):
#> STUDYID "STUDYID" 0.82in left
#> USUBJID "Subject ID" 1.20in left
#> ARM "ARM" 0.84in left
#> TRTA "TRTA" 0.84in left
#> TRTAN "TRTAN" 0.50in right
#> AGE "AGE" 0.50in right
#> SEX "SEX" 0.50in left
#> RACE "RACE" 2.17in left
#> ... and 19 more
#> Header: valign=bottom
#> Rows: sort_by=USUBJID,ASTDT, wrap
#> Rules: 1 hline(s)
#> Footnotes (1):
#> 1. [left] "Source: ADAE"

## ── Listing with wrap = TRUE for long text ───────────────────────────────
# wrap is TRUE by default in fr_listing(), but can also be set via fr_rows().

adcm |>
  fr_listing() |>
  fr_cols(
    USUBJID = fr_col("Subject", width = 1.2),
    CMDECOD = fr_col("Medication", width = 2.0),
    CMCAT   = fr_col("Category", width = 1.5),
    CMSTDT  = fr_col("Start Date", width = 1.0),
    CMENDT  = fr_col("End Date", width = 1.0)
  ) |>
  fr_rows(wrap = TRUE) |>
  fr_titles("Listing 16.2.4 Concomitant Medications") |>
  fr_footnotes("Source: ADCM")
#> 
#> ── fr_spec: Listing 
#> Data: 597 rows x 13 columns
#> Page: landscape letter, 8pt Times New Roman
#> Titles (1):
#> 1. [center] "Listing 16.2.4 Concomitant Medications"
#> Columns (13 visible of 13):
#> STUDYID "STUDYID" 0.82in left
#> USUBJID "Subject" 1.20in left
#> ARM "ARM" 0.84in left
#> TRTA "TRTA" 0.84in left
#> TRTAN "TRTAN" 0.50in right
#> AGE "AGE" 0.50in right
#> SEX "SEX" 0.50in left
#> SAFFL "SAFFL" 0.50in left
#> ... and 5 more
#> Header: valign=bottom
#> Rows: wrap
#> Rules: 1 hline(s)
#> Footnotes (1):
#> 1. [left] "Source: ADCM"

## ── Listing with page_by pagination ──────────────────────────────────────
# Each treatment arm starts on a new page.

adae |>
  fr_listing() |>
  fr_cols(
    USUBJID  = fr_col("Subject ID", width = 1.2),
    AEBODSYS = fr_col("System Organ Class", width = 2.0),
    AEDECOD  = fr_col("Preferred Term", width = 1.5),
    AESEV    = fr_col("Severity", width = 0.8),
    TRTA     = fr_col(visible = FALSE)
  ) |>
  fr_rows(page_by = "TRTA") |>
  fr_titles("Listing 16.2.7.1 Adverse Events by Treatment Arm")
#> 
#> ── fr_spec: Listing 
#> Data: 758 rows x 27 columns
#> Page: landscape letter, 8pt Times New Roman
#> Titles (1):
#> 1. [center] "Listing 16.2.7.1 Adverse Events by Treatment Arm"
#> Columns (26 visible of 27):
#> STUDYID "STUDYID" 0.82in left
#> USUBJID "Subject ID" 1.20in left
#> ARM "ARM" 0.84in left
#> TRTAN "TRTAN" 0.50in right
#> AGE "AGE" 0.50in right
#> SEX "SEX" 0.50in left
#> RACE "RACE" 2.17in left
#> SAFFL "SAFFL" 0.50in left
#> ... and 18 more
#> Header: valign=bottom
#> Rows: page_by=TRTA, wrap
#> Rules: 1 hline(s)

## ── sort_by + suppress together ────────────────────────────────────────
# Sort by subject and start day, suppress repeated subject IDs.

adae |>
  fr_listing() |>
  fr_cols(
    USUBJID = fr_col("Subject ID", width = 1.3),
    AEDECOD = fr_col("Preferred Term", width = 2.0),
    AESEV   = fr_col("Severity", width = 0.8),
    ASTDY   = fr_col("Study Day", width = 0.8, align = "right"),
    ADURN   = fr_col("Duration (days)", width = 1.0, align = "right")
  ) |>
  fr_rows(
    sort_by = c("USUBJID", "ASTDY"),
    suppress = "USUBJID"
  ) |>
  fr_titles("Listing 16.2.7.2 Adverse Events Sorted by Subject and Study Day") |>
  fr_footnotes("Repeated subject IDs are suppressed for readability.")
#> 
#> ── fr_spec: Listing 
#> Data: 758 rows x 27 columns
#> Page: landscape letter, 8pt Times New Roman
#> Titles (1):
#> 1. [center] "Listing 16.2.7.2 Adverse Events Sorted by Subject and Stu..."
#> Columns (27 visible of 27):
#> STUDYID "STUDYID" 0.82in left
#> USUBJID "Subject ID" 1.30in left
#> ARM "ARM" 0.84in left
#> TRTA "TRTA" 0.84in left
#> TRTAN "TRTAN" 0.50in right
#> AGE "AGE" 0.50in right
#> SEX "SEX" 0.50in left
#> RACE "RACE" 2.17in left
#> ... and 19 more
#> Header: valign=bottom
#> Rows: sort_by=USUBJID,ASTDY, wrap
#> Rules: 1 hline(s)
#> Footnotes (1):
#> 1. [left] "Repeated subject IDs are suppressed for readability."
```
