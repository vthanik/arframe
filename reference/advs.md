# Vital Signs Analysis Dataset (ADVS)

Synthetic record-level vital signs dataset from study TFRM-2024-001.
Contains one row per subject per parameter per visit. Some subjects have
missing parameters to create **natural N variation** across vital sign
groups — exactly the scenario that per-group N-count headers address.

Designed as the source for
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
`.n` data frame parameter when the display table
([tbl_vs](https://vthanik.github.io/arframe/reference/tbl_vs.md)) is
pre-summarized.

## Usage

``` r
advs
```

## Format

A data frame with approximately 1900 rows and 12 variables:

- STUDYID:

  Study identifier (`"TFRM-2024-001"`)

- USUBJID:

  Unique subject identifier

- TRTA:

  Actual treatment arm: `"Placebo"`, `"Zomerane 50mg"`, or
  `"Zomerane 100mg"`

- TRTAN:

  Actual treatment numeric code: `0`, `50`, or `100`

- SAFFL:

  Safety population flag (`"Y"`)

- PARAM:

  Vital sign parameter: `"Systolic BP (mmHg)"`, `"Diastolic BP (mmHg)"`,
  `"Heart Rate (bpm)"`, `"Weight (kg)"`, or `"Temperature (C)"`

- PARAMCD:

  Parameter code: `"SYSBP"`, `"DIABP"`, `"HR"`, `"WEIGHT"`, or `"TEMP"`

- AVISIT:

  Analysis visit: `"Baseline"`, `"Week 12"`, or `"Week 24"`

- AVAL:

  Analysis value (measured vital sign)

- BASE:

  Baseline value (copy of AVAL at Baseline visit)

- CHG:

  Change from baseline (`NA` at baseline)

- ABLFL:

  Baseline record flag: `"Y"` at Baseline, `NA` otherwise

## Source

Synthetic data generated in `data-raw/create_adam_datasets.R` using
subjects from
[adsl](https://vthanik.github.io/arframe/reference/adsl.md).

## N variation by design

Not all subjects have every parameter measured. Availability rates
decrease with dose and parameter complexity — Temperature has the most
missing data, Blood Pressure the least. This produces realistic
per-group N differences:

|             |         |          |           |
|-------------|---------|----------|-----------|
| Parameter   | Placebo | Zom 50mg | Zom 100mg |
| Systolic BP | 45      | ~41      | ~43       |
| Heart Rate  | ~40     | ~42      | ~38       |
| Temperature | ~38     | ~42      | ~36       |

## See also

[tbl_vs](https://vthanik.github.io/arframe/reference/tbl_vs.md) for the
pre-summarized display table,
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
for N-count formatting.

## Examples

``` r
# Subjects per parameter per arm
with(
  advs[advs$AVISIT == "Baseline", ],
  tapply(USUBJID, list(PARAM, TRTA), function(x) length(unique(x)))
)
#>                     Placebo Zomerane 100mg Zomerane 50mg
#> Diastolic BP (mmHg)      45             43            45
#> Heart Rate (bpm)         40             38            42
#> Systolic BP (mmHg)       45             43            41
#> Temperature (C)          38             36            42
#> Weight (kg)              43             42            41

# Use as N-count source via data frame
vs_n <- aggregate(
  USUBJID ~ PARAM + TRTA, data = advs[advs$AVISIT == "Baseline", ],
  FUN = function(x) length(unique(x))
)
names(vs_n) <- c("param", "trt", "n")
tbl_vs |>
  fr_table() |>
  fr_rows(page_by = "param") |>
  fr_cols(.n = vs_n, .n_format = "{label}\n(N={n})")
#> 
#> ── fr_spec: Table 
#> Data: 60 rows x 12 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (12 visible of 12):
#> param "param" 1.16in left
#> timepoint "timepoint" 0.58in left
#> statistic "statistic" 0.62in left
#> placebo_base "placebo_base" 0.77in left
#> placebo_value "placebo_value" 0.82in left
#> placebo_chg "placebo_chg" 0.72in left
#> zom_50mg_base "zom_50mg_base" 0.94in left
#> zom_50mg_value "zom_50mg_value" 0.99in left
#> ... and 4 more
#> Header: valign=bottom
#> Rows: page_by=param
#> Rules: 1 hline(s)
```
