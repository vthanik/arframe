# Vital Signs Change from Baseline Table (Table 14.3.5.1)

Pre-summarized vital signs table in wide format with per-arm sub-columns
for Baseline, Value, and Change from Baseline. Multiple timepoints
(Baseline, Week 12, Week 24) per parameter.

Designed to demonstrate advanced arframe features: `page_by` for
per-parameter pages,
[`fr_spans()`](https://vthanik.github.io/arframe/reference/fr_spans.md)
for two-level spanning headers,
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
with `.n` data frame for per-group N-counts, and
`fr_cols(.split = TRUE)` for wide layouts.

Pair with [advs](https://vthanik.github.io/arframe/reference/advs.md)
via a data frame in
[`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
`.n` to get per-parameter N-counts in column headers.

## Usage

``` r
tbl_vs
```

## Format

A data frame with 60 rows and 12 variables:

- param:

  Vital sign parameter name (page_by key): `"Systolic BP (mmHg)"`,
  `"Diastolic BP (mmHg)"`, `"Heart Rate (bpm)"`, `"Weight (kg)"`,
  `"Temperature (C)"`

- timepoint:

  Visit timepoint: `"Baseline"`, `"Week 12"`, or `"Week 24"`

- statistic:

  Summary statistic: `"n"`, `"Mean (SD)"`, `"Median"`, or `"Min, Max"`

- placebo_base:

  Placebo — baseline value

- placebo_value:

  Placebo — value at timepoint

- placebo_chg:

  Placebo — change from baseline (blank at Baseline)

- zom_50mg_base:

  Zomerane 50mg — baseline value

- zom_50mg_value:

  Zomerane 50mg — value at timepoint

- zom_50mg_chg:

  Zomerane 50mg — change from baseline

- zom_100mg_base:

  Zomerane 100mg — baseline value

- zom_100mg_value:

  Zomerane 100mg — value at timepoint

- zom_100mg_chg:

  Zomerane 100mg — change from baseline

## Source

Synthetic data generated in `data-raw/create_tbl_datasets.R` from
[advs](https://vthanik.github.io/arframe/reference/advs.md).

## See also

[advs](https://vthanik.github.io/arframe/reference/advs.md) for the
record-level source data,
[`fr_header()`](https://vthanik.github.io/arframe/reference/fr_header.md)
for N-count label formatting.

## Examples

``` r
head(tbl_vs, 8)
#>                param timepoint statistic  placebo_base placebo_value
#> 1 Systolic BP (mmHg)  Baseline         n            45            45
#> 2 Systolic BP (mmHg)  Baseline Mean (SD) 136.8 (17.61) 136.8 (17.61)
#> 3 Systolic BP (mmHg)  Baseline    Median         136.6         136.6
#> 4 Systolic BP (mmHg)  Baseline  Min, Max   86.5, 181.5   86.5, 181.5
#> 5 Systolic BP (mmHg)   Week 12         n            45            45
#> 6 Systolic BP (mmHg)   Week 12 Mean (SD) 136.8 (17.61) 136.8 (17.57)
#> 7 Systolic BP (mmHg)   Week 12    Median         136.6         137.2
#> 8 Systolic BP (mmHg)   Week 12  Min, Max   86.5, 181.5   86.9, 181.8
#>   placebo_chg zom_50mg_base zom_50mg_value zom_50mg_chg zom_100mg_base
#> 1                        41             41                          43
#> 2             131.6 (13.15)  131.6 (13.15)               135.6 (16.63)
#> 3                     131.4          131.4                       135.6
#> 4              104.2, 154.1   104.2, 154.1                 86.3, 170.3
#> 5          45            41             41           41             43
#> 6 -0.0 (1.47) 131.6 (13.15)  128.4 (13.28)  -3.1 (1.40)  135.6 (16.63)
#> 7         0.0         131.4          128.8         -3.2          135.6
#> 8   -3.4, 3.1  104.2, 154.1    99.9, 152.1    -5.6, 0.1    86.3, 170.3
#>   zom_100mg_value zom_100mg_chg
#> 1              43              
#> 2   135.6 (16.63)              
#> 3           135.6              
#> 4     86.3, 170.3              
#> 5              43            43
#> 6   130.1 (16.67)   -5.5 (1.59)
#> 7           130.7          -5.2
#> 8     80.8, 167.1    -8.9, -0.5
```
