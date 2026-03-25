# Demographics and Baseline Characteristics Table (Table 14.1.1)

Pre-summarized demographics table in wide format, ready for use with
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).
One row per characteristic line (headers, summary statistics, category
counts). Columns are treatment arms.

## Usage

``` r
tbl_demog
```

## Format

A data frame with columns:

- characteristic:

  Row label (e.g. `"Age (years)"`, `" Mean (SD)"`)

- placebo:

  Placebo arm summary

- zom_50mg:

  Zomerane 50mg arm summary

- zom_100mg:

  Zomerane 100mg arm summary

- total:

  All subjects summary

- group:

  Characteristic group key. Age uses separate keys for continuous
  (`"age_cont"`) and categorical (`"age_cat"`) summaries; other blocks
  use `"n"`, `"sex"`, `"race"`, `"bmi"`, `"mmse"`, `"completion"`. Use
  with `fr_rows(blank_after = "group")` to insert visual separation
  between blocks. Hide with `fr_cols(group = fr_col(visible = FALSE))`.

## Source

Synthetic data generated in `data-raw/create_tbl_datasets.R` from
[adsl](https://vthanik.github.io/arframe/reference/adsl.md).

## Examples

``` r
head(tbl_demog, 10)
#>      characteristic     placebo    zom_50mg   zom_100mg       total    group
#> 1       Subjects, n          45          45          45         135        n
#> 2       Age (years)                                                 age_cont
#> 3         Mean (SD) 75.0 (6.75) 73.1 (8.43) 75.3 (7.09) 74.4 (7.46) age_cont
#> 4            Median        74.0        74.0        73.0        74.0 age_cont
#> 5          Min, Max  65.0, 88.0  55.0, 88.0  55.0, 88.0  55.0, 88.0 age_cont
#> 6  Age Group, n (%)                                                  age_cat
#> 7               <65           0    7 (15.6)     1 (2.2)     8 (5.9)  age_cat
#> 8             65-80   36 (80.0)   29 (64.4)   31 (68.9)   96 (71.1)  age_cat
#> 9               >80    9 (20.0)    9 (20.0)   13 (28.9)   31 (23.0)  age_cat
#> 10       Sex, n (%)                                                      sex
```
