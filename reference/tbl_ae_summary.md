# Overall Adverse Event Summary Table (Table 14.3.1.1)

Pre-summarized overall AE summary table showing counts and percentages
of subjects with TEAEs by category: any TEAE, related, serious, leading
to discontinuation, leading to death, and by maximum severity grade.

Matches the pharma standard "Overall Summary of Treatment-Emergent
Adverse Events" shell. Column order follows industry convention: active
treatments first, placebo last.

## Usage

``` r
tbl_ae_summary
```

## Format

A data frame with 10 rows and 5 columns:

- category:

  Row label (e.g. `"Subjects with at Least One TEAE"`,
  `" Related TEAE"`, `" Mild"`). Indented rows (prefixed with two
  spaces) are sub-categories

- zom_50mg:

  Zomerane 50mg arm: n (%) or plain n

- zom_100mg:

  Zomerane 100mg arm: n (%) or plain n

- placebo:

  Placebo arm: n (%) or plain n

- total:

  All subjects: n (%) or plain n

## Source

Synthetic data generated in `data-raw/create_tbl_datasets.R` from
[adae](https://vthanik.github.io/arframe/reference/adae.md) and
[adsl](https://vthanik.github.io/arframe/reference/adsl.md).

## Examples

``` r
tbl_ae_summary
#>                                  category  zom_50mg  zom_100mg   placebo
#> 1           Subjects in Safety Population        45         45        45
#> 2         Subjects with at Least One TEAE 44 (97.8) 45 (100.0) 44 (97.8)
#> 3                            Related TEAE 37 (82.2)  41 (91.1) 31 (68.9)
#> 4                            Serious TEAE  8 (17.8)  16 (35.6)  5 (11.1)
#> 5         TEAE Leading to Discontinuation  6 (13.3)  12 (26.7)   3 (6.7)
#> 6                   TEAE Leading to Death         0          0         0
#> 7  Subjects with TEAE by Maximum Severity                               
#> 8                                    Mild  5 (11.1)    2 (4.4) 16 (35.6)
#> 9                                Moderate 17 (37.8)  11 (24.4) 17 (37.8)
#> 10                                 Severe 22 (48.9)  32 (71.1) 11 (24.4)
#>         total
#> 1         135
#> 2  133 (98.5)
#> 3  109 (80.7)
#> 4   29 (21.5)
#> 5   21 (15.6)
#> 6           0
#> 7            
#> 8   23 (17.0)
#> 9   45 (33.3)
#> 10  65 (48.1)

# Quick table
spec <- tbl_ae_summary |>
  fr_table() |>
  fr_cols(
    category  = fr_col("", width = 3),
    zom_50mg  = fr_col("Zomerane 50mg"),
    zom_100mg = fr_col("Zomerane 100mg"),
    placebo   = fr_col("Placebo"),
    total     = fr_col("Total")
  )
```
