# Time-to-Event Summary Table (Table 14.2.1.1)

Pre-summarized time-to-withdrawal table matching the pharma reference
shell for Kaplan-Meier survival analysis output. Includes four sections:

- **Event counts**: events and censored n (%) per arm

- **KM percentile estimates**: 25th percentile, median, 75th percentile
  with 95% confidence intervals

- **Log-rank test**: two-sided p-value

- **Hazard ratios**: active vs placebo with 95% CI

KM estimates and hazard ratios are pre-computed synthetic values (no
`survival` package dependency). Values are realistic for a 24-week trial
with low event rates.

## Usage

``` r
tbl_tte
```

## Format

A data frame with 12 rows and 5 columns:

- section:

  Section grouping key: `"Time to Study Withdrawal"`, `"KM Estimates"`,
  `"Log-Rank Test"`, or `"Hazard Ratio"`. Use with
  `fr_rows(group_by = "section", blank_after = "section")` for visual
  separation between blocks

- statistic:

  Row label with indentation (e.g. `" Median (95% CI) [a]"`,
  `" Zom 50mg vs Placebo"`). Section headers have no indent; detail rows
  are indented with two spaces

- zom_50mg:

  Zomerane 50mg arm value

- zom_100mg:

  Zomerane 100mg arm value

- placebo:

  Placebo arm value

## Source

Synthetic data generated in `data-raw/create_tbl_datasets.R` from
[adtte](https://vthanik.github.io/arframe/reference/adtte.md) (TTWD
parameter).

## Examples

``` r
tbl_tte
#>                     section                       statistic
#> 1  Time to Study Withdrawal        Time to Study Withdrawal
#> 2  Time to Study Withdrawal                   Events, n (%)
#> 3  Time to Study Withdrawal                 Censored, n (%)
#> 4              KM Estimates Time to Study Withdrawal (Days)
#> 5              KM Estimates    25th Percentile (95% CI) [a]
#> 6              KM Estimates             Median (95% CI) [a]
#> 7              KM Estimates    75th Percentile (95% CI) [a]
#> 8             Log-Rank Test                   Log-Rank Test
#> 9             Log-Rank Test           Two-sided p-value [b]
#> 10             Hazard Ratio       Hazard Ratio (95% CI) [c]
#> 11             Hazard Ratio             Zom 50mg vs Placebo
#> 12             Hazard Ratio            Zom 100mg vs Placebo
#>                zom_50mg            zom_100mg     placebo
#> 1                                                       
#> 2               4 (8.9)             8 (17.8)     1 (2.2)
#> 3             41 (91.1)            37 (82.2)   44 (97.8)
#> 4                                                       
#> 5  168.0 (152.4, 183.6) 168.0 (157.0, 179.0) NE (NE, NE)
#> 6  168.0 (155.0, 181.0) 168.0 (158.8, 177.2) NE (NE, NE)
#> 7           NE (NE, NE)          NE (NE, NE) NE (NE, NE)
#> 8                                                       
#> 9                 0.287                                 
#> 10                                                      
#> 11 1.520 (0.650, 3.570)                                 
#> 12                      2.140 (0.970, 4.700)            

# Render with section grouping and footnotes
spec <- tbl_tte |>
  fr_table() |>
  fr_cols(
    section   = fr_col(visible = FALSE),
    statistic = fr_col("", width = 3),
    zom_50mg  = fr_col("Zomerane 50mg"),
    zom_100mg = fr_col("Zomerane 100mg"),
    placebo   = fr_col("Placebo")
  ) |>
  fr_rows(group_by = "section", blank_after = "section") |>
  fr_footnotes(
    "[a] Kaplan-Meier estimate.",
    "[b] Two-sided log-rank test.",
    "[c] Cox proportional hazards model."
  )
```
