# Subject Disposition Table (Table 14.1.3)

Pre-summarized disposition table showing randomized, completed, and
discontinued counts with discontinuation reasons as indented sub-rows.

## Usage

``` r
tbl_disp
```

## Format

A data frame with 8 rows and 5 columns:

- category:

  Row label (e.g. `"Randomized"`, `" Adverse Event"`)

- placebo:

  Placebo arm n or n (%)

- zom_50mg:

  Zomerane 50mg arm n or n (%)

- zom_100mg:

  Zomerane 100mg arm n or n (%)

- total:

  All subjects n or n (%)

## Source

Synthetic data generated in `data-raw/create_tbl_datasets.R` from
[adsl](https://vthanik.github.io/arframe/reference/adsl.md).

## Examples

``` r
tbl_disp
#>               category   placebo  zom_50mg zom_100mg      total
#> 1           Randomized        45        45        45        135
#> 2            Completed 44 (97.8) 41 (91.1) 37 (82.2) 122 (90.4)
#> 3         Discontinued   1 (2.2)   4 (8.9)  8 (17.8)   13 (9.6)
#> 4        Adverse Event   1 (2.2)   2 (4.4)   3 (6.7)    6 (4.4)
#> 5     Withdrew Consent         0   1 (2.2)   3 (6.7)    4 (3.0)
#> 6    Lost to Follow-up         0         0         0          0
#> 7     Lack of Efficacy         0   1 (2.2)   2 (4.4)    3 (2.2)
#> 8   Physician Decision         0         0         0          0
```
