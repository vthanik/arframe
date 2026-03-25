# Adverse Events by System Organ Class Table (Table 14.3.1.2)

Pre-summarized adverse event table with SOC header rows and indented PT
rows. Columns are treatment arms showing n (%) of subjects.

## Usage

``` r
tbl_ae_soc
```

## Format

A data frame with columns:

- soc:

  System Organ Class name (structural column)

- pt:

  Display label: SOC name for header rows, indented PT for detail rows

- row_type:

  `"total"`, `"soc"`, or `"pt"`

- placebo:

  Placebo arm n (%)

- zom_50mg:

  Zomerane 50mg arm n (%)

- zom_100mg:

  Zomerane 100mg arm n (%)

- total:

  All subjects n (%)

## Source

Synthetic data generated in `data-raw/create_tbl_datasets.R` from
[adae](https://vthanik.github.io/arframe/reference/adae.md).

## Examples

``` r
head(tbl_ae_soc, 10)
#>                           soc                         pt row_type   placebo
#> 1      SUBJECTS WITH >=1 TEAE     SUBJECTS WITH >=1 TEAE    total 44 (97.8)
#> 2  Gastrointestinal disorders Gastrointestinal disorders      soc 17 (37.8)
#> 3  Gastrointestinal disorders                     Nausea       pt  5 (11.1)
#> 4  Gastrointestinal disorders                   Vomiting       pt   1 (2.2)
#> 5  Gastrointestinal disorders                  Diarrhoea       pt   2 (4.4)
#> 6  Gastrointestinal disorders                 Flatulence       pt  5 (11.1)
#> 7  Gastrointestinal disorders       Abdominal pain upper       pt   4 (8.9)
#> 8  Gastrointestinal disorders                  Dyspepsia       pt   1 (2.2)
#> 9  Gastrointestinal disorders               Constipation       pt   1 (2.2)
#> 10   Nervous system disorders   Nervous system disorders      soc 14 (31.1)
#>     zom_50mg  zom_100mg      total
#> 1  44 (97.8) 45 (100.0) 133 (98.5)
#> 2  28 (62.2)  27 (60.0)  72 (53.3)
#> 3  10 (22.2)   9 (20.0)  24 (17.8)
#> 4   6 (13.3)  11 (24.4)  18 (13.3)
#> 5   7 (15.6)   8 (17.8)  17 (12.6)
#> 6   5 (11.1)    3 (6.7)   13 (9.6)
#> 7    2 (4.4)   5 (11.1)   11 (8.1)
#> 8    4 (8.9)    2 (4.4)    7 (5.2)
#> 9    2 (4.4)    3 (6.7)    6 (4.4)
#> 10 24 (53.3)  26 (57.8)  64 (47.4)
```
