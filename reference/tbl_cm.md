# Concomitant Medications Table (Table 14.4.1)

Pre-summarized concomitant medications table with category header rows
and indented medication rows. Structure mirrors
[tbl_ae_soc](https://vthanik.github.io/arframe/reference/tbl_ae_soc.md).

## Usage

``` r
tbl_cm
```

## Format

A data frame with columns:

- category:

  Medication category (structural column)

- medication:

  Display label: category name or indented drug name

- row_type:

  `"total"`, `"category"`, or `"drug"`

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
[adcm](https://vthanik.github.io/arframe/reference/adcm.md).

## Examples

``` r
head(tbl_cm, 10)
#>                      category                 medication row_type   placebo
#> 1  ANY CONCOMITANT MEDICATION ANY CONCOMITANT MEDICATION    total 44 (97.8)
#> 2           ANTIHYPERTENSIVES          ANTIHYPERTENSIVES category 31 (68.9)
#> 3           ANTIHYPERTENSIVES                 AMLODIPINE     drug 18 (40.0)
#> 4           ANTIHYPERTENSIVES                 LISINOPRIL     drug 12 (26.7)
#> 5           ANTIHYPERTENSIVES                 METOPROLOL     drug 12 (26.7)
#> 6           ANTIHYPERTENSIVES                   ATENOLOL     drug   4 (8.9)
#> 7                 SUPPLEMENTS                SUPPLEMENTS category 30 (66.7)
#> 8                 SUPPLEMENTS                  VITAMIN D     drug 19 (42.2)
#> 9                 SUPPLEMENTS          CALCIUM CARBONATE     drug 11 (24.4)
#> 10                SUPPLEMENTS              MULTIVITAMINS     drug 11 (24.4)
#>      zom_50mg  zom_100mg      total
#> 1  45 (100.0) 45 (100.0) 134 (99.3)
#> 2   26 (57.8)  37 (82.2)  94 (69.6)
#> 3   13 (28.9)  25 (55.6)  56 (41.5)
#> 4   13 (28.9)  15 (33.3)  40 (29.6)
#> 5    7 (15.6)  10 (22.2)  29 (21.5)
#> 6    5 (11.1)   9 (20.0)  18 (13.3)
#> 7   26 (57.8)  32 (71.1)  88 (65.2)
#> 8   15 (33.3)  20 (44.4)  54 (40.0)
#> 9    9 (20.0)  15 (33.3)  35 (25.9)
#> 10  10 (22.2)  13 (28.9)  34 (25.2)
```
