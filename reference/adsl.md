# Subject Level Analysis Dataset (ADSL)

Synthetic CDISC ADaM Subject Level (ADSL) dataset from fictional study
**TFRM-2024-001**, a 24-week randomized, double-blind,
placebo-controlled trial of Zomerane (a fictional anti-cholinesterase
inhibitor) in mild-to-moderate Alzheimer's Disease.

Contains one row per subject (N = 135). Used for:

- **Demographics table** (Table 14.1.1): age, sex, race, BMI by
  treatment arm

- **Disposition table** (Table 14.1.3): completion, discontinuation
  reasons

Variable names follow the CDISC ADaM Implementation Guide v1.3.

## Usage

``` r
adsl
```

## Format

A data frame with 135 rows and 31 variables:

- STUDYID:

  Study identifier (`"TFRM-2024-001"`)

- USUBJID:

  Unique subject identifier (`"TFR-NNN-NNNN"`)

- SUBJID:

  Subject identifier within study

- SITEID:

  Site identifier (`"001"` to `"005"`)

- ARM:

  Planned treatment arm: `"Placebo"`, `"Zomerane 50mg"`, or
  `"Zomerane 100mg"`

- TRT01P:

  Planned treatment (same as `ARM`)

- TRT01PN:

  Planned treatment numeric code: `0`, `50`, or `100`

- TRT01A:

  Actual treatment (same as `TRT01P` — no switches)

- TRT01AN:

  Actual treatment numeric code

- TRTSDT:

  Treatment start date

- TRTEDT:

  Treatment end date

- TRTDURD:

  Treatment duration in days

- AGE:

  Age in years at randomization (55–88)

- AGEGR1:

  Age group: `"<65"`, `"65-80"`, or `">80"`

- AGEGR1N:

  Age group numeric: 1, 2, or 3

- AGEU:

  Age units (`"YEARS"`)

- SEX:

  Sex: `"F"` (Female) or `"M"` (Male)

- RACE:

  Race (CDISC controlled terminology): `"WHITE"`,
  `"BLACK OR AFRICAN AMERICAN"`, `"ASIAN"`,
  `"AMERICAN INDIAN OR ALASKA NATIVE"`

- ETHNIC:

  Ethnicity: `"HISPANIC OR LATINO"` or `"NOT HISPANIC OR LATINO"`

- COUNTRY:

  Country of enrolment: `"USA"`, `"GBR"`, `"CAN"`, `"DEU"`, or `"FRA"`

- HEIGHTBL:

  Baseline height (cm)

- WEIGHTBL:

  Baseline weight (kg)

- BMIBL:

  Baseline BMI (kg/m squared)

- MMSEBL:

  Baseline Mini-Mental State Examination score (10–26); lower scores
  indicate greater cognitive impairment

- DURDIS:

  Disease duration at baseline (months)

- DURDSGR1:

  Disease duration group: `"<12"` or `">=12"` months

- SAFFL:

  Safety population flag: `"Y"` (all randomized subjects)

- ITTFL:

  Intent-to-treat population flag: `"Y"` (all randomized)

- EFFFL:

  Efficacy population flag: `"Y"` if treatment duration \>= 14 days

- EOSSTT:

  End of study status: `"COMPLETED"` or `"DISCONTINUED"`

- DCSREAS:

  Discontinuation reason (NA if completed): `"Adverse Event"`,
  `"Withdrew Consent"`, `"Lost to Follow-up"`, `"Lack of Efficacy"`, or
  `"Physician Decision"`

## Source

Synthetic data generated in `data-raw/create_adam_datasets.R`. No real
patient data. Follows CDISC ADaM ADSL specifications.

## Examples

``` r
# Quick look
head(adsl)
#>         STUDYID      USUBJID SUBJID SITEID     ARM  TRT01P TRT01PN  TRT01A
#> 1 TFRM-2024-001 TFR-001-0001   0001    001 Placebo Placebo       0 Placebo
#> 2 TFRM-2024-001 TFR-005-0002   0002    005 Placebo Placebo       0 Placebo
#> 3 TFRM-2024-001 TFR-001-0003   0003    001 Placebo Placebo       0 Placebo
#> 4 TFRM-2024-001 TFR-005-0004   0004    005 Placebo Placebo       0 Placebo
#> 5 TFRM-2024-001 TFR-003-0005   0005    003 Placebo Placebo       0 Placebo
#> 6 TFRM-2024-001 TFR-003-0006   0006    003 Placebo Placebo       0 Placebo
#>   TRT01AN     TRTSDT     TRTEDT TRTDURD AGE AGEGR1 AGEGR1N  AGEU SEX  RACE
#> 1       0 2020-12-24 2021-06-09     168  72  65-80       2 YEARS   M WHITE
#> 2       0 2020-01-25 2020-07-10     168  79  65-80       2 YEARS   F ASIAN
#> 3       0 2020-10-30 2021-04-15     168  69  65-80       2 YEARS   F WHITE
#> 4       0 2020-05-24 2020-11-07     168  85    >80       3 YEARS   F WHITE
#> 5       0 2020-12-21 2021-06-06     168  69  65-80       2 YEARS   F WHITE
#> 6       0 2020-04-03 2020-09-17     168  85    >80       3 YEARS   F WHITE
#>                   ETHNIC COUNTRY HEIGHTBL WEIGHTBL BMIBL MMSEBL DURDIS DURDSGR1
#> 1     HISPANIC OR LATINO     USA      175     80.9  26.4     15     29     >=12
#> 2 NOT HISPANIC OR LATINO     USA      176     64.6  20.9     22     24     >=12
#> 3     HISPANIC OR LATINO     USA      161     48.8  18.8     14     45     >=12
#> 4 NOT HISPANIC OR LATINO     DEU      171     68.8  23.5     21     26     >=12
#> 5 NOT HISPANIC OR LATINO     USA      159     71.9  28.4     20     37     >=12
#> 6 NOT HISPANIC OR LATINO     USA      162     69.8  26.6     16     15     >=12
#>   SAFFL ITTFL EFFFL    EOSSTT DCSREAS
#> 1     Y     Y     Y COMPLETED    <NA>
#> 2     Y     Y     Y COMPLETED    <NA>
#> 3     Y     Y     Y COMPLETED    <NA>
#> 4     Y     Y     Y COMPLETED    <NA>
#> 5     Y     Y     Y COMPLETED    <NA>
#> 6     Y     Y     Y COMPLETED    <NA>
table(adsl$ARM)
#> 
#>        Placebo Zomerane 100mg  Zomerane 50mg 
#>             45             45             45 
table(adsl$EOSSTT, adsl$ARM)
#>               
#>                Placebo Zomerane 100mg Zomerane 50mg
#>   COMPLETED         44             37            41
#>   DISCONTINUED       1              8             4

# Demographics table input
subset(adsl, SAFFL == "Y", select = c(ARM, AGE, SEX, RACE, BMIBL))
#>                ARM AGE SEX                             RACE BMIBL
#> 1          Placebo  72   M                            WHITE  26.4
#> 2          Placebo  79   F                            ASIAN  20.9
#> 3          Placebo  69   F                            WHITE  18.8
#> 4          Placebo  85   F                            WHITE  23.5
#> 5          Placebo  69   F                            WHITE  28.4
#> 6          Placebo  85   F                            WHITE  26.6
#> 7          Placebo  83   M                            WHITE  20.0
#> 8          Placebo  78   F                            WHITE  20.2
#> 9          Placebo  78   F                            WHITE  33.9
#> 10         Placebo  80   F                            WHITE  30.0
#> 11         Placebo  65   M                            WHITE  25.8
#> 12         Placebo  80   M                            WHITE  29.9
#> 13         Placebo  71   F                            ASIAN  21.4
#> 14         Placebo  69   F                            WHITE  28.8
#> 15         Placebo  65   F                            WHITE  30.2
#> 16         Placebo  86   M                            WHITE  24.1
#> 17         Placebo  78   F                            WHITE  42.3
#> 18         Placebo  65   F                            WHITE  30.4
#> 19         Placebo  78   M                            WHITE  24.1
#> 20         Placebo  72   F                            WHITE  30.4
#> 21         Placebo  72   M                            WHITE  20.2
#> 22         Placebo  77   F                            WHITE  30.5
#> 23         Placebo  75   F                            WHITE  30.2
#> 24         Placebo  82   M                            WHITE  25.8
#> 25         Placebo  70   M                            ASIAN  27.8
#> 26         Placebo  70   M                            ASIAN  30.3
#> 27         Placebo  88   F                            WHITE  31.8
#> 28         Placebo  88   M                            WHITE  29.3
#> 29         Placebo  67   M                            WHITE  30.8
#> 30         Placebo  74   F                            ASIAN  31.8
#> 31         Placebo  70   F                            WHITE  28.3
#> 32         Placebo  88   M                            WHITE  29.2
#> 33         Placebo  69   F                            WHITE  22.5
#> 34         Placebo  83   F                            WHITE  22.7
#> 35         Placebo  67   F                            WHITE  26.9
#> 36         Placebo  69   F                            WHITE  23.7
#> 37         Placebo  78   M                            ASIAN  30.5
#> 38         Placebo  67   F                            WHITE  25.6
#> 39         Placebo  71   M        BLACK OR AFRICAN AMERICAN  32.3
#> 40         Placebo  78   F                            WHITE  23.3
#> 41         Placebo  75   M                            WHITE  18.7
#> 42         Placebo  73   M                            WHITE  24.5
#> 43         Placebo  74   M                            WHITE  24.4
#> 44         Placebo  67   F                            WHITE  34.2
#> 45         Placebo  74   F                            WHITE  28.6
#> 46   Zomerane 50mg  70   F                            WHITE  25.3
#> 47   Zomerane 50mg  65   F                            WHITE  32.2
#> 48   Zomerane 50mg  81   M                            WHITE  25.1
#> 49   Zomerane 50mg  80   M                            WHITE  24.1
#> 50   Zomerane 50mg  80   M                            WHITE  22.9
#> 51   Zomerane 50mg  88   M                            WHITE  26.0
#> 52   Zomerane 50mg  69   F                            WHITE  29.1
#> 53   Zomerane 50mg  68   F                            WHITE  25.3
#> 54   Zomerane 50mg  75   M                            ASIAN  20.3
#> 55   Zomerane 50mg  71   F                            WHITE  28.4
#> 56   Zomerane 50mg  74   M AMERICAN INDIAN OR ALASKA NATIVE  25.2
#> 57   Zomerane 50mg  74   M        BLACK OR AFRICAN AMERICAN  30.6
#> 58   Zomerane 50mg  69   M                            ASIAN  20.1
#> 59   Zomerane 50mg  77   M                            WHITE  22.4
#> 60   Zomerane 50mg  70   F                            WHITE  33.9
#> 61   Zomerane 50mg  74   F                            WHITE  23.6
#> 62   Zomerane 50mg  57   M                            WHITE  23.8
#> 63   Zomerane 50mg  70   F                            WHITE  29.3
#> 64   Zomerane 50mg  74   F                            WHITE  22.3
#> 65   Zomerane 50mg  55   M                            WHITE  21.2
#> 66   Zomerane 50mg  55   F                            ASIAN  26.0
#> 67   Zomerane 50mg  77   F                            ASIAN  25.9
#> 68   Zomerane 50mg  74   F        BLACK OR AFRICAN AMERICAN  24.1
#> 69   Zomerane 50mg  88   M        BLACK OR AFRICAN AMERICAN  24.1
#> 70   Zomerane 50mg  82   F                            WHITE  18.9
#> 71   Zomerane 50mg  88   F        BLACK OR AFRICAN AMERICAN  26.1
#> 72   Zomerane 50mg  61   F                            WHITE  36.2
#> 73   Zomerane 50mg  70   F                            WHITE  22.2
#> 74   Zomerane 50mg  81   F                            ASIAN  23.8
#> 75   Zomerane 50mg  69   M        BLACK OR AFRICAN AMERICAN  25.7
#> 76   Zomerane 50mg  75   F                            ASIAN  31.8
#> 77   Zomerane 50mg  83   F                            WHITE  36.5
#> 78   Zomerane 50mg  64   F                            WHITE  25.9
#> 79   Zomerane 50mg  60   M                            WHITE  29.5
#> 80   Zomerane 50mg  86   M                            WHITE  32.9
#> 81   Zomerane 50mg  74   F                            WHITE  19.2
#> 82   Zomerane 50mg  60   F                            WHITE  29.0
#> 83   Zomerane 50mg  75   F                            WHITE  27.9
#> 84   Zomerane 50mg  68   F                            WHITE  31.4
#> 85   Zomerane 50mg  78   F                            WHITE  30.4
#> 86   Zomerane 50mg  77   F                            WHITE  25.2
#> 87   Zomerane 50mg  79   F                            WHITE  34.6
#> 88   Zomerane 50mg  81   M                            WHITE  28.0
#> 89   Zomerane 50mg  74   F                            WHITE  26.9
#> 90   Zomerane 50mg  69   M AMERICAN INDIAN OR ALASKA NATIVE  17.3
#> 91  Zomerane 100mg  83   F                            WHITE  27.3
#> 92  Zomerane 100mg  66   F        BLACK OR AFRICAN AMERICAN  33.8
#> 93  Zomerane 100mg  69   F                            WHITE  18.9
#> 94  Zomerane 100mg  72   M                            WHITE  23.9
#> 95  Zomerane 100mg  73   M                            WHITE  26.2
#> 96  Zomerane 100mg  74   F                            WHITE  24.6
#> 97  Zomerane 100mg  55   F                            WHITE  24.0
#> 98  Zomerane 100mg  85   M                            WHITE  21.0
#> 99  Zomerane 100mg  68   F                            ASIAN  18.6
#> 100 Zomerane 100mg  67   M                            WHITE  20.8
#> 101 Zomerane 100mg  77   M        BLACK OR AFRICAN AMERICAN  16.7
#> 102 Zomerane 100mg  67   M                            WHITE  29.8
#> 103 Zomerane 100mg  83   M                            WHITE  18.5
#> 104 Zomerane 100mg  83   M                            WHITE  21.9
#> 105 Zomerane 100mg  88   F                            WHITE  32.4
#> 106 Zomerane 100mg  73   F                            WHITE  29.7
#> 107 Zomerane 100mg  80   M                            WHITE  19.1
#> 108 Zomerane 100mg  88   F                            WHITE  19.3
#> 109 Zomerane 100mg  77   F                            WHITE  26.9
#> 110 Zomerane 100mg  67   M                            WHITE  25.0
#> 111 Zomerane 100mg  84   F                            WHITE  28.5
#> 112 Zomerane 100mg  83   M                            WHITE  16.8
#> 113 Zomerane 100mg  81   M                            WHITE  20.2
#> 114 Zomerane 100mg  81   M                            WHITE  14.2
#> 115 Zomerane 100mg  75   M        BLACK OR AFRICAN AMERICAN  20.8
#> 116 Zomerane 100mg  81   M        BLACK OR AFRICAN AMERICAN  28.5
#> 117 Zomerane 100mg  88   M                            WHITE  29.9
#> 118 Zomerane 100mg  71   F                            WHITE  20.9
#> 119 Zomerane 100mg  78   F                            ASIAN  24.6
#> 120 Zomerane 100mg  68   F                            WHITE  29.2
#> 121 Zomerane 100mg  72   F                            WHITE  26.6
#> 122 Zomerane 100mg  73   F                            WHITE  33.0
#> 123 Zomerane 100mg  82   F                            WHITE  24.8
#> 124 Zomerane 100mg  71   M                            WHITE  19.9
#> 125 Zomerane 100mg  71   M                            ASIAN  35.3
#> 126 Zomerane 100mg  73   M                            WHITE  27.8
#> 127 Zomerane 100mg  73   M                            ASIAN  20.7
#> 128 Zomerane 100mg  76   M                            WHITE  21.9
#> 129 Zomerane 100mg  65   F                            ASIAN  44.5
#> 130 Zomerane 100mg  73   M                            WHITE  26.2
#> 131 Zomerane 100mg  78   F        BLACK OR AFRICAN AMERICAN  24.7
#> 132 Zomerane 100mg  71   M                            WHITE  23.7
#> 133 Zomerane 100mg  73   F                            WHITE  32.4
#> 134 Zomerane 100mg  80   M                            WHITE  21.4
#> 135 Zomerane 100mg  71   M                            WHITE  24.6
```
