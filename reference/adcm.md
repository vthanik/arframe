# Concomitant Medications Analysis Dataset (ADCM)

Synthetic CDISC ADaM Concomitant Medications (ADCM) dataset from study
TFRM-2024-001. Contains one row per subject per concomitant medication.
Used for:

- **Concomitant medications table** (Table 14.4.1): subjects using each
  medication category and individual agent, by treatment arm

Medications reflect realistic co-morbidities in an elderly Alzheimer's
population: hypertension, diabetes, pain, GI protection, and supplements
are common. Medication use is independent of treatment assignment.

## Usage

``` r
adcm
```

## Format

A data frame with approximately 600 rows and 13 variables:

- STUDYID:

  Study identifier

- USUBJID:

  Unique subject identifier

- ARM:

  Planned treatment arm

- TRTA:

  Actual treatment arm

- TRTAN:

  Actual treatment numeric code

- AGE:

  Subject age

- SEX:

  Subject sex

- SAFFL:

  Safety population flag

- CMDECOD:

  Decoded medication name (WHO Drug Dictionary-style, uppercase), e.g.
  `"PARACETAMOL"`, `"ATORVASTATIN"`, `"VITAMIN D"`

- CMCAT:

  ATC-inspired medication category, e.g. `"ANALGESICS"`,
  `"ANTIHYPERTENSIVES"`, `"LIPID MODIFYING AGENTS"`, `"SUPPLEMENTS"`

- CMSTDT:

  Concomitant medication start date

- CMENDT:

  Concomitant medication end date (`NA` if ongoing at data cut)

- ONGOING:

  Logical; `TRUE` if medication was ongoing at data cut

## Source

Synthetic data generated in `data-raw/create_adam_datasets.R`.

## Examples

``` r
# Unique subjects per medication category (for conmed table)
tapply(adcm$USUBJID, adcm$CMCAT, function(x) length(unique(x)))
#>              ANALGESICS          ANTICOAGULANTS           ANTIDIABETICS 
#>                      74                      11                      40 
#>       ANTIHYPERTENSIVES             ANXIOLYTICS               DIURETICS 
#>                      94                      18                      30 
#> GASTROINTESTINAL AGENTS  LIPID MODIFYING AGENTS             SUPPLEMENTS 
#>                      41                      65                      88 
#>          THYROID AGENTS 
#>                      23 

# Most common individual medications
sort(table(adcm$CMDECOD), decreasing = TRUE)
#> 
#>        AMLODIPINE         VITAMIN D      ATORVASTATIN           ASPIRIN 
#>                56                54                45                42 
#>        LISINOPRIL       PARACETAMOL CALCIUM CARBONATE     MULTIVITAMINS 
#>                40                36                35                34 
#>        FUROSEMIDE         METFORMIN        METOPROLOL        OMEPRAZOLE 
#>                30                30                29                27 
#>       SIMVASTATIN     LEVOTHYROXINE          ATENOLOL         LORAZEPAM 
#>                25                23                18                18 
#>      PANTOPRAZOLE         IBUPROFEN         GLIPIZIDE          WARFARIN 
#>                18                14                12                11 

# Subjects on lipid-modifying agents, by arm
statins <- subset(adcm, CMCAT == "LIPID MODIFYING AGENTS")
table(statins$ARM)
#> 
#>        Placebo Zomerane 100mg  Zomerane 50mg 
#>             19             23             28 
```
