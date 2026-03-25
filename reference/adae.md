# Adverse Events Analysis Dataset (ADAE)

Synthetic CDISC ADaM Adverse Events (ADAE) dataset from study
TFRM-2024-001. Contains one row per subject per adverse event. Used for:

- **AE by SOC/PT table** (Table 14.3.1): incidence by body system and
  preferred term, within treatment arm

- **Overall AE summary**: subjects with TEAEs by seriousness,
  relatedness, severity, and action taken

All adverse events in this dataset are treatment-emergent
(`TRTEMFL = "Y"`). Gastrointestinal and nervous system events are
dose-related, reflecting the cholinergic class effect of Zomerane.

## Usage

``` r
adae
```

## Format

A data frame with approximately 750 rows and 27 variables:

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

  Subject age at time of event

- SEX:

  Subject sex

- RACE:

  Subject race

- SAFFL:

  Safety population flag

- AEBODSYS:

  MedDRA System Organ Class (SOC), e.g. `"Gastrointestinal disorders"`,
  `"Nervous system disorders"`

- AEDECOD:

  MedDRA Preferred Term (PT), e.g. `"Nausea"`, `"Dizziness"`

- AESEV:

  Severity: `"MILD"`, `"MODERATE"`, or `"SEVERE"`

- AETOXGR:

  CTCAE toxicity grade: `"1"`, `"2"`, `"3"`, or `"4"`. Derived from
  AESEV: MILD maps to 1–2, MODERATE to 2–3, SEVERE to 3–4

- AESER:

  Serious adverse event flag: `"Y"` or `"N"`

- AEREL:

  Relationship to study drug: `"PROBABLE"`, `"POSSIBLE"`, `"REMOTE"`, or
  `"NONE"`

- AEACN:

  Action taken with study treatment: `"DOSE NOT CHANGED"`,
  `"DRUG INTERRUPTED"`, `"DRUG WITHDRAWN"`, `"DOSE REDUCED"`, or
  `"NOT APPLICABLE"`

- AEOUT:

  Outcome: `"RECOVERED/RESOLVED"` or `"NOT RECOVERED/NOT RESOLVED"`

- ASTDT:

  AE start date

- AENDT:

  AE end date

- ASTDY:

  AE start day relative to treatment start

- AENDY:

  AE end day relative to treatment start

- ADURN:

  AE duration in days

- TRTEMFL:

  Treatment-emergent flag (`"Y"` for all records)

- AESEQ:

  AE sequence number within subject

- AOCCFL:

  First-occurrence flag (any AE, per subject): `"Y"` or `NA`

- AOCCSFL:

  First SOC occurrence flag (per subject per SOC): `"Y"` or `NA`

- AOCCPFL:

  First PT occurrence flag (per subject per PT): `"Y"` or `NA`

## Source

Synthetic data generated in `data-raw/create_adam_datasets.R`.

## Examples

``` r
# Subjects with at least one treatment-emergent AE
te_aes <- subset(adae, TRTEMFL == "Y" & !is.na(AOCCFL))
table(te_aes$ARM)
#> 
#>        Placebo Zomerane 100mg  Zomerane 50mg 
#>             44             45             44 

# AEs by body system
sort(table(adae$AEBODSYS), decreasing = TRUE)
#> 
#>                           Gastrointestinal disorders 
#>                                                   96 
#>                             Nervous system disorders 
#>                                                   80 
#>                          Infections and infestations 
#>                                                   61 
#> General disorders and administration site conditions 
#>                                                   53 
#>      Respiratory, thoracic and mediastinal disorders 
#>                                                   47 
#>      Musculoskeletal and connective tissue disorders 
#>                                                   45 
#>                                   Vascular disorders 
#>                                                   45 
#>               Skin and subcutaneous tissue disorders 
#>                                                   43 
#>                                Psychiatric disorders 
#>                                                   42 
#>                                    Cardiac disorders 
#>                                                   41 
#>                   Metabolism and nutrition disorders 
#>                                                   38 
#>                                       Investigations 
#>                                                   27 
#>                          Renal and urinary disorders 
#>                                                   27 
#>             Reproductive system and breast disorders 
#>                                                   27 
#>                                        Eye disorders 
#>                                                   26 
#>                          Ear and labyrinth disorders 
#>                                                   22 
#>                 Blood and lymphatic system disorders 
#>                                                   19 
#>                              Hepatobiliary disorders 
#>                                                   19 

# Serious AEs only
subset(adae, AESER == "Y", select = c(USUBJID, ARM, AEDECOD, AESEV))
#>          USUBJID            ARM                    AEDECOD  AESEV
#> 37  TFR-001-0042        Placebo                    Anxiety SEVERE
#> 82  TFR-001-0068  Zomerane 50mg                  Epistaxis SEVERE
#> 145 TFR-001-0131 Zomerane 100mg                  Hot flush SEVERE
#> 154 TFR-002-0026        Placebo               Hypertension SEVERE
#> 186 TFR-002-0043        Placebo                    Anxiety SEVERE
#> 209 TFR-002-0071  Zomerane 50mg         Oropharyngeal pain SEVERE
#> 215 TFR-002-0075  Zomerane 50mg                    Anxiety SEVERE
#> 220 TFR-002-0080  Zomerane 50mg                   Nocturia SEVERE
#> 255 TFR-002-0090  Zomerane 50mg           Weight decreased SEVERE
#> 257 TFR-002-0090  Zomerane 50mg                    Dry eye SEVERE
#> 259 TFR-002-0091 Zomerane 100mg                       Rash SEVERE
#> 264 TFR-002-0093 Zomerane 100mg                   Asthenia SEVERE
#> 272 TFR-002-0093 Zomerane 100mg               Hypertension SEVERE
#> 289 TFR-002-0099 Zomerane 100mg                   Asthenia SEVERE
#> 369 TFR-003-0050  Zomerane 50mg                   Vomiting SEVERE
#> 416 TFR-003-0092 Zomerane 100mg                      Cough SEVERE
#> 418 TFR-003-0092 Zomerane 100mg                Bradycardia SEVERE
#> 458 TFR-003-0116 Zomerane 100mg         Oropharyngeal pain SEVERE
#> 481 TFR-003-0130 Zomerane 100mg                     Tremor SEVERE
#> 482 TFR-003-0130 Zomerane 100mg                      Cough SEVERE
#> 545 TFR-004-0059  Zomerane 50mg                 Somnolence SEVERE
#> 586 TFR-004-0102 Zomerane 100mg               Hypokalaemia SEVERE
#> 589 TFR-004-0102 Zomerane 100mg                       Rash SEVERE
#> 593 TFR-004-0111 Zomerane 100mg    Urinary tract infection SEVERE
#> 599 TFR-004-0113 Zomerane 100mg                       Rash SEVERE
#> 622 TFR-005-0004        Placebo Blood creatinine increased SEVERE
#> 639 TFR-005-0020        Placebo                Rhinorrhoea SEVERE
#> 666 TFR-005-0079  Zomerane 50mg                  Diarrhoea SEVERE
#> 699 TFR-005-0100 Zomerane 100mg               Hypokalaemia SEVERE
#> 704 TFR-005-0103 Zomerane 100mg                 Somnolence SEVERE
#> 707 TFR-005-0103 Zomerane 100mg          Confusional state SEVERE
#> 709 TFR-005-0103 Zomerane 100mg   Hepatic enzyme increased SEVERE
#> 715 TFR-005-0105 Zomerane 100mg        Atrial fibrillation SEVERE
#> 730 TFR-005-0122 Zomerane 100mg                    Dysuria SEVERE
#> 733 TFR-005-0124 Zomerane 100mg                 Flatulence SEVERE
#> 755 TFR-005-0133 Zomerane 100mg                  Dyspepsia SEVERE

# Action taken distribution
table(adae$AEACN)
#> 
#> DOSE NOT CHANGED     DOSE REDUCED DRUG INTERRUPTED   DRUG WITHDRAWN 
#>              503               72               74               23 
#>   NOT APPLICABLE 
#>               86 
```
