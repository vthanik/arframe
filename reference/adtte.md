# Time to Event Analysis Dataset (ADTTE)

Synthetic CDISC ADaM Time-to-Event (ADTTE) dataset from study
TFRM-2024-001. Contains one row per subject per time-to-event parameter.
Used for:

- **Time to event table** (Table 14.2.1): Kaplan-Meier estimates, median
  time-to-event, and hazard ratios by treatment arm

Two parameters are included:

- `"TTWD"` — Time to Study Withdrawal (event = discontinued, censored =
  completed)

- `"TTAE"` — Time to First Adverse Event (event = first AE, censored =
  no AE)

## Usage

``` r
adtte
```

## Format

A data frame with 270 rows (135 subjects x 2 parameters) and 16
variables:

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

- ITTFL:

  Intent-to-treat population flag

- PARAMCD:

  Parameter code: `"TTWD"` or `"TTAE"`

- PARAM:

  Parameter label: `"Time to Study Withdrawal (Days)"` or
  `"Time to First Adverse Event (Days)"`

- AVAL:

  Analysis value — time in days from treatment start to event or
  censoring

- CNSR:

  Censoring indicator: `0` = event occurred, `1` = censored

- STARTDT:

  Treatment start date (used as reference/origin date)

- ADT:

  Date of event or censoring

- ADY:

  Relative day of event or censoring

## Source

Synthetic data generated in `data-raw/create_adam_datasets.R`.

## Examples

``` r
# Subset to one parameter
ttwd <- subset(adtte, PARAMCD == "TTWD")
table(ttwd$ARM, ttwd$CNSR)  # 0 = withdrew, 1 = completed
#>                 
#>                   0  1
#>   Placebo         1 44
#>   Zomerane 100mg  8 37
#>   Zomerane 50mg   4 41

# Median time to first AE by arm (unadjusted)
ttae <- subset(adtte, PARAMCD == "TTAE")
tapply(ttae$AVAL, ttae$ARM, median)
#>        Placebo Zomerane 100mg  Zomerane 50mg 
#>             24             13             16 
```
