# ─────────────────────────────────────────────────────────────────────────────
# data-raw/create_adam_datasets.R
#
# Generates five synthetic CDISC ADaM datasets for use in arframe examples
# and documentation. All datasets share the same subjects and are internally
# consistent.
#
# Fictional study: TFRM-2024-001
#   Drug:        Zomerane (fictional anti-cholinesterase inhibitor)
#   Indication:  Mild-to-moderate Alzheimer's Disease
#   Design:      Randomized, double-blind, placebo-controlled, 24-week
#   Arms:        Placebo | Zomerane 50mg | Zomerane 100mg
#   N:           135 subjects (45 per arm, 5 sites)
#
# Datasets produced:
#   adsl  — Subject Level Analysis Dataset     (Table 14.1.1 demographics)
#   adae  — Adverse Events Analysis Dataset    (Table 14.3.1 AE by SOC/PT)
#   adtte — Time to Event Analysis Dataset     (Table 14.2.x time to event)
#   adcm  — Concomitant Medications Dataset    (Table 14.4.1 conmed)
#   advs  — Vital Signs Analysis Dataset       (Table 14.3.5 vital signs)
#
# Style: tidyverse (dplyr, tidyr, purrr) with base pipe |>
#
# Run once to regenerate:
#   source("data-raw/create_adam_datasets.R")
# ─────────────────────────────────────────────────────────────────────────────

library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(purrr)

set.seed(20240101)

# ── Study constants ──────────────────────────────────────────────────────────

STUDYID     <- "TFRM-2024-001"
N_PER_ARM   <- 45L
ARMS        <- c("Placebo", "Zomerane 50mg", "Zomerane 100mg")
ARM_DOSES   <- c(0L, 50L, 100L)
N_TOTAL     <- N_PER_ARM * length(ARMS)
STUDY_DAYS  <- 168L


# ══════════════════════════════════════════════════════════════════════════════
# 1. ADSL — Subject Level Analysis Dataset
# ══════════════════════════════════════════════════════════════════════════════

# ── Identifiers ──────────────────────────────────────────────────────────────

site_pool <- sprintf("%03d", 1:5)

adsl <- tibble(
  STUDYID = STUDYID,
  SITEID  = sample(site_pool, N_TOTAL, replace = TRUE),
  SUBJID  = sprintf("%04d", seq_len(N_TOTAL)),
  ARM     = rep(ARMS, each = N_PER_ARM),
  TRT01PN = rep(ARM_DOSES, each = N_PER_ARM)
) |>
  mutate(
    USUBJID = paste0("TFR-", SITEID, "-", SUBJID),
    TRT01P  = ARM,
    TRT01A  = ARM,
    TRT01AN = TRT01PN
  )

# ── Demographics ─────────────────────────────────────────────────────────────

adsl <- adsl |>
  mutate(
    AGE = as.integer(pmax(55L, pmin(88L, round(rnorm(n(), mean = 74, sd = 7))))),
    AGEGR1 = case_when(
      AGE < 65  ~ "<65",
      AGE <= 80 ~ "65-80",
      TRUE      ~ ">80"
    ),
    AGEGR1N = case_match(AGEGR1, "<65" ~ 1L, "65-80" ~ 2L, ">80" ~ 3L),
    AGEU = "YEARS",
    SEX  = sample(c("F", "M"), n(), replace = TRUE, prob = c(0.55, 0.45)),
    RACE = sample(
      c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN",
        "AMERICAN INDIAN OR ALASKA NATIVE"),
      n(), replace = TRUE, prob = c(0.76, 0.10, 0.12, 0.02)
    ),
    ETHNIC = sample(
      c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"),
      n(), replace = TRUE, prob = c(0.12, 0.88)
    ),
    COUNTRY = sample(
      c("USA", "GBR", "CAN", "DEU", "FRA"),
      n(), replace = TRUE, prob = c(0.38, 0.20, 0.17, 0.15, 0.10)
    ),
    HEIGHTBL = if_else(
      SEX == "M",
      pmax(158, pmin(193, round(rnorm(n(), 174, 7)))),
      pmax(148, pmin(180, round(rnorm(n(), 161, 6))))
    ),
    WEIGHTBL = round(pmax(45, pmin(115, rnorm(n(), 73, 13))), 1),
    BMIBL    = round(WEIGHTBL / (HEIGHTBL / 100)^2, 1)
  )

# ── Study dates & disposition ────────────────────────────────────────────────

disc_prob_map <- c(Placebo = 0.09, `Zomerane 50mg` = 0.13, `Zomerane 100mg` = 0.18)

disc_reasons <- c("Adverse Event", "Withdrew Consent", "Lost to Follow-up",
                   "Lack of Efficacy", "Physician Decision")
disc_reason_probs <- list(
  Placebo            = c(0.20, 0.40, 0.25, 0.10, 0.05),
  `Zomerane 50mg`    = c(0.45, 0.25, 0.15, 0.10, 0.05),
  `Zomerane 100mg`   = c(0.55, 0.20, 0.10, 0.10, 0.05)
)

rand_origin <- as.Date("2020-01-06")

adsl <- adsl |>
  mutate(
    TRTSDT  = rand_origin + sample(0:358, n(), replace = TRUE),
    DISCFL  = rbinom(n(), 1L, prob = disc_prob_map[ARM]),
    DCSREAS = map2_chr(DISCFL, ARM, \(fl, arm) {
      if (fl == 1L) sample(disc_reasons, 1L, prob = disc_reason_probs[[arm]])
      else NA_character_
    }),
    EOSSTT  = if_else(DISCFL == 0L, "COMPLETED", "DISCONTINUED"),
    disc_day = round(runif(n(), 0.20, 0.95) * (STUDY_DAYS - 1L)),
    TRTEDT  = if_else(
      DISCFL == 0L,
      TRTSDT + STUDY_DAYS - 1L,
      TRTSDT + as.integer(disc_day)
    ),
    TRTDURD = as.integer(TRTEDT - TRTSDT + 1L)
  )

# ── Population flags & disease characteristics ──────────────────────────────

adsl <- adsl |>
  mutate(
    SAFFL    = "Y",
    ITTFL    = "Y",
    EFFFL    = if_else(TRTDURD >= 14L, "Y", "N"),
    DURDIS   = round(runif(n(), 6, 60)),
    DURDSGR1 = if_else(DURDIS < 12, "<12", ">=12"),
    MMSEBL   = as.integer(pmax(10L, pmin(26L, round(rnorm(n(), 20, 4)))))
  ) |>
  select(-DISCFL, -disc_day) |>
  # Reorder columns to match CDISC convention

  select(
    STUDYID, USUBJID, SUBJID, SITEID, ARM, TRT01P, TRT01PN, TRT01A, TRT01AN,
    TRTSDT, TRTEDT, TRTDURD, AGE, AGEGR1, AGEGR1N, AGEU, SEX, RACE, ETHNIC,
    COUNTRY, HEIGHTBL, WEIGHTBL, BMIBL, MMSEBL, DURDIS, DURDSGR1,
    SAFFL, ITTFL, EFFFL, EOSSTT, DCSREAS
  ) |>
  as.data.frame()

cat("adsl: ", nrow(adsl), "rows x", ncol(adsl), "cols\n")
cat("  Arms:", paste(table(adsl$ARM), collapse = " / "), "\n")
cat("  Discontinued:", sum(adsl$EOSSTT == "DISCONTINUED"), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# 2. ADAE — Adverse Events Analysis Dataset
# ══════════════════════════════════════════════════════════════════════════════

# AE term catalog — realistic for cholinesterase inhibitor in elderly
ae_catalog <- tibble(
  AEBODSYS = c(
    # Gastrointestinal disorders (7 PTs — class effect, dose-related)
    rep("Gastrointestinal disorders", 7),
    # Nervous system disorders (6 PTs — class effect, dose-related)
    rep("Nervous system disorders", 6),
    # General disorders and administration site conditions (4 PTs)
    rep("General disorders and administration site conditions", 4),
    # Musculoskeletal and connective tissue disorders (4 PTs)
    rep("Musculoskeletal and connective tissue disorders", 4),
    # Infections and infestations (5 PTs)
    rep("Infections and infestations", 5),
    # Cardiac disorders (5 PTs)
    rep("Cardiac disorders", 5),
    # Skin and subcutaneous tissue disorders (5 PTs)
    rep("Skin and subcutaneous tissue disorders", 5),
    # Psychiatric disorders (5 PTs)
    rep("Psychiatric disorders", 5),
    # Respiratory, thoracic and mediastinal disorders (5 PTs)
    rep("Respiratory, thoracic and mediastinal disorders", 5),
    # Metabolism and nutrition disorders (4 PTs)
    rep("Metabolism and nutrition disorders", 4),
    # Eye disorders (4 PTs)
    rep("Eye disorders", 4),
    # Vascular disorders (4 PTs)
    rep("Vascular disorders", 4),
    # Renal and urinary disorders (4 PTs)
    rep("Renal and urinary disorders", 4),
    # Hepatobiliary disorders (3 PTs)
    rep("Hepatobiliary disorders", 3),
    # Ear and labyrinth disorders (3 PTs)
    rep("Ear and labyrinth disorders", 3),
    # Blood and lymphatic system disorders (3 PTs)
    rep("Blood and lymphatic system disorders", 3),
    # Reproductive system and breast disorders (3 PTs)
    rep("Reproductive system and breast disorders", 3),
    # Investigations (3 PTs)
    rep("Investigations", 3)
  ),
  AEDECOD = c(
    "Nausea", "Vomiting", "Diarrhoea", "Abdominal pain upper",
    "Constipation", "Dyspepsia", "Flatulence",
    "Headache", "Dizziness", "Somnolence", "Tremor", "Insomnia", "Paraesthesia",
    "Fatigue", "Peripheral oedema", "Asthenia", "Pyrexia",
    "Back pain", "Arthralgia", "Myalgia", "Pain in extremity",
    "Nasopharyngitis", "Urinary tract infection", "Upper respiratory tract infection",
    "Bronchitis", "Influenza",
    "Bradycardia", "Palpitations", "Tachycardia", "Atrial fibrillation", "Extrasystoles",
    "Hyperhidrosis", "Rash", "Pruritus", "Dermatitis", "Dry skin",
    "Anxiety", "Depression", "Agitation", "Confusional state", "Hallucination",
    "Cough", "Dyspnoea", "Epistaxis", "Rhinorrhoea", "Oropharyngeal pain",
    "Decreased appetite", "Hypokalaemia", "Dehydration", "Weight decreased",
    "Vision blurred", "Dry eye", "Conjunctivitis", "Lacrimation increased",
    "Hypertension", "Hypotension", "Flushing", "Hot flush",
    "Pollakiuria", "Incontinence", "Nocturia", "Dysuria",
    "Hepatic enzyme increased", "Cholelithiasis", "Hepatic steatosis",
    "Tinnitus", "Vertigo", "Ear pain",
    "Anaemia", "Leukopenia", "Thrombocytopenia",
    "Erectile dysfunction", "Gynaecomastia", "Menstrual disorder",
    "Blood creatinine increased", "Weight increased",
    "Alanine aminotransferase increased"
  ),
  freq_pbo = c(
    0.10, 0.07, 0.08, 0.06, 0.08, 0.07, 0.05,
    0.10, 0.08, 0.06, 0.05, 0.08, 0.05,
    0.11, 0.07, 0.06, 0.08,
    0.10, 0.08, 0.06, 0.05,
    0.13, 0.09, 0.10, 0.07, 0.06,
    0.06, 0.06, 0.05, 0.05, 0.05,
    0.07, 0.07, 0.06, 0.05, 0.05,
    0.08, 0.07, 0.09, 0.06, 0.05,
    0.08, 0.06, 0.05, 0.05, 0.06,
    0.07, 0.05, 0.05, 0.06,
    0.06, 0.05, 0.05, 0.05,
    0.09, 0.05, 0.05, 0.06,
    0.06, 0.05, 0.05, 0.05,
    0.05, 0.05, 0.05,
    0.06, 0.05, 0.05,
    0.05, 0.05, 0.05,
    0.05, 0.05, 0.05,
    0.05, 0.05, 0.05
  ),
  freq_low = c(
    0.18, 0.12, 0.12, 0.08, 0.10, 0.09, 0.07,
    0.14, 0.12, 0.10, 0.08, 0.10, 0.06,
    0.13, 0.09, 0.08, 0.10,
    0.10, 0.08, 0.08, 0.06,
    0.12, 0.09, 0.10, 0.07, 0.06,
    0.07, 0.07, 0.06, 0.05, 0.04,
    0.08, 0.08, 0.07, 0.06, 0.05,
    0.09, 0.08, 0.10, 0.07, 0.06,
    0.09, 0.06, 0.05, 0.05, 0.06,
    0.09, 0.06, 0.06, 0.07,
    0.06, 0.05, 0.05, 0.04,
    0.09, 0.06, 0.05, 0.06,
    0.07, 0.05, 0.05, 0.04,
    0.06, 0.04, 0.04,
    0.06, 0.05, 0.04,
    0.05, 0.04, 0.04,
    0.04, 0.04, 0.04,
    0.06, 0.05, 0.05
  ),
  freq_hi = c(
    0.25, 0.18, 0.15, 0.10, 0.12, 0.12, 0.08,
    0.18, 0.15, 0.12, 0.10, 0.12, 0.08,
    0.15, 0.10, 0.09, 0.12,
    0.09, 0.07, 0.09, 0.08,
    0.10, 0.08, 0.09, 0.07, 0.06,
    0.09, 0.08, 0.07, 0.07, 0.06,
    0.10, 0.09, 0.08, 0.07, 0.06,
    0.10, 0.09, 0.11, 0.08, 0.07,
    0.10, 0.07, 0.06, 0.06, 0.07,
    0.11, 0.07, 0.07, 0.08,
    0.07, 0.06, 0.06, 0.05,
    0.09, 0.07, 0.06, 0.07,
    0.08, 0.07, 0.06, 0.05,
    0.08, 0.06, 0.05,
    0.07, 0.06, 0.05,
    0.07, 0.05, 0.05,
    0.05, 0.05, 0.05,
    0.08, 0.07, 0.07
  )
)

# Build AE records using cross-join + vectorized filtering
adae <- adsl |>
  select(STUDYID, USUBJID, ARM, TRT01A, TRT01AN, AGE, SEX, RACE, SAFFL,
         TRTSDT, TRTDURD) |>
  cross_join(ae_catalog) |>
  mutate(
    freq = case_match(
      ARM,
      "Placebo"          ~ freq_pbo,
      "Zomerane 50mg"    ~ freq_low,
      "Zomerane 100mg"   ~ freq_hi
    ),
    occurs = runif(n()) < freq
  ) |>
  filter(occurs) |>
  mutate(
    TRTA  = ARM,
    TRTAN = TRT01AN,
    # Timing
    ASTDY = map_int(TRTDURD, \(d) sample.int(d, 1L)),
    ADURN = sample(1L:21L, n(), replace = TRUE),
    AENDY = pmin(ASTDY + ADURN - 1L, TRTDURD),
    ADURN = AENDY - ASTDY + 1L,
    ASTDT = TRTSDT + ASTDY - 1L,
    AENDT = TRTSDT + AENDY - 1L,
    # Severity: higher dose → more moderate/severe
    sev_probs = case_match(
      TRT01AN,
      0L   ~ list(c(0.72, 0.23, 0.05)),
      50L  ~ list(c(0.60, 0.30, 0.10)),
      100L ~ list(c(0.50, 0.35, 0.15))
    ),
    AESEV = map_chr(sev_probs, \(p) sample(c("MILD", "MODERATE", "SEVERE"), 1L, prob = p)),
    AESER = if_else(AESEV == "SEVERE" & runif(n()) < 0.30, "Y", "N"),
    # Causality: GI/nervous AEs more likely related in active arms
    gi_ns = AEBODSYS %in% c("Gastrointestinal disorders", "Nervous system disorders"),
    rel_probs = case_when(
      TRT01AN == 0L & gi_ns  ~ list(c(0.05, 0.10, 0.15, 0.70)),
      TRT01AN == 0L & !gi_ns ~ list(c(0.15, 0.15, 0.10, 0.60)),
      TRT01AN > 0L  & gi_ns  ~ list(c(0.10, 0.35, 0.40, 0.15)),
      TRT01AN > 0L  & !gi_ns ~ list(c(0.10, 0.20, 0.25, 0.45))
    ),
    AEREL = map_chr(rel_probs, \(p) sample(c("PROBABLE", "POSSIBLE", "REMOTE", "NONE"), 1L, prob = p)),
    # Outcome
    AEOUT = if_else(
      AESEV == "SEVERE" & AESER == "Y",
      sample(c("RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED"), n(), replace = TRUE, prob = c(0.70, 0.30)),
      sample(c("RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED"), n(), replace = TRUE, prob = c(0.92, 0.08))
    ),
    # Action taken with study treatment (new column)
    AEACN = case_when(
      AESER == "Y" ~ sample(c("DRUG WITHDRAWN", "DRUG INTERRUPTED"), n(), replace = TRUE, prob = c(0.60, 0.40)),
      AESEV == "SEVERE" ~ sample(c("DRUG INTERRUPTED", "DOSE REDUCED", "DOSE NOT CHANGED"), n(), replace = TRUE, prob = c(0.50, 0.30, 0.20)),
      AESEV == "MODERATE" ~ sample(c("DOSE NOT CHANGED", "DOSE REDUCED", "DRUG INTERRUPTED"), n(), replace = TRUE, prob = c(0.60, 0.25, 0.15)),
      TRUE ~ sample(c("DOSE NOT CHANGED", "NOT APPLICABLE"), n(), replace = TRUE, prob = c(0.80, 0.20))
    ),
    # Toxicity grade (new column) — mapped from severity
    AETOXGR = case_match(
      AESEV,
      "MILD"     ~ sample(c("1", "2"), n(), replace = TRUE, prob = c(0.80, 0.20)),
      "MODERATE" ~ sample(c("2", "3"), n(), replace = TRUE, prob = c(0.70, 0.30)),
      "SEVERE"   ~ sample(c("3", "4"), n(), replace = TRUE, prob = c(0.65, 0.35))
    ),
    TRTEMFL = "Y"
  ) |>
  select(
    STUDYID, USUBJID, ARM, TRTA, TRTAN, AGE, SEX, RACE, SAFFL,
    AEBODSYS, AEDECOD, AESEV, AETOXGR, AESER, AEREL, AEACN, AEOUT,
    ASTDT, AENDT, ASTDY, AENDY, ADURN, TRTEMFL
  ) |>
  arrange(USUBJID, ASTDY, AEBODSYS)

# Sequence number and first-occurrence flags
adae <- adae |>
  group_by(USUBJID) |>
  mutate(
    AESEQ   = row_number(),
    AOCCFL  = if_else(row_number() == 1L, "Y", NA_character_)
  ) |>
  group_by(USUBJID, AEBODSYS) |>
  mutate(AOCCSFL = if_else(row_number() == 1L, "Y", NA_character_)) |>
  group_by(USUBJID, AEDECOD) |>
  mutate(AOCCPFL = if_else(row_number() == 1L, "Y", NA_character_)) |>
  ungroup() |>
  as.data.frame()

cat("adae:", nrow(adae), "rows x", ncol(adae), "cols\n")
cat("  Subjects with AE:", n_distinct(adae$USUBJID), "/", N_TOTAL, "\n")
cat("  AEs by arm:\n")
print(table(adae$ARM))


# ══════════════════════════════════════════════════════════════════════════════
# 3. ADTTE — Time to Event Analysis Dataset
# ══════════════════════════════════════════════════════════════════════════════

# First AE day per subject (for TTAE parameter)
first_ae_day <- adae |>
  summarize(first_day = min(ASTDY), .by = USUBJID)

adtte <- adsl |>
  select(STUDYID, USUBJID, ARM, TRT01A, TRT01AN, AGE, SEX, SAFFL, ITTFL,
         TRTSDT, TRTEDT, TRTDURD, EOSSTT) |>
  # Create two rows per subject (TTWD + TTAE)
  cross_join(tibble(
    PARAMCD = c("TTWD", "TTAE"),
    PARAM   = c("Time to Study Withdrawal (Days)", "Time to First Adverse Event (Days)")
  )) |>
  left_join(first_ae_day, by = "USUBJID") |>
  mutate(
    # TTWD: event = discontinued, censored = completed
    # TTAE: event = had AE, censored = no AE by end
    AVAL = case_when(
      PARAMCD == "TTWD" ~ as.numeric(TRTDURD),
      PARAMCD == "TTAE" & !is.na(first_day) ~ as.numeric(first_day),
      PARAMCD == "TTAE" ~ as.numeric(TRTDURD)
    ),
    CNSR = case_when(
      PARAMCD == "TTWD" & EOSSTT == "DISCONTINUED" ~ 0L,
      PARAMCD == "TTWD" ~ 1L,
      PARAMCD == "TTAE" & !is.na(first_day) ~ 0L,
      PARAMCD == "TTAE" ~ 1L
    ),
    ADT = case_when(
      PARAMCD == "TTWD" ~ TRTEDT,
      PARAMCD == "TTAE" & !is.na(first_day) ~ TRTSDT + as.integer(first_day) - 1L,
      PARAMCD == "TTAE" ~ TRTEDT
    ),
    STARTDT = TRTSDT,
    ADY     = as.integer(AVAL)
  ) |>
  select(STUDYID, USUBJID, ARM, TRTA = TRT01A, TRTAN = TRT01AN,
         AGE, SEX, SAFFL, ITTFL, PARAMCD, PARAM, AVAL, CNSR, STARTDT, ADT, ADY) |>
  as.data.frame()

cat("adtte:", nrow(adtte), "rows x", ncol(adtte), "cols\n")
cat("  Events (TTWD):", sum(adtte$CNSR[adtte$PARAMCD == "TTWD"] == 0), "\n")
cat("  Events (TTAE):", sum(adtte$CNSR[adtte$PARAMCD == "TTAE"] == 0), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# 4. ADCM — Concomitant Medications Analysis Dataset
# ══════════════════════════════════════════════════════════════════════════════

cm_catalog <- tibble(
  CMDECOD = c(
    "PARACETAMOL", "IBUPROFEN", "ASPIRIN",
    "AMLODIPINE", "LISINOPRIL", "METOPROLOL", "ATENOLOL",
    "METFORMIN", "GLIPIZIDE",
    "OMEPRAZOLE", "PANTOPRAZOLE",
    "CALCIUM CARBONATE", "VITAMIN D", "MULTIVITAMINS",
    "WARFARIN", "ATORVASTATIN", "SIMVASTATIN",
    "LEVOTHYROXINE", "FUROSEMIDE", "LORAZEPAM"
  ),
  CMCAT = c(
    "ANALGESICS", "ANALGESICS", "ANALGESICS",
    "ANTIHYPERTENSIVES", "ANTIHYPERTENSIVES", "ANTIHYPERTENSIVES", "ANTIHYPERTENSIVES",
    "ANTIDIABETICS", "ANTIDIABETICS",
    "GASTROINTESTINAL AGENTS", "GASTROINTESTINAL AGENTS",
    "SUPPLEMENTS", "SUPPLEMENTS", "SUPPLEMENTS",
    "ANTICOAGULANTS", "LIPID MODIFYING AGENTS", "LIPID MODIFYING AGENTS",
    "THYROID AGENTS", "DIURETICS", "ANXIOLYTICS"
  ),
  prev = c(
    0.30, 0.10, 0.28,
    0.38, 0.32, 0.22, 0.14,
    0.18, 0.10,
    0.22, 0.14,
    0.30, 0.38, 0.28,
    0.08, 0.36, 0.18,
    0.14, 0.20, 0.12
  )
)

adcm <- adsl |>
  select(STUDYID, USUBJID, ARM, TRT01A, TRT01AN, AGE, SEX, SAFFL, TRTSDT, TRTDURD) |>
  cross_join(cm_catalog) |>
  mutate(occurs = runif(n()) < prev) |>
  filter(occurs) |>
  mutate(
    TRTA = ARM,
    TRTAN = TRT01AN,
    # Pre-existing (75%) or new conmed (25%)
    pre_existing = runif(n()) < 0.75,
    CMSTDT = if_else(
      pre_existing,
      TRTSDT - as.integer(sample(30L:730L, n(), replace = TRUE)),
      TRTSDT + map_int(TRTDURD, \(d) sample.int(as.integer(d), 1L))
    ),
    # Ongoing (60%) or resolved (40%)
    ONGOING = runif(n()) < 0.60,
    CMENDT  = if_else(ONGOING, as.Date(NA), CMSTDT + sample(14L:180L, n(), replace = TRUE))
  ) |>
  select(STUDYID, USUBJID, ARM, TRTA, TRTAN, AGE, SEX, SAFFL,
         CMDECOD, CMCAT, CMSTDT, CMENDT, ONGOING) |>
  arrange(USUBJID, CMCAT, CMDECOD) |>
  as.data.frame()

cat("adcm:", nrow(adcm), "rows x", ncol(adcm), "cols\n")
cat("  Subjects with conmed:", n_distinct(adcm$USUBJID), "/", N_TOTAL, "\n")
cat("  Conmeds by category:\n")
print(sort(table(adcm$CMCAT), decreasing = TRUE))


# ══════════════════════════════════════════════════════════════════════════════
# 5. ADVS — Vital Signs Analysis Dataset
# ══════════════════════════════════════════════════════════════════════════════

set.seed(20240301)  # Separate seed for backward-compatible N counts

params <- tribble(
  ~PARAM,                  ~PARAMCD, ~mean, ~sd, ~avail_pbo, ~avail_low, ~avail_hi, ~eff_pbo, ~eff_low, ~eff_hi,
  "Systolic BP (mmHg)",    "SYSBP",  135,   15,  1.00,       0.98,       0.96,       0,       -5,       -8,
  "Diastolic BP (mmHg)",   "DIABP",  85,    10,  1.00,       0.98,       0.96,       0,       -3,       -5,
  "Heart Rate (bpm)",      "HR",     75,    12,  0.93,       0.91,       0.87,       0,       -2,       -4,
  "Weight (kg)",           "WEIGHT", 78,    14,  0.96,       0.93,       0.89,       0,        0.5,      1.0,
  "Temperature (C)",       "TEMP",   36.6,  0.3, 0.89,       0.84,       0.80,       0,        0,        0
)

visits <- c("Baseline", "Week 12", "Week 24")

# Build records: for each subject × parameter, check availability then generate visits
advs <- adsl |>
  select(STUDYID, USUBJID, TRT01A, TRT01AN, SAFFL) |>
  cross_join(params) |>
  mutate(
    avail = case_match(
      TRT01A,
      "Placebo"          ~ avail_pbo,
      "Zomerane 50mg"    ~ avail_low,
      "Zomerane 100mg"   ~ avail_hi
    ),
    has_param = runif(n()) < avail,
    effect = case_match(
      TRT01A,
      "Placebo"          ~ eff_pbo,
      "Zomerane 50mg"    ~ eff_low,
      "Zomerane 100mg"   ~ eff_hi
    ),
    base_val = rnorm(n(), mean, sd)
  ) |>
  filter(has_param) |>
  # Expand to visits
  cross_join(tibble(AVISIT = visits, visit_idx = seq_along(visits))) |>
  mutate(
    TRTA  = TRT01A,
    TRTAN = TRT01AN,
    time_frac = visit_idx / length(visits),
    AVAL = if_else(
      AVISIT == "Baseline",
      round(base_val, 1),
      round(base_val + effect * time_frac + rnorm(n(), 0, sd * 0.1), 1)
    ),
    BASE  = round(base_val, 1),
    CHG   = if_else(AVISIT == "Baseline", NA_real_, round(AVAL - BASE, 1)),
    ABLFL = if_else(AVISIT == "Baseline", "Y", NA_character_)
  ) |>
  select(STUDYID, USUBJID, TRTA, TRTAN, SAFFL, PARAM, PARAMCD, AVISIT, AVAL, BASE, CHG, ABLFL) |>
  as.data.frame()

cat("\nadvs:", nrow(advs), "rows x", ncol(advs), "cols\n")
cat("  Subjects per param per arm:\n")
for (pname in unique(advs$PARAM)) {
  ns <- tapply(
    advs$USUBJID[advs$PARAM == pname & advs$AVISIT == "Baseline"],
    advs$TRTA[advs$PARAM == pname & advs$AVISIT == "Baseline"],
    \(x) length(unique(x))
  )
  cat("    ", pname, ":", paste(names(ns), ns, sep = "=", collapse = ", "), "\n")
}


# ══════════════════════════════════════════════════════════════════════════════
# Save to data/
# ══════════════════════════════════════════════════════════════════════════════

usethis::use_data(adsl, adae, adtte, adcm, advs, overwrite = TRUE)

cat("\nDone. All ADaM datasets saved to data/\n")
