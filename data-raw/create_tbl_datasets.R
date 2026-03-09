# ─────────────────────────────────────────────────────────────────────────────
# data-raw/create_tbl_datasets.R
#
# Creates presentation-ready (TFL-ready) summary tables from the synthetic
# ADaM datasets produced by create_adam_datasets.R.
#
# Fictional study: TFRM-2024-001
#   Drug:   Zomerane (50mg | 100mg) vs Placebo
#   N:      135 subjects (45 per arm)
#
# Tables produced:
#   tbl_demog      — Table 14.1.5  Demographics and Baseline Characteristics
#   tbl_ae_soc     — Table 14.3.1  Adverse Events by SOC / Preferred Term
#   tbl_ae_summary — GS_CSR_AE_T_001  Overall AE Summary (NEW)
#   tbl_disp       — Table 14.1.3  Subject Disposition
#   tbl_tte        — Table 14.2.1  Time-to-Event Summary (REDESIGNED)
#   tbl_cm         — Table 14.4.1  Concomitant Medications
#   tbl_vs         — Table 14.3.5  Vital Signs Change from Baseline (REDESIGNED)
#
# Prerequisites:
#   Run create_adam_datasets.R first to produce data/*.rda
#
# Style: tidyverse (dplyr, tidyr, purrr) with base pipe |>
#
# Run once to regenerate:
#   source("data-raw/create_tbl_datasets.R")
# ─────────────────────────────────────────────────────────────────────────────

library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(purrr)

# ── Load ADaM datasets ──────────────────────────────────────────────────────

load("data/adsl.rda")
load("data/adae.rda")
load("data/adtte.rda")
load("data/adcm.rda")
load("data/advs.rda")


# ── Formatting helpers ───────────────────────────────────────────────────────

fmt_mean_sd <- function(x) sprintf("%.1f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
fmt_median  <- function(x) sprintf("%.1f", median(x, na.rm = TRUE))
fmt_minmax  <- function(x) sprintf("%.1f, %.1f", min(x, na.rm = TRUE), max(x, na.rm = TRUE))
fmt_n_pct   <- function(n, denom) sprintf("%d (%.1f)", as.integer(n), 100 * n / denom)
fmt_ci      <- function(est, lo, hi) sprintf("%.1f (%.1f, %.1f)", est, lo, hi)
fmt_hr_ci   <- function(hr, lo, hi) sprintf("%.3f (%.3f, %.3f)", hr, lo, hi)
fmt_pval    <- function(p) {
  if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
}

# Arms in display order (active first, placebo last — pharma convention)
arm_levels   <- c("Placebo", "Zomerane 50mg", "Zomerane 100mg")
arm_colnames <- c("placebo", "zom_50mg", "zom_100mg")

N_PER_ARM <- 45L
N_TOTAL   <- 135L


# ══════════════════════════════════════════════════════════════════════════════
# 1. tbl_demog — Demographics and Baseline Characteristics
# ══════════════════════════════════════════════════════════════════════════════

# Helper: summarize a continuous variable by arm → wide rows
summarize_continuous <- function(data, var, label) {
  var_sym <- rlang::sym(var)

  by_arm <- data |>
    summarize(
      mean_sd = fmt_mean_sd(!!var_sym),
      median  = fmt_median(!!var_sym),
      minmax  = fmt_minmax(!!var_sym),
      .by = ARM
    )

  total <- data |>
    summarize(
      mean_sd = fmt_mean_sd(!!var_sym),
      median  = fmt_median(!!var_sym),
      minmax  = fmt_minmax(!!var_sym)
    )

  make_wide <- function(stat_label, stat_col) {
    vals <- setNames(by_arm[[stat_col]], by_arm$ARM)
    tibble(
      characteristic = paste0("  ", stat_label),
      placebo   = vals[["Placebo"]],
      zom_50mg  = vals[["Zomerane 50mg"]],
      zom_100mg = vals[["Zomerane 100mg"]],
      total     = total[[stat_col]]
    )
  }

  bind_rows(
    tibble(characteristic = label, placebo = "", zom_50mg = "", zom_100mg = "", total = ""),
    make_wide("Mean (SD)", "mean_sd"),
    make_wide("Median", "median"),
    make_wide("Min, Max", "minmax")
  )
}

# Helper: summarize a categorical variable by arm → wide rows
summarize_categorical <- function(data, var, label, levels = NULL) {
  var_sym <- rlang::sym(var)
  if (is.null(levels)) levels <- sort(unique(data[[var]]))

  header <- tibble(characteristic = label, placebo = "", zom_50mg = "", zom_100mg = "", total = "")

  rows <- map(levels, \(lv) {
    by_arm <- data |>
      summarize(n = sum(!!var_sym == lv), .by = ARM)
    vals <- setNames(by_arm$n, by_arm$ARM)

    tibble(
      characteristic = paste0("  ", lv),
      placebo   = fmt_n_pct(vals[["Placebo"]],         N_PER_ARM),
      zom_50mg  = fmt_n_pct(vals[["Zomerane 50mg"]],   N_PER_ARM),
      zom_100mg = fmt_n_pct(vals[["Zomerane 100mg"]],  N_PER_ARM),
      total     = fmt_n_pct(sum(data[[var]] == lv),     N_TOTAL)
    )
  })

  bind_rows(header, rows)
}

# Build demographics table
n_row <- tibble(
  characteristic = "Subjects, n",
  placebo   = as.character(N_PER_ARM),
  zom_50mg  = as.character(N_PER_ARM),
  zom_100mg = as.character(N_PER_ARM),
  total     = as.character(N_TOTAL)
)

tbl_demog <- bind_rows(
  n_row |> mutate(group = "n"),
  summarize_continuous(adsl, "AGE", "Age (years)") |> mutate(group = "age_cont"),
  summarize_categorical(adsl, "AGEGR1", "Age Group, n (%)", c("<65", "65-80", ">80")) |>
    mutate(group = "age_cat"),
  summarize_categorical(adsl, "SEX", "Sex, n (%)", c("F", "M")) |>
    mutate(characteristic = case_match(characteristic,
      "  F" ~ "  Female", "  M" ~ "  Male", .default = characteristic
    )) |>
    mutate(group = "sex"),
  summarize_categorical(adsl, "RACE", "Race, n (%)",
    c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "AMERICAN INDIAN OR ALASKA NATIVE")) |>
    mutate(characteristic = case_match(characteristic,
      "  WHITE" ~ "  White",
      "  BLACK OR AFRICAN AMERICAN" ~ "  Black or African American",
      "  ASIAN" ~ "  Asian",
      "  AMERICAN INDIAN OR ALASKA NATIVE" ~ "  American Indian or Alaska Native",
      .default = characteristic
    )) |>
    mutate(group = "race"),
  summarize_continuous(adsl, "BMIBL", "BMI (kg/m2)") |> mutate(group = "bmi"),
  summarize_continuous(adsl, "MMSEBL", "MMSE Score at Baseline") |> mutate(group = "mmse"),
  summarize_categorical(adsl, "EOSSTT", "Study Completion, n (%)",
    c("COMPLETED", "DISCONTINUED")) |>
    mutate(characteristic = case_match(characteristic,
      "  COMPLETED" ~ "  Completed", "  DISCONTINUED" ~ "  Discontinued",
      .default = characteristic
    )) |>
    mutate(group = "completion")
) |>
  as.data.frame()

cat("tbl_demog:", nrow(tbl_demog), "rows x", ncol(tbl_demog), "cols\n")


# ══════════════════════════════════════════════════════════════════════════════
# 2. tbl_ae_soc — Adverse Events by System Organ Class and Preferred Term
# ══════════════════════════════════════════════════════════════════════════════

saf <- adsl |> filter(SAFFL == "Y")
ae  <- adae |> filter(SAFFL == "Y", TRTEMFL == "Y")

n_saf_df <- saf |> count(ARM, name = "n_saf")
n_saf <- setNames(n_saf_df$n_saf, n_saf_df$ARM)
n_saf_total <- nrow(saf)

# Count unique subjects for a given filter
ae_n_pct <- function(ae_data, arm_val = NULL, soc = NULL, pt = NULL) {
  sub <- ae_data
  if (!is.null(arm_val)) sub <- sub |> filter(ARM == arm_val)
  if (!is.null(soc))     sub <- sub |> filter(AEBODSYS == soc)
  if (!is.null(pt))      sub <- sub |> filter(AEDECOD == pt)
  n <- n_distinct(sub$USUBJID)
  denom <- if (!is.null(arm_val)) n_saf[[arm_val]] else n_saf_total
  fmt_n_pct(n, denom)
}

make_ae_row <- function(soc_val, pt_label, row_type, soc_filter = NULL, pt_filter = NULL) {
  pbo <- ae_n_pct(ae, "Placebo",         soc_filter, pt_filter)
  z50 <- ae_n_pct(ae, "Zomerane 50mg",   soc_filter, pt_filter)
  z10 <- ae_n_pct(ae, "Zomerane 100mg",  soc_filter, pt_filter)
  tot <- ae_n_pct(ae, NULL,               soc_filter, pt_filter)
  tibble(
    soc       = soc_val,
    pt        = pt_label,
    row_type  = row_type,
    placebo   = pbo,
    zom_50mg  = z50,
    zom_100mg = z10,
    total     = tot
  )
}

# Total row
ae_rows <- list(make_ae_row("SUBJECTS WITH >=1 TEAE", "SUBJECTS WITH >=1 TEAE", "total"))

# SOCs sorted by descending total incidence
soc_order <- ae |>
  summarize(n = n_distinct(USUBJID), .by = AEBODSYS) |>
  arrange(desc(n))

for (soc_name in soc_order$AEBODSYS) {
  ae_rows <- c(ae_rows, list(make_ae_row(soc_name, soc_name, "soc", soc_filter = soc_name)))

  pt_order <- ae |>
    filter(AEBODSYS == soc_name) |>
    summarize(n = n_distinct(USUBJID), .by = AEDECOD) |>
    arrange(desc(n))

  for (pt_name in pt_order$AEDECOD) {
    ae_rows <- c(ae_rows, list(
      make_ae_row(soc_name, pt_name, "pt", soc_filter = soc_name, pt_filter = pt_name)
    ))
  }
}

tbl_ae_soc <- bind_rows(ae_rows) |> as.data.frame()

cat("tbl_ae_soc:", nrow(tbl_ae_soc), "rows x", ncol(tbl_ae_soc), "cols\n")


# ══════════════════════════════════════════════════════════════════════════════
# 3. tbl_ae_summary — Overall AE Summary (GS_CSR_AE_T_001) — NEW
# ══════════════════════════════════════════════════════════════════════════════

# Helper: count subjects meeting a condition, per arm + total
ae_summary_row <- function(category, filter_fn) {
  per_arm <- ae |>
    filter(filter_fn(pick(everything()))) |>
    summarize(n = n_distinct(USUBJID), .by = ARM)

  # Ensure all arms present
  all_arms <- tibble(ARM = arm_levels) |>
    left_join(per_arm, by = "ARM") |>
    mutate(n = coalesce(n, 0L))

  n_total <- sum(all_arms$n)

  tibble(
    category  = category,
    zom_50mg  = fmt_n_pct(all_arms$n[all_arms$ARM == "Zomerane 50mg"],  N_PER_ARM),
    zom_100mg = fmt_n_pct(all_arms$n[all_arms$ARM == "Zomerane 100mg"], N_PER_ARM),
    placebo   = fmt_n_pct(all_arms$n[all_arms$ARM == "Placebo"],        N_PER_ARM),
    total     = fmt_n_pct(n_total, N_TOTAL)
  )
}

# Safety population row (uses adsl, not ae)
safety_pop <- tibble(
  category  = "Subjects in Safety Population",
  zom_50mg  = as.character(N_PER_ARM),
  zom_100mg = as.character(N_PER_ARM),
  placebo   = as.character(N_PER_ARM),
  total     = as.character(N_TOTAL)
)

# For max severity per subject, we need a subject-level summary
ae_subj <- ae |>
  mutate(sev_n = case_match(AESEV, "MILD" ~ 1L, "MODERATE" ~ 2L, "SEVERE" ~ 3L)) |>
  summarize(
    max_sev = max(sev_n),
    ARM = first(ARM),
    .by = USUBJID
  ) |>
  mutate(max_sev_label = case_match(max_sev, 1L ~ "Mild", 2L ~ "Moderate", 3L ~ "Severe"))

# Build the summary rows
tbl_ae_summary <- bind_rows(
  safety_pop,
  ae_summary_row("Subjects with at Least One TEAE", \(d) TRUE),
  ae_summary_row("  Related TEAE", \(d) d$AEREL %in% c("PROBABLE", "POSSIBLE")),
  ae_summary_row("  Serious TEAE", \(d) d$AESER == "Y"),
  ae_summary_row("  TEAE Leading to Discontinuation", \(d) d$AEACN == "DRUG WITHDRAWN"),
  ae_summary_row("  TEAE Leading to Death", \(d) rep(FALSE, nrow(d))),  # No deaths in study
  # Severity section header
  tibble(category = "Subjects with TEAE by Maximum Severity",
         zom_50mg = "", zom_100mg = "", placebo = "", total = ""),
  # Severity breakdown — from subject-level max severity
  map(c("Mild", "Moderate", "Severe"), \(sev) {
    per_arm <- ae_subj |>
      filter(max_sev_label == sev) |>
      count(ARM, name = "n")
    all_arms <- tibble(ARM = arm_levels) |>
      left_join(per_arm, by = "ARM") |>
      mutate(n = coalesce(n, 0L))
    n_total <- sum(all_arms$n)
    tibble(
      category  = paste0("  ", sev),
      zom_50mg  = fmt_n_pct(all_arms$n[all_arms$ARM == "Zomerane 50mg"],  N_PER_ARM),
      zom_100mg = fmt_n_pct(all_arms$n[all_arms$ARM == "Zomerane 100mg"], N_PER_ARM),
      placebo   = fmt_n_pct(all_arms$n[all_arms$ARM == "Placebo"],        N_PER_ARM),
      total     = fmt_n_pct(n_total, N_TOTAL)
    )
  }) |> bind_rows()
) |>
  as.data.frame()

cat("tbl_ae_summary:", nrow(tbl_ae_summary), "rows x", ncol(tbl_ae_summary), "cols\n")
print(tbl_ae_summary)


# ══════════════════════════════════════════════════════════════════════════════
# 4. tbl_disp — Subject Disposition
# ══════════════════════════════════════════════════════════════════════════════

disp_count <- function(data, arm_val, category) {
  sub <- data |> filter(ARM == arm_val)
  n <- switch(category,
    "Randomized"   = nrow(sub),
    "Completed"    = sum(sub$EOSSTT == "COMPLETED"),
    "Discontinued" = sum(sub$EOSSTT == "DISCONTINUED"),
    sum(sub$DCSREAS == category, na.rm = TRUE)
  )
  denom <- nrow(sub)
  if (category == "Randomized") as.character(n) else fmt_n_pct(n, denom)
}

disp_categories <- c("Randomized", "Completed", "Discontinued")
disp_reasons <- c("Adverse Event", "Withdrew Consent", "Lost to Follow-up",
                   "Lack of Efficacy", "Physician Decision")

disp_rows <- map(disp_categories, \(cat) {
  tibble(
    category  = cat,
    placebo   = disp_count(adsl, "Placebo", cat),
    zom_50mg  = disp_count(adsl, "Zomerane 50mg", cat),
    zom_100mg = disp_count(adsl, "Zomerane 100mg", cat),
    total     = if (cat == "Randomized") as.character(N_TOTAL)
                else fmt_n_pct(sum(adsl$EOSSTT == toupper(cat)), N_TOTAL)
  )
})

reason_rows <- map(disp_reasons, \(reason) {
  tibble(
    category  = paste0("  ", reason),
    placebo   = disp_count(adsl, "Placebo", reason),
    zom_50mg  = disp_count(adsl, "Zomerane 50mg", reason),
    zom_100mg = disp_count(adsl, "Zomerane 100mg", reason),
    total     = fmt_n_pct(sum(adsl$DCSREAS == reason, na.rm = TRUE), N_TOTAL)
  )
})

tbl_disp <- bind_rows(disp_rows, reason_rows) |> as.data.frame()

cat("tbl_disp:", nrow(tbl_disp), "rows x", ncol(tbl_disp), "cols\n")


# ══════════════════════════════════════════════════════════════════════════════
# 5. tbl_tte — Time-to-Event Summary (Table 14.2.1.1) — REDESIGNED
# ══════════════════════════════════════════════════════════════════════════════
#
# Reference shell: GS_CSR format with sections for event counts,
# KM percentile estimates with 95% CI, log-rank test, and hazard ratios.
#
# Statistics are pre-computed synthetically (no survival package needed).
# Values are realistic for a 24-week Alzheimer's trial.

ttwd <- adtte |> filter(PARAMCD == "TTWD")

# Per-arm event summary
arm_stats <- ttwd |>
  summarize(
    n       = n(),
    n_event = sum(CNSR == 0),
    n_cens  = sum(CNSR == 1),
    med     = median(AVAL),
    q25     = quantile(AVAL, 0.25),
    q75     = quantile(AVAL, 0.75),
    .by = ARM
  ) |>
  arrange(match(ARM, arm_levels))

# Synthetic KM-style percentile estimates with CIs
# (Based on actual data quantiles with plausible CI widths)
set.seed(42)  # Reproducible synthetic stats

make_tte_arm_stats <- function(stats_row) {
  n_e <- stats_row$n_event
  n_c <- stats_row$n_cens
  med <- stats_row$med
  q25 <- stats_row$q25
  q75 <- stats_row$q75

  # CI half-width scales with sqrt(n)
  ci_hw <- 26.0 / sqrt(max(n_e, 1))

  list(
    event_pct   = fmt_n_pct(n_e, stats_row$n),
    cens_pct    = fmt_n_pct(n_c, stats_row$n),
    q25_ci      = if (n_e >= 3) fmt_ci(q25, q25 - ci_hw * 1.2, q25 + ci_hw * 1.2) else "NE (NE, NE)",
    median_ci   = if (n_e >= 3) fmt_ci(med, med - ci_hw, med + ci_hw) else "NE (NE, NE)",
    q75_ci      = "NE (NE, NE)"  # Typically not reached in 24-week trial
  )
}

arm_tte <- map(seq_len(nrow(arm_stats)), \(i) make_tte_arm_stats(arm_stats[i, ]))
names(arm_tte) <- arm_stats$ARM

# Synthetic log-rank p-value and hazard ratios
# (Plausible values for modest treatment effect)
lr_pval <- 0.287
hr_50_vs_pbo  <- list(hr = 1.52, lo = 0.65, hi = 3.57)
hr_100_vs_pbo <- list(hr = 2.14, lo = 0.97, hi = 4.70)

# Build the table row by row matching reference shell
make_row <- function(section, statistic, zom_50mg = "", zom_100mg = "", placebo = "") {
  tibble(section = section, statistic = statistic,
         zom_50mg = zom_50mg, zom_100mg = zom_100mg, placebo = placebo)
}

tbl_tte <- bind_rows(
  # Section 1: Event counts
  make_row("Time to Study Withdrawal", "Time to Study Withdrawal"),
  make_row("Time to Study Withdrawal", "  Events, n (%)",
    arm_tte[["Zomerane 50mg"]]$event_pct,
    arm_tte[["Zomerane 100mg"]]$event_pct,
    arm_tte[["Placebo"]]$event_pct),
  make_row("Time to Study Withdrawal", "  Censored, n (%)",
    arm_tte[["Zomerane 50mg"]]$cens_pct,
    arm_tte[["Zomerane 100mg"]]$cens_pct,
    arm_tte[["Placebo"]]$cens_pct),
  # Section 2: KM percentile estimates
  make_row("KM Estimates", "Time to Study Withdrawal (Days)"),
  make_row("KM Estimates", "  25th Percentile (95% CI) [a]",
    arm_tte[["Zomerane 50mg"]]$q25_ci,
    arm_tte[["Zomerane 100mg"]]$q25_ci,
    arm_tte[["Placebo"]]$q25_ci),
  make_row("KM Estimates", "  Median (95% CI) [a]",
    arm_tte[["Zomerane 50mg"]]$median_ci,
    arm_tte[["Zomerane 100mg"]]$median_ci,
    arm_tte[["Placebo"]]$median_ci),
  make_row("KM Estimates", "  75th Percentile (95% CI) [a]",
    arm_tte[["Zomerane 50mg"]]$q75_ci,
    arm_tte[["Zomerane 100mg"]]$q75_ci,
    arm_tte[["Placebo"]]$q75_ci),
  # Section 3: Log-rank test
  make_row("Log-Rank Test", "Log-Rank Test"),
  make_row("Log-Rank Test", "  Two-sided p-value [b]",
    zom_50mg = fmt_pval(lr_pval)),
  # Section 4: Hazard ratios
  make_row("Hazard Ratio", "Hazard Ratio (95% CI) [c]"),
  make_row("Hazard Ratio", "  Zom 50mg vs Placebo",
    zom_50mg = fmt_hr_ci(hr_50_vs_pbo$hr, hr_50_vs_pbo$lo, hr_50_vs_pbo$hi)),
  make_row("Hazard Ratio", "  Zom 100mg vs Placebo",
    zom_100mg = fmt_hr_ci(hr_100_vs_pbo$hr, hr_100_vs_pbo$lo, hr_100_vs_pbo$hi))
) |>
  as.data.frame()

cat("tbl_tte:", nrow(tbl_tte), "rows x", ncol(tbl_tte), "cols\n")
print(tbl_tte)


# ══════════════════════════════════════════════════════════════════════════════
# 6. tbl_cm — Concomitant Medications by Category and Agent
# ══════════════════════════════════════════════════════════════════════════════

cm_saf <- adcm |> filter(SAFFL == "Y")

cm_n_pct <- function(cm_data, arm_val = NULL, cat = NULL, drug = NULL) {
  sub <- cm_data
  if (!is.null(arm_val)) sub <- sub |> filter(ARM == arm_val)
  if (!is.null(cat))     sub <- sub |> filter(CMCAT == cat)
  if (!is.null(drug))    sub <- sub |> filter(CMDECOD == drug)
  n <- n_distinct(sub$USUBJID)
  denom <- if (!is.null(arm_val)) n_saf[[arm_val]] else n_saf_total
  fmt_n_pct(n, denom)
}

make_cm_row <- function(cat_val, drug_label, row_type, cat_filter = NULL, drug_filter = NULL) {
  pbo <- cm_n_pct(cm_saf, "Placebo",         cat_filter, drug_filter)
  z50 <- cm_n_pct(cm_saf, "Zomerane 50mg",   cat_filter, drug_filter)
  z10 <- cm_n_pct(cm_saf, "Zomerane 100mg",  cat_filter, drug_filter)
  tot <- cm_n_pct(cm_saf, NULL,               cat_filter, drug_filter)
  tibble(
    category   = cat_val,
    medication = drug_label,
    row_type   = row_type,
    placebo    = pbo,
    zom_50mg   = z50,
    zom_100mg  = z10,
    total      = tot
  )
}

cm_rows <- list(make_cm_row("ANY CONCOMITANT MEDICATION", "ANY CONCOMITANT MEDICATION", "total"))

cat_order <- cm_saf |>
  summarize(n = n_distinct(USUBJID), .by = CMCAT) |>
  arrange(desc(n))

for (cat_name in cat_order$CMCAT) {
  cm_rows <- c(cm_rows, list(make_cm_row(cat_name, cat_name, "category", cat_filter = cat_name)))
  drug_order <- cm_saf |>
    filter(CMCAT == cat_name) |>
    summarize(n = n_distinct(USUBJID), .by = CMDECOD) |>
    arrange(desc(n))
  for (drug_name in drug_order$CMDECOD) {
    cm_rows <- c(cm_rows, list(
      make_cm_row(cat_name, paste0("  ", drug_name), "drug", cat_filter = cat_name, drug_filter = drug_name)
    ))
  }
}

tbl_cm <- bind_rows(cm_rows) |> as.data.frame()

cat("tbl_cm:", nrow(tbl_cm), "rows x", ncol(tbl_cm), "cols\n")


# ══════════════════════════════════════════════════════════════════════════════
# 7. tbl_vs — Vital Signs Change from Baseline (Table 14.3.5.1) — REDESIGNED
# ══════════════════════════════════════════════════════════════════════════════
#
# Wide format with spanning headers: per arm, three sub-columns
# (Baseline, Value, Change from Baseline). Multiple timepoints.
#
# Demonstrates: page_by, fr_spans (two-level spanning), fr_header(n=function),
# col_split for wide layouts.

arms_vs    <- c("Placebo", "Zomerane 50mg", "Zomerane 100mg")
arm_pfx    <- c("placebo", "zom_50mg", "zom_100mg")
visits_vs  <- c("Baseline", "Week 12", "Week 24")
stats      <- c("n", "Mean (SD)", "Median", "Min, Max")

# Build wide summary for each parameter × timepoint × arm
vs_rows <- list()

for (pname in unique(advs$PARAM)) {
  for (visit in visits_vs) {
    for (stat in stats) {
      row <- tibble(param = pname, timepoint = visit, statistic = stat)

      for (a in seq_along(arms_vs)) {
        pfx <- arm_pfx[a]
        pdata <- advs |> filter(PARAM == pname, TRTA == arms_vs[a])

        # Baseline column: always from Baseline visit
        base_vals <- pdata |> filter(AVISIT == "Baseline") |> pull(AVAL)
        # Value column: from current visit
        val_vals  <- pdata |> filter(AVISIT == visit) |> pull(AVAL)
        # Change column: from current visit
        chg_vals  <- pdata |> filter(AVISIT == visit) |> pull(CHG)

        base_str <- switch(stat,
          "n"         = as.character(length(base_vals[!is.na(base_vals)])),
          "Mean (SD)" = fmt_mean_sd(base_vals),
          "Median"    = fmt_median(base_vals),
          "Min, Max"  = fmt_minmax(base_vals)
        )

        val_str <- switch(stat,
          "n"         = as.character(length(val_vals[!is.na(val_vals)])),
          "Mean (SD)" = fmt_mean_sd(val_vals),
          "Median"    = fmt_median(val_vals),
          "Min, Max"  = fmt_minmax(val_vals)
        )

        # Change from baseline: blank at Baseline visit
        if (visit == "Baseline") {
          chg_str <- ""
        } else {
          chg_clean <- chg_vals[!is.na(chg_vals)]
          chg_str <- switch(stat,
            "n"         = as.character(length(chg_clean)),
            "Mean (SD)" = fmt_mean_sd(chg_clean),
            "Median"    = fmt_median(chg_clean),
            "Min, Max"  = fmt_minmax(chg_clean)
          )
        }

        row[[paste0(pfx, "_base")]]  <- base_str
        row[[paste0(pfx, "_value")]] <- val_str
        row[[paste0(pfx, "_chg")]]   <- chg_str
      }

      vs_rows <- c(vs_rows, list(row))
    }
  }
}

tbl_vs <- bind_rows(vs_rows) |> as.data.frame()

cat("tbl_vs:", nrow(tbl_vs), "rows x", ncol(tbl_vs), "cols\n")
cat("  Parameters:", paste(unique(tbl_vs$param), collapse = ", "), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# Save all to data/
# ══════════════════════════════════════════════════════════════════════════════

usethis::use_data(tbl_demog, tbl_ae_soc, tbl_ae_summary, tbl_disp, tbl_tte, tbl_cm, tbl_vs,
                  overwrite = TRUE)

cat("\nDone. All TBL datasets saved to data/\n")
