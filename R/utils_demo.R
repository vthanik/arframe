# A tiny bundled demo catalog: ADSL/ADVS/ADTTE/ADAE, written to parquet and
# registered against a fresh arpillar catalog. Shared by the store tests, the
# later shinytest2 apps, and screenshots -- one fixture builder, not a copy
# per surface.

#' A small demo catalog (ADSL/ADVS/ADTTE/ADAE), opened and registered
#'
#' Writes four minimal analysis-ready datasets to parquet under `dir` and
#' registers each against a freshly opened [arpillar::engine_open()] catalog.
#' The caller owns the returned catalog's lifecycle -- close it with
#' [arpillar::engine_close()].
#' @param dir *Where to write the parquet files.* `<character(1)>: default
#'   tempdir()`.
#' @return *`<catalog>`.* An open, registered arpillar catalog with `ADSL`,
#'   `ADVS`, `ADTTE`, and `ADAE` in the `WORK` library.
#' @noRd
.demo_catalog <- function(dir = tempdir()) {
  # Deterministic: every consumer (store tests, shinytest2 apps, screenshots)
  # must see identical values so previews and snapshots are stable run to run.
  withr::local_seed(20260702L)
  con <- arpillar::engine_open()

  adsl <- data.frame(
    USUBJID = sprintf("SUBJ-%03d", 1:12),
    TRT01P = rep(c("Placebo", "Xanomeline"), each = 6L),
    AGE = c(63L, 71L, 58L, 66L, 74L, 61L, 69L, 55L, 72L, 64L, 60L, 68L),
    SEX = rep(c("M", "F"), 6L),
    SAFFL = rep("Y", 12L),
    stringsAsFactors = FALSE
  )

  # RACE/DISCFL/DCDECOD/EXDOSE: fixed lookups (never RNG), matching the
  # ADAE AEBODSYS/AEDECOD pattern below -- ADSL itself never calls
  # rnorm()/runif(), so these only need to stay deterministic themselves,
  # not sequence-safe against a stream. Coverage for the demographics/
  # disposition/exposure presets (arpillar::preset()) so they are fully
  # role-covered on the demo ADSL and recommendable, not dropped by the
  # Add-output var-coverage filter (mod_add_output.R's .missing_vars()).
  adsl$RACE <- rep(
    c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN"),
    times = 4L
  )
  # 3 of 12 subjects discontinued (one per arm pairing), rest completed.
  adsl$DISCFL <- rep(c("Y", "N", "N", "N"), times = 3L)
  adsl$DCDECOD <- ifelse(adsl$DISCFL == "Y", "ADVERSE EVENT", "COMPLETED")
  # Placebo = 0 mg; Xanomeline = the CDISC-pilot 81 mg/day dose.
  adsl$EXDOSE <- ifelse(adsl$TRT01P == "Placebo", 0, 81)

  visits <- c("Baseline", "Week 4", "Week 8")
  advs <- data.frame(
    USUBJID = rep(adsl$USUBJID, each = length(visits)),
    TRT01P = rep(adsl$TRT01P, each = length(visits)),
    AVISIT = rep(visits, times = nrow(adsl)),
    AVISITN = rep(seq_along(visits) - 1L, times = nrow(adsl)),
    PARAMCD = "SYSBP",
    AVAL = round(
      stats::rnorm(nrow(adsl) * length(visits), mean = 128, sd = 10),
      1
    ),
    stringsAsFactors = FALSE
  )
  # CHG: a fixed (non-RNG) change-from-baseline lookup keyed on visit --
  # 0 at Baseline by construction, a small fixed step thereafter. Appended
  # after AVAL so the rnorm() draw above is untouched; covers
  # mean_over_time's CHG role var.
  chg_by_visit <- c("Baseline" = 0, "Week 4" = 2.5, "Week 8" = 4.5)
  advs$CHG <- unname(chg_by_visit[advs$AVISIT])

  adtte <- data.frame(
    USUBJID = adsl$USUBJID,
    TRT01P = adsl$TRT01P,
    AVAL = round(stats::runif(nrow(adsl), min = 20, max = 400), 1),
    CNSR = rep(c(0L, 1L), 6L),
    stringsAsFactors = FALSE
  )

  # ADAE: occurrence-shaped (AEBODSYS/AEDECOD, the *TERM|*DECOD suffix
  # detect_structure() keys on) -- a fixed lookup, not a random draw, so
  # appending it after ADVS/ADTTE never perturbs the rnorm()/runif() stream
  # those two already consume above. Two AE records per subject: a GI event
  # alternating Nausea/Vomiting (odd/even subject), plus a Cardiac
  # Atrial-fibrillation event for every subject -- all three PTs across two
  # SOCs appear at least once. TRTEMFL is the column both ae_overall/
  # ae_soc_pt presets filter on.
  gi_pt <- rep(c("Nausea", "Vomiting"), times = 6L)
  adae <- data.frame(
    USUBJID = rep(adsl$USUBJID, each = 2L),
    TRT01P = rep(adsl$TRT01P, each = 2L),
    AEBODSYS = rep(
      c("Gastrointestinal disorders", "Cardiac disorders"),
      times = 12L
    ),
    AEDECOD = as.vector(rbind(gi_pt, rep("Atrial fibrillation", 12L))),
    TRTEMFL = "Y",
    stringsAsFactors = FALSE
  )

  .demo_register(con, "ADSL", adsl, dir)
  .demo_register(con, "ADVS", advs, dir)
  .demo_register(con, "ADTTE", adtte, dir)
  .demo_register(con, "ADAE", adae, dir)
  con
}

#' Write one demo data frame to parquet and register it.
#' @noRd
.demo_register <- function(con, name, data, dir) {
  path <- file.path(dir, paste0(tolower(name), ".parquet"))
  artoo::write_parquet(data, path)
  arpillar::register_dataset(con, name, path)
  invisible(NULL)
}
