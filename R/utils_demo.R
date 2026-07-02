# A tiny bundled demo catalog: ADSL/ADVS/ADTTE, written to parquet and
# registered against a fresh arpillar catalog. Shared by the store tests, the
# later shinytest2 apps, and screenshots -- one fixture builder, not a copy
# per surface.

#' A small demo catalog (ADSL/ADVS/ADTTE), opened and registered
#'
#' Writes three minimal analysis-ready datasets to parquet under `dir` and
#' registers each against a freshly opened [arpillar::engine_open()] catalog.
#' The caller owns the returned catalog's lifecycle -- close it with
#' [arpillar::engine_close()].
#' @param dir *Where to write the parquet files.* `<character(1)>: default
#'   tempdir()`.
#' @return *`<catalog>`.* An open, registered arpillar catalog with `ADSL`,
#'   `ADVS`, and `ADTTE` in the `WORK` library.
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

  adtte <- data.frame(
    USUBJID = adsl$USUBJID,
    TRT01P = adsl$TRT01P,
    AVAL = round(stats::runif(nrow(adsl), min = 20, max = 400), 1),
    CNSR = rep(c(0L, 1L), 6L),
    stringsAsFactors = FALSE
  )

  .demo_register(con, "ADSL", adsl, dir)
  .demo_register(con, "ADVS", advs, dir)
  .demo_register(con, "ADTTE", adtte, dir)
  con
}

#' Write one demo data frame to parquet and register it.
#' @noRd
.demo_register <- function(con, name, data, dir) {
  path <- file.path(dir, paste0(tolower(name), ".parquet"))
  nanoparquet::write_parquet(data, path)
  arpillar::register_dataset(con, name, path)
  invisible(NULL)
}
