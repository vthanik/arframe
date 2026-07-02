# shinytest2 fixture for the Data-mode explorer. Writes two tiny parquet
# "study folders" to a tempdir, then launches the real app pointed at them
# via the `folders` on-ramp -- so the e2e exercises mount + the
# suspendWhenHidden contract (Data mode starts CSS-hidden; its outputs must
# render anyway when the user switches modes client-side).
library(arframe)

.root <- file.path(tempdir(), "e2e-data")
.mk <- function(sub, names) {
  d <- file.path(.root, sub)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (nm in names) {
    df <- data.frame(
      USUBJID = sprintf("S%03d", 1:4),
      AGE = c(40L, 55L, 61L, 48L)
    )
    nanoparquet::write_parquet(df, file.path(d, paste0(nm, ".parquet")))
  }
  d
}
.adam <- .mk("adam", c("adsl", "adae"))
.sdtm <- .mk("sdtm", "dm")

arframe(folders = c(.adam, .sdtm))
