# shinytest2 fixture app for the Task-6 frame smoke test + screenshot. Builds
# the bundled demo parquet via arframe:::.demo_catalog(), then hands the
# three file paths to the real launcher so arframe() opens its own catalog
# (a launched app cannot share a live DuckDB connection across processes).
library(arframe)

.con <- arframe:::.demo_catalog(tempdir())
.paths <- c(
  ADSL = arpillar::dataset_path(.con, "ADSL"),
  ADVS = arpillar::dataset_path(.con, "ADVS"),
  ADTTE = arpillar::dataset_path(.con, "ADTTE")
)
arpillar::engine_close(.con)

arframe(data = .paths, daemons = 0)
