# shinytest2 fixture app for the Task-8b Add-output overlay smoke test +
# screenshot. Builds the bundled demo parquet (arframe:::.demo_catalog(),
# now ADSL/ADVS/ADTTE/ADAE) and hands the four file paths to the real
# launcher so arframe() opens its own catalog (a launched app cannot share
# a live DuckDB connection across processes, matching the Task-6/7 frame/
# contents fixtures' pattern) -- a fresh "Untitled report" with nothing
# added yet, so clicking "+ Add output" is the first thing the test does.
library(arframe)

.con <- arframe:::.demo_catalog(tempdir())
.paths <- c(
  ADSL = arpillar::dataset_path(.con, "ADSL"),
  ADVS = arpillar::dataset_path(.con, "ADVS"),
  ADTTE = arpillar::dataset_path(.con, "ADTTE"),
  ADAE = arpillar::dataset_path(.con, "ADAE")
)
arpillar::engine_close(.con)

arframe(data = .paths)
