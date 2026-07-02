# shinytest2 fixture app for the Task-7 Contents smoke test + screenshot.
# Builds the bundled demo parquet (arframe:::.demo_catalog()), then a
# 3-output report spanning both live kinds with mixed statuses (one READY
# summary/table, one DRAFT crosstab/table, one DRAFT line/figure) so the TOC
# shows grouped rows with stamps and a selection -- serialized to project
# JSON and handed to the real launcher (a launched app cannot share a live
# DuckDB connection across processes, so the catalog is rebuilt from the
# registered file paths, matching the Task-6 frame fixture's pattern).
library(arframe)

.con <- arframe:::.demo_catalog(tempdir())
.paths <- c(
  ADSL = arpillar::dataset_path(.con, "ADSL"),
  ADVS = arpillar::dataset_path(.con, "ADVS"),
  ADTTE = arpillar::dataset_path(.con, "ADTTE")
)

.ready <- arpillar::object(
  id = "out001",
  type = "summary",
  title = "Demographics and Baseline Characteristics",
  dataset = "ADSL",
  roles = list(
    arpillar::role(
      slot = "treatment",
      items = list(arpillar::data_item(name = "TRT01P"))
    ),
    arpillar::role(
      slot = "summarize",
      items = list(arpillar::data_item(name = "AGE", role_type = "measure"))
    )
  )
)
.draft_table <- arpillar::object(
  id = "out002",
  type = "crosstab",
  title = "Summary of Categorical Variables",
  dataset = "ADSL"
)
.draft_figure <- arpillar::object(
  id = "out003",
  type = "line",
  title = "Mean Over Time",
  dataset = "ADVS"
)

.report <- arpillar::report(
  id = "report1",
  name = "Contents screenshot fixture",
  pages = list(
    arpillar::page(
      id = "p1",
      objects = list(.ready, .draft_table, .draft_figure)
    )
  )
)
.project <- tempfile(fileext = ".json")
arpillar::report_to_json(.report, path = .project)
arpillar::engine_close(.con)

arframe(project = .project, data = .paths, daemons = 0)
