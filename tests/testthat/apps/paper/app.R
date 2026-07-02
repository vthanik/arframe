# shinytest2 fixture app for the Task-9 paper smoke test + screenshots.
# Builds the bundled demo parquet (arframe:::.demo_catalog(), which
# includes ADAE), then a 4-output report: a READY summary table (real demo
# ADSL columns -- the bundled `demographics` preset requests a RACE column
# the minimal demo ADSL lacks, so this fixture hand-rolls the roles instead,
# matching test-mod_paper.R's own fixture builder), a READY occurrence
# table (`ae_overall` on ADAE, PT-only incidence rows), a DRAFT crosstab
# (ghost shell), and a summary object with a bogus-column role (the error
# summary) -- serialized to project JSON and handed to the real launcher (a
# launched app cannot share a live DuckDB connection across processes, so
# the catalog is rebuilt from the registered file paths, matching the
# Task-6/7 fixture apps' own pattern).
library(arframe)

.con <- arframe:::.demo_catalog(tempdir())
.paths <- c(
  ADSL = arpillar::dataset_path(.con, "ADSL"),
  ADVS = arpillar::dataset_path(.con, "ADVS"),
  ADTTE = arpillar::dataset_path(.con, "ADTTE"),
  ADAE = arpillar::dataset_path(.con, "ADAE")
)

.ready_table <- arpillar::object(
  id = "out001",
  type = "summary",
  title = "Demographics and Baseline Characteristics",
  dataset = "ADSL",
  options = list(number = "14.1.1", number_label = "Table"),
  footnotes = "Safety Population.",
  roles = list(
    arpillar::role(
      slot = "treatment",
      items = list(arpillar::data_item(name = "TRT01P"))
    ),
    arpillar::role(
      slot = "summarize",
      items = list(
        arpillar::data_item(
          name = "AGE",
          label = "Age (years)",
          role_type = "measure"
        ),
        arpillar::data_item(name = "SEX", label = "Sex", role_type = "category")
      )
    )
  )
)

.ready_ae <- arpillar::object(
  id = "out002",
  type = "occurrence",
  title = "Overall Summary of Treatment-Emergent Adverse Events",
  dataset = "ADAE",
  options = list(
    number = "14.3.1",
    number_label = "Table",
    population = "ADSL"
  ),
  footnotes = "Safety Population. A subject is counted once per preferred term.",
  filters = list(list(column = "TRTEMFL", op = "==", value = "Y")),
  roles = list(
    arpillar::role(
      slot = "treatment",
      items = list(arpillar::data_item(name = "TRT01P"))
    ),
    arpillar::role(
      slot = "hierarchy",
      items = list(arpillar::data_item(name = "AEDECOD"))
    )
  )
)

.draft_table <- arpillar::object(
  id = "out003",
  type = "crosstab",
  title = "Subject Disposition",
  dataset = "ADSL"
)

.broken_table <- arpillar::object(
  id = "out004",
  type = "summary",
  title = "Broken Output (demo)",
  dataset = "ADSL",
  roles = list(
    arpillar::role(
      slot = "treatment",
      items = list(arpillar::data_item(name = "TRT01P"))
    ),
    arpillar::role(
      slot = "summarize",
      items = list(arpillar::data_item(
        name = "BOGUSVAR",
        role_type = "measure"
      ))
    )
  )
)

.report <- arpillar::report(
  id = "report1",
  name = "Paper screenshot fixture",
  pages = list(
    arpillar::page(
      id = "p1",
      objects = list(.ready_table, .ready_ae, .draft_table, .broken_table)
    )
  )
)
.project <- tempfile(fileext = ".json")
arpillar::report_to_json(.report, path = .project)
arpillar::engine_close(.con)

arframe(project = .project, data = .paths)
