# Export package (v5, decision #8): the whole-deliverable zip -- outputs/ +
# programs/ + report.json + manifest.csv, ready outputs rendered, the rest
# skipped and reported. The render leg is the export-identical seam.

# A store whose report carries TWO ready summary outputs (one demographics-
# style on ADSL, one occurrence on ADAE) plus ONE draft (no roles). The
# ready ones render; the draft is skipped.
.ex_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))

  ready1 <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))
  shiny::isolate(update_object(store, ready1, function(o) {
    S7::set_props(
      o,
      title = "Demographics",
      options = list(number = "14.1.1", number_label = "Table"),
      roles = list(
        arpillar::role(
          slot = "treatment",
          items = list(arpillar::data_item(name = "TRT01P"))
        ),
        arpillar::role(
          slot = "summarize",
          items = list(arpillar::data_item(
            name = "AGE",
            label = "Age",
            role_type = "measure"
          ))
        )
      )
    )
  }))
  ready2 <- shiny::isolate(add_from_preset(store, "ae_overall", "ADAE"))
  shiny::isolate(update_object(store, ready2, function(o) {
    # Merge, never replace -- the occurrence preset carries required
    # options (population, hier_sort) that a wholesale replace would drop,
    # dropping the output back to draft.
    S7::set_props(
      o,
      options = c(o@options, list(number = "14.3.1", number_label = "Table"))
    )
  }))
  draft <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))
  shiny::isolate(update_object(store, draft, function(o) {
    S7::set_props(
      o,
      options = c(o@options, list(number = "14.1.2", number_label = "Table"))
    )
  }))

  list(con = con, store = store, ready = c(ready1, ready2), draft = draft)
}

test_that(".report_slug is filesystem-safe with a sane fallback", {
  r1 <- arpillar::report(
    id = "r",
    name = "CS-401 Primary Safety!",
    pages = list()
  )
  expect_identical(.report_slug(r1), "cs-401-primary-safety")
  r2 <- arpillar::report(id = "r", name = "***", pages = list())
  expect_identical(.report_slug(r2), "report")
})

test_that(".build_export_package renders ready outputs, skips drafts, writes the tree", {
  fx <- .ex_store()
  withr::defer(arpillar::engine_close(fx$con))
  dir <- withr::local_tempdir()

  res <- shiny::isolate(
    .build_export_package(fx$store, dir, stamp = "2026-07-02T14:02:00")
  )

  # Two ready rendered, one draft skipped.
  expect_length(res$ready, 2L)
  expect_length(res$skipped, 1L)
  expect_identical(res$skipped, fx$draft)

  # outputs/: exactly the two ready RTFs, each non-empty and RTF-headed.
  rtfs <- list.files(file.path(dir, "outputs"), pattern = "\\.rtf$")
  expect_length(rtfs, 2L)
  expect_true(any(grepl("14-1-1", rtfs)))
  expect_true(any(grepl("14-3-1", rtfs)))
  first <- readLines(file.path(dir, "outputs", rtfs[[1]]), n = 1L, warn = FALSE)
  expect_match(first, "\\{\\\\rtf", perl = TRUE)

  # programs/: one .R per output (all three) plus run-all.R.
  progs <- list.files(file.path(dir, "programs"), pattern = "\\.R$")
  expect_true("run-all.R" %in% progs)
  expect_length(setdiff(progs, "run-all.R"), 3L)
  expect_silent(parse(file.path(dir, "programs", "run-all.R")))

  # report.json round-trips back to a report.
  expect_true(file.exists(file.path(dir, "report.json")))
  reopened <- arpillar::report_from_json(file.path(dir, "report.json"))
  expect_length(.all_objects(reopened), 3L)
})

test_that(".build_export_package writes a manifest with one row per output", {
  fx <- .ex_store()
  withr::defer(arpillar::engine_close(fx$con))
  dir <- withr::local_tempdir()

  shiny::isolate(
    .build_export_package(fx$store, dir, stamp = "2026-07-02T14:02:00")
  )
  m <- utils::read.csv(file.path(dir, "manifest.csv"), stringsAsFactors = FALSE)

  expect_equal(nrow(m), 3L)
  expect_setequal(
    colnames(m),
    c("file", "number", "title", "label", "dataset", "status", "timestamp")
  )
  expect_equal(sum(m$status == "ready"), 2L)
  expect_true(all(m$timestamp == "2026-07-02T14:02:00"))
  # Ready rows carry an outputs/ path; the draft's file cell is empty.
  expect_true(all(grepl("^outputs/", m$file[m$status == "ready"])))
})

test_that(".zip_export produces a readable archive containing the tree", {
  fx <- .ex_store()
  withr::defer(arpillar::engine_close(fx$con))
  dir <- file.path(withr::local_tempdir(), "cs-401")
  dir.create(dir)
  shiny::isolate(
    .build_export_package(fx$store, dir, stamp = "2026-07-02T14:02:00")
  )
  zf <- withr::local_tempfile(fileext = ".zip")

  .zip_export(dir, zf)
  expect_true(file.exists(zf))
  entries <- zip::zip_list(zf)$filename
  expect_true(any(grepl("report.json", entries)))
  expect_true(any(grepl("manifest.csv", entries)))
  expect_true(any(grepl("programs/run-all.R", entries)))
})

test_that("mod_frame export downloadHandler zips a named package", {
  fx <- .ex_store()
  withr::defer(arpillar::engine_close(fx$con))
  shiny::isolate(store <- fx$store)

  shiny::testServer(mod_frame_server, args = list(store = fx$store), {
    path <- output$export_btn
    expect_match(basename(path), "\\.zip$")
    expect_true(file.exists(path))
    entries <- zip::zip_list(path)$filename
    expect_true(any(grepl("report.json", entries)))
    # The export is logged (the QC/incompleteness trail).
    expect_match(store$rv$log[[length(store$rv$log)]], "export", fixed = TRUE)
  })
})
