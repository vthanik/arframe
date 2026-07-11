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

# The Shiny wiring of the export button is async now (Task 16): it invokes an
# ExtendedTask on the daemon pool and delivers via a hidden download link.
# That reactive glue is browser-only; the render + package assembly are
# covered in test-fct_async.R (daemon byte-identical + `.build_export_package(
# rendered=)`), and the button/hidden-link affordances in test-mod_frame.R.

test_that("the export click's report renders a self-consistent package, no source line", {
  fx <- .ex_store()
  withr::defer(arpillar::engine_close(fx$con))
  dir <- withr::local_tempdir()

  # The export button hands .build_export_package the SAME render-prepared
  # copy the daemon serialized (mod_frame); the sync leg here proves it
  # assembles emitted RTFs, programs, and report.json. The auto source line
  # was removed 2026-07-09 (user call) -- NONE of the artifacts carry one.
  injected <- shiny::isolate(.report_for_export(fx$store$rv$report))
  res <- shiny::isolate(
    .build_export_package(
      fx$store,
      dir,
      stamp = "2026-07-02T14:02:00",
      report = injected
    )
  )
  expect_gt(length(res$ready), 0L)

  rtfs <- list.files(file.path(dir, "outputs"), full.names = TRUE)
  expect_gt(length(rtfs), 0L)
  for (f in rtfs) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_gt(nchar(txt), 0L)
    expect_no_match(txt, "Source:", fixed = TRUE)
  }

  # The archival spec carries no source line either.
  spec <- paste(
    readLines(file.path(dir, "report.json"), warn = FALSE),
    collapse = "\n"
  )
  expect_no_match(spec, "Source:", fixed = TRUE)

  # The live store report stays clean -- no source at rest.
  live <- shiny::isolate(fx$store$rv$report)
  for (obj in .all_objects(live)) {
    expect_null(obj@options$source)
  }
})

test_that(".sync_output_dir falls back to output/ when output_rtf_dir is blank, not just NULL", {
  # Twin of the .emit_programs blank-path bug: Setup > Paths writes "" for an
  # unset output dir, and `%||%` keeps it -- spilling renders into the project
  # root instead of ./output/. A blank string must fall back to output/.
  fx <- .ex_store()
  withr::defer(arpillar::engine_close(fx$con))
  store <- fx$store
  dir <- withr::local_tempdir()
  shiny::isolate({
    store$rv$path <- dir
    r <- store$rv$report
    store$rv$report <- S7::set_props(
      r,
      theme = modifyList(r@theme, list(paths = list(output_rtf_dir = "")))
    )
  })

  slugs <- shiny::isolate(arpillar::output_slugs(store$rv$report))
  slug1 <- slugs[[fx$ready[[1]]]]
  src <- file.path(withr::local_tempdir(), paste0(slug1, ".rtf"))
  writeLines("rtf", src)

  shiny::isolate(.sync_output_dir(store, list(src)))

  expect_true(file.exists(file.path(dir, "output", paste0(slug1, ".rtf"))))
  # No .rtf spilled into the project root.
  expect_length(list.files(dir, pattern = "\\.rtf$"), 0L)
})
