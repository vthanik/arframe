# Async export (plan Task 16): the whole-package RTF render moved onto a mirai
# daemon so the galley stays live. A DuckDB connection NEVER crosses the daemon
# boundary -- the daemon gets only the report JSON + the dataset file PATHS and
# re-opens its own engine. These tests exercise the daemon render seam directly
# (off-Shiny); the ExtendedTask reactive glue in mod_frame.R is browser-only.

.async_fixture <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  mk <- function(ds, meas) {
    id <- shiny::isolate(add_from_generator(store, "summary", ds))
    shiny::isolate(update_object(store, id, function(o) {
      S7::set_props(
        o,
        roles = list(
          arpillar::role(
            slot = "treatment",
            items = list(arpillar::data_item(name = "TRT01P"))
          ),
          arpillar::role(
            slot = "summarize",
            items = list(arpillar::data_item(
              name = meas,
              role_type = "measure"
            ))
          )
        )
      )
    }))
    id
  }
  id1 <- mk("ADSL", "AGE")
  id2 <- mk("ADVS", "AVAL")
  list(con = con, store = store, id1 = id1, id2 = id2)
}

test_that(".export_dataset_paths resolves each ready output's dataset to a file", {
  f <- .async_fixture()
  withr::defer(arpillar::engine_close(f$con))
  report <- shiny::isolate(f$store$rv$report)
  paths <- .export_dataset_paths(f$store, report)
  expect_setequal(names(paths), c("ADSL", "ADVS"))
  expect_true(all(file.exists(unlist(paths))))
})

test_that(".export_names maps each ready output id to its slug filename", {
  f <- .async_fixture()
  withr::defer(arpillar::engine_close(f$con))
  report <- shiny::isolate(f$store$rv$report)
  nm <- .export_names(report)
  expect_setequal(names(nm), c(f$id1, f$id2))
  expect_true(all(grepl("\\.rtf$", unlist(nm))))
})

test_that("export_task() builds a shiny ExtendedTask", {
  expect_true(inherits(export_task(), "ExtendedTask"))
})

test_that("export_mirai renders every ready output in a daemon, byte-identical to the sync seam", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  f <- .async_fixture()
  withr::defer(arpillar::engine_close(f$con))
  report <- shiny::isolate(f$store$rv$report)
  obj1 <- .find_object(report, f$id1)
  obj2 <- .find_object(report, f$id2)

  # In-session references through the sync seam the daemon must match exactly.
  ref_dir <- withr::local_tempdir()
  r1 <- file.path(ref_dir, "1.rtf")
  arpillar::render_rtf(arpillar::build_ard(f$con, obj1), obj1, r1)
  r2 <- file.path(ref_dir, "2.rtf")
  arpillar::render_rtf(arpillar::build_ard(f$con, obj2), obj2, r2)

  json <- arpillar::report_to_json(report)
  paths <- .export_dataset_paths(f$store, report)
  names_map <- .export_names(report)
  out_dir <- withr::local_tempdir()

  mirai::daemons(1, dispatcher = FALSE, .compute = "arframe")
  withr::defer(mirai::daemons(0, .compute = "arframe"))

  m <- export_mirai(json, paths, out_dir, names_map)
  mirai::call_mirai(m)
  res <- m$data

  expect_false(inherits(res, "miraiError"))
  expect_length(res, 2L)
  expect_true(all(file.exists(res)))
  d1 <- file.path(out_dir, names_map[[f$id1]])
  d2 <- file.path(out_dir, names_map[[f$id2]])
  expect_identical(
    readBin(r1, "raw", file.size(r1)),
    readBin(d1, "raw", file.size(d1))
  )
  expect_identical(
    readBin(r2, "raw", file.size(r2)),
    readBin(d2, "raw", file.size(d2))
  )
})

test_that(".build_export_package(rendered=) assembles the tree from pre-rendered files, no re-render", {
  f <- .async_fixture()
  withr::defer(arpillar::engine_close(f$con))
  report <- shiny::isolate(f$store$rv$report)
  dir <- withr::local_tempdir()
  out_dir <- file.path(dir, "outputs")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Stand in for the daemon: write each ready output's RTF under its slug.
  names_map <- .export_names(report)
  rendered <- list()
  for (id in names(names_map)) {
    p <- file.path(out_dir, names_map[[id]])
    writeLines("{\\rtf1}", p)
    rendered[[id]] <- p
  }

  res <- shiny::isolate(
    .build_export_package(
      f$store,
      dir,
      stamp = "2026-07-02T00:00:00",
      rendered = rendered
    )
  )
  expect_length(res$ready, 2L)
  m <- utils::read.csv(file.path(dir, "manifest.csv"), stringsAsFactors = FALSE)
  expect_equal(sum(m$status == "ready"), 2L)
  # The cheap parts are still assembled on the main process.
  expect_true(file.exists(file.path(dir, "programs", "run-all.R")))
  expect_true(file.exists(file.path(dir, "report.json")))
})

test_that(".build_export_package(rendered=) prunes stale RTFs against the current slug set, not this pass's render", {
  f <- .async_fixture()
  withr::defer(arpillar::engine_close(f$con))
  report <- shiny::isolate(f$store$rv$report)
  dir <- withr::local_tempdir()
  out_dir <- file.path(dir, "outputs")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  names_map <- .export_names(report)

  # A completely unclaimed file (a renamed/deleted output's leftover) --
  # must be pruned.
  writeLines("{\\rtf1}", file.path(out_dir, "stale-name.rtf"))

  # id1's last-known-good render already on disk, but id1's render FAILS
  # this pass (absent from `rendered`) -- its file must survive, since id1
  # is still a current output (fail loud on render, never on disk state).
  writeLines("{\\rtf1 old}", file.path(out_dir, names_map[[f$id1]]))

  # id2 renders successfully this pass.
  p2 <- file.path(out_dir, names_map[[f$id2]])
  writeLines("{\\rtf1 new}", p2)
  rendered <- stats::setNames(list(p2), f$id2)

  res <- shiny::isolate(
    .build_export_package(
      f$store,
      dir,
      stamp = "2026-07-02T00:00:00",
      rendered = rendered
    )
  )

  # id2 ready; id1 reported as an error (ready status, but not rendered
  # this pass) -- yet its RTF is not deleted.
  expect_setequal(res$ready, f$id2)
  expect_setequal(res$skipped, f$id1)

  rtfs <- list.files(out_dir, pattern = "\\.rtf$")
  expect_setequal(rtfs, unname(unlist(names_map)))
  expect_false("stale-name.rtf" %in% rtfs)
  expect_identical(
    readLines(file.path(out_dir, names_map[[f$id1]]), warn = FALSE),
    "{\\rtf1 old}"
  )
})
