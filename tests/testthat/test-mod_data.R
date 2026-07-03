# Data mode (v5, decision #8): the SOURCES multi-folder tree, the explorer
# detail table, the drill grid, and the mount/delete actions. Fixtures mount
# REAL parquet files from two temp "study folders" (decision #9 uses the
# CDISC pilot folders at launch; tests stay hermetic with tempdirs).

# ---- fixtures --------------------------------------------------------------

#' Write `n` tiny parquet datasets into `dir`, returning `dir`.
#' @noRd
.md_write_folder <- function(dir, names) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  for (nm in names) {
    df <- data.frame(
      USUBJID = sprintf("S%03d", 1:5),
      TRT01P = rep(c("Placebo", "Drug"), length.out = 5L),
      AGE = c(34L, 56L, 45L, 23L, 67L),
      stringsAsFactors = FALSE
    )
    nanoparquet::write_parquet(df, file.path(dir, paste0(nm, ".parquet")))
  }
  dir
}

#' A store with two mounted folders: `adam` (adsl, adae) and `sdtm` (dm).
#' @noRd
.md_store <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  adam <- .md_write_folder(file.path(root, "adam"), c("adsl", "adae"))
  sdtm <- .md_write_folder(file.path(root, "sdtm"), "dm")
  con <- arpillar::engine_open()
  store <- shiny::isolate(new_store(con))
  shiny::isolate(.mount_folder(store, adam))
  shiny::isolate(.mount_folder(store, sdtm))
  list(con = con, store = store, adam = adam, sdtm = sdtm)
}

# ---- pure helpers ----------------------------------------------------------

test_that(".folder_datasets lists only registerable files, name = uppercased stem", {
  dir <- withr::local_tempdir()
  .md_write_folder(dir, c("adsl", "adae"))
  writeLines("x", file.path(dir, "readme.txt"))
  writeLines("<xml/>", file.path(dir, "define.xml"))

  ds <- .folder_datasets(dir)
  expect_setequal(names(ds), c("ADSL", "ADAE"))
  expect_true(all(grepl("\\.parquet$", ds)))
})

test_that(".mount_folder registers into WORK, records folder + kind by name, bumps nonce", {
  fx <- .md_store()
  withr::defer(arpillar::engine_close(fx$con))

  grid <- arpillar::catalog_grid(fx$con)
  expect_equal(nrow(grid), 3L)
  # Every dataset lives in the single WORK library (the engine resolves an
  # output's @dataset there) -- the source FOLDER is arframe-side provenance.
  expect_identical(unique(grid$library), "WORK")
  expect_identical(shiny::isolate(.source_folder(fx$store, "ADSL")), "adam")
  expect_identical(shiny::isolate(.source_folder(fx$store, "DM")), "sdtm")
  expect_identical(shiny::isolate(.source_kind(fx$store, "ADSL")), ".parquet")
  expect_gt(shiny::isolate(fx$store$rv$catalog_nonce), 0L)
})

test_that(".mount_folder is idempotent -- re-mounting the same folder adds nothing", {
  fx <- .md_store()
  withr::defer(arpillar::engine_close(fx$con))

  n <- shiny::isolate(.mount_folder(fx$store, fx$adam))
  expect_identical(n, 0L)
  expect_equal(nrow(arpillar::catalog_grid(fx$con)), 3L)
})

test_that(".fmt_bytes renders B / KB / MB", {
  expect_identical(.fmt_bytes(512), "512 B")
  expect_identical(.fmt_bytes(1536), "1.5 KB")
  expect_identical(.fmt_bytes(1024^2 * 2.5), "2.5 MB")
  expect_identical(.fmt_bytes(NA_real_), "--")
})

test_that(".explorer_grid and .explorer_table handle an empty catalog", {
  con <- arpillar::engine_open()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  grid <- .explorer_grid(store)
  expect_equal(nrow(grid), 0L)
  html <- as.character(.explorer_table(shiny::NS("data"), grid, NULL))
  expect_match(html, "No datasets", fixed = TRUE)
})

# ---- UI --------------------------------------------------------------------

test_that("mod_data_ui has the sources rail, filter, and the four toolbar actions", {
  html <- as.character(mod_data_ui("data"))
  expect_match(html, "ar-data-rail", fixed = TRUE)
  expect_match(html, "ar-dx-filter", fixed = TRUE)
  expect_match(html, 'id="data-view"', fixed = TRUE)
  expect_match(html, 'id="data-import_file"', fixed = TRUE)
  expect_match(html, 'id="data-import_folder"', fixed = TRUE)
  expect_match(html, 'id="data-delete"', fixed = TRUE)
})

test_that("arframe.js carries the Data-mode delegated handlers", {
  js <- readLines(
    system.file("www", "arframe.js", package = "arframe"),
    warn = FALSE
  )
  txt <- paste(js, collapse = "\n")
  expect_match(txt, "data-ar-source", fixed = TRUE)
  expect_match(txt, "ar-dx-row", fixed = TRUE)
  expect_match(txt, "dblclick", fixed = TRUE)
  expect_match(txt, "ar-dx-filter", fixed = TRUE)
})

# ---- server ----------------------------------------------------------------

test_that("mod_data_server: the sources tree lists both libraries with counts", {
  fx <- .md_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_data_server, args = list(store = fx$store), {
    html <- as.character(output$sources$html)
    expect_match(html, 'data-ar-source="adam"', fixed = TRUE)
    expect_match(html, 'data-ar-source="sdtm"', fixed = TRUE)
    expect_match(html, "In-memory data", fixed = TRUE)
  })
})

test_that("mod_data_server: the explorer lists every dataset; a source filters it", {
  fx <- .md_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_data_server, args = list(store = fx$store), {
    all_html <- as.character(output$explorer$html)
    expect_match(all_html, "ADSL", fixed = TRUE)
    expect_match(all_html, "ADAE", fixed = TRUE)
    expect_match(all_html, "DM", fixed = TRUE)
    expect_match(all_html, ".parquet", fixed = TRUE)
    expect_match(all_html, "LAZY", fixed = TRUE)

    session$setInputs(source = "sdtm")
    expect_identical(store$rv$data_source, "sdtm")
    sdtm_html <- as.character(output$explorer$html)
    expect_match(sdtm_html, "DM", fixed = TRUE)
    expect_no_match(sdtm_html, "ADSL", fixed = TRUE)
  })
})

test_that("mod_data_server: focus, View data opens the grid, back closes it", {
  fx <- .md_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_data_server, args = list(store = fx$store), {
    session$setInputs(focus = "ADSL")
    expect_identical(store$rv$data_focus, "ADSL")

    session$setInputs(view = 1)
    expect_identical(store$rv$grid_dataset, "ADSL")
    grid_html <- as.character(output$explorer$html)
    expect_match(grid_html, "ar-dx-grid", fixed = TRUE)
    expect_match(grid_html, "USUBJID", fixed = TRUE)
    expect_match(grid_html, "ar-colpick", fixed = TRUE)
    # The property panel + sortable headers ship with the grid.
    expect_match(grid_html, "ar-prop-body", fixed = TRUE)
    expect_match(grid_html, 'data-ar-sort="USUBJID"', fixed = TRUE)
    expect_match(grid_html, 'data-ar-col="USUBJID"', fixed = TRUE)

    session$setInputs(grid_back = 1)
    expect_null(store$rv$grid_dataset)
  })
})

test_that("mod_data_server: the tree 'Add folder' CTA relays a click to the chooser", {
  fx <- .md_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_data_server, args = list(store = fx$store), {
    # The relay posts an ar-click message targeting the toolbar chooser --
    # no store mutation, just a no-error passthrough.
    expect_no_error(session$setInputs(import_folder_tree = 1))
  })
})

test_that("mod_data_server: double-click opens the grid directly", {
  fx <- .md_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_data_server, args = list(store = fx$store), {
    session$setInputs(open = "ADAE")
    expect_identical(store$rv$grid_dataset, "ADAE")
  })
})

test_that("mod_data_server: Delete unmounts the focused dataset", {
  fx <- .md_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_data_server, args = list(store = fx$store), {
    session$setInputs(focus = "DM")
    before <- store$rv$catalog_nonce
    session$setInputs(delete = 1)

    expect_equal(nrow(arpillar::catalog_grid(store$con)), 2L)
    expect_false("DM" %in% arpillar::catalog_grid(store$con)$name)
    expect_null(store$rv$data_focus)
    expect_gt(store$rv$catalog_nonce, before)
  })
})

# ---- end-to-end: the suspendWhenHidden regression --------------------------

test_that("arframe() Data mode renders the explorer after a client-side mode switch", {
  # REGRESSION: Data mode's `sources`/`explorer` uiOutputs sit in a body
  # that starts CSS-hidden (Report is the default mode). Shiny suspends
  # hidden outputs, and the mode switch is a pure client-side class flip
  # the server never sees -- so without `suspendWhenHidden = FALSE` the
  # explorer stays permanently blank. testServer cannot catch this (it does
  # not model output suspension), so it needs a real browser.
  skip_on_cran()
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("apps/data"),
    name = "data",
    height = 900,
    width = 1500,
    load_timeout = 45000
  )
  withr::defer(app$stop())

  # Switch to Data mode the way a click does: the delegated handler posts
  # `frame-mode`, and the CSS class flips -- no server output changes, so
  # drive it directly rather than via set_inputs (which waits for output).
  app$run_js('Shiny.setInputValue("frame-mode","data",{priority:"event"})')
  app$wait_for_idle(timeout = 8000)

  rows <- app$get_js('document.querySelectorAll(".ar-dx-row").length')
  expect_gt(as.numeric(rows), 0)
  srcs <- app$get_js(
    'Array.from(document.querySelectorAll("[data-ar-source]")).map(function(e){return e.getAttribute("data-ar-source")}).join("|")'
  )
  expect_match(srcs, "adam", fixed = TRUE)
  expect_match(srcs, "sdtm", fixed = TRUE)
})

# ---- column labels / property panel / sort (data meta) ---------------------

test_that(".column_picker shows the label and embeds per-column metadata", {
  meta <- data.frame(
    name = c("AGE", "SEX"),
    label = c("Age in Years", ""),
    type = c("measure", "category"),
    length = c("8", "1"),
    format = c("", ""),
    stringsAsFactors = FALSE
  )
  html <- as.character(.column_picker(meta))
  expect_match(html, "Age in Years", fixed = TRUE)
  expect_match(html, 'data-ar-col="AGE"', fixed = TRUE)
  expect_match(html, 'data-ar-type="measure"', fixed = TRUE)
  # The first row is pre-selected for the property panel.
  expect_match(html, "ar-colpick-item-sel", fixed = TRUE)
})

test_that(".property_panel renders the Property/Value rows for the first column", {
  meta <- data.frame(
    name = "AGE",
    label = "Age in Years",
    type = "measure",
    length = "8",
    format = "8.1",
    stringsAsFactors = FALSE
  )
  html <- as.character(.property_panel(meta))
  expect_match(html, "ar-prop-body", fixed = TRUE)
  expect_match(html, "Age in Years", fixed = TRUE)
  # measure -> the SAS-facing word.
  expect_match(html, "Numeric", fixed = TRUE)
  expect_match(html, "8.1", fixed = TRUE)
})

test_that(".grid_preview headers are typed and sortable, rows keep their original index", {
  sample <- data.frame(
    AGE = c(3L, 1L, 2L),
    SEX = c("M", "F", "M"),
    stringsAsFactors = FALSE
  )
  meta <- data.frame(
    name = c("AGE", "SEX"),
    label = c("", ""),
    type = c("measure", "category"),
    length = c("", ""),
    format = c("", ""),
    stringsAsFactors = FALSE
  )
  html <- as.character(.grid_preview(sample, meta))
  expect_match(html, "ar-dx-th", fixed = TRUE)
  expect_match(html, 'data-ar-sort="AGE"', fixed = TRUE)
  expect_match(html, 'data-ar-sort-type="measure"', fixed = TRUE)
  expect_match(html, 'data-ar-sort-type="category"', fixed = TRUE)
  expect_match(html, 'data-ar-orig="0"', fixed = TRUE)
})
