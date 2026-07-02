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

test_that(".mount_folder registers under the folder library, records kinds, bumps nonce", {
  fx <- .md_store()
  withr::defer(arpillar::engine_close(fx$con))

  grid <- arpillar::catalog_grid(fx$con)
  expect_equal(nrow(grid), 3L)
  expect_setequal(unique(grid$library), c("adam", "sdtm"))
  # Kind recorded at mount time (the catalog does not surface it).
  expect_identical(
    shiny::isolate(.source_kind(fx$store, "ADSL", "adam")),
    ".parquet"
  )
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
    session$setInputs(focus = list(name = "ADSL", lib = "adam"))
    expect_identical(store$rv$data_focus, list(name = "ADSL", library = "adam"))

    session$setInputs(view = 1)
    expect_identical(
      store$rv$grid_dataset,
      list(name = "ADSL", library = "adam")
    )
    grid_html <- as.character(output$explorer$html)
    expect_match(grid_html, "ar-dx-grid", fixed = TRUE)
    expect_match(grid_html, "USUBJID", fixed = TRUE)
    expect_match(grid_html, "ar-colpick", fixed = TRUE)

    session$setInputs(grid_back = 1)
    expect_null(store$rv$grid_dataset)
  })
})

test_that("mod_data_server: double-click opens the grid directly", {
  fx <- .md_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_data_server, args = list(store = fx$store), {
    session$setInputs(open = list(name = "ADAE", lib = "adam"))
    expect_identical(
      store$rv$grid_dataset,
      list(name = "ADAE", library = "adam")
    )
  })
})

test_that("mod_data_server: Delete unmounts the focused dataset", {
  fx <- .md_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_data_server, args = list(store = fx$store), {
    session$setInputs(focus = list(name = "DM", lib = "sdtm"))
    before <- store$rv$catalog_nonce
    session$setInputs(delete = 1)

    expect_equal(nrow(arpillar::catalog_grid(store$con)), 2L)
    expect_false("DM" %in% arpillar::catalog_grid(store$con)$name)
    expect_null(store$rv$data_focus)
    expect_gt(store$rv$catalog_nonce, before)
  })
})
