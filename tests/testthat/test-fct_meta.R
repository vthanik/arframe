# Dataset column metadata (Data mode): SAS-style Label / Type / Length /
# Format per variable, read from the on-disk file via artoo (which recovers
# the labels the DuckDB catalog's SQL metadata drops), memoized per dataset.

test_that(".dataset_meta reads artoo labels + format from the source, memoized", {
  skip_if_not_installed("artoo")
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  # A labelled xpt so the artoo label path is exercised deterministically
  # (the nanoparquet demo datasets carry no labels).
  df <- data.frame(
    SUBJ = c("a", "b"),
    AGE = c(30L, 40L),
    stringsAsFactors = FALSE
  )
  attr(df$SUBJ, "label") <- "Subject Id"
  attr(df$AGE, "label") <- "Age in Years"
  tf <- tempfile(fileext = ".xpt")
  artoo::write_xpt(df, tf)
  arpillar::register_dataset(con, "LBL", tf)
  store <- shiny::isolate(new_store(con))

  m <- .dataset_meta(store, "LBL")
  expect_setequal(colnames(m), c("name", "label", "type", "length", "format"))
  expect_identical(m$label[m$name == "SUBJ"], "Subject Id")
  expect_identical(m$label[m$name == "AGE"], "Age in Years")
  # Memoized: the second read returns the cached frame.
  expect_identical(store$meta[["LBL"]], m)
})

test_that(".dataset_meta falls back to a data_items shape for a label-less dataset", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  m <- .dataset_meta(store, "ADSL")
  expect_setequal(colnames(m), c("name", "label", "type", "length", "format"))
  expect_true("TRT01P" %in% m$name)
  expect_true(all(m$type %in% c("measure", "category", "date")))
  # Order matches the catalog's own column order (data_items).
  di <- arpillar::data_items(con, "ADSL")
  expect_identical(m$name, di$name)
})

test_that(".unmount_dataset clears the metadata memo", {
  skip_if_not_installed("artoo")
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  m <- .dataset_meta(store, "ADSL")
  expect_false(is.null(store$meta[["ADSL"]]))
  shiny::isolate(.unmount_dataset(store, "ADSL"))
  expect_null(store$meta[["ADSL"]])
})
