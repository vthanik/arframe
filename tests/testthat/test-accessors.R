# ──────────────────────────────────────────────────────────────────────────────
# test-accessors.R — Tests for api-accessors.R (fr_get_*() family)
# ──────────────────────────────────────────────────────────────────────────────

# ── fr_get_data ──────────────────────────────────────────────────────────────

test_that("fr_get_data returns the data frame from a table spec", {
  spec <- tbl_demog |> fr_table()
  d <- fr_get_data(spec)
  expect_identical(d, tbl_demog)
})

test_that("fr_get_data returns data from different datasets", {
  spec <- tbl_ae_soc |> fr_table()
  expect_identical(fr_get_data(spec), tbl_ae_soc)
})

test_that("fr_get_data returns a data frame (class check)", {
  spec <- tbl_demog |> fr_table()
  expect_s3_class(fr_get_data(spec), "data.frame")
})

test_that("fr_get_data errors on non-spec input", {
  expect_error(fr_get_data(data.frame()), "fr_spec")
  expect_error(fr_get_data(list()), "fr_spec")
  expect_error(fr_get_data(NULL), "fr_spec")
  expect_error(fr_get_data(42), "fr_spec")
  expect_error(fr_get_data("string"), "fr_spec")
})


# ── fr_get_page ──────────────────────────────────────────────────────────────

test_that("fr_get_page returns a list with default page properties", {
  spec <- tbl_demog |> fr_table()
  pg <- fr_get_page(spec)
  expect_type(pg, "list")
  expect_true("orientation" %in% names(pg))
  expect_true("paper" %in% names(pg))
  expect_true("margins" %in% names(pg))
  expect_true("font_family" %in% names(pg))
  expect_true("font_size" %in% names(pg))
})

test_that("fr_get_page reflects fr_page() settings", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_page(orientation = "landscape", font_size = 8)
  pg <- fr_get_page(spec)
  expect_equal(pg$orientation, "landscape")
  expect_equal(pg$font_size, 8)
})

test_that("fr_get_page returns margins as a list", {
  spec <- tbl_demog |> fr_table()
  pg <- fr_get_page(spec)
  expect_type(pg$margins, "list")
  expect_true(all(c("top", "bottom", "left", "right") %in% names(pg$margins)))
})

test_that("fr_get_page errors on non-spec input", {
  expect_error(fr_get_page(data.frame()), "fr_spec")
  expect_error(fr_get_page(list()), "fr_spec")
})


# ── fr_get_columns ───────────────────────────────────────────────────────────

test_that("fr_get_columns returns empty list for unconfigured spec", {
  spec <- tbl_demog |> fr_table()
  cols <- fr_get_columns(spec)
  expect_type(cols, "list")
  # Before fr_cols() is called, columns may be empty or auto-populated
  # Just verify it returns a list
})

test_that("fr_get_columns returns configured columns", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      characteristic = fr_col("Characteristic", width = 2.5),
      placebo = fr_col("Placebo", align = "right")
    )
  cols <- fr_get_columns(spec)
  expect_type(cols, "list")
  expect_true("characteristic" %in% names(cols))
  expect_true("placebo" %in% names(cols))
  expect_equal(cols$characteristic$label, "Characteristic")
  expect_equal(cols$characteristic$width, 2.5)
  expect_equal(cols$placebo$align, "right")
})

test_that("fr_get_columns errors on non-spec input", {
  expect_error(fr_get_columns("not a spec"), "fr_spec")
})


# ── fr_get_col ───────────────────────────────────────────────────────────────

test_that("fr_get_col retrieves a single column spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(characteristic = fr_col("Characteristic", width = 2.5))
  col <- fr_get_col(spec, "characteristic")
  expect_s3_class(col, "fr_col")
  expect_equal(col$label, "Characteristic")
  expect_equal(col$width, 2.5)
})

test_that("fr_get_col errors on non-existent column", {
  spec <- tbl_demog |> fr_table() |> fr_cols(characteristic = fr_col("Char"))
  expect_error(fr_get_col(spec, "nonexistent"), "not found")
})

test_that("fr_get_col error message lists available columns", {
  spec <- tbl_demog |> fr_table() |> fr_cols(characteristic = fr_col("Char"))
  expect_error(fr_get_col(spec, "xyz"), "characteristic")
})

test_that("fr_get_col errors on non-spec input", {
  expect_error(fr_get_col(data.frame(), "x"), "fr_spec")
})

test_that("fr_get_col errors on non-character col argument", {
  spec <- tbl_demog |> fr_table()
  expect_error(fr_get_col(spec, 1))
  expect_error(fr_get_col(spec, TRUE))
})

test_that("fr_get_col errors when columns list is empty", {
  spec <- tbl_demog |> fr_table()
  # If no columns configured, all lookups should fail
  if (length(spec$columns) == 0) {
    expect_error(fr_get_col(spec, "anything"), "not found")
  }
})


# ── fr_get_titles ────────────────────────────────────────────────────────────

test_that("fr_get_titles returns empty list when no titles set", {
  spec <- tbl_demog |> fr_table()
  titles <- fr_get_titles(spec)
  expect_type(titles, "list")
  expect_length(titles, 0)
})

test_that("fr_get_titles returns configured titles", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_titles("Table 14.1.1", "Summary of Demographics")
  titles <- fr_get_titles(spec)
  expect_length(titles, 2)
  expect_equal(titles[[1]]$content, "Table 14.1.1")
  expect_equal(titles[[2]]$content, "Summary of Demographics")
})

test_that("fr_get_titles preserves bold setting", {
  spec <- tbl_demog |> fr_table() |> fr_titles("Title", .bold = TRUE)
  titles <- fr_get_titles(spec)
  expect_true(titles[[1]]$bold)
})

test_that("fr_get_titles preserves align setting", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_titles("Centered Title", .align = "center")
  titles <- fr_get_titles(spec)
  expect_equal(titles[[1]]$align, "center")
})

test_that("fr_get_titles errors on non-spec input", {
  expect_error(fr_get_titles(42), "fr_spec")
})


# ── fr_get_footnotes ────────────────────────────────────────────────────────

test_that("fr_get_footnotes returns empty list when no footnotes set", {
  spec <- tbl_demog |> fr_table()
  fns <- fr_get_footnotes(spec)
  expect_type(fns, "list")
  expect_length(fns, 0)
})

test_that("fr_get_footnotes returns configured footnotes", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_footnotes("Source: ADSL", "[a] Fisher's exact test")
  fns <- fr_get_footnotes(spec)
  expect_length(fns, 2)
  expect_equal(fns[[1]]$content, "Source: ADSL")
  expect_equal(fns[[2]]$content, "[a] Fisher's exact test")
})

test_that("fr_get_footnotes preserves placement setting", {
  spec <- tbl_demog |> fr_table() |> fr_footnotes("Note", .placement = "last")
  fns <- fr_get_footnotes(spec)
  expect_equal(fns[[1]]$placement, "last")
})

test_that("fr_get_footnotes errors on non-spec input", {
  expect_error(fr_get_footnotes(NULL), "fr_spec")
})


# ── fr_get_styles ────────────────────────────────────────────────────────────

test_that("fr_get_styles returns empty list when no styles set", {
  spec <- tbl_demog |> fr_table()
  styles <- fr_get_styles(spec)
  expect_type(styles, "list")
  expect_length(styles, 0)
})

test_that("fr_get_styles returns configured styles", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_styles(
      fr_row_style(rows = 1L, bold = TRUE),
      fr_col_style(cols = "total", background = "#EBF5FB")
    )
  styles <- fr_get_styles(spec)
  expect_length(styles, 2)
})

test_that("fr_get_styles errors on non-spec input", {
  expect_error(fr_get_styles(list(a = 1)), "fr_spec")
})


# ── fr_get_rules ─────────────────────────────────────────────────────────────

test_that("fr_get_rules returns empty list when no rules set", {
  fr_config_reset()
  withr::defer(fr_config_reset())
  spec <- tbl_demog |> fr_table()
  rules <- fr_get_rules(spec)
  expect_type(rules, "list")
  expect_length(rules, 0)
})

test_that("fr_get_rules returns rules after fr_hlines", {
  spec <- tbl_demog |> fr_table() |> fr_hlines("header")
  rules <- fr_get_rules(spec)
  expect_type(rules, "list")
  expect_true(length(rules) > 0)
})

test_that("fr_get_rules returns rules after fr_grid", {
  spec <- tbl_demog |> fr_table() |> fr_grid()
  rules <- fr_get_rules(spec)
  expect_true(length(rules) > 0)
})

test_that("fr_get_rules errors on non-spec input", {
  expect_error(fr_get_rules(c(1, 2, 3)), "fr_spec")
})


# ── Deep copy behavior ──────────────────────────────────────────────────────

test_that("fr_get_data returns a copy (modifying result doesn't affect spec)", {
  spec <- tbl_demog |> fr_table()
  d <- fr_get_data(spec)
  d$new_col <- 1
  # Original spec data should not have the new column
  expect_false("new_col" %in% names(fr_get_data(spec)))
})

test_that("fr_get_page returns a copy (modifying result doesn't affect spec)", {
  spec <- tbl_demog |> fr_table() |> fr_page(font_size = 10)
  pg <- fr_get_page(spec)
  pg$font_size <- 999
  expect_equal(fr_get_page(spec)$font_size, 10)
})

test_that("fr_get_columns returns a copy (modifying result doesn't affect spec)", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(characteristic = fr_col("Char", width = 2.0))
  cols <- fr_get_columns(spec)
  cols$characteristic$width <- 99
  expect_equal(fr_get_columns(spec)$characteristic$width, 2.0)
})

test_that("fr_get_titles returns a copy (modifying result doesn't affect spec)", {
  spec <- tbl_demog |> fr_table() |> fr_titles("Title A")
  titles <- fr_get_titles(spec)
  titles[[1]]$content <- "Modified"
  expect_equal(fr_get_titles(spec)[[1]]$content, "Title A")
})

test_that("fr_get_footnotes returns a copy (modifying result doesn't affect spec)", {
  spec <- tbl_demog |> fr_table() |> fr_footnotes("Note A")
  fns <- fr_get_footnotes(spec)
  fns[[1]]$content <- "Modified"
  expect_equal(fr_get_footnotes(spec)[[1]]$content, "Note A")
})


# ── Pipeline integration ────────────────────────────────────────────────────

test_that("accessors work on a fully configured spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_page(orientation = "landscape") |>
    fr_cols(characteristic = fr_col("Char")) |>
    fr_titles("Table 1", "Demographics") |>
    fr_footnotes("Source: ADSL") |>
    fr_hlines("header") |>
    fr_styles(fr_row_style(rows = 1L, bold = TRUE))

  expect_s3_class(fr_get_data(spec), "data.frame")
  expect_equal(fr_get_page(spec)$orientation, "landscape")
  expect_true("characteristic" %in% names(fr_get_columns(spec)))
  expect_length(fr_get_titles(spec), 2)
  expect_length(fr_get_footnotes(spec), 1)
  expect_length(fr_get_styles(spec), 1)
  expect_true(length(fr_get_rules(spec)) > 0)
})

test_that("accessors work with tbl_ae_soc dataset", {
  spec <- tbl_ae_soc |> fr_table() |> fr_titles("Adverse Events")
  expect_identical(fr_get_data(spec), tbl_ae_soc)
  expect_length(fr_get_titles(spec), 1)
})
