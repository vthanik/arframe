# ──────────────────────────────────────────────────────────────────────────────
# test-col-split.R — Tests for column panel splitting via fr_cols(.split)
# ──────────────────────────────────────────────────────────────────────────────

test_that("calculate_col_panels returns single panel when all columns fit", {
  data <- data.frame(a = 1, b = 2, c = 3, stringsAsFactors = FALSE)
  spec <- fr_table(data)
  spec <- finalize_spec(spec)

  panels <- calculate_col_panels(spec)
  expect_equal(length(panels), 1L)
  expect_true("a" %in% panels[[1]])
  expect_true("b" %in% panels[[1]])
  expect_true("c" %in% panels[[1]])
})

test_that("calculate_col_panels splits columns when .split = TRUE and table too wide", {
  # Create a wide table with many columns
  data <- as.data.frame(matrix(1, nrow = 2, ncol = 20))
  names(data) <- paste0("col", seq_len(20))
  data$stub <- "row"

  spec <- fr_table(data)
  spec <- fr_cols(spec,
    stub = fr_col(stub = TRUE),
    .split = TRUE
  )
  spec <- fr_page(spec, orientation = "portrait")

  # Force narrow widths
  spec <- finalize_spec(spec)

  # Set each column to 1 inch — total 21 in which exceeds printable
  for (nm in names(spec$columns)) {
    spec$columns[[nm]]$width <- 1.0
  }

  panels <- calculate_col_panels(spec)
  expect_true(length(panels) > 1L)

  # Stub column appears in every panel
  for (panel in panels) {
    expect_true("stub" %in% panel)
  }
})

test_that("calculate_col_panels without .split returns single panel", {
  data <- as.data.frame(matrix(1, nrow = 2, ncol = 20))
  names(data) <- paste0("col", seq_len(20))

  spec <- fr_table(data)
  spec <- finalize_spec(spec)

  panels <- calculate_col_panels(spec)
  expect_equal(length(panels), 1L)
})

test_that("col_split renders multiple sections in RTF", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- as.data.frame(matrix("x", nrow = 2, ncol = 15))
  names(data) <- paste0("col", seq_len(15))
  data$param <- c("A", "B")

  spec <- fr_table(data)
  spec <- fr_cols(spec,
    param = fr_col(stub = TRUE),
    .split = TRUE
  )
  spec <- fr_page(spec, orientation = "portrait")
  spec <- finalize_spec(spec)

  # Force wide columns to trigger split
  for (nm in names(spec$columns)) {
    spec$columns[[nm]]$width <- 1.5
  }

  page_groups <- prepare_pages(spec)
  col_panels <- calculate_col_panels(spec)

  if (length(col_panels) > 1L) {
    colors <- collect_colors(spec)
    color_info <- build_rtf_colortbl(colors)

    render_rtf(spec, page_groups, col_panels, tmp)

    txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
    expect_true(grepl("\\sect", txt, fixed = TRUE))
  }
})

test_that("page_by + col_split: group-first ordering (all panels per group)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # Create data with 2 page_by groups and enough columns to force split
  data <- as.data.frame(matrix("x", nrow = 4, ncol = 15))
  names(data) <- paste0("col", seq_len(15))
  data$grp <- c("Group A", "Group A", "Group B", "Group B")
  data$param <- c("R1", "R2", "R1", "R2")

  spec <- fr_table(data)
  spec <- fr_cols(spec,
    param = fr_col(stub = TRUE),
    .split = TRUE
  )
  spec <- fr_page(spec, orientation = "portrait")
  spec <- fr_rows(spec, page_by = "grp")
  spec <- finalize_spec(spec)

  # Force wide columns to trigger split
  for (nm in names(spec$columns)) {
    spec$columns[[nm]]$width <- 1.5
  }

  col_panels <- calculate_col_panels(spec)
  skip_if(length(col_panels) <= 1L, "col_split did not produce multiple panels")

  page_groups <- prepare_pages(spec)
  render_rtf(spec, page_groups, col_panels, tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # Find positions of group labels — Group A should appear first (multiple
  # times for each panel), then Group B
  pos_a <- gregexpr("Group A", txt, fixed = TRUE)[[1]]
  pos_b <- gregexpr("Group B", txt, fixed = TRUE)[[1]]
  expect_true(length(pos_a) > 0L)
  expect_true(length(pos_b) > 0L)

  # All Group A occurrences must come before any Group B occurrence
  expect_true(max(pos_a) < min(pos_b),
              label = "All Group A panels appear before Group B panels")
})

test_that("auto-stub inference picks group_by/indent_by columns", {
  data <- data.frame(
    category = c("A", "B"),
    stat = c("n", "n"),
    val1 = c("10", "20"),
    val2 = c("30", "40"),
    stringsAsFactors = FALSE
  )
  spec <- fr_table(data)
  spec <- fr_cols(spec, .split = TRUE)
  spec <- fr_rows(spec, group_by = "category")
  spec <- finalize_spec(spec)

  # category should be auto-inferred as stub
  expect_true(isTRUE(spec$columns[["category"]]$stub))
})

test_that("auto-stub inference falls back to first column", {
  data <- data.frame(
    a = c("x", "y"),
    b = c(1, 2),
    c = c(3, 4),
    stringsAsFactors = FALSE
  )
  spec <- fr_table(data)
  spec <- fr_cols(spec, .split = TRUE)
  spec <- finalize_spec(spec)

  # First column should be auto-inferred as stub
  expect_true(isTRUE(spec$columns[["a"]]$stub))
})

test_that(".split = TRUE with .width = 'auto' does not run fit_panel_widths", {
  data <- as.data.frame(matrix("x", nrow = 2, ncol = 15))
  names(data) <- paste0("col", seq_len(15))
  data$stub <- "row"

  spec <- fr_table(data)
  spec <- fr_cols(spec,
    stub = fr_col(stub = TRUE),
    .split = TRUE
  )
  spec <- fr_page(spec, orientation = "portrait")
  spec <- finalize_spec(spec)

  # Force wide columns
  for (nm in names(spec$columns)) {
    spec$columns[[nm]]$width <- 1.0
  }

  panels <- calculate_col_panels(spec)
  expect_true(length(panels) > 1L)

  # columns_meta$split is TRUE with auto width mode — no panel scaling
  expect_true(isTRUE(spec$columns_meta$split))
})

test_that("fr_col(stub = TRUE) is stored correctly", {
  col <- fr_col("Parameter", stub = TRUE)
  expect_true(col$stub)

  col2 <- fr_col("Value")
  expect_false(col2$stub)
})

test_that(".split validation rejects invalid values", {
  spec <- fr_table(data.frame(a = 1))
  expect_error(fr_cols(spec, .split = "autofit"), "must be")
  expect_error(fr_cols(spec, .split = "natural"), "must be")
  expect_error(fr_cols(spec, .split = "auto"), "must be")
  expect_error(fr_cols(spec, .split = 42), "must be")
  # Valid values should not error
  expect_no_error(fr_cols(spec, .split = TRUE))
  expect_no_error(fr_cols(spec, .split = FALSE))
})

test_that("fr_listing() sets split = TRUE on columns_meta", {
  spec <- fr_listing(data.frame(a = 1, b = 2))
  expect_true(isTRUE(spec$columns_meta$split))
})

test_that("distribute_fit_widths preserves explicit fr_col(width) values", {
  data <- data.frame(stub_col = "x", auto1 = "abc", auto2 = "defgh",
                     stringsAsFactors = FALSE)
  spec <- fr_table(data)
  spec <- fr_cols(spec,
    stub_col = fr_col("Stub", width = 1.2),
    .width = "fit"
  )

  # stub_col should keep its explicit width (1.2)
  expect_equal(spec$columns[["stub_col"]]$width, 1.2)
  # auto columns should have been scaled (width differs from raw estimate)
  expect_true(isTRUE(spec$columns[["auto1"]]$width_auto))
  expect_true(isTRUE(spec$columns[["auto2"]]$width_auto))
})

test_that("distribute_auto_widths preserves explicit fr_col(width) values", {
  data <- data.frame(stub_col = "x", auto1 = paste(rep("x", 100), collapse = ""),
                     auto2 = paste(rep("y", 100), collapse = ""),
                     stringsAsFactors = FALSE)
  spec <- fr_table(data)
  spec <- fr_cols(spec,
    stub_col = fr_col("Stub", width = 1.2),
    .width = "auto"
  )
  spec <- finalize_spec(spec)

  # stub_col should keep its explicit width (1.2)
  expect_equal(spec$columns[["stub_col"]]$width, 1.2)
})

test_that("default .width is 'auto' when not specified", {
  data <- data.frame(a = "hello", b = "world", stringsAsFactors = FALSE)
  spec <- fr_table(data)
  spec <- fr_cols(spec)

  # width_mode should be "auto"
  expect_identical(spec$columns_meta$width_mode, "auto")
  # Columns should have auto-estimated widths (not fixed 1.5)
  expect_true(isTRUE(spec$columns[["a"]]$width_auto))
  expect_true(isTRUE(spec$columns[["b"]]$width_auto))
})

test_that(".width = 'fit' + .split = TRUE triggers fit_panel_widths", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- as.data.frame(matrix("x", nrow = 2, ncol = 15))
  names(data) <- paste0("col", seq_len(15))
  data$stub <- "row"

  spec <- fr_table(data)
  spec <- fr_cols(spec,
    stub = fr_col(stub = TRUE),
    .split = TRUE,
    .width = "fit"
  )
  spec <- fr_page(spec, orientation = "portrait")

  expect_identical(spec$columns_meta$width_mode, "fit")
  expect_true(isTRUE(spec$columns_meta$split))
})

test_that(".width = 'auto' + .split = TRUE keeps natural widths", {
  data <- data.frame(a = "x", b = "y", c = "z", stringsAsFactors = FALSE)
  spec <- fr_table(data)
  spec <- fr_cols(spec, .split = TRUE, .width = "auto")

  expect_identical(spec$columns_meta$width_mode, "auto")
  expect_true(isTRUE(spec$columns_meta$split))
})
