# ──────────────────────────────────────────────────────────────────────────────
# test-style-priority.R — Tests for style cascade: col < row < cell
#
# Verifies that the style application order (col → row → cell, later wins)
# is correctly enforced by checking rendered HTML output for inline styles.
# ──────────────────────────────────────────────────────────────────────────────


render_html_string <- function(spec) {
  tf <- tempfile(fileext = ".html")
  on.exit(unlink(tf), add = TRUE)
  fr_render(spec, tf)
  paste(readLines(tf, warn = FALSE), collapse = "\n")
}


test_that("cell style overrides row style for same property", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(group = fr_col(visible = FALSE)) |>
    fr_styles(
      fr_row_style(rows = 1L, background = "#AAAAAA"),
      fr_style(rows = 1L, cols = "placebo", background = "#FFFFFF")
    )

  html <- render_html_string(spec)
  # The placebo cell in row 1 should have #FFFFFF (cell override), not #AAAAAA
  expect_true(grepl("#FFFFFF", html))
})


test_that("row style overrides col style for same property", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(group = fr_col(visible = FALSE)) |>
    fr_styles(
      fr_col_style(cols = "placebo", background = "#AAAAAA"),
      fr_row_style(rows = 1L, background = "#FFFFFF")
    )

  html <- render_html_string(spec)
  # Row 1 should have #FFFFFF (row override), not #AAAAAA
  expect_true(grepl("#FFFFFF", html))
})


test_that("later styles in same scope win over earlier ones", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(group = fr_col(visible = FALSE)) |>
    fr_styles(
      fr_row_style(rows = 1L, background = "#AAAAAA"),
      fr_row_style(rows = 1L, background = "#BBBBBB")
    )

  html <- render_html_string(spec)
  # Later style (#BBBBBB) should win
  expect_true(grepl("#BBBBBB", html))
})


test_that("unset cell properties inherit from row", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(group = fr_col(visible = FALSE)) |>
    fr_styles(
      fr_row_style(rows = 1L, bold = TRUE, background = "#EEEEEE"),
      fr_style(rows = 1L, cols = "placebo", background = "#FFFFFF")
    )

  html <- render_html_string(spec)
  # Cell-level background (#FFFFFF) present
  expect_true(grepl("#FFFFFF", html))
  # Bold inherited from row (font-weight:bold appears for row 1 cells)
  expect_true(grepl("font-weight:bold", html))
})


test_that("group_style has lower precedence than fr_styles", {
  spec <- data.frame(
    variable = c("Sex", "Sex", "Age", "Age"),
    stat = c("Female", "Male", "Mean", "Median"),
    value = c("27", "18", "75", "74"),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_cols(variable = fr_col(visible = FALSE)) |>
    fr_rows(
      group_by = list(cols = "variable", label = "stat"),
      group_style = list(bold = TRUE, background = "#AAAAAA")
    ) |>
    fr_styles(
      fr_row_style(rows = "group_headers", background = "#FFFFFF")
    )

  fspec <- finalize_spec(spec)

  gs_idx <- NULL
  fs_idx <- NULL
  for (i in seq_along(fspec$cell_styles)) {
    s <- fspec$cell_styles[[i]]
    if (identical(s$background, "#AAAAAA") && isTRUE(s$bold)) gs_idx <- i
    if (identical(s$background, "#FFFFFF")) fs_idx <- i
  }
  expect_false(is.null(gs_idx))
  expect_false(is.null(fs_idx))
  expect_gt(fs_idx, gs_idx)
})
