# ──────────────────────────────────────────────────────────────────────────────
# test-group-style.R — Tests for group_style and rows = "group_headers"
# ──────────────────────────────────────────────────────────────────────────────

# ── Helper data ──────────────────────────────────────────────────────────────

make_label_data <- function() {
  data.frame(
    variable = c("Sex", "Sex", "Age", "Age", "Age"),
    stat = c("Female", "Male", "Mean (SD)", "Median", "Min, Max"),
    value = c("27 (60.0)", "18 (40.0)", "75.0 (6.8)", "74.0", "65, 88"),
    stringsAsFactors = FALSE
  )
}

make_leaf_data <- function() {
  data.frame(
    soc = c(
      "GI disorders", "GI disorders", "GI disorders",
      "Nervous system", "Nervous system"
    ),
    pt = c("Nausea", "Vomiting", "Diarrhoea", "Headache", "Dizziness"),
    total = c("24 (17.8)", "18 (13.3)", "12 (8.9)", "30 (22.2)", "15 (11.1)"),
    stringsAsFactors = FALSE
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# group_style on fr_rows()
# ══════════════════════════════════════════════════════════════════════════════

test_that("group_style bold applies to label group headers", {
  spec <- make_label_data() |>
    fr_table() |>
    fr_cols(variable = fr_col(visible = FALSE)) |>
    fr_rows(
      group_by = list(cols = "variable", label = "stat"),
      group_style = list(bold = TRUE)
    )

  expect_equal(spec$body$group_style, list(bold = TRUE))

  fspec <- finalize_spec(spec)
  # Header rows should be at positions 1 (Sex) and 4 (Age)
  gs <- fspec$cell_styles[[1L]]
  expect_true(gs$bold)
  expect_equal(gs$rows, c(1L, 4L))
})


test_that("group_style bold applies to leaf hierarchy non-leaf rows", {
  spec <- make_leaf_data() |>
    fr_table() |>
    fr_rows(
      group_by = list(cols = c("soc", "pt"), leaf = "pt"),
      group_style = list(bold = TRUE)
    )

  fspec <- finalize_spec(spec)
  # SOC header rows should be bold
  gs <- fspec$cell_styles[[1L]]
  expect_true(gs$bold)

  # Verify they target SOC rows (row_level == "soc")
  soc_rows <- which(fspec$data[["__row_level__"]] == "soc")
  expect_equal(gs$rows, soc_rows)
})


test_that("group_style per-level applies only to named levels", {
  spec <- make_leaf_data() |>
    fr_table() |>
    fr_rows(
      group_by = list(cols = c("soc", "pt"), leaf = "pt"),
      group_style = list(soc = list(bold = TRUE, background = "#F0F0F0"))
    )

  fspec <- finalize_spec(spec)
  gs <- fspec$cell_styles[[1L]]
  expect_true(gs$bold)
  expect_equal(gs$background, "#F0F0F0")

  # Only SOC rows — not PT rows
  soc_rows <- which(fspec$data[["__row_level__"]] == "soc")
  expect_equal(gs$rows, soc_rows)
})


test_that("group_style with color resolves hex and CSS names", {
  spec <- make_label_data() |>
    fr_table() |>
    fr_cols(variable = fr_col(visible = FALSE)) |>
    fr_rows(
      group_by = list(cols = "variable", label = "stat"),
      group_style = list(bold = TRUE, color = "navy")
    )

  # resolve_color should have converted "navy" to hex
  expect_equal(spec$body$group_style$color, "#000080")
})


test_that("group_style without group_label or leaf is silently ignored", {
  spec <- make_label_data() |>
    fr_table() |>
    fr_rows(group_style = list(bold = TRUE))

  fspec <- finalize_spec(spec)
  # No group headers to style — should not error

  # cell_styles should not contain a group_style-derived entry
  has_bold_all <- any(vapply(fspec$cell_styles, function(s) {
    isTRUE(s$bold) && length(s$rows) > 0L
  }, logical(1)))
  expect_false(has_bold_all)
})


test_that("group_style with invalid keys errors", {
  expect_error(
    make_label_data() |>
      fr_table() |>
      fr_rows(group_style = list(foobar = TRUE)),
    "Unknown style"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# rows = "group_headers" in fr_styles()
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_row_style rows=group_headers targets all group headers", {
  spec <- make_label_data() |>
    fr_table() |>
    fr_cols(variable = fr_col(visible = FALSE)) |>
    fr_rows(group_by = list(cols = "variable", label = "stat")) |>
    fr_styles(
      fr_row_style(rows = "group_headers", bold = TRUE, background = "#E8E8E8")
    )

  # Before finalize, rows should be the string sentinel
  expect_equal(spec$cell_styles[[1L]]$rows, "group_headers")

  fspec <- finalize_spec(spec)
  # After finalize, should be resolved to integer positions
  resolved <- NULL
  for (s in fspec$cell_styles) {
    if (isTRUE(s$bold) && identical(s$background, "#E8E8E8")) {
      resolved <- s
      break
    }
  }
  expect_false(is.null(resolved))
  expect_equal(resolved$rows, c(1L, 4L))
})


test_that("fr_row_style rows=group_headers:soc targets specific level", {
  spec <- make_leaf_data() |>
    fr_table() |>
    fr_rows(group_by = list(cols = c("soc", "pt"), leaf = "pt")) |>
    fr_styles(
      fr_row_style(rows = "group_headers:soc", bold = TRUE)
    )

  fspec <- finalize_spec(spec)
  # Find the resolved style
  resolved <- NULL
  for (s in fspec$cell_styles) {
    if (isTRUE(s$bold) && !is.character(s$rows)) {
      resolved <- s
      break
    }
  }
  expect_false(is.null(resolved))

  soc_rows <- which(fspec$data[["__row_level__"]] == "soc")
  expect_equal(resolved$rows, soc_rows)
})


test_that("fr_style rows=group_headers with cols targets cell intersection", {
  spec <- make_label_data() |>
    fr_table() |>
    fr_cols(variable = fr_col(visible = FALSE)) |>
    fr_rows(group_by = list(cols = "variable", label = "stat")) |>
    fr_styles(
      fr_style(rows = "group_headers", cols = "value", color = "#CC0000")
    )

  fspec <- finalize_spec(spec)
  # Find the style targeting value column
  resolved <- NULL
  for (s in fspec$cell_styles) {
    if (identical(s$color, "#CC0000")) {
      resolved <- s
      break
    }
  }
  expect_false(is.null(resolved))
  expect_equal(resolved$rows, c(1L, 4L))
  expect_equal(resolved$cols, "value")
})


# ══════════════════════════════════════════════════════════════════════════════
# Precedence
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_styles overrides group_style for same property", {
  spec <- make_label_data() |>
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

  # group_style is prepended (lower precedence), fr_styles appended (higher)
  # The fr_styles entry should come AFTER the group_style entry
  gs_idx <- NULL
  fs_idx <- NULL
  for (i in seq_along(fspec$cell_styles)) {
    s <- fspec$cell_styles[[i]]
    if (identical(s$background, "#AAAAAA") && isTRUE(s$bold)) gs_idx <- i
    if (identical(s$background, "#FFFFFF")) fs_idx <- i
  }
  expect_false(is.null(gs_idx))
  expect_false(is.null(fs_idx))
  # fr_styles entry should come after group_style entry (higher index = wins)
  expect_gt(fs_idx, gs_idx)
})


test_that("group_style from fr_theme is inherited", {
  fr_theme(group_style = list(bold = TRUE))
  on.exit(fr_theme_reset(), add = TRUE)

  spec <- make_label_data() |>
    fr_table() |>
    fr_cols(variable = fr_col(visible = FALSE)) |>
    fr_rows(group_by = list(cols = "variable", label = "stat"))

  expect_true(spec$body$group_style$bold)

  fspec <- finalize_spec(spec)
  gs <- fspec$cell_styles[[1L]]
  expect_true(gs$bold)
  expect_equal(gs$rows, c(1L, 4L))
})


test_that("per-table group_style overrides theme group_style", {
  fr_theme(group_style = list(bold = TRUE, background = "#AAAAAA"))
  on.exit(fr_theme_reset(), add = TRUE)

  spec <- make_label_data() |>
    fr_table() |>
    fr_cols(variable = fr_col(visible = FALSE)) |>
    fr_rows(
      group_by = list(cols = "variable", label = "stat"),
      group_style = list(bold = TRUE, background = "#FFFFFF")
    )

  # Per-table overrides theme
  expect_equal(spec$body$group_style$background, "#FFFFFF")
})


# ══════════════════════════════════════════════════════════════════════════════
# page_by styling via fr_styles()
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_row_style rows=page_by stores in page_by_styles", {
  spec <- tbl_vs[tbl_vs$timepoint == "Week 24", ] |>
    fr_table() |>
    fr_rows(page_by = "param") |>
    fr_styles(fr_row_style(rows = "page_by", bold = TRUE))
  expect_length(spec$page_by_styles, 1L)
  expect_true(spec$page_by_styles[[1L]]$bold)
})


test_that("page_by bold renders in HTML output", {
  spec <- tbl_vs[tbl_vs$timepoint == "Week 24", ] |>
    fr_table() |>
    fr_rows(page_by = "param") |>
    fr_styles(fr_row_style(rows = "page_by", bold = TRUE))
  tf <- tempfile(fileext = ".html")
  on.exit(unlink(tf), add = TRUE)
  fr_render(spec, tf)
  content <- paste(readLines(tf), collapse = "\n")
  expect_true(grepl("font-weight: bold", content))
})


test_that("HTML page_by label is not bold by default", {
  spec <- tbl_vs[tbl_vs$timepoint == "Week 24", ] |>
    fr_table() |>
    fr_rows(page_by = "param")
  tf <- tempfile(fileext = ".html")
  on.exit(unlink(tf), add = TRUE)
  fr_render(spec, tf)
  content <- paste(readLines(tf), collapse = "\n")
  # .ar-page-by CSS should have font-weight: normal
  expect_true(grepl("ar-page-by.*font-weight: normal", content))
  # page-by div should NOT have inline bold style
  expect_false(grepl("ar-page-by.*font-weight: bold", content))
})


test_that("page_by bold renders in RTF output", {
  spec <- tbl_vs[tbl_vs$timepoint == "Week 24", ] |>
    fr_table() |>
    fr_rows(page_by = "param") |>
    fr_styles(fr_row_style(rows = "page_by", bold = TRUE))
  tf <- tempfile(fileext = ".rtf")
  on.exit(unlink(tf), add = TRUE)
  fr_render(spec, tf)
  content <- paste(readLines(tf, warn = FALSE), collapse = "\n")
  expect_true(grepl("\\\\b ", content))
})


test_that("page_by style from fr_theme is inherited", {
  fr_theme(page_by_style = list(bold = TRUE))
  on.exit(fr_theme_reset(), add = TRUE)
  spec <- tbl_vs[tbl_vs$timepoint == "Week 24", ] |>
    fr_table() |>
    fr_rows(page_by = "param")
  expect_length(spec$page_by_styles, 1L)
  expect_true(spec$page_by_styles[[1L]]$bold)
})


test_that("rows=page_by without page_by config is silently stored", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_styles(fr_row_style(rows = "page_by", bold = TRUE))
  expect_length(spec$page_by_styles, 1L)
})
