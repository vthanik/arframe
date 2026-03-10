# ──────────────────────────────────────────────────────────────────────────────
# test-api-cols.R — Tests for fr_cols() in api-cols.R
# ──────────────────────────────────────────────────────────────────────────────

df_simple <- data.frame(
  arm    = c("Placebo", "Drug"),
  n      = c(45L, 45L),
  pct    = c(50.0, 50.0),
  stringsAsFactors = FALSE
)

# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — .width modes
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols with .width = 'auto' estimates column widths from content", {
  spec <- fr_table(df_simple) |>
    fr_cols(.width = "auto")

  # All columns should have numeric widths (auto-estimated)
  for (col in spec$columns) {
    expect_true(is.numeric(col$width), info = paste("Column:", col$id))
    expect_true(col$width > 0, info = paste("Column:", col$id))
  }
})

test_that("fr_cols with .width = 'fit' fills printable width exactly", {
  spec <- fr_table(df_simple) |>
    fr_cols(.width = "fit")

  printable <- printable_area_inches(spec$page)[["width"]]
  vis <- Filter(function(c) !isFALSE(c$visible), spec$columns)
  total <- sum(vapply(vis, function(c) c$width, numeric(1)))
  expect_equal(total, printable, tolerance = 0.01)
})

test_that("fr_cols with .width = 'equal' distributes width equally among unfixed columns", {
  spec <- fr_table(df_simple) |>
    fr_cols(.width = "equal")

  vis <- Filter(function(c) !isFALSE(c$visible), spec$columns)
  widths <- vapply(vis, function(c) c$width, numeric(1))

  # All columns should have equal width since none have explicit widths

  expect_true(all(abs(widths - widths[1]) < 0.001))
})

test_that("fr_cols .width = 'equal' preserves fixed-width columns", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      arm = fr_col("Arm", width = 2.5),
      .width = "equal"
    )

  expect_equal(spec$columns[["arm"]]$width, 2.5)

  # Remaining columns should have equal widths (different from 2.5)
  n_width <- spec$columns[["n"]]$width
  pct_width <- spec$columns[["pct"]]$width
  expect_equal(n_width, pct_width, tolerance = 0.001)
})

test_that("fr_cols with percentage .width sets fr_pct widths", {
  spec <- fr_table(df_simple) |>
    fr_cols(.width = "25%")

  for (col in spec$columns) {
    expect_true(inherits(col$width, "fr_pct"),
                info = paste("Column:", col$id))
    expect_equal(as.numeric(col$width), 0.25)
  }
})

test_that("fr_cols with numeric .width sets fixed width", {
  spec <- fr_table(df_simple) |>
    fr_cols(.width = 2.0)

  for (col in spec$columns) {
    expect_equal(col$width, 2.0, info = paste("Column:", col$id))
  }
})

test_that("fr_cols with NULL .width uses auto mode", {
  spec <- fr_table(df_simple) |>
    fr_cols()

  # Default is "auto" — columns get content-based widths, not fixed 1.5
  expect_identical(spec$columns_meta$width_mode, "auto")
  for (col in spec$columns) {
    expect_true(is.numeric(col$width) && col$width > 0, info = paste("Column:", col$id))
    expect_true(isTRUE(col$width_auto), info = paste("Column:", col$id))
  }
})

test_that("fr_cols per-column width overrides .width default", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      arm = fr_col("Arm", width = 3.0),
      .width = 1.0
    )

  expect_equal(spec$columns[["arm"]]$width, 3.0)
  expect_equal(spec$columns[["n"]]$width, 1.0)
  expect_equal(spec$columns[["pct"]]$width, 1.0)
})

test_that("fr_cols per-column percentage width works", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      arm = fr_col("Arm", width = "30%")
    )

  expect_true(inherits(spec$columns[["arm"]]$width, "fr_pct"))
  expect_equal(as.numeric(spec$columns[["arm"]]$width), 0.30)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — .width validation errors
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols rejects invalid .width string", {
  expect_error(
    fr_table(df_simple) |> fr_cols(.width = "banana"),
    class = "rlang_error"
  )
})

test_that("fr_cols rejects negative numeric .width", {
  expect_error(
    fr_table(df_simple) |> fr_cols(.width = -1),
    class = "rlang_error"
  )
})

test_that("fr_cols rejects .width percentage over 100%", {
  expect_error(
    fr_table(df_simple) |> fr_cols(.width = "150%"),
    class = "rlang_error"
  )
})

test_that("fr_cols rejects .width = '0%'", {
  expect_error(
    fr_table(df_simple) |> fr_cols(.width = "0%"),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — .align
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols .align overrides type-detection for all columns", {
  spec <- fr_table(df_simple) |>
    fr_cols(.align = "center")

  for (col in spec$columns) {
    expect_equal(col$align, "center", info = paste("Column:", col$id))
  }
})

test_that("fr_cols NULL .align auto-detects: numeric = right, character = left", {
  spec <- fr_table(df_simple) |>
    fr_cols()

  expect_equal(spec$columns[["arm"]]$align, "left")
  expect_equal(spec$columns[["n"]]$align, "right")
  expect_equal(spec$columns[["pct"]]$align, "right")
})

test_that("fr_cols rejects invalid .align value", {
  expect_error(
    fr_table(df_simple) |> fr_cols(.align = "diagonal"),
    class = "rlang_error"
  )
})

test_that("fr_cols .align = 'decimal' is accepted", {
  spec <- fr_table(df_simple) |>
    fr_cols(.align = "decimal")

  for (col in spec$columns) {
    expect_equal(col$align, "decimal")
  }
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — .label_fn
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols .label_fn transforms auto-generated labels", {
  spec <- fr_table(df_simple) |>
    fr_cols(.label_fn = toupper)

  expect_equal(spec$columns[["arm"]]$label, "ARM")
  expect_equal(spec$columns[["n"]]$label, "N")
  expect_equal(spec$columns[["pct"]]$label, "PCT")
})

test_that("fr_cols .label_fn does NOT affect explicitly-set labels", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      arm = fr_col("Treatment Arm"),
      .label_fn = toupper
    )

  # Explicit label is preserved

  expect_equal(spec$columns[["arm"]]$label, "Treatment Arm")
  # Auto-generated labels get the transform
  expect_equal(spec$columns[["n"]]$label, "N")
  expect_equal(spec$columns[["pct"]]$label, "PCT")
})

test_that("fr_cols .label_fn works with rlang-style lambda", {
  spec <- fr_table(df_simple) |>
    fr_cols(.label_fn = ~ gsub("_", " ", .x))

  # No underscores in df_simple names, but the function should still run
  expect_equal(spec$columns[["arm"]]$label, "arm")
  expect_equal(spec$columns[["pct"]]$label, "pct")
})

test_that("fr_cols .label_fn does NOT affect .list labels", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      .list = c(arm = "My Arm Label"),
      .label_fn = toupper
    )

  # .list label wins
  expect_equal(spec$columns[["arm"]]$label, "My Arm Label")
  # Auto-generated labels still get .label_fn
  expect_equal(spec$columns[["n"]]$label, "N")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — .list
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols .list with character vector sets labels", {
  spec <- fr_table(df_simple) |>
    fr_cols(.list = c(arm = "Treatment", pct = "Percent (%)"))

  expect_equal(spec$columns[["arm"]]$label, "Treatment")
  expect_equal(spec$columns[["pct"]]$label, "Percent (%)")
  expect_equal(spec$columns[["n"]]$label, "n")  # unchanged
})

test_that("fr_cols .list with named list works", {
  spec <- fr_table(df_simple) |>
    fr_cols(.list = list(arm = "Treatment", n = "Count"))

  expect_equal(spec$columns[["arm"]]$label, "Treatment")
  expect_equal(spec$columns[["n"]]$label, "Count")
})

test_that("fr_cols .list overrides explicit labels from ...", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      arm = fr_col("Arm Col"),
      .list = c(arm = "List Wins")
    )

  expect_equal(spec$columns[["arm"]]$label, "List Wins")
})

test_that("fr_cols .list rejects unnamed input", {
  expect_error(
    fr_table(df_simple) |> fr_cols(.list = c("Treatment", "Percent")),
    class = "rlang_error"
  )
})

test_that("fr_cols .list rejects non-character non-list input", {
  expect_error(
    fr_table(df_simple) |> fr_cols(.list = 42),
    class = "rlang_error"
  )
})

test_that("fr_cols .list errors on column not in data", {
  expect_error(
    fr_table(df_simple) |> fr_cols(.list = c(nonexistent = "Label")),
    "not found"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — named ... arguments
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols accepts character label as shorthand", {
  spec <- fr_table(df_simple) |>
    fr_cols(arm = "Treatment Arm")

  expect_equal(spec$columns[["arm"]]$label, "Treatment Arm")
  expect_s3_class(spec$columns[["arm"]], "fr_col")
})

test_that("fr_cols errors on column name not in data", {
  expect_error(
    fr_table(df_simple) |> fr_cols(bogus = "Label"),
    "not found"
  )
})

test_that("fr_cols errors on unnamed non-formula argument", {
  expect_error(
    fr_table(df_simple) |> fr_cols(fr_col("Test")),
    class = "rlang_error"
  )
})

test_that("fr_cols errors when spec is not fr_spec", {
  expect_error(
    fr_cols(data.frame(x = 1)),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — tidyselect formulas
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols tidyselect formula applies fr_col to matched columns", {
  skip_if_not_installed("tidyselect")

  spec <- fr_table(df_simple) |>
    fr_cols(
      c(n, pct) ~ fr_col(align = "center")
    )

  expect_equal(spec$columns[["n"]]$align, "center")
  expect_equal(spec$columns[["pct"]]$align, "center")
  # arm not matched — should get default left
  expect_equal(spec$columns[["arm"]]$align, "left")
})

test_that("fr_cols tidyselect formula with character label", {
  skip_if_not_installed("tidyselect")

  spec <- fr_table(df_simple) |>
    fr_cols(
      c(n, pct) ~ "Numeric Column"
    )

  expect_equal(spec$columns[["n"]]$label, "Numeric Column")
  expect_equal(spec$columns[["pct"]]$label, "Numeric Column")
})

test_that("fr_cols tidyselect with starts_with helper", {
  skip_if_not_installed("tidyselect")

  df <- data.frame(trt_a = 1, trt_b = 2, other = 3)
  spec <- fr_table(df) |>
    fr_cols(
      starts_with("trt") ~ fr_col(width = 2.0, align = "right")
    )

  expect_equal(spec$columns[["trt_a"]]$width, 2.0)
  expect_equal(spec$columns[["trt_b"]]$width, 2.0)
  expect_equal(spec$columns[["trt_a"]]$align, "right")
  # 'other' not matched — auto-estimated width
  expect_true(is.numeric(spec$columns[["other"]]$width))
  expect_true(isTRUE(spec$columns[["other"]]$width_auto))
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — replaces on repeated calls
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols replaces entire column config on repeated calls", {
  spec <- fr_table(df_simple) |>
    fr_cols(arm = fr_col("First Label", width = 3.0)) |>
    fr_cols(arm = fr_col("Second Label", width = 1.0))

  expect_equal(spec$columns[["arm"]]$label, "Second Label")
  expect_equal(spec$columns[["arm"]]$width, 1.0)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — visible = FALSE
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols visible = FALSE is stored on column", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      arm = fr_col("Arm", visible = FALSE)
    )

  expect_false(spec$columns[["arm"]]$visible)
  # Other columns should still be visible (NULL or TRUE)
  expect_true(is.null(spec$columns[["n"]]$visible) ||
                isTRUE(spec$columns[["n"]]$visible))
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — column order preserved from data frame
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols preserves data frame column order", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      pct = "Percent",
      arm = "Treatment",
      n   = "Count"
    )

  expect_equal(names(spec$columns), c("arm", "n", "pct"))
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — combination of .list, .label_fn, explicit
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols label priority: .list > explicit > .label_fn > column name", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      n = fr_col("Explicit N"),
      .list = c(n = "List N"),
      .label_fn = toupper
    )

  # .list wins for 'n'
  expect_equal(spec$columns[["n"]]$label, "List N")
  # .label_fn applies to 'arm' and 'pct' (not explicitly set, not in .list)
  expect_equal(spec$columns[["arm"]]$label, "ARM")
  expect_equal(spec$columns[["pct"]]$label, "PCT")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — auto width with built-in dataset
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols .width = 'auto' works with tbl_demog", {
  spec <- fr_table(tbl_demog) |>
    fr_cols(.width = "auto")

  for (col in spec$columns) {
    expect_true(is.numeric(col$width))
    expect_true(col$width >= 0.5)  # minimum clamp
    expect_true(col$width <= 5.0)  # maximum clamp
  }
})

test_that("fr_cols .width = 'fit' with tbl_demog fills page", {
  spec <- fr_table(tbl_demog) |>
    fr_cols(.width = "fit")

  printable <- printable_area_inches(spec$page)[["width"]]
  vis <- Filter(function(c) !isFALSE(c$visible), spec$columns)
  total <- sum(vapply(vis, function(c) c$width, numeric(1)))
  expect_equal(total, printable, tolerance = 0.01)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols() — edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols handles single-column data frame", {
  df <- data.frame(x = 1:5)
  spec <- fr_table(df) |>
    fr_cols(x = fr_col("Count", width = 2.0))

  expect_equal(length(spec$columns), 1L)
  expect_equal(spec$columns[["x"]]$label, "Count")
  expect_equal(spec$columns[["x"]]$width, 2.0)
})

test_that("fr_cols with all NAs in a column gets auto width", {
  df <- data.frame(x = c(NA_character_, NA_character_), y = c("a", "b"))
  spec <- fr_table(df) |>
    fr_cols(.width = "auto")

  expect_true(is.numeric(spec$columns[["x"]]$width))
  expect_true(spec$columns[["x"]]$width >= 0.5)
})

test_that("fr_cols with empty strings in a column gets auto width", {
  df <- data.frame(x = c("", ""), y = c("hello", "world"))
  spec <- fr_table(df) |>
    fr_cols(.width = "auto")

  expect_true(is.numeric(spec$columns[["x"]]$width))
  expect_true(spec$columns[["x"]]$width >= 0.5)
})

test_that("fr_cols handles data with many columns", {
  df <- as.data.frame(setNames(as.list(1:20), paste0("col_", 1:20)))
  spec <- fr_table(df) |>
    fr_cols(.width = "auto")

  expect_equal(length(spec$columns), 20L)
})


