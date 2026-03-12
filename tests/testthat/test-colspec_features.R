df_simple <- data.frame(
  arm = c("Placebo", "Drug"),
  n = c(45L, 45L),
  pct = c(50.0, 50.0),
  stringsAsFactors = FALSE
)

test_that("fr_cols accepts .list for pre-formatted labels", {
  spec <- fr_table(df_simple) |>
    fr_cols(.list = c("arm" = "Treatment\n(N=100)", "pct" = "Percent"))

  expect_equal(spec$columns[["arm"]]$label, "Treatment\n(N=100)")
  expect_equal(spec$columns[["pct"]]$label, "Percent")
})

test_that("fr_cols .n stores bulk N-counts on columns_meta", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      arm = fr_col("Treatment"),
      pct = fr_col("Percent"),
      .n = c("Treatment" = 45, "Percent" = 45),
      .n_format = "{label}\n(N={n})"
    )

  expect_equal(spec$columns_meta$n, c("Treatment" = 45, "Percent" = 45))
  expect_equal(spec$columns_meta$n_format, "{label}\n(N={n})")
})

test_that("fr_header(align=) sets uniform header alignment", {
  spec <- fr_table(df_simple) |>
    fr_header(align = "center")

  expect_equal(spec$header$align, "center")
})

test_that("fr_col header_align overrides fr_header(align=)", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      arm = fr_col("Arm", header_align = "left")
    ) |>
    fr_header(align = "center")

  # Per-column header_align should survive; fr_header sets the default
  expect_equal(spec$columns[["arm"]]$header_align, "left")
  expect_equal(spec$header$align, "center")
})

test_that("build_header_cell_grid uses header_align over align", {
  spec <- fr_table(df_simple) |>
    fr_cols(
      arm = fr_col("Arm", align = "left", header_align = "center"),
      n = fr_col("N", align = "right")
    )

  vis <- Filter(function(c) !isFALSE(c$visible), spec$columns)
  grid <- build_header_cell_grid(vis, spec$cell_styles, spec$page, 1L)

  # arm: header_align = "center" should win over align = "left"
  expect_equal(unname(grid$align[grid$col_name == "arm"]), "center")
  # n: no header_align, should use align = "right"
  expect_equal(unname(grid$align[grid$col_name == "n"]), "right")
})

test_that("fr_cols tidyselect formulas correctly expand to matched columns", {
  # Requires tidyselect
  skip_if_not_installed("tidyselect")

  spec <- fr_table(df_simple) |>
    fr_cols(
      c(n, pct) ~ fr_col(width = 2.5, align = "right"),
      starts_with("a") ~ "Treatment Arm"
    )

  expect_equal(spec$columns[["n"]]$width, 2.5)
  expect_equal(spec$columns[["n"]]$align, "right")
  expect_equal(spec$columns[["pct"]]$width, 2.5)
  expect_equal(spec$columns[["pct"]]$align, "right")
  expect_equal(spec$columns[["arm"]]$label, "Treatment Arm")
})
