# ──────────────────────────────────────────────────────────────────────────────
# test-render-figure.R — Tests for render-figure.R
# ──────────────────────────────────────────────────────────────────────────────

test_that("compile_xelatex_doc runs without error when xelatex available", {
  skip_if(!nzchar(Sys.which("xelatex")), "XeLaTeX not available")
  tmp_tex <- tempfile(fileext = ".tex")
  writeLines("\\documentclass{article}\\begin{document}test\\end{document}", tmp_tex)
  on.exit(unlink(c(tmp_tex, sub("\\.tex$", ".pdf", tmp_tex))), add = TRUE)
  expect_no_error(tlframe:::compile_xelatex_doc(tmp_tex))
})

test_that("render_figure_rtf creates RTF file with ggplot", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  spec <- p |>
    fr_figure() |>
    fr_titles("Figure 1", "Test Plot") |>
    fr_footnotes("Source: test data")

  finalized <- tlframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  tlframe:::render_figure_rtf(finalized, tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 0)

  # Check RTF structure
  content <- readLines(tmp, warn = FALSE)
  combined <- paste(content, collapse = "\n")
  expect_match(combined, "\\\\rtf1")
  expect_match(combined, "\\\\pngblip")
  expect_match(combined, "Figure 1")
  expect_match(combined, "Test Plot")
  expect_match(combined, "Source: test data")
})

test_that("render_figure_rtf with pagehead writes header", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  spec <- p |>
    fr_figure() |>
    fr_pagehead(left = "Study X")

  finalized <- tlframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  tlframe:::render_figure_rtf(finalized, tmp)
  expect_true(file.exists(tmp))
})

test_that("render_figure_rtf respects custom dimensions", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  spec <- p |> fr_figure(width = 5, height = 3)
  finalized <- tlframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  tlframe:::render_figure_rtf(finalized, tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 0)
})

test_that("render_figure_pdf generates LaTeX for ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if(!nzchar(Sys.which("xelatex")), "XeLaTeX not available")

  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  spec <- p |>
    fr_figure() |>
    fr_titles("Figure PDF Test") |>
    fr_footnotes("Note")

  finalized <- tlframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  # May fail to compile but shouldn't error in R

  expect_no_error(tlframe:::render_figure_pdf(finalized, tmp))
})

test_that("render_figure_rtf with no titles/footnotes", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  spec <- p |> fr_figure()
  finalized <- tlframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  tlframe:::render_figure_rtf(finalized, tmp)
  expect_true(file.exists(tmp))
})

test_that("render_figure_rtf with landscape orientation", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  spec <- p |>
    fr_figure() |>
    fr_page(orientation = "landscape")

  finalized <- tlframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  tlframe:::render_figure_rtf(finalized, tmp)
  content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(content, "lndscpsxn")
})
