# ──────────────────────────────────────────────────────────────────────────────
# test-render-figure.R — Tests for render-figure.R
# ──────────────────────────────────────────────────────────────────────────────

test_that("compile_xelatex_doc runs without error when xelatex available", {
  skip_if(!nzchar(Sys.which("xelatex")), "XeLaTeX not available")
  tmp_tex <- tempfile(fileext = ".tex")
  writeLines(
    "\\documentclass{article}\\begin{document}test\\end{document}",
    tmp_tex
  )
  on.exit(unlink(c(tmp_tex, sub("\\.tex$", ".pdf", tmp_tex))), add = TRUE)
  expect_no_error(arframe:::compile_xelatex_doc(tmp_tex))
})

test_that("render_figure_rtf creates RTF file with ggplot", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  spec <- p |>
    fr_figure() |>
    fr_titles("Figure 1", "Test Plot") |>
    fr_footnotes("Source: test data")

  finalized <- arframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  arframe:::render_figure_rtf(finalized, tmp)
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

  finalized <- arframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  arframe:::render_figure_rtf(finalized, tmp)
  expect_true(file.exists(tmp))
})

test_that("render_figure_rtf respects custom dimensions", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  spec <- p |> fr_figure(width = 5, height = 3)
  finalized <- arframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  arframe:::render_figure_rtf(finalized, tmp)
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

  finalized <- arframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  # May fail to compile but shouldn't error in R

  expect_no_error(arframe:::render_figure_pdf(finalized, tmp))
})

test_that("render_figure_rtf with no titles/footnotes", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  spec <- p |> fr_figure()
  finalized <- arframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  arframe:::render_figure_rtf(finalized, tmp)
  expect_true(file.exists(tmp))
})

test_that("render_figure_rtf with landscape orientation", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  spec <- p |>
    fr_figure() |>
    fr_page(orientation = "landscape")

  finalized <- arframe:::finalize_spec(spec)
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  arframe:::render_figure_rtf(finalized, tmp)
  content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(content, "lndscpsxn")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_figure — public API dispatch via fr_render
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_figure renders via public fr_render API", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  p |>
    fr_figure() |>
    fr_titles("Figure 1") |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
  content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(content, "\\\\rtf1")
  expect_match(content, "Figure 1", fixed = TRUE)
})


test_that("fr_figure full pipeline with titles/footnotes/pagehead via fr_render", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  tmp_rtf <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp_rtf), add = TRUE)

  # Full pipeline through the public API with all chrome
  p |>
    fr_figure(width = 4, height = 3) |>
    fr_titles("Figure 14.1.1", "Scatter Plot of X vs Y") |>
    fr_footnotes("Source: test data", "Program: test-render-figure.R") |>
    fr_pagehead(left = "Study ABC") |>
    fr_page(orientation = "landscape") |>
    fr_render(tmp_rtf)

  expect_true(file.exists(tmp_rtf))
  expect_gt(file.info(tmp_rtf)$size, 0)

  content <- paste(readLines(tmp_rtf, warn = FALSE), collapse = "\n")

  # RTF document structure
  expect_match(content, "\\\\rtf1")
  expect_match(content, "\\\\pngblip")

  # Titles rendered in output
  expect_match(content, "Figure 14.1.1", fixed = TRUE)
  expect_match(content, "Scatter Plot of X vs Y", fixed = TRUE)

  # Footnotes rendered in output
  expect_match(content, "Source: test data", fixed = TRUE)
  expect_match(content, "Program: test-render-figure.R", fixed = TRUE)

  # Landscape orientation applied
  expect_match(content, "lndscpsxn")
})


# ══════════════════════════════════════════════════════════════════════════════
# Multi-page figure rendering
# ══════════════════════════════════════════════════════════════════════════════

test_that("multi-page figure RTF creates multiple pages with \\sect", {
  skip_if_not_installed("ggplot2")

  p1 <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p2 <- ggplot2::ggplot(data.frame(x = 1:5, y = 5:1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  list(p1, p2) |>
    fr_figure() |>
    fr_titles("Figure 1") |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
  content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")

  # Two PNG images embedded
  matches <- gregexpr("\\\\pngblip", content)[[1]]
  expect_equal(length(matches), 2L)

  # Section break between pages
  expect_match(content, "\\\\sect")

  # Title appears twice (once per page)
  title_matches <- gregexpr("Figure 1", content, fixed = TRUE)[[1]]
  expect_equal(length(title_matches), 2L)
})

test_that("multi-page figure RTF resolves meta tokens in titles", {
  skip_if_not_installed("ggplot2")

  p1 <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p2 <- ggplot2::ggplot(data.frame(x = 1:5, y = 5:1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  meta <- data.frame(
    subgroup = c("Adults", "Pediatrics"),
    stringsAsFactors = FALSE
  )

  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  list(p1, p2) |>
    fr_figure(meta = meta) |>
    fr_titles("KM Curve", "Subgroup: {subgroup}") |>
    fr_render(tmp)

  content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")

  # Per-page token resolution
  expect_match(content, "Subgroup: Adults", fixed = TRUE)
  expect_match(content, "Subgroup: Pediatrics", fixed = TRUE)

  # Raw token should NOT appear
  expect_false(grepl("{subgroup}", content, fixed = TRUE))
})

test_that("multi-page figure RTF resolves meta tokens in footnotes", {
  skip_if_not_installed("ggplot2")

  p1 <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p2 <- ggplot2::ggplot(data.frame(x = 1:3, y = 3:1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  meta <- data.frame(n = c(80, 55), stringsAsFactors = FALSE)

  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  list(p1, p2) |>
    fr_figure(meta = meta) |>
    fr_footnotes("N = {n}") |>
    fr_render(tmp)

  content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(content, "N = 80", fixed = TRUE)
  expect_match(content, "N = 55", fixed = TRUE)
})

test_that("multi-page figure RTF with no meta works (no token resolution)", {
  skip_if_not_installed("ggplot2")

  p1 <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p2 <- ggplot2::ggplot(data.frame(x = 1:3, y = 3:1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  list(p1, p2) |>
    fr_figure() |>
    fr_titles("Figure 1") |>
    fr_render(tmp)

  content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  matches <- gregexpr("\\\\pngblip", content)[[1]]
  expect_equal(length(matches), 2L)
})

test_that("multi-page figure with multiple meta columns", {
  skip_if_not_installed("ggplot2")

  p1 <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p2 <- ggplot2::ggplot(data.frame(x = 1:3, y = 3:1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p3 <- ggplot2::ggplot(
    data.frame(x = 1:3, y = c(2, 2, 2)),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point()

  meta <- data.frame(
    subgroup = c("Adults", "Pediatrics", "Geriatrics"),
    n = c(80, 55, 30),
    stringsAsFactors = FALSE
  )

  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  list(p1, p2, p3) |>
    fr_figure(meta = meta) |>
    fr_titles("Figure 14.1.1", "Subgroup: {subgroup} (N={n})") |>
    fr_render(tmp)

  content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(content, "Subgroup: Adults (N=80)", fixed = TRUE)
  expect_match(content, "Subgroup: Pediatrics (N=55)", fixed = TRUE)
  expect_match(content, "Subgroup: Geriatrics (N=30)", fixed = TRUE)
})

test_that("single-plot figure still works after multi-page changes", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  p |>
    fr_figure() |>
    fr_titles("Single Figure") |>
    fr_render(tmp)

  content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  matches <- gregexpr("\\\\pngblip", content)[[1]]
  expect_equal(length(matches), 1L)
  expect_match(content, "Single Figure", fixed = TRUE)
})
