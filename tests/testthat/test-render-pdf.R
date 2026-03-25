# ──────────────────────────────────────────────────────────────────────────────
# test-render-pdf.R — Tests for render-pdf.R
# ──────────────────────────────────────────────────────────────────────────────

test_that("find_xelatex returns character or NULL", {
  result <- arframe:::find_xelatex()
  expect_true(is.null(result) || is.character(result))
})

test_that("find_xelatex returns a path when xelatex is on PATH", {
  skip_if(!nzchar(Sys.which("xelatex")), "XeLaTeX not available")
  result <- arframe:::find_xelatex()
  expect_type(result, "character")
  expect_true(nzchar(result))
})

test_that("render_pdf runs without error", {
  skip_if(!nzchar(Sys.which("xelatex")), "XeLaTeX not available")

  spec <- tbl_demog |> fr_table() |> fr_titles("Test Table")
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(fr_render(spec, tmp))
})

test_that("render_pdf with pagehead and pagefoot", {
  skip_if(!nzchar(Sys.which("xelatex")), "XeLaTeX not available")

  spec <- tbl_demog |>
    fr_table() |>
    fr_titles("PDF Test") |>
    fr_pagehead(left = "Study X", right = "{datetime}") |>
    fr_pagefoot(center = "Page {thepage} of {total_pages}")
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)
  fr_render(spec, tmp)
  expect_true(file.exists(tmp))
})

test_that("render_pdf cleans up temp files", {
  skip_if(!nzchar(Sys.which("xelatex")), "XeLaTeX not available")

  spec <- data.frame(a = 1:3) |> fr_table()
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)
  fr_render(spec, tmp)
  # .aux and .log should be cleaned up in tempdir
  base_name <- tools::file_path_sans_ext(basename(tmp))
  aux <- file.path(tempdir(), paste0(base_name, ".aux"))
  expect_false(file.exists(aux))
})

test_that("compile_xelatex_doc errors when XeLaTeX not found (mocked)", {
  local_mocked_bindings(find_xelatex = function() NULL, .package = "arframe")
  # Also mock tinytex as unavailable
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "tinytex") {
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  tmp_tex <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp_tex), add = TRUE)
  writeLines(
    "\\documentclass{article}\\begin{document}hi\\end{document}",
    tmp_tex
  )
  expect_error(
    arframe:::compile_xelatex_doc(tmp_tex),
    "XeLaTeX not found"
  )
})


# ── fr_latex_deps() ─────────────────────────────────────────────────────────

test_that("fr_latex_deps returns character vector containing tabularray", {
  deps <- fr_latex_deps()
  expect_type(deps, "character")
  expect_true(length(deps) > 0L)
  expect_true("tabularray" %in% deps)
  expect_true("fontspec" %in% deps)
  expect_true("booktabs" %in% deps)
})


# ── fr_install_latex_deps() ─────────────────────────────────────────────────

test_that("fr_install_latex_deps skips tinytex when unavailable", {
  # When all packages are already installed, it should succeed
  # regardless of tinytex availability
  expect_no_error(fr_install_latex_deps())
})


# ── report_latex_failure() ──────────────────────────────────────────────────

test_that("report_latex_failure detects missing packages from log", {
  log_file <- tempfile(fileext = ".log")
  on.exit(unlink(log_file), add = TRUE)
  writeLines(
    c(
      "This is XeTeX, Version 3.14",
      "! LaTeX Error: File `tabularray.sty' not found.",
      "Type X to quit or <RETURN> to proceed."
    ),
    log_file
  )

  expect_error(
    arframe:::report_latex_failure(log_file, "test.tex"),
    "tabularray"
  )
})

test_that("report_latex_failure shows log tail for non-package errors", {
  log_file <- tempfile(fileext = ".log")
  on.exit(unlink(log_file), add = TRUE)
  writeLines(
    c(
      "This is XeTeX, Version 3.14",
      "! Undefined control sequence.",
      "l.42 \\badcommand"
    ),
    log_file
  )

  expect_error(
    arframe:::report_latex_failure(log_file, "test.tex"),
    "compilation failed"
  )
})

test_that("report_latex_failure handles missing log file", {
  expect_error(
    arframe:::report_latex_failure("/nonexistent/file.log", "test.tex"),
    "No log file found"
  )
})


# ── OSFONTDIR management in compile_xelatex_doc ──────────────────────────

test_that("compile_xelatex_doc sets OSFONTDIR when ARFRAME_FONT_DIR is set", {
  tmp <- tempfile("fontdir")
  dir.create(tmp)
  file.create(file.path(tmp, "test.ttf"))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  withr::local_envvar(ARFRAME_FONT_DIR = tmp, OSFONTDIR = NA)

  # Mock compilation to capture OSFONTDIR during execution
  captured_osfontdir <- NULL
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "tinytex") {
        captured_osfontdir <<- Sys.getenv("OSFONTDIR", unset = NA)
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  local_mocked_bindings(find_xelatex = function() NULL, .package = "arframe")

  tmp_tex <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp_tex), add = TRUE)
  writeLines(
    "\\documentclass{article}\\begin{document}hi\\end{document}",
    tmp_tex
  )
  try(arframe:::compile_xelatex_doc(tmp_tex), silent = TRUE)

  # OSFONTDIR should have been set during execution
  expect_true(!is.na(captured_osfontdir))
  expect_true(grepl(normalizePath(tmp), captured_osfontdir, fixed = TRUE))

  # OSFONTDIR should be restored (unset) after execution
  expect_true(is.na(Sys.getenv("OSFONTDIR", unset = NA)))
})

test_that("compile_xelatex_doc appends to existing OSFONTDIR", {
  tmp <- tempfile("fontdir")
  dir.create(tmp)
  file.create(file.path(tmp, "test.ttf"))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  withr::local_envvar(ARFRAME_FONT_DIR = tmp, OSFONTDIR = "/existing/path")

  captured_osfontdir <- NULL
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "tinytex") {
        captured_osfontdir <<- Sys.getenv("OSFONTDIR", unset = NA)
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  local_mocked_bindings(find_xelatex = function() NULL, .package = "arframe")

  tmp_tex <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp_tex), add = TRUE)
  writeLines(
    "\\documentclass{article}\\begin{document}hi\\end{document}",
    tmp_tex
  )
  try(arframe:::compile_xelatex_doc(tmp_tex), silent = TRUE)

  # Should contain both the font dir and existing path
  expect_true(grepl(normalizePath(tmp), captured_osfontdir, fixed = TRUE))
  expect_true(grepl("/existing/path", captured_osfontdir, fixed = TRUE))

  # OSFONTDIR should be restored to original value
  expect_equal(Sys.getenv("OSFONTDIR"), "/existing/path")
})
