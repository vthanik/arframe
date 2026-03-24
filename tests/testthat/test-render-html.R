# ──────────────────────────────────────────────────────────────────────────────
# test-render-html.R — Tests for HTML rendering
# ──────────────────────────────────────────────────────────────────────────────

# Helper: render to temp HTML and return content as string
render_html_str <- function(spec) {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  fr_render(spec, tmp)
  paste0(readLines(tmp, warn = FALSE), collapse = "\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# Smoke Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_render creates a valid HTML file from minimal pipeline", {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "Sex"),
    value = c("65.2 (10.1)", "50 (55.6%)"),
    stringsAsFactors = FALSE
  )

  data |> fr_table() |> fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- paste0(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(startsWith(txt, "<!DOCTYPE html>"))
  expect_true(grepl("</html>", txt, fixed = TRUE))
})

test_that("fr_render detects .htm extension for HTML backend", {
  tmp <- tempfile(fileext = ".htm")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = "hello") |> fr_table() |> fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- paste0(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(startsWith(txt, "<!DOCTYPE html>"))
})

# ══════════════════════════════════════════════════════════════════════════════
# Content Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("HTML output contains title text", {
  spec <- data.frame(x = "hello") |>
    fr_table() |>
    fr_titles("Table 14.1.1 Demographics")

  txt <- render_html_str(spec)
  expect_true(grepl("Table 14.1.1 Demographics", txt, fixed = TRUE))
})

test_that("HTML output contains footnote text", {
  spec <- data.frame(x = "hello") |>
    fr_table() |>
    fr_footnotes("Source: ADSL")

  txt <- render_html_str(spec)
  expect_true(grepl("Source: ADSL", txt, fixed = TRUE))
})

test_that("HTML output contains header labels", {
  spec <- data.frame(age = 65, sex = "M") |>
    fr_table() |>
    fr_cols(
      age = fr_col("Age (years)"),
      sex = fr_col("Sex")
    )

  txt <- render_html_str(spec)
  expect_true(grepl("Age (years)", txt, fixed = TRUE))
  expect_true(grepl("Sex", txt, fixed = TRUE))
})

test_that("HTML output contains body data", {
  spec <- data.frame(
    param = c("Weight", "Height"),
    val = c("72.5", "168.3"),
    stringsAsFactors = FALSE
  ) |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("Weight", txt, fixed = TRUE))
  expect_true(grepl("72.5", txt, fixed = TRUE))
  expect_true(grepl("168.3", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Structure Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("spanning headers produce colspan", {
  spec <- data.frame(a = 1, b = 2, c = 3) |>
    fr_table() |>
    fr_spans("AB Span" = c("a", "b"))

  txt <- render_html_str(spec)
  expect_true(grepl("colspan=\"2\"", txt, fixed = TRUE))
  expect_true(grepl("AB Span", txt, fixed = TRUE))
})

test_that("borders produce border- CSS", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_hlines("header")

  txt <- render_html_str(spec)
  expect_true(grepl("border-", txt, fixed = TRUE))
})

test_that("sections produced for page_by groups", {
  spec <- data.frame(
    grp = c("Male", "Male", "Female", "Female"),
    val = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(page_by = "grp")

  txt <- render_html_str(spec)
  # Should have multiple <section> elements
  n_sections <- length(gregexpr("<section class=\"ar-section\">", txt)[[1L]])
  expect_gte(n_sections, 2L)
  expect_true(grepl("Male", txt, fixed = TRUE))
  expect_true(grepl("Female", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Style Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("bold cell style produces font-weight:bold", {
  spec <- data.frame(x = c("a", "b")) |>
    fr_table() |>
    fr_styles(fr_row_style(rows = 1, bold = TRUE))

  txt <- render_html_str(spec)
  expect_true(grepl("font-weight:bold", txt, fixed = TRUE))
})

test_that("background color produces background-color CSS", {
  spec <- data.frame(x = c("a", "b")) |>
    fr_table() |>
    fr_styles(fr_row_style(rows = 1, background = "#ff0000"))

  txt <- render_html_str(spec)
  expect_true(grepl("background-color:#FF0000", txt, fixed = TRUE))
})

test_that("indent produces padding-left CSS", {
  spec <- data.frame(x = c("  indented", "normal")) |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("padding-left:", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Sentinel / Markup Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_super produces <sup> in HTML", {
  spec <- data.frame(x = "{fr_super(1)} test") |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("<sup>1</sup>", txt, fixed = TRUE))
})

test_that("fr_sub produces <sub> in HTML", {
  spec <- data.frame(x = "H{fr_sub(2)}O") |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("<sub>2</sub>", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Escaping Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("HTML special characters are escaped", {
  spec <- data.frame(x = "a < b & c > d") |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("&lt;", txt, fixed = TRUE))
  expect_true(grepl("&amp;", txt, fixed = TRUE))
  expect_true(grepl("&gt;", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# CSS / Design Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("HTML output includes premium CSS classes", {
  spec <- data.frame(x = 1) |> fr_table()
  txt <- render_html_str(spec)

  expect_true(grepl("ar-page", txt, fixed = TRUE))
  expect_true(grepl("ar-table", txt, fixed = TRUE))
  expect_true(grepl("ar-section", txt, fixed = TRUE))
  expect_true(grepl("table-layout: fixed", txt, fixed = TRUE))
  expect_true(grepl("border-collapse: collapse", txt, fixed = TRUE))
})

test_that("HTML output includes @media print rules", {
  spec <- data.frame(x = 1) |> fr_table()
  txt <- render_html_str(spec)

  expect_true(grepl("@media print", txt, fixed = TRUE))
  expect_true(grepl("page-break-before", txt, fixed = TRUE))
})

test_that("colgroup with fixed widths is present", {
  spec <- data.frame(a = 1, b = 2) |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = 2.0), b = fr_col("B", width = 3.0))

  txt <- render_html_str(spec)
  expect_true(grepl("<colgroup>", txt, fixed = TRUE))
  expect_true(grepl("<col style=\"width:", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Pagehead / Pagefoot Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("pagehead and pagefoot render in HTML", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_pagehead(left = "Study ABC", right = "Draft") |>
    fr_pagefoot(center = "Page {thepage} of {total_pages}")

  txt <- render_html_str(spec)
  expect_true(grepl("Study ABC", txt, fixed = TRUE))
  expect_true(grepl("Draft", txt, fixed = TRUE))
  expect_true(grepl("ar-chrome", txt, fixed = TRUE))
  # Token resolution
  expect_true(grepl("Page 1 of 1", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Decimal Alignment Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("decimal-aligned cells get white-space:pre", {
  spec <- data.frame(val = c("12.34", "1.5", "100.0")) |>
    fr_table() |>
    fr_cols(val = fr_col("Value", align = "decimal"))

  txt <- render_html_str(spec)
  expect_true(grepl("white-space:pre", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Integration: tbl_demog smoke test
# ══════════════════════════════════════════════════════════════════════════════

test_that("tbl_demog renders to HTML without error", {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  tbl_demog |>
    fr_table() |>
    fr_titles("Table 14.1.1", "Summary of Demographics") |>
    fr_footnotes("Source: ADSL") |>
    fr_hlines("header") |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- paste0(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("Table 14.1.1", txt, fixed = TRUE))
  expect_true(grepl("Summary of Demographics", txt, fixed = TRUE))
  expect_true(grepl("Source: ADSL", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Figure Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("figure renders to HTML with embedded PNG", {
  skip_if_not_installed("ggplot2")
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  p |>
    fr_figure() |>
    fr_titles("Figure 1", "Test Figure") |>
    fr_footnotes("Source: test data") |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- paste0(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("<!DOCTYPE html>", txt, fixed = TRUE))
  expect_true(grepl("data:image/png;base64,", txt, fixed = TRUE))
  expect_true(grepl("Figure 1", txt, fixed = TRUE))
  expect_true(grepl("Test Figure", txt, fixed = TRUE))
  expect_true(grepl("Source: test data", txt, fixed = TRUE))
})
