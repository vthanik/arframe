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

# ══════════════════════════════════════════════════════════════════════════════
# Footnote Placement Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("footnotes with placement='last' only appear on final section", {
  # With page_by, we get multiple sections.

  # "every" footnotes should appear in all sections.
  # "last" footnotes should appear only in the final section.
  spec <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(page_by = "grp") |>
    fr_footnotes(
      "Note: every page.",
      list("Source: last page only.", placement = "last")
    )

  txt <- render_html_str(spec)
  # Both should appear somewhere (final section has both)
  expect_true(grepl("Note: every page.", txt, fixed = TRUE))
  expect_true(grepl("Source: last page only.", txt, fixed = TRUE))

  # Count occurrences of "every page" — should be in 2 sections
  n_every <- length(gregexpr("Note: every page\\.", txt)[[1L]])
  expect_gte(n_every, 2L)
})

test_that("footnote separator renders <hr> when enabled", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_footnotes("A note.", .separator = TRUE)

  txt <- render_html_str(spec)
  expect_true(grepl("ar-fn-sep", txt, fixed = TRUE))
  expect_true(grepl("<hr", txt, fixed = TRUE))
})

test_that("footnote with custom font_size produces font-size CSS", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_footnotes(list("Tiny note.", font_size = 6))

  txt <- render_html_str(spec)
  expect_true(grepl("font-size:6pt", txt, fixed = TRUE))
})

test_that("footnote with center alignment produces text-align:center", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_footnotes(list("Center note.", align = "center"))

  txt <- render_html_str(spec)
  expect_true(grepl("text-align:center", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Title Continuation / Style Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("title with bold produces font-weight:bold", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_titles(list("Bold Title", bold = TRUE))

  txt <- render_html_str(spec)
  expect_true(grepl("font-weight:bold", txt, fixed = TRUE))
  expect_true(grepl("Bold Title", txt, fixed = TRUE))
})

test_that("title with custom font_size produces font-size CSS", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_titles(list("Big Title", font_size = 14))

  txt <- render_html_str(spec)
  expect_true(grepl("font-size:14pt", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Sentinel Markup Tests (bold, italic, underline, newline)
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_bold produces <b> in HTML", {
  spec <- data.frame(x = "{fr_bold('important')}") |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("<b>important</b>", txt, fixed = TRUE))
})

test_that("fr_italic produces <em> in HTML", {
  spec <- data.frame(x = "{fr_italic('note')}") |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("<em>note</em>", txt, fixed = TRUE))
})

test_that("fr_underline produces <u> in HTML", {
  spec <- data.frame(x = "{fr_underline('key')}") |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("<u>key</u>", txt, fixed = TRUE))
})

test_that("fr_newline produces <br> in HTML", {
  spec <- data.frame(x = "line1{fr_newline()}line2") |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("line1<br", txt, fixed = TRUE))
  expect_true(grepl("line2", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Spanner Header with hline
# ══════════════════════════════════════════════════════════════════════════════

test_that("spanner with hline produces border-bottom on span cell", {
  spec <- data.frame(a = 1, b = 2, c = 3) |>
    fr_table() |>
    fr_spans("Span AB" = c("a", "b"), .hline = TRUE)

  txt <- render_html_str(spec)
  expect_true(grepl("border-bottom", txt, fixed = TRUE))
  expect_true(grepl("Span AB", txt, fixed = TRUE))
})

test_that("spanner with empty gap cell renders <th></th>", {
  spec <- data.frame(a = 1, b = 2, c = 3) |>
    fr_table() |>
    fr_spans("Span AB" = c("a", "b"))

  txt <- render_html_str(spec)
  # Column c is not spanned, so it should get an empty <th></th>
  expect_true(grepl("<th></th>", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Chrome (pagehead/pagefoot) Style Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("pagehead with bold produces font-weight:bold", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_pagehead(left = "Study", bold = TRUE)

  txt <- render_html_str(spec)
  expect_true(grepl("font-weight:bold", txt, fixed = TRUE))
  expect_true(grepl("Study", txt, fixed = TRUE))
})

test_that("pagehead with custom font_size produces font-size CSS", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_pagehead(left = "Study", font_size = 7)

  txt <- render_html_str(spec)
  expect_true(grepl("font-size:7pt", txt, fixed = TRUE))
})

test_that("pagefoot wraps in ar-chrome-foot div", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_pagefoot(center = "Footer text")

  txt <- render_html_str(spec)
  expect_true(grepl("ar-chrome-foot", txt, fixed = TRUE))
  expect_true(grepl("Footer text", txt, fixed = TRUE))
})

test_that("pagefoot with left/center/right renders all three spans", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_pagefoot(left = "L", center = "C", right = "R")

  txt <- render_html_str(spec)
  expect_true(grepl("ar-chrome-left", txt, fixed = TRUE))
  expect_true(grepl("ar-chrome-center", txt, fixed = TRUE))
  expect_true(grepl("ar-chrome-right", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Viewer Mode CSS
# ══════════════════════════════════════════════════════════════════════════════

test_that("viewer mode produces clean CSS with padding and max-width", {
  spec <- data.frame(x = 1) |>
    fr_table()

  spec$.viewer <- TRUE
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  fr_render(spec, tmp)
  txt <- paste0(readLines(tmp, warn = FALSE), collapse = "\n")

  # Viewer mode body has padding
  expect_true(grepl("padding: 12px 16px", txt, fixed = TRUE))
  # Should have max-width on ar-table
  expect_true(grepl("max-width: 100%", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Body Cell Style Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("italic cell style produces font-style:italic", {
  spec <- data.frame(x = c("a", "b")) |>
    fr_table() |>
    fr_styles(fr_row_style(rows = 1, italic = TRUE))

  txt <- render_html_str(spec)
  expect_true(grepl("font-style:italic", txt, fixed = TRUE))
})

test_that("underline cell style produces text-decoration:underline", {
  spec <- data.frame(x = c("a", "b")) |>
    fr_table() |>
    fr_styles(fr_row_style(rows = 1, underline = TRUE))

  txt <- render_html_str(spec)
  expect_true(grepl("text-decoration:underline", txt, fixed = TRUE))
})

test_that("custom font color produces color CSS", {
  spec <- data.frame(x = c("a", "b")) |>
    fr_table() |>
    fr_styles(fr_row_style(rows = 1, color = "#0000ff"))

  txt <- render_html_str(spec)
  expect_true(grepl("color:#0000FF", txt, fixed = TRUE))
})

test_that("custom font_size on row style produces font-size CSS", {
  spec <- data.frame(x = c("a", "b")) |>
    fr_table() |>
    fr_styles(fr_row_style(rows = 1, font_size = 12))

  txt <- render_html_str(spec)
  expect_true(grepl("font-size:12pt", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Header Style Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("header italic via cell style produces font-style:italic in thead", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_styles(fr_style(region = "header", italic = TRUE))

  txt <- render_html_str(spec)
  expect_true(grepl("font-style:italic", txt, fixed = TRUE))
})

test_that("header underline via cell style produces text-decoration:underline in thead", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_styles(fr_style(region = "header", underline = TRUE))

  txt <- render_html_str(spec)
  expect_true(grepl("text-decoration:underline", txt, fixed = TRUE))
})

test_that("header background color produces background-color in thead", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_header(background = "#cccccc")

  txt <- render_html_str(spec)
  expect_true(grepl("background-color:#CCCCCC", txt, fixed = TRUE))
})

test_that("header custom font_size produces font-size in thead", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_header(font_size = 12)

  txt <- render_html_str(spec)
  expect_true(grepl("font-size:12pt", txt, fixed = TRUE))
})

test_that("header custom color produces color CSS in thead", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_header(color = "#ff0000")

  txt <- render_html_str(spec)
  expect_true(grepl("color:#FF0000", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Blank Row Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("blank rows render with ar-blank class", {
  spec <- data.frame(
    x = c("a", "", "b"),
    y = c("1", "", "2"),
    stringsAsFactors = FALSE
  ) |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("ar-blank", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Empty Body Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("empty data frame renders empty tbody", {
  spec <- data.frame(x = character(0), y = character(0)) |>
    fr_table()

  txt <- suppressWarnings(render_html_str(spec))
  expect_true(grepl("<tbody></tbody>", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Font Stack Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("monospace font produces monospace generic in CSS", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_page(font_family = "Courier New")

  txt <- render_html_str(spec)
  expect_true(grepl("monospace", txt, fixed = TRUE))
  expect_true(grepl("Courier New", txt, fixed = TRUE))
})

test_that("sans-serif font produces sans-serif generic in CSS", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_page(font_family = "Arial")

  txt <- render_html_str(spec)
  expect_true(grepl("sans-serif", txt, fixed = TRUE))
  expect_true(grepl("Arial", txt, fixed = TRUE))
})

test_that("serif font (default) produces serif generic in CSS", {
  spec <- data.frame(x = 1) |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("serif", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Border Edge Cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("vlines produce left/right border CSS", {
  spec <- data.frame(a = 1, b = 2) |>
    fr_table() |>
    fr_vlines("all")

  txt <- render_html_str(spec)
  # Vertical lines → border-left or border-right
  has_border <- grepl("border-left:", txt, fixed = TRUE) ||
    grepl("border-right:", txt, fixed = TRUE)
  expect_true(has_border)
})

test_that("grid preset produces both horizontal and vertical borders", {
  spec <- data.frame(a = 1, b = 2) |>
    fr_table() |>
    fr_grid()

  txt <- render_html_str(spec)
  expect_true(grepl("border-", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Page-by Visibility Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("page_by with visible=FALSE suppresses page-by label div", {
  spec <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(page_by = list(cols = "grp", visible = FALSE))

  txt <- render_html_str(spec)
  # The page-by div element should not be present (CSS class def is OK)
  expect_false(grepl("<div class=\"ar-page-by\">", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Multiline Content Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("newlines in title content produce <br> tags", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_titles("Line 1\nLine 2")

  txt <- render_html_str(spec)
  expect_true(grepl("Line 1<br", txt, fixed = TRUE))
  expect_true(grepl("Line 2", txt, fixed = TRUE))
})

test_that("newlines in body cell content produce <br> tags", {
  spec <- data.frame(x = "Row1\nRow2") |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("Row1<br", txt, fixed = TRUE))
  expect_true(grepl("Row2", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Spacing Configuration Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_spacing settings affect CSS margin/padding values", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_titles("Title") |>
    fr_footnotes("Note") |>
    fr_spacing(titles_after = 2L, footnotes_before = 3L)

  txt <- render_html_str(spec)
  # With 2 blank lines and ~1.4 line-height, margin-bottom should be ~2.8em
  expect_true(grepl("margin-bottom: 2.8em", txt, fixed = TRUE))
  # With 3 blank lines, margin-top should be ~4.2em
  expect_true(grepl("margin-top: 4.2em", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# HTML Escaping Edge Cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("double quotes are escaped in HTML body", {
  spec <- data.frame(x = 'He said "hello"') |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("&quot;", txt, fixed = TRUE))
})

test_that("html_escape handles empty character vector", {
  result <- html_escape(character(0))
  expect_identical(result, character(0))
})

# ══════════════════════════════════════════════════════════════════════════════
# Linestyle Mapping Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("html_linestyle maps all supported styles", {
  expect_equal(html_linestyle("solid"), "solid")
  expect_equal(html_linestyle("dashed"), "dashed")
  expect_equal(html_linestyle("dotted"), "dotted")
  expect_equal(html_linestyle("double"), "double")
  expect_equal(html_linestyle("dashdot"), "dashed")
  # Default fallback
  expect_equal(html_linestyle(NULL), "solid")
  expect_equal(html_linestyle("unknown"), "solid")
})

# ══════════════════════════════════════════════════════════════════════════════
# scope_css Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("scope_css scopes body selector to container ID", {
  css <- "body {\n  margin: 0;\n}\n"
  result <- scope_css(css, "arframe-123")
  expect_true(grepl("#arframe-123 \\{", result))
  expect_false(grepl("body", result, fixed = TRUE))
})

test_that("scope_css scopes * selector", {
  css <- "* {\n  box-sizing: border-box;\n}\n"
  result <- scope_css(css, "arframe-123")
  expect_true(grepl("#arframe-123 \\*", result))
})

test_that("scope_css scopes .ar- class selectors", {
  css <- ".ar-table {\n  border: none;\n}\n"
  result <- scope_css(css, "arframe-123")
  expect_true(grepl("#arframe-123 .ar-table", result, fixed = TRUE))
})

test_that("scope_css handles comma-separated selectors", {
  css <- "table, th, td {\n  border: none;\n}\n"
  result <- scope_css(css, "arframe-123")
  expect_true(grepl("#arframe-123 table", result, fixed = TRUE))
  expect_true(grepl("#arframe-123 th", result, fixed = TRUE))
  expect_true(grepl("#arframe-123 td", result, fixed = TRUE))
})

test_that("scope_css preserves @media blocks and scopes inner rules", {
  css <- "@media print {\n  .ar-page { margin: 0; }\n}\n"
  result <- scope_css(css, "arframe-123")
  expect_true(grepl("@media print", result, fixed = TRUE))
  expect_true(grepl("#arframe-123 .ar-page", result, fixed = TRUE))
})

test_that("scope_css scopes body inside @media block", {
  css <- "@media print {\n  body { background: white; }\n}\n"
  result <- scope_css(css, "arframe-123")
  expect_true(grepl("#arframe-123 \\{", result))
})

# ══════════════════════════════════════════════════════════════════════════════
# Column Header Label Fallback
# ══════════════════════════════════════════════════════════════════════════════

test_that("column without label uses column name in header", {
  spec <- data.frame(my_col = c(1, 2, 3)) |>
    fr_table()

  txt <- render_html_str(spec)
  expect_true(grepl("my_col", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Col Gap / Cell Padding Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("custom col_gap changes cell padding in CSS", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_page(col_gap = 8)

  txt <- render_html_str(spec)
  # col_gap=8, cell_pad_lr = 4pt
  expect_true(grepl("padding: 3px 4pt", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Valign in Body Rows
# ══════════════════════════════════════════════════════════════════════════════

test_that("valign='middle' on cell produces vertical-align:middle", {
  spec <- data.frame(x = c("a", "b")) |>
    fr_table() |>
    fr_styles(fr_row_style(rows = 1, valign = "middle"))

  txt <- render_html_str(spec)
  expect_true(grepl("vertical-align:middle", txt, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Header Valign Tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("header valign setting appears in thead", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_header(valign = "top")

  txt <- render_html_str(spec)
  expect_true(grepl("vertical-align:top", txt, fixed = TRUE))
})
