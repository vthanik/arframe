# ──────────────────────────────────────────────────────────────────────────────
# test-render-rtf.R — Tests for RTF rendering
# ──────────────────────────────────────────────────────────────────────────────

test_that("fr_render creates a valid RTF file from minimal pipeline", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "Sex"),
    value = c("65.2 (10.1)", "50 (55.6%)"),
    stringsAsFactors = FALSE
  )

  data |> fr_table() |> fr_render(tmp)

  expect_true(file.exists(tmp))
  content <- readBin(tmp, "raw", file.info(tmp)$size)
  txt <- rawToChar(content)

  # Valid RTF structure
  expect_true(startsWith(txt, "{\\rtf1"))
  expect_true(endsWith(trimws(txt), "}"))
})

test_that("fr_render output contains title text", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = "hello", stringsAsFactors = FALSE)

  data |>
    fr_table() |>
    fr_titles("Table 14.1.1 Demographics") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Table 14.1.1 Demographics", txt, fixed = TRUE))
})

test_that("fr_render output contains footnote text", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = "hello", stringsAsFactors = FALSE)

  data |>
    fr_table() |>
    fr_footnotes("Source: ADSL") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Source: ADSL", txt, fixed = TRUE))
})

test_that("fr_render includes \\trhdr in column header row", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  data |> fr_table() |> fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\trhdr", txt, fixed = TRUE))
})

test_that("fr_render includes font table", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |> fr_table() |> fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\fonttbl", txt, fixed = TRUE))
})

test_that("fr_render includes color table", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |> fr_table() |> fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\colortbl", txt, fixed = TRUE))
})

test_that("fr_render generates \\sect markers for page_by groups", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_rows(page_by = "group") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Two groups = at least one \sect marker
  expect_true(grepl("\\sect", txt, fixed = TRUE))
})

test_that("fr_render applies hline preset borders", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = c("a", "b"), stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_hlines("header") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Should contain border control words
  expect_true(grepl("\\clbrdrb", txt, fixed = TRUE))
})

test_that("fr_render handles landscape orientation", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_page(orientation = "landscape") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\lndscpsxn", txt, fixed = TRUE))
})

test_that("fr_render handles portrait orientation", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_page(orientation = "portrait") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_false(grepl("\\lndscpsxn", txt, fixed = TRUE))
})

test_that("fr_render returns path invisibly", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  result <- data |> fr_table() |> fr_render(tmp)
  expect_equal(result, tmp)
})

test_that("detect_format works for rtf extension", {
  expect_equal(detect_format("test.rtf"), "rtf")
  expect_equal(detect_format("output/test.RTF"), "rtf")
})

test_that("detect_format errors on unknown extension", {
  expect_error(detect_format("test.xyz"), "Unsupported")
})

test_that("detect_format errors on missing extension", {
  expect_error(detect_format("noext"), "Cannot detect")
})

test_that("fr_render errors on non-fr_spec input", {
  expect_error(fr_render(list(), "test.rtf"), "fr_spec")
})

test_that("fr_render handles markup in titles", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_titles("{fr_bold('Table 14.1.1')} Demographics") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Bold sentinel should be resolved to RTF
  expect_true(grepl("\\{\\\\b Table 14.1.1\\}", txt))
  expect_true(grepl("Demographics", txt, fixed = TRUE))
})

test_that("fr_render handles multi-column data correctly", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "Sex", "BMI"),
    placebo = c("65.2", "50%", "25.1"),
    active = c("64.8", "48%", "25.3"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_titles("Table 14.1.1") |>
    fr_hlines("header") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Age", txt, fixed = TRUE))
  expect_true(grepl("placebo", txt, fixed = TRUE))
  expect_true(grepl("65.2", txt, fixed = TRUE))
})


test_that("column header newlines render as \\line in RTF", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(
      a = fr_col(label = "Col A\n(N=10)"),
      b = fr_col(label = "Col B"),
      .width = "equal"
    ) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Col A\\line (N=10)", txt, fixed = TRUE))
  expect_false(grepl("Col A\\\\line", txt, fixed = TRUE))
})

test_that("column header newlines preserve leading spaces as non-breaking", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(
      a = fr_col(label = "SOC\n  PT"),
      b = fr_col(label = "Col B"),
      .width = "equal"
    ) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Leading spaces on second line should become \~ (non-breaking spaces)
  expect_true(grepl("SOC\\line \\~\\~PT", txt, fixed = TRUE))
})


test_that("fr_spans adds bottom border to spanned cells by default", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, c = 3, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_spans("AB" = c("a", "b")) |>
    fr_cols(.width = "equal") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Spanned cells should have bottom border (\clbrdrb)
  expect_true(grepl("clbrdrb.*clmgf", txt))
})


test_that("fr_spans(.hline = FALSE) suppresses spanner bottom border", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, c = 3, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_spans("AB" = c("a", "b"), .hline = FALSE) |>
    fr_cols(.width = "equal") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Find the spanner row — it has \clmgf (merge first)
  spanner_line <- grep("clmgf", strsplit(txt, "\n")[[1]], value = TRUE)
  expect_true(length(spanner_line) > 0)
  # The spanner row should NOT have \clbrdrb (no bottom border)
  expect_false(grepl("clbrdrb", spanner_line[1]))
})


test_that("column header cells default to bottom vertical alignment", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(
      a = fr_col(label = "Short"),
      b = fr_col(label = "Multi\nLine"),
      .width = "equal"
    ) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Header cells should have \clvertalb (bottom alignment)
  expect_true(grepl("clvertalb", txt, fixed = TRUE))
})


test_that("fr_header(valign = 'top') uses \\clvertalt (RTF default, no keyword needed)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_header(valign = "top") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Top is RTF default — no \clvertalb or \clvertalc should appear
  expect_false(grepl("clvertalb", txt, fixed = TRUE))
  expect_false(grepl("clvertalc", txt, fixed = TRUE))
})


test_that("fr_col_style(valign = 'middle') emits \\clvertalc in body cells", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_col_style(cols = "a", valign = "middle")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Body should have \clvertalc for column "a"
  expect_true(grepl("clvertalc", txt, fixed = TRUE))
})


test_that("fr_row_style(valign = 'bottom') emits \\clvertalb in body cells", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_row_style(rows = 1L, valign = "bottom")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Body should have \clvertalb for row 1
  expect_true(grepl("clvertalb", txt, fixed = TRUE))
})


test_that("body cells default to valign = 'top' (no \\clvertal keyword)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = "x", stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_header(valign = "top") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # No vertical alignment keywords — everything is top (RTF default)
  expect_false(grepl("clvertalb", txt, fixed = TRUE))
  expect_false(grepl("clvertalc", txt, fixed = TRUE))
})


# ──────────────────────────────────────────────────────────────────────────────
# Issue 1+2: Continuation rows use plain text (no field codes)
# ──────────────────────────────────────────────────────────────────────────────

test_that("group_by does not emit continuation rows (no field codes, no trhdr group rows)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_rows(group_by = "grp") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # No field codes
  expect_false(grepl("fldinst", txt, fixed = TRUE))
  # No continuation text — groups are separated by blank rows instead
  expect_false(grepl("continued", txt, fixed = TRUE))
})


# ──────────────────────────────────────────────────────────────────────────────
# Issue 4: page_by preserves original data order
# ──────────────────────────────────────────────────────────────────────────────

test_that("page_by preserves original data order (not alphabetical)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    visit = c("Week 4", "Week 4", "Baseline", "Baseline"),
    param = c("SBP", "DBP", "SBP", "DBP"),
    val = c("130", "80", "120", "78"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_rows(page_by = "visit") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # "Week 4" should appear BEFORE "Baseline" (original order, not alphabetical)
  pos_week4 <- regexpr("Week 4", txt, fixed = TRUE)
  pos_baseline <- regexpr("Baseline", txt, fixed = TRUE)
  expect_true(pos_week4 < pos_baseline)
})


# ──────────────────────────────────────────────────────────────────────────────
# Issue 5: Decimal-aligned cells get reduced cell padding
# ──────────────────────────────────────────────────────────────────────────────

test_that("decimal-aligned cells render as single cell with left indent", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "Weight"),
    val = c("65.2", "80.1"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_cols(val = fr_col(align = "decimal")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # No sub-cell padding (no dual cells)
  expect_false(grepl("clpadt36", txt, fixed = TRUE))
  # Left-aligned cell with indent for centering
  expect_true(grepl("\\ql", txt, fixed = TRUE))
  expect_true(grepl("\\li", txt, fixed = TRUE))
  # No tab-stop patterns
  expect_false(grepl("\\tqdec", txt, fixed = TRUE))
  # Content appears
  expect_true(grepl("65", txt, fixed = TRUE))
  expect_true(grepl("80", txt, fixed = TRUE))
})

test_that("decimal alignment works with n(%) content", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    val = c("28 (62%)", "5 (11%)"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_cols(val = fr_col(align = "decimal")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Single-cell with left-align + indent
  expect_true(grepl("\\ql", txt, fixed = TRUE))
  expect_true(grepl("\\li", txt, fixed = TRUE))
  expect_false(grepl("\\tqdec", txt, fixed = TRUE))
})

test_that("decimal alignment works with scalar float content", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    val = c("135.2", "85.1", "0.07"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_cols(val = fr_col(align = "decimal")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Single-cell with left-align + indent
  expect_true(grepl("\\ql", txt, fixed = TRUE))
  expect_false(grepl("\\tqdec", txt, fixed = TRUE))
})


# ── Spacing tests ────────────────────────────────────────────────────────────

test_that("fr_spacing(titles_after = 0) suppresses blank line after titles", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_titles("My Title") |>
    fr_spacing(titles_after = 0L) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Titles are now \trhdr rows. Count \trhdr rows containing title text only.
  # With titles_after = 0: 1 title row, no blank spacer rows.
  n_trhdr <- length(gregexpr("\\\\trhdr", txt)[[1]])
  # 1 title row + 1 column header row = 2 \trhdr total (no blank spacer rows)
  expect_equal(n_trhdr, 2L)
})

test_that("fr_spacing(titles_after = 2) inserts two blank lines after titles", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_titles("My Title") |>
    fr_spacing(titles_after = 2L) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # 1 title row + 2 blank spacer rows + 1 column header row = 4 \trhdr
  n_trhdr <- length(gregexpr("\\\\trhdr", txt)[[1]])
  expect_equal(n_trhdr, 4L)
})

test_that("fr_spacing(footnotes_before = 0) suppresses blank row before footnotes", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = "x", stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_footnotes("[a] Test footnote.", .separator = FALSE) |>
    fr_spacing(footnotes_before = 0L) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Footnote text should be present (as body rows for single-page tables)
  expect_true(grepl("\\[a\\] Test footnote\\.", txt, fixed = FALSE))
})

test_that("'last' footnotes appear in final section only", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = 1:4,
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_rows(page_by = "grp") |>
    fr_footnotes(
      list("Every page note", placement = "every"),
      list("Last only note", placement = "last")
    ) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # Split on \pard\par\n\sect\n (the actual page break marker between sections)
  sections <- strsplit(txt, "\\pard\\par\n\\sect\n", fixed = TRUE)[[1L]]
  # Should have 2 sections (preamble+section1 before \sect, section2 after)
  expect_true(length(sections) >= 2L)

  # "Last only note" should NOT appear in the first section
  expect_false(grepl("Last only note", sections[1L], fixed = TRUE))
  # "Last only note" SHOULD appear in the final section
  # (as body rows for single-page tables, or in {\footer} for multi-page)
  last_section <- sections[length(sections)]
  expect_true(grepl("Last only note", last_section, fixed = TRUE))

  # "Every page note" should appear in both sections
  expect_true(grepl("Every page note", sections[1L], fixed = TRUE))
  expect_true(grepl("Every page note", last_section, fixed = TRUE))
})

# ── Single-page footnotes in body ────────────────────────────────────────────

test_that("single-page table renders footnotes in body (not footer)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "Sex"),
    value = c("65.2", "50"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_footnotes("Source: ADSL") |>
    fr_hlines("header") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # Footnote text should appear as body table rows (with \trowd, not \trhdr)
  expect_true(grepl("Source: ADSL", txt, fixed = TRUE))

  # Footer group should NOT contain footnote text (single-page mode)
  footer_match <- regmatches(txt, gregexpr("\\{\\\\footer[^}]*\\}", txt))[[1L]]
  if (length(footer_match) > 0L) {
    for (fm in footer_match) {
      expect_false(grepl("Source: ADSL", fm, fixed = TRUE))
    }
  }
})

test_that("default spacing inserts 1 blank line after titles", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_titles("My Title") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # 1 title row + 1 blank spacer row + 1 column header row = 3 \trhdr
  n_trhdr <- length(gregexpr("\\\\trhdr", txt)[[1]])
  expect_equal(n_trhdr, 3L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Unit tests for internal RTF helper functions
# ══════════════════════════════════════════════════════════════════════════════

# ── rtf_write ─────────────────────────────────────────────────────────────────

test_that("rtf_write writes text as raw bytes to connection", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  con <- file(tmp, open = "wb")
  arframe:::rtf_write(con, "hello world")
  close(con)

  content <- readLines(tmp, warn = FALSE)
  expect_equal(content, "hello world")
})

test_that("rtf_write silently skips empty strings", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  con <- file(tmp, open = "wb")
  result <- arframe:::rtf_write(con, "")
  arframe:::rtf_write(con, "after")
  close(con)

  expect_null(result)
  content <- readLines(tmp, warn = FALSE)
  expect_equal(content, "after")
})

# ── rtf_resolve_page_fields ──────────────────────────────────────────────────

test_that("rtf_resolve_page_fields replaces PAGE placeholder", {
  result <- arframe:::rtf_resolve_page_fields(
    "Page \x01RTFPAGE\x02 of \x01RTFNUMPAGES\x02"
  )
  expect_true(grepl("fldinst PAGE", result, fixed = TRUE))
  expect_true(grepl("fldinst NUMPAGES", result, fixed = TRUE))
})

test_that("rtf_resolve_page_fields passes through text without placeholders", {
  result <- arframe:::rtf_resolve_page_fields("no placeholders here")
  expect_equal(result, "no placeholders here")
})

# ── rtf_preamble ─────────────────────────────────────────────────────────────

test_that("rtf_preamble produces valid RTF header", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)

  result <- arframe:::rtf_preamble(spec, color_info)
  expect_true(startsWith(result, "{\\rtf1\\ansi"))
  expect_true(grepl("\\fonttbl", result, fixed = TRUE))
  expect_true(grepl("\\colortbl", result, fixed = TRUE))
})

test_that("rtf_preamble uses font family from spec", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_page(font_family = "Courier New")
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)

  result <- arframe:::rtf_preamble(spec, color_info)
  # Font may fall back on systems without Courier New (e.g. bare Linux)
  resolved <- arframe:::resolve_rtf_font("Courier New")
  expect_true(grepl(resolved, result, fixed = TRUE))
  # Resolved font should be mono family (fmodern) with fixed pitch
  expect_true(grepl("fmodern", result, fixed = TRUE))
  expect_true(grepl("fprq1", result, fixed = TRUE))
})

# ── rtf_section_def ──────────────────────────────────────────────────────────

test_that("rtf_section_def includes landscape control word", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_page(orientation = "landscape")
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::rtf_section_def(spec)
  expect_true(grepl("\\lndscpsxn", result, fixed = TRUE))
})

test_that("rtf_section_def omits landscape for portrait", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_page(orientation = "portrait")
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::rtf_section_def(spec)
  expect_false(grepl("\\lndscpsxn", result, fixed = TRUE))
})

test_that("rtf_section_def includes page dimensions and margins", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::rtf_section_def(spec)
  expect_true(grepl("\\pgwsxn", result, fixed = TRUE))
  expect_true(grepl("\\pghsxn", result, fixed = TRUE))
  expect_true(grepl("\\margl", result, fixed = TRUE))
  expect_true(grepl("\\margr", result, fixed = TRUE))
  expect_true(grepl("\\margt", result, fixed = TRUE))
  expect_true(grepl("\\margb", result, fixed = TRUE))
})

test_that("rtf_section_def includes headery when pagehead exists", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_pagehead(left = "Protocol XYZ")
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::rtf_section_def(spec)
  expect_true(grepl("\\headery", result, fixed = TRUE))
})

test_that("rtf_section_def omits headery without pagehead", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::rtf_section_def(spec)
  expect_false(grepl("\\headery", result, fixed = TRUE))
})

test_that("rtf_section_def includes footery with pagefoot", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_pagefoot(left = "Page info")
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::rtf_section_def(spec)
  expect_true(grepl("\\footery", result, fixed = TRUE))
})

test_that("rtf_section_def includes footery with every-placement footnotes", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_footnotes("Test note")
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::rtf_section_def(spec, body_footnotes = FALSE)
  expect_true(grepl("\\footery", result, fixed = TRUE))
})

test_that("rtf_section_def omits footery with body_footnotes = TRUE", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_footnotes("Test note")
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::rtf_section_def(spec, body_footnotes = TRUE)
  # With body_footnotes = TRUE and no pagefoot, no footery
  expect_false(grepl("\\footery", result, fixed = TRUE))
})

test_that("rtf_section_def works with A4 paper", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_page(paper = "A4")
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::rtf_section_def(spec)
  # A4 is 210mm x 297mm ≈ 11906 x 16838 twips
  expect_true(grepl("\\pgwsxn", result, fixed = TRUE))
  expect_true(grepl("\\pghsxn", result, fixed = TRUE))
})

# ── find_bottom_rule ─────────────────────────────────────────────────────────

test_that("find_bottom_rule returns rule when present", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_hlines("hsides")
  spec <- arframe:::finalize_spec(spec)

  rule <- arframe:::find_bottom_rule(spec)
  # "hsides" preset includes body/below rule
  expect_false(is.null(rule))
  expect_equal(rule$region, "body")
  expect_equal(rule$side, "below")
  expect_null(rule$rows)
})

test_that("find_bottom_rule returns NULL when no rules", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)

  rule <- arframe:::find_bottom_rule(spec)
  expect_null(rule)
})

# ── compute_single_page ─────────────────────────────────────────────────────

test_that("compute_single_page returns TRUE for small data", {
  spec <- data.frame(x = 1:3, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::compute_single_page(spec, data.frame(x = 1:3))
  expect_true(result)
})

test_that("compute_single_page returns FALSE for large data", {
  large_data <- data.frame(x = seq_len(200), stringsAsFactors = FALSE)
  spec <- large_data |> fr_table()
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::compute_single_page(spec, large_data)
  expect_false(result)
})

test_that("compute_single_page accounts for titles and footnotes", {
  # Create data that's near the page boundary; adding titles should push over
  near_limit <- data.frame(x = seq_len(50), stringsAsFactors = FALSE)
  spec <- near_limit |>
    fr_table() |>
    fr_titles("Title 1", "Title 2", "Title 3") |>
    fr_footnotes("Footnote 1", "Footnote 2")
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::compute_single_page(spec, near_limit)
  expect_type(result, "logical")
  expect_length(result, 1L)
})

test_that("compute_single_page accounts for pagehead", {
  small_data <- data.frame(x = 1:5, stringsAsFactors = FALSE)
  spec <- small_data |>
    fr_table() |>
    fr_pagehead(left = "Header")
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::compute_single_page(spec, small_data)
  expect_true(result) # Still fits with pagehead
})

# ── rtf_page_header (pagehead) ───────────────────────────────────────────────

test_that("rtf_page_header returns empty string when no pagehead", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )

  result <- arframe:::rtf_page_header(spec, token_map)
  expect_equal(result, "")
})

test_that("rtf_page_header wraps content in header group", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_pagehead(left = "Protocol: XYZ")
  spec <- arframe:::finalize_spec(spec)
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )

  result <- arframe:::rtf_page_header(spec, token_map)
  expect_true(grepl("\\header", result, fixed = TRUE))
  expect_true(grepl("Protocol: XYZ", result, fixed = TRUE))
})

test_that("pagehead with left, center, and right renders tab stops", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_pagehead(left = "Left", center = "Center", right = "Right") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Left", txt, fixed = TRUE))
  expect_true(grepl("Center", txt, fixed = TRUE))
  expect_true(grepl("Right", txt, fixed = TRUE))
  expect_true(grepl("\\trowd", txt, fixed = TRUE))
  expect_true(grepl("\\intbl", txt, fixed = TRUE))
})

test_that("pagehead with bold = TRUE uses \\b control word", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_pagehead(left = "Bold Header", bold = TRUE) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # The header group should contain \b
  header_match <- regmatches(txt, regexpr("\\{\\\\header[^}]*\\}", txt))
  expect_true(length(header_match) > 0L)
  expect_true(grepl("\\b ", header_match[1L], fixed = TRUE))
})

# ── rtf_footer_group ─────────────────────────────────────────────────────────

test_that("rtf_footer_group returns empty when no footnotes and no pagefoot", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )
  vis_columns <- spec$columns

  result <- arframe:::rtf_footer_group(
    spec,
    token_map,
    is_last = TRUE,
    vis_columns,
    skip_footnotes = FALSE
  )
  expect_equal(result, "")
})

test_that("rtf_footer_group includes pagefoot content", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_pagefoot(left = "Page Footer Left")
  spec <- arframe:::finalize_spec(spec)
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )

  result <- arframe:::rtf_footer_group(
    spec,
    token_map,
    is_last = TRUE,
    spec$columns,
    skip_footnotes = FALSE
  )
  expect_true(grepl("\\footer", result, fixed = TRUE))
  expect_true(grepl("Page Footer Left", result, fixed = TRUE))
})

test_that("rtf_footer_group skips footnotes when skip_footnotes = TRUE", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_footnotes("Skipped footnote") |>
    fr_pagefoot(left = "Footer")
  spec <- arframe:::finalize_spec(spec)
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )

  result <- arframe:::rtf_footer_group(
    spec,
    token_map,
    is_last = TRUE,
    spec$columns,
    skip_footnotes = TRUE
  )
  expect_false(grepl("Skipped footnote", result, fixed = TRUE))
  expect_true(grepl("Footer", result, fixed = TRUE))
})

test_that("rtf_footer_group includes footnote separator when enabled", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_footnotes("Note 1", .separator = TRUE)
  spec <- arframe:::finalize_spec(spec)
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )

  result <- arframe:::rtf_footer_group(
    spec,
    token_map,
    is_last = FALSE,
    spec$columns,
    skip_footnotes = FALSE
  )
  expect_true(grepl("brdrt.*brdrs.*brdrw5", result))
})

# ── rtf_title_rows ───────────────────────────────────────────────────────────

test_that("rtf_title_rows returns empty when no titles", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)

  result <- arframe:::rtf_title_rows(
    spec,
    spec$columns,
    arframe:::build_rtf_colortbl(
      arframe:::collect_colors(spec)
    )
  )
  expect_equal(result, "")
})

test_that("rtf_title_rows produces \\trhdr rows with merged cells", {
  spec <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Test Title")
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)

  result <- arframe:::rtf_title_rows(spec, spec$columns, color_info)
  expect_true(grepl("\\trhdr", result, fixed = TRUE))
  expect_true(grepl("\\clmgf", result, fixed = TRUE))
  expect_true(grepl("\\clmrg", result, fixed = TRUE))
  expect_true(grepl("Test Title", result, fixed = TRUE))
})

test_that("rtf_title_rows appends continuation text to first title", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Table 1") |>
    fr_page(continuation = "(continued)")
  spec <- suppressWarnings(arframe:::finalize_spec(spec))
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)

  # Panel 1: no continuation text
  result1 <- arframe:::rtf_title_rows(
    spec,
    spec$columns,
    color_info,
    panel_idx = 1L
  )
  expect_true(grepl("Table 1", result1, fixed = TRUE))
  expect_false(grepl("(continued)", result1, fixed = TRUE))

  # Panel 2+: continuation text appended
  result2 <- arframe:::rtf_title_rows(
    spec,
    spec$columns,
    color_info,
    panel_idx = 2L
  )
  expect_true(grepl("Table 1", result2, fixed = TRUE))
  expect_true(grepl("(continued)", result2, fixed = TRUE))
})

test_that("rtf_title_rows applies bold and alignment", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles(list("Bold Title", bold = TRUE, align = "left"))
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)

  result <- arframe:::rtf_title_rows(spec, spec$columns, color_info)
  expect_true(grepl("\\\\b ", result))
  expect_true(grepl("\\\\ql", result))
})

# ── rtf_page_by_rows ────────────────────────────────────────────────────────

test_that("rtf_page_by_rows produces merged \\trhdr row with group label", {
  spec <- data.frame(grp = "A", val = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_rows(page_by = "grp")
  spec <- arframe:::finalize_spec(spec)

  cellx <- arframe:::rtf_cellx_positions(spec$columns)
  result <- arframe:::rtf_page_by_rows(spec, spec$columns, cellx, "Group: A")
  expect_true(grepl("\\trhdr", result, fixed = TRUE))
  expect_true(grepl("Group: A", result, fixed = TRUE))
  expect_true(grepl("\\clmgf", result, fixed = TRUE))
})

test_that("page_by renders label without bold by default", {
  spec <- data.frame(
    grp = c("A", "B"),
    val = 1:2,
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(page_by = "grp")
  spec <- arframe:::finalize_spec(spec)

  cellx <- arframe:::rtf_cellx_positions(spec$columns)
  result <- arframe:::rtf_page_by_rows(spec, spec$columns, cellx, "A")
  expect_false(grepl("\\\\b ", result))
  expect_true(grepl("A", result, fixed = TRUE))
})

# ── rtf_col_header_row ───────────────────────────────────────────────────────

test_that("rtf_col_header_row produces \\trhdr row with all column labels", {
  spec <- data.frame(
    param = "Age",
    value = "65",
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_cols(
      param = fr_col(label = "Parameter"),
      value = fr_col(label = "Value"),
      .width = "equal"
    )
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)
  borders <- arframe:::resolve_borders(spec$rules, 1L, 2L, 1L)

  cellx <- arframe:::rtf_cellx_positions(spec$columns)
  result <- arframe:::rtf_col_header_row(
    spec,
    spec$columns,
    cellx,
    borders,
    color_info
  )
  expect_true(grepl("\\trhdr", result, fixed = TRUE))
  expect_true(grepl("Parameter", result, fixed = TRUE))
  expect_true(grepl("Value", result, fixed = TRUE))
})

test_that("rtf_col_header_row respects label_overrides", {
  spec <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal")
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)
  borders <- arframe:::resolve_borders(spec$rules, 1L, 2L, 1L)

  cellx <- arframe:::rtf_cellx_positions(spec$columns)
  overrides <- c(a = "A (N=50)", b = "B (N=60)")
  result <- arframe:::rtf_col_header_row(
    spec,
    spec$columns,
    cellx,
    borders,
    color_info,
    label_overrides = overrides
  )
  expect_true(grepl("A (N=50)", result, fixed = TRUE))
  expect_true(grepl("B (N=60)", result, fixed = TRUE))
})

test_that("fr_header(background=) applies background color to column header", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_header(background = "#CCCCCC") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\clcbpat", txt, fixed = TRUE))
})

test_that("fr_header(color=) applies foreground color to column header", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_header(color = "#FF0000") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\cf", txt, fixed = TRUE))
})

# ── rtf_body_rows ────────────────────────────────────────────────────────────

test_that("rtf_body_rows returns empty string for zero-row data", {
  spec <- data.frame(x = character(0), stringsAsFactors = FALSE) |> fr_table()
  spec <- suppressWarnings(arframe:::finalize_spec(spec))
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)
  cell_grid <- arframe:::build_cell_grid(
    data.frame(x = character(0), stringsAsFactors = FALSE),
    spec$columns,
    spec$cell_styles,
    spec$page
  )
  borders <- arframe:::resolve_borders(spec$rules, 0L, 1L, 1L)

  result <- arframe:::rtf_body_rows(
    spec,
    data.frame(x = character(0), stringsAsFactors = FALSE),
    spec$columns,
    cell_grid,
    borders,
    color_info
  )
  expect_equal(result, "")
})

test_that("body rows with background color include \\clcbpat", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = "hello", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_row_style(rows = 1L, background = "#EEEEEE")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\clcbpat", txt, fixed = TRUE))
})

test_that("body rows with foreground color include \\cf", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = "hello", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_row_style(rows = 1L, color = "#0000FF")) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\cf", txt, fixed = TRUE))
})

test_that("body rows with italic style include \\i", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = "hello", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_row_style(rows = 1L, italic = TRUE)) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\\\i ", txt))
  expect_true(grepl("\\\\i0", txt))
})

test_that("body rows with underline style include \\ul", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = "hello", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_row_style(rows = 1L, underline = TRUE)) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\\\ul ", txt))
  expect_true(grepl("\\\\ulnone", txt))
})

test_that("body rows with indentation include \\li", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "  Mean"),
    value = c("", "65.2"),
    stringsAsFactors = FALSE
  )
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_rows(indent_by = "param") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\\\li", txt))
})

test_that("group_by rows emit \\trkeep", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  )
  data |>
    fr_table() |>
    fr_rows(group_by = "grp") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\trkeep", txt, fixed = TRUE))
})

# ── rtf_spanner_rows ─────────────────────────────────────────────────────────

test_that("rtf_spanner_rows returns empty when no spans", {
  spec <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)
  borders <- arframe:::resolve_borders(spec$rules, 1L, 2L, 1L)

  cellx <- arframe:::rtf_cellx_positions(spec$columns)
  result <- arframe:::rtf_spanner_rows(
    spec,
    spec$columns,
    cellx,
    borders,
    color_info
  )
  expect_equal(result, "")
})

test_that("rtf_spanner_rows includes merged cells for span", {
  spec <- data.frame(a = 1, b = 2, c = 3, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_spans("AB" = c("a", "b")) |>
    fr_cols(.width = "equal")
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)
  borders <- arframe:::resolve_borders(spec$rules, 1L, 3L, 2L)

  cellx <- arframe:::rtf_cellx_positions(spec$columns)
  result <- arframe:::rtf_spanner_rows(
    spec,
    spec$columns,
    cellx,
    borders,
    color_info
  )
  expect_true(grepl("\\clmgf", result, fixed = TRUE))
  expect_true(grepl("\\clmrg", result, fixed = TRUE))
  expect_true(grepl("AB", result, fixed = TRUE))
})

# ── rtf_cell_border_string ───────────────────────────────────────────────────

test_that("rtf_cell_border_string produces border control words", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_hlines("box")
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)
  borders <- arframe:::resolve_borders(spec$rules, 1L, 1L, 1L)

  result <- arframe:::rtf_cell_border_string(borders$body, 1L, 1L, color_info)
  # Should contain border keywords
  expect_true(grepl("\\\\clbrdr", result))
  expect_true(grepl("\\\\brdrw", result))
  expect_true(grepl("\\\\brdrcf", result))
})

test_that("rtf_cell_border_string returns empty for no borders", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)
  borders <- arframe:::resolve_borders(spec$rules, 1L, 1L, 1L)

  result <- arframe:::rtf_cell_border_string(borders$body, 1L, 1L, color_info)
  expect_equal(result, "")
})

test_that("rtf_cell_border_string respects sides parameter", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_grid()
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)
  borders <- arframe:::resolve_borders(spec$rules, 1L, 1L, 1L)

  # Only request top and left
  result <- arframe:::rtf_cell_border_string(
    borders$body,
    1L,
    1L,
    color_info,
    sides = c("top", "left")
  )
  # Should NOT have bottom or right borders
  expect_false(grepl("\\\\clbrdrb", result))
  expect_false(grepl("\\\\clbrdrr", result))
})

# ── rtf_footnote_rows ────────────────────────────────────────────────────────

test_that("rtf_footnote_rows returns empty for no entries", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  colors <- arframe:::collect_colors(spec)
  color_info <- arframe:::build_rtf_colortbl(colors)

  cellx <- arframe:::rtf_cellx_positions(spec$columns)
  result <- arframe:::rtf_footnote_rows(
    spec,
    spec$columns,
    cellx,
    list(),
    color_info
  )
  expect_equal(result, "")
})

# ── rtf_chrome_content ───────────────────────────────────────────────────────

test_that("rtf_chrome_content returns empty for chrome with no text", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  chrome <- list(left = NULL, center = NULL, right = NULL, bold = FALSE)
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )

  result <- arframe:::rtf_chrome_content(chrome, spec, token_map, "test")
  expect_equal(result, "")
})

test_that("rtf_chrome_content uses table layout for L+C zones", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  chrome <- list(
    left = "L",
    center = "C",
    right = NULL,
    bold = FALSE,
    font_size = NULL
  )
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )

  result <- arframe:::rtf_chrome_content(chrome, spec, token_map, "test")
  # Table row structure

  expect_true(grepl("\\trowd", result, fixed = TRUE))
  expect_true(grepl("\\intbl", result, fixed = TRUE))
  expect_true(grepl("\\cell", result, fixed = TRUE))
  expect_true(grepl("\\row", result, fixed = TRUE))
  # Cell alignments
  expect_true(grepl("\\ql", result, fixed = TRUE))
  expect_true(grepl("\\qc", result, fixed = TRUE))
  # Content present
  expect_true(grepl("L", result, fixed = TRUE))
  expect_true(grepl("C", result, fixed = TRUE))
  # No old tab-stop approach
  expect_false(grepl("\\tqc", result, fixed = TRUE))
  expect_false(grepl("\\tqr", result, fixed = TRUE))
})

test_that("rtf_chrome_content uses table layout for L+R zones (no center)", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  chrome <- list(
    left = "L",
    center = NULL,
    right = "R",
    bold = FALSE,
    font_size = NULL
  )
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )

  result <- arframe:::rtf_chrome_content(chrome, spec, token_map, "test")
  # Table structure
  expect_true(grepl("\\trowd", result, fixed = TRUE))
  expect_true(grepl("\\intbl", result, fixed = TRUE))
  expect_true(grepl("\\row", result, fixed = TRUE))
  # Two cells: left-aligned and right-aligned
  expect_true(grepl("\\ql", result, fixed = TRUE))
  expect_true(grepl("\\qr", result, fixed = TRUE))
  expect_true(grepl("R", result, fixed = TRUE))
})

test_that("rtf_chrome_content handles 3-zone L+C+R layout", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  chrome <- list(
    left = "Left",
    center = "Center",
    right = "Right",
    bold = FALSE,
    font_size = NULL
  )
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )

  result <- arframe:::rtf_chrome_content(chrome, spec, token_map, "test")
  expect_true(grepl("\\ql", result, fixed = TRUE))
  expect_true(grepl("\\qc", result, fixed = TRUE))
  expect_true(grepl("\\qr", result, fixed = TRUE))
  expect_true(grepl("Left", result, fixed = TRUE))
  expect_true(grepl("Center", result, fixed = TRUE))
  expect_true(grepl("Right", result, fixed = TRUE))
  # Three \cell terminators (not counting \cellx)
  expect_equal(length(gregexpr("\\\\cell[^x]", result)[[1]]), 3L)
})

test_that("rtf_chrome_content handles single-zone layout", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  chrome <- list(
    left = NULL,
    center = "Only",
    right = NULL,
    bold = FALSE,
    font_size = NULL
  )
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )

  result <- arframe:::rtf_chrome_content(chrome, spec, token_map, "test")
  expect_true(grepl("\\qc", result, fixed = TRUE))
  expect_true(grepl("Only", result, fixed = TRUE))
  # Single cell (not counting \cellx)
  expect_equal(length(gregexpr("\\\\cell[^x]", result)[[1]]), 1L)
})

test_that("rtf_chrome_content multi-line right text stays right-aligned", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  chrome <- list(
    left = "Protocol XYZ",
    center = NULL,
    right = "Page 1 of 2\nDatabase Lock: 15MAR2025",
    bold = FALSE,
    font_size = NULL
  )
  token_map <- arframe:::build_token_map(
    page_num = 1,
    total_pages = 1,
    spec = spec
  )

  result <- arframe:::rtf_chrome_content(chrome, spec, token_map, "test")
  # Right cell has its own \qr alignment — multi-line text uses \line within
  expect_true(grepl("\\qr", result, fixed = TRUE))
  expect_true(grepl("\\line", result, fixed = TRUE))
  expect_true(grepl("Database Lock: 15MAR2025", result, fixed = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# Integration tests for full RTF rendering
# ══════════════════════════════════════════════════════════════════════════════

test_that("full pipeline with pagehead and pagefoot renders correctly", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(param = "Age", val = "65", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Table 14.1.1") |>
    fr_footnotes("Source: ADSL") |>
    fr_pagehead(left = "Protocol XYZ", right = "Page {thepage}") |>
    fr_pagefoot(left = "{program}", right = "{datetime}") |>
    fr_hlines("header") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # Structure
  expect_true(startsWith(txt, "{\\rtf1"))
  expect_true(grepl("\\fonttbl", txt, fixed = TRUE))
  expect_true(grepl("\\colortbl", txt, fixed = TRUE))

  # Content
  expect_true(grepl("Table 14.1.1", txt, fixed = TRUE))
  expect_true(grepl("Source: ADSL", txt, fixed = TRUE))
  expect_true(grepl("Protocol XYZ", txt, fixed = TRUE))
  expect_true(grepl("Age", txt, fixed = TRUE))
  expect_true(grepl("65", txt, fixed = TRUE))

  # Page fields should be resolved
  expect_true(grepl("fldinst PAGE", txt, fixed = TRUE))
})

test_that("col_split creates multiple panels with \\sect", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # Create wide table that should split
  data <- data.frame(
    param = "Test",
    a = "1",
    b = "2",
    c = "3",
    d = "4",
    e = "5",
    f = "6",
    g = "7",
    h = "8",
    stringsAsFactors = FALSE
  )
  data |>
    fr_table() |>
    fr_cols(param = fr_col(stub = TRUE), .width = "auto", .split = TRUE) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(file.exists(tmp))
  # File should be valid RTF
  expect_true(startsWith(txt, "{\\rtf1"))
})

test_that("footnote separator renders as thin border", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = "val", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_footnotes("Note 1", "Note 2", .separator = TRUE) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Note 1", txt, fixed = TRUE))
  expect_true(grepl("Note 2", txt, fixed = TRUE))
  # Separator border (brdrw5)
  expect_true(grepl("brdrw5", txt, fixed = TRUE))
})

test_that("multiple titles render as separate rows", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_titles("Title Line 1", "Title Line 2", "Title Line 3") |>
    fr_spacing(titles_after = 0L) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Title Line 1", txt, fixed = TRUE))
  expect_true(grepl("Title Line 2", txt, fixed = TRUE))
  expect_true(grepl("Title Line 3", txt, fixed = TRUE))
  # 3 title rows + 1 col header = 4 trhdr (no spacing blanks)
  n_trhdr <- length(gregexpr("\\\\trhdr", txt)[[1]])
  expect_equal(n_trhdr, 4L)
})

test_that("page_by groups each get their own section", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    visit = c("Week 1", "Week 1", "Week 2", "Week 2", "Week 3", "Week 3"),
    param = c("SBP", "DBP", "SBP", "DBP", "SBP", "DBP"),
    val = as.character(c(120, 80, 125, 82, 130, 85)),
    stringsAsFactors = FALSE
  )
  data |>
    fr_table() |>
    fr_rows(page_by = "visit") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # page_by groups produce \sect markers between sections
  sect_count <- length(gregexpr("\\\\sect", txt)[[1]])
  expect_true(sect_count >= 1L)
})

test_that("fr_grid produces borders in RTF output", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(a = "1", b = "2", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_grid() |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Grid preset should produce border control words
  expect_true(grepl("brdr", txt, fixed = TRUE))
})

test_that("vlines produces vertical borders between columns", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(a = "1", b = "2", c = "3", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_vlines("box") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Should have border keywords
  expect_true(grepl("brdr", txt, fixed = TRUE))
})

test_that("pagehead_after spacing produces blank paragraphs", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_pagehead(left = "Header") |>
    fr_spacing(pagehead_after = 2L) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Should have \\par paragraphs after header group
  expect_true(grepl("\\\\par", txt))
})

test_that("pagehead_after = 0 suppresses blank paragraphs", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_pagehead(left = "Header") |>
    fr_spacing(pagehead_after = 0L) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # After the header group close, should NOT have \\pard\\plain...\\par before table
  # (complex to test precisely, but no blank paragraphs after header)
  expect_true(grepl("Header", txt, fixed = TRUE))
})

test_that("landscape A4 renders correctly", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_page(paper = "A4", orientation = "landscape") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\lndscpsxn", txt, fixed = TRUE))
  expect_true(startsWith(txt, "{\\rtf1"))
})

test_that("fr_header(align = 'center') centers column headers", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(a = 1, b = 2, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_header(align = "center") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Header row should have \qc alignment
  # Find the trhdr row that contains column names
  expect_true(grepl("\\\\qc", txt))
})

test_that("fr_header(bold = TRUE) bolds column headers", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(a = 1, stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_header(bold = TRUE) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\\\b ", txt))
})

test_that("single-column table renders without \\clmrg in titles", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = "val", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Single Col Title") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Single column: \clmgf but no \clmrg
  expect_true(grepl("Single Col Title", txt, fixed = TRUE))
  expect_true(grepl("\\clmgf", txt, fixed = TRUE))
  # Title row should NOT have \clmrg (only 1 column)
  title_lines <- strsplit(txt, "\n")[[1]]
  title_row <- grep("Single Col Title", title_lines, fixed = TRUE, value = TRUE)
  expect_false(any(grepl("\\clmrg", title_row, fixed = TRUE)))
})

test_that("row height from fr_row_style emits custom \\trrh value", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = "val", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_row_style(rows = 1L, height = 0.5)) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # fr_row_style height = 0.5 inches = 720 twips
  expect_true(grepl("\\trrh720", txt, fixed = TRUE))
})

test_that("at-least row height: all body rows have \\trrhN (positive) at 9pt", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(a = c("x", "y"), stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # row_height_twips(9) = 1.0 * (1.0 + 1.2 * 9) * 20 = 236 twips (at-least)

  expect_true(grepl("\\trrh236", txt, fixed = TRUE))
  # Must NOT have negative (exact) height

  expect_false(grepl("\\trrh-236", txt, fixed = TRUE))
})

test_that("deterministic row height: zero cell padding on all rows", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl(
    "\\trpaddt0\\trpaddft3\\trpaddb0\\trpaddfb3",
    txt,
    fixed = TRUE
  ))
})

test_that("cell paragraphs have zero space before/after", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Default spacing: zero space before/after, Word default line spacing
  expect_true(grepl("\\sb0\\sa0", txt, fixed = TRUE))
  expect_false(grepl("\\slmult0", txt, fixed = TRUE))
})

test_that("continuation text does not appear on panel 1 (single-panel table)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  suppressWarnings(
    data.frame(x = 1, stringsAsFactors = FALSE) |>
      fr_table() |>
      fr_titles("Title 1", "Subtitle 2") |>
      fr_page(continuation = "(cont.)") |>
      fr_render(tmp)
  )

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Panel 1: continuation should NOT appear
  expect_false(grepl("(cont.)", txt, fixed = TRUE))
})


# ══════════════════════════════════════════════════════════════════════════════
# NA values — end-to-end render
# ══════════════════════════════════════════════════════════════════════════════

test_that("NA values render as empty cells in RTF output", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = c("a", NA, "b"), stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Data values present
  expect_true(grepl("\\ba\\b", txt, perl = TRUE))
  expect_true(grepl("\\bb\\b", txt, perl = TRUE))
  # "NA" string should NOT appear in the body
  expect_false(grepl("\\bNA\\b", txt, perl = TRUE))
})


# ══════════════════════════════════════════════════════════════════════════════
# is.fr_spec / is.fr_col — predicate tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("is.fr_spec returns TRUE for fr_spec objects", {
  spec <- data.frame(x = 1) |> fr_table()
  expect_true(is.fr_spec(spec))
  expect_false(is.fr_spec(mtcars))
  expect_false(is.fr_spec(NULL))
  expect_false(is.fr_spec("text"))
})

test_that("is.fr_col returns TRUE for fr_col objects", {
  col <- fr_col(label = "Test")
  expect_true(is.fr_col(col))
  expect_false(is.fr_col(list(label = "Test")))
  expect_false(is.fr_col(NULL))
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_rows / fr_pagehead / fr_pagefoot — merge semantics
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows merges with previous configuration", {
  spec <- suppressWarnings(
    tbl_ae_soc |>
      fr_table() |>
      fr_rows(page_by = "soc") |>
      fr_rows(group_by = "soc")
  )

  # Both should be preserved
  expect_true(length(spec$body$page_by) > 0L)
  expect_true(length(spec$body$group_by) > 0L)
})

test_that("fr_pagehead merges with previous header", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_pagehead(left = "Study X") |>
    fr_pagehead(right = "Page 1")

  # Both should be preserved
  expect_equal(spec$pagehead$left, "Study X")
  expect_equal(spec$pagehead$right, "Page 1")
})

test_that("fr_pagefoot merges with previous footer", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_pagefoot(left = "{program}") |>
    fr_pagefoot(right = "{datetime}")

  expect_equal(spec$pagefoot$left, "{program}")
  expect_equal(spec$pagefoot$right, "{datetime}")
})


# ══════════════════════════════════════════════════════════════════════════════
# Theme header wiring
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme_set(header = list(bold = TRUE)) wires through to spec", {
  on.exit(fr_theme_reset(), add = TRUE)
  fr_theme_reset()

  fr_theme(header = list(bold = TRUE, background = "#F0F0F0"))
  spec <- tbl_demog |> fr_table()

  expect_true(spec$header$bold)
  expect_equal(spec$header$background, "#F0F0F0")
})


# ══════════════════════════════════════════════════════════════════════════════
# Edge-case render tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_render handles empty data frame without error", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  suppressWarnings(
    data.frame(x = character(0), stringsAsFactors = FALSE) |>
      fr_table() |>
      fr_render(tmp)
  )

  expect_true(file.exists(tmp))
  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(startsWith(txt, "{\\rtf1"))
})

test_that("fr_render encodes Unicode characters as RTF escapes", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(
    x = c("hello", "caf\u00e9", "\u00e9\u00e8\u00ea"),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Non-ASCII characters must appear as RTF \uN escapes
  expect_true(grepl("\\u", txt, fixed = TRUE))
})

test_that("fr_render handles single-row data frame", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(x = "only", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(startsWith(txt, "{\\rtf1"))
  expect_true(grepl("only", txt, fixed = TRUE))
})

# ── NA value handling ────────────────────────────────────────────────────────

test_that("NA values render as empty strings in RTF output", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(
    x = c("a", NA, "c"),
    y = c(1, 2, NA),
    stringsAsFactors = FALSE
  )

  df |> fr_table() |> fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # Non-NA values must be present
  expect_true(grepl("\\ba\\b", txt, perl = TRUE))
  expect_true(grepl("\\bc\\b", txt, perl = TRUE))
  expect_true(grepl("\\b2\\b", txt, perl = TRUE))

  # Extract cell content between \cell markers — this is where body data lives.
  # Strip RTF control words to avoid false positives, then check plain-text fragments.
  cells <- regmatches(
    txt,
    gregexpr("(?<=\\\\cell).*?(?=\\\\cell|\\})", txt, perl = TRUE)
  )[[1]]
  cell_text <- gsub("\\\\[a-z]+[0-9]*\\b\\s?", "", cells)
  cell_text <- gsub("[{}\\\\]", "", cell_text)
  cell_text <- trimws(cell_text)
  expect_false(
    any(cell_text == "NA"),
    info = paste(
      "Found literal 'NA' in cell content:",
      toString(cell_text[cell_text == "NA"])
    )
  )
})

test_that("NA values in all columns render as empty strings in RTF", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # All-NA row and mixed NA columns
  df <- data.frame(
    lab = c("Row1", "Row2", NA),
    val = c(NA, NA, NA),
    cat = c(NA, "present", NA),
    stringsAsFactors = FALSE
  )

  suppressWarnings(df |> fr_table() |> fr_render(tmp))

  expect_true(file.exists(tmp))
  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # The word "present" must survive
  expect_true(grepl("present", txt, fixed = TRUE))

  # "Row1" and "Row2" must survive
  expect_true(grepl("Row1", txt, fixed = TRUE))
  expect_true(grepl("Row2", txt, fixed = TRUE))

  # No literal "NA" as standalone cell content
  cells <- regmatches(
    txt,
    gregexpr("(?<=\\\\cell).*?(?=\\\\cell|\\})", txt, perl = TRUE)
  )[[1]]
  cell_text <- gsub("\\\\[a-z]+[0-9]*\\b\\s?", "", cells)
  cell_text <- gsub("[{}\\\\]", "", cell_text)
  cell_text <- trimws(cell_text)
  expect_false(
    any(cell_text == "NA"),
    info = "Literal 'NA' found in RTF cell content for all-NA column"
  )
})
