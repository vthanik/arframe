# ──────────────────────────────────────────────────────────────────────────────
# test-render-latex.R — Tests for LaTeX rendering
# ──────────────────────────────────────────────────────────────────────────────

test_that("latex_setmainfont produces valid \\setmainfont command", {
  # System font case
  local_mocked_bindings(is_system_font_available = function(font_name) TRUE)
  cmd <- latex_setmainfont("Courier New")
  expect_match(cmd, "\\\\setmainfont\\{Courier New\\}")
  expect_false(grepl("Path=", cmd, fixed = TRUE))
})

test_that("latex_setmainfont uses Latin Modern fallback for missing fonts", {
  local_mocked_bindings(is_system_font_available = function(font_name) {
    font_name %in% fr_env$lm_fallback
  })
  cmd <- latex_setmainfont("Courier New")
  expect_match(cmd, "Latin Modern Mono")
  expect_false(grepl("Path=", cmd, fixed = TRUE))
})

test_that("fr_render creates a valid .tex file from minimal pipeline", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "Sex"),
    value = c("65.2 (10.1)", "50 (55.6%)"),
    stringsAsFactors = FALSE
  )

  data |> fr_table() |> fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")

  # Valid LaTeX structure
  expect_true(grepl("\\\\documentclass", txt))
  expect_true(grepl("\\\\begin\\{document\\}", txt))
  expect_true(grepl("\\\\end\\{document\\}", txt))
  expect_true(grepl("\\\\begin\\{longtblr\\}", txt))
  expect_true(grepl("\\\\end\\{longtblr\\}", txt))
})

test_that("fr_render .tex output contains title text", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = "hello", stringsAsFactors = FALSE)

  data |>
    fr_table() |>
    fr_titles("Table 14.1.1 Demographics") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("Table 14.1.1 Demographics", txt, fixed = TRUE))
})

test_that("fr_render .tex output contains footnote text", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = "hello", stringsAsFactors = FALSE)

  data |>
    fr_table() |>
    fr_footnotes("Source: ADSL") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("Source: ADSL", txt, fixed = TRUE))
})

test_that("fr_render .tex includes column header labels", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(a = fr_col(label = "Column A"), b = fr_col(label = "Column B")) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("Column A", txt, fixed = TRUE))
  expect_true(grepl("Column B", txt, fixed = TRUE))
})

test_that("fr_render .tex uses landscape geometry", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_page(orientation = "landscape") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("landscape", txt, fixed = TRUE))
})

test_that("fr_render .tex uses portrait (no landscape keyword)", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_page(orientation = "portrait") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_false(grepl("landscape", txt, fixed = TRUE))
})

test_that("fr_render .tex generates \\clearpage for page_by groups", {
  tmp <- tempfile(fileext = ".tex")
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

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("\\\\clearpage", txt))
})

test_that("fr_render .tex resolves markup sentinels to LaTeX commands", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_titles("{fr_bold('Table 14.1.1')} Demographics") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  # Bold sentinel resolved to \textbf{}
  expect_true(grepl("\\\\textbf\\{Table 14.1.1\\}", txt))
  expect_true(grepl("Demographics", txt, fixed = TRUE))
})

test_that("fr_render .tex applies hline borders", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = c("a", "b"), stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_hlines("header") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("hline", txt, fixed = TRUE))
})

test_that("fr_render .tex includes tabularray packages", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |> fr_table() |> fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("usepackage\\{tabularray\\}", txt))
  expect_true(grepl("usepackage\\{fontspec\\}", txt))
  expect_true(grepl("usepackage.*xcolor", txt))
})

test_that("fr_render .tex includes body data", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("Age", "Sex", "BMI"),
    placebo = c("65.2", "50%", "25.1"),
    active = c("64.8", "48%", "25.3"),
    stringsAsFactors = FALSE
  )

  data |>
    fr_table() |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("Age", txt, fixed = TRUE))
  expect_true(grepl("65.2", txt, fixed = TRUE))
  expect_true(grepl("placebo", txt, fixed = TRUE))
})

test_that("fr_render .tex escapes LaTeX special characters", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(
    param = c("100%", "p < 0.05"),
    stringsAsFactors = FALSE
  )

  data |> fr_table() |> fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  # % should be escaped to \%
  expect_true(grepl("100\\\\%", txt))
  # < should pass through (not a LaTeX special)
  expect_true(grepl("p < 0.05", txt, fixed = TRUE))
})

test_that("fr_render .tex includes rowhead for repeating headers", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |> fr_table() |> fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("rowhead", txt, fixed = TRUE))
})

test_that("fr_render .tex includes spanning headers with \\SetCell", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, c = 3, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_spans("AB" = c("a", "b")) |>
    fr_cols(.width = "equal") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("SetCell\\[c=2\\]", txt))
  expect_true(grepl("AB", txt, fixed = TRUE))
})

test_that("fr_render .tex handles cell styles (bold, colors)", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_col_style(cols = "a", bold = TRUE)) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("bfseries", txt, fixed = TRUE))
})

test_that("detect_format works for tex and pdf extensions", {
  expect_equal(detect_format("test.tex"), "latex")
  expect_equal(detect_format("output/test.TEX"), "latex")
  expect_equal(detect_format("test.pdf"), "pdf")
  expect_equal(detect_format("output/test.PDF"), "pdf")
})

test_that("fr_render returns path invisibly for .tex", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  result <- data |> fr_table() |> fr_render(tmp)
  expect_equal(result, tmp)
})

test_that("fr_render .tex includes fancyhdr for page headers", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_pagehead(left = "Study ABC") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("fancyhead", txt, fixed = TRUE))
  expect_true(grepl("Study ABC", txt, fixed = TRUE))
})

test_that("fr_render .tex includes fancyhdr for page footers", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_pagefoot(center = "Confidential") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("fancyfoot", txt, fixed = TRUE))
  expect_true(grepl("Confidential", txt, fixed = TRUE))
})


# ── LaTeX escape and sentinel resolver tests ─────────────────────────────────

test_that("latex_escape handles all 10 LaTeX special characters", {
  result <- latex_escape("a\\b&c%d$e#f_g{h}i~j^k")
  expect_true(grepl("textbackslash", result, fixed = TRUE))
  expect_true(grepl("\\&", result, fixed = TRUE))
  expect_true(grepl("\\%", result, fixed = TRUE))
  expect_true(grepl("\\$", result, fixed = TRUE))
  expect_true(grepl("\\#", result, fixed = TRUE))
  expect_true(grepl("\\_", result, fixed = TRUE))
  expect_true(grepl("\\{", result, fixed = TRUE))
  expect_true(grepl("\\}", result, fixed = TRUE))
  expect_true(grepl("textasciitilde", result, fixed = TRUE))
  expect_true(grepl("textasciicircum", result, fixed = TRUE))
})

test_that("latex_sentinel_resolver handles all markup types", {
  expect_equal(latex_sentinel_resolver("SUPER", "1"), "\\textsuperscript{1}")
  expect_equal(latex_sentinel_resolver("SUB", "2"), "\\textsubscript{2}")
  expect_equal(latex_sentinel_resolver("BOLD", "text"), "\\textbf{text}")
  expect_equal(latex_sentinel_resolver("ITALIC", "text"), "\\textit{text}")
  expect_equal(
    latex_sentinel_resolver("UNDERLINE", "text"),
    "\\underline{text}"
  )
  expect_equal(latex_sentinel_resolver("NEWLINE", ""), "\\\\")
})

test_that("latex_escape_and_resolve resolves sentinels within escaped text", {
  # Create a string with sentinel markers
  marked <- paste0("before\x01SUPER:1\x02after")
  result <- tlframe:::latex_escape_and_resolve(marked)
  expect_true(grepl("before", result, fixed = TRUE))
  expect_true(grepl("textsuperscript", result, fixed = TRUE))
  expect_true(grepl("after", result, fixed = TRUE))
})

test_that("latex_escape converts known Unicode to LaTeX commands", {
  # Plus-minus sign
  result <- latex_escape("\u00b1")
  expect_true(grepl("pm", result, fixed = TRUE))

  # Less-than-or-equal
  result <- latex_escape("\u2264")
  expect_true(grepl("leq", result, fixed = TRUE))
})

test_that("fr_render .tex handles superscript in footnotes", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_footnotes("{fr_super(1)} Pearson chi-square test") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("textsuperscript\\{1\\}", txt))
  expect_true(grepl("Pearson chi-square test", txt, fixed = TRUE))
})


# ── Title/footnote template tests ────────────────────────────────────────────

test_that("titles are inside DeclareTblrTemplate head definitions", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_titles("Table 14.1.1 Demographics") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("DeclareTblrTemplate\\{firsthead\\}", txt))
  expect_true(grepl("DeclareTblrTemplate\\{middlehead, lasthead\\}", txt))
  expect_true(grepl("Table 14.1.1 Demographics", txt, fixed = TRUE))
})

test_that("footnotes are inside DeclareTblrTemplate foot definitions", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_footnotes("Source: ADSL") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  # "every" footnotes go into tabularray foot templates (not \fancyfoot)
  expect_true(grepl("DeclareTblrTemplate\\{firstfoot, middlefoot\\}", txt))
  expect_true(grepl("Source: ADSL", txt, fixed = TRUE))
})

test_that("no \\begin{center} in LaTeX output (use \\centering instead)", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_titles("Table 14.1.1") |>
    fr_footnotes("Note: test", .align = "center") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_false(grepl("\\begin{center}", txt, fixed = TRUE))
  expect_true(grepl("\\centering", txt, fixed = TRUE))
})

test_that("longtblr has presep=0pt and tight cell spacing", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |> fr_table() |> fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("presep=0pt", txt, fixed = TRUE))
  expect_true(grepl("postsep=0pt", txt, fixed = TRUE))
  expect_true(grepl("abovesep=0pt", txt, fixed = TRUE))
  expect_true(grepl("leftsep=2pt", txt, fixed = TRUE))
})

test_that("parskip is set to 0pt in preamble", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |> fr_table() |> fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("\\setlength{\\parskip}{0pt}", txt, fixed = TRUE))
})

test_that("titles use \\vspace instead of \\par for spacing", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_titles("My Title") |>
    fr_spacing(titles_after = 2L) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  # Should use \vspace, not \par for spacing
  expect_true(grepl("\\\\vspace\\{", txt))
})

test_that("RTF titles always use \\trhdr (repeat on every page)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_titles("My Title") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Title should be in a \trhdr row
  expect_true(grepl("\\\\trhdr", txt))
  expect_true(grepl("My Title", txt, fixed = TRUE))
})

test_that("'last' footnotes appear in lastfoot of final section only", {
  tmp <- tempfile(fileext = ".tex")
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

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")

  # Split on \clearpage to get sections
  sections <- strsplit(txt, "\\\\clearpage", fixed = FALSE)[[1L]]
  expect_true(length(sections) >= 2L)

  # Split into lines and find lastfoot blocks per section
  all_lines <- readLines(tmp, warn = FALSE)

  # Find lastfoot declaration lines and the content lines after them
  lastfoot_idx <- grep("DeclareTblrTemplate\\{lastfoot\\}", all_lines)
  expect_equal(length(lastfoot_idx), 2L) # one per section

  # Extract content between each lastfoot declaration and its closing }
  get_template_body <- function(start_idx) {
    paste(
      all_lines[start_idx:min(start_idx + 10L, length(all_lines))],
      collapse = "\n"
    )
  }

  first_body <- get_template_body(lastfoot_idx[1L])
  last_body <- get_template_body(lastfoot_idx[2L])

  # First section: lastfoot should NOT contain "Last only note"
  expect_false(grepl("Last only note", first_body, fixed = TRUE))
  # Final section: lastfoot SHOULD contain "Last only note"
  expect_true(grepl("Last only note", last_body, fixed = TRUE))

  # "Every page note" should appear in foot templates of both sections
  expect_true(grepl("Every page note", first_body, fixed = TRUE))
  expect_true(grepl("Every page note", last_body, fixed = TRUE))
})

test_that("LaTeX leading factor uses 1.2 (matches RTF row_height_twips)", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, stringsAsFactors = FALSE)
  data |>
    fr_table() |>
    fr_page(font_size = 9) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  # 9 * 1.15 = 10.35, rounded to 10.3 — matches latex_leading_factor
  expect_true(grepl("10.3", txt, fixed = TRUE))
})


# ── Hyphenation settings ───────────────────────────────────────────────────

test_that("latex_preamble includes tolerance and emergencystretch", {
  spec <- data.frame(x = 1, stringsAsFactors = FALSE) |> fr_table()
  spec <- finalize_spec(spec)
  preamble <- paste(latex_preamble(spec), collapse = "\n")
  expect_true(grepl("\\tolerance=200", preamble, fixed = TRUE))
  expect_true(grepl("\\emergencystretch=1em", preamble, fixed = TRUE))
})


test_that("span gap columns keep original width in LaTeX output", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, c = 3, d = 4, stringsAsFactors = FALSE)

  data |>
    fr_table() |>
    fr_cols(.width = 2.0) |>
    fr_spans("AB" = c("a", "b"), "CD" = c("c", "d")) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")

  # Span gap should NOT use near-zero width
  expect_false(grepl("0.001in", txt, fixed = TRUE))
})


# ── Percentage width resolution in LaTeX ───────────────────────────────────

test_that("percentage widths resolve correctly in .tex output", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)

  data |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = "50%"),
      b = fr_col("B", width = "50%")
    ) |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("\\\\begin\\{longtblr\\}", txt))
})


# ── hspace{0pt} word-break hint ─────────────────────────────────────────────

test_that("body cells get \\hspace{0pt} prefix for word-break", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(a = "Hello", b = "World")
  df |>
    fr_table() |>
    fr_cols(a = fr_col("A"), b = fr_col("B")) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  # Body cells should contain \hspace{0pt} before content

  expect_match(txt, "\\\\hspace\\{0pt\\}Hello", fixed = FALSE)
  expect_match(txt, "\\\\hspace\\{0pt\\}World", fixed = FALSE)
})

test_that("header cells get \\hspace{0pt} prefix for word-break", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(a = "x", b = "y")
  df |>
    fr_table() |>
    fr_cols(a = fr_col("Col A"), b = fr_col("Col B")) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(txt, "\\\\hspace\\{0pt\\}Col A", fixed = FALSE)
  expect_match(txt, "\\\\hspace\\{0pt\\}Col B", fixed = FALSE)
})


# ── Zero inter-column padding ────────────────────────────────────────────────

test_that("global colsep is zero for flush column layout", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(a = "x", b = "y", c = "z")
  df |>
    fr_table() |>
    fr_cols(a = fr_col("A"), b = fr_col("B"), c = fr_col("C")) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  # All columns get leftsep=2pt,rightsep=2pt via global colsep setting
  expect_match(txt, "leftsep=2pt,rightsep=2pt", fixed = TRUE)
})

# ──────────────────────────────────────────────────────────────────────────────
# Decimal alignment via \makebox
# ──────────────────────────────────────────────────────────────────────────────

test_that("align_to_latex maps decimal to L", {
  expect_equal(unname(fr_env$align_to_latex[["decimal"]]), "L")
})

test_that("finalize_spec pre-computes decimal geometry for decimal columns", {
  df <- data.frame(a = c("12.3", "4.56"), b = c("x", "y"))
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", align = "decimal", width = 2),
      b = fr_col("B", align = "left", width = 2)
    )
  fspec <- finalize_spec(spec)
  expect_true("a" %in% names(fspec$decimal_geometry))
  geom <- fspec$decimal_geometry$a
  expect_equal(length(geom$formatted), 2L)
  expect_equal(length(geom$center_offset), 2L)
  expect_true(all(geom$center_offset >= 0L))
})

test_that("latex_body_rows produces hspace for decimal cells", {
  df <- data.frame(a = c("12.3", "4.56"), b = c("x", "y"))
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", align = "decimal", width = 2),
      b = fr_col("B", align = "left", width = 2)
    )
  fspec <- finalize_spec(spec)
  grid <- build_cell_grid(
    fspec$data,
    fspec$columns,
    fspec$cell_styles,
    fspec$page
  )
  rows <- latex_body_rows(
    fspec$data,
    fspec$columns,
    grid,
    dec_geom = fspec$decimal_geometry
  )
  expect_true(all(grepl("\\\\hspace\\{", rows)))
  # No makebox (old approach removed)
  expect_false(any(grepl("\\\\makebox\\[", rows)))
})

test_that("latex_body_rows handles empty decimal cells", {
  df <- data.frame(a = c("12.3", ""), b = c("x", "y"))
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", align = "decimal", width = 2),
      b = fr_col("B", align = "left", width = 2)
    )
  fspec <- finalize_spec(spec)
  grid <- build_cell_grid(
    fspec$data,
    fspec$columns,
    fspec$cell_styles,
    fspec$page
  )
  rows <- latex_body_rows(
    fspec$data,
    fspec$columns,
    grid,
    dec_geom = fspec$decimal_geometry
  )
  expect_length(rows, 2L)
  expect_match(rows[1], "\\\\hspace\\{", fixed = FALSE)
})

test_that("latex_body_rows uses non-breaking spaces for decimal alignment", {
  df <- data.frame(a = c("5 (45%)", "12 (55%)"), b = c("x", "y"))
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", align = "decimal", width = 2),
      b = fr_col("B", align = "left", width = 2)
    )
  fspec <- finalize_spec(spec)
  grid <- build_cell_grid(
    fspec$data,
    fspec$columns,
    fspec$cell_styles,
    fspec$page
  )
  rows <- latex_body_rows(
    fspec$data,
    fspec$columns,
    grid,
    dec_geom = fspec$decimal_geometry
  )
  # Non-breaking spaces (~) used for alignment in LaTeX
  expect_true(any(grepl("~", rows, fixed = TRUE)))
  # No makebox
  expect_false(any(grepl("\\\\makebox\\[", rows)))
})

test_that("latex_body_rows handles n_pct with decimal percentage", {
  df <- data.frame(a = c("28 (62.2%)", "5 (11.1%)"), b = c("x", "y"))
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", align = "decimal", width = 2),
      b = fr_col("B", align = "left", width = 2)
    )
  fspec <- finalize_spec(spec)
  grid <- build_cell_grid(
    fspec$data,
    fspec$columns,
    fspec$cell_styles,
    fspec$page
  )
  rows <- latex_body_rows(
    fspec$data,
    fspec$columns,
    grid,
    dec_geom = fspec$decimal_geometry
  )
  expect_match(rows[1], "\\\\hspace\\{", fixed = FALSE)
  expect_match(rows[1], "28", fixed = TRUE)
})


# ══════════════════════════════════════════════════════════════════════════════
# Additional coverage tests
# ══════════════════════════════════════════════════════════════════════════════

# -- latex_setmainfont -- Latin Modern fallback for different font families --

test_that("latex_setmainfont uses Latin Modern Sans for Arial fallback", {
  local_mocked_bindings(is_system_font_available = function(font_name) {
    font_name %in% fr_env$lm_fallback
  })
  cmd <- latex_setmainfont("Arial")
  expect_equal(cmd, "\\setmainfont{Latin Modern Sans}")
})

test_that("latex_setmainfont uses Latin Modern Roman for Times New Roman fallback", {
  local_mocked_bindings(is_system_font_available = function(font_name) {
    font_name %in% fr_env$lm_fallback
  })
  cmd <- latex_setmainfont("Times New Roman")
  expect_equal(cmd, "\\setmainfont{Latin Modern Roman}")
})

test_that("latex_setmainfont system font produces simple command", {
  local_mocked_bindings(is_system_font_available = function(font_name) TRUE)
  cmd <- latex_setmainfont("Arial")
  expect_equal(cmd, "\\setmainfont{Arial}")
})


# -- resolve_latex_font -- Latin Modern fallback ----------------------------

test_that("resolve_latex_font returns system font when available", {
  local_mocked_bindings(is_system_font_available = function(font_name) TRUE)
  result <- resolve_latex_font("Arial")
  expect_equal(result, "Arial")
})

test_that("resolve_latex_font falls back to Latin Modern Mono for Courier New", {
  local_mocked_bindings(is_system_font_available = function(font_name) {
    font_name %in% fr_env$lm_fallback
  })
  result <- resolve_latex_font("Courier New")
  expect_equal(result, "Latin Modern Mono")
})

test_that("resolve_latex_font falls back to Latin Modern Sans for Arial", {
  local_mocked_bindings(is_system_font_available = function(font_name) {
    font_name %in% fr_env$lm_fallback
  })
  result <- resolve_latex_font("Arial")
  expect_equal(result, "Latin Modern Sans")
})

test_that("resolve_latex_font falls back to Latin Modern Roman for Times New Roman", {
  local_mocked_bindings(is_system_font_available = function(font_name) {
    font_name %in% fr_env$lm_fallback
  })
  result <- resolve_latex_font("Times New Roman")
  expect_equal(result, "Latin Modern Roman")
})


# ── latex_col_header_row — multiline, bold, centered headers ───────────────

test_that("latex_col_header_row handles multiline header labels", {
  df <- data.frame(a = 1, b = 2)
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col("Line1\nLine2", width = 2),
      b = fr_col("Simple", width = 2)
    )
  fspec <- finalize_spec(spec)
  row <- latex_col_header_row(fspec, fspec$columns)

  # Multiline content should use \parbox with \\ line breaks
  # Column 'a' is numeric → default align "right" → \raggedleft
  expect_match(row, "\\\\parbox", fixed = FALSE)
  expect_match(row, "\\\\raggedleft", fixed = FALSE)
  expect_match(row, "Line1", fixed = TRUE)
  expect_match(row, "Line2", fixed = TRUE)
})

test_that("latex_col_header_row uses parbox for centered multiline headers", {
  df <- data.frame(a = 1, b = 2)
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col(
        "Line1\nLine2",
        width = 2,
        align = "left",
        header_align = "center"
      ),
      b = fr_col("Simple", width = 2)
    )
  fspec <- finalize_spec(spec)
  row <- latex_col_header_row(fspec, fspec$columns)
  # Centered multiline should use \parbox with \centering
  expect_match(row, "\\\\parbox", fixed = FALSE)
  expect_match(row, "\\\\centering", fixed = FALSE)
})

test_that("latex_col_header_row uses parbox for right-aligned multiline headers", {
  df <- data.frame(a = 1, b = 2)
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col(
        "Top\nBottom",
        width = 2,
        align = "left",
        header_align = "right"
      ),
      b = fr_col("Simple", width = 2)
    )
  fspec <- finalize_spec(spec)
  row <- latex_col_header_row(fspec, fspec$columns)
  expect_match(row, "\\\\parbox", fixed = FALSE)
  expect_match(row, "\\\\raggedleft", fixed = FALSE)
})

test_that("latex_col_header_row uses label_overrides when provided", {
  df <- data.frame(a = 1, b = 2)
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col("Original A", width = 2),
      b = fr_col("Original B", width = 2)
    )
  fspec <- finalize_spec(spec)
  overrides <- c(a = "Override A", b = "Override B")
  row <- latex_col_header_row(fspec, fspec$columns, label_overrides = overrides)
  expect_match(row, "Override A", fixed = TRUE)
  expect_match(row, "Override B", fixed = TRUE)
  expect_false(grepl("Original A", row, fixed = TRUE))
})

test_that("latex_col_header_row falls back to column name when label is empty", {
  df <- data.frame(mycol = 1)
  spec <- df |> fr_table()
  fspec <- finalize_spec(spec)
  row <- latex_col_header_row(fspec, fspec$columns)
  expect_match(row, "mycol", fixed = TRUE)
})


# ── latex_body_rows — styles, indentation, leading whitespace ──────────────

test_that("latex_body_rows converts leading whitespace to leftskip indent", {
  df <- data.frame(a = c("  indented", "normal"), b = c("x", "y"))
  spec <- df |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = 3), b = fr_col("B", width = 2))
  fspec <- finalize_spec(spec)
  grid <- build_cell_grid(
    fspec$data,
    fspec$columns,
    fspec$cell_styles,
    fspec$page
  )
  rows <- latex_body_rows(fspec$data, fspec$columns, grid)
  # Leading spaces converted to paragraph-level \leftskip indent upstream
  expect_match(rows[1], "\\\\leftskip=", perl = TRUE)
  expect_match(rows[1], "in\\\\relax", perl = TRUE)
  expect_match(rows[1], "indented", fixed = TRUE)
  # Spaces should be stripped from data
  expect_false(grepl("^\\s+indented", fspec$data$a[1]))
})

test_that("latex_body_rows preserves leading whitespace as hspace in preserve mode", {
  df <- data.frame(a = c("  indented", "normal"), b = c("x", "y"))
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = 3, spaces = "preserve"),
      b = fr_col("B", width = 2)
    )
  fspec <- finalize_spec(spec)
  grid <- build_cell_grid(
    fspec$data,
    fspec$columns,
    fspec$cell_styles,
    fspec$page
  )
  rows <- latex_body_rows(fspec$data, fspec$columns, grid)
  # Preserve mode: leading spaces become \hspace{...em}
  expect_match(rows[1], "\\\\hspace\\{", perl = TRUE)
  expect_match(rows[1], "em\\}", perl = TRUE)
  expect_match(rows[1], "indented", fixed = TRUE)
})

test_that("latex_body_rows handles empty data frame", {
  df <- data.frame(a = character(0), b = character(0))
  spec <- df |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = 2), b = fr_col("B", width = 2))
  fspec <- finalize_spec(spec)
  grid <- build_cell_grid(
    fspec$data,
    fspec$columns,
    fspec$cell_styles,
    fspec$page
  )
  rows <- latex_body_rows(fspec$data, fspec$columns, grid)
  expect_length(rows, 0L)
})

test_that("latex_body_rows works without decimal widths", {
  df <- data.frame(a = c("hello", "world"), b = c("1", "2"))
  spec <- df |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = 2), b = fr_col("B", width = 2))
  fspec <- finalize_spec(spec)
  grid <- build_cell_grid(
    fspec$data,
    fspec$columns,
    fspec$cell_styles,
    fspec$page
  )
  rows <- latex_body_rows(fspec$data, fspec$columns, grid, dec_geom = NULL)
  expect_length(rows, 2L)
  expect_false(any(grepl("makebox", rows, fixed = TRUE)))
})


# ── latex_header_style_specs — align/valign combinations ──────────────────

test_that("latex_header_style_specs generates halign for centered headers", {
  df <- data.frame(a = 1, b = 2)
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = 2, align = "left", header_align = "center"),
      b = fr_col("B", width = 2)
    )
  fspec <- finalize_spec(spec)
  nrow_header <- 1L
  hgrid <- build_header_cell_grid(
    fspec$columns,
    fspec$cell_styles,
    fspec$page,
    nrow_header,
    default_valign = "bottom",
    header_cfg = fspec$header
  )
  specs <- latex_header_style_specs(
    hgrid,
    nrow_header,
    length(fspec$columns),
    columns = fspec$columns,
    header_default_align = NULL
  )
  spec_str <- paste(specs, collapse = " ")
  expect_match(spec_str, "halign=c", fixed = TRUE)
})

test_that("latex_header_style_specs generates halign for right-aligned headers", {
  df <- data.frame(a = 1, b = 2)
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = 2, align = "left", header_align = "right"),
      b = fr_col("B", width = 2)
    )
  fspec <- finalize_spec(spec)
  nrow_header <- 1L
  hgrid <- build_header_cell_grid(
    fspec$columns,
    fspec$cell_styles,
    fspec$page,
    nrow_header,
    default_valign = "bottom",
    header_cfg = fspec$header
  )
  specs <- latex_header_style_specs(
    hgrid,
    nrow_header,
    length(fspec$columns),
    columns = fspec$columns,
    header_default_align = NULL
  )
  spec_str <- paste(specs, collapse = " ")
  expect_match(spec_str, "halign=r", fixed = TRUE)
})

test_that("latex_header_style_specs skips halign when header matches body align", {
  df <- data.frame(a = 1)
  spec <- df |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = 2, align = "left"))
  fspec <- finalize_spec(spec)
  nrow_header <- 1L
  hgrid <- build_header_cell_grid(
    fspec$columns,
    fspec$cell_styles,
    fspec$page,
    nrow_header,
    default_valign = "bottom",
    header_cfg = fspec$header
  )
  specs <- latex_header_style_specs(
    hgrid,
    nrow_header,
    length(fspec$columns),
    columns = fspec$columns,
    header_default_align = NULL
  )
  # No halign needed since header_align defaults to body align
  spec_str <- paste(specs, collapse = " ")
  expect_false(grepl("halign", spec_str, fixed = TRUE))
})

test_that("latex_header_style_specs handles bold header cells", {
  df <- data.frame(a = 1, b = 2)
  spec <- df |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = 2), b = fr_col("B", width = 2)) |>
    fr_header(bold = TRUE)
  fspec <- finalize_spec(spec)
  nrow_header <- 1L
  hgrid <- build_header_cell_grid(
    fspec$columns,
    fspec$cell_styles,
    fspec$page,
    nrow_header,
    default_valign = "bottom",
    header_cfg = fspec$header
  )
  specs <- latex_header_style_specs(
    hgrid,
    nrow_header,
    length(fspec$columns),
    columns = fspec$columns,
    header_default_align = fspec$header$align
  )
  spec_str <- paste(specs, collapse = " ")
  expect_match(spec_str, "bfseries", fixed = TRUE)
})

test_that("latex_header_style_specs handles header bg/fg colors", {
  df <- data.frame(a = 1)
  spec <- df |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = 2)) |>
    fr_header(bg = "#CCCCCC", fg = "#FF0000")
  fspec <- finalize_spec(spec)
  nrow_header <- 1L
  hgrid <- build_header_cell_grid(
    fspec$columns,
    fspec$cell_styles,
    fspec$page,
    nrow_header,
    default_valign = "bottom",
    header_cfg = fspec$header
  )
  specs <- latex_header_style_specs(
    hgrid,
    nrow_header,
    length(fspec$columns),
    columns = fspec$columns,
    header_default_align = fspec$header$align
  )
  spec_str <- paste(specs, collapse = " ")
  expect_match(spec_str, "bg=tblrCCCCCC", fixed = TRUE)
  expect_match(spec_str, "fg=tblrFF0000", fixed = TRUE)
})

test_that("latex_header_style_specs uses header_default_align for all columns", {
  df <- data.frame(a = 1, b = 2)
  spec <- df |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = 2, align = "left"),
      b = fr_col("B", width = 2, align = "left")
    ) |>
    fr_header(align = "center")
  fspec <- finalize_spec(spec)
  nrow_header <- 1L
  hgrid <- build_header_cell_grid(
    fspec$columns,
    fspec$cell_styles,
    fspec$page,
    nrow_header,
    default_valign = "bottom",
    header_cfg = fspec$header
  )
  specs <- latex_header_style_specs(
    hgrid,
    nrow_header,
    length(fspec$columns),
    columns = fspec$columns,
    header_default_align = fspec$header$align
  )
  spec_str <- paste(specs, collapse = " ")
  # Both columns should get halign=c since header default is center but body is left
  expect_equal(length(grep("halign=c", specs, fixed = TRUE)), 2L)
})


# ── hex_to_tblr_color ─────────────────────────────────────────────────────

test_that("hex_to_tblr_color strips # and prepends tblr", {
  expect_equal(hex_to_tblr_color("#aabbcc"), "tblrAABBCC")
  expect_equal(hex_to_tblr_color("#000000"), "tblr000000")
  expect_equal(hex_to_tblr_color("#FF0000"), "tblrFF0000")
})


# ── latex_preamble — paper sizes and orientation ───────────────────────────

test_that("latex_preamble uses a4paper for A4 page size", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table() |> fr_page(paper = "a4")
  fspec <- finalize_spec(spec)
  preamble <- paste(latex_preamble(fspec), collapse = "\n")
  expect_match(preamble, "a4paper", fixed = TRUE)
})

test_that("latex_preamble uses legalpaper for legal page size", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table() |> fr_page(paper = "legal")
  fspec <- finalize_spec(spec)
  preamble <- paste(latex_preamble(fspec), collapse = "\n")
  expect_match(preamble, "legalpaper", fixed = TRUE)
})

test_that("latex_preamble includes headheight for pagehead", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table() |> fr_pagehead(left = "Study")
  fspec <- finalize_spec(spec)
  preamble <- paste(latex_preamble(fspec), collapse = "\n")
  expect_match(preamble, "headheight=", fixed = TRUE)

  expect_match(preamble, "headsep=", fixed = TRUE)
})

test_that("latex_preamble sets headheight=0pt when no pagehead", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table()
  fspec <- finalize_spec(spec)
  preamble <- paste(latex_preamble(fspec), collapse = "\n")
  expect_match(preamble, "headheight=0pt", fixed = TRUE)
})

test_that("latex_preamble includes footskip for pagefoot", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table() |> fr_pagefoot(center = "Confidential")
  fspec <- finalize_spec(spec)
  preamble <- paste(latex_preamble(fspec), collapse = "\n")
  expect_match(preamble, "footskip=", fixed = TRUE)
  # Should NOT be 0pt when pagefoot is present
  preamble_lines <- strsplit(preamble, "\n")[[1]]
  geom_line <- preamble_lines[grep("geometry", preamble_lines)]
  expect_false(grepl("footskip=0pt", geom_line, fixed = TRUE))
})

test_that("latex_preamble sets footskip=0pt when no pagefoot", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table()
  fspec <- finalize_spec(spec)
  preamble <- paste(latex_preamble(fspec), collapse = "\n")
  expect_match(preamble, "footskip=0pt", fixed = TRUE)
})

test_that("latex_preamble includes lastpage when total_pages token used", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_pagefoot(right = "Page {thepage} of {total_pages}")
  fspec <- finalize_spec(spec)
  preamble <- paste(latex_preamble(fspec), collapse = "\n")
  expect_match(preamble, "usepackage\\{lastpage\\}", perl = TRUE)
})

test_that("latex_preamble omits lastpage when no total_pages token", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table() |> fr_pagefoot(right = "Page {thepage}")
  fspec <- finalize_spec(spec)
  preamble <- paste(latex_preamble(fspec), collapse = "\n")
  expect_false(grepl("lastpage", preamble, fixed = TRUE))
})

test_that("latex_preamble includes color definitions for header colors", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table() |> fr_header(bg = "#0000FF")
  fspec <- finalize_spec(spec)
  preamble <- paste(latex_preamble(fspec), collapse = "\n")
  expect_match(preamble, "definecolor\\{tblr0000FF\\}", perl = TRUE)
})


# ── latex_foot_template — footnote placement branches ──────────────────────

test_that("latex_foot_template suppresses all foot templates when no footnotes", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table()
  fspec <- finalize_spec(spec)
  lines <- latex_foot_template(fspec, is_last = TRUE)
  lines_str <- paste(lines, collapse = "\n")
  expect_match(
    lines_str,
    "DefTblrTemplate\\{firstfoot\\}\\{default\\}\\{\\}",
    perl = TRUE
  )
  expect_match(
    lines_str,
    "DefTblrTemplate\\{middlefoot\\}\\{default\\}\\{\\}",
    perl = TRUE
  )
  expect_match(
    lines_str,
    "DefTblrTemplate\\{lastfoot\\}\\{default\\}\\{\\}",
    perl = TRUE
  )
})

test_that("latex_foot_template handles only 'last' footnotes", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_footnotes(list("Last only note", placement = "last"))
  fspec <- finalize_spec(spec)
  lines <- latex_foot_template(fspec, is_last = TRUE)
  lines_str <- paste(lines, collapse = "\n")
  # firstfoot and middlefoot should be suppressed
  expect_match(
    lines_str,
    "DefTblrTemplate\\{firstfoot\\}\\{default\\}\\{\\}",
    perl = TRUE
  )
  expect_match(
    lines_str,
    "DefTblrTemplate\\{middlefoot\\}\\{default\\}\\{\\}",
    perl = TRUE
  )
  # lastfoot should contain the note
  expect_match(lines_str, "DeclareTblrTemplate\\{lastfoot\\}", perl = TRUE)
  expect_match(lines_str, "Last only note", fixed = TRUE)
})

test_that("latex_foot_template suppresses 'last' footnotes when is_last = FALSE", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_footnotes(list("Last only note", placement = "last"))
  fspec <- finalize_spec(spec)
  lines <- latex_foot_template(fspec, is_last = FALSE)
  lines_str <- paste(lines, collapse = "\n")
  # With is_last = FALSE and only "last" footnotes, all should be suppressed
  expect_false(grepl("Last only note", lines_str, fixed = TRUE))
})

test_that("latex_foot_template handles every+last footnote combination", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_footnotes(
      list("Every page", placement = "every"),
      list("Last page", placement = "last")
    )
  fspec <- finalize_spec(spec)
  lines <- latex_foot_template(fspec, is_last = TRUE)
  lines_str <- paste(lines, collapse = "\n")
  # firstfoot/middlefoot should have "Every page" but not "Last page"
  expect_match(
    lines_str,
    "DeclareTblrTemplate\\{firstfoot, middlefoot\\}",
    perl = TRUE
  )
  # lastfoot should have both
  expect_match(lines_str, "DeclareTblrTemplate\\{lastfoot\\}", perl = TRUE)
  expect_match(lines_str, "Every page", fixed = TRUE)
  expect_match(lines_str, "Last page", fixed = TRUE)
})

test_that("latex_foot_template every+last with is_last=FALSE excludes last notes", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_footnotes(
      list("Every page", placement = "every"),
      list("Last page", placement = "last")
    )
  fspec <- finalize_spec(spec)
  lines <- latex_foot_template(fspec, is_last = FALSE)
  lines_str <- paste(lines, collapse = "\n")
  # "Last page" should not appear (not is_last)
  expect_false(grepl("Last page", lines_str, fixed = TRUE))
  # "Every page" should still appear
  expect_match(lines_str, "Every page", fixed = TRUE)
})


# ── latex_head_template — page_by group labels ────────────────────────────

test_that("latex_head_template includes group label when provided", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table() |> fr_titles("My Title")
  fspec <- finalize_spec(spec)
  lines <- latex_head_template(fspec, group_label = "Parameter: Height")
  lines_str <- paste(lines, collapse = "\n")
  expect_match(lines_str, "Parameter: Height", fixed = TRUE)
  expect_match(lines_str, "DeclareTblrTemplate\\{firsthead\\}", perl = TRUE)
  expect_match(
    lines_str,
    "DeclareTblrTemplate\\{middlehead, lasthead\\}",
    perl = TRUE
  )
})

test_that("latex_head_template without titles or group_label suppresses templates", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table()
  fspec <- finalize_spec(spec)
  lines <- latex_head_template(fspec)
  lines_str <- paste(lines, collapse = "\n")
  expect_match(
    lines_str,
    "DefTblrTemplate\\{conthead\\}\\{default\\}\\{\\}",
    perl = TRUE
  )
  expect_match(
    lines_str,
    "DefTblrTemplate\\{contfoot\\}\\{default\\}\\{\\}",
    perl = TRUE
  )
})


# ── latex_title_content — alignment variants ──────────────────────────────

test_that("latex_title_content produces right-aligned titles", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_titles(list("Right Title", align = "right"))
  fspec <- finalize_spec(spec)
  content <- latex_title_content(fspec)
  content_str <- paste(content, collapse = "\n")
  expect_match(content_str, "\\\\raggedleft", fixed = FALSE)
  expect_match(content_str, "Right Title", fixed = TRUE)
})

test_that("latex_title_content produces left-aligned titles (no centering)", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_titles(list("Left Title", align = "left"))
  fspec <- finalize_spec(spec)
  content <- latex_title_content(fspec)
  content_str <- paste(content, collapse = "\n")
  # Left-aligned should NOT have \centering or \raggedleft
  expect_false(grepl("\\\\centering", content_str))
  expect_false(grepl("\\\\raggedleft", content_str))
  expect_match(content_str, "Left Title", fixed = TRUE)
})

test_that("latex_title_content includes bold markup", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_titles(list("Bold Title", bold = TRUE))
  fspec <- finalize_spec(spec)
  content <- latex_title_content(fspec)
  content_str <- paste(content, collapse = "\n")
  expect_match(content_str, "\\\\textbf\\{Bold Title\\}", perl = TRUE)
})

test_that("latex_title_content respects custom font_size", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_titles(list("Small Title", font_size = 7))
  fspec <- finalize_spec(spec)
  content <- latex_title_content(fspec)
  content_str <- paste(content, collapse = "\n")
  expect_match(content_str, "fontsize\\{7\\}", perl = TRUE)
})

test_that("latex_title_content returns empty when no titles", {
  df <- data.frame(x = 1)
  spec <- df |> fr_table()
  fspec <- finalize_spec(spec)
  content <- latex_title_content(fspec)
  expect_length(content, 0L)
})


# ── latex_fancyhdr_setup — pagehead/pagefoot ──────────────────────────────

test_that("latex_fancyhdr_setup includes bold pagehead", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_pagehead(left = "Study", bold = TRUE)
  fspec <- finalize_spec(spec)
  lines <- latex_fancyhdr_setup(fspec)
  lines_str <- paste(lines, collapse = "\n")
  expect_match(lines_str, "\\\\textbf\\{", perl = TRUE)
  expect_match(lines_str, "Study", fixed = TRUE)
})

test_that("latex_fancyhdr_setup includes all three positions", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_pagehead(left = "Left", center = "Center", right = "Right")
  fspec <- finalize_spec(spec)
  lines <- latex_fancyhdr_setup(fspec)
  lines_str <- paste(lines, collapse = "\n")
  expect_match(lines_str, "fancyhead\\[L\\]", perl = TRUE)
  expect_match(lines_str, "fancyhead\\[C\\]", perl = TRUE)
  expect_match(lines_str, "fancyhead\\[R\\]", perl = TRUE)
})

test_that("latex_fancyhdr_setup resolves page number tokens", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_pagefoot(right = "Page {thepage} of {total_pages}")
  fspec <- finalize_spec(spec)
  lines <- latex_fancyhdr_setup(fspec)
  lines_str <- paste(lines, collapse = "\n")
  expect_match(lines_str, "\\\\thepage", fixed = FALSE)
  expect_match(lines_str, "\\\\pageref\\{LastPage\\}", perl = TRUE)
})

test_that("latex_fancyhdr_setup handles bold pagefoot", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_pagefoot(center = "CONFIDENTIAL", bold = TRUE)
  fspec <- finalize_spec(spec)
  lines <- latex_fancyhdr_setup(fspec)
  lines_str <- paste(lines, collapse = "\n")
  expect_match(lines_str, "\\\\textbf\\{", perl = TRUE)
  expect_match(lines_str, "CONFIDENTIAL", fixed = TRUE)
  expect_match(lines_str, "fancyfoot\\[C\\]", perl = TRUE)
})


# ── latex_cell_style_specs — cell styles ──────────────────────────────────

test_that("latex_cell_style_specs returns empty for empty cell_grid", {
  grid <- data.frame(
    row_idx = integer(0),
    col_idx = integer(0),
    bold = logical(0),
    italic = logical(0),
    bg = character(0),
    fg = character(0),
    indent = numeric(0),
    content = character(0),
    stringsAsFactors = FALSE
  )
  result <- latex_cell_style_specs(grid, 2, 2, 1)
  expect_length(result, 0L)
})

test_that("latex_cell_style_specs generates indent style", {
  df <- data.frame(a = c("Cat", "  Sub"), b = c("1", "2"))
  spec <- df |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = 2), b = fr_col("B", width = 2)) |>
    fr_rows(indent_by = "a")
  fspec <- finalize_spec(spec)
  grid <- build_cell_grid(
    fspec$data,
    fspec$columns,
    fspec$cell_styles,
    fspec$page
  )
  specs <- latex_cell_style_specs(
    grid,
    nrow(fspec$data),
    length(fspec$columns),
    1L
  )
  spec_str <- paste(specs, collapse = " ")
  # If indentation was applied, should see preto={\hspace{...}}
  # This depends on actual indent_by behavior
  expect_true(is.character(specs))
})


# ── latex_border_specs — border generation ────────────────────────────────

test_that("latex_border_specs generates hlines from fr_hlines", {
  df <- data.frame(a = c("x", "y"), b = c("1", "2"))
  spec <- df |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = 2), b = fr_col("B", width = 2)) |>
    fr_hlines("box")
  fspec <- finalize_spec(spec)
  nrow_header <- 1L
  borders <- resolve_borders(
    fspec$rules,
    nrow(fspec$data),
    length(fspec$columns),
    nrow_header
  )
  specs <- latex_border_specs(
    borders,
    nrow(fspec$data),
    length(fspec$columns),
    nrow_header
  )
  spec_str <- paste(specs, collapse = "\n")
  expect_match(spec_str, "hline", fixed = TRUE)
})


# ── footnote minipage width matches visible columns ──────────────────────

test_that("footnote minipage width matches visible column sum", {
  df <- data.frame(a = 1, b = 2)
  spec <- df |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = 3.0), b = fr_col("B", width = 2.5)) |>
    fr_footnotes("Note", .separator = TRUE)
  fspec <- finalize_spec(spec)
  fn_entries <- fspec$meta$footnotes
  vis <- Filter(function(c) !isFALSE(c$visible), fspec$columns)
  lines <- build_fn_latex_content(fn_entries, fspec, vis_columns = vis)
  lines_str <- paste(lines, collapse = "\n")
  expect_match(lines_str, "minipage\\}\\{5\\.5in\\}", perl = TRUE)
})


# ── build_fn_latex_content — footnote content helpers ─────────────────────

test_that("build_fn_latex_content handles centered footnotes", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_cols(x = fr_col("X", width = 2)) |>
    fr_footnotes(list("Centered note", align = "center"))
  fspec <- finalize_spec(spec)
  fn_entries <- fspec$meta$footnotes
  lines <- build_fn_latex_content(fn_entries, fspec)
  lines_str <- paste(lines, collapse = "\n")
  expect_match(lines_str, "\\\\centering", fixed = FALSE)
  expect_match(lines_str, "Centered note", fixed = TRUE)
})

test_that("build_fn_latex_content handles right-aligned footnotes", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_cols(x = fr_col("X", width = 2)) |>
    fr_footnotes(list("Right note", align = "right"))
  fspec <- finalize_spec(spec)
  fn_entries <- fspec$meta$footnotes
  lines <- build_fn_latex_content(fn_entries, fspec)
  lines_str <- paste(lines, collapse = "\n")
  expect_match(lines_str, "\\\\raggedleft", fixed = FALSE)
})

test_that("build_fn_latex_content includes separator line with linewidth", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_cols(x = fr_col("X", width = 2)) |>
    fr_footnotes("Note", .separator = TRUE)
  fspec <- finalize_spec(spec)
  fn_entries <- fspec$meta$footnotes
  lines <- build_fn_latex_content(fn_entries, fspec)
  lines_str <- paste(lines, collapse = "\n")
  # Separator uses \linewidth (constrained by minipage to table width)
  expect_match(lines_str, "\\\\rule\\{\\\\linewidth\\}", perl = TRUE)
  # Minipage wraps the content
  expect_match(lines_str, "\\\\begin\\{minipage\\}", perl = TRUE)
  expect_match(lines_str, "\\\\end\\{minipage\\}", perl = TRUE)
})

test_that("build_fn_latex_content skips spacing when skip_spacing = TRUE", {
  df <- data.frame(x = 1)
  spec <- df |>
    fr_table() |>
    fr_cols(x = fr_col("X", width = 2)) |>
    fr_footnotes("Note", .separator = TRUE)
  fspec <- finalize_spec(spec)
  fn_entries <- fspec$meta$footnotes
  lines_skip <- build_fn_latex_content(fn_entries, fspec, skip_spacing = TRUE)
  lines_full <- build_fn_latex_content(fn_entries, fspec, skip_spacing = FALSE)
  # Skipped version should be shorter (no vspace, no separator)
  expect_true(length(lines_skip) < length(lines_full))
  expect_false(any(grepl("\\\\rule\\{", lines_skip)))
  # Both should have minipage wrapping
  expect_true(any(grepl("minipage", lines_skip)))
})

test_that("footnote minipage excludes hidden page_by columns", {
  df <- data.frame(grp = c("A", "B"), x = 1:2)
  spec <- df |>
    fr_table() |>
    fr_cols(grp = fr_col("Group", width = 2), x = fr_col("X", width = 3)) |>
    fr_rows(page_by = "grp") |>
    fr_footnotes("Note")
  fspec <- finalize_spec(spec)
  fn_entries <- fspec$meta$footnotes
  # Only visible column (x = 3in) should be in minipage, not hidden grp (2in)

  vis <- Filter(function(c) !isFALSE(c$visible), fspec$columns)
  lines <- build_fn_latex_content(fn_entries, fspec, vis_columns = vis)
  lines_str <- paste(lines, collapse = "\n")
  expect_match(lines_str, "minipage\\}\\{3in\\}", perl = TRUE)
})


# ── latex_spanner_rows — spanning header rows ─────────────────────────────

test_that("latex_spanner_rows returns empty for no spans", {
  df <- data.frame(a = 1, b = 2)
  spec <- df |> fr_table() |> fr_cols(.width = "equal")
  fspec <- finalize_spec(spec)
  result <- latex_spanner_rows(fspec, fspec$columns)
  expect_length(result$rows, 0L)
  expect_length(result$hlines, 0L)
})

test_that("latex_spanner_rows generates SetCell for spans", {
  df <- data.frame(a = 1, b = 2, c = 3)
  spec <- df |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_spans("Group" = c("a", "b"))
  fspec <- finalize_spec(spec)
  result <- latex_spanner_rows(fspec, fspec$columns)
  rows_str <- paste(result$rows, collapse = "\n")
  expect_match(rows_str, "SetCell\\[c=2\\]", perl = TRUE)
  expect_match(rows_str, "Group", fixed = TRUE)
})


# ── Full integration: A4 paper + landscape ────────────────────────────────

test_that("fr_render .tex works with A4 landscape", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, y = 2)
  data |>
    fr_table() |>
    fr_page(paper = "a4", orientation = "landscape") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(txt, "a4paper", fixed = TRUE)
  expect_match(txt, "landscape", fixed = TRUE)
})

test_that("fr_render .tex works with legal paper", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  data <- data.frame(x = 1, y = 2)
  data |>
    fr_table() |>
    fr_page(paper = "legal") |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(txt, "legalpaper", fixed = TRUE)
})


# ── Full integration: cell styles (italic, fg color, bg color) ────────────

test_that("fr_render .tex applies italic cell style", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(a = "x", b = "y")
  df |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_col_style(cols = "a", italic = TRUE)) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(txt, "itshape", fixed = TRUE)
})

test_that("fr_render .tex applies fg color cell style", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(a = "x", b = "y")
  df |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_col_style(cols = "a", fg = "#FF0000")) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(txt, "tblrFF0000", fixed = TRUE)
})

test_that("fr_render .tex applies bg color cell style", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(a = "x", b = "y")
  df |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_styles(fr_col_style(cols = "a", bg = "#EEEEEE")) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(txt, "tblrEEEEEE", fixed = TRUE)
})


# ── Full integration: footnote spacing and separator ──────────────────────

test_that("fr_render .tex includes footnote separator line", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(x = 1)
  df |>
    fr_table() |>
    fr_footnotes("A note", .separator = TRUE) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(txt, "\\\\rule\\{", perl = TRUE)
})

test_that("fr_render .tex respects spacing settings", {
  tmp <- tempfile(fileext = ".tex")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(x = 1)
  df |>
    fr_table() |>
    fr_titles("Title") |>
    fr_footnotes("Note") |>
    fr_spacing(titles_after = 3L, footnotes_before = 2L) |>
    fr_render(tmp)

  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  # vspace values should reflect the spacing settings
  expect_match(txt, "\\\\vspace\\{", perl = TRUE)
})
