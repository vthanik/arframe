# ─────────────────────────────────────────────────────────────────────────────
# test-listing-figure.R — Tests for fr_listing() and fr_figure() in api.R
# ─────────────────────────────────────────────────────────────────────────────

# Shared fixtures
df_listing <- data.frame(
  USUBJID = c("SUBJ-001", "SUBJ-001", "SUBJ-002"),
  AEDECOD = c("Headache", "Nausea", "Dizziness"),
  AESEV = c("MILD", "MODERATE", "MILD"),
  ASTDY = c(5L, 12L, 3L),
  stringsAsFactors = FALSE
)

df_multi <- data.frame(
  USUBJID = c("S01", "S02", "S03"),
  TRTA = c("Placebo", "Drug A", "Placebo"),
  AEDECOD = c("Headache", "Nausea", "Cough"),
  stringsAsFactors = FALSE
)


# ══════════════════════════════════════════════════════════════════════════════
# fr_listing — basic construction
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_listing creates fr_spec with type = listing", {
  spec <- fr_listing(df_listing)
  expect_s3_class(spec, "fr_spec")
  expect_equal(spec$type, "listing")
})

test_that("fr_listing stores the input data unchanged", {
  spec <- fr_listing(df_listing)
  expect_identical(spec$data, df_listing)
})

test_that("fr_listing has default sub-structures", {
  spec <- fr_listing(df_listing)
  expect_s3_class(spec$meta, "fr_meta")
  expect_s3_class(spec$page, "fr_page")
  expect_s3_class(spec$body, "fr_body")
  expect_s3_class(spec$header, "fr_header")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_listing — listing-specific defaults
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_listing sets font_size = 8", {
  spec <- fr_listing(df_listing)
  expect_equal(spec$page$font_size, 8)
})

test_that("fr_listing enables col_split", {
  spec <- fr_listing(df_listing)
  expect_true(isTRUE(spec$columns_meta$split))
})

test_that("fr_listing enables wrap", {
  spec <- fr_listing(df_listing)
  expect_true(spec$body$wrap)
})

test_that("fr_listing sets left alignment for all columns", {
  spec <- fr_listing(df_listing)
  for (nm in names(df_listing)) {
    expect_equal(
      spec$columns[[nm]]$align,
      "left",
      label = paste0("column '", nm, "' align")
    )
  }
})

test_that("fr_listing sets header hline by default", {
  spec <- fr_listing(df_listing)
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_length(hlines, 1L)
  expect_equal(hlines[[1]]$region, "header")
  expect_equal(hlines[[1]]$side, "below")
})

test_that("fr_listing column ids match data names", {
  spec <- fr_listing(df_listing)
  for (nm in names(df_listing)) {
    expect_equal(
      spec$columns[[nm]]$id,
      nm,
      label = paste0("column '", nm, "' id")
    )
  }
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_listing — defaults are overridable
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_listing font_size overridable via fr_page", {
  spec <- df_listing |>
    fr_listing() |>
    fr_page(font_size = 10)
  expect_equal(spec$page$font_size, 10)
})

test_that("fr_listing split overridable via columns_meta", {
  spec <- df_listing |>
    fr_listing()
  spec$columns_meta$split <- FALSE
  expect_false(spec$columns_meta$split)
})

test_that("fr_listing wrap overridable via fr_rows", {
  spec <- df_listing |>
    fr_listing() |>
    fr_rows(wrap = FALSE)
  expect_false(spec$body$wrap)
})

test_that("fr_listing alignment overridable via fr_cols", {
  spec <- df_listing |>
    fr_listing() |>
    fr_cols(ASTDY = fr_col("Study Day", align = "right"))
  expect_equal(spec$columns[["ASTDY"]]$align, "right")
})

test_that("fr_listing hlines overridable", {
  spec <- df_listing |>
    fr_listing() |>
    fr_hlines("booktabs")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_length(hlines, 3L)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_listing — pipeline compatibility
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_listing works with fr_titles and fr_footnotes", {
  spec <- df_listing |>
    fr_listing() |>
    fr_titles("Listing 16.2.7 Adverse Events") |>
    fr_footnotes("Source: ADAE")
  expect_length(spec$meta$titles, 1L)
  expect_length(spec$meta$footnotes, 1L)
  expect_equal(spec$meta$titles[[1]]$content, "Listing 16.2.7 Adverse Events")
})

test_that("fr_listing works with fr_rows sort_by", {
  spec <- df_listing |>
    fr_listing() |>
    fr_rows(sort_by = c("USUBJID", "ASTDY"))
  expect_equal(spec$body$sort_by, c("USUBJID", "ASTDY"))
})

test_that("fr_listing works with fr_rows repeat_cols", {
  spec <- df_listing |>
    fr_listing() |>
    fr_rows(repeat_cols = "USUBJID")
  expect_equal(spec$body$repeat_cols, "USUBJID")
})

test_that("fr_listing works with fr_rows page_by", {
  spec <- df_multi |>
    fr_listing() |>
    fr_rows(page_by = "TRTA")
  expect_equal(spec$body$page_by, "TRTA")
})

test_that("fr_listing works with fr_cols column config", {
  spec <- df_listing |>
    fr_listing() |>
    fr_cols(
      USUBJID = fr_col("Subject ID", width = 1.2),
      AEDECOD = fr_col("Preferred Term"),
      AESEV = fr_col("Severity", width = 1.0)
    )
  expect_equal(spec$columns[["USUBJID"]]$label, "Subject ID")
  expect_equal(spec$columns[["USUBJID"]]$width, 1.2)
  expect_equal(spec$columns[["AESEV"]]$width, 1.0)
})

test_that("fr_listing works with fr_header", {
  spec <- df_listing |>
    fr_listing() |>
    fr_header(bold = TRUE)
  expect_true(spec$header$bold)
})

test_that("fr_listing works with fr_spans", {
  spec <- df_listing |>
    fr_listing() |>
    fr_spans("AE Details" = c("AEDECOD", "AESEV"))
  expect_length(spec$header$spans, 1L)
  expect_equal(spec$header$spans[[1]]$label, "AE Details")
})

test_that("fr_listing works with fr_pagehead and fr_pagefoot", {
  spec <- df_listing |>
    fr_listing() |>
    fr_pagehead(left = "Study XYZ") |>
    fr_pagefoot(right = "{datetime}")
  expect_s3_class(spec$pagehead, "fr_pagechrome")
  expect_s3_class(spec$pagefoot, "fr_pagechrome")
  expect_equal(spec$pagehead$left, "Study XYZ")
})

test_that("fr_listing works with fr_styles", {
  spec <- df_listing |>
    fr_listing() |>
    fr_styles(fr_row_style(rows = 1L, bold = TRUE))
  expect_length(spec$cell_styles, 1L)
})

test_that("fr_listing works with fr_spacing", {
  spec <- df_listing |>
    fr_listing() |>
    fr_spacing(titles_after = 2L)
  expect_equal(spec$spacing$titles_after, 2L)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_listing — input validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_listing errors on non-data.frame input", {
  expect_error(fr_listing("not a df"), class = "rlang_error")
  expect_error(fr_listing(list(a = 1)), class = "rlang_error")
  expect_error(fr_listing(1:10), class = "rlang_error")
  expect_error(fr_listing(NULL), class = "rlang_error")
})

test_that("fr_listing error message mentions 'data'", {
  expect_error(fr_listing("bad"), "data")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_listing — works with built-in datasets
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_listing works with adae dataset", {
  spec <- adae |> fr_listing()
  expect_s3_class(spec, "fr_spec")
  expect_equal(spec$type, "listing")
  expect_equal(nrow(spec$data), nrow(adae))
})

test_that("fr_listing works with adcm dataset", {
  spec <- adcm |> fr_listing()
  expect_s3_class(spec, "fr_spec")
  expect_equal(spec$type, "listing")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_listing — full pipeline integration
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_listing full pipeline assembles correct spec", {
  spec <- df_listing |>
    fr_listing() |>
    fr_titles("Listing 16.2.7 Adverse Events", "Safety Analysis Set") |>
    fr_footnotes("Source: ADAE") |>
    fr_cols(
      USUBJID = fr_col("Subject ID", width = 1.3),
      AEDECOD = fr_col("Preferred Term"),
      AESEV = fr_col("Severity", width = 1.0),
      ASTDY = fr_col("Study Day", width = 0.8, align = "right")
    ) |>
    fr_rows(
      sort_by = c("USUBJID", "ASTDY"),
      repeat_cols = "USUBJID"
    ) |>
    fr_header(bold = TRUE) |>
    fr_page(font_size = 7) |>
    fr_pagehead(left = "Study ABC-001", right = "Page {thepage}") |>
    fr_hlines("booktabs") |>
    fr_styles(fr_row_style(rows = 1L, bold = TRUE))

  expect_s3_class(spec, "fr_spec")
  expect_equal(spec$type, "listing")
  expect_length(spec$meta$titles, 2L)
  expect_length(spec$meta$footnotes, 1L)
  expect_equal(spec$body$sort_by, c("USUBJID", "ASTDY"))
  expect_equal(spec$body$repeat_cols, "USUBJID")
  expect_true(spec$header$bold)
  expect_equal(spec$page$font_size, 7) # overridden from listing default of 8
  expect_s3_class(spec$pagehead, "fr_pagechrome")
  expect_length(spec$cell_styles, 1L)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_listing vs fr_table — differences
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_listing and fr_table produce different types", {
  spec_tbl <- fr_table(df_listing)
  spec_lst <- fr_listing(df_listing)
  expect_equal(spec_tbl$type, "table")
  expect_equal(spec_lst$type, "listing")
})

test_that("fr_listing has smaller default font than fr_table", {
  spec_tbl <- fr_table(df_listing)
  spec_lst <- fr_listing(df_listing)
  expect_equal(spec_tbl$page$font_size, 9)
  expect_equal(spec_lst$page$font_size, 8)
})

test_that("fr_listing enables split unlike fr_table", {
  spec_tbl <- fr_table(df_listing)
  spec_lst <- fr_listing(df_listing)
  expect_identical(spec_tbl$columns_meta$split, FALSE)
  expect_true(isTRUE(spec_lst$columns_meta$split))
})

test_that("fr_listing forces left align on numeric columns", {
  spec_tbl <- fr_table(df_listing) |> fr_cols()
  spec_lst <- fr_listing(df_listing)
  # fr_table auto-detects right for numeric; fr_listing forces left
  expect_equal(spec_tbl$columns[["ASTDY"]]$align, "right")
  expect_equal(spec_lst$columns[["ASTDY"]]$align, "left")
})

test_that("fr_listing has header hline by default, fr_table does not", {
  spec_tbl <- fr_table(df_listing)
  spec_lst <- fr_listing(df_listing)
  expect_length(spec_tbl$rules, 0L)
  hlines_lst <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec_lst$rules
  )
  expect_length(hlines_lst, 1L)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_figure — input validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_figure errors on string input", {
  expect_error(fr_figure("not a plot"), class = "rlang_error")
})

test_that("fr_figure errors on numeric input", {
  expect_error(fr_figure(42), class = "rlang_error")
})

test_that("fr_figure errors on data.frame input", {
  expect_error(fr_figure(data.frame(x = 1)), class = "rlang_error")
})

test_that("fr_figure errors on list input", {
  expect_error(fr_figure(list(x = 1, y = 2)), class = "rlang_error")
})

test_that("fr_figure errors on NULL input", {
  expect_error(fr_figure(NULL), class = "rlang_error")
})

test_that("fr_figure error message mentions ggplot or recordedplot", {
  expect_error(fr_figure("bad"), "ggplot|recordedplot")
})

test_that("fr_figure validates width must be positive", {
  # Create a fake recordedplot to pass plot validation
  fake_plot <- structure(list(), class = "recordedplot")
  expect_error(fr_figure(fake_plot, width = -1), class = "rlang_error")
  expect_error(fr_figure(fake_plot, width = 0), class = "rlang_error")
})

test_that("fr_figure validates height must be positive", {
  fake_plot <- structure(list(), class = "recordedplot")
  expect_error(fr_figure(fake_plot, height = -1), class = "rlang_error")
  expect_error(fr_figure(fake_plot, height = 0), class = "rlang_error")
})

test_that("fr_figure validates width is numeric", {
  fake_plot <- structure(list(), class = "recordedplot")
  expect_error(fr_figure(fake_plot, width = "wide"), class = "rlang_error")
})

test_that("fr_figure validates height is numeric", {
  fake_plot <- structure(list(), class = "recordedplot")
  expect_error(fr_figure(fake_plot, height = "tall"), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_figure — construction with recordedplot
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_figure creates fr_spec from recordedplot", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fr_figure(fake_plot)
  expect_s3_class(spec, "fr_spec")
  expect_equal(spec$type, "figure")
})

test_that("fr_figure stores the plot object", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fr_figure(fake_plot)
  expect_identical(spec$plot, fake_plot)
})

test_that("fr_figure has empty data frame", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fr_figure(fake_plot)
  expect_true(is.data.frame(spec$data))
  expect_equal(nrow(spec$data), 0L)
})

test_that("fr_figure stores width and height as NULL by default", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fr_figure(fake_plot)
  expect_null(spec$figure_width)
  expect_null(spec$figure_height)
})

test_that("fr_figure stores explicit width and height", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fr_figure(fake_plot, width = 7, height = 4.5)
  expect_equal(spec$figure_width, 7)
  expect_equal(spec$figure_height, 4.5)
})

test_that("fr_figure stores width only when height is NULL", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fr_figure(fake_plot, width = 6)
  expect_equal(spec$figure_width, 6)
  expect_null(spec$figure_height)
})

test_that("fr_figure stores height only when width is NULL", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fr_figure(fake_plot, height = 5)
  expect_null(spec$figure_width)
  expect_equal(spec$figure_height, 5)
})

test_that("fr_figure has default sub-structures", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fr_figure(fake_plot)
  expect_s3_class(spec$meta, "fr_meta")
  expect_s3_class(spec$page, "fr_page")
  expect_s3_class(spec$body, "fr_body")
  expect_s3_class(spec$header, "fr_header")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_figure — construction with ggplot
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_figure accepts ggplot object", {
  fake_gg <- structure(list(), class = "ggplot")
  spec <- fr_figure(fake_gg)
  expect_s3_class(spec, "fr_spec")
  expect_equal(spec$type, "figure")
  expect_identical(spec$plot, fake_gg)
})

test_that("fr_figure accepts ggplot with width and height", {
  fake_gg <- structure(list(), class = "ggplot")
  spec <- fr_figure(fake_gg, width = 8, height = 5)
  expect_equal(spec$figure_width, 8)
  expect_equal(spec$figure_height, 5)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_figure — pipeline compatibility
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_figure works with fr_titles", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fake_plot |>
    fr_figure() |>
    fr_titles("Figure 14.2.1 Time-to-Event")
  expect_length(spec$meta$titles, 1L)
  expect_equal(spec$meta$titles[[1]]$content, "Figure 14.2.1 Time-to-Event")
})

test_that("fr_figure works with fr_footnotes", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fake_plot |>
    fr_figure() |>
    fr_footnotes("Source: ADTTE")
  expect_length(spec$meta$footnotes, 1L)
})

test_that("fr_figure works with fr_page", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fake_plot |>
    fr_figure() |>
    fr_page(orientation = "portrait")
  expect_equal(spec$page$orientation, "portrait")
})

test_that("fr_figure works with fr_pagehead and fr_pagefoot", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fake_plot |>
    fr_figure() |>
    fr_pagehead(left = "Study ABC") |>
    fr_pagefoot(right = "{datetime}")
  expect_s3_class(spec$pagehead, "fr_pagechrome")
  expect_s3_class(spec$pagefoot, "fr_pagechrome")
})

test_that("fr_figure works with fr_spacing", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fake_plot |>
    fr_figure() |>
    fr_spacing(titles_after = 2L)
  expect_equal(spec$spacing$titles_after, 2L)
})

test_that("fr_figure full pipeline assembles correct spec", {
  fake_plot <- structure(list(), class = "recordedplot")
  spec <- fake_plot |>
    fr_figure(width = 7, height = 4.5) |>
    fr_titles("Figure 14.2.1 Time-to-Event", "ITT Population") |>
    fr_footnotes("Source: ADTTE", "Kaplan-Meier estimate") |>
    fr_page(orientation = "landscape") |>
    fr_pagehead(left = "Study ABC-001") |>
    fr_pagefoot(right = "{datetime}")

  expect_s3_class(spec, "fr_spec")
  expect_equal(spec$type, "figure")
  expect_equal(spec$figure_width, 7)
  expect_equal(spec$figure_height, 4.5)
  expect_length(spec$meta$titles, 2L)
  expect_length(spec$meta$footnotes, 2L)
  expect_equal(spec$page$orientation, "landscape")
  expect_s3_class(spec$pagehead, "fr_pagechrome")
  expect_s3_class(spec$pagefoot, "fr_pagechrome")
  expect_identical(spec$plot, fake_plot)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_listing — end-to-end render
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_listing renders to RTF with expected content", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  df_listing |>
    fr_listing() |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # RTF preamble
  expect_true(grepl("\\rtf1", txt, fixed = TRUE))
  # Column headers from data
  expect_true(grepl("USUBJID", txt, fixed = TRUE))
  expect_true(grepl("AEDECOD", txt, fixed = TRUE))
  # Data content
  expect_true(grepl("Headache", txt, fixed = TRUE))
  expect_true(grepl("SUBJ-001", txt, fixed = TRUE))
})

test_that("fr_listing RTF output uses 8pt font (\\fs16) by default", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  df_listing |>
    fr_listing() |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # font_size=8 means \fs16 in RTF (half-points)
  # Body cells should contain \fs16
  expect_true(grepl("\\fs16 ", txt, fixed = TRUE))
  # Body cells use \pard\plain\intbl with \fs
  expect_true(grepl("\\pard\\plain\\intbl", txt, fixed = TRUE))
})

test_that("fr_listing RTF output has left alignment for all columns", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # Include a numeric column (ASTDY) that would normally be right-aligned
  # in fr_table but should be left-aligned in fr_listing
  df_listing |>
    fr_listing() |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  lines <- strsplit(txt, "\n")[[1]]

  # Body cells should use \ql (left alignment)
  body_cells <- grep("\\pard\\plain\\intbl", lines, fixed = TRUE, value = TRUE)
  expect_true(length(body_cells) > 0L)

  # All body cells should be left-aligned (\ql), none right-aligned (\qr)
  expect_true(all(grepl("\\ql", body_cells, fixed = TRUE)))
  expect_false(any(grepl("\\qr", body_cells, fixed = TRUE)))
})

test_that("fr_listing RTF output contains all data values", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  df_listing |>
    fr_listing() |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # All data values should be present
  expect_true(grepl("SUBJ-001", txt, fixed = TRUE))
  expect_true(grepl("SUBJ-002", txt, fixed = TRUE))
  expect_true(grepl("Headache", txt, fixed = TRUE))
  expect_true(grepl("Nausea", txt, fixed = TRUE))
  expect_true(grepl("Dizziness", txt, fixed = TRUE))
  expect_true(grepl("MILD", txt, fixed = TRUE))
  expect_true(grepl("MODERATE", txt, fixed = TRUE))
  # All column headers
  expect_true(grepl("USUBJID", txt, fixed = TRUE))
  expect_true(grepl("AEDECOD", txt, fixed = TRUE))
  expect_true(grepl("AESEV", txt, fixed = TRUE))
  expect_true(grepl("ASTDY", txt, fixed = TRUE))
})

test_that("fr_listing RTF output with titles and footnotes", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  df_listing |>
    fr_listing() |>
    fr_titles("Listing 16.2.7 Adverse Events") |>
    fr_footnotes("Source: ADAE") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  expect_true(grepl("Listing 16.2.7 Adverse Events", txt, fixed = TRUE))
  expect_true(grepl("Source: ADAE", txt, fixed = TRUE))
})

test_that("fr_listing RTF font size changes when overridden via fr_page", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  df_listing |>
    fr_listing() |>
    fr_page(font_size = 7) |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # font_size=7 means \fs14 in RTF (half-points)
  expect_true(grepl("\\fs14 ", txt, fixed = TRUE))
})

test_that("fr_listing RTF alignment differs from fr_table for numeric cols", {
  tmp_listing <- tempfile(fileext = ".rtf")
  tmp_table <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(tmp_listing, tmp_table)), add = TRUE)

  df_listing |> fr_listing() |> fr_render(tmp_listing)
  df_listing |> fr_table() |> fr_render(tmp_table)

  txt_listing <- rawToChar(readBin(
    tmp_listing,
    "raw",
    file.info(tmp_listing)$size
  ))
  txt_table <- rawToChar(readBin(
    tmp_table,
    "raw",
    file.info(tmp_table)$size
  ))

  listing_lines <- strsplit(txt_listing, "\n")[[1]]
  table_lines <- strsplit(txt_table, "\n")[[1]]

  # Listing: no right-aligned body cells
  listing_body <- grep(
    "\\pard\\plain\\intbl",
    listing_lines,
    fixed = TRUE,
    value = TRUE
  )
  expect_false(any(grepl("\\qr", listing_body, fixed = TRUE)))

  # Table: ASTDY (numeric) should be right-aligned
  table_body <- grep(
    "\\pard\\plain\\intbl",
    table_lines,
    fixed = TRUE,
    value = TRUE
  )
  expect_true(any(grepl("\\qr", table_body, fixed = TRUE)))
})

test_that("fr_listing full pipeline renders to RTF", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  df_listing |>
    fr_listing() |>
    fr_titles("Listing 16.2.7 Adverse Events", "Safety Analysis Set") |>
    fr_footnotes("Source: ADAE") |>
    fr_cols(
      USUBJID = fr_col("Subject ID"),
      AEDECOD = fr_col("Preferred Term"),
      AESEV = fr_col("Severity"),
      ASTDY = fr_col("Study Day")
    ) |>
    fr_header(bold = TRUE) |>
    fr_hlines("booktabs") |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))

  # Custom column labels rendered
  expect_true(grepl("Subject ID", txt, fixed = TRUE))
  expect_true(grepl("Preferred Term", txt, fixed = TRUE))
  expect_true(grepl("Severity", txt, fixed = TRUE))
  expect_true(grepl("Study Day", txt, fixed = TRUE))
  # Titles and footnotes
  expect_true(grepl("Listing 16.2.7 Adverse Events", txt, fixed = TRUE))
  expect_true(grepl("Safety Analysis Set", txt, fixed = TRUE))
  expect_true(grepl("Source: ADAE", txt, fixed = TRUE))
  # Data values still present
  expect_true(grepl("Headache", txt, fixed = TRUE))
  expect_true(grepl("SUBJ-001", txt, fixed = TRUE))
  # Bold header: RTF bold is \b
  expect_true(grepl("\\b ", txt, fixed = TRUE))
  # 8pt font in body cells
  expect_true(grepl("\\fs16 ", txt, fixed = TRUE))
})
