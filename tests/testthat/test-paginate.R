# ══════════════════════════════════════════════════════════════════════════════
# test-paginate.R — R-side pagination tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("compute_column_char_widths returns positive integers", {
  cols <- list(a = fr_col("A", width = 2.0))
  cols$a$id <- "a"
  page <- new_fr_page()
  result <- compute_column_char_widths(cols, page)
  expect_true(all(result > 0L))
})

test_that("measure_cell_height returns 1 for single-line text", {
  expect_equal(measure_cell_height("hello world", 80L), 1L)
})

test_that("measure_cell_height counts newlines", {
  expect_equal(measure_cell_height("line1\nline2\nline3", 80L), 3L)
})

test_that("measure_cell_height wraps long text", {
  long <- paste(rep("word", 20), collapse = " ")
  h <- measure_cell_height(long, 20L)
  expect_gt(h, 1L)
})

test_that("measure_cell_height returns 1 for empty text", {
  expect_equal(measure_cell_height("", 80L), 1L)
})

test_that("calculate_row_heights returns uniform twips for short content", {
  df <- data.frame(a = c("x", "y", "z"), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 2.0))
  cols$a$id <- "a"
  page <- new_fr_page()
  heights <- calculate_row_heights(df, cols, page)
  one_row <- row_height_twips(page$font_size)
  expect_equal(heights, rep(one_row, 3L))
})

test_that("calculate_row_heights detects multi-line content", {
  df <- data.frame(a = c("x", "line1\nline2", "z"), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 2.0))
  cols$a$id <- "a"
  page <- new_fr_page()
  heights <- calculate_row_heights(df, cols, page)
  one_row <- row_height_twips(page$font_size)
  expect_equal(heights[1], one_row)
  expect_gt(heights[2], one_row)
  expect_equal(heights[3], one_row)
})

test_that("calculate_page_budget returns twips value", {
  spec <- data.frame(a = "x", stringsAsFactors = FALSE) |> fr_table()
  spec <- finalize_spec(spec)
  budget <- calculate_page_budget(spec)
  one_row <- row_height_twips(spec$page$font_size)
  # Budget should be at least 5 rows worth of twips
  expect_gte(budget, 5L * one_row)
  # Budget should be reasonable (< full page height)
  page_h <- paper_dims_twips(spec$page$paper, spec$page$orientation)[["height"]]
  expect_lt(budget, page_h)
})

test_that("calculate_page_budget does not subtract pagehead/pagefoot/every footnotes", {
  # Budget should be the same with or without pagehead/pagefoot/every footnotes
  # because those live in RTF margin areas, not the body area.
  spec_base <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1")
  spec_base <- finalize_spec(spec_base)
  budget_base <- calculate_page_budget(spec_base)

  spec_chrome <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1") |>
    fr_pagehead(left = "Page {thepage}") |>
    fr_pagefoot(left = "Footer") |>
    fr_footnotes("Note 1", .placement = "every")
  spec_chrome <- finalize_spec(spec_chrome)
  budget_chrome <- calculate_page_budget(spec_chrome)

  # Budget should be identical — margin chrome does not reduce body area
  expect_equal(budget_chrome, budget_base)
})

test_that("calculate_page_budget reserves space for last footnotes", {
  spec_no_fn <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1")
  spec_no_fn <- finalize_spec(spec_no_fn)

  spec_last_fn <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1") |>
    fr_footnotes("Note 1", "Note 2", .placement = "last")
  spec_last_fn <- finalize_spec(spec_last_fn)

  budget_no_fn <- calculate_page_budget(spec_no_fn)
  budget_last_fn <- calculate_page_budget(spec_last_fn)

  one_row <- row_height_twips(spec_no_fn$page$font_size)
  # 2 last footnotes + 1 footnotes_before spacing = 3 rows worth of twips
  expect_equal(budget_no_fn - budget_last_fn, 3L * one_row)
})

test_that("calculate_page_budget accounts for large footer overflow", {
  # With tiny margins (0.25in = 360 twips) and many "every" footnotes,

  # the footer content overflows the margin and eats into body area.
  # At 9pt, one_row = 236 twips. 8 every footnotes + 1 spacing = 9 lines
  # = 9 * 236 = 2124 twips, but margin is only 360 twips.
  # Overflow = 2124 - 360 = 1764 twips = ~7 rows lost.
  spec_small_margin <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1") |>
    fr_page(margins = 0.25) |>
    fr_footnotes(
      "fn1",
      "fn2",
      "fn3",
      "fn4",
      "fn5",
      "fn6",
      "fn7",
      "fn8",
      .placement = "every"
    )
  spec_small_margin <- finalize_spec(spec_small_margin)

  spec_no_fn <- data.frame(a = "x", stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_titles("Title 1") |>
    fr_page(margins = 0.25)
  spec_no_fn <- finalize_spec(spec_no_fn)

  budget_no_fn <- calculate_page_budget(spec_no_fn)
  budget_overflow <- calculate_page_budget(spec_small_margin)

  # The large footer should reduce the budget
  expect_lt(budget_overflow, budget_no_fn)
  # With ~7 rows of overflow, budget should drop significantly
  expect_gte(budget_no_fn - budget_overflow, 5L)
})

test_that("paginate_rows assigns all to page 1 when content fits", {
  rh <- row_height_twips(9)
  heights <- rep(rh, 10L)
  df <- data.frame(
    grp = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"),
    val = as.character(1:10),
    stringsAsFactors = FALSE
  )
  pages <- paginate_rows(heights, 50L * rh, df, "grp")
  expect_true(all(pages == 1L))
})

test_that("paginate_rows splits when budget exceeded", {
  rh <- row_height_twips(9)
  heights <- rep(rh, 20L)
  df <- data.frame(
    grp = c(rep("A", 10), rep("B", 10)),
    val = as.character(1:20),
    stringsAsFactors = FALSE
  )
  pages <- paginate_rows(heights, 12L * rh, df, "grp")
  expect_true(max(pages) >= 2L)
})

test_that("paginate_rows keeps small groups together", {
  rh <- row_height_twips(9)
  heights <- rep(rh, 10L)
  df <- data.frame(
    grp = c(rep("A", 5), rep("B", 5)),
    val = as.character(1:10),
    stringsAsFactors = FALSE
  )
  # Budget of 6 rows: group A (5 rows) fits, then group B goes to next page
  pages <- paginate_rows(heights, 6L * rh, df, "grp")
  # Group A should be on page 1
  expect_true(all(pages[1:5] == 1L))
  # Group B should be on page 2
  expect_true(all(pages[6:10] == 2L))
})

test_that("paginate_rows handles empty input", {
  pages <- paginate_rows(integer(0), 50L, data.frame(), "grp")
  expect_length(pages, 0L)
})

test_that("deferred blank saves page break", {
  rh <- row_height_twips(9)
  grp <- c(rep("A", 6), rep("B", 5), rep("C", 7), rep("D", 6))
  df <- data.frame(
    grp = c(grp[1:6], "", grp[7:11], "", grp[12:18], "", grp[19:24]),
    val = c(
      paste0("a", 1:6),
      "",
      paste0("b", 1:5),
      "",
      paste0("c", 1:7),
      "",
      paste0("d", 1:6)
    ),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  pages <- paginate_rows(heights, 26L * rh, df, "grp")
  expect_equal(max(pages), 1L)
})

test_that("genuine page break preserved with deferred blanks", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = c(rep("A", 15), "", rep("B", 15)),
    val = c(paste0("a", 1:15), "", paste0("b", 1:15)),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  pages <- paginate_rows(heights, 20L * rh, df, "grp")
  expect_equal(max(pages), 2L)
  expect_true(all(pages[1:15] == 1L))
  expect_true(all(pages[17:31] == 2L))
})

test_that("last group without trailing blank paginates correctly", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = c(rep("A", 5), "", rep("B", 5)),
    val = c(paste0("a", 1:5), "", paste0("b", 1:5)),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  pages <- paginate_rows(heights, 12L * rh, df, "grp")
  expect_equal(max(pages), 1L)
})

test_that("trailing blank at page boundary stays with its group", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = c(rep("A", 8), "", rep("B", 8)),
    val = c(paste0("a", 1:8), "", paste0("b", 1:8)),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  pages <- paginate_rows(heights, 10L * rh, df, "grp")
  expect_true(all(pages[1:8] == 1L))
  expect_equal(pages[9], 1L)
  expect_true(all(pages[10:17] == 2L))
})

test_that("final blank after last group gets page 0 (excluded)", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = c(rep("A", 5), "", rep("B", 5), ""),
    val = c(paste0("a", 1:5), "", paste0("b", 1:5), ""),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  pages <- paginate_rows(heights, 20L * rh, df, "grp")
  expect_true(all(pages[1:5] == 1L))
  expect_true(all(pages[7:11] == 1L))
  expect_equal(pages[6], 1L)
  expect_equal(pages[12], 0L)
})

test_that("large group split applies widow protection", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = rep("A", 22),
    val = as.character(1:22),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 22L)
  pages <- paginate_rows(
    heights,
    20L * rh,
    df,
    "grp",
    orphan_min = 3L,
    widow_min = 3L
  )
  expect_equal(max(pages), 2L)
  expect_gte(sum(pages == 2L), 3L)
})

test_that("large group split respects orphan_min floor when stealing", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = rep("A", 7),
    val = as.character(1:7),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 7L)
  pages <- paginate_rows(
    heights,
    5L * rh,
    df,
    "grp",
    orphan_min = 3L,
    widow_min = 3L
  )
  expect_equal(max(pages), 2L)
  expect_gte(sum(pages == 1L), 3L)
  expect_gte(sum(pages == 2L), 3L)
})

test_that("large group split cannot steal below orphan_min", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = rep("A", 5),
    val = as.character(1:5),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, 5L)
  pages <- paginate_rows(
    heights,
    3L * rh,
    df,
    "grp",
    orphan_min = 3L,
    widow_min = 3L
  )
  expect_equal(max(pages), 2L)
  expect_equal(sum(pages == 1L), 3L)
  expect_equal(sum(pages == 2L), 2L)
})

test_that("large group split suppresses blank rows at page boundaries", {
  rh <- row_height_twips(9)
  df <- data.frame(
    grp = c(rep("A", 6), "", rep("A", 6), "", rep("A", 6)),
    val = c(paste0("a", 1:6), "", paste0("b", 1:6), "", paste0("c", 1:6)),
    stringsAsFactors = FALSE
  )
  heights <- rep(rh, nrow(df))
  pages <- paginate_rows(
    heights,
    8L * rh,
    df,
    "grp",
    orphan_min = 2L,
    widow_min = 2L
  )

  data_rows <- which(df$val != "")
  expect_true(all(pages[data_rows] > 0L))
})

test_that("RTF render with group_by produces valid RTF", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # Create a table with group_by
  df <- data.frame(
    soc = c(rep("SOC A", 3), rep("SOC B", 3)),
    pt = paste0("PT-", 1:6),
    n = as.character(c(10, 20, 30, 40, 50, 60)),
    stringsAsFactors = FALSE
  )
  df |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_rows(group_by = "soc") |>
    fr_render(tmp)

  # File should be valid RTF
  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\\\rtf1", txt, fixed = FALSE))
})

test_that("RTF render without group_by uses RTF-native pagination", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  df <- data.frame(
    a = as.character(1:10),
    stringsAsFactors = FALSE
  )
  df |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("\\\\rtf1", txt, fixed = FALSE))
  # Without group_by, no \trpagebb should appear
  expect_false(grepl("trpagebb", txt, fixed = TRUE))
})

test_that("RTF render with group_by uses trpagebb for page breaks", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # Create enough data to require multiple pages (budget ~52 in portrait)
  # Two large groups that together exceed page budget
  n_per_group <- 40
  df <- data.frame(
    soc = c(rep("SOC A", n_per_group), rep("SOC B", n_per_group)),
    pt = paste0("PT-", seq_len(2 * n_per_group)),
    n = as.character(seq_len(2 * n_per_group)),
    stringsAsFactors = FALSE
  )
  df |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_rows(group_by = "soc") |>
    fr_page(orientation = "portrait") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # Should use \trpagebb for page breaks, not \sect for sub-pages
  expect_true(grepl("trpagebb", txt, fixed = TRUE))
  # Table should be continuous (no \sect within the single section)
  sect_count <- lengths(regmatches(txt, gregexpr("\\\\sect", txt)))
  expect_lte(sect_count, 1L)
})

test_that("RTF render with group_by has no \\trkeep (R-side pagination)", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # Enough data to trigger multi-page pagination (budget ~52 in portrait)
  n_per_group <- 40
  df <- data.frame(
    soc = c(rep("SOC A", n_per_group), rep("SOC B", n_per_group)),
    pt = paste0("PT-", seq_len(2 * n_per_group)),
    n = as.character(seq_len(2 * n_per_group)),
    stringsAsFactors = FALSE
  )
  df |>
    fr_table() |>
    fr_cols(.width = "equal") |>
    fr_rows(group_by = "soc") |>
    fr_page(orientation = "portrait") |>
    fr_render(tmp)

  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  # When R-side pagination is active, \trkeep should not appear
  # (page breaks are explicitly controlled by \trpagebb)
  expect_false(grepl("trkeep", txt, fixed = TRUE))
})

test_that("group_by + indent_by uses only group_by for pagination keys", {
  rh <- row_height_twips(9)
  heights <- rep(rh, 12L)
  df <- data.frame(
    soc = c(
      rep("Eye disorders", 4),
      "",
      rep("Cardiac disorders", 4),
      "",
      rep("Skin disorders", 2)
    ),
    pt = c(
      "Eye disorders",
      "Cataract",
      "Glaucoma",
      "Dry eye",
      "",
      "Cardiac disorders",
      "Tachycardia",
      "Bradycardia",
      "Palpitations",
      "",
      "Skin disorders",
      "Rash"
    ),
    n = as.character(1:12),
    stringsAsFactors = FALSE
  )
  # Budget of 6 rows: Eye disorders (4 rows) + blank fits
  pages <- paginate_rows(heights, 6L * rh, df, "soc")
  expect_true(all(pages[1:4] == 1L))
  expect_true(all(pages[6:9] == 2L))
})
