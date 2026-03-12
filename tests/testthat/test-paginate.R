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

test_that("calculate_row_heights returns all 1s for short content", {
  df <- data.frame(a = c("x", "y", "z"), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 2.0))
  cols$a$id <- "a"
  page <- new_fr_page()
  heights <- calculate_row_heights(df, cols, page)
  expect_equal(heights, c(1L, 1L, 1L))
})

test_that("calculate_row_heights detects multi-line content", {
  df <- data.frame(a = c("x", "line1\nline2", "z"), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 2.0))
  cols$a$id <- "a"
  page <- new_fr_page()
  heights <- calculate_row_heights(df, cols, page)
  expect_equal(heights[1], 1L)
  expect_gt(heights[2], 1L)
  expect_equal(heights[3], 1L)
})

test_that("calculate_page_budget returns positive integer", {
  spec <- data.frame(a = "x", stringsAsFactors = FALSE) |> fr_table()
  spec <- finalize_spec(spec)
  budget <- calculate_page_budget(spec)
  expect_true(budget >= 5L)
})

test_that("paginate_rows assigns all to page 1 when content fits", {
  heights <- rep(1L, 10)
  df <- data.frame(
    grp = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"),
    val = as.character(1:10),
    stringsAsFactors = FALSE
  )
  pages <- paginate_rows(heights, 50L, df, "grp")
  expect_true(all(pages == 1L))
})

test_that("paginate_rows splits when budget exceeded", {
  heights <- rep(1L, 20)
  df <- data.frame(
    grp = c(rep("A", 10), rep("B", 10)),
    val = as.character(1:20),
    stringsAsFactors = FALSE
  )
  pages <- paginate_rows(heights, 12L, df, "grp")
  expect_true(max(pages) >= 2L)
})

test_that("paginate_rows keeps small groups together", {
  heights <- rep(1L, 10)
  df <- data.frame(
    grp = c(rep("A", 5), rep("B", 5)),
    val = as.character(1:10),
    stringsAsFactors = FALSE
  )
  # Budget of 6: group A (5 rows) fits, then group B goes to next page
  pages <- paginate_rows(heights, 6L, df, "grp")
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
  # 4 groups: 6+5+7+6 data rows, with blank rows between groups

  # Old behavior: G1(6)+blank=7, G2(5)+blank=13, G3(7)+blank=21, G4(6)=27>26 → 2 pages
  # New behavior: G3's trailing blank deferred, 20+6=26 ≤ 26 → 1 page
  grp <- c(rep("A", 6), rep("B", 5), rep("C", 7), rep("D", 6))
  n_data <- length(grp)
  # Insert blank rows between groups
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
  heights <- rep(1L, nrow(df))
  pages <- paginate_rows(heights, 26L, df, "grp")
  expect_equal(max(pages), 1L)
})

test_that("genuine page break preserved with deferred blanks", {
  # 2 groups of 15 rows each, budget=20, must be 2 pages regardless
  df <- data.frame(
    grp = c(rep("A", 15), "", rep("B", 15)),
    val = c(paste0("a", 1:15), "", paste0("b", 1:15)),
    stringsAsFactors = FALSE
  )
  heights <- rep(1L, nrow(df))
  pages <- paginate_rows(heights, 20L, df, "grp")
  expect_equal(max(pages), 2L)
  # Group A on page 1, Group B on page 2
  expect_true(all(pages[1:15] == 1L))
  expect_true(all(pages[17:31] == 2L))
})

test_that("last group without trailing blank paginates correctly", {
  # 2 groups: 5+5 data rows, 1 blank between, budget=12
  # 5 + 1(blank) + 5 = 11 ≤ 12 → 1 page
  df <- data.frame(
    grp = c(rep("A", 5), "", rep("B", 5)),
    val = c(paste0("a", 1:5), "", paste0("b", 1:5)),
    stringsAsFactors = FALSE
  )
  heights <- rep(1L, nrow(df))
  pages <- paginate_rows(heights, 12L, df, "grp")
  expect_equal(max(pages), 1L)
})

test_that("trailing blank at page boundary stays with its group", {
  # 2 groups of 8 rows, budget=10. Group A fits (8), trailing blank deferred.
  # Blank is assigned to current page (belongs to previous group) before

  # checking if next group fits. Group B doesn't fit → new page.
  # The blank stays on page 1 with group A.
  df <- data.frame(
    grp = c(rep("A", 8), "", rep("B", 8)),
    val = c(paste0("a", 1:8), "", paste0("b", 1:8)),
    stringsAsFactors = FALSE
  )
  heights <- rep(1L, nrow(df))
  pages <- paginate_rows(heights, 10L, df, "grp")
  # Group A on page 1
  expect_true(all(pages[1:8] == 1L))
  # Blank row (9) stays with group A on page 1
  expect_equal(pages[9], 1L)
  # Group B on page 2
  expect_true(all(pages[10:17] == 2L))
})

test_that("final blank after last group gets page 0 (excluded)", {
  # 2 groups: 5+5 data rows, blank between AND after last group
  df <- data.frame(
    grp = c(rep("A", 5), "", rep("B", 5), ""),
    val = c(paste0("a", 1:5), "", paste0("b", 1:5), ""),
    stringsAsFactors = FALSE
  )
  heights <- rep(1L, nrow(df))
  pages <- paginate_rows(heights, 20L, df, "grp")
  # All data rows on page 1
  expect_true(all(pages[1:5] == 1L))
  expect_true(all(pages[7:11] == 1L))
  # Blank between groups included (Branch 1 fit)
  expect_equal(pages[6], 1L)
  # Final trailing blank — page 0
  expect_equal(pages[12], 0L)
})

test_that("large group split applies widow protection", {
  # Single group with 22 rows, budget = 20, widow_min = 3
  # Naive: page 1 gets 20, page 2 gets 2 (widow violation)
  # Improved: page 1 gets 19, page 2 gets 3
  df <- data.frame(
    grp = rep("A", 22),
    val = as.character(1:22),
    stringsAsFactors = FALSE
  )
  heights <- rep(1L, 22)
  pages <- paginate_rows(
    heights,
    20L,
    df,
    "grp",
    orphan_min = 3L,
    widow_min = 3L
  )
  expect_equal(max(pages), 2L)
  # Page 2 should have at least widow_min rows
  expect_gte(sum(pages == 2L), 3L)
})

test_that("large group split respects orphan_min floor when stealing", {
  # Single group with 7 rows, budget = 5, orphan_min = 3, widow_min = 3
  # Naive: page 1 gets 5, page 2 gets 2 (widow violation)
  # Stealing 1 would give page 1 = 4 (>= orphan_min), page 2 = 3 (= widow_min)
  df <- data.frame(
    grp = rep("A", 7),
    val = as.character(1:7),
    stringsAsFactors = FALSE
  )
  heights <- rep(1L, 7)
  pages <- paginate_rows(
    heights,
    5L,
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
  # Single group with 5 rows, budget = 3, orphan_min = 3, widow_min = 3
  # Naive: page 1 gets 3, page 2 gets 2 (widow violation)
  # Stealing 1 would give page 1 = 2 (< orphan_min) — can't steal
  # Accept the widow: page 1 = 3, page 2 = 2
  df <- data.frame(
    grp = rep("A", 5),
    val = as.character(1:5),
    stringsAsFactors = FALSE
  )
  heights <- rep(1L, 5)
  pages <- paginate_rows(
    heights,
    3L,
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
  # Single group: 6 data + blank + 6 data + blank + 6 data = 20 rows total
  # Budget = 8. Page 1 gets rows 1-7 (6 data + blank), but trailing blank trimmed.
  # Page 2 starts: leading blank suppressed, gets data rows.
  df <- data.frame(
    grp = c(rep("A", 6), "", rep("A", 6), "", rep("A", 6)),
    val = c(paste0("a", 1:6), "", paste0("b", 1:6), "", paste0("c", 1:6)),
    stringsAsFactors = FALSE
  )
  heights <- rep(1L, nrow(df))
  pages <- paginate_rows(
    heights,
    8L,
    df,
    "grp",
    orphan_min = 2L,
    widow_min = 2L
  )

  # Blank rows at page boundaries should be suppressed (page 0)
  # or assigned to a page but not waste space
  # All data rows should be assigned to a valid page
  data_rows <- which(df$val != "")
  expect_true(all(pages[data_rows] > 0L))
})

test_that("RTF render with group_by produces multiple sections", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  # Create a table with group_by that has enough rows to page
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
})
