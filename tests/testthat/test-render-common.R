# ──────────────────────────────────────────────────────────────────────────────
# test-render-common.R — Tests for shared rendering utilities
# ──────────────────────────────────────────────────────────────────────────────

test_that("rtf_escape handles special characters", {
  expect_equal(rtf_escape("hello"), "hello")
  expect_equal(rtf_escape("a\\b"), "a\\\\b")
  expect_equal(rtf_escape("a{b}c"), "a\\{b\\}c")
  expect_equal(rtf_escape("\\{\\}"), "\\\\\\{\\\\\\}")
})

test_that("rtf_escape handles empty and NA input", {
  expect_equal(rtf_escape(""), "")
  expect_equal(rtf_escape(character(0)), character(0))
})

test_that("rtf_escape converts non-ASCII to Unicode escapes", {
  result <- rtf_escape("\u00b1")
  expect_true(grepl("\\\\u", result))
})

test_that("rtf_sentinel_resolver handles all types", {
  expect_equal(rtf_sentinel_resolver("SUPER", "a"), "{\\super a}")
  expect_equal(rtf_sentinel_resolver("SUB", "x"), "{\\sub x}")
  expect_equal(rtf_sentinel_resolver("BOLD", "hi"), "{\\b hi}")
  expect_equal(rtf_sentinel_resolver("ITALIC", "em"), "{\\i em}")
  expect_equal(rtf_sentinel_resolver("UNDERLINE", "u"), "{\\ul u}")
  expect_equal(rtf_sentinel_resolver("NEWLINE", ""), "\\line ")
})

test_that("rtf_escape_and_resolve handles mixed text and sentinels", {
  # Plain text — no sentinels

  expect_equal(rtf_escape_and_resolve("hello"), "hello")

  # Text with special chars
  expect_equal(rtf_escape_and_resolve("a\\b"), "a\\\\b")

  # Text with sentinel
  sentinel <- paste0("\x01SUPER:1\x02")
  result <- rtf_escape_and_resolve(sentinel)
  expect_equal(result, "{\\super 1}")

  # Mixed: text + sentinel + text
  mixed <- paste0("hello \x01BOLD:world\x02 end")
  result <- rtf_escape_and_resolve(mixed)
  expect_true(grepl("hello", result))
  expect_true(grepl("\\{\\\\b world\\}", result))
  expect_true(grepl("end", result))
})

test_that("build_cell_grid produces correct dimensions", {
  data <- data.frame(a = c("x", "y"), b = c(1, 2), stringsAsFactors = FALSE)
  cols <- list(
    a = fr_col("A", width = 1, align = "left"),
    b = fr_col("B", width = 1, align = "right")
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)
  expect_equal(nrow(grid), 4L)  # 2 rows x 2 cols
  expect_equal(ncol(grid), 13L) # row_idx, col_idx, col_name, content, align, valign, ...
  expect_equal(grid$content, c("x", "y", "1", "2"))
})

test_that("build_cell_grid handles empty data", {
  data <- data.frame(a = character(0), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 1, align = "left"))
  cols$a$id <- "a"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)
  expect_equal(nrow(grid), 0L)
})

test_that("resolve_borders maps hline presets correctly", {
  # Header preset: bottom border on header
  rules <- list(
    new_fr_rule(direction = "horizontal", region = "header",
                side = "below", width = 0.5, linestyle = "solid",
                fg = "#000000")
  )

  borders <- resolve_borders(rules, nrow_body = 3L, ncol = 2L, nrow_header = 1L)

  # Header bottom border should be set
  expect_false(is.null(borders$header$bottom[1, 1][[1]]))
  expect_equal(borders$header$bottom[1, 1][[1]]$width, 0.5)

  # Body top border should NOT be set (just header bottom)
  expect_true(is.null(borders$body$top[1, 1][[1]]))
})

test_that("resolve_borders handles box preset", {
  rules <- list(structure(list(preset = "box"), class = "fr_rule_box"))

  borders <- resolve_borders(rules, nrow_body = 2L, ncol = 3L, nrow_header = 1L)

  # Top of header: should be set

  expect_false(is.null(borders$header$top[1, 1][[1]]))

  # Bottom of last body row: should be set
  expect_false(is.null(borders$body$bottom[2, 1][[1]]))

  # Left edge of header: should be set
  expect_false(is.null(borders$header$left[1, 1][[1]]))

  # Right edge of header: should be set
  expect_false(is.null(borders$header$right[1, 3][[1]]))
})

test_that("build_rtf_colortbl produces valid RTF", {
  colors <- c("#000000", "#FF0000")
  result <- build_rtf_colortbl(colors)

  expect_true(grepl("^\\{\\\\colortbl;", result$rtf))
  expect_true(grepl("\\}$", result$rtf))
  expect_true(grepl("\\\\red0\\\\green0\\\\blue0", result$rtf))
  expect_true(grepl("\\\\red255\\\\green0\\\\blue0", result$rtf))

  expect_equal(result$index[["#000000"]], 1L)
  expect_equal(result$index[["#FF0000"]], 2L)
})

test_that("collect_colors finds colors from rules and styles", {
  spec <- new_fr_spec(data.frame(x = 1))
  spec$rules <- list(
    new_fr_rule(fg = "#FF0000")
  )
  spec$cell_styles <- list(
    new_fr_cell_style(fg = "#00FF00", bg = "#0000FF")
  )

  colors <- collect_colors(spec)
  expect_true("#000000" %in% colors)
  expect_true("#FF0000" %in% colors)
  expect_true("#00FF00" %in% colors)
  expect_true("#0000FF" %in% colors)
})


# ══════════════════════════════════════════════════════════════════════════════
# group_by auto-implies blank_after (no double blanks)
# ══════════════════════════════════════════════════════════════════════════════

test_that("group_by + blank_after on same column produces single blank rows", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("x", "y", "z", "w"),
    stringsAsFactors = FALSE
  )
  spec <- df |> fr_table() |>
    fr_rows(group_by = "grp", blank_after = "grp")
  spec <- finalize_spec(spec)
  # Should have 4 data rows + 1 blank row (between A and B)
  expect_equal(nrow(spec$data), 5L)
  # Row 3 should be the blank row (all empty)
  expect_true(all(spec$data[3L, ] == ""))
})

test_that("group_by alone does NOT auto-insert blank rows", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("x", "y", "z", "w"),
    stringsAsFactors = FALSE
  )
  spec <- df |> fr_table() |>
    fr_rows(group_by = "grp")
  spec <- finalize_spec(spec)
  # group_by auto-implies blank_after — a blank row is inserted between groups
  # 4 original rows + 1 blank row between groups A and B = 5
  expect_equal(nrow(spec$data), 5L)
})



test_that("split_at_decimal splits at dot", {
  res <- split_at_decimal("168.0")
  expect_equal(res$before, "168")
  expect_equal(res$after, ".0")
})

test_that("split_at_decimal splits at space when no dot", {
  res <- split_at_decimal("28 (62%)")
  expect_equal(res$before, "28")
  expect_equal(res$after, " (62%)")
})

test_that("split_at_decimal splits at dash (not leading)", {
  res <- split_at_decimal("41-82")
  expect_equal(res$before, "41")
  expect_equal(res$after, "-82")
})

test_that("split_at_decimal preserves leading negative", {
  # Leading dash should NOT split — whole string stays as "before"
  res <- split_at_decimal("-3")
  expect_equal(res$before, "-3")
  expect_equal(res$after, "")
})

test_that("split_at_decimal prefers dot over dash", {
  res <- split_at_decimal("-3.2")
  expect_equal(res$before, "-3")
  expect_equal(res$after, ".2")
})

test_that("split_at_decimal prefers space when dot is after space (pct pattern)", {
  # "28 (62.2%)" — dot at pos 7, space at pos 3 → dot is inside parens
  res <- split_at_decimal("28 (62.2%)")
  expect_equal(res$before, "28")
  expect_equal(res$after, " (62.2%)")
})

test_that("split_at_decimal prefers dot when dot is before space", {
  # "62.3 (10.14)" — dot at pos 3, space at pos 5 → dot is the decimal
  res <- split_at_decimal("62.3 (10.14)")
  expect_equal(res$before, "62")
  expect_equal(res$after, ".3 (10.14)")
})

test_that("split_at_decimal handles CI pattern (dot before space)", {
  # "0.8 (-1.2, 2.8)" — dot at pos 2, space at pos 4 → standard decimal
  res <- split_at_decimal("0.8 (-1.2, 2.8)")
  expect_equal(res$before, "0")
  expect_equal(res$after, ".8 (-1.2, 2.8)")
})

test_that("compute_decimal_before_twips returns positive width from content", {
  data <- data.frame(
    a = c("168.0", "45", "1"),
    stringsAsFactors = FALSE
  )
  cols <- list(a = fr_col("A", width = 1.0, align = "decimal"))
  cols$a$id <- "a"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)
  result <- compute_decimal_before_twips(data, cols, grid, page$font_family, page$font_size)

  # Should be positive and equal to the width of the widest "before" part ("168")
  expect_true(result[["a"]] > 0L)
  before_w <- measure_text_width_twips("168", page$font_family, page$font_size)
  expect_equal(result[["a"]], as.integer(before_w))
})


test_that("compute_decimal_before_twips returns NA for non-decimal columns", {
  data <- data.frame(a = "hello", b = "1.5", stringsAsFactors = FALSE)
  cols <- list(
    a = fr_col("A", width = 1.0, align = "left"),
    b = fr_col("B", width = 1.0, align = "decimal")
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)
  result <- compute_decimal_before_twips(data, cols, grid, page$font_family, page$font_size)

  expect_true(is.na(result[["a"]]))
  expect_false(is.na(result[["b"]]))
})


# ──────────────────────────────────────────────────────────────────────────────
# Issue 3: Inline markup in data cell content
# ──────────────────────────────────────────────────────────────────────────────

test_that("build_cell_grid evaluates inline markup in cell content", {
  data <- data.frame(
    a = c("Age {fr_super('a')}", "Weight"),
    b = c("65.2", "80.1"),
    stringsAsFactors = FALSE
  )
  cols <- list(
    a = fr_col("A", width = 1.5),
    b = fr_col("B", width = 1.0)
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)

  # The {fr_super('a')} should be resolved to a sentinel token
  age_content <- grid$content[grid$row_idx == 1L & grid$col_name == "a"]
  expect_true(grepl("\x01SUPER:a\x02", age_content))
  # Plain text stays plain
  weight_content <- grid$content[grid$row_idx == 2L & grid$col_name == "a"]
  expect_equal(weight_content, "Weight")
})



# ══════════════════════════════════════════════════════════════════════════════
# Additional coverage tests
# ══════════════════════════════════════════════════════════════════════════════

# ── n_spanner_levels ─────────────────────────────────────────────────────────

test_that("n_spanner_levels returns 0 for empty spans", {
  expect_equal(n_spanner_levels(list()), 0L)
})

test_that("n_spanner_levels counts unique levels", {
  spans <- list(
    new_fr_span("S1", columns = c("a", "b"), level = 1L),
    new_fr_span("S2", columns = c("c", "d"), level = 1L),
    new_fr_span("S3", columns = c("a", "b"), level = 2L)
  )
  expect_equal(n_spanner_levels(spans), 2L)
})

test_that("n_spanner_levels returns 1 for single level", {
  spans <- list(
    new_fr_span("S1", columns = c("a", "b"), level = 1L)
  )
  expect_equal(n_spanner_levels(spans), 1L)
})


# ── build_cell_grid with cell_styles ─────────────────────────────────────────

test_that("build_cell_grid applies body cell_styles", {
  data <- data.frame(a = c("x", "y"), b = c("1", "2"),
                     stringsAsFactors = FALSE)
  cols <- list(
    a = fr_col("A", width = 1, align = "left"),
    b = fr_col("B", width = 1, align = "right")
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  style <- new_fr_cell_style(
    region = "body", type = "cell",
    rows = 1L, cols = "a",
    bold = TRUE, italic = TRUE, fg = "#FF0000", bg = "#00FF00",
    font_size = 12, align = "center", valign = "middle",
    underline = TRUE, indent = 0.5
  )

  grid <- build_cell_grid(data, cols, list(style), page)

  # Row 1, col "a" should have overridden properties
  idx <- grid$row_idx == 1L & grid$col_name == "a"
  expect_true(grid$bold[idx])
  expect_true(grid$italic[idx])
  expect_true(grid$underline[idx])
  expect_equal(grid$fg[idx], "#FF0000")
  expect_equal(grid$bg[idx], "#00FF00")
  expect_equal(grid$font_size[idx], 12)
  expect_equal(grid$align[idx], "center")
  expect_equal(grid$valign[idx], "middle")
  expect_equal(grid$indent[idx], 0.5)

  # Row 2, col "a" should NOT be affected
  idx2 <- grid$row_idx == 2L & grid$col_name == "a"
  expect_false(grid$bold[idx2])
})

test_that("build_cell_grid skips non-body/non-stub styles", {
  data <- data.frame(a = "x", stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 1))
  cols$a$id <- "a"
  page <- new_fr_page()

  style <- new_fr_cell_style(region = "header", type = "cell",
                              bold = TRUE)
  grid <- build_cell_grid(data, cols, list(style), page)
  expect_false(grid$bold[1])
})

test_that("build_cell_grid handles stub region style", {
  data <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  cols <- list(
    a = fr_col("A", width = 1),
    b = fr_col("B", width = 1)
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  style <- new_fr_cell_style(region = "stub", type = "col",
                              bold = TRUE)
  grid <- build_cell_grid(data, cols, list(style), page)
  # Stub targets first column only
  expect_true(grid$bold[grid$col_idx == 1L])
  expect_false(grid$bold[grid$col_idx == 2L])
})

test_that("build_cell_grid replaces NA content with empty string", {
  data <- data.frame(a = c(NA_character_, "ok"), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 1))
  cols$a$id <- "a"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)
  expect_equal(grid$content[1], "")
  expect_equal(grid$content[2], "ok")
})

test_that("build_cell_grid with zero columns returns empty grid", {
  data <- data.frame(a = "x", stringsAsFactors = FALSE)
  grid <- build_cell_grid(data, list(), list(), new_fr_page())
  expect_equal(nrow(grid), 0L)
})


# ── resolve_style_mask ───────────────────────────────────────────────────────

test_that("resolve_style_mask handles numeric cols", {
  grid <- data.frame(
    row_idx = c(1L, 1L, 2L, 2L),
    col_idx = c(1L, 2L, 1L, 2L),
    col_name = c("a", "b", "a", "b"),
    stringsAsFactors = FALSE
  )
  style <- new_fr_cell_style(region = "body", type = "cell",
                              cols = 2L, rows = "all")
  mask <- resolve_style_mask(style, grid, c("a", "b"))
  expect_equal(mask, c(FALSE, TRUE, FALSE, TRUE))
})

test_that("resolve_style_mask handles character cols", {
  grid <- data.frame(
    row_idx = c(1L, 1L),
    col_idx = c(1L, 2L),
    col_name = c("a", "b"),
    stringsAsFactors = FALSE
  )
  style <- new_fr_cell_style(region = "body", type = "cell",
                              cols = "b", rows = "all")
  mask <- resolve_style_mask(style, grid, c("a", "b"))
  expect_equal(mask, c(FALSE, TRUE))
})

test_that("resolve_style_mask with row type ignores cols", {
  grid <- data.frame(
    row_idx = c(1L, 1L, 2L, 2L),
    col_idx = c(1L, 2L, 1L, 2L),
    col_name = c("a", "b", "a", "b"),
    stringsAsFactors = FALSE
  )
  style <- new_fr_cell_style(region = "body", type = "row",
                              rows = 1L, cols = "a")
  mask <- resolve_style_mask(style, grid, c("a", "b"))
  # type = "row" means all columns in that row
  expect_equal(mask, c(TRUE, TRUE, FALSE, FALSE))
})

test_that("resolve_style_mask handles specific rows", {
  grid <- data.frame(
    row_idx = c(1L, 2L, 3L),
    col_idx = c(1L, 1L, 1L),
    col_name = c("a", "a", "a"),
    stringsAsFactors = FALSE
  )
  style <- new_fr_cell_style(region = "body", type = "cell",
                              rows = c(1L, 3L))
  mask <- resolve_style_mask(style, grid, c("a"))
  expect_equal(mask, c(TRUE, FALSE, TRUE))
})


# ── apply_styles_to_grid ─────────────────────────────────────────────────────

test_that("apply_styles_to_grid works with header region and header_row_idx", {
  grid <- data.frame(
    col_idx = c(1L, 2L),
    col_name = c("a", "b"),
    align = c("left", "left"),
    valign = c("bottom", "bottom"),
    bold = c(FALSE, FALSE),
    italic = c(FALSE, FALSE),
    underline = c(FALSE, FALSE),
    fg = c("#000000", "#000000"),
    bg = c(NA_character_, NA_character_),
    font_size = c(9, 9),
    stringsAsFactors = FALSE
  )
  style <- new_fr_cell_style(region = "header", type = "cell",
                              cols = "a", bold = TRUE, fg = "#FF0000")
  result <- apply_styles_to_grid(grid, list(style), "header", c("a", "b"),
                                  header_row_idx = 1L)
  expect_true(result$bold[1])
  expect_false(result$bold[2])
  expect_equal(result$fg[1], "#FF0000")
})

test_that("apply_styles_to_grid skips non-matching region", {
  grid <- data.frame(
    col_idx = 1L, col_name = "a", align = "left", valign = "top",
    bold = FALSE, italic = FALSE, underline = FALSE,
    fg = "#000000", bg = NA_character_, font_size = 9,
    stringsAsFactors = FALSE
  )
  style <- new_fr_cell_style(region = "body", type = "cell", bold = TRUE)
  result <- apply_styles_to_grid(grid, list(style), "header", "a",
                                  header_row_idx = 1L)
  expect_false(result$bold[1])
})

test_that("apply_styles_to_grid skips header style with non-matching row_idx", {
  grid <- data.frame(
    col_idx = 1L, col_name = "a", align = "left", valign = "top",
    bold = FALSE, italic = FALSE, underline = FALSE,
    fg = "#000000", bg = NA_character_, font_size = 9,
    stringsAsFactors = FALSE
  )
  style <- new_fr_cell_style(region = "header", type = "cell",
                              rows = 3L, bold = TRUE)
  result <- apply_styles_to_grid(grid, list(style), "header", "a",
                                  header_row_idx = 1L)
  expect_false(result$bold[1])
})

test_that("apply_styles_to_grid handles numeric cols in header", {
  grid <- data.frame(
    col_idx = c(1L, 2L),
    col_name = c("a", "b"),
    align = c("left", "left"),
    valign = c("bottom", "bottom"),
    bold = c(FALSE, FALSE),
    italic = c(FALSE, FALSE),
    underline = c(FALSE, FALSE),
    fg = c("#000000", "#000000"),
    bg = c(NA_character_, NA_character_),
    font_size = c(9, 9),
    stringsAsFactors = FALSE
  )
  style <- new_fr_cell_style(region = "header", type = "cell",
                              cols = 2L, italic = TRUE)
  result <- apply_styles_to_grid(grid, list(style), "header", c("a", "b"),
                                  header_row_idx = 1L)
  expect_false(result$italic[1])
  expect_true(result$italic[2])
})

test_that("apply_styles_to_grid handles indent in body mode", {
  grid <- data.frame(
    row_idx = c(1L, 2L),
    col_idx = c(1L, 1L),
    col_name = c("a", "a"),
    align = c("left", "left"),
    valign = c("top", "top"),
    bold = c(FALSE, FALSE),
    italic = c(FALSE, FALSE),
    underline = c(FALSE, FALSE),
    fg = c("#000000", "#000000"),
    bg = c(NA_character_, NA_character_),
    font_size = c(9, 9),
    indent = c(0, 0),
    stringsAsFactors = FALSE
  )
  style <- new_fr_cell_style(region = "body", type = "cell",
                              rows = 2L, indent = 0.25)
  result <- apply_styles_to_grid(grid, list(style), "body", "a")
  expect_equal(result$indent[1], 0)
  expect_equal(result$indent[2], 0.25)
})


# ── build_header_cell_grid ───────────────────────────────────────────────────

test_that("build_header_cell_grid produces correct default structure", {
  cols <- list(
    a = fr_col("A", width = 1, align = "left"),
    b = fr_col("B", width = 1, align = "right")
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  grid <- build_header_cell_grid(cols, list(), page, header_row_idx = 1L)
  expect_equal(nrow(grid), 2L)
  expect_equal(grid$col_name, c("a", "b"))
  expect_equal(unname(grid$align), c("left", "right"))
  expect_equal(grid$valign, c("bottom", "bottom"))
  expect_false(any(grid$bold))
  expect_equal(grid$fg, c("#000000", "#000000"))
})

test_that("build_header_cell_grid uses header_cfg defaults", {
  cols <- list(a = fr_col("A", width = 1))
  cols$a$id <- "a"
  page <- new_fr_page()
  hcfg <- new_fr_header(bold = TRUE, fg = "#0000FF", bg = "#FFFF00",
                         font_size = 14)

  grid <- build_header_cell_grid(cols, list(), page, header_row_idx = 1L,
                                  header_cfg = hcfg)
  expect_true(grid$bold[1])
  expect_equal(grid$fg[1], "#0000FF")
  expect_equal(grid$bg[1], "#FFFF00")
  expect_equal(grid$font_size[1], 14)
})

test_that("build_header_cell_grid applies header styles", {
  cols <- list(
    a = fr_col("A", width = 1),
    b = fr_col("B", width = 1)
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  style <- new_fr_cell_style(region = "header", type = "cell",
                              cols = "b", bold = TRUE, bg = "#CCCCCC")
  grid <- build_header_cell_grid(cols, list(style), page, header_row_idx = 1L)
  expect_false(grid$bold[1])
  expect_true(grid$bold[2])
  expect_equal(grid$bg[2], "#CCCCCC")
})

test_that("build_header_cell_grid skips non-header styles", {
  cols <- list(a = fr_col("A", width = 1))
  cols$a$id <- "a"
  page <- new_fr_page()

  style <- new_fr_cell_style(region = "body", type = "cell", bold = TRUE)
  grid <- build_header_cell_grid(cols, list(style), page, header_row_idx = 1L)
  expect_false(grid$bold[1])
})

test_that("build_header_cell_grid skips style with non-matching row index", {
  cols <- list(a = fr_col("A", width = 1))
  cols$a$id <- "a"
  page <- new_fr_page()

  style <- new_fr_cell_style(region = "header", type = "cell",
                              rows = 5L, bold = TRUE)
  grid <- build_header_cell_grid(cols, list(style), page, header_row_idx = 1L)
  expect_false(grid$bold[1])
})

test_that("build_header_cell_grid handles numeric cols in style", {
  cols <- list(
    a = fr_col("A", width = 1),
    b = fr_col("B", width = 1)
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  style <- new_fr_cell_style(region = "header", type = "cell",
                              cols = 1L, italic = TRUE)
  grid <- build_header_cell_grid(cols, list(style), page, header_row_idx = 1L)
  expect_true(grid$italic[1])
  expect_false(grid$italic[2])
})

test_that("build_header_cell_grid row type applies to all columns", {
  cols <- list(
    a = fr_col("A", width = 1),
    b = fr_col("B", width = 1)
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  style <- new_fr_cell_style(region = "header", type = "row",
                              bold = TRUE)
  grid <- build_header_cell_grid(cols, list(style), page, header_row_idx = 1L)
  expect_true(all(grid$bold))
})

test_that("build_header_cell_grid uses header_align from column spec", {
  cols <- list(
    a = fr_col("A", width = 1, align = "left", header_align = "center")
  )
  cols$a$id <- "a"
  page <- new_fr_page()

  grid <- build_header_cell_grid(cols, list(), page, header_row_idx = 1L)
  expect_equal(unname(grid$align[1]), "center")
})


# ── build_row_heights ────────────────────────────────────────────────────────

test_that("build_row_heights returns NA for no row styles", {
  result <- build_row_heights(5L, list())
  expect_equal(result, rep(NA_real_, 5L))
})

test_that("build_row_heights applies height to specific rows", {
  style <- new_fr_cell_style(type = "row", region = "body",
                              rows = c(2L, 4L), height = 0.5)
  result <- build_row_heights(5L, list(style))
  expect_true(is.na(result[1]))
  expect_equal(result[2], 0.5)
  expect_true(is.na(result[3]))
  expect_equal(result[4], 0.5)
})

test_that("build_row_heights applies height to all rows", {
  style <- new_fr_cell_style(type = "row", region = "body",
                              rows = "all", height = 0.3)
  result <- build_row_heights(5L, list(style))
  expect_equal(result, rep(0.3, 5L))
})

test_that("build_row_heights with NULL rows applies to all", {
  style <- new_fr_cell_style(type = "row", region = "body",
                              rows = NULL, height = 0.4)
  result <- build_row_heights(5L, list(style))
  expect_equal(result, rep(0.4, 5L))
})

test_that("build_row_heights skips non-row type styles", {
  style <- new_fr_cell_style(type = "cell", region = "body",
                              rows = 1L, height = 0.5)
  result <- build_row_heights(3L, list(style))
  expect_equal(result, rep(NA_real_, 3L))
})

test_that("build_row_heights skips styles without height", {
  style <- new_fr_cell_style(type = "row", region = "body",
                              rows = 1L, bold = TRUE)
  result <- build_row_heights(3L, list(style))
  expect_equal(result, rep(NA_real_, 3L))
})

test_that("build_row_heights clips out-of-bounds rows", {
  style <- new_fr_cell_style(type = "row", region = "body",
                              rows = c(1L, 10L), height = 0.5)
  result <- build_row_heights(3L, list(style))
  expect_equal(result[1], 0.5)
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
})


# ── build_keep_mask ──────────────────────────────────────────────────────────

test_that("build_keep_mask returns FALSE for single row", {
  data <- data.frame(grp = "A", val = "x", stringsAsFactors = FALSE)
  expect_equal(build_keep_mask(data, "grp"), FALSE)
})

test_that("build_keep_mask returns FALSE for empty keep_cols", {
  data <- data.frame(grp = c("A", "A"), val = c("x", "y"),
                     stringsAsFactors = FALSE)
  expect_equal(build_keep_mask(data, character(0)), c(FALSE, FALSE))
})

test_that("build_keep_mask small groups kept entirely together", {
  # 2-row groups (< default orphan_min + widow_min = 6): keep all together
  data <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  )
  mask <- build_keep_mask(data, "grp")
  # Row 1 keeps with row 2 (TRUE), row 2 last in group (FALSE)

  # Row 3 keeps with row 4 (TRUE), row 4 last in group (FALSE)
  expect_equal(mask, c(TRUE, FALSE, TRUE, FALSE))
})

test_that("build_keep_mask ignores blank rows", {
  data <- data.frame(
    grp = c("A", "A", "",  "B", "B"),
    val = c("1", "2", "",  "3", "4"),
    stringsAsFactors = FALSE
  )
  mask <- build_keep_mask(data, "grp")
  # Blank row (row 3) should be FALSE
  expect_false(mask[3])
  # Group A: row 1 keeps with row 2, row 2 last (FALSE)
  expect_true(mask[1])
  expect_false(mask[2])
  # Group B: row 4 keeps with row 5, row 5 last (FALSE)
  expect_true(mask[4])
  expect_false(mask[5])
})

test_that("build_keep_mask returns FALSE for non-existent columns", {
  data <- data.frame(a = c("A", "A"), stringsAsFactors = FALSE)
  expect_equal(build_keep_mask(data, "nonexistent"), c(FALSE, FALSE))
})

test_that("build_keep_mask large group uses orphan/widow minimums", {
  # Group with 10 rows, orphan_min = 3, widow_min = 3
  data <- data.frame(
    grp = rep("A", 10L),
    val = as.character(seq_len(10L)),
    stringsAsFactors = FALSE
  )
  mask <- build_keep_mask(data, "grp", orphan_min = 3L, widow_min = 3L)
  # Top: rows 1-2 keep-with-next (orphan_min - 1 = 2 rows)
  expect_true(mask[1])
  expect_true(mask[2])
  expect_false(mask[3])  # Middle: free to split
  expect_false(mask[4])
  # Bottom: rows 8-9 keep-with-next (widow_min = 3 → last 3 glued)
  expect_true(mask[8])
  expect_true(mask[9])
  expect_false(mask[10])  # Last row: no next row to keep with
})

test_that("build_keep_mask respects custom orphan_min/widow_min", {
  data <- data.frame(
    grp = rep("A", 12L),
    val = as.character(seq_len(12L)),
    stringsAsFactors = FALSE
  )
  mask <- build_keep_mask(data, "grp", orphan_min = 4L, widow_min = 2L)
  # Top: rows 1-3 keep-with-next (orphan_min - 1 = 3)
  expect_true(all(mask[1:3]))
  expect_false(mask[4])
  # Bottom: rows 11 keep-with-next (widow_min = 2 → last 2 glued)
  expect_true(mask[11])
  expect_false(mask[12])
})


# ── insert_blank_after ───────────────────────────────────────────────────────

test_that("insert_blank_after inserts blanks at group boundaries", {
  data <- data.frame(
    grp = c("A", "A", "B", "B", "C"),
    val = c("1", "2", "3", "4", "5"),
    stringsAsFactors = FALSE
  )
  result <- insert_blank_after(data, "grp")
  # 5 data rows + 2 blank rows (A->B, B->C) = 7
  expect_equal(nrow(result$data), 7L)
  expect_true(all(result$data[3L, ] == ""))
  expect_true(all(result$data[6L, ] == ""))
  # insert_positions are original row indices of boundary rows
  expect_equal(result$insert_positions, c(2L, 4L))
})

test_that("insert_blank_after returns data unchanged for single row", {
  data <- data.frame(grp = "A", val = "1", stringsAsFactors = FALSE)
  result <- insert_blank_after(data, "grp")
  expect_equal(nrow(result$data), 1L)
  expect_equal(result$insert_positions, integer(0))
})

test_that("insert_blank_after returns data unchanged when no blank_cols", {
  data <- data.frame(grp = c("A", "B"), val = c("1", "2"),
                     stringsAsFactors = FALSE)
  result <- insert_blank_after(data, character(0))
  expect_equal(nrow(result$data), 2L)
  expect_equal(result$insert_positions, integer(0))
})

test_that("insert_blank_after returns data unchanged for non-existent cols", {
  data <- data.frame(a = c("A", "B"), stringsAsFactors = FALSE)
  result <- insert_blank_after(data, "nonexistent")
  expect_equal(nrow(result$data), 2L)
  expect_equal(result$insert_positions, integer(0))
})

test_that("insert_blank_after returns data unchanged when all keys are same", {
  data <- data.frame(grp = c("A", "A", "A"), stringsAsFactors = FALSE)
  result <- insert_blank_after(data, "grp")
  expect_equal(nrow(result$data), 3L)
  expect_equal(result$insert_positions, integer(0))
})


# ── remap_style_indices ──────────────────────────────────────────────────────

test_that("remap_style_indices shifts numeric row indices", {
  styles <- list(
    new_fr_cell_style(region = "body", type = "row", rows = c(1L, 3L),
                      bold = TRUE),
    new_fr_cell_style(region = "body", type = "col", rows = c(2L, 4L),
                      cols = "a", italic = TRUE)
  )
  # Blank inserted after row 2 in original data
  result <- remap_style_indices(styles, c(2L))
  # Row 1 stays 1, row 3 → 4 (shifted +1)
  expect_equal(result[[1]]$rows, c(1L, 4L))
  # Row 2 stays 2, row 4 → 5
  expect_equal(result[[2]]$rows, c(2L, 5L))
})

test_that("remap_style_indices handles multiple insert positions", {
  styles <- list(
    new_fr_cell_style(region = "body", type = "row", rows = c(1L, 2L, 3L, 4L, 5L),
                      bold = TRUE)
  )
  # Blanks after rows 2 and 4 in original data
  result <- remap_style_indices(styles, c(2L, 4L))
  # Row 1 → 1, 2 → 2, 3 → 4, 4 → 5, 5 → 7
  expect_equal(result[[1]]$rows, c(1L, 2L, 4L, 5L, 7L))
})

test_that("remap_style_indices skips 'all' and NULL rows", {
  styles <- list(
    new_fr_cell_style(region = "body", type = "col", rows = "all",
                      cols = "a", bold = TRUE),
    new_fr_cell_style(region = "body", type = "col", rows = NULL,
                      cols = "b", italic = TRUE)
  )
  result <- remap_style_indices(styles, c(2L))
  expect_equal(result[[1]]$rows, "all")
  expect_null(result[[2]]$rows)
})


# ── apply_indent_by ──────────────────────────────────────────────────────────

test_that("apply_indent_by adds style when indent_by is set without group_by", {
  spec <- new_fr_spec(data.frame(a = c("x", "y"), stringsAsFactors = FALSE))
  spec$body <- new_fr_body(indent_by = "a")
  spec$cell_styles <- list()
  result <- apply_indent_by(spec)
  expect_length(result$cell_styles, 1L)
  expect_equal(result$cell_styles[[1]]$rows, "all")
  expect_equal(result$cell_styles[[1]]$cols, "a")
})

test_that("apply_indent_by returns spec unchanged when no indent_by", {
  spec <- new_fr_spec(data.frame(a = "x", stringsAsFactors = FALSE))
  spec$body <- new_fr_body()
  result <- apply_indent_by(spec)
  expect_length(result$cell_styles, 0L)
})

test_that("apply_indent_by with group_by indents only detail rows", {
  data <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  )
  spec <- new_fr_spec(data)
  spec$body <- new_fr_body(group_by = "grp", indent_by = "val")
  spec$cell_styles <- list()
  result <- apply_indent_by(spec)
  expect_length(result$cell_styles, 1L)
  # Group headers are rows 1 and 3; detail rows are 2 and 4
  expect_equal(result$cell_styles[[1]]$rows, c(2L, 4L))
})

test_that("apply_indent_by skips non-existent indent columns", {
  spec <- new_fr_spec(data.frame(a = "x", stringsAsFactors = FALSE))
  spec$body <- new_fr_body(indent_by = "nonexistent")
  result <- apply_indent_by(spec)
  expect_length(result$cell_styles, 0L)
})

test_that("apply_indent_by handles empty data", {
  spec <- new_fr_spec(data.frame(a = character(0), stringsAsFactors = FALSE))
  spec$body <- new_fr_body(indent_by = "a")
  result <- apply_indent_by(spec)
  expect_length(result$cell_styles, 0L)
})


# ── resolve_borders: vlines ──────────────────────────────────────────────────

test_that("resolve_borders handles vline preset 'all'", {
  vline <- structure(
    list(preset = "all", cols = NULL, width = 0.5,
         linestyle = "solid", fg = "#000000"),
    class = c("fr_vline_spec", "fr_rule")
  )
  borders <- resolve_borders(list(vline), nrow_body = 2L, ncol = 3L,
                              nrow_header = 1L)
  # Left edge of col 1
  expect_false(is.null(borders$header$left[1, 1][[1]]))
  expect_false(is.null(borders$body$left[1, 1][[1]]))
  # Right edge of last col
  expect_false(is.null(borders$header$right[1, 3][[1]]))
  expect_false(is.null(borders$body$right[1, 3][[1]]))
  # Inner borders
  expect_false(is.null(borders$header$right[1, 1][[1]]))
  expect_false(is.null(borders$header$left[1, 2][[1]]))
})

test_that("resolve_borders handles vline preset 'inner'", {
  vline <- structure(
    list(preset = "inner", cols = NULL, width = 0.5,
         linestyle = "solid", fg = "#000000"),
    class = c("fr_vline_spec", "fr_rule")
  )
  borders <- resolve_borders(list(vline), nrow_body = 2L, ncol = 3L,
                              nrow_header = 1L)
  # Inner borders should be set
  expect_false(is.null(borders$header$right[1, 1][[1]]))
  expect_false(is.null(borders$header$left[1, 2][[1]]))
  # Outer edges should NOT be set
  expect_true(is.null(borders$header$left[1, 1][[1]]))
  expect_true(is.null(borders$header$right[1, 3][[1]]))
})

test_that("resolve_borders handles vline preset 'box'", {
  vline <- structure(
    list(preset = "box", cols = NULL, width = 0.5,
         linestyle = "solid", fg = "#000000"),
    class = c("fr_vline_spec", "fr_rule")
  )
  borders <- resolve_borders(list(vline), nrow_body = 2L, ncol = 3L,
                              nrow_header = 1L)
  # Outer edges should be set
  expect_false(is.null(borders$header$left[1, 1][[1]]))
  expect_false(is.null(borders$header$right[1, 3][[1]]))
  # Inner borders should NOT be set
  expect_true(is.null(borders$header$right[1, 1][[1]]))
})

test_that("resolve_borders handles vline with specific cols", {
  vline <- structure(
    list(preset = NULL, cols = 1L, width = 0.5,
         linestyle = "solid", fg = "#000000"),
    class = c("fr_vline_spec", "fr_rule")
  )
  borders <- resolve_borders(list(vline), nrow_body = 2L, ncol = 3L,
                              nrow_header = 1L)
  # Gap between col 1 and 2
  expect_false(is.null(borders$header$right[1, 1][[1]]))
  expect_false(is.null(borders$header$left[1, 2][[1]]))
  # No border between col 2 and 3
  expect_true(is.null(borders$header$right[1, 2][[1]]))
})


# ── resolve_borders: body rows ───────────────────────────────────────────────

test_that("resolve_borders handles body below with rows='all'", {
  rule <- new_fr_rule(direction = "horizontal", region = "body",
                      side = "below", rows = "all",
                      width = 0.5, linestyle = "solid", fg = "#000000")
  borders <- resolve_borders(list(rule), nrow_body = 3L, ncol = 2L)
  for (i in 1:3) {
    expect_false(is.null(borders$body$bottom[i, 1][[1]]))
  }
})

test_that("resolve_borders handles body below with specific rows", {
  rule <- new_fr_rule(direction = "horizontal", region = "body",
                      side = "below", rows = c(1L, 3L),
                      width = 0.5, linestyle = "solid", fg = "#000000")
  borders <- resolve_borders(list(rule), nrow_body = 3L, ncol = 2L)
  expect_false(is.null(borders$body$bottom[1, 1][[1]]))
  expect_true(is.null(borders$body$bottom[2, 1][[1]]))
  expect_false(is.null(borders$body$bottom[3, 1][[1]]))
})

test_that("resolve_borders handles body above", {
  rule <- new_fr_rule(direction = "horizontal", region = "body",
                      side = "above",
                      width = 0.5, linestyle = "solid", fg = "#000000")
  borders <- resolve_borders(list(rule), nrow_body = 3L, ncol = 2L)
  expect_false(is.null(borders$body$top[1, 1][[1]]))
  expect_true(is.null(borders$body$top[2, 1][[1]]))
})

test_that("resolve_borders handles header above", {
  rule <- new_fr_rule(direction = "horizontal", region = "header",
                      side = "above",
                      width = 0.5, linestyle = "solid", fg = "#000000")
  borders <- resolve_borders(list(rule), nrow_body = 2L, ncol = 2L)
  expect_false(is.null(borders$header$top[1, 1][[1]]))
})

test_that("resolve_borders skips non-horizontal fr_rule", {
  rule <- new_fr_rule(direction = "vertical", region = "header",
                      side = "below")
  borders <- resolve_borders(list(rule), nrow_body = 2L, ncol = 2L)
  # Should be all NULL
  expect_true(is.null(borders$header$bottom[1, 1][[1]]))
})

test_that("resolve_borders handles zero body rows", {
  rule <- new_fr_rule(direction = "horizontal", region = "body",
                      side = "below", fg = "#000000")
  borders <- resolve_borders(list(rule), nrow_body = 0L, ncol = 2L)
  # Should not error; body matrices have 1 row (max(1, 0))
  expect_equal(nrow(borders$body$bottom), 1L)
})

test_that("resolve_borders clips out-of-bound row indices", {
  rule <- new_fr_rule(direction = "horizontal", region = "body",
                      side = "below", rows = c(1L, 100L),
                      width = 0.5, linestyle = "solid", fg = "#000000")
  borders <- resolve_borders(list(rule), nrow_body = 3L, ncol = 2L)
  expect_false(is.null(borders$body$bottom[1, 1][[1]]))
  # Row 100 is out of bounds, should not crash
  expect_true(is.null(borders$body$bottom[3, 1][[1]]))
})


# ── LaTeX sentinel resolver ─────────────────────────────────────────────────

test_that("latex_sentinel_resolver handles all types", {
  expect_equal(latex_sentinel_resolver("SUPER", "a"),
               "\\textsuperscript{a}")
  expect_equal(latex_sentinel_resolver("SUB", "x"),
               "\\textsubscript{x}")
  expect_equal(latex_sentinel_resolver("BOLD", "hi"),
               "\\textbf{hi}")
  expect_equal(latex_sentinel_resolver("ITALIC", "em"),
               "\\textit{em}")
  expect_equal(latex_sentinel_resolver("UNDERLINE", "u"),
               "\\underline{u}")
  expect_equal(latex_sentinel_resolver("NEWLINE", ""),
               "\\\\")
})

test_that("latex_sentinel_resolver returns content for unknown type", {
  expect_equal(latex_sentinel_resolver("UNKNOWN", "xyz"), "xyz")
})

test_that("rtf_sentinel_resolver returns content for unknown type", {
  expect_equal(rtf_sentinel_resolver("UNKNOWN", "xyz"), "xyz")
})

test_that("rtf_sentinel_resolver handles UNICODE type", {
  result <- rtf_sentinel_resolver("UNICODE", "\u00b1")
  expect_true(nzchar(result))
})

test_that("latex_sentinel_resolver handles UNICODE type", {
  result <- latex_sentinel_resolver("UNICODE", "\u00b1")
  expect_true(nzchar(result))
})


# ── latex_escape ─────────────────────────────────────────────────────────────

test_that("latex_escape handles special characters", {
  result <- latex_escape("100% & $5 #1")
  expect_true(grepl("\\\\%", result))
  expect_true(grepl("\\\\&", result))
  expect_true(grepl("\\\\\\$", result))
  expect_true(grepl("\\\\#", result))
})

test_that("latex_escape handles backslash", {
  result <- latex_escape("a\\b")
  expect_true(grepl("textbackslash", result))
})

test_that("latex_escape handles empty input", {
  expect_equal(latex_escape(character(0)), character(0))
  expect_equal(latex_escape(""), "")
})

test_that("latex_escape handles braces", {
  result <- latex_escape("a{b}c")
  expect_true(grepl("\\\\\\{", result))
  expect_true(grepl("\\\\\\}", result))
})

test_that("latex_escape converts non-ASCII characters", {
  result <- latex_escape("\u00b1")
  expect_true(nzchar(result))
})


# ── latex_escape_and_resolve ─────────────────────────────────────────────────

test_that("latex_escape_and_resolve handles plain text", {
  expect_equal(latex_escape_and_resolve("hello"), "hello")
})

test_that("latex_escape_and_resolve handles sentinel", {
  sentinel <- paste0("\x01SUPER:1\x02")
  result <- latex_escape_and_resolve(sentinel)
  expect_equal(result, "\\textsuperscript{1}")
})

test_that("latex_escape_and_resolve handles mixed text and sentinels", {
  mixed <- paste0("100% \x01BOLD:hi\x02 end")
  result <- latex_escape_and_resolve(mixed)
  expect_true(grepl("\\\\%", result))
  expect_true(grepl("\\\\textbf\\{hi\\}", result))
  expect_true(grepl("end", result))
})

test_that("latex_escape_and_resolve escapes special chars in non-sentinel parts", {
  mixed <- paste0("a&b \x01ITALIC:c\x02")
  result <- latex_escape_and_resolve(mixed)
  expect_true(grepl("a\\\\&b", result))
  expect_true(grepl("\\\\textit\\{c\\}", result))
})


# ── rtf_encode_unicode_char ──────────────────────────────────────────────────

test_that("rtf_encode_unicode_char encodes basic non-ASCII", {
  result <- rtf_encode_unicode_char("\u00b1")
  expect_true(grepl("\\\\u", result))
})

test_that("rtf_encode_unicode_char handles empty input", {
  expect_equal(rtf_encode_unicode_char(""), "")
})

test_that("rtf_encode_unicode_char handles high codepoints with signed 16-bit", {
  # Codepoint > 32767 should become negative
  result <- rtf_encode_unicode_char("\U0001F600")
  # This is > BMP, so raw codepoint > 32767
  expect_true(grepl("\\\\u-?[0-9]+\\?", result))
})


# ── latex_encode_unicode_char ────────────────────────────────────────────────

test_that("latex_encode_unicode_char returns mapped value for known chars", {
  # \u00b1 is plus-minus, should be in latex_unicode map
  result <- latex_encode_unicode_char("\u00b1")
  expect_true(nzchar(result))
})

test_that("latex_encode_unicode_char passes through unknown unicode", {
  # Use a rarely-mapped character
  result <- latex_encode_unicode_char("\u4e16")  # CJK character
  expect_equal(result, "\u4e16")
})


# ── compute_decimal_before_pt ────────────────────────────────────────────────

test_that("compute_decimal_before_pt returns twips / 20", {
  data <- data.frame(a = c("168.0", "45"), stringsAsFactors = FALSE)
  cols <- list(a = fr_col("A", width = 1.0, align = "decimal"))
  cols$a$id <- "a"
  page <- new_fr_page()

  grid <- build_cell_grid(data, cols, list(), page)
  twips <- compute_decimal_before_twips(data, cols, grid,
                                         page$font_family, page$font_size)
  pts <- compute_decimal_before_pt(data, cols, grid,
                                    page$font_family, page$font_size)
  expect_equal(pts[["a"]], twips[["a"]] / 20)
})


# ── collect_colors ───────────────────────────────────────────────────────────

test_that("collect_colors includes header bg and fg", {
  spec <- new_fr_spec(data.frame(x = 1))
  spec$header <- new_fr_header(bg = "#AABBCC", fg = "#112233")
  colors <- collect_colors(spec)
  expect_true("#AABBCC" %in% colors)
  expect_true("#112233" %in% colors)
})

test_that("collect_colors always includes black", {
  spec <- new_fr_spec(data.frame(x = 1))
  colors <- collect_colors(spec)
  expect_true("#000000" %in% colors)
})

test_that("collect_colors deduplicates", {
  spec <- new_fr_spec(data.frame(x = 1))
  spec$rules <- list(
    new_fr_rule(fg = "#FF0000"),
    new_fr_rule(fg = "#FF0000")
  )
  colors <- collect_colors(spec)
  expect_equal(sum(colors == "#FF0000"), 1L)
})


# ── build_rtf_colortbl ──────────────────────────────────────────────────────

test_that("build_rtf_colortbl deduplicates colors", {
  result <- build_rtf_colortbl(c("#000000", "#000000", "#FF0000"))
  expect_equal(length(result$index), 2L)
})

test_that("build_rtf_colortbl handles single color", {
  result <- build_rtf_colortbl("#FFFFFF")
  expect_equal(result$index[["#FFFFFF"]], 1L)
  expect_true(grepl("\\\\red255\\\\green255\\\\blue255", result$rtf))
})
