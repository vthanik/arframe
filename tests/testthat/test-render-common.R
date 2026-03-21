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
  expect_equal(nrow(grid), 4L) # 2 rows x 2 cols
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
    new_fr_rule(
      direction = "horizontal",
      region = "header",
      side = "below",
      width = 0.5,
      linestyle = "solid",
      fg = "#000000"
    )
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
  spec <- df |> fr_table() |> fr_rows(group_by = "grp", blank_after = "grp")
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
  spec <- df |> fr_table() |> fr_rows(group_by = "grp", blank_after = "grp")
  spec <- finalize_spec(spec)
  # blank_after inserts a blank row between groups A and B
  # 4 original rows + 1 blank row = 5
  expect_equal(nrow(spec$data), 5L)
})


# ── Decimal alignment (tests for R/decimal.R) ────────────────────────────────

# --- detect_stat_type tests ---

test_that("detect_stat_type identifies missing values", {
  expect_equal(detect_stat_type(""), "missing")
  expect_equal(detect_stat_type("NE"), "missing")
  expect_equal(detect_stat_type("NA"), "missing")
  expect_equal(detect_stat_type("NC"), "missing")
  expect_equal(detect_stat_type("-"), "missing")
})

test_that("detect_stat_type identifies n_only", {
  expect_equal(detect_stat_type("84"), "n_only")
  expect_equal(detect_stat_type("0"), "n_only")
  expect_equal(detect_stat_type("100"), "n_only")
})

test_that("detect_stat_type identifies scalar_float", {
  expect_equal(detect_stat_type("12.3"), "scalar_float")
  expect_equal(detect_stat_type("135.20"), "scalar_float")
  expect_equal(detect_stat_type("-2.5"), "scalar_float")
})

test_that("detect_stat_type identifies n_pct", {
  expect_equal(detect_stat_type("42 (50.0%)"), "n_pct")
  expect_equal(detect_stat_type("1 (2.2)"), "n_pct")
  expect_equal(detect_stat_type("100 (100.0%)"), "n_pct")
})

test_that("detect_stat_type identifies n_over_N_pct", {
  expect_equal(detect_stat_type("42/84 (50.0%)"), "n_over_N_pct")
})

test_that("detect_stat_type identifies est_spread", {
  expect_equal(detect_stat_type("75.0 (6.75)"), "est_spread")
  expect_equal(detect_stat_type("12.3 (4.56)"), "est_spread")
})

test_that("detect_stat_type identifies est_ci", {
  expect_equal(detect_stat_type("168.0 (152.4, 183.6)"), "est_ci")
  expect_equal(detect_stat_type("1.520 (0.650, 3.570)"), "est_ci")
  expect_equal(detect_stat_type("-2.1 (-4.3, 0.1)"), "est_ci")
})

test_that("detect_stat_type identifies range_pair", {
  expect_equal(detect_stat_type("2.0, 45.0"), "range_pair")
  expect_equal(detect_stat_type("65.0, 88.0"), "range_pair")
})

test_that("detect_stat_type identifies pvalue", {
  expect_equal(detect_stat_type("<0.001"), "pvalue")
  expect_equal(detect_stat_type(">0.999"), "pvalue")
})

test_that("detect_stat_type identifies int_range", {
  expect_equal(detect_stat_type("10 - 365"), "int_range")
})

# --- parse_stat_value tests ---

test_that("parse_stat_value parses n_pct correctly", {
  p <- parse_stat_value("42 (50.0%)", "n_pct")
  expect_equal(p$type, "n_pct")
  expect_equal(p$n, "42")
  expect_equal(p$pct_int, "50")
  expect_equal(p$pct_dec, "0")
  expect_equal(p$pct_sign, "%")
})

test_that("parse_stat_value parses scalar_float correctly", {
  p <- parse_stat_value("135.2", "scalar_float")
  expect_equal(p$type, "scalar_float")
  expect_equal(p$sign, "")
  expect_equal(p$int, "135")
  expect_equal(p$dec, "2")
})

test_that("parse_stat_value parses est_ci with negative values", {
  p <- parse_stat_value("-2.1 (-4.3, 0.1)", "est_ci")
  expect_equal(p$type, "est_ci")
  expect_equal(p$est_sign, "-")
  expect_equal(p$est_int, "2")
  expect_equal(p$est_dec, "1")
  expect_equal(p$lo_sign, "-")
  expect_equal(p$lo_int, "4")
})

test_that("parse_stat_value parses range_pair correctly", {
  p <- parse_stat_value("65.0, 88.0", "range_pair")
  expect_equal(p$type, "range_pair")
  expect_equal(p$l_int, "65")
  expect_equal(p$l_dec, "0")
  expect_equal(p$r_int, "88")
  expect_equal(p$r_dec, "0")
})

# --- align_decimal_column tests ---

test_that("align_decimal_column aligns n(%) values to same nchar", {
  vals <- c("100 (100.0%)", "42 (50.0%)", "1 (2.2%)")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
  expect_true(nchars[1] > 0L)
})

test_that("align_decimal_column aligns scalar floats with decimal alignment", {
  vals <- c("135.2", "85.1", "0.07")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
  # Decimal points should be at the same position
  dot_pos <- regexpr("\\.", result)
  expect_true(all(dot_pos == dot_pos[1]))
})

test_that("align_decimal_column right-aligns plain integers", {
  vals <- c("84", "7", "100")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
  # Right-aligned: "100" has no leading space, " 84" has 1, "  7" has 2
  expect_equal(nchar(sub("^ +", "", result[3])), 3L) # "100"
})

test_that("align_decimal_column handles mixed with missing", {
  vals <- c("42 (50.0%)", "", "NE")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
  # Missing values should be all spaces
  expect_equal(trimws(result[2]), "")
  expect_equal(trimws(result[3]), "")
})

test_that("align_decimal_column handles range_pair values", {
  vals <- c("2.0, 45.0", "65.0, 88.0")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

test_that("align_decimal_column handles est_spread values", {
  vals <- c("75.0 (6.75)", "74.0 (8.20)")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

# --- compute_all_decimal_geometry integration ---

test_that("compute_all_decimal_geometry returns NULL for non-decimal specs", {
  spec <- data.frame(a = "x", stringsAsFactors = FALSE) |> fr_table()
  spec <- finalize_spec(spec)
  expect_null(spec$decimal_geometry)
})

test_that("compute_all_decimal_geometry produces formatted vector and center_offset", {
  spec <- data.frame(a = c("12.3", "4.56"), stringsAsFactors = FALSE) |>
    fr_table() |>
    fr_cols(a = fr_col("A", align = "decimal", width = 2))
  spec <- finalize_spec(spec)
  expect_true("a" %in% names(spec$decimal_geometry))
  geom <- spec$decimal_geometry$a
  expect_equal(length(geom$formatted), 2L)
  expect_equal(length(geom$center_offset), 2L)
  expect_true(all(geom$center_offset >= 0L))
  expect_true(all(nchar(geom$formatted) == nchar(geom$formatted[1])))
})

# --- Family-aware type priority tests ---

test_that("align_decimal_column: n_pct dominates over n_only in mixed count column (disposition)", {
  # Disposition pattern: n_only values outnumber n_pct, but count family groups them
  vals <- c("45", "44 (97.8)", "1 (2.2)", "1 (2.2)", "0", "0", "0", "0")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  # All same width

  expect_true(all(nchars == nchars[1]))
  # "45" should be right-aligned within n field, not right-aligned to full width
  # The n_pct "44" and n_only "45" should have their n parts aligned
  trimmed_45 <- trimws(result[1])
  trimmed_44 <- sub("\\s*\\(.*", "", trimws(result[2]))
  # Both should right-justify at the same position
  pos_45 <- regexpr("\\d+", result[1])
  pos_44 <- regexpr("\\d+", result[2])
  expect_equal(
    attr(pos_45, "match.length") + as.integer(pos_45),
    attr(pos_44, "match.length") + as.integer(pos_44)
  )
})

test_that("align_decimal_column: est_spread dominates with family tie-breaking (vitals)", {
  # Vitals pattern: 4-way tie between n_only, est_spread, scalar_float, range_pair
  vals <- c("45", "136.8 (17.61)", "136.6", "86.5, 181.5")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
  # "45" should align with integer part of "136.8", not be right-aligned to full width
  # After trimming, "45" should not be at the far right
  trailing_spaces_45 <- nchar(result[1]) - nchar(sub(" +$", "", result[1]))
  expect_true(trailing_spaces_45 > 0L)
})

test_that("align_decimal_column: n_only in estimate column aligns to integer part", {
  vals <- c("45", "136.8 (17.61)", "75.2 (8.30)")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
  # "45" padded right (trailing spaces), not left-only
  trailing_spaces <- nchar(result[1]) - nchar(sub(" +$", "", result[1]))
  expect_true(trailing_spaces > 0L)
})

test_that("align_decimal_column: scalar_float in estimate column aligns decimals", {
  vals <- c("0.0", "-0.0 (1.47)", "0.3 (2.10)")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
  # "0.0" decimal should align with "-0.0" decimal
  dot_pos_float <- regexpr("\\.", result[1])
  dot_pos_est <- regexpr("\\.", result[2])
  expect_equal(as.integer(dot_pos_float), as.integer(dot_pos_est))
})

# --- Group-aware decimal geometry tests ---

test_that("compute_all_decimal_geometry aligns per group_by independently", {
  df <- data.frame(
    section = c("A", "A", "B", "B"),
    stat = c(
      "4 (8.9)",
      "41 (91.1)",
      "168.0 (152.4, 183.6)",
      "100.0 (90.0, 110.0)"
    ),
    stringsAsFactors = FALSE
  )
  spec <- df |>
    fr_table() |>
    fr_rows(group_by = "section") |>
    fr_cols(stat = fr_col("Stat", align = "decimal", width = 3))
  spec <- finalize_spec(spec)
  geom <- spec$decimal_geometry$stat
  expect_true(!is.null(geom))
  # group_by adds a blank_after row between groups, so 5 rows total
  expect_equal(length(geom$formatted), nrow(spec$data))
  # Group A (n_pct) rows should have parentheses, Group B (est_ci) should have comma
  grp_a <- geom$formatted[spec$data$section == "A"]
  grp_b <- geom$formatted[spec$data$section == "B"]
  expect_true(all(grepl("\\(", trimws(grp_a[nzchar(trimws(grp_a))]))))
  expect_true(all(grepl(",", trimws(grp_b[nzchar(trimws(grp_b))]))))
})

test_that("compute_all_decimal_geometry aligns per page_by independently", {
  df <- data.frame(
    param = c("SBP", "SBP", "DBP", "DBP"),
    stat = c("45", "136.8 (17.61)", "45", "80.2 (10.30)"),
    stringsAsFactors = FALSE
  )
  spec <- df |>
    fr_table() |>
    fr_cols(stat = fr_col("Stat", align = "decimal", width = 3))
  spec$body$page_by <- "param"
  spec <- finalize_spec(spec)
  geom <- spec$decimal_geometry$stat
  expect_true(!is.null(geom))
  expect_equal(length(geom$formatted), 4L)
  # center_offset is a per-row vector
  expect_equal(length(geom$center_offset), 4L)
  expect_true(all(geom$center_offset >= 0L))
  # Within each group, nchar should be uniform
  sbp <- geom$formatted[df$param == "SBP"]
  dbp <- geom$formatted[df$param == "DBP"]
  expect_true(all(nchar(sbp) == nchar(sbp[1])))
  expect_true(all(nchar(dbp) == nchar(dbp[1])))
})

test_that("group_by uses global alignment (groups share a page)", {
  df <- data.frame(
    section = c("A", "A", "B", "B"),
    stat = c(
      "4 (8.9)",
      "41 (91.1)",
      "168.0 (152.4, 183.6)",
      "100.0 (90.0, 110.0)"
    ),
    stringsAsFactors = FALSE
  )
  spec <- df |>
    fr_table() |>
    fr_rows(group_by = "section") |>
    fr_cols(stat = fr_col("Stat", align = "decimal", width = 3))
  spec <- finalize_spec(spec)
  geom <- spec$decimal_geometry$stat
  expect_equal(length(geom$center_offset), nrow(spec$data))
  # group_by does not split alignment — all rows share one global alignment
  # so center_offset is uniform across all rows
  expect_equal(length(unique(geom$center_offset)), 1L)
})

test_that("page_by orig_rows: page 2 uses correct global indices", {
  # Core bug: page 2 rows used local index i instead of global orig_rows[i]
  df <- data.frame(
    param = c("SBP", "SBP", "SBP", "DBP", "DBP", "DBP"),
    stat = c("45", "136.8 (17.61)", "136.6", "45", "80.2 (10.30)", "79.5"),
    stringsAsFactors = FALSE
  )
  spec <- df |>
    fr_table() |>
    fr_cols(stat = fr_col("Stat", align = "decimal", width = 3))
  spec$body$page_by <- "param"
  spec <- finalize_spec(spec)

  # Simulate what prepare_pages does
  pages <- prepare_pages(spec)
  expect_equal(length(pages), 2L)

  # Page 2 (DBP) should have orig_rows pointing to rows 4,5,6
  orig <- pages[[2]]$orig_rows
  expect_equal(orig, 4:6)

  # The formatted values at orig indices should match DBP content
  geom <- spec$decimal_geometry$stat
  dbp_formatted <- geom$formatted[orig]
  expect_true(all(nzchar(trimws(dbp_formatted))))
})

test_that("page_by with same stat types: smart global alignment", {
  # VS table: all params have same stat types (n_only + est_spread + scalar_float)
  # → smart check detects same patterns → global alignment → consistent widths
  df <- data.frame(
    param = c("SBP", "SBP", "SBP", "DBP", "DBP", "DBP"),
    stat = c("45", "136.8 (17.61)", "136.6", "45", "80.2 (10.30)", "79.5"),
    stringsAsFactors = FALSE
  )
  spec <- df |>
    fr_table() |>
    fr_cols(stat = fr_col("Stat", align = "decimal", width = 3))
  spec$body$page_by <- "param"
  spec <- finalize_spec(spec)
  geom <- spec$decimal_geometry$stat

  # Same stat type patterns → global alignment → uniform nchar
  expect_equal(
    length(unique(nchar(geom$formatted[nzchar(geom$formatted)]))),
    1L
  )
  # Uniform center_offset (all rows get same scalar offset)
  expect_equal(length(unique(geom$center_offset)), 1L)
})

test_that("page_by with different stat types: per-page alignment", {
  # Different type patterns per page → per-group alignment preserved
  df <- data.frame(
    param = c("Weight", "Weight", "Pulse", "Pulse"),
    stat = c("78.42 (12.35)", "80.10 (11.50)", "72 (8)", "68 (7)"),
    stringsAsFactors = FALSE
  )
  spec <- df |>
    fr_table() |>
    fr_cols(stat = fr_col("Stat", align = "decimal", width = 3))
  spec$body$page_by <- "param"
  spec <- finalize_spec(spec)
  geom <- spec$decimal_geometry$stat

  # Different decimal precision per page → per-group alignment
  wt <- geom$formatted[df$param == "Weight"]
  pl <- geom$formatted[df$param == "Pulse"]
  # Within each page, nchar is uniform
  expect_equal(length(unique(nchar(wt))), 1L)
  expect_equal(length(unique(nchar(pl))), 1L)
  # But nchar differs between pages (different precision)
  expect_false(nchar(wt[1]) == nchar(pl[1]))
})

test_that("AE table: no page_by, group_by=soc → global alignment", {
  # AE-style: group_by for SOC labels, but alignment should be global
  df <- data.frame(
    soc = c("SOC1", "SOC1", "SOC2", "SOC2"),
    stat = c("100 (100%)", "78 (50%)", "8 (3%)", "0"),
    stringsAsFactors = FALSE
  )
  spec <- df |>
    fr_table() |>
    fr_rows(group_by = "soc") |>
    fr_cols(stat = fr_col("Stat", align = "decimal", width = 3))
  spec <- finalize_spec(spec)
  geom <- spec$decimal_geometry$stat

  # Global alignment: uniform nchar and center_offset across all SOC groups
  non_blank <- nzchar(trimws(geom$formatted))
  expect_equal(length(unique(nchar(geom$formatted[non_blank]))), 1L)
  expect_equal(length(unique(geom$center_offset)), 1L)
})

test_that("demographics: no page_by, no group_by → global alignment", {
  # Demographics: mixed types, single global alignment
  df <- data.frame(
    stat = c("143", "75.7 (8.19)", "77.0", "54, 89"),
    stringsAsFactors = FALSE
  )
  spec <- df |>
    fr_table() |>
    fr_cols(stat = fr_col("Stat", align = "decimal", width = 3))
  spec <- finalize_spec(spec)
  geom <- spec$decimal_geometry$stat

  # All formatted values should be the same nchar (global alignment)
  expect_equal(length(unique(nchar(geom$formatted))), 1L)
  # Single center_offset value
  expect_equal(length(unique(geom$center_offset)), 1L)
})

test_that("range_pair aligns with estimate in est_spread-dominant column", {
  vals <- c("45", "136.8 (17.61)", "136.6", "86.5, 181.5")
  result <- align_decimal_column(vals)
  # range_pair left value decimal should align with est decimal
  est_dot <- regexpr("\\.", result[2])
  rng_dot <- regexpr("\\.", result[4])
  expect_equal(as.integer(est_dot), as.integer(rng_dot))
  # n_only should right-align to integer part
  expect_true(nchar(result[1]) == nchar(result[2]))
})


# --- New type detection tests ---

test_that("detect_stat_type identifies BLQ/INF/-INF as missing", {
  expect_equal(detect_stat_type("BLQ"), "missing")
  expect_equal(detect_stat_type("INF"), "missing")
  expect_equal(detect_stat_type("-INF"), "missing")
})

test_that("detect_stat_type identifies est_ci with missing tokens", {
  expect_equal(detect_stat_type("14.3 (11.2, NR)"), "est_ci")
  expect_equal(detect_stat_type("NR (NR, NR)"), "est_ci")
  expect_equal(detect_stat_type("0.087 (0.034, NR)"), "est_ci")
  expect_equal(detect_stat_type("3.120 (1.840, INF)"), "est_ci")
})

test_that("detect_stat_type identifies est_ci_bracket", {
  expect_equal(detect_stat_type("0.0 [0.0, 0.0]"), "est_ci_bracket")
  expect_equal(detect_stat_type("53.0 [45.0, 60.0]"), "est_ci_bracket")
  expect_equal(detect_stat_type("102.0 [88.4, 116.2]"), "est_ci_bracket")
})

test_that("detect_stat_type identifies est_spread_pct", {
  expect_equal(detect_stat_type("0.10 (8.7%)"), "est_spread_pct")
  expect_equal(detect_stat_type("52.43 (23.4%)"), "est_spread_pct")
  expect_equal(detect_stat_type("1240.40 (23.4%)"), "est_spread_pct")
})

test_that("detect_stat_type identifies n_over_N", {
  expect_equal(detect_stat_type("0/120"), "n_over_N")
  expect_equal(detect_stat_type("1/120"), "n_over_N")
  expect_equal(detect_stat_type("108/120"), "n_over_N")
})

test_that("detect_stat_type identifies n_over_float", {
  expect_equal(detect_stat_type("0/234.6"), "n_over_float")
  expect_equal(detect_stat_type("12/234.6"), "n_over_float")
  expect_equal(detect_stat_type("108/234.6"), "n_over_float")
})

test_that("detect_stat_type identifies est_ci_pval", {
  expect_equal(
    detect_stat_type("-0.08 (-0.21, 0.05) 0.194"),
    "est_ci_pval"
  )
  expect_equal(
    detect_stat_type("12.40 (9.80, 15.00) <0.001"),
    "est_ci_pval"
  )
})

test_that("detect_stat_type identifies n_pct_rate", {
  expect_equal(detect_stat_type("0 (0.0) 0.00"), "n_pct_rate")
  expect_equal(detect_stat_type("3 (2.5) 1.28"), "n_pct_rate")
  expect_equal(detect_stat_type("42 (35.0) 17.94"), "n_pct_rate")
})

test_that("detect_stat_type identifies n_over_N_pct_ci", {
  expect_equal(
    detect_stat_type("0/120 (0.0) [0.0, 3.0]"),
    "n_over_N_pct_ci"
  )
  expect_equal(
    detect_stat_type("12/120 (10.0) [5.6, 16.9]"),
    "n_over_N_pct_ci"
  )
})

test_that("detect_stat_type identifies est_spread_pct_ci", {
  expect_equal(
    detect_stat_type("8.1 (24.2%) (7.3, 8.9)"),
    "est_spread_pct_ci"
  )
  expect_equal(
    detect_stat_type("1240.4 (23.4%) (1124.2, 1368.8)"),
    "est_spread_pct_ci"
  )
})

# --- New type alignment tests ---

test_that("align_decimal_column: est_ci with missing tokens all same nchar", {
  vals <- c("14.3 (11.2, NR)", "0.087 (0.034, NR)", "NR (NR, NR)")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
  expect_true(nchars[1] > 0L)
})

test_that("align_decimal_column: est_ci_bracket values all same nchar", {
  vals <- c("0.0 [0.0, 0.0]", "53.0 [45.0, 60.0]", "102.0 [88.4, 116.2]")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

test_that("align_decimal_column: est_spread_pct values all same nchar", {
  vals <- c("0.10 (8.7%)", "52.43 (23.4%)", "1240.40 (23.4%)")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

test_that("align_decimal_column: n_over_N values all same nchar", {
  vals <- c("0/120", "1/120", "108/120")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

test_that("align_decimal_column: n_over_float values all same nchar", {
  vals <- c("0/234.6", "12/234.6", "108/234.6")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

test_that("align_decimal_column: est_ci_pval compound all same nchar", {
  vals <- c(
    "-0.08 (-0.21, 0.05) 0.194",
    "12.40 (9.80, 15.00) <0.001"
  )
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

test_that("align_decimal_column: n_pct_rate compound all same nchar", {
  vals <- c("0 (0.0) 0.00", "3 (2.5) 1.28", "42 (35.0) 17.94")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

test_that("align_decimal_column: n_over_N_pct_ci compound all same nchar", {
  vals <- c(
    "0/120 (0.0) [0.0, 3.0]",
    "12/120 (10.0) [5.6, 16.9]",
    "120/120 (100.0) [97.0, 100.0]"
  )
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

test_that("align_decimal_column: est_spread_pct_ci compound all same nchar", {
  vals <- c(
    "8.1 (24.2%) (7.3, 8.9)",
    "1240.4 (23.4%) (1124.2, 1368.8)"
  )
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

# --- Token-heavy compound and edge cases ---

test_that("align_decimal_column: est_ci_pval with NR tokens all same nchar", {
  vals <- c(
    "NR (NR, NR) -",
    "14.3 (11.2, NR) 0.194",
    "3.70 (1.24, 6.16) 0.003"
  )
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

test_that("align_decimal_column: pure est_spread_pct_ci (no mixing)", {
  vals <- c(
    "8.1 (24.2%) (7.3, 8.9)",
    "52.4 (18.1%) (48.2, 57.0)",
    "1240.4 (23.4%) (1124.2, 1368.8)"
  )
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

# --- Cross-type fallback: est_ci_bracket <-> est_ci ---

test_that("est_ci_bracket aligns with est_ci dominant: delimiters swap", {
  vals <- c("5.0 [3.0, 7.0]", "10.0 (8.0, 12.0)", "15.0 (13.0, 17.0)")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

# --- Cross-type fallback: n_over_N in n_over_N_pct ---

test_that("n_over_N aligns in n_over_N_pct dominant column", {
  vals <- c("0/120", "3/45 (6.7)", "10/45 (22.2)")
  result <- align_decimal_column(vals)
  nchars <- nchar(result)
  expect_true(all(nchars == nchars[1]))
})

# --- Phase 1: New pattern detection tests ---

test_that("detect_stat_type recognizes triple-dash as missing", {
  expect_equal(detect_stat_type("---"), "missing")
  expect_equal(detect_stat_type("--"), "missing")
})

test_that("detect_stat_type recognizes double em-dash as missing", {
  expect_equal(detect_stat_type("\u2014\u2014"), "missing")
})

test_that("detect_stat_type recognizes = prefix as pvalue", {
  expect_equal(detect_stat_type("=0.500"), "pvalue")
})

test_that("parse_stat_value parses = pvalue correctly", {
  p <- parse_stat_value("=0.500", "pvalue")
  expect_equal(p$prefix, "=")
  expect_equal(p$int, "0")
  expect_equal(p$dec, "500")
})


# --- Cross-type fallback tests ---

test_that("pvalue aligns with scalar_float: decimal dots align", {
  vals <- c("<0.001", "12.34", "5.67")
  result <- align_decimal_column(vals)
  expect_equal(nchar(result[1]), nchar(result[2]))
  # Decimal dots should align
  dot1 <- regexpr("\\.", result[1])
  dot2 <- regexpr("\\.", result[2])
  expect_equal(as.integer(dot1), as.integer(dot2))
})

test_that("pvalue aligns with est_spread: pvalue decimal aligns with est decimal", {
  vals <- c("<0.050", "136.8 (17.61)")
  result <- align_decimal_column(vals)
  expect_equal(nchar(result[1]), nchar(result[2]))
  # First dot in pvalue should align with est decimal dot
  pv_dot <- regexpr("\\.", result[1])
  est_dot <- regexpr("\\.", result[2])
  expect_equal(as.integer(pv_dot), as.integer(est_dot))
})

test_that("n_pct aligns with n_over_N_pct: parentheses align", {
  vals <- c("3 (6.7)", "3/45 (6.7)", "10/45 (22.2)")
  result <- align_decimal_column(vals)
  expect_equal(nchar(result[1]), nchar(result[2]))
  # Opening parentheses should align
  paren1 <- regexpr("\\(", result[1])
  paren2 <- regexpr("\\(", result[2])
  expect_equal(as.integer(paren1), as.integer(paren2))
})

test_that("est_spread aligns with est_ci: estimate decimals align", {
  vals <- c("75.0 (6.75)", "168.0 (152.4, 183.6)")
  result <- align_decimal_column(vals)
  expect_equal(nchar(result[1]), nchar(result[2]))
  # Estimate decimal dots should align
  dot1 <- regexpr("\\.", result[1])
  dot2 <- regexpr("\\.", result[2])
  expect_equal(as.integer(dot1), as.integer(dot2))
})


# --- Fill existing test gaps ---

test_that("n_only in n_over_N_pct column aligns to numerator", {
  vals <- c("45", "3/45 (6.7)", "10/45 (22.2)")
  result <- align_decimal_column(vals)
  expect_equal(nchar(result[1]), nchar(result[2]))
})

test_that("n_only in pvalue column aligns to integer part", {
  vals <- c("5", "<0.001", ">0.999")
  result <- align_decimal_column(vals)
  expect_equal(nchar(result[1]), nchar(result[2]))
})

test_that("n_only in range_pair column aligns to left-value integer part", {
  vals <- c("45", "2.0, 45.0", "65.0, 88.0")
  result <- align_decimal_column(vals)
  expect_equal(nchar(result[1]), nchar(result[2]))
})

test_that("n_only in int_range column aligns to left integer", {
  vals <- c("5", "10 - 365", "1 - 180")
  result <- align_decimal_column(vals)
  expect_equal(nchar(result[1]), nchar(result[2]))
})

test_that("scalar_float in est_ci column aligns decimal with estimate", {
  vals <- c("5.67", "168.0 (152.4, 183.6)")
  result <- align_decimal_column(vals)
  expect_equal(nchar(result[1]), nchar(result[2]))
  dot1 <- regexpr("\\.", result[1])
  dot2 <- regexpr("\\.", result[2])
  expect_equal(as.integer(dot1), as.integer(dot2))
})


# --- Edge case tests ---

test_that("all-missing column returns all empty strings", {
  vals <- c("", "NA", "---")
  result <- align_decimal_column(vals)
  expect_true(all(result == ""))
})

test_that("compute_stat_widths safety net: non-zero width when typed is empty", {
  parsed <- list(
    list(type = "n_only", n = "42", raw = "42"),
    list(
      type = "n_pct",
      n = "3",
      pct_prefix = "",
      pct_int = "6",
      pct_dec = "7",
      pct_sign = "%",
      raw = "3 (6.7%)"
    )
  )
  # Ask for widths of a type that doesn't exist in parsed_values
  widths <- compute_stat_widths(parsed, "est_spread")
  # Should have non-zero full_width from raw values

  expect_true(widths$full_width > 0L)
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
  data <- data.frame(a = c("x", "y"), b = c("1", "2"), stringsAsFactors = FALSE)
  cols <- list(
    a = fr_col("A", width = 1, align = "left"),
    b = fr_col("B", width = 1, align = "right")
  )
  cols$a$id <- "a"
  cols$b$id <- "b"
  page <- new_fr_page()

  style <- new_fr_cell_style(
    region = "body",
    type = "cell",
    rows = 1L,
    cols = "a",
    bold = TRUE,
    italic = TRUE,
    fg = "#FF0000",
    bg = "#00FF00",
    font_size = 12,
    align = "center",
    valign = "middle",
    underline = TRUE,
    indent = 0.5
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

  style <- new_fr_cell_style(region = "header", type = "cell", bold = TRUE)
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

  style <- new_fr_cell_style(region = "stub", type = "col", bold = TRUE)
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
  style <- new_fr_cell_style(
    region = "body",
    type = "cell",
    cols = 2L,
    rows = "all"
  )
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
  style <- new_fr_cell_style(
    region = "body",
    type = "cell",
    cols = "b",
    rows = "all"
  )
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
  style <- new_fr_cell_style(
    region = "body",
    type = "row",
    rows = 1L,
    cols = "a"
  )
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
  style <- new_fr_cell_style(region = "body", type = "cell", rows = c(1L, 3L))
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
  style <- new_fr_cell_style(
    region = "header",
    type = "cell",
    cols = "a",
    bold = TRUE,
    fg = "#FF0000"
  )
  result <- apply_styles_to_grid(
    grid,
    list(style),
    "header",
    c("a", "b"),
    header_row_idx = 1L
  )
  expect_true(result$bold[1])
  expect_false(result$bold[2])
  expect_equal(result$fg[1], "#FF0000")
})

test_that("apply_styles_to_grid skips non-matching region", {
  grid <- data.frame(
    col_idx = 1L,
    col_name = "a",
    align = "left",
    valign = "top",
    bold = FALSE,
    italic = FALSE,
    underline = FALSE,
    fg = "#000000",
    bg = NA_character_,
    font_size = 9,
    stringsAsFactors = FALSE
  )
  style <- new_fr_cell_style(region = "body", type = "cell", bold = TRUE)
  result <- apply_styles_to_grid(
    grid,
    list(style),
    "header",
    "a",
    header_row_idx = 1L
  )
  expect_false(result$bold[1])
})

test_that("apply_styles_to_grid skips header style with non-matching row_idx", {
  grid <- data.frame(
    col_idx = 1L,
    col_name = "a",
    align = "left",
    valign = "top",
    bold = FALSE,
    italic = FALSE,
    underline = FALSE,
    fg = "#000000",
    bg = NA_character_,
    font_size = 9,
    stringsAsFactors = FALSE
  )
  style <- new_fr_cell_style(
    region = "header",
    type = "cell",
    rows = 3L,
    bold = TRUE
  )
  result <- apply_styles_to_grid(
    grid,
    list(style),
    "header",
    "a",
    header_row_idx = 1L
  )
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
  style <- new_fr_cell_style(
    region = "header",
    type = "cell",
    cols = 2L,
    italic = TRUE
  )
  result <- apply_styles_to_grid(
    grid,
    list(style),
    "header",
    c("a", "b"),
    header_row_idx = 1L
  )
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
  style <- new_fr_cell_style(
    region = "body",
    type = "cell",
    rows = 2L,
    indent = 0.25
  )
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
  hcfg <- new_fr_header(
    bold = TRUE,
    fg = "#0000FF",
    bg = "#FFFF00",
    font_size = 14
  )

  grid <- build_header_cell_grid(
    cols,
    list(),
    page,
    header_row_idx = 1L,
    header_cfg = hcfg
  )
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

  style <- new_fr_cell_style(
    region = "header",
    type = "cell",
    cols = "b",
    bold = TRUE,
    bg = "#CCCCCC"
  )
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

  style <- new_fr_cell_style(
    region = "header",
    type = "cell",
    rows = 5L,
    bold = TRUE
  )
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

  style <- new_fr_cell_style(
    region = "header",
    type = "cell",
    cols = 1L,
    italic = TRUE
  )
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

  style <- new_fr_cell_style(region = "header", type = "row", bold = TRUE)
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
  style <- new_fr_cell_style(
    type = "row",
    region = "body",
    rows = c(2L, 4L),
    height = 0.5
  )
  result <- build_row_heights(5L, list(style))
  expect_true(is.na(result[1]))
  expect_equal(result[2], 0.5)
  expect_true(is.na(result[3]))
  expect_equal(result[4], 0.5)
})

test_that("build_row_heights applies height to all rows", {
  style <- new_fr_cell_style(
    type = "row",
    region = "body",
    rows = "all",
    height = 0.3
  )
  result <- build_row_heights(5L, list(style))
  expect_equal(result, rep(0.3, 5L))
})

test_that("build_row_heights with NULL rows applies to all", {
  style <- new_fr_cell_style(
    type = "row",
    region = "body",
    rows = NULL,
    height = 0.4
  )
  result <- build_row_heights(5L, list(style))
  expect_equal(result, rep(0.4, 5L))
})

test_that("build_row_heights skips non-row type styles", {
  style <- new_fr_cell_style(
    type = "cell",
    region = "body",
    rows = 1L,
    height = 0.5
  )
  result <- build_row_heights(3L, list(style))
  expect_equal(result, rep(NA_real_, 3L))
})

test_that("build_row_heights skips styles without height", {
  style <- new_fr_cell_style(
    type = "row",
    region = "body",
    rows = 1L,
    bold = TRUE
  )
  result <- build_row_heights(3L, list(style))
  expect_equal(result, rep(NA_real_, 3L))
})

test_that("build_row_heights clips out-of-bounds rows", {
  style <- new_fr_cell_style(
    type = "row",
    region = "body",
    rows = c(1L, 10L),
    height = 0.5
  )
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
  data <- data.frame(
    grp = c("A", "A"),
    val = c("x", "y"),
    stringsAsFactors = FALSE
  )
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
    grp = c("A", "A", "", "B", "B"),
    val = c("1", "2", "", "3", "4"),
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

test_that("build_keep_mask keeps entire group when it fits on page", {
  # Group with 10 rows, default page_rows = Inf → all fit
  data <- data.frame(
    grp = rep("A", 10L),
    val = as.character(seq_len(10L)),
    stringsAsFactors = FALSE
  )
  mask <- build_keep_mask(data, "grp", orphan_min = 3L, widow_min = 3L)
  # All rows except last get keepn (full chain)
  expect_true(all(mask[1:9]))
  expect_false(mask[10]) # Last row: no next row to keep with
})

test_that("build_keep_mask uses orphan/widow when group exceeds page", {
  # Group with 10 rows, page_rows = 5 → group doesn't fit
  data <- data.frame(
    grp = rep("A", 10L),
    val = as.character(seq_len(10L)),
    stringsAsFactors = FALSE
  )
  mask <- build_keep_mask(
    data,
    "grp",
    orphan_min = 3L,
    widow_min = 3L,
    page_rows = 5L
  )
  # Top: rows 1-2 keep-with-next (orphan_min - 1 = 2 rows)
  expect_true(mask[1])
  expect_true(mask[2])
  expect_false(mask[3]) # Middle: free to split
  expect_false(mask[4])
  # Bottom: rows 8-9 keep-with-next (widow_min = 3 → last 3 glued)
  expect_true(mask[8])
  expect_true(mask[9])
  expect_false(mask[10]) # Last row: no next row to keep with
})

test_that("build_keep_mask respects custom orphan_min/widow_min on oversized group", {
  data <- data.frame(
    grp = rep("A", 12L),
    val = as.character(seq_len(12L)),
    stringsAsFactors = FALSE
  )
  mask <- build_keep_mask(
    data,
    "grp",
    orphan_min = 4L,
    widow_min = 2L,
    page_rows = 8L
  )
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
  data <- data.frame(
    grp = c("A", "B"),
    val = c("1", "2"),
    stringsAsFactors = FALSE
  )
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
    new_fr_cell_style(
      region = "body",
      type = "row",
      rows = c(1L, 3L),
      bold = TRUE
    ),
    new_fr_cell_style(
      region = "body",
      type = "col",
      rows = c(2L, 4L),
      cols = "a",
      italic = TRUE
    )
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
    new_fr_cell_style(
      region = "body",
      type = "row",
      rows = c(1L, 2L, 3L, 4L, 5L),
      bold = TRUE
    )
  )
  # Blanks after rows 2 and 4 in original data
  result <- remap_style_indices(styles, c(2L, 4L))
  # Row 1 → 1, 2 → 2, 3 → 4, 4 → 5, 5 → 7
  expect_equal(result[[1]]$rows, c(1L, 2L, 4L, 5L, 7L))
})

test_that("remap_style_indices skips 'all' and NULL rows", {
  styles <- list(
    new_fr_cell_style(
      region = "body",
      type = "col",
      rows = "all",
      cols = "a",
      bold = TRUE
    ),
    new_fr_cell_style(
      region = "body",
      type = "col",
      rows = NULL,
      cols = "b",
      italic = TRUE
    )
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


# ── apply_leading_indent ─────────────────────────────────────────────────────

test_that("apply_leading_indent strips spaces and creates indent styles", {
  spec <- new_fr_spec(data.frame(
    a = c("  indented", "normal", "   deep"),
    stringsAsFactors = FALSE
  ))
  spec$columns <- build_default_columns(
    spec$data,
    list(),
    NULL,
    "auto",
    NULL,
    NULL,
    NULL,
    spec$page
  )
  spec$columns_meta$spaces <- "indent"
  result <- apply_leading_indent(spec)
  # Data should have spaces stripped

  expect_equal(result$data$a[1], "indented")
  expect_equal(result$data$a[2], "normal")
  expect_equal(result$data$a[3], "deep")
  # Should have 2 indent levels (2 spaces and 3 spaces)
  expect_length(result$cell_styles, 2L)
  expect_true(result$cell_styles[[1]]$indent > 0)
  expect_true(result$cell_styles[[2]]$indent > result$cell_styles[[1]]$indent)
})

test_that("apply_leading_indent skips preserve mode columns", {
  spec <- new_fr_spec(data.frame(
    a = c("  indented", "normal"),
    stringsAsFactors = FALSE
  ))
  spec$columns <- build_default_columns(
    spec$data,
    list(),
    NULL,
    "auto",
    NULL,
    NULL,
    NULL,
    spec$page
  )
  spec$columns$a$spaces <- "preserve"
  spec$columns_meta$spaces <- "indent"
  result <- apply_leading_indent(spec)
  # Data should NOT have spaces stripped
  expect_equal(result$data$a[1], "  indented")
  expect_length(result$cell_styles, 0L)
})

test_that("apply_leading_indent skips columns with no leading spaces", {
  spec <- new_fr_spec(data.frame(
    a = c("no spaces", "here"),
    stringsAsFactors = FALSE
  ))
  spec$columns <- build_default_columns(
    spec$data,
    list(),
    NULL,
    "auto",
    NULL,
    NULL,
    NULL,
    spec$page
  )
  spec$columns_meta$spaces <- "indent"
  result <- apply_leading_indent(spec)
  expect_length(result$cell_styles, 0L)
})

test_that("apply_leading_indent handles empty data", {
  spec <- new_fr_spec(data.frame(
    a = character(0),
    stringsAsFactors = FALSE
  ))
  spec$columns <- build_default_columns(
    spec$data,
    list(),
    NULL,
    "auto",
    NULL,
    NULL,
    NULL,
    spec$page
  )
  spec$columns_meta$spaces <- "indent"
  result <- apply_leading_indent(spec)
  expect_length(result$cell_styles, 0L)
})

test_that("apply_leading_indent global preserve mode skips all columns", {
  spec <- new_fr_spec(data.frame(
    a = c("  indented", "normal"),
    stringsAsFactors = FALSE
  ))
  spec$columns <- build_default_columns(
    spec$data,
    list(),
    NULL,
    "auto",
    NULL,
    NULL,
    NULL,
    spec$page
  )
  spec$columns_meta$spaces <- "preserve"
  result <- apply_leading_indent(spec)
  expect_equal(result$data$a[1], "  indented")
  expect_length(result$cell_styles, 0L)
})


# ── resolve_borders: vlines ──────────────────────────────────────────────────

test_that("resolve_borders handles vline preset 'all'", {
  vline <- structure(
    list(
      preset = "all",
      cols = NULL,
      width = 0.5,
      linestyle = "solid",
      fg = "#000000"
    ),
    class = c("fr_vline_spec", "fr_rule")
  )
  borders <- resolve_borders(
    list(vline),
    nrow_body = 2L,
    ncol = 3L,
    nrow_header = 1L
  )
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
    list(
      preset = "inner",
      cols = NULL,
      width = 0.5,
      linestyle = "solid",
      fg = "#000000"
    ),
    class = c("fr_vline_spec", "fr_rule")
  )
  borders <- resolve_borders(
    list(vline),
    nrow_body = 2L,
    ncol = 3L,
    nrow_header = 1L
  )
  # Inner borders should be set
  expect_false(is.null(borders$header$right[1, 1][[1]]))
  expect_false(is.null(borders$header$left[1, 2][[1]]))
  # Outer edges should NOT be set
  expect_true(is.null(borders$header$left[1, 1][[1]]))
  expect_true(is.null(borders$header$right[1, 3][[1]]))
})

test_that("resolve_borders handles vline preset 'box'", {
  vline <- structure(
    list(
      preset = "box",
      cols = NULL,
      width = 0.5,
      linestyle = "solid",
      fg = "#000000"
    ),
    class = c("fr_vline_spec", "fr_rule")
  )
  borders <- resolve_borders(
    list(vline),
    nrow_body = 2L,
    ncol = 3L,
    nrow_header = 1L
  )
  # Outer edges should be set
  expect_false(is.null(borders$header$left[1, 1][[1]]))
  expect_false(is.null(borders$header$right[1, 3][[1]]))
  # Inner borders should NOT be set
  expect_true(is.null(borders$header$right[1, 1][[1]]))
})

test_that("resolve_borders handles vline with specific cols", {
  vline <- structure(
    list(
      preset = NULL,
      cols = 1L,
      width = 0.5,
      linestyle = "solid",
      fg = "#000000"
    ),
    class = c("fr_vline_spec", "fr_rule")
  )
  borders <- resolve_borders(
    list(vline),
    nrow_body = 2L,
    ncol = 3L,
    nrow_header = 1L
  )
  # Gap between col 1 and 2
  expect_false(is.null(borders$header$right[1, 1][[1]]))
  expect_false(is.null(borders$header$left[1, 2][[1]]))
  # No border between col 2 and 3
  expect_true(is.null(borders$header$right[1, 2][[1]]))
})


# ── resolve_borders: body rows ───────────────────────────────────────────────

test_that("resolve_borders handles body below with rows='all'", {
  rule <- new_fr_rule(
    direction = "horizontal",
    region = "body",
    side = "below",
    rows = "all",
    width = 0.5,
    linestyle = "solid",
    fg = "#000000"
  )
  borders <- resolve_borders(list(rule), nrow_body = 3L, ncol = 2L)
  for (i in 1:3) {
    expect_false(is.null(borders$body$bottom[i, 1][[1]]))
  }
})

test_that("resolve_borders handles body below with specific rows", {
  rule <- new_fr_rule(
    direction = "horizontal",
    region = "body",
    side = "below",
    rows = c(1L, 3L),
    width = 0.5,
    linestyle = "solid",
    fg = "#000000"
  )
  borders <- resolve_borders(list(rule), nrow_body = 3L, ncol = 2L)
  expect_false(is.null(borders$body$bottom[1, 1][[1]]))
  expect_true(is.null(borders$body$bottom[2, 1][[1]]))
  expect_false(is.null(borders$body$bottom[3, 1][[1]]))
})

test_that("resolve_borders handles body above", {
  rule <- new_fr_rule(
    direction = "horizontal",
    region = "body",
    side = "above",
    width = 0.5,
    linestyle = "solid",
    fg = "#000000"
  )
  borders <- resolve_borders(list(rule), nrow_body = 3L, ncol = 2L)
  expect_false(is.null(borders$body$top[1, 1][[1]]))
  expect_true(is.null(borders$body$top[2, 1][[1]]))
})

test_that("resolve_borders handles header above", {
  rule <- new_fr_rule(
    direction = "horizontal",
    region = "header",
    side = "above",
    width = 0.5,
    linestyle = "solid",
    fg = "#000000"
  )
  borders <- resolve_borders(list(rule), nrow_body = 2L, ncol = 2L)
  expect_false(is.null(borders$header$top[1, 1][[1]]))
})

test_that("resolve_borders skips non-horizontal fr_rule", {
  rule <- new_fr_rule(direction = "vertical", region = "header", side = "below")
  borders <- resolve_borders(list(rule), nrow_body = 2L, ncol = 2L)
  # Should be all NULL
  expect_true(is.null(borders$header$bottom[1, 1][[1]]))
})

test_that("resolve_borders handles zero body rows", {
  rule <- new_fr_rule(
    direction = "horizontal",
    region = "body",
    side = "below",
    fg = "#000000"
  )
  borders <- resolve_borders(list(rule), nrow_body = 0L, ncol = 2L)
  # Should not error; body matrices have 0 rows (empty table early return)
  expect_equal(nrow(borders$body$bottom), 0L)
})

test_that("resolve_borders clips out-of-bound row indices", {
  rule <- new_fr_rule(
    direction = "horizontal",
    region = "body",
    side = "below",
    rows = c(1L, 100L),
    width = 0.5,
    linestyle = "solid",
    fg = "#000000"
  )
  borders <- resolve_borders(list(rule), nrow_body = 3L, ncol = 2L)
  expect_false(is.null(borders$body$bottom[1, 1][[1]]))
  # Row 100 is out of bounds, should not crash
  expect_true(is.null(borders$body$bottom[3, 1][[1]]))
})


# ── LaTeX sentinel resolver ─────────────────────────────────────────────────

test_that("latex_sentinel_resolver handles all types", {
  expect_equal(latex_sentinel_resolver("SUPER", "a"), "\\textsuperscript{a}")
  expect_equal(latex_sentinel_resolver("SUB", "x"), "\\textsubscript{x}")
  expect_equal(latex_sentinel_resolver("BOLD", "hi"), "\\textbf{hi}")
  expect_equal(latex_sentinel_resolver("ITALIC", "em"), "\\textit{em}")
  expect_equal(latex_sentinel_resolver("UNDERLINE", "u"), "\\underline{u}")
  expect_equal(latex_sentinel_resolver("NEWLINE", ""), "\\\\")
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
  result <- latex_encode_unicode_char("\u4e16") # CJK character
  expect_equal(result, "\u4e16")
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


# ── newline_to_rtf_line ──────────────────────────────────────────────────────

test_that("newline_to_rtf_line preserves leading spaces as \\~", {
  # No spaces — standard \line

  expect_equal(newline_to_rtf_line("A\nB"), "A\\line B")
  # Leading spaces become \~
  expect_equal(newline_to_rtf_line("SOC\n  PT"), "SOC\\line \\~\\~PT")
  # Single space
  expect_equal(newline_to_rtf_line("X\n Y"), "X\\line \\~Y")
  # No newline — pass-through
  expect_equal(newline_to_rtf_line("hello"), "hello")
  # Multiple lines
  expect_equal(newline_to_rtf_line("A\n B\n  C"), "A\\line \\~B\\line \\~\\~C")
})

test_that("newline_to_rtf_line is vectorized", {
  result <- newline_to_rtf_line(c("A\n  B", "C\nD"))
  expect_equal(result, c("A\\line \\~\\~B", "C\\line D"))
})


# ── newline_to_latex_break ───────────────────────────────────────────────────

test_that("newline_to_latex_break preserves leading spaces as ~", {
  expect_equal(newline_to_latex_break("A\nB"), "A \\\\ B")
  expect_equal(newline_to_latex_break("SOC\n  PT"), "SOC \\\\ ~~PT")
  expect_equal(newline_to_latex_break("hello"), "hello")
})

test_that("newline_to_latex_break is vectorized", {
  result <- newline_to_latex_break(c("A\n  B", "C\nD"))
  expect_equal(result, c("A \\\\ ~~B", "C \\\\ D"))
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_emdash / fr_endash — sentinel output
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_emdash returns a UNICODE sentinel for U+2014", {
  m <- fr_emdash()
  expect_s3_class(m, "fr_markup")
  expect_equal(m$type, "UNICODE")
  expect_equal(m$content, "\u2014")

  s <- format(m)
  expect_true(grepl("\x01", s, fixed = TRUE))
  expect_true(grepl("\x02", s, fixed = TRUE))
  expect_true(grepl("UNICODE", s, fixed = TRUE))
})

test_that("fr_endash returns a UNICODE sentinel for U+2013", {
  m <- fr_endash()
  expect_s3_class(m, "fr_markup")
  expect_equal(m$type, "UNICODE")
  expect_equal(m$content, "\u2013")

  s <- format(m)
  expect_true(grepl("\x01", s, fixed = TRUE))
  expect_true(grepl("\x02", s, fixed = TRUE))
  expect_true(grepl("UNICODE", s, fixed = TRUE))
})

test_that("fr_emdash and fr_endash produce distinct sentinels", {
  expect_false(identical(format(fr_emdash()), format(fr_endash())))
})
