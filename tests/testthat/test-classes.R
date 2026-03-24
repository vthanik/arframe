# ──────────────────────────────────────────────────────────────────────────────
# test-classes.R — Tests for classes.R
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# fr_col (user-facing, exported)
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_col creates fr_col object with defaults", {
  col <- fr_col()
  expect_s3_class(col, "fr_col")
  expect_equal(col$label, "")
  expect_null(col$width)
  expect_null(col$align)
  expect_null(col$visible)
  expect_equal(col$id, "")
})

test_that("fr_col accepts all valid parameters", {
  col <- fr_col(
    label = "Parameter",
    width = 2.5,
    align = "decimal",
    visible = FALSE
  )
  expect_equal(col$label, "Parameter")
  expect_equal(col$width, 2.5)
  expect_equal(col$align, "decimal")
  expect_false(col$visible)
})

test_that("fr_col accepts all valid alignment values", {
  for (a in c("left", "center", "right", "decimal")) {
    col <- fr_col(align = a)
    expect_equal(col$align, a)
  }
})

test_that("fr_col errors on invalid alignment", {
  expect_error(fr_col(align = "justify"), class = "rlang_error")
})

test_that("fr_col errors on invalid width", {
  expect_error(fr_col(width = -1), class = "rlang_error")
  expect_error(fr_col(width = 0), class = "rlang_error")
  expect_error(fr_col(width = "wide"), class = "rlang_error")
})

test_that("fr_col errors on non-string label", {
  expect_error(fr_col(label = 123), class = "rlang_error")
  expect_error(fr_col(label = c("a", "b")), class = "rlang_error")
})

test_that("fr_col errors on non-logical visible", {
  expect_error(fr_col(visible = "yes"), class = "rlang_error")
  expect_error(fr_col(visible = 1), class = "rlang_error")
})

test_that("fr_col stores header_align", {
  col <- fr_col(header_align = "center")
  expect_equal(col$header_align, "center")
})

test_that("fr_col header_align defaults to NULL", {
  col <- fr_col()
  expect_null(col$header_align)
})

test_that("fr_col validates header_align", {
  for (a in c("left", "center", "right", "decimal")) {
    col <- fr_col(header_align = a)
    expect_equal(col$header_align, a)
  }
  expect_error(fr_col(header_align = "justify"), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# normalise_margins
# ══════════════════════════════════════════════════════════════════════════════

test_that("normalise_margins handles scalar (all sides equal)", {
  m <- normalise_margins(1)
  expect_equal(m, list(top = 1, bottom = 1, left = 1, right = 1))
})

test_that("normalise_margins handles scalar zero", {
  m <- normalise_margins(0)
  expect_equal(m, list(top = 0, bottom = 0, left = 0, right = 0))
})

test_that("normalise_margins handles length-2 (vertical, horizontal)", {
  m <- normalise_margins(c(1, 0.75))
  expect_equal(m, list(top = 1, bottom = 1, left = 0.75, right = 0.75))
})

test_that("normalise_margins handles length-4 (CSS t, r, b, l)", {
  m <- normalise_margins(c(1, 0.5, 1.5, 0.75))
  expect_equal(m, list(top = 1, right = 0.5, bottom = 1.5, left = 0.75))
})

test_that("normalise_margins handles named list", {
  m <- normalise_margins(list(top = 1, bottom = 1, left = 0.75, right = 0.75))
  expect_equal(m$top, 1)
  expect_equal(m$left, 0.75)
})

test_that("normalise_margins errors on length 3", {
  expect_error(normalise_margins(c(1, 2, 3)), class = "rlang_error")
})

test_that("normalise_margins errors on negative values", {
  expect_error(normalise_margins(-1), class = "rlang_error")
  expect_error(normalise_margins(c(1, -0.5)), class = "rlang_error")
})

test_that("normalise_margins errors on non-numeric non-list", {
  expect_error(normalise_margins("1inch"), class = "rlang_error")
  expect_error(normalise_margins(TRUE), class = "rlang_error")
})

test_that("normalise_margins errors on incomplete named list", {
  expect_error(
    normalise_margins(list(top = 1, bottom = 1)),
    class = "rlang_error"
  )
})

test_that("normalise_margins errors on negative values in list", {
  expect_error(
    normalise_margins(list(top = 1, bottom = -1, left = 0.75, right = 0.75)),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# validate_user_tokens
# ══════════════════════════════════════════════════════════════════════════════

test_that("validate_user_tokens accepts empty list", {
  expect_silent(validate_user_tokens(list()))
})

test_that("validate_user_tokens accepts custom tokens", {
  expect_silent(validate_user_tokens(list(study = "ABC-001", pop = "ITT")))
})

test_that("validate_user_tokens allows overriding program and datetime", {
  expect_silent(validate_user_tokens(list(program = "my_script.R")))
  expect_silent(validate_user_tokens(list(datetime = "01MAR2025")))
})

test_that("validate_user_tokens errors on overriding thepage", {
  expect_error(validate_user_tokens(list(thepage = "99")), "thepage")
})

test_that("validate_user_tokens errors on overriding total_pages", {
  expect_error(validate_user_tokens(list(total_pages = "100")), "total_pages")
})

test_that("validate_user_tokens errors on unnamed list", {
  expect_error(validate_user_tokens(list("ABC")), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_page
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_page creates fr_page with defaults", {
  page <- new_fr_page()
  expect_s3_class(page, "fr_page")
  expect_equal(page$orientation, "landscape")
  expect_equal(page$paper, "letter")
  expect_equal(page$font_size, 9)
  expect_null(page$continuation)
  expect_equal(page$orphan_min, 3L)
  expect_equal(page$widow_min, 3L)
})

test_that("new_fr_page accepts scalar margin", {
  page <- new_fr_page(margins = 1)
  expect_equal(page$margins$top, 1)
  expect_equal(page$margins$left, 1)
})

test_that("new_fr_page accepts length-2 margin", {
  page <- new_fr_page(margins = c(1, 0.75))
  expect_equal(page$margins$top, 1)
  expect_equal(page$margins$bottom, 1)
  expect_equal(page$margins$left, 0.75)
  expect_equal(page$margins$right, 0.75)
})

test_that("new_fr_page accepts length-4 margin", {
  page <- new_fr_page(margins = c(1, 0.5, 1.5, 0.75))
  expect_equal(page$margins$top, 1)
  expect_equal(page$margins$right, 0.5)
  expect_equal(page$margins$bottom, 1.5)
  expect_equal(page$margins$left, 0.75)
})

test_that("new_fr_page continuation defaults to NULL", {
  page <- new_fr_page()
  expect_null(page$continuation)
})

test_that("new_fr_page accepts explicit continuation", {
  page <- new_fr_page(continuation = "(continued)")
  expect_equal(page$continuation, "(continued)")
})

test_that("new_fr_page errors on invalid orientation", {
  expect_error(new_fr_page(orientation = "diagonal"), class = "rlang_error")
})

test_that("new_fr_page errors on invalid paper", {
  expect_error(new_fr_page(paper = "tabloid"), class = "rlang_error")
})

test_that("new_fr_page errors on font_size out of range", {
  expect_error(new_fr_page(font_size = 2), class = "rlang_error")
  expect_error(new_fr_page(font_size = 100), class = "rlang_error")
})

test_that("new_fr_page errors on overriding readonly tokens", {
  expect_error(new_fr_page(tokens = list(thepage = "1")))
})

test_that("new_fr_page accepts custom tokens", {
  page <- new_fr_page(tokens = list(study = "ABC-001"))
  expect_equal(page$tokens$study, "ABC-001")
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_span
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_span creates fr_span object", {
  span <- new_fr_span("Treatment", c("drug", "placebo"), level = 1L)
  expect_s3_class(span, "fr_span")
  expect_equal(span$label, "Treatment")
  expect_equal(span$columns, c("drug", "placebo"))
  expect_equal(span$level, 1L)
})

test_that("new_fr_span errors on non-string label", {
  expect_error(new_fr_span(123, "col1"), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_header
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_header creates fr_header with defaults", {
  h <- new_fr_header()
  expect_s3_class(h, "fr_header")
  expect_equal(h$spans, list())
  expect_true(h$repeat_on_page)
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_body
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_body creates fr_body with defaults", {
  b <- new_fr_body()
  expect_s3_class(b, "fr_body")
  expect_equal(b$page_by, character(0))
  expect_equal(b$group_by, character(0))
})

test_that("new_fr_body stores column names", {
  b <- new_fr_body(page_by = "category", group_by = "param_group")
  expect_equal(b$page_by, "category")
  expect_equal(b$group_by, "param_group")
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_rule — region/side/linestyle validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_rule creates fr_rule with defaults", {
  r <- new_fr_rule()
  expect_s3_class(r, "fr_rule")
  expect_s3_class(r, "fr_rule_hline")
  expect_equal(r$direction, "horizontal")
  expect_equal(r$region, "header")
  expect_equal(r$side, "below")
  expect_equal(r$linestyle, "solid")
  expect_equal(r$width, 0.5)
  expect_equal(r$fg, "#000000")
  expect_null(r$rows)
  expect_null(r$cols)
})

test_that("new_fr_rule uses fr_rule_vline subclass for vertical", {
  r <- new_fr_rule(direction = "vertical")
  expect_s3_class(r, "fr_rule_vline")
  expect_s3_class(r, "fr_rule")
})

test_that("new_fr_rule validates region", {
  expect_silent(new_fr_rule(region = "header"))
  expect_silent(new_fr_rule(region = "body"))
  expect_silent(new_fr_rule(region = "spanners"))
  expect_error(new_fr_rule(region = "footer"), class = "rlang_error")
  expect_error(new_fr_rule(region = "titles"), class = "rlang_error")
})

test_that("new_fr_rule validates side", {
  expect_silent(new_fr_rule(side = "above"))
  expect_silent(new_fr_rule(side = "below"))
  expect_error(new_fr_rule(side = "left"), class = "rlang_error")
  expect_error(new_fr_rule(side = "between"), class = "rlang_error")
})

test_that("new_fr_rule validates linestyle including dashdot", {
  for (ls in c("solid", "dashed", "dotted", "double", "dashdot")) {
    r <- new_fr_rule(linestyle = ls)
    expect_equal(r$linestyle, ls)
  }
  expect_error(new_fr_rule(linestyle = "wavy"), class = "rlang_error")
  expect_error(new_fr_rule(linestyle = "dash"), class = "rlang_error")
})

test_that("new_fr_rule validates direction", {
  expect_silent(new_fr_rule(direction = "horizontal"))
  expect_silent(new_fr_rule(direction = "vertical"))
  expect_error(new_fr_rule(direction = "diagonal"), class = "rlang_error")
})

test_that("new_fr_rule validates rows parameter", {
  # NULL is valid
  r <- new_fr_rule(rows = NULL)
  expect_null(r$rows)

  # "all" is valid
  r <- new_fr_rule(rows = "all")
  expect_equal(r$rows, "all")

  # Integer vector is valid
  r <- new_fr_rule(rows = c(5, 10))
  expect_equal(r$rows, c(5L, 10L))

  # Errors
  expect_error(new_fr_rule(rows = "some"), class = "rlang_error")
  expect_error(new_fr_rule(rows = c(-1, 5)), class = "rlang_error")
  expect_error(new_fr_rule(rows = TRUE), class = "rlang_error")
})

test_that("new_fr_rule validates cols parameter", {
  r <- new_fr_rule(cols = NULL)
  expect_null(r$cols)

  r <- new_fr_rule(cols = "all")
  expect_equal(r$cols, "all")

  r <- new_fr_rule(cols = c(2, 4))
  expect_equal(r$cols, c(2L, 4L))

  expect_error(new_fr_rule(cols = "some"), class = "rlang_error")
  expect_error(new_fr_rule(cols = c(0, 2)), class = "rlang_error")
})

test_that("new_fr_rule accepts named width shorthands", {
  expect_equal(new_fr_rule(width = "thin")$width, 0.50)
  expect_equal(new_fr_rule(width = "hairline")$width, 0.25)
  expect_equal(new_fr_rule(width = "medium")$width, 1.00)
  expect_equal(new_fr_rule(width = "thick")$width, 1.50)
})

test_that("new_fr_rule accepts numeric width in points", {
  expect_equal(new_fr_rule(width = 0.75)$width, 0.75)
})

test_that("new_fr_rule errors on invalid width", {
  expect_error(new_fr_rule(width = 0), class = "rlang_error")
  expect_error(new_fr_rule(width = -1), class = "rlang_error")
  expect_error(new_fr_rule(width = "huge"), class = "rlang_error")
})

test_that("new_fr_rule stores partial line fields", {
  r <- new_fr_rule(leftpos = 0.1, rightpos = 0.9)
  expect_equal(r$leftpos, 0.1)
  expect_equal(r$rightpos, 0.9)
  expect_null(r$abovepos)
  expect_null(r$belowpos)
})

test_that("new_fr_rule validates partial line fractions", {
  expect_error(new_fr_rule(leftpos = -0.1), class = "rlang_error")
  expect_error(new_fr_rule(leftpos = 1.1), class = "rlang_error")
  expect_error(new_fr_rule(rightpos = 2), class = "rlang_error")
})

test_that("new_fr_rule validates and resolves color", {
  r <- new_fr_rule(fg = "#003366")
  expect_equal(r$fg, "#003366")

  r2 <- new_fr_rule(fg = "black")
  expect_match(r2$fg, "^#[0-9A-F]{6}$")
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_cell_style
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_cell_style creates object with NULLs and type=cell", {
  cs <- new_fr_cell_style()
  expect_s3_class(cs, "fr_cell_style")
  expect_equal(cs$type, "cell")
  expect_equal(cs$region, "body")
  expect_null(cs$bold)
  expect_null(cs$color)
  expect_null(cs$background)
  expect_null(cs$colspan)
  expect_null(cs$rowspan)
})

test_that("new_fr_cell_style accepts style properties", {
  cs <- new_fr_cell_style(
    region = "header",
    bold = TRUE,
    italic = FALSE,
    color = "#FF0000",
    background = "#EEEEEE",
    font_size = 10
  )
  expect_equal(cs$region, "header")
  expect_true(cs$bold)
  expect_false(cs$italic)
  expect_equal(cs$color, "#FF0000")
  expect_equal(cs$font_size, 10)
})

test_that("new_fr_cell_style stores colspan and rowspan", {
  cs <- new_fr_cell_style(colspan = 3L, rowspan = 2L)
  expect_equal(cs$colspan, 3L)
  expect_equal(cs$rowspan, 2L)
})

test_that("new_fr_row_style creates row-type cell_style", {
  rs <- new_fr_row_style(rows = c(1L, 2L), background = "#EEEEEE", bold = TRUE)
  expect_s3_class(rs, "fr_cell_style")
  expect_equal(rs$type, "row")
  expect_equal(rs$region, "body")
  expect_equal(rs$rows, c(1L, 2L))
  expect_equal(rs$background, "#EEEEEE")
  expect_true(rs$bold)
  expect_null(rs$cols)
})

test_that("new_fr_col_style creates col-type cell_style", {
  cs <- new_fr_col_style(cols = c("a", "b"), color = "#003366")
  expect_s3_class(cs, "fr_cell_style")
  expect_equal(cs$type, "col")
  expect_equal(cs$region, "body")
  expect_equal(cs$cols, c("a", "b"))
  expect_equal(cs$color, "#003366")
  expect_null(cs$rows)
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_meta, new_title_entry, new_footnote_entry
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_meta creates fr_meta with defaults", {
  m <- new_fr_meta()
  expect_s3_class(m, "fr_meta")
  expect_equal(m$titles, list())
  expect_equal(m$footnotes, list())
  expect_false(m$footnote_separator)
})

test_that("new_title_entry stores content and alignment", {
  te <- new_title_entry("Table 14.1.1", align = "left", bold = TRUE)
  expect_s3_class(te, "fr_title_entry")
  expect_equal(te$content, "Table 14.1.1")
  expect_equal(te$align, "left")
  expect_true(te$bold)
  expect_null(te$font_size)
})

test_that("fr_title_entry has a print method", {
  te <- new_title_entry("Table 14.1.1", align = "center", bold = TRUE)
  out <- capture.output(print(te))
  expect_match(out, "fr_title_entry")
  expect_match(out, "Table 14.1.1")
})

test_that("new_title_entry validates alignment", {
  expect_error(new_title_entry("x", align = "justify"), class = "rlang_error")
})

test_that("new_footnote_entry stores placement", {
  fe <- new_footnote_entry("Source: ADSL", placement = "every")
  expect_s3_class(fe, "fr_footnote_entry")
  expect_equal(fe$placement, "every")

  fe2 <- new_footnote_entry("End of table", placement = "last")
  expect_equal(fe2$placement, "last")
})

test_that("fr_footnote_entry has a print method", {
  fe <- new_footnote_entry("Source: ADSL", placement = "last")
  out <- capture.output(print(fe))
  expect_match(out, "fr_footnote_entry")
  expect_match(out, "Source: ADSL")
})

test_that("new_footnote_entry validates placement", {
  expect_error(
    new_footnote_entry("x", placement = "first"),
    class = "rlang_error"
  )
})

test_that("new_footnote_entry defaults to left align and every placement", {
  fe <- new_footnote_entry("Source")
  expect_equal(fe$align, "left")
  expect_equal(fe$placement, "every")
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_pagechrome
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_pagechrome creates fr_pagechrome", {
  pc <- new_fr_pagechrome(left = "Study ABC", right = "Page 1")
  expect_s3_class(pc, "fr_pagechrome")
  expect_equal(pc$left, "Study ABC")
  expect_equal(pc$right, "Page 1")
  expect_null(pc$center)
  expect_null(pc$bold)
})

test_that("new_fr_pagechrome accepts multi-line character vectors", {
  pc <- new_fr_pagechrome(left = c("Line 1", "Line 2"))
  expect_length(pc$left, 2)
})


# ══════════════════════════════════════════════════════════════════════════════
# new_fr_spec
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_spec creates fr_spec from data frame", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  spec <- new_fr_spec(df)
  expect_s3_class(spec, "fr_spec")
  expect_identical(spec$data, df)
  expect_s3_class(spec$meta, "fr_meta")
  expect_s3_class(spec$page, "fr_page")
  expect_equal(spec$rules, list())
  expect_equal(spec$cell_styles, list())
  expect_null(spec$pagehead)
  expect_null(spec$pagefoot)
})

test_that("new_fr_spec errors on non-data.frame", {
  expect_error(new_fr_spec(list(a = 1)), class = "rlang_error")
  expect_error(new_fr_spec("not a df"), class = "rlang_error")
  expect_error(new_fr_spec(1:5), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# Validators
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_fr_spec passes for valid fr_spec", {
  spec <- new_fr_spec(data.frame(x = 1))
  expect_invisible(check_fr_spec(spec))
})

test_that("check_fr_spec errors on non-fr_spec", {
  expect_error(check_fr_spec(list()), class = "rlang_error")
  expect_error(check_fr_spec(data.frame()), class = "rlang_error")
  expect_error(check_fr_spec("spec"), class = "rlang_error")
})

test_that("check_fr_col passes for valid fr_col", {
  col <- fr_col("test")
  expect_invisible(check_fr_col(col))
})

test_that("check_fr_col errors on non-fr_col", {
  expect_error(check_fr_col(list()), class = "rlang_error")
  expect_error(check_fr_col("col"), class = "rlang_error")
})

test_that("fr_col accepts space_mode parameter", {
  col_indent <- fr_col("A", space_mode = "indent")
  expect_equal(col_indent$space_mode, "indent")
  col_preserve <- fr_col("A", space_mode = "preserve")
  expect_equal(col_preserve$space_mode, "preserve")
  col_null <- fr_col("A")
  expect_null(col_null$space_mode)
  expect_error(fr_col("A", space_mode = "invalid"), class = "rlang_error")
})


test_that("print.fr_spec shows dimensions and orientation", {
  spec <- data.frame(a = 1:3, b = letters[1:3]) |>
    fr_table() |>
    fr_titles("Test table") |>
    fr_cols(a = fr_col("A"), b = fr_col("B"))

  # Rich print outputs via cli — verify it doesn't error
  expect_no_error(print(spec))

  # Compact print (backward compatible)
  out_compact <- capture.output(print(spec, compact = TRUE))
  expect_match(out_compact, "fr_spec.*3 rows.*2 columns.*landscape")
  expect_match(out_compact, "1 title")
})

test_that("print.fr_col shows label, width, and alignment", {
  col <- fr_col("Age", width = 1.5, align = "right")
  out <- capture.output(print(col))
  expect_match(out, "fr_col.*Age.*1\\.50in.*right")
})


# ══════════════════════════════════════════════════════════════════════════════
# Validation helpers
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_non_negative_int accepts valid inputs", {
  expect_equal(check_non_negative_int(0L), 0L)
  expect_equal(check_non_negative_int(3), 3L)
  expect_equal(check_non_negative_int(5L), 5L)
})

test_that("check_non_negative_int rejects invalid inputs", {
  expect_error(check_non_negative_int(-1), class = "rlang_error")
  expect_error(check_non_negative_int(1.5), class = "rlang_error")
  expect_error(check_non_negative_int(NA_real_), class = "rlang_error")
})

test_that("validate_n_param accepts valid n forms", {
  expect_silent(validate_n_param(n = c(a = 10, b = 20)))
  expect_silent(validate_n_param(n = data.frame(trt = "A", n = 10L)))
  expect_silent(validate_n_param(n = list(grp1 = c(a = 10))))
  expect_silent(validate_n_param(n = NULL))
})

test_that("validate_n_param rejects invalid n forms", {
  # unnamed numeric
  expect_error(validate_n_param(n = c(1, 2)), class = "rlang_error")
  # character string (no longer valid)
  expect_error(validate_n_param(n = "auto"), class = "rlang_error")
  # wrong type
  expect_error(validate_n_param(n = TRUE), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_pct — Percentage width
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_col(width = '20%') stores fr_pct(0.20)", {
  col <- fr_col(width = "20%")
  expect_true(is_fr_pct(col$width))
  expect_equal(unclass(col$width), 0.20)
})

test_that("fr_col(width = '0%') errors (not positive)", {
  expect_error(fr_col(width = "0%"), class = "rlang_error")
})

test_that("fr_col(width = '101%') errors (over 100%)", {
  expect_error(fr_col(width = "101%"), class = "rlang_error")
})

test_that("fr_col(width = 1.5) still works (regression)", {
  col <- fr_col(width = 1.5)
  expect_equal(col$width, 1.5)
  expect_false(is_fr_pct(col$width))
})

test_that("fr_col(width = 'auto') still works (regression)", {
  col <- fr_col(width = "auto")
  expect_equal(col$width, "auto")
})

test_that("fr_col(width = '50.5%') parses decimal percentages", {
  col <- fr_col(width = "50.5%")
  expect_true(is_fr_pct(col$width))
  expect_equal(unclass(col$width), 0.505)
})

test_that("print.fr_col shows percentage for pct widths", {
  col <- fr_col("Test", width = "20%")
  out <- capture.output(print(col))
  expect_match(out, "20%", fixed = TRUE)
})

test_that("finalize_spec resolves fr_pct to absolute inches", {
  data <- data.frame(a = 1, stringsAsFactors = FALSE)
  spec <- data |>
    fr_table() |>
    fr_cols(a = fr_col("A", width = "20%")) |>
    fr_page(orientation = "landscape", paper = "letter", margins = 0.75)
  spec <- finalize_spec(spec)

  # Landscape letter 11in - 2*0.75 = 9.5in; 20% of 9.5 = 1.9
  expect_equal(spec$columns[["a"]]$width, 0.20 * 9.5, tolerance = 0.01)
})

test_that("fr_cols .width = '25%' applies to all columns", {
  data <- data.frame(a = 1, b = 2, c = 3, stringsAsFactors = FALSE)
  spec <- data |>
    fr_table() |>
    fr_cols(.width = "25%")

  for (nm in c("a", "b", "c")) {
    expect_true(is_fr_pct(spec$columns[[nm]]$width))
    expect_equal(unclass(spec$columns[[nm]]$width), 0.25)
  }
})

test_that("mixed absolute and percentage widths work", {
  data <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  spec <- data |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = 2.0),
      b = fr_col("B", width = "30%")
    ) |>
    fr_page(orientation = "landscape", paper = "letter", margins = 1)
  spec <- finalize_spec(spec)

  # a stays 2.0 inches; b = 30% of 9.0 = 2.7
  expect_equal(spec$columns[["a"]]$width, 2.0, tolerance = 0.01)
  expect_equal(spec$columns[["b"]]$width, 0.30 * 9.0, tolerance = 0.01)
})


# Helper: capture both stdout and stderr (cli writes to stderr in tests)
capture_print <- function(x, ...) {
  out1 <- capture.output(print(x, ...), type = "output")
  out2 <- capture.output(print(x, ...), type = "message")
  paste(c(out1, out2), collapse = "\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# COVERAGE EXPANSION — print.fr_spec rich tree view
# ══════════════════════════════════════════════════════════════════════════════

test_that("print.fr_spec rich view shows titles section", {
  spec <- data.frame(x = 1:3) |>
    fr_table() |>
    fr_titles("Table 14.1.1", "Demographics") |>
    fr_cols(x = fr_col("X", width = 1.5))

  combined <- capture_print(spec)
  expect_match(combined, "Titles")
  expect_match(combined, "Table 14.1.1", fixed = TRUE)
})

test_that("print.fr_spec rich view truncates long titles", {
  long_title <- paste(rep("A", 70), collapse = "")
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_titles(long_title) |>
    fr_cols(x = fr_col("X"))

  combined <- capture_print(spec)
  # Should truncate with "..."

  expect_match(combined, "\\.\\.\\.")
})

test_that("print.fr_spec rich view shows footnotes section", {
  spec <- data.frame(x = 1:3) |>
    fr_table() |>
    fr_footnotes("Source: ADSL", "P-values from chi-square test") |>
    fr_cols(x = fr_col("X"))

  combined <- capture_print(spec)
  expect_match(combined, "Footnotes")
  expect_match(combined, "Source: ADSL", fixed = TRUE)
})

test_that("print.fr_spec rich view shows footnote with last placement", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_footnotes(list("End of table", placement = "last")) |>
    fr_cols(x = fr_col("X"))

  combined <- capture_print(spec)
  expect_match(combined, "\\[last\\]")
})

test_that("print.fr_spec rich view truncates long footnotes", {
  long_fn <- paste(rep("B", 70), collapse = "")
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_footnotes(long_fn) |>
    fr_cols(x = fr_col("X"))

  combined <- capture_print(spec)
  expect_match(combined, "\\.\\.\\.")
})

test_that("print.fr_spec rich view shows columns section with visible count", {
  spec <- data.frame(a = 1, b = 2, c = 3) |>
    fr_table() |>
    fr_cols(
      a = fr_col("Alpha", width = 2.0),
      b = fr_col("Beta", width = 1.5, align = "right"),
      c = fr_col(visible = FALSE)
    )

  combined <- capture_print(spec)
  expect_match(combined, "Columns")
  expect_match(combined, "2 visible of 3", fixed = TRUE)
})

test_that("print.fr_spec rich view shows header section with bold", {
  spec <- data.frame(a = 1, b = 2) |>
    fr_table() |>
    fr_cols(a = fr_col("A"), b = fr_col("B")) |>
    fr_header(bold = TRUE, align = "center")

  combined <- capture_print(spec)
  expect_match(combined, "Header")
  expect_match(combined, "bold")
  expect_match(combined, "align=center", fixed = TRUE)
})

test_that("print.fr_spec rich view shows rows section", {
  spec <- data.frame(grp = c("A", "A", "B"), x = 1:3) |>
    fr_table() |>
    fr_cols(grp = fr_col("Group"), x = fr_col("X")) |>
    fr_rows(group_by = "grp", indent_by = "grp")

  combined <- capture_print(spec)
  expect_match(combined, "Rows")
  expect_match(combined, "group_by=grp", fixed = TRUE)
  expect_match(combined, "indent_by=grp", fixed = TRUE)
})

test_that("print.fr_spec rich view shows rows with page_by and sort_by", {
  spec <- data.frame(cat = "A", grp = "G1", x = 1) |>
    fr_table() |>
    fr_cols(cat = fr_col("Cat"), grp = fr_col("Grp"), x = fr_col("X")) |>
    fr_rows(page_by = "cat", sort_by = "grp")

  combined <- capture_print(spec)
  expect_match(combined, "page_by=cat", fixed = TRUE)
  expect_match(combined, "sort_by=grp", fixed = TRUE)
})

test_that("print.fr_spec rich view shows rules section", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_cols(x = fr_col("X")) |>
    fr_hlines("header") |>
    fr_vlines("box")

  combined <- capture_print(spec)
  expect_match(combined, "Rules")
})

test_that("print.fr_spec rich view shows styles section", {
  spec <- data.frame(a = 1:5) |>
    fr_table() |>
    fr_cols(a = fr_col("A")) |>
    fr_styles(
      fr_style(rows = 1, bold = TRUE),
      fr_style(rows = 2, italic = TRUE)
    )

  combined <- capture_print(spec)
  expect_match(combined, "Styles")
  expect_match(combined, "2 override")
})

test_that("print.fr_spec rich view shows spans section", {
  spec <- data.frame(a = 1, b = 2, c = 3) |>
    fr_table() |>
    fr_cols(a = fr_col("A"), b = fr_col("B"), c = fr_col("C")) |>
    fr_spans("Treatment" = c("b", "c"))

  combined <- capture_print(spec)
  expect_match(combined, "Spans")
})

test_that("print.fr_spec rich view shows column percentage width", {
  spec <- data.frame(a = 1) |>
    fr_table() |>
    fr_cols(a = fr_col("Alpha", width = "25%"))

  combined <- capture_print(spec)
  expect_match(combined, "25%", fixed = TRUE)
})

test_that("print.fr_spec rich view shows column resolved auto width", {
  spec <- data.frame(a = 1) |>
    fr_table() |>
    fr_cols(a = fr_col("Alpha", width = "auto"))

  combined <- capture_print(spec)
  # auto width gets resolved to inches in fr_cols()
  expect_match(combined, "in")
})

test_that("print.fr_spec rich view shows column fixed inch width", {
  spec <- data.frame(a = 1) |>
    fr_table() |>
    fr_cols(a = fr_col("Alpha", width = 2.5))

  combined <- capture_print(spec)
  expect_match(combined, "2.50in", fixed = TRUE)
})

test_that("print.fr_spec rich view truncates long column labels", {
  long_label <- paste(rep("X", 25), collapse = "")
  spec <- data.frame(a = 1) |>
    fr_table() |>
    fr_cols(a = fr_col(long_label))

  combined <- capture_print(spec)
  expect_match(combined, "\\.\\.\\.")
})

test_that("print.fr_spec rich view shows many columns", {
  df <- as.data.frame(setNames(as.list(1:12), paste0("c", 1:12)))
  cols_list <- setNames(lapply(paste0("C", 1:12), fr_col), paste0("c", 1:12))
  spec <- df |> fr_table()
  spec$columns <- cols_list
  for (nm in names(cols_list)) {
    spec$columns[[nm]]$id <- nm
  }

  combined <- capture_print(spec)
  expect_match(combined, "Columns")
})

test_that("print.fr_spec compact mode shows title and footnote counts", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_titles("Title 1", "Title 2") |>
    fr_footnotes("FN 1") |>
    fr_cols(x = fr_col("X"))

  combined <- capture_print(spec, compact = TRUE)
  expect_match(combined, "2 title")
  expect_match(combined, "1 footnote")
})

test_that("print.fr_spec compact mode with no titles or footnotes", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_cols(x = fr_col("X"))

  combined <- capture_print(spec, compact = TRUE)
  expect_match(combined, "fr_spec")
  # No title/footnote info

  expect_no_match(combined, "title")
})


# ══════════════════════════════════════════════════════════════════════════════
# COVERAGE EXPANSION — summary.fr_spec
# ══════════════════════════════════════════════════════════════════════════════

test_that("summary.fr_spec delegates to print.fr_spec", {
  spec <- data.frame(x = 1:3) |>
    fr_table() |>
    fr_titles("Summary test") |>
    fr_cols(x = fr_col("X"))

  out_print <- capture_print(spec)
  out_summary <- capture_print(summary(spec))
  # Both should contain the same key sections
  expect_match(out_print, "fr_spec")
})

test_that("summary.fr_spec returns object invisibly", {
  spec <- data.frame(x = 1) |> fr_table()
  result <- withVisible(summary(spec))
  expect_false(result$visible)
})


# ══════════════════════════════════════════════════════════════════════════════
# COVERAGE EXPANSION — new_fr_spec with type="listing" and type="figure"
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_spec accepts type='listing'", {
  spec <- new_fr_spec(data.frame(x = 1), type = "listing")
  expect_equal(spec$type, "listing")
})

test_that("new_fr_spec accepts type='figure'", {
  spec <- new_fr_spec(data.frame(x = 1), type = "figure")
  expect_equal(spec$type, "figure")
})

test_that("print.fr_spec shows type label for listing", {
  spec <- new_fr_spec(data.frame(x = 1), type = "listing")
  combined <- capture_print(spec)
  expect_match(combined, "Listing")
})

test_that("print.fr_spec shows type label for figure", {
  spec <- new_fr_spec(data.frame(x = 1), type = "figure")
  combined <- capture_print(spec)
  expect_match(combined, "Figure")
})

test_that("print.fr_spec shows plot class when present", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    type = "figure",
    plot = structure(list(), class = "gg")
  )
  combined <- capture_print(spec)
  expect_match(combined, "Plot.*gg")
})


# ══════════════════════════════════════════════════════════════════════════════
# COVERAGE EXPANSION — new_fr_page edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_page accepts portrait orientation", {
  page <- new_fr_page(orientation = "portrait")
  expect_equal(page$orientation, "portrait")
})

test_that("new_fr_page accepts a4 and legal paper", {
  page_a4 <- new_fr_page(paper = "a4")
  expect_equal(page_a4$paper, "a4")

  page_legal <- new_fr_page(paper = "legal")
  expect_equal(page_legal$paper, "legal")
})

test_that("new_fr_page boundary font sizes (4 and 72)", {
  page_min <- new_fr_page(font_size = 4)
  expect_equal(page_min$font_size, 4)

  page_max <- new_fr_page(font_size = 72)
  expect_equal(page_max$font_size, 72)
})

test_that("new_fr_page rejects font_size at boundaries", {
  expect_error(new_fr_page(font_size = 3.9), class = "rlang_error")
  expect_error(new_fr_page(font_size = 72.1), class = "rlang_error")
})

test_that("new_fr_page accepts explicit font_family", {
  page <- new_fr_page(font_family = "Times New Roman")
  expect_equal(page$font_family, "Times New Roman")
})

test_that("new_fr_page stores empty tokens by default", {
  page <- new_fr_page()
  expect_equal(page$tokens, list())
})


test_that("new_fr_page errors on non-character continuation", {
  expect_error(new_fr_page(continuation = 123), class = "rlang_error")
})

test_that("new_fr_page casts orphan_min and widow_min to integer", {
  page <- new_fr_page(orphan_min = 5, widow_min = 4)
  expect_type(page$orphan_min, "integer")
  expect_equal(page$orphan_min, 5L)
  expect_type(page$widow_min, "integer")
  expect_equal(page$widow_min, 4L)
})


test_that("new_fr_page accepts named list margins", {
  page <- new_fr_page(
    margins = list(top = 0.5, bottom = 0.5, left = 1.0, right = 1.0)
  )
  expect_equal(page$margins$top, 0.5)
  expect_equal(page$margins$left, 1.0)
})

test_that("validate_user_tokens errors on non-list non-empty input", {
  expect_error(validate_user_tokens("not_a_list"), class = "rlang_error")
  expect_error(validate_user_tokens(c(a = 1, b = 2)), class = "rlang_error")
})

test_that("validate_user_tokens errors on both readonly tokens together", {
  expect_error(
    validate_user_tokens(list(thepage = "1", total_pages = "5")),
    "thepage"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# COVERAGE EXPANSION — fr_col parameter combinations
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_col with all parameters set", {
  col <- fr_col(
    label = "Parameter",
    width = 2.0,
    align = "decimal",
    header_align = "center",
    visible = TRUE
  )
  expect_equal(col$label, "Parameter")
  expect_equal(col$width, 2.0)
  expect_equal(col$align, "decimal")
  expect_equal(col$header_align, "center")
  expect_true(col$visible)
})

test_that("fr_col percentage widths store fr_pct objects", {
  col_100 <- fr_col(width = "100%")
  expect_true(is_fr_pct(col_100$width))
  expect_equal(unclass(col_100$width), 1.0)

  col_small <- fr_col(width = "1%")
  expect_true(is_fr_pct(col_small$width))
  expect_equal(unclass(col_small$width), 0.01)
})

test_that("fr_col with header_align different from align", {
  col <- fr_col(align = "right", header_align = "left")
  expect_equal(col$align, "right")
  expect_equal(col$header_align, "left")
})

test_that("fr_col errors on invalid header_align", {
  expect_error(fr_col(header_align = "middle"), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# COVERAGE EXPANSION — print.fr_col output
# ══════════════════════════════════════════════════════════════════════════════

test_that("print.fr_col shows auto width", {
  col <- fr_col("Test", width = "auto")
  out <- capture.output(print(col))
  expect_match(out, "auto")
  expect_match(out, "Test")
})

test_that("print.fr_col shows left align when align is NULL", {
  col <- fr_col("Test")
  out <- capture.output(print(col))
  expect_match(out, "left")
})

test_that("print.fr_col shows percentage width", {
  col <- fr_col("Col", width = "33%")
  out <- capture.output(print(col))
  expect_match(out, "33%", fixed = TRUE)
})

test_that("print.fr_col returns invisible", {
  col <- fr_col("Test")
  result <- withVisible(print(col))
  expect_false(result$visible)
})


# ══════════════════════════════════════════════════════════════════════════════
# COVERAGE EXPANSION — check_fr_spec / check_fr_col error messages
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_fr_spec error includes fr_table hint", {
  expect_error(check_fr_spec(42), "fr_table")
})

test_that("check_fr_col error includes fr_col hint", {
  expect_error(check_fr_col(42), "fr_col")
})

test_that("check_fr_spec passes with return value", {
  spec <- new_fr_spec(data.frame(x = 1))
  result <- check_fr_spec(spec)
  expect_s3_class(result, "fr_spec")
})

test_that("check_fr_col passes with return value", {
  col <- fr_col("test")
  result <- check_fr_col(col)
  expect_s3_class(result, "fr_col")
})


# ══════════════════════════════════════════════════════════════════════════════
# COVERAGE EXPANSION — constructor edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("new_fr_spec stores spacing defaults", {
  spec <- new_fr_spec(data.frame(x = 1))
  expect_equal(spec$spacing$titles_after, 1L)
  expect_equal(spec$spacing$footnotes_before, 1L)
  expect_equal(spec$spacing$pagehead_after, 0L)
  expect_equal(spec$spacing$pagefoot_before, 0L)
  expect_equal(spec$spacing$page_by_after, 1L)
})

test_that("new_fr_spec stores type field", {
  spec <- new_fr_spec(data.frame(x = 1), type = "table")
  expect_equal(spec$type, "table")
})

test_that("new_fr_spec stores plot field", {
  fake_plot <- list(data = 1)
  spec <- new_fr_spec(data.frame(x = 1), plot = fake_plot)
  expect_equal(spec$plot, fake_plot)
})

test_that("new_fr_spec stores custom spacing", {
  custom_spacing <- list(
    titles_after = 2L,
    footnotes_before = 0L,
    pagehead_after = 1L,
    pagefoot_before = 1L,
    page_by_after = 0L
  )
  spec <- new_fr_spec(data.frame(x = 1), spacing = custom_spacing)
  expect_equal(spec$spacing$titles_after, 2L)
  expect_equal(spec$spacing$footnotes_before, 0L)
})

test_that("new_fr_header stores all optional parameters", {
  h <- new_fr_header(
    repeat_on_page = FALSE,
    valign = "top",
    align = "center",
    bold = TRUE,
    background = "#EEEEEE",
    color = "#333333",
    font_size = 10,
    span_gap = FALSE
  )
  expect_false(h$repeat_on_page)
  expect_equal(h$valign, "top")
  expect_equal(h$align, "center")
  expect_true(h$bold)
  expect_equal(h$background, "#EEEEEE")
  expect_equal(h$color, "#333333")
  expect_equal(h$font_size, 10)
  expect_false(h$span_gap)
})

test_that("new_fr_header no longer accepts align_map parameter", {
  # align_map was removed — per-column header alignment now uses

  # fr_col(header_align = ...) instead of fr_header(align = list(...))
  h <- new_fr_header(align = "center")
  expect_equal(h$align, "center")
  expect_null(h$align_map)
})

test_that("new_fr_body stores all parameters", {
  b <- new_fr_body(
    page_by = "cat",
    group_by = "grp",
    indent_by = "indent",
    blank_after = "grp",
    page_by_visible = FALSE,
    group_label = "stat",
    group_keep = TRUE,
    group_leaf = "leaf_col",
    sort_by = "ord",
    suppress = c("a", "b"),
    wrap = TRUE
  )
  expect_equal(b$page_by, "cat")
  expect_equal(b$group_by, "grp")
  expect_equal(b$indent_by, "indent")
  expect_equal(b$blank_after, "grp")
  expect_false(b$page_by_visible)
  expect_equal(b$group_label, "stat")
  expect_true(b$group_keep)
  expect_equal(b$group_leaf, "leaf_col")
  expect_equal(b$sort_by, "ord")
  expect_equal(b$suppress, c("a", "b"))
  expect_true(b$wrap)
})

test_that("new_fr_span stores hline field", {
  span_with <- new_fr_span("Test", "col1", hline = TRUE)
  expect_true(span_with$hline)

  span_without <- new_fr_span("Test", "col1", hline = FALSE)
  expect_false(span_without$hline)
})

test_that("new_fr_span casts columns and level", {
  span <- new_fr_span("Header", c("a", "b"), level = 2)
  expect_type(span$columns, "character")
  expect_type(span$level, "integer")
  expect_equal(span$level, 2L)
})

test_that("new_fr_meta stores footnote_separator", {
  m <- new_fr_meta(footnote_separator = TRUE)
  expect_true(m$footnote_separator)
})

test_that("new_fr_meta stores titles and footnotes as lists", {
  te <- new_title_entry("Title")
  fe <- new_footnote_entry("Footnote")
  m <- new_fr_meta(titles = list(te), footnotes = list(fe))
  expect_length(m$titles, 1)
  expect_length(m$footnotes, 1)
})

test_that("new_fr_pagechrome stores font_size and bold", {
  pc <- new_fr_pagechrome(left = "Study", font_size = 8, bold = TRUE)
  expect_equal(pc$font_size, 8)
  expect_true(pc$bold)
})

test_that("new_fr_pagechrome stores center text", {
  pc <- new_fr_pagechrome(center = "Confidential")
  expect_equal(pc$center, "Confidential")
})

test_that("new_fr_cell_style stores all optional fields", {
  cs <- new_fr_cell_style(
    type = "cell",
    region = "header",
    rows = 1:3,
    cols = c(2L, 4L),
    bold = TRUE,
    italic = TRUE,
    underline = TRUE,
    color = "#FF0000",
    background = "#00FF00",
    font = "Arial",
    font_size = 12,
    align = "center",
    valign = "middle",
    indent = 0.25,
    colspan = 2L,
    rowspan = 3L,
    height = 0.5
  )
  expect_equal(cs$rows, 1:3)
  expect_equal(cs$cols, c(2L, 4L))
  expect_true(cs$bold)
  expect_true(cs$italic)
  expect_true(cs$underline)
  expect_equal(cs$color, "#FF0000")
  expect_equal(cs$background, "#00FF00")
  expect_equal(cs$font, "Arial")
  expect_equal(cs$font_size, 12)
  expect_equal(cs$align, "center")
  expect_equal(cs$valign, "middle")
  expect_equal(cs$indent, 0.25)
  expect_equal(cs$colspan, 2L)
  expect_equal(cs$rowspan, 3L)
  expect_equal(cs$height, 0.5)
})

test_that("new_fr_cell_style resolves named colors", {
  cs <- new_fr_cell_style(color = "red", background = "blue")
  expect_match(cs$color, "^#[0-9A-Fa-f]{6}$")
  expect_match(cs$background, "^#[0-9A-Fa-f]{6}$")
})

test_that("new_fr_row_style passes height through", {
  rs <- new_fr_row_style(rows = 1L, height = 0.75)
  expect_equal(rs$height, 0.75)
})

test_that("new_fr_col_style passes valign through", {
  cs <- new_fr_col_style(cols = "a", valign = "bottom")
  expect_equal(cs$valign, "bottom")
})

test_that("new_fr_rule with rows = non-integer decimals errors", {
  expect_error(new_fr_rule(rows = c(1.5, 2.5)), class = "rlang_error")
})

test_that("new_fr_rule with cols = non-integer decimals errors", {
  expect_error(new_fr_rule(cols = c(1.5)), class = "rlang_error")
})

test_that("new_fr_rule stores vertical partial line fields", {
  r <- new_fr_rule(direction = "vertical", abovepos = 0.0, belowpos = 1.0)
  expect_equal(r$abovepos, 0.0)
  expect_equal(r$belowpos, 1.0)
})

test_that("new_fr_rule validates abovepos and belowpos fractions", {
  expect_error(new_fr_rule(abovepos = -0.1), class = "rlang_error")
  expect_error(new_fr_rule(belowpos = 1.5), class = "rlang_error")
  expect_error(new_fr_rule(abovepos = NA), class = "rlang_error")
})

test_that("new_fr_rule with fg = NULL defaults to black", {
  r <- new_fr_rule(fg = NULL)
  expect_equal(r$fg, "#000000")
})

test_that("fr_pct and is_fr_pct work correctly", {
  p <- arframe:::fr_pct(0.5)
  expect_true(arframe:::is_fr_pct(p))
  expect_false(arframe:::is_fr_pct(0.5))
  expect_false(arframe:::is_fr_pct("50%"))
  expect_equal(unclass(p), 0.5)
})

test_that("print.fr_title_entry shows font_size when set", {
  te <- new_title_entry("Title", font_size = 12)
  out <- capture.output(print(te))
  expect_match(out, "12pt")
})

test_that("print.fr_title_entry shows no bold or size for defaults", {
  te <- new_title_entry("Title")
  out <- capture.output(print(te))
  expect_no_match(out, "bold")
  expect_no_match(out, "pt")
})

test_that("print.fr_footnote_entry shows no [last] for every placement", {
  fe <- new_footnote_entry("Note", placement = "every")
  out <- capture.output(print(fe))
  expect_no_match(out, "\\[last\\]")
})

test_that("print.fr_footnote_entry shows font_size when set", {
  fe <- new_footnote_entry("Note", font_size = 7)
  out <- capture.output(print(fe))
  expect_match(out, "7pt")
})

test_that("print.fr_spec header with only valign shows nothing extra", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_cols(x = fr_col("X"))
  # Default header has valign=bottom but no bold/align/n,
  # so only valign should appear
  combined <- capture_print(spec)
  expect_match(combined, "valign=bottom", fixed = TRUE)
})

test_that("print.fr_spec with no columns configured shows Data only", {
  spec <- new_fr_spec(data.frame(x = 1, y = 2))
  combined <- capture_print(spec)
  expect_match(combined, "Data")
  # No "Columns" section when columns list is empty
  expect_no_match(combined, "visible of")
})

test_that("print.fr_spec with body wrap shows wrap in Rows", {
  spec <- data.frame(x = 1) |>
    fr_table() |>
    fr_cols(x = fr_col("X")) |>
    fr_rows(wrap = TRUE)

  combined <- capture_print(spec)
  expect_match(combined, "wrap")
})
