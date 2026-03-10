# ─────────────────────────────────────────────────────────────────────────────
# test-style.R — Comprehensive tests for api-style.R
#   fr_style, fr_row_style, fr_col_style, fr_styles, fr_style_if,
#   fr_style_explain, fr_rows_matches, resolve_conditional_style
# ─────────────────────────────────────────────────────────────────────────────

# Shared fixture data
df_style <- data.frame(
  characteristic = c("Age", "Sex", "Race", "Total"),
  placebo        = c("35.2 (8.1)", "25 (55.6)", "40 (88.9)", "45"),
  zom_50mg       = c("36.1 (7.9)", "22 (48.9)", "38 (84.4)", "45"),
  total          = c("71.3 (8.0)", "47 (52.2)", "78 (86.7)", "90"),
  stringsAsFactors = FALSE
)

df_pval <- data.frame(
  characteristic = c("Age", "Sex", "Weight"),
  treatment      = c("50 (23.5)", "30 (14.1)", "45 (21.1)"),
  placebo        = c("55 (25.8)", "28 (13.1)", "52 (24.4)"),
  pvalue         = c("0.042", "0.310", "0.003"),
  stringsAsFactors = FALSE
)


# ══════════════════════════════════════════════════════════════════════════════
# fr_rows_matches
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows_matches with exact value creates selector", {
  sel <- fr_rows_matches("characteristic", value = "Total")
  expect_s3_class(sel, "fr_rows_selector")
  expect_equal(sel$col, "characteristic")
  expect_equal(sel$value, "Total")
  expect_null(sel$pattern)
  expect_false(sel$ignore.case)
})

test_that("fr_rows_matches with pattern creates selector", {
  sel <- fr_rows_matches("characteristic", pattern = "^Age")
  expect_s3_class(sel, "fr_rows_selector")
  expect_equal(sel$col, "characteristic")
  expect_null(sel$value)
  expect_equal(sel$pattern, "^Age")
  expect_false(sel$ignore.case)
})

test_that("fr_rows_matches with pattern and ignore.case", {
  sel <- fr_rows_matches("characteristic", pattern = "total", ignore.case = TRUE)
  expect_true(sel$ignore.case)
})

test_that("fr_rows_matches errors when col is not a scalar string", {
  expect_error(fr_rows_matches(1L, value = "x"), class = "rlang_error")
  expect_error(fr_rows_matches(c("a", "b"), value = "x"), class = "rlang_error")
  expect_error(fr_rows_matches(NULL, value = "x"), class = "rlang_error")
})

test_that("fr_rows_matches errors when neither value nor pattern given", {
  expect_error(fr_rows_matches("col"), "value.*pattern")
})

test_that("fr_rows_matches errors when both value and pattern given", {
  expect_error(fr_rows_matches("col", value = "x", pattern = "y"), "not both")
})

test_that("fr_rows_matches errors when pattern is not a scalar string", {
  expect_error(fr_rows_matches("col", pattern = c("a", "b")), class = "rlang_error")
  expect_error(fr_rows_matches("col", pattern = 42), class = "rlang_error")
})

test_that("fr_rows_matches errors on non-logical ignore.case", {
  expect_error(fr_rows_matches("col", value = "x", ignore.case = "yes"),
               class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_rows_matches resolution through fr_styles
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows_matches resolves exact value in fr_styles pipeline", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_row_style(rows = fr_rows_matches("characteristic", value = "Total"),
                   bold = TRUE)
    )
  # Selector should be resolved to integer row index
  style <- spec$cell_styles[[1]]
  expect_equal(style$rows, 4L)
  expect_true(style$bold)
})

test_that("fr_rows_matches resolves pattern in fr_styles pipeline", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_row_style(
        rows = fr_rows_matches("characteristic", pattern = "^(Age|Sex)"),
        bg = "#F0F0F0"
      )
    )
  style <- spec$cell_styles[[1]]
  expect_equal(sort(style$rows), c(1L, 2L))
})

test_that("fr_rows_matches pattern with ignore.case resolves correctly", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_row_style(
        rows = fr_rows_matches("characteristic", pattern = "total",
                               ignore.case = TRUE),
        bold = TRUE
      )
    )
  style <- spec$cell_styles[[1]]
  expect_equal(style$rows, 4L)
})

test_that("fr_rows_matches errors when column not found in data", {
  expect_error(
    df_style |>
      fr_table() |>
      fr_styles(
        fr_row_style(rows = fr_rows_matches("nonexistent", value = "x"))
      ),
    "not found"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_style — constructor with all parameter combinations
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_style stores all properties correctly", {
  s <- fr_style(
    region = "body", rows = c(1L, 2L), cols = c("placebo", "total"),
    bold = TRUE, italic = TRUE, underline = TRUE,
    fg = "#CC0000", bg = "#F0F0F0", font_size = 10,
    align = "center", valign = "middle",
    indent = 0.25, colspan = 2L, rowspan = 3L
  )
  expect_s3_class(s, "fr_cell_style")
  expect_equal(s$type, "cell")
  expect_equal(s$region, "body")
  expect_equal(s$rows, c(1L, 2L))
  expect_equal(s$cols, c("placebo", "total"))
  expect_true(s$bold)
  expect_true(s$italic)
  expect_true(s$underline)
  expect_equal(s$fg, "#CC0000")
  expect_equal(s$bg, "#F0F0F0")
  expect_equal(s$font_size, 10)
  expect_equal(s$align, "center")
  expect_equal(s$valign, "middle")
  expect_equal(s$indent, 0.25)
  expect_equal(s$colspan, 2L)
  expect_equal(s$rowspan, 3L)
})

test_that("fr_style validates region", {
  expect_error(fr_style(region = "footer"), class = "rlang_error")
  expect_error(fr_style(region = "table"), class = "rlang_error")
})

test_that("fr_style validates valign", {
  expect_error(fr_style(valign = "center"), class = "rlang_error")
  expect_error(fr_style(valign = "baseline"), class = "rlang_error")
})

test_that("fr_style with all valid aligns", {
  for (a in c("left", "center", "right", "decimal")) {
    s <- fr_style(align = a)
    expect_equal(s$align, a)
  }
})

test_that("fr_style with all valid valigns", {
  for (v in c("top", "middle", "bottom")) {
    s <- fr_style(valign = v)
    expect_equal(s$valign, v)
  }
})

test_that("fr_style with rows='all'", {
  s <- fr_style(rows = "all", bold = TRUE)
  expect_equal(s$rows, "all")
})

test_that("fr_style font_size=0 is rejected", {
  expect_error(fr_style(font_size = 0), class = "rlang_error")
})

test_that("fr_style with decimal align", {
  s <- fr_style(align = "decimal")
  expect_equal(s$align, "decimal")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_row_style — additional paths
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_row_style stores all properties", {
  rs <- fr_row_style(
    rows = c(2L, 4L), bold = TRUE, italic = TRUE, underline = TRUE,
    fg = "#003366", bg = "#EBF5FB", font_size = 8,
    align = "right", valign = "bottom", height = 0.5
  )
  expect_s3_class(rs, "fr_cell_style")
  expect_equal(rs$type, "row")
  expect_equal(rs$rows, c(2L, 4L))
  expect_true(rs$bold)
  expect_true(rs$italic)
  expect_true(rs$underline)
  expect_equal(rs$fg, "#003366")
  expect_equal(rs$bg, "#EBF5FB")
  expect_equal(rs$font_size, 8)
  expect_equal(rs$align, "right")
  expect_equal(rs$valign, "bottom")
  expect_equal(rs$height, 0.5)
})

test_that("fr_row_style with rows=NULL targets all rows", {
  rs <- fr_row_style(bold = TRUE)
  expect_null(rs$rows)
})

test_that("fr_row_style validates valign", {
  expect_error(fr_row_style(valign = "center"), class = "rlang_error")
})

test_that("fr_row_style cols is always NULL (row type)", {
  rs <- fr_row_style(rows = 1L, bold = TRUE)
  expect_null(rs$cols)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_col_style — additional paths
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_col_style stores all properties", {
  cs <- fr_col_style(
    cols = c("placebo", "zom_50mg"), bold = TRUE, italic = TRUE,
    underline = TRUE, fg = "#003366", bg = "#EBF5FB",
    font_size = 11, align = "center", valign = "middle"
  )
  expect_s3_class(cs, "fr_cell_style")
  expect_equal(cs$type, "col")
  expect_equal(cs$cols, c("placebo", "zom_50mg"))
  expect_true(cs$bold)
  expect_true(cs$italic)
  expect_true(cs$underline)
  expect_equal(cs$fg, "#003366")
  expect_equal(cs$bg, "#EBF5FB")
  expect_equal(cs$font_size, 11)
  expect_equal(cs$align, "center")
  expect_equal(cs$valign, "middle")
})

test_that("fr_col_style validates valign", {
  expect_error(fr_col_style(valign = "center"), class = "rlang_error")
})

test_that("fr_col_style validates font_size", {
  expect_error(fr_col_style(font_size = -5), class = "rlang_error")
  expect_error(fr_col_style(font_size = 0), class = "rlang_error")
})

test_that("fr_col_style rows is always NULL (col type)", {
  cs <- fr_col_style(cols = "total")
  expect_null(cs$rows)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_styles — additional paths
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_styles errors when first arg is not an fr_spec", {
  expect_error(fr_styles("not a spec", fr_style(bold = TRUE)),
               class = "rlang_error")
})

test_that("fr_styles resolves fr_rows_matches in fr_style (cell type)", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style(
        rows = fr_rows_matches("characteristic", value = "Race"),
        cols = "total",
        bold = TRUE
      )
    )
  style <- spec$cell_styles[[1]]
  expect_equal(style$rows, 3L)
  expect_equal(style$cols, "total")
})

test_that("fr_styles with multiple non-style objects errors on each", {
  expect_error(
    fr_table(df_style) |> fr_styles(42),
    "not a style"
  )
  expect_error(
    fr_table(df_style) |> fr_styles(fr_style(bold = TRUE), "bad"),
    "not a style"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_style_if — conditional styling
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_style_if with formula condition and apply_to='row'", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        cols = "characteristic",
        condition = ~ .x == "Total",
        apply_to = "row",
        bold = TRUE
      )
    )
  expect_length(spec$cell_styles, 1L)
  style <- spec$cell_styles[[1]]
  expect_equal(style$type, "row")
  expect_equal(style$rows, 4L)
  expect_true(style$bold)
  expect_null(style$cols)
})

test_that("fr_style_if with formula condition and apply_to='cell'", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        cols = "characteristic",
        condition = ~ .x == "Total",
        apply_to = "cell",
        fg = "#CC0000"
      )
    )
  expect_length(spec$cell_styles, 1L)
  style <- spec$cell_styles[[1]]
  expect_equal(style$type, "cell")
  expect_equal(style$rows, 4L)
  expect_equal(style$cols, "characteristic")
})

test_that("fr_style_if zebra striping (cols=NULL, row-index condition)", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        condition = ~ (.x %% 2) == 0,
        bg = "#F5F5F5",
        apply_to = "row"
      )
    )
  expect_length(spec$cell_styles, 1L)
  style <- spec$cell_styles[[1]]
  expect_equal(style$type, "row")
  expect_equal(style$rows, c(2L, 4L))
  expect_equal(style$bg, "#F5F5F5")
})

test_that("fr_style_if zebra with apply_to='cell' and cols=NULL", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        condition = ~ (.x %% 2) == 1,
        bg = "#FFFFFF",
        apply_to = "cell"
      )
    )
  style <- spec$cell_styles[[1]]
  # cols=NULL + apply_to="cell" => type is "cell" but cols is NULL
  expect_equal(style$type, "cell")
  expect_equal(style$rows, c(1L, 3L))
})

test_that("fr_style_if with function (not formula)", {
  is_total <- function(x) x == "Total"
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        cols = "characteristic",
        condition = is_total,
        apply_to = "row",
        bold = TRUE, bg = "#E8E8E8"
      )
    )
  style <- spec$cell_styles[[1]]
  expect_equal(style$rows, 4L)
  expect_true(style$bold)
  expect_equal(style$bg, "#E8E8E8")
})

test_that("fr_style_if with multiple cols evaluates each independently", {
  spec <- df_pval |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        cols = c("treatment", "placebo"),
        condition = ~ grepl("^5", .x),
        apply_to = "cell",
        fg = "#999999"
      )
    )
  # Should produce 2 styles (one per column), each targeting matching rows
  expect_length(spec$cell_styles, 2L)
  expect_equal(spec$cell_styles[[1]]$cols, "treatment")
  expect_equal(spec$cell_styles[[2]]$cols, "placebo")
})

test_that("fr_style_if with multiple cols and apply_to='row'", {
  spec <- df_pval |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        cols = c("treatment", "placebo"),
        condition = ~ grepl("^5", .x),
        apply_to = "row",
        bold = TRUE
      )
    )
  # Each column produces one row-type style
  for (style in spec$cell_styles) {
    expect_equal(style$type, "row")
    expect_null(style$cols)
  }
})

test_that("fr_style_if no matches produces empty cell_styles", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        cols = "characteristic",
        condition = ~ .x == "NONEXISTENT",
        bold = TRUE
      )
    )
  expect_length(spec$cell_styles, 0L)
})

test_that("fr_style_if no matches with cols=NULL produces empty cell_styles", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        condition = ~ .x > 100,
        bg = "#FF0000",
        apply_to = "row"
      )
    )
  expect_length(spec$cell_styles, 0L)
})

test_that("fr_style_if errors on non-formula/function condition", {
  expect_error(
    fr_style_if(condition = "bad"),
    "formula"
  )
  expect_error(
    fr_style_if(condition = 42),
    "formula"
  )
})

test_that("fr_style_if validates apply_to", {
  expect_error(
    fr_style_if(condition = ~ TRUE, apply_to = "column"),
    class = "rlang_error"
  )
})

test_that("fr_style_if validates align", {
  expect_error(
    fr_style_if(condition = ~ TRUE, align = "justify"),
    class = "rlang_error"
  )
})

test_that("fr_style_if with valid align stores it", {
  s <- fr_style_if(condition = ~ TRUE, align = "center")
  expect_equal(s$align, "center")
})

test_that("fr_style_if resolves named colours", {
  s <- fr_style_if(condition = ~ TRUE, fg = "red", bg = "navy")
  expect_equal(s$fg, "#FF0000")
  expect_equal(s$bg, "#000080")
})

test_that("fr_style_if stores all style properties", {
  s <- fr_style_if(
    condition = ~ TRUE,
    cols = "x",
    apply_to = "row",
    bold = TRUE, italic = TRUE, underline = TRUE,
    fg = "#CC0000", bg = "#F0F0F0",
    font_size = 8, align = "right"
  )
  expect_s3_class(s, "fr_conditional_style")
  expect_s3_class(s, "fr_cell_style")
  expect_true(s$bold)
  expect_true(s$italic)
  expect_true(s$underline)
  expect_equal(s$fg, "#CC0000")
  expect_equal(s$bg, "#F0F0F0")
  expect_equal(s$font_size, 8)
  expect_equal(s$align, "right")
  expect_equal(s$cols, "x")
  expect_equal(s$apply_to, "row")
})

test_that("fr_style_if errors when column not found in data", {
  expect_error(
    df_style |>
      fr_table() |>
      fr_styles(
        fr_style_if(cols = "nonexistent", condition = ~ TRUE, bold = TRUE)
      ),
    "not found"
  )
})

test_that("fr_style_if condition error is caught and reported", {
  expect_error(
    df_style |>
      fr_table() |>
      fr_styles(
        fr_style_if(
          cols = "characteristic",
          condition = ~ stop("test error"),
          bold = TRUE
        )
      ),
    "Error evaluating"
  )
})

test_that("fr_style_if condition error with cols=NULL is caught", {
  expect_error(
    df_style |>
      fr_table() |>
      fr_styles(
        fr_style_if(
          condition = ~ stop("test error"),
          bold = TRUE
        )
      ),
    "Error evaluating"
  )
})

test_that("fr_style_if handles NA results in condition", {
  spec <- df_pval |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        cols = "pvalue",
        condition = ~ as.numeric(.x) < 0.05,
        apply_to = "row",
        bold = TRUE
      )
    )
  # Rows 1 and 3 have pvalue < 0.05 (0.042 and 0.003)
  style <- spec$cell_styles[[1]]
  expect_equal(sort(style$rows), c(1L, 3L))
})

test_that("fr_style_if with grepl pattern matching", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        cols = "characteristic",
        condition = ~ grepl("^(Age|Sex)", .x),
        apply_to = "row",
        bg = "#F0F0F0"
      )
    )
  style <- spec$cell_styles[[1]]
  expect_equal(sort(style$rows), c(1L, 2L))
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_style_explain — diagnostic tool
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_style_explain with no matching styles", {
  spec <- df_style |>
    fr_table()
  result <- fr_style_explain(spec, row = 1L, col = "characteristic")
  expect_length(result$layers, 0L)
  # Final should be defaults
  expect_false(result$final$bold)
  expect_false(result$final$italic)
  expect_equal(result$final$fg, "#000000")
})

test_that("fr_style_explain with col_style and row_style overlap", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_col_style(cols = "total", bg = "#EBF5FB"),
      fr_row_style(rows = 1L, bold = TRUE)
    )
  result <- fr_style_explain(spec, row = 1L, col = "total")
  expect_length(result$layers, 2L)
  expect_true(result$final$bold)
  expect_equal(result$final$bg, "#EBF5FB")
})

test_that("fr_style_explain with cell style override", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_col_style(cols = "total", bg = "#EBF5FB"),
      fr_row_style(rows = 1L, bg = "#FFF3CD", bold = TRUE),
      fr_style(region = "body", rows = 1L, cols = "total",
               fg = "#CC0000", italic = TRUE)
    )
  result <- fr_style_explain(spec, row = 1L, col = "total")
  expect_length(result$layers, 3L)
  expect_true(result$final$bold)
  expect_true(result$final$italic)
  expect_equal(result$final$fg, "#CC0000")
  # Row style overrides col style bg

  expect_equal(result$final$bg, "#FFF3CD")
})

test_that("fr_style_explain with numeric column index", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_row_style(rows = 1L, bold = TRUE)
    )
  # "total" is the 4th column
  result <- fr_style_explain(spec, row = 1L, col = 4L)
  expect_length(result$layers, 1L)
  expect_true(result$final$bold)
})

test_that("fr_style_explain errors on out-of-range column index", {
  spec <- df_style |> fr_table()
  expect_error(fr_style_explain(spec, row = 1L, col = 99L), "out of range")
})

test_that("fr_style_explain errors on out-of-range row", {
  spec <- df_style |> fr_table()
  expect_error(fr_style_explain(spec, row = 0L, col = 1L), "out of range")
  expect_error(fr_style_explain(spec, row = 999L, col = 1L), "out of range")
})

test_that("fr_style_explain errors on unknown column name", {
  spec <- df_style |> fr_table()
  expect_error(fr_style_explain(spec, row = 1L, col = "unknown"), "not found")
})

test_that("fr_style_explain errors when spec is not fr_spec", {
  expect_error(fr_style_explain("not a spec", row = 1L, col = 1L),
               class = "rlang_error")
})

test_that("fr_style_explain skips non-body/stub region styles", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style(region = "header", bold = TRUE),
      fr_row_style(rows = 1L, italic = TRUE)
    )
  result <- fr_style_explain(spec, row = 1L, col = "characteristic")
  # Only the row_style (body region) should match, not header style
  expect_length(result$layers, 1L)
  expect_true(result$final$italic)
  expect_false(result$final$bold)
})

test_that("fr_style_explain style with rows='all' matches any row", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_row_style(rows = "all", bg = "#F5F5F5")
    )
  result <- fr_style_explain(spec, row = 3L, col = "total")
  expect_length(result$layers, 1L)
  expect_equal(result$final$bg, "#F5F5F5")
})

test_that("fr_style_explain style with rows=NULL matches any row", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_col_style(cols = "total", bg = "#EBF5FB")
    )
  # col_style has rows=NULL, should match any row
  result <- fr_style_explain(spec, row = 2L, col = "total")
  expect_length(result$layers, 1L)
  expect_equal(result$final$bg, "#EBF5FB")
})

test_that("fr_style_explain row_style that doesn't match specific row", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_row_style(rows = 1L, bold = TRUE)
    )
  # Row 2 shouldn't match
  result <- fr_style_explain(spec, row = 2L, col = "total")
  expect_length(result$layers, 0L)
  expect_false(result$final$bold)
})

test_that("fr_style_explain col_style that doesn't match specific col", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_col_style(cols = "total", bg = "#EBF5FB")
    )
  result <- fr_style_explain(spec, row = 1L, col = "characteristic")
  expect_length(result$layers, 0L)
})

test_that("fr_style_explain shows font_size, align, valign, indent overrides", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style(region = "body", rows = 1L, cols = "total",
               font_size = 8, align = "right", valign = "bottom",
               indent = 0.5)
    )
  result <- fr_style_explain(spec, row = 1L, col = "total")
  expect_equal(result$final$font_size, 8)
  expect_equal(result$final$align, "right")
  expect_equal(result$final$valign, "bottom")
  expect_equal(result$final$indent, 0.5)
})

test_that("fr_style_explain picks up column default alignment", {
  spec <- df_style |>
    fr_table() |>
    fr_cols(total = fr_col("Total", align = "center"))
  result <- fr_style_explain(spec, row = 1L, col = "total")
  expect_equal(result$final$align, "center")
})

test_that("fr_style_explain stub region style matches body cells", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style(region = "stub", bold = TRUE)
    )
  # "stub" is the first column = "characteristic"
  result <- fr_style_explain(spec, row = 1L, col = "characteristic")
  expect_length(result$layers, 1L)
  expect_true(result$final$bold)
})


# ══════════════════════════════════════════════════════════════════════════════
# Integration: fr_style_if through fr_style_explain
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_style_if resolved styles are visible in fr_style_explain", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        cols = "characteristic",
        condition = ~ .x == "Total",
        apply_to = "row",
        bold = TRUE, bg = "#FFF3CD"
      )
    )
  result <- fr_style_explain(spec, row = 4L, col = "total")
  expect_true(result$final$bold)
  expect_equal(result$final$bg, "#FFF3CD")

  # Non-matching row should not have the style
  result2 <- fr_style_explain(spec, row = 1L, col = "total")
  expect_false(result2$final$bold)
})


# ══════════════════════════════════════════════════════════════════════════════
# Edge cases and mixed scenarios
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_styles handles mix of regular and conditional styles", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_col_style(cols = "total", bg = "#EBF5FB"),
      fr_style_if(
        cols = "characteristic",
        condition = ~ .x == "Total",
        apply_to = "row",
        bold = TRUE
      ),
      fr_row_style(rows = 1L, italic = TRUE)
    )
  # Should have 3 resolved styles
  expect_true(length(spec$cell_styles) >= 3L)
})

test_that("fr_styles accumulates conditional styles across calls", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style_if(cols = "characteristic", condition = ~ .x == "Age",
                  bold = TRUE, apply_to = "row")
    ) |>
    fr_styles(
      fr_style_if(cols = "characteristic", condition = ~ .x == "Total",
                  bg = "#F0F0F0", apply_to = "row")
    )
  expect_length(spec$cell_styles, 2L)
})

test_that("multiple fr_rows_matches in one fr_styles call", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_row_style(
        rows = fr_rows_matches("characteristic", value = "Age"),
        bg = "#E8F4FD"
      ),
      fr_row_style(
        rows = fr_rows_matches("characteristic", value = "Total"),
        bold = TRUE
      )
    )
  expect_length(spec$cell_styles, 2L)
  expect_equal(spec$cell_styles[[1]]$rows, 1L)
  expect_equal(spec$cell_styles[[2]]$rows, 4L)
})

test_that("fr_style_if with numeric column condition", {
  df_num <- data.frame(
    label = c("A", "B", "C"),
    value = c(10, 50, 90),
    stringsAsFactors = FALSE
  )
  spec <- df_num |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        cols = "value",
        condition = ~ .x > 40,
        apply_to = "cell",
        fg = "#CC0000"
      )
    )
  expect_length(spec$cell_styles, 1L)
  style <- spec$cell_styles[[1]]
  expect_equal(sort(style$rows), c(2L, 3L))
  expect_equal(style$cols, "value")
})


# ══════════════════════════════════════════════════════════════════════════════
# Tidyselect integration — cols parameter
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_col_style with starts_with() resolves in fr_styles", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_col_style(cols = starts_with("zom_"), bg = "#EBF5FB")
    )
  expect_length(spec$cell_styles, 1L)
  expect_equal(spec$cell_styles[[1]]$cols, "zom_50mg")
})

test_that("fr_col_style with contains() resolves in fr_styles", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_col_style(cols = contains("50"), italic = TRUE)
    )
  expect_length(spec$cell_styles, 1L)
  expect_equal(spec$cell_styles[[1]]$cols, "zom_50mg")
})

test_that("fr_style with starts_with() resolves in fr_styles", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style(cols = starts_with("zom_"), bold = TRUE)
    )
  expect_length(spec$cell_styles, 1L)
  expect_equal(spec$cell_styles[[1]]$cols, "zom_50mg")
})

test_that("fr_style_if with starts_with() resolves in fr_styles", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_style_if(
        cols = starts_with("zom_"),
        condition = ~ grepl("36", .x),
        apply_to = "cell",
        fg = "#CC0000"
      )
    )
  expect_length(spec$cell_styles, 1L)
  expect_equal(spec$cell_styles[[1]]$cols, "zom_50mg")
})

test_that("fr_col_style with everything() resolves to all columns", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_col_style(cols = everything(), bg = "#F5F5F5")
    )
  expect_length(spec$cell_styles, 1L)
  expect_equal(spec$cell_styles[[1]]$cols, names(df_style))
})

test_that("fr_col_style with matches() regex resolves in fr_styles", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_col_style(cols = matches("^(placebo|total)$"), align = "right")
    )
  expect_length(spec$cell_styles, 1L)
  expect_equal(spec$cell_styles[[1]]$cols, c("placebo", "total"))
})

test_that("tidyselect deferred: standalone style object resolves later", {
  # Create style outside pipeline (no data context)
  my_style <- fr_col_style(cols = starts_with("zom_"), bg = "#EBF5FB")
  # cols should be stored as a quosure
  expect_true(rlang::is_quosure(my_style$cols))

  # Resolve when applied via fr_styles
  spec <- df_style |>
    fr_table() |>
    fr_styles(my_style)
  expect_equal(spec$cell_styles[[1]]$cols, "zom_50mg")
})

test_that("character cols still work as before (no regression)", {
  spec <- df_style |>
    fr_table() |>
    fr_styles(
      fr_col_style(cols = c("placebo", "total"), bg = "#F0F0F0")
    )
  expect_length(spec$cell_styles, 1L)
  expect_equal(spec$cell_styles[[1]]$cols, c("placebo", "total"))
})

test_that("NULL cols still targets all columns (no regression)", {
  style <- fr_col_style(bg = "#F0F0F0")
  expect_null(style$cols)
})
