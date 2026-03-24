# ─────────────────────────────────────────────────────────────────────────────
# test-api.R — Tests for api.R pipeline verbs
# ─────────────────────────────────────────────────────────────────────────────

# Shared fixture: minimal data frames used across all tests
df_simple <- data.frame(
  arm = c("Placebo", "Drug"),
  n = c(45L, 45L),
  pct = c(50.0, 50.0),
  stringsAsFactors = FALSE
)

df_ae <- data.frame(
  AEBODSYS = c("Nervous system disorders", "Gastrointestinal disorders"),
  AEDECOD = c("Headache", "Nausea"),
  n = c(10L, 15L),
  stringsAsFactors = FALSE
)


# ══════════════════════════════════════════════════════════════════════════════
# fr_table
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_table creates fr_spec from data frame", {
  spec <- fr_table(df_simple)
  expect_s3_class(spec, "fr_spec")
  expect_identical(spec$data, df_simple)
})

test_that("fr_table spec has default meta, page, body, header", {
  fr_config_reset()
  withr::defer(fr_config_reset())
  spec <- fr_table(df_simple)
  expect_s3_class(spec$meta, "fr_meta")
  expect_s3_class(spec$page, "fr_page")
  expect_s3_class(spec$body, "fr_body")
  expect_s3_class(spec$header, "fr_header")
  expect_equal(spec$meta$titles, list())
  expect_equal(spec$meta$footnotes, list())
  expect_equal(spec$rules, list())
  expect_equal(spec$cell_styles, list())
  expect_null(spec$pagehead)
  expect_null(spec$pagefoot)
})

test_that("fr_table errors on non-data.frame", {
  expect_error(fr_table(list(a = 1)), class = "rlang_error")
  expect_error(fr_table("not a df"), class = "rlang_error")
  expect_error(fr_table(1:10), class = "rlang_error")
  expect_error(fr_table(NULL), class = "rlang_error")
})

test_that("fr_table works with built-in adsl dataset", {
  spec <- fr_table(adsl)
  expect_s3_class(spec, "fr_spec")
  expect_equal(nrow(spec$data), 135L)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_titles
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_titles adds one title", {
  spec <- fr_table(df_simple) |> fr_titles("Table 14.1.1")
  expect_length(spec$meta$titles, 1L)
  expect_equal(spec$meta$titles[[1]]$content, "Table 14.1.1")
  expect_equal(spec$meta$titles[[1]]$align, "center")
  expect_false(spec$meta$titles[[1]]$bold)
})

test_that("fr_titles adds multiple titles", {
  spec <- fr_table(df_simple) |>
    fr_titles("Title Line 1", "Title Line 2", "Title Line 3")
  expect_length(spec$meta$titles, 3L)
  expect_equal(spec$meta$titles[[2]]$content, "Title Line 2")
})

test_that("fr_titles applies .align and .bold to all titles", {
  spec <- fr_table(df_simple) |>
    fr_titles("T1", "T2", .align = "left", .bold = TRUE)
  expect_equal(spec$meta$titles[[1]]$align, "left")
  expect_true(spec$meta$titles[[1]]$bold)
  expect_equal(spec$meta$titles[[2]]$align, "left")
})

test_that("fr_titles replaces titles on second call", {
  spec <- fr_table(df_simple) |>
    fr_titles("First Title") |>
    fr_titles("Second Title")
  expect_length(spec$meta$titles, 1L)
  expect_equal(spec$meta$titles[[1]]$content, "Second Title")
})

test_that("fr_titles with list allows per-title styling", {
  spec <- fr_table(df_simple) |>
    fr_titles(
      list("Bold Title", bold = TRUE, align = "left"),
      "Plain Title"
    )
  expect_true(spec$meta$titles[[1]]$bold)
  expect_equal(spec$meta$titles[[1]]$align, "left")
  expect_false(spec$meta$titles[[2]]$bold)
  expect_equal(spec$meta$titles[[2]]$align, "center") # default
})

test_that("fr_titles with no titles returns spec unchanged", {
  spec <- fr_table(df_simple)
  spec2 <- fr_titles(spec)
  expect_equal(spec2$meta$titles, list())
})

test_that("fr_titles errors on invalid .align", {
  expect_error(
    fr_table(df_simple) |> fr_titles("T", .align = "justify"),
    class = "rlang_error"
  )
})

test_that("fr_titles errors on non-fr_spec input", {
  expect_error(fr_titles(list(), "T"), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_footnotes
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_footnotes adds one footnote", {
  spec <- fr_table(df_simple) |> fr_footnotes("Source: ADSL")
  expect_length(spec$meta$footnotes, 1L)
  expect_equal(spec$meta$footnotes[[1]]$content, "Source: ADSL")
  expect_equal(spec$meta$footnotes[[1]]$align, "left")
  expect_equal(spec$meta$footnotes[[1]]$placement, "every")
})

test_that("fr_footnotes adds multiple footnotes", {
  spec <- fr_table(df_simple) |>
    fr_footnotes("Note 1", "Note 2")
  expect_length(spec$meta$footnotes, 2L)
})

test_that("fr_footnotes replaces on second call", {
  spec <- fr_table(df_simple) |>
    fr_footnotes("First") |>
    fr_footnotes("Second")
  expect_length(spec$meta$footnotes, 1L)
  expect_equal(spec$meta$footnotes[[1]]$content, "Second")
})

test_that("fr_footnotes sets separator flag", {
  spec <- fr_table(df_simple) |> fr_footnotes("Note", .separator = FALSE)
  expect_false(spec$meta$footnote_separator)
})

test_that("fr_footnotes placement = 'last' works", {
  spec <- fr_table(df_simple) |>
    fr_footnotes(list("End note", placement = "last"))
  expect_equal(spec$meta$footnotes[[1]]$placement, "last")
})

test_that("fr_footnotes errors on invalid .placement", {
  expect_error(
    fr_table(df_simple) |> fr_footnotes("N", .placement = "first"),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_cols
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_cols with no args builds defaults for all columns", {
  spec <- fr_table(df_simple) |> fr_cols()
  expect_length(spec$columns, 3L)
  expect_named(spec$columns, c("arm", "n", "pct"))
  expect_s3_class(spec$columns[["arm"]], "fr_col")
})

test_that("fr_cols sets id from column name", {
  spec <- fr_table(df_simple) |> fr_cols()
  expect_equal(spec$columns[["arm"]]$id, "arm")
})

test_that("fr_cols uses user-supplied fr_col", {
  spec <- fr_table(df_simple) |>
    fr_cols(arm = fr_col("Treatment", width = 2.0, align = "center"))
  expect_equal(spec$columns[["arm"]]$label, "Treatment")
  expect_equal(spec$columns[["arm"]]$width, 2.0)
  expect_equal(spec$columns[["arm"]]$align, "center")
})

test_that("fr_cols fills defaults for unconfigured columns", {
  spec <- fr_table(df_simple) |>
    fr_cols(arm = fr_col("Treatment", width = 2.0))
  # n and pct should still exist with defaults
  expect_s3_class(spec$columns[["n"]], "fr_col")
  expect_s3_class(spec$columns[["pct"]], "fr_col")
})

test_that("fr_cols auto-aligns numeric columns to right", {
  spec <- fr_table(df_simple) |> fr_cols()
  expect_equal(spec$columns[["n"]]$align, "right")
  expect_equal(spec$columns[["pct"]]$align, "right")
  expect_equal(spec$columns[["arm"]]$align, "left")
})

test_that("fr_cols errors on unnamed argument", {
  expect_error(
    fr_table(df_simple) |> fr_cols(fr_col("X")),
    class = "rlang_error"
  )
})

test_that("fr_cols errors on column not in data", {
  expect_error(
    fr_table(df_simple) |> fr_cols(NONEXISTENT = fr_col("X")),
    class = "rlang_error"
  )
})

test_that("fr_cols accepts string shorthand", {
  spec <- fr_table(df_simple) |> fr_cols(arm = "Treatment Arm")
  expect_equal(spec$columns[["arm"]]$label, "Treatment Arm")
})

test_that("fr_cols errors on non-fr_col non-string value", {
  expect_error(
    fr_table(df_simple) |> fr_cols(arm = 42L),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_spans
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_spans adds a span", {
  spec <- fr_table(df_simple) |>
    fr_spans("Numbers" = c("n", "pct"))
  expect_length(spec$header$spans, 1L)
  expect_s3_class(spec$header$spans[[1]], "fr_span")
  expect_equal(spec$header$spans[[1]]$label, "Numbers")
  expect_equal(spec$header$spans[[1]]$columns, c("n", "pct"))
  expect_equal(spec$header$spans[[1]]$level, 1L)
})

test_that("fr_spans appends on second call", {
  spec <- fr_table(df_simple) |>
    fr_spans("A" = "n") |>
    fr_spans("B" = "pct")
  expect_length(spec$header$spans, 2L)
})

test_that("fr_spans respects .level", {
  spec <- fr_table(df_simple) |>
    fr_spans("Top" = c("n", "pct"), .level = 2L)
  expect_equal(spec$header$spans[[1]]$level, 2L)
})

test_that("fr_spans with no args returns spec unchanged", {
  spec <- fr_table(df_simple)
  spec2 <- fr_spans(spec)
  expect_equal(spec2$header$spans, list())
})

test_that("fr_spans errors on unnamed argument", {
  expect_error(
    fr_table(df_simple) |> fr_spans(c("n", "pct")),
    class = "rlang_error"
  )
})

test_that("fr_spans errors on column not in data", {
  expect_error(
    fr_table(df_simple) |> fr_spans("X" = c("n", "NONEXISTENT")),
    class = "rlang_error"
  )
})

test_that("fr_spans errors on empty column vector", {
  expect_error(
    fr_table(df_simple) |> fr_spans("X" = character(0)),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_rows
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_rows with no args sets empty fr_body", {
  spec <- fr_table(df_ae) |> fr_rows()
  expect_s3_class(spec$body, "fr_body")
  expect_equal(spec$body$page_by, character(0))
})

test_that("fr_rows sets page_by", {
  spec <- fr_table(df_ae) |> fr_rows(page_by = "AEBODSYS")
  expect_equal(spec$body$page_by, "AEBODSYS")
})

test_that("fr_rows sets all body options", {
  # page_by == group_by warns but still stores values
  expect_warning(
    fr_table(df_ae) |>
      fr_rows(
        page_by = "AEBODSYS",
        group_by = "AEBODSYS",
        blank_after = "AEBODSYS"
      ),
    "page_by.*group_by.*share"
  )
  spec <- suppressWarnings(
    fr_table(df_ae) |>
      fr_rows(
        page_by = "AEBODSYS",
        group_by = "AEBODSYS",
        blank_after = "AEBODSYS"
      )
  )
  expect_equal(spec$body$page_by, "AEBODSYS")
  expect_equal(spec$body$group_by, "AEBODSYS")
  expect_equal(spec$body$blank_after, "AEBODSYS")
})

test_that("fr_rows replaces body on second call", {
  spec <- fr_table(df_ae) |>
    fr_rows(page_by = "AEBODSYS") |>
    fr_rows(page_by = "AEDECOD")
  expect_equal(spec$body$page_by, "AEDECOD")
})

test_that("fr_rows errors on non-character column name", {
  expect_error(
    fr_table(df_ae) |> fr_rows(page_by = 1L),
    class = "rlang_error"
  )
})

test_that("fr_rows errors on column not in data", {
  expect_error(
    fr_table(df_ae) |> fr_rows(page_by = "NONEXISTENT"),
    class = "rlang_error"
  )
})

test_that("page_by columns are auto-hidden when visible is NULL", {
  spec <- fr_table(df_ae) |>
    fr_rows(page_by = "AEBODSYS")
  fspec <- arframe:::finalize_spec(spec)
  expect_false(fspec$columns[["AEBODSYS"]]$visible)
})

test_that("explicit visible = TRUE on page_by column is respected", {
  spec <- fr_table(df_ae) |>
    fr_cols(AEBODSYS = fr_col(visible = TRUE)) |>
    fr_rows(page_by = "AEBODSYS")
  fspec <- arframe:::finalize_spec(spec)
  expect_true(fspec$columns[["AEBODSYS"]]$visible)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_page (pipeline verb)
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_page sets orientation", {
  spec <- fr_table(df_simple) |> fr_page(orientation = "portrait")
  expect_equal(spec$page$orientation, "portrait")
})

test_that("fr_page sets font_size", {
  spec <- fr_table(df_simple) |> fr_page(font_size = 10)
  expect_equal(spec$page$font_size, 10)
})

test_that("fr_page preserves unchanged options", {
  spec <- fr_table(df_simple) |>
    fr_page(orientation = "portrait", font_size = 8) |>
    fr_page(paper = "a4")
  # orientation and font_size should still be from first call
  expect_equal(spec$page$orientation, "portrait")
  expect_equal(spec$page$font_size, 8)
  expect_equal(spec$page$paper, "a4")
})

test_that("fr_page sets margins", {
  spec <- fr_table(df_simple) |> fr_page(margins = c(1, 0.75))
  expect_equal(spec$page$margins$top, 1)
  expect_equal(spec$page$margins$left, 0.75)
  expect_equal(spec$page$margins$right, 0.75)
  expect_equal(spec$page$margins$bottom, 1)
})

test_that("fr_page sets custom tokens", {
  spec <- fr_table(df_simple) |>
    fr_page(tokens = list(study = "ABC-001"))
  expect_equal(spec$page$tokens$study, "ABC-001")
})

test_that("fr_page errors on invalid orientation", {
  expect_error(
    fr_table(df_simple) |> fr_page(orientation = "diagonal"),
    class = "rlang_error"
  )
})

test_that("fr_page errors on invalid paper", {
  expect_error(
    fr_table(df_simple) |> fr_page(paper = "tabloid"),
    class = "rlang_error"
  )
})

test_that("fr_page errors on overriding readonly token", {
  expect_error(
    fr_table(df_simple) |> fr_page(tokens = list(thepage = "1")),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_pagehead / fr_pagefoot
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_pagehead sets left/center/right zones", {
  spec <- fr_table(df_simple) |>
    fr_pagehead(
      left = "Study ABC",
      center = "CONFIDENTIAL",
      right = "Page {thepage}"
    )
  expect_s3_class(spec$pagehead, "fr_pagechrome")
  expect_equal(spec$pagehead$left, "Study ABC")
  expect_equal(spec$pagehead$center, "CONFIDENTIAL")
  expect_equal(spec$pagehead$right, "Page {thepage}")
})

test_that("fr_pagehead defaults: pagehead is NULL before being set", {
  fr_config_reset()
  withr::defer(fr_config_reset())
  spec <- fr_table(df_simple)
  expect_null(spec$pagehead)
})

test_that("fr_pagehead bold flag", {
  spec <- fr_table(df_simple) |> fr_pagehead(left = "X", bold = TRUE)
  expect_true(spec$pagehead$bold)
})

test_that("fr_pagefoot sets zones independently", {
  spec <- fr_table(df_simple) |>
    fr_pagefoot(left = "{program}", right = "{datetime}")
  expect_s3_class(spec$pagefoot, "fr_pagechrome")
  expect_equal(spec$pagefoot$left, "{program}")
  expect_equal(spec$pagefoot$right, "{datetime}")
  expect_null(spec$pagefoot$center)
})

test_that("fr_pagehead replaces on second call", {
  spec <- fr_table(df_simple) |>
    fr_pagehead(left = "First") |>
    fr_pagehead(left = "Second")
  expect_equal(spec$pagehead$left, "Second")
})

test_that("fr_pagehead errors on invalid bold", {
  expect_error(
    fr_table(df_simple) |> fr_pagehead(bold = "yes"),
    class = "rlang_error"
  )
})

test_that("fr_pagehead errors on invalid font_size", {
  expect_error(
    fr_table(df_simple) |> fr_pagehead(left = "X", font_size = -1),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_hlines
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_hlines booktabs creates 3 rules", {
  spec <- fr_table(df_simple) |> fr_hlines("booktabs")
  expect_length(spec$rules, 3L)
  expect_s3_class(spec$rules[[1]], "fr_rule")
})

test_that("fr_hlines header creates 1 rule below header", {
  spec <- fr_table(df_simple) |> fr_hlines("header")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_length(hlines, 1L)
  expect_equal(hlines[[1]]$region, "header")
  expect_equal(hlines[[1]]$side, "below")
})

test_that("fr_hlines open creates 2 rules (above + below header)", {
  spec <- fr_table(df_simple) |> fr_hlines("open")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_length(hlines, 2L)
  expect_equal(hlines[[1]]$side, "above")
  expect_equal(hlines[[2]]$side, "below")
})

test_that("fr_hlines booktabs rule widths are correct", {
  spec <- fr_table(df_simple) |> fr_hlines("booktabs")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  widths <- vapply(hlines, `[[`, numeric(1), "width")
  expect_equal(widths, c(1.0, 0.5, 1.0))
})

test_that("fr_hlines hsides creates 2 rules", {
  spec <- fr_table(df_simple) |> fr_hlines("hsides")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_length(hlines, 2L)
})

test_that("fr_hlines above creates 1 rule", {
  spec <- fr_table(df_simple) |> fr_hlines("above")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_length(hlines, 1L)
  expect_equal(hlines[[1]]$side, "above")
})

test_that("fr_hlines void clears all horizontal rules", {
  spec <- fr_table(df_simple) |>
    fr_hlines("booktabs") |>
    fr_hlines("void")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_length(hlines, 0L)
})

test_that("fr_hlines void preserves existing vertical rules", {
  spec <- fr_table(df_simple) |>
    fr_vlines("box") |>
    fr_hlines("booktabs") |>
    fr_hlines("void")
  # vlines should still be present
  vlines <- Filter(function(r) inherits(r, "fr_vline_spec"), spec$rules)
  expect_length(vlines, 1L)
})

test_that("fr_hlines box creates fr_rule_box marker", {
  spec <- fr_table(df_simple) |> fr_hlines("box")
  expect_true(any(vapply(spec$rules, inherits, logical(1), "fr_rule_box")))
})

test_that("fr_hlines replaces horizontal rules on second call", {
  spec <- fr_table(df_simple) |>
    fr_hlines("booktabs") |>
    fr_hlines("hsides")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_length(hlines, 2L)
})

test_that("fr_hlines width overrides rule width", {
  spec <- fr_table(df_simple) |> fr_hlines("header", width = "thick")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_equal(hlines[[1]]$width, 1.5)
})

test_that("fr_hlines width accepts numeric", {
  spec <- fr_table(df_simple) |> fr_hlines("header", width = 0.75)
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_equal(hlines[[1]]$width, 0.75)
})

test_that("fr_hlines color overrides rule color", {
  spec <- fr_table(df_simple) |> fr_hlines("header", color = "#003366")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_equal(hlines[[1]]$fg, "#003366")
})

test_that("fr_hlines linestyle overrides rule style", {
  spec <- fr_table(df_simple) |> fr_hlines("header", linestyle = "dashed")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_equal(hlines[[1]]$linestyle, "dashed")
})

test_that("fr_hlines linestyle accepts dashdot", {
  spec <- fr_table(df_simple) |> fr_hlines("header", linestyle = "dashdot")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_equal(hlines[[1]]$linestyle, "dashdot")
})

test_that("fr_hlines errors on unknown preset", {
  expect_error(fr_table(df_simple) |> fr_hlines("fancy"), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_vlines
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_vlines creates fr_vline_spec", {
  spec <- fr_table(df_simple) |> fr_vlines("box")
  vlines <- Filter(function(r) inherits(r, "fr_vline_spec"), spec$rules)
  expect_length(vlines, 1L)
  expect_equal(vlines[[1]]$preset, "box")
})

test_that("fr_vlines void clears all vertical rules", {
  spec <- fr_table(df_simple) |>
    fr_vlines("box") |>
    fr_vlines("void")
  vlines <- Filter(function(r) inherits(r, "fr_vline_spec"), spec$rules)
  expect_length(vlines, 0L)
})

test_that("fr_vlines void preserves horizontal rules", {
  spec <- fr_table(df_simple) |>
    fr_hlines("header") |>
    fr_vlines("box") |>
    fr_vlines("void")
  hlines <- Filter(
    function(r) inherits(r, "fr_rule") && r$direction == "horizontal",
    spec$rules
  )
  expect_length(hlines, 1L)
})

test_that("fr_vlines replaces vlines on second call", {
  spec <- fr_table(df_simple) |>
    fr_vlines("box") |>
    fr_vlines("all")
  vlines <- Filter(function(r) inherits(r, "fr_vline_spec"), spec$rules)
  expect_length(vlines, 1L)
  expect_equal(vlines[[1]]$preset, "all")
})

test_that("fr_vlines accepts custom cols", {
  spec <- fr_table(df_simple) |> fr_vlines("void") |> fr_vlines(cols = 1L)
  vlines <- Filter(function(r) inherits(r, "fr_vline_spec"), spec$rules)
  expect_length(vlines, 1L)
  expect_equal(vlines[[1]]$cols, 1L)
})

test_that("fr_vlines width and linestyle stored", {
  spec <- fr_table(df_simple) |>
    fr_vlines("box", width = "thick", linestyle = "dashed")
  vlines <- Filter(function(r) inherits(r, "fr_vline_spec"), spec$rules)
  expect_equal(vlines[[1]]$width, 1.5)
  expect_equal(vlines[[1]]$linestyle, "dashed")
})

test_that("fr_vlines errors on unknown preset", {
  expect_error(fr_table(df_simple) |> fr_vlines("sides"), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_style / fr_styles
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_style creates fr_cell_style with type=cell", {
  s <- fr_style(region = "header", bold = TRUE)
  expect_s3_class(s, "fr_cell_style")
  expect_equal(s$type, "cell")
  expect_equal(s$region, "header")
  expect_true(s$bold)
})

test_that("fr_style defaults region to body", {
  s <- fr_style()
  expect_equal(s$region, "body")
})

test_that("fr_style accepts stub region", {
  s <- fr_style(region = "stub", bold = TRUE)
  expect_equal(s$region, "stub")
})

test_that("fr_style NULL properties are NULL", {
  s <- fr_style()
  expect_null(s$bold)
  expect_null(s$color)
  expect_null(s$background)
  expect_null(s$colspan)
  expect_null(s$rowspan)
})

test_that("fr_style stores colspan and rowspan", {
  s <- fr_style(colspan = 3L, rowspan = 2L)
  expect_equal(s$colspan, 3L)
  expect_equal(s$rowspan, 2L)
})

test_that("fr_style validates align", {
  expect_error(fr_style(align = "justify"), class = "rlang_error")
})

test_that("fr_style validates font_size", {
  expect_error(fr_style(font_size = -1), class = "rlang_error")
})


# ── fr_row_style ─────────────────────────────────────────────────────────────

test_that("fr_row_style creates row-type fr_cell_style", {
  rs <- fr_row_style(rows = 1L, bold = TRUE, background = "#EEEEEE")
  expect_s3_class(rs, "fr_cell_style")
  expect_equal(rs$type, "row")
  expect_equal(rs$region, "body")
  expect_equal(rs$rows, 1L)
  expect_true(rs$bold)
  expect_equal(rs$background, "#EEEEEE")
  expect_null(rs$cols)
})

test_that("fr_row_style rows=all is stored", {
  rs <- fr_row_style(rows = "all", color = "#CC0000")
  expect_equal(rs$rows, "all")
})

test_that("fr_row_style height stored", {
  rs <- fr_row_style(height = 0.3)
  expect_equal(rs$height, 0.3)
})

test_that("fr_row_style validates align", {
  expect_error(fr_row_style(align = "justify"), class = "rlang_error")
})

test_that("fr_row_style validates font_size", {
  expect_error(fr_row_style(font_size = 0), class = "rlang_error")
})

test_that("fr_row_style validates height", {
  expect_error(fr_row_style(height = 0), class = "rlang_error")
  expect_error(fr_row_style(height = -1), class = "rlang_error")
})


# ── fr_col_style ─────────────────────────────────────────────────────────────

test_that("fr_col_style creates col-type fr_cell_style", {
  cs <- fr_col_style(cols = "total", background = "#EBF5FB")
  expect_s3_class(cs, "fr_cell_style")
  expect_equal(cs$type, "col")
  expect_equal(cs$region, "body")
  expect_equal(cs$cols, "total")
  expect_equal(cs$background, "#EBF5FB")
  expect_null(cs$rows)
})

test_that("fr_col_style cols=NULL targets all columns", {
  cs <- fr_col_style(bold = TRUE)
  expect_null(cs$cols)
})

test_that("fr_col_style validates align", {
  expect_error(fr_col_style(align = "justify"), class = "rlang_error")
})


# ── fr_styles ─────────────────────────────────────────────────────────────────

test_that("fr_styles appends mixed style types", {
  spec <- fr_table(df_simple) |>
    fr_styles(
      fr_style(region = "header", bold = TRUE),
      fr_row_style(rows = 1L, background = "#EEEEEE"),
      fr_col_style(cols = "a", color = "#003366")
    )
  expect_length(spec$cell_styles, 3L)
  expect_equal(spec$cell_styles[[1]]$type, "cell")
  expect_equal(spec$cell_styles[[2]]$type, "row")
  expect_equal(spec$cell_styles[[3]]$type, "col")
})

test_that("fr_styles accumulates across calls", {
  spec <- fr_table(df_simple) |>
    fr_styles(fr_style(region = "header", bold = TRUE)) |>
    fr_styles(fr_row_style(rows = 1L, background = "#F0F0F0"))
  expect_length(spec$cell_styles, 2L)
})

test_that("fr_styles with no args returns spec unchanged", {
  spec <- fr_table(df_simple)
  spec2 <- fr_styles(spec)
  expect_equal(spec2$cell_styles, list())
})

test_that("fr_styles errors on non-fr_cell_style input", {
  expect_error(
    fr_table(df_simple) |> fr_styles(list(bold = TRUE)),
    class = "rlang_error"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# Full pipeline integration
# ══════════════════════════════════════════════════════════════════════════════

test_that("full pipeline assembles correct fr_spec", {
  spec <- adsl |>
    fr_table() |>
    fr_titles("Table 14.1.1", "Full Analysis Set") |>
    fr_footnotes("Source: ADSL", .separator = TRUE) |>
    fr_cols(ARM = fr_col("Treatment Arm", width = 2.0)) |>
    fr_spans("Demographics" = c("AGE", "SEX")) |>
    fr_rows(page_by = "ARM") |>
    fr_page(orientation = "landscape", font_size = 9) |>
    fr_pagehead(left = "TFRM-2024-001", right = "Page {thepage}") |>
    fr_pagefoot(left = "{program}", right = "{datetime}") |>
    fr_hlines("booktabs") |>
    fr_styles(fr_style(region = "header", bold = TRUE))

  expect_s3_class(spec, "fr_spec")
  expect_length(spec$meta$titles, 2L)
  expect_length(spec$meta$footnotes, 1L)
  expect_length(spec$header$spans, 1L)
  expect_equal(spec$body$page_by, "ARM")
  expect_length(spec$rules, 3L)
  expect_length(spec$cell_styles, 1L)
  expect_s3_class(spec$pagehead, "fr_pagechrome")
  expect_s3_class(spec$pagefoot, "fr_pagechrome")
  expect_equal(spec$page$font_size, 9)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_style_explain
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_style_explain returns matching layers", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_styles(
      fr_col_style(cols = "total", background = "#EBF5FB"),
      fr_row_style(rows = 1L, bold = TRUE)
    )
  result <- fr_style_explain(spec, row = 1L, col = "total")
  expect_type(result, "list")
  expect_named(result, c("final", "layers"))
  # Should match both col_style (total) and row_style (row 1)
  expect_equal(length(result$layers), 2L)
  expect_true(result$final$bold)
  expect_equal(result$final$background, "#EBF5FB")
})

test_that("fr_style_explain errors on invalid row/col", {
  spec <- tbl_demog |> fr_table()
  expect_error(fr_style_explain(spec, row = 999L, col = 1L))
  expect_error(fr_style_explain(spec, row = 1L, col = "nonexistent"))
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_titles — additional coverage
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_titles list form with 'content' key", {
  spec <- fr_table(df_simple) |>
    fr_titles(list(content = "Table 1"))
  expect_equal(spec$meta$titles[[1]]$content, "Table 1")
})

test_that("fr_titles list form with unnamed first element", {
  spec <- fr_table(df_simple) |>
    fr_titles(list("Table 1", bold = TRUE))
  expect_equal(spec$meta$titles[[1]]$content, "Table 1")
  expect_true(spec$meta$titles[[1]]$bold)
})

test_that("fr_titles list form per-line align override", {
  spec <- fr_table(df_simple) |>
    fr_titles(
      list("Left Title", align = "left"),
      list("Right Title", align = "right")
    )
  expect_equal(spec$meta$titles[[1]]$align, "left")
  expect_equal(spec$meta$titles[[2]]$align, "right")
})

test_that("fr_titles list form per-line font_size override", {
  spec <- fr_table(df_simple) |>
    fr_titles(
      list("Big Title", font_size = 12),
      list("Small Title", font_size = 8)
    )
  expect_equal(spec$meta$titles[[1]]$font_size, 12)
  expect_equal(spec$meta$titles[[2]]$font_size, 8)
})

test_that("fr_titles .bold = TRUE applies to all string entries", {
  spec <- fr_table(df_simple) |>
    fr_titles("T1", "T2", .bold = TRUE)
  expect_true(spec$meta$titles[[1]]$bold)
  expect_true(spec$meta$titles[[2]]$bold)
})

test_that("fr_titles .font_size applies to all string entries", {
  spec <- fr_table(df_simple) |>
    fr_titles("T1", "T2", .font_size = 11)
  expect_equal(spec$meta$titles[[1]]$font_size, 11)
  expect_equal(spec$meta$titles[[2]]$font_size, 11)
})

test_that("fr_titles list form bold overrides .bold default", {
  spec <- fr_table(df_simple) |>
    fr_titles(
      list("Bold", bold = TRUE),
      "Not Bold",
      .bold = FALSE
    )
  expect_true(spec$meta$titles[[1]]$bold)
  expect_false(spec$meta$titles[[2]]$bold)
})

test_that("fr_titles .align = 'right' applies to all entries", {
  spec <- fr_table(df_simple) |>
    fr_titles("T1", "T2", .align = "right")
  expect_equal(spec$meta$titles[[1]]$align, "right")
  expect_equal(spec$meta$titles[[2]]$align, "right")
})

test_that("fr_titles errors on non-character, non-list entry", {
  expect_error(
    fr_table(df_simple) |> fr_titles(42),
    class = "rlang_error"
  )
})

test_that("fr_titles clearing resets to empty list", {
  spec <- fr_table(df_simple) |>
    fr_titles("Title A", "Title B") |>
    fr_titles()
  expect_equal(spec$meta$titles, list())
  expect_length(spec$meta$titles, 0L)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_footnotes — additional coverage
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_footnotes list form with 'content' key", {
  spec <- fr_table(df_simple) |>
    fr_footnotes(list(content = "Source: ADSL."))
  expect_equal(spec$meta$footnotes[[1]]$content, "Source: ADSL.")
})

test_that("fr_footnotes list form with unnamed first element", {
  spec <- fr_table(df_simple) |>
    fr_footnotes(list("Source note.", placement = "last"))
  expect_equal(spec$meta$footnotes[[1]]$content, "Source note.")
  expect_equal(spec$meta$footnotes[[1]]$placement, "last")
})

test_that("fr_footnotes list form per-line align override", {
  spec <- fr_table(df_simple) |>
    fr_footnotes(
      list("[a] Note.", align = "center"),
      list("Source.", align = "right")
    )
  expect_equal(spec$meta$footnotes[[1]]$align, "center")
  expect_equal(spec$meta$footnotes[[2]]$align, "right")
})

test_that("fr_footnotes list form per-line font_size override", {
  spec <- fr_table(df_simple) |>
    fr_footnotes(
      list("[a] Note.", font_size = 8),
      list("Source.", font_size = 7)
    )
  expect_equal(spec$meta$footnotes[[1]]$font_size, 8)
  expect_equal(spec$meta$footnotes[[2]]$font_size, 7)
})

test_that("fr_footnotes .align = 'center' applies to string entries", {
  spec <- fr_table(df_simple) |>
    fr_footnotes("Note 1", "Note 2", .align = "center")
  expect_equal(spec$meta$footnotes[[1]]$align, "center")
  expect_equal(spec$meta$footnotes[[2]]$align, "center")
})

test_that("fr_footnotes .placement = 'last' applies to all entries", {
  spec <- fr_table(df_simple) |>
    fr_footnotes("Note 1", "Note 2", .placement = "last")
  expect_equal(spec$meta$footnotes[[1]]$placement, "last")
  expect_equal(spec$meta$footnotes[[2]]$placement, "last")
})

test_that("fr_footnotes .font_size applies to all string entries", {
  spec <- fr_table(df_simple) |>
    fr_footnotes("N1", "N2", .font_size = 7)
  expect_equal(spec$meta$footnotes[[1]]$font_size, 7)
  expect_equal(spec$meta$footnotes[[2]]$font_size, 7)
})

test_that("fr_footnotes .separator = TRUE is stored", {
  spec <- fr_table(df_simple) |>
    fr_footnotes("Note", .separator = TRUE)
  expect_true(spec$meta$footnote_separator)
})

test_that("fr_footnotes clearing resets to empty list and preserves separator", {
  spec <- fr_table(df_simple) |>
    fr_footnotes("Old note", .separator = TRUE) |>
    fr_footnotes(.separator = FALSE)
  expect_equal(spec$meta$footnotes, list())
  expect_false(spec$meta$footnote_separator)
})

test_that("fr_footnotes errors on non-character, non-list entry", {
  expect_error(
    fr_table(df_simple) |> fr_footnotes(42),
    class = "rlang_error"
  )
})

test_that("fr_footnotes errors on non-fr_spec input", {
  expect_error(fr_footnotes(list(), "N"), class = "rlang_error")
})

test_that("fr_footnotes errors on invalid .separator", {
  expect_error(
    fr_table(df_simple) |> fr_footnotes("N", .separator = "yes"),
    class = "rlang_error"
  )
})

test_that("fr_footnotes list placement overrides .placement default", {
  spec <- fr_table(df_simple) |>
    fr_footnotes(
      "[a] Repeated note.",
      list("Source.", placement = "last"),
      .placement = "every"
    )
  expect_equal(spec$meta$footnotes[[1]]$placement, "every")
  expect_equal(spec$meta$footnotes[[2]]$placement, "last")
})
