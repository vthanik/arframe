# ──────────────────────────────────────────────────────────────────────────────
# test-theme.R — Tests for fr_theme(), fr_theme_set(), fr_theme_get(),
#                fr_theme_reset()
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# fr_theme_get / fr_theme_reset — baseline behaviour
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme_get returns empty list when no theme is set", {
  withr::defer(fr_theme_reset())
  fr_theme_reset()
  result <- fr_theme_get()
  expect_type(result, "list")
  expect_length(result, 0L)
})

test_that("fr_theme_reset returns invisible NULL", {
  result <- fr_theme_reset()
  expect_null(result)
  expect_invisible(fr_theme_reset())
})

test_that("fr_theme_reset clears a previously set theme", {
  withr::defer(fr_theme_reset())
  fr_theme(font_size = 8)
  expect_true(length(fr_theme_get()) > 0L)
  fr_theme_reset()
  expect_length(fr_theme_get(), 0L)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_theme — set and retrieve individual parameters
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme sets font_size and fr_theme_get retrieves it", {
  withr::defer(fr_theme_reset())
  fr_theme(font_size = 8)
  theme <- fr_theme_get()
  expect_equal(theme$font_size, 8)
})

test_that("fr_theme sets orientation", {
  withr::defer(fr_theme_reset())
  fr_theme(orientation = "landscape")
  expect_equal(fr_theme_get()$orientation, "landscape")
})

test_that("fr_theme sets paper", {
  withr::defer(fr_theme_reset())
  fr_theme(paper = "a4")
  expect_equal(fr_theme_get()$paper, "a4")
})

test_that("fr_theme sets font_family", {
  withr::defer(fr_theme_reset())
  fr_theme(font_family = "Courier New")
  expect_equal(fr_theme_get()$font_family, "Courier New")
})

test_that("fr_theme sets margins", {
  withr::defer(fr_theme_reset())
  fr_theme(margins = c(1, 1, 1, 1))
  expect_equal(fr_theme_get()$margins, c(1, 1, 1, 1))
})

test_that("fr_theme sets tokens", {
  withr::defer(fr_theme_reset())
  fr_theme(tokens = list(study = "ABC-001", cutoff = "01JAN2025"))
  tk <- fr_theme_get()$tokens
  expect_equal(tk$study, "ABC-001")
  expect_equal(tk$cutoff, "01JAN2025")
})

test_that("fr_theme sets pagehead", {
  withr::defer(fr_theme_reset())
  fr_theme(pagehead = list(left = "Left", right = "Right"))
  ph <- fr_theme_get()$pagehead
  expect_equal(ph$left, "Left")
  expect_equal(ph$right, "Right")
})

test_that("fr_theme sets pagefoot", {
  withr::defer(fr_theme_reset())
  fr_theme(pagefoot = list(left = "{program}", center = "Page {thepage}"))
  pf <- fr_theme_get()$pagefoot
  expect_equal(pf$left, "{program}")
  expect_equal(pf$center, "Page {thepage}")
})

test_that("fr_theme sets hlines preset", {
  withr::defer(fr_theme_reset())
  fr_theme(hlines = "header")
  expect_equal(fr_theme_get()$hlines, "header")
})

test_that("fr_theme sets vlines preset", {
  withr::defer(fr_theme_reset())
  fr_theme(vlines = "box")
  expect_equal(fr_theme_get()$vlines, "box")
})

test_that("fr_theme sets spacing", {
  withr::defer(fr_theme_reset())
  fr_theme(spacing = list(titles_after = 2L, footnotes_before = 3L))
  sp <- fr_theme_get()$spacing
  expect_equal(sp$titles_after, 2L)
  expect_equal(sp$footnotes_before, 3L)
})

test_that("fr_theme sets header with span_gap", {
  withr::defer(fr_theme_reset())
  fr_theme(header = list(span_gap = FALSE))
  expect_false(fr_theme_get()$header$span_gap)
})

test_that("fr_theme sets footnote_separator", {
  withr::defer(fr_theme_reset())
  fr_theme(footnote_separator = FALSE)
  expect_false(fr_theme_get()$footnote_separator)

  fr_theme(footnote_separator = TRUE)
  expect_true(fr_theme_get()$footnote_separator)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_theme — multiple parameters at once
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme sets multiple parameters simultaneously", {
  withr::defer(fr_theme_reset())
  fr_theme(
    font_size = 9,
    orientation = "landscape",
    paper = "letter",
    hlines = "booktabs",
    vlines = "all"
  )
  theme <- fr_theme_get()
  expect_equal(theme$font_size, 9)
  expect_equal(theme$orientation, "landscape")
  expect_equal(theme$paper, "letter")
  expect_equal(theme$hlines, "booktabs")
  expect_equal(theme$vlines, "all")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_theme — merging behaviour (does NOT discard existing keys)
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme merges new settings into existing theme", {
  withr::defer(fr_theme_reset())
  fr_theme(font_size = 9, hlines = "header")
  fr_theme(vlines = "box")

  theme <- fr_theme_get()
  expect_equal(theme$font_size, 9)
  expect_equal(theme$hlines, "header")
  expect_equal(theme$vlines, "box")
})

test_that("fr_theme overwrites existing key on repeated call", {
  withr::defer(fr_theme_reset())
  fr_theme(font_size = 9)
  fr_theme(font_size = 11)
  expect_equal(fr_theme_get()$font_size, 11)
})

test_that("fr_theme returns invisible NULL", {
  withr::defer(fr_theme_reset())
  result <- fr_theme(font_size = 8)
  expect_null(result)
  expect_invisible(fr_theme(font_size = 8))
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_theme_set — alias for fr_theme
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme_set is identical to fr_theme", {
  expect_identical(fr_theme_set, fr_theme)
})

test_that("fr_theme_set sets parameters like fr_theme", {
  withr::defer(fr_theme_reset())
  fr_theme_set(font_size = 10, orientation = "portrait")
  theme <- fr_theme_get()
  expect_equal(theme$font_size, 10)
  expect_equal(theme$orientation, "portrait")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_theme — all hline presets
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme accepts all valid hline presets", {
  withr::defer(fr_theme_reset())
  for (preset in c(
    "header",
    "open",
    "hsides",
    "above",
    "below",
    "box",
    "booktabs",
    "void"
  )) {
    fr_theme_reset()
    fr_theme(hlines = preset)
    expect_equal(fr_theme_get()$hlines, preset)
  }
})

test_that("fr_theme accepts all valid vline presets", {
  withr::defer(fr_theme_reset())
  for (preset in c("box", "all", "inner", "void")) {
    fr_theme_reset()
    fr_theme(vlines = preset)
    expect_equal(fr_theme_get()$vlines, preset)
  }
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_theme — validation / error paths
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme errors on invalid font_size (non-positive)", {
  withr::defer(fr_theme_reset())
  expect_error(fr_theme(font_size = -1))
  expect_error(fr_theme(font_size = 0))
})

test_that("fr_theme errors on non-numeric font_size", {
  withr::defer(fr_theme_reset())
  expect_error(fr_theme(font_size = "big"))
})

test_that("fr_theme errors on invalid hlines preset", {
  withr::defer(fr_theme_reset())
  expect_error(fr_theme(hlines = "nonexistent"))
})

test_that("fr_theme errors on invalid vlines preset", {
  withr::defer(fr_theme_reset())
  expect_error(fr_theme(vlines = "nonexistent"))
})

test_that("fr_theme errors on non-logical footnote_separator", {
  withr::defer(fr_theme_reset())
  expect_error(fr_theme(footnote_separator = "yes"))
  expect_error(fr_theme(footnote_separator = 1))
})

test_that("fr_theme errors on non-list header", {
  withr::defer(fr_theme_reset())
  expect_error(fr_theme(header = "bad"), "must be a list")
})

test_that("fr_theme errors on non-logical header$span_gap", {
  withr::defer(fr_theme_reset())
  expect_error(fr_theme(header = list(span_gap = "yes")))
  expect_error(fr_theme(header = list(span_gap = 1)))
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_theme — application to fr_table() specs
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme font_size applies to new specs via fr_table", {
  withr::defer(fr_theme_reset())
  fr_theme(font_size = 7)
  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$font_size, 7)
})

test_that("fr_theme orientation applies to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(orientation = "portrait")
  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$orientation, "portrait")
})

test_that("fr_theme paper applies to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(paper = "a4")
  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$paper, "a4")
})

test_that("fr_theme font_family applies to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(font_family = "Courier New")
  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$font_family, "Courier New")
})

test_that("fr_theme hlines applies to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(hlines = "booktabs")
  spec <- tbl_demog |> fr_table()
  expect_true(length(spec$rules) > 0L)
})

test_that("fr_theme vlines applies to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(vlines = "box")
  spec <- tbl_demog |> fr_table()
  # vlines stored in spec$rules as fr_vline_spec objects
  vlines <- Filter(function(r) inherits(r, "fr_vline_spec"), spec$rules)
  expect_true(length(vlines) > 0L)
})

test_that("fr_theme spacing applies to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(spacing = list(titles_after = 3L, footnotes_before = 2L))
  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$spacing$titles_after, 3L)
  expect_equal(spec$spacing$footnotes_before, 2L)
})

test_that("fr_theme pagehead applies to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(pagehead = list(left = "Study X", right = "Page"))
  spec <- tbl_demog |> fr_table()
  expect_equal(spec$pagehead$left, "Study X")
  expect_equal(spec$pagehead$right, "Page")
})

test_that("fr_theme pagefoot applies to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(pagefoot = list(left = "Program", center = "Center"))
  spec <- tbl_demog |> fr_table()
  expect_equal(spec$pagefoot$left, "Program")
  expect_equal(spec$pagefoot$center, "Center")
})

test_that("fr_theme footnote_separator applies to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(footnote_separator = FALSE)
  spec <- tbl_demog |> fr_table()
  expect_false(spec$meta$footnote_separator)
})

test_that("fr_theme header span_gap applies to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(header = list(span_gap = FALSE))
  spec <- tbl_demog |> fr_table()
  expect_false(spec$header$span_gap)
})

test_that("fr_theme tokens apply to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(tokens = list(study = "STUDY-99"))
  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$tokens$study, "STUDY-99")
})

test_that("fr_theme margins apply to new specs", {
  withr::defer(fr_theme_reset())
  fr_theme(margins = c(0.5, 0.5, 0.5, 0.5))
  spec <- tbl_demog |> fr_table()
  expect_equal(
    spec$page$margins,
    list(top = 0.5, right = 0.5, bottom = 0.5, left = 0.5)
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# Per-table verbs override theme
# ══════════════════════════════════════════════════════════════════════════════

test_that("per-table fr_page overrides theme font_size", {
  withr::defer(fr_theme_reset())
  fr_theme(font_size = 7)
  spec <- tbl_demog |> fr_table() |> fr_page(font_size = 10)
  expect_equal(spec$page$font_size, 10)
})

test_that("per-table fr_page overrides theme orientation", {
  withr::defer(fr_theme_reset())
  fr_theme(orientation = "landscape")
  spec <- tbl_demog |> fr_table() |> fr_page(orientation = "portrait")
  expect_equal(spec$page$orientation, "portrait")
})

test_that("per-table fr_hlines overrides theme hlines", {
  withr::defer(fr_theme_reset())
  fr_theme(hlines = "booktabs")
  spec <- tbl_demog |> fr_table() |> fr_hlines("header")
  # Should have the header preset rules, not booktabs
  hlines <- Filter(function(r) inherits(r, "fr_rule_hline"), spec$rules)
  expect_true(length(hlines) >= 1L)
})


# ══════════════════════════════════════════════════════════════════════════════
# No theme set — fr_table uses defaults
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_table uses defaults when no theme is set", {
  withr::defer(fr_theme_reset())
  fr_theme_reset()
  spec <- tbl_demog |> fr_table()
  # Default orientation is landscape, font_size is 9
  expect_equal(spec$page$orientation, "landscape")
  expect_equal(spec$page$font_size, 9)
})


# ══════════════════════════════════════════════════════════════════════════════
# Edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme with no arguments does not error", {
  withr::defer(fr_theme_reset())
  expect_no_error(fr_theme())
})

test_that("fr_theme with no arguments preserves existing theme", {
  withr::defer(fr_theme_reset())
  fr_theme(font_size = 8)
  fr_theme()
  expect_equal(fr_theme_get()$font_size, 8)
})

test_that("fr_theme_reset is idempotent", {
  fr_theme_reset()
  fr_theme_reset()
  expect_length(fr_theme_get(), 0L)
})

test_that("fr_theme with NULL values does not set keys", {
  withr::defer(fr_theme_reset())
  fr_theme_reset()
  fr_theme(font_size = NULL, orientation = NULL)
  expect_length(fr_theme_get(), 0L)
})

test_that("fr_theme pagehead with bold and font_size", {
  withr::defer(fr_theme_reset())
  fr_theme(pagehead = list(left = "A", bold = TRUE, font_size = 7))
  ph <- fr_theme_get()$pagehead
  expect_equal(ph$bold, TRUE)
  expect_equal(ph$font_size, 7)
})

test_that("fr_theme pagefoot with all three positions", {
  withr::defer(fr_theme_reset())
  fr_theme(pagefoot = list(left = "L", center = "C", right = "R"))
  pf <- fr_theme_get()$pagefoot
  expect_equal(pf$left, "L")
  expect_equal(pf$center, "C")
  expect_equal(pf$right, "R")
})

test_that("fr_theme full study workflow sets everything correctly", {
  withr::defer(fr_theme_reset())
  fr_theme(
    orientation = "landscape",
    paper = "letter",
    font_family = "Times New Roman",
    font_size = 9,
    margins = c(1, 1, 1, 1),
    tokens = list(study = "TFRM-001", cutoff = "31DEC2025"),
    pagehead = list(left = "{study}", right = "{cutoff}"),
    pagefoot = list(left = "{program}", right = "{datetime}"),
    hlines = "header",
    vlines = "box",
    spacing = list(titles_after = 1L, footnotes_before = 1L),
    header = list(span_gap = TRUE),
    footnote_separator = TRUE
  )

  theme <- fr_theme_get()
  expect_equal(theme$orientation, "landscape")
  expect_equal(theme$paper, "letter")
  expect_equal(theme$font_family, "Times New Roman")
  expect_equal(theme$font_size, 9)
  expect_equal(theme$margins, c(1, 1, 1, 1))
  expect_equal(theme$tokens$study, "TFRM-001")
  expect_equal(theme$pagehead$left, "{study}")
  expect_equal(theme$pagefoot$right, "{datetime}")
  expect_equal(theme$hlines, "header")
  expect_equal(theme$vlines, "box")
  expect_equal(theme$spacing$titles_after, 1L)
  expect_true(theme$header$span_gap)
  expect_true(theme$footnote_separator)

  # Verify it applies to a new spec

  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$font_size, 9)
  expect_equal(spec$page$orientation, "landscape")
  expect_equal(spec$page$font_family, "Times New Roman")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_theme — page_by_bold / page_by_align wiring into spec$body
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme wires page_by_bold and page_by_align into spec$body", {
  withr::defer(fr_theme_reset())
  fr_theme_reset()
  fr_theme(page_by_bold = TRUE, page_by_align = "center")

  spec <- tbl_demog |> fr_table()

  expect_true(spec$body$page_by_bold)
  expect_equal(spec$body$page_by_align, "center")
})

test_that("fr_theme page_by_bold FALSE is wired into spec$body", {
  withr::defer(fr_theme_reset())
  fr_theme_reset()
  fr_theme(page_by_bold = FALSE, page_by_align = "right")

  spec <- tbl_demog |> fr_table()

  expect_false(spec$body$page_by_bold)
  expect_equal(spec$body$page_by_align, "right")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_theme — continuation wiring into spec$page
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_theme wires continuation into spec$page", {
  withr::defer(fr_theme_reset())
  fr_theme_reset()
  fr_theme(continuation = "(continued)")

  spec <- tbl_demog |> fr_table()

  expect_equal(spec$page$continuation, "(continued)")
})

test_that("fr_theme continuation is retrievable from theme store", {
  withr::defer(fr_theme_reset())
  fr_theme_reset()
  fr_theme(continuation = "(cont.)")

  expect_equal(fr_theme_get()$continuation, "(cont.)")
})
