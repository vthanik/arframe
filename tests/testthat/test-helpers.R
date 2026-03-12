# ──────────────────────────────────────────────────────────────────────────────
# test-helpers.R — Tests for helpers.R
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# Error conditions
# ══════════════════════════════════════════════════════════════════════════════

test_that("tlframe_error raises error with tlframe_error class", {
  expect_error(
    tlframe_error("test error"),
    class = "tlframe_error"
  )
})


test_that("tlframe_error supports cli interpolation via ...", {
  # cli_abort evaluates glue expressions in caller_env(), which is
  # tlframe_error's frame -- not the test's frame. So {col_name} from
  # a local variable here is not visible. Use a literal .val instead.
  expect_error(
    tlframe_error("Column {.val age} not found"),
    "age"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# normalise_text
# ══════════════════════════════════════════════════════════════════════════════

test_that("normalise_text passes through plain strings", {
  expect_equal(normalise_text("hello"), "hello")
  expect_equal(normalise_text(""), "")
})

test_that("normalise_text evaluates markup", {
  result <- normalise_text("{fr_super(1)} test")
  expect_true(grepl("\x01SUPER:1\x02", result, fixed = TRUE))
})

test_that("normalise_text errors on non-character", {
  expect_error(normalise_text(123), class = "rlang_error")
  expect_error(normalise_text(NULL), class = "rlang_error")
})

test_that("normalise_text errors on vector input", {
  expect_error(normalise_text(c("a", "b")), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# get_timestamp
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_timestamp returns pharma format string", {
  ts <- get_timestamp()
  expect_true(is.character(ts))
  expect_true(nzchar(ts))
  # Format: ddMONyyyy HH:MM:SS (e.g., "03MAR2026 14:30:00")
  expect_match(ts, "^\\d{2}[A-Za-z]{3}\\d{4} \\d{2}:\\d{2}:\\d{2}$")
})


# ══════════════════════════════════════════════════════════════════════════════
# get_source_path
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_source_path returns character scalar", {
  result <- get_source_path()
  expect_true(is.character(result))
  expect_length(result, 1)
})


# ══════════════════════════════════════════════════════════════════════════════
# build_token_map
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_token_map includes all built-in tokens", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 1, total_pages = 5, spec = spec)

  expect_true("thepage" %in% names(tm))
  expect_true("total_pages" %in% names(tm))
  expect_true("program" %in% names(tm))
  expect_true("datetime" %in% names(tm))

  expect_equal(tm$thepage, "1")
  expect_equal(tm$total_pages, "5")
})

test_that("build_token_map uses pagination engine values for readonly tokens", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 3, total_pages = 10, spec = spec)
  expect_equal(tm$thepage, "3")
  expect_equal(tm$total_pages, "10")
})

test_that("build_token_map merges custom tokens", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(study = "ABC-001", pop = "ITT"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$study, "ABC-001")
  expect_equal(tm$pop, "ITT")
})

test_that("build_token_map allows program override via tokens", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(program = "my_script.R"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$program, "my_script.R")
})

test_that("build_token_map allows datetime override via tokens", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(datetime = "01MAR2025 10:00:00"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$datetime, "01MAR2025 10:00:00")
})

test_that("build_token_map does not duplicate overridable tokens in custom", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(program = "test.R", study = "XYZ"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(sum(names(tm) == "program"), 1)
  expect_equal(tm$study, "XYZ")
})


# ══════════════════════════════════════════════════════════════════════════════
# build_default_columns
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_default_columns creates fr_col for each column", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), c = c(TRUE, FALSE, TRUE))
  cols <- build_default_columns(df)

  expect_length(cols, 3)
  expect_named(cols, c("a", "b", "c"))
  for (col in cols) {
    expect_s3_class(col, "fr_col")
  }
})

test_that("build_default_columns sets id from column name", {
  df <- data.frame(param = "x", value = 1)
  cols <- build_default_columns(df)
  expect_equal(cols$param$id, "param")
  expect_equal(cols$value$id, "value")
})

test_that("build_default_columns auto-detects alignment", {
  df <- data.frame(name = "Alice", score = 95.5, flag = TRUE)
  cols <- build_default_columns(df)
  expect_equal(cols$name$align, "left")
  expect_equal(cols$score$align, "right")
  expect_equal(cols$flag$align, "left")
})

test_that("build_default_columns uses column name as label", {
  df <- data.frame(param = "x")
  cols <- build_default_columns(df)
  expect_equal(cols$param$label, "param")
})

test_that("build_default_columns preserves user-configured columns", {
  df <- data.frame(param = "x", value = 1)
  configured <- list(param = fr_col("Parameter", width = 2.5, align = "left"))
  cols <- build_default_columns(df, configured = configured)

  expect_equal(cols$param$label, "Parameter")
  expect_equal(cols$param$width, 2.5)
  expect_equal(cols$param$id, "param")
  expect_equal(cols$value$label, "value")
  expect_equal(cols$value$align, "right")
})

test_that("build_default_columns returns fr_col objects", {
  df <- data.frame(x = 1)
  cols <- build_default_columns(df)
  expect_s3_class(cols$x, "fr_col")
})


# ══════════════════════════════════════════════════════════════════════════════
# printable_area_inches / printable_area_twips
# ══════════════════════════════════════════════════════════════════════════════

test_that("printable_area_inches returns positive dimensions", {
  page <- new_fr_page()
  area <- printable_area_inches(page)
  expect_named(area, c("width", "height"))
  expect_gt(area[["width"]], 0)
  expect_gt(area[["height"]], 0)
})

test_that("printable_area_inches accounts for margins", {
  page_small <- new_fr_page(margins = 0.5)
  page_large <- new_fr_page(margins = 2)
  area_small <- printable_area_inches(page_small)
  area_large <- printable_area_inches(page_large)
  expect_gt(area_small[["width"]], area_large[["width"]])
  expect_gt(area_small[["height"]], area_large[["height"]])
})

test_that("printable_area_twips is consistent with inches version", {
  page <- new_fr_page()
  area_in <- printable_area_inches(page)
  area_tw <- printable_area_twips(page)
  expect_equal(area_tw[["width"]], inches_to_twips(area_in[["width"]]))
  expect_equal(area_tw[["height"]], inches_to_twips(area_in[["height"]]))
})

test_that("landscape is wider than portrait", {
  landscape <- new_fr_page(orientation = "landscape", margins = 1)
  portrait <- new_fr_page(orientation = "portrait", margins = 1)
  area_l <- printable_area_inches(landscape)
  area_p <- printable_area_inches(portrait)
  expect_gt(area_l[["width"]], area_p[["width"]])
  expect_lt(area_l[["height"]], area_p[["height"]])
})


# ══════════════════════════════════════════════════════════════════════════════
# label_to_plain
# ══════════════════════════════════════════════════════════════════════════════

test_that("label_to_plain passes through plain text", {
  expect_equal(label_to_plain("hello"), "hello")
  expect_equal(label_to_plain(""), "")
})

test_that("label_to_plain strips sentinels", {
  s <- paste0("kg/m", markup_sentinel("SUPER", "2"))
  expect_equal(label_to_plain(s), "kg/m2")
})

test_that("label_to_plain handles non-character input", {
  expect_equal(label_to_plain(123), "123")
})
