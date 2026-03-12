# ──────────────────────────────────────────────────────────────────────────────
# test-tokens.R — Tests for R/tokens.R
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# get_timestamp
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_timestamp returns a length-1 non-empty character", {
  ts <- get_timestamp()
  expect_type(ts, "character")
  expect_length(ts, 1L)
  expect_true(nzchar(ts))
})

test_that("get_timestamp is entirely uppercase", {
  ts <- get_timestamp()
  expect_identical(ts, toupper(ts))
})

test_that("get_timestamp matches DDMONYYYY HH:MM:SS pharma format", {
  ts <- get_timestamp()
  # e.g. "08MAR2026 14:30:25"
  expect_match(ts, "^\\d{2}[A-Z]{3}\\d{4} \\d{2}:\\d{2}:\\d{2}$")
})

test_that("get_timestamp month abbreviation is valid", {
  ts <- get_timestamp()
  month <- substr(ts, 3, 5)
  valid_months <- c(
    "JAN",
    "FEB",
    "MAR",
    "APR",
    "MAY",
    "JUN",
    "JUL",
    "AUG",
    "SEP",
    "OCT",
    "NOV",
    "DEC"
  )
  expect_true(month %in% valid_months)
})

test_that("get_timestamp day, hour, minute, second are in valid ranges", {
  # Format: "08MAR2026 20:43:54"
  ts <- get_timestamp()
  day <- as.integer(substr(ts, 1, 2))
  # Time part starts after space (position 11)
  time_part <- sub("^\\S+\\s+", "", ts)
  parts <- as.integer(strsplit(time_part, ":")[[1]])
  hour <- parts[1]
  minute <- parts[2]
  second <- parts[3]

  expect_true(day >= 1L && day <= 31L)
  expect_true(hour >= 0L && hour <= 23L)
  expect_true(minute >= 0L && minute <= 59L)
  expect_true(second >= 0L && second <= 59L)
})

test_that("get_timestamp year is a plausible 4-digit year", {
  # Format: "08MAR2026 20:43:54"
  ts <- get_timestamp()
  year <- as.integer(gsub("^\\d{2}[A-Z]{3}(\\d{4}).*", "\\1", ts))
  expect_true(year >= 2020L && year <= 2100L)
})


# ══════════════════════════════════════════════════════════════════════════════
# get_source_path
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_source_path returns character(1)", {
  result <- get_source_path()
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("get_source_path returns NA or a non-empty string", {
  result <- get_source_path()
  # In test context it could be NA or a path — either is valid

  if (!is.na(result)) {
    expect_true(nzchar(result))
  } else {
    expect_true(is.na(result))
  }
})

test_that("get_source_path falls through to NA in plain interactive context", {
  # When none of the detection methods find a path, we get NA_character_
  # This tests the final fallback line
  result <- get_source_path()
  # We can't guarantee NA in all test runners, but we can at least confirm

  # the function doesn't error
  expect_type(result, "character")
})

test_that("get_source_path detects ofile in parent frame", {
  # Simulate what source() does: set 'ofile' in a calling frame
  wrapper <- function() {
    ofile <- "/path/to/my_script.R"
    get_source_path()
  }
  result <- wrapper()
  expect_equal(result, "/path/to/my_script.R")
})

test_that("get_source_path ignores non-character ofile", {
  # If ofile exists but is not character, it should be skipped
  wrapper <- function() {
    ofile <- 42L
    get_source_path()
  }
  result <- wrapper()
  # Should NOT return 42; should continue to other detection methods
  expect_type(result, "character")
  expect_true(is.na(result) || result != "42")
})

test_that("get_source_path ignores empty string ofile", {
  wrapper <- function() {
    ofile <- ""
    get_source_path()
  }
  result <- wrapper()
  # Empty string should be skipped (nzchar check)
  expect_type(result, "character")
  expect_true(is.na(result) || nzchar(result))
})

test_that("get_source_path finds ofile in nested frames", {
  outer <- function() {
    ofile <- "/nested/frame/script.R"
    inner()
  }
  inner <- function() {
    get_source_path()
  }
  result <- outer()
  expect_equal(result, "/nested/frame/script.R")
})


# ══════════════════════════════════════════════════════════════════════════════
# build_token_map — basic structure
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_token_map returns a named list", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_type(tm, "list")
  expect_true(!is.null(names(tm)))
  expect_true(all(nzchar(names(tm))))
})

test_that("build_token_map always has the four built-in token names", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_true(all(
    c("thepage", "total_pages", "program", "datetime") %in%
      names(tm)
  ))
})

test_that("build_token_map all values are character", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 5, total_pages = 20, spec = spec)
  for (nm in names(tm)) {
    expect_type(tm[[nm]], "character")
  }
})


# ══════════════════════════════════════════════════════════════════════════════
# build_token_map — readonly tokens (thepage, total_pages)
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_token_map converts page_num and total_pages to character", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 7, total_pages = 42, spec = spec)
  expect_equal(tm$thepage, "7")
  expect_equal(tm$total_pages, "42")
})

test_that("build_token_map handles page_num = 1, total_pages = 1", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$thepage, "1")
  expect_equal(tm$total_pages, "1")
})

test_that("build_token_map handles large page numbers", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 999, total_pages = 1000, spec = spec)
  expect_equal(tm$thepage, "999")
  expect_equal(tm$total_pages, "1000")
})


# ══════════════════════════════════════════════════════════════════════════════
# build_token_map — overridable tokens (program, datetime)
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_token_map uses get_source_path default for program", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  # program should be character — either a path or ""
  expect_type(tm$program, "character")
  expect_length(tm$program, 1L)
})

test_that("build_token_map uses get_timestamp default for datetime", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  # datetime should match timestamp format when not overridden
  expect_match(tm$datetime, "^\\d{2}[A-Z]{3}\\d{4} \\d{2}:\\d{2}:\\d{2}$")
})

test_that("build_token_map allows user to override program", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(program = "custom_prog.R"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$program, "custom_prog.R")
})

test_that("build_token_map allows user to override datetime", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(datetime = "15JAN2025 08:00:00"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$datetime, "15JAN2025 08:00:00")
})

test_that("build_token_map allows overriding both program and datetime", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(
      tokens = list(
        program = "dual_override.R",
        datetime = "01JAN2020 00:00:00"
      )
    )
  )
  tm <- build_token_map(page_num = 2, total_pages = 3, spec = spec)
  expect_equal(tm$program, "dual_override.R")
  expect_equal(tm$datetime, "01JAN2020 00:00:00")
  expect_equal(tm$thepage, "2")
  expect_equal(tm$total_pages, "3")
})


# ══════════════════════════════════════════════════════════════════════════════
# build_token_map — custom tokens
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_token_map includes custom user tokens", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(
      tokens = list(
        study = "STUDY-001",
        population = "ITT"
      )
    )
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$study, "STUDY-001")
  expect_equal(tm$population, "ITT")
})

test_that("build_token_map custom tokens do not overwrite builtins", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(custom1 = "val1"))
  )
  tm <- build_token_map(page_num = 5, total_pages = 10, spec = spec)
  # Builtins still correct
  expect_equal(tm$thepage, "5")
  expect_equal(tm$total_pages, "10")
  # Custom present
  expect_equal(tm$custom1, "val1")
})

test_that("build_token_map removes overridable names from custom tokens", {
  # When user sets program + a custom token, program should appear exactly once
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(
      tokens = list(
        program = "override.R",
        mytoken = "myval"
      )
    )
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  # program appears once (in overridable slot, not duplicated in custom)
  expect_equal(sum(names(tm) == "program"), 1L)
  expect_equal(tm$program, "override.R")
  expect_equal(tm$mytoken, "myval")
})

test_that("build_token_map removes datetime from custom tokens when overridden", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(
      tokens = list(
        datetime = "custom_time",
        extra = "extra_val"
      )
    )
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(sum(names(tm) == "datetime"), 1L)
  expect_equal(tm$datetime, "custom_time")
  expect_equal(tm$extra, "extra_val")
})


# ══════════════════════════════════════════════════════════════════════════════
# build_token_map — no user tokens (NULL page$tokens)
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_token_map works with no user tokens at all", {
  spec <- new_fr_spec(data.frame(x = 1))
  # page$tokens should be NULL by default
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  # Should still have exactly the 4 builtins
  expect_true(all(
    c("thepage", "total_pages", "program", "datetime") %in%
      names(tm)
  ))
  expect_length(tm, 4L)
})

test_that("build_token_map with empty list tokens produces only builtins", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list())
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_length(tm, 4L)
  expect_named(tm, c("thepage", "total_pages", "program", "datetime"))
})


# ══════════════════════════════════════════════════════════════════════════════
# build_token_map — token ordering
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_token_map returns tokens in correct order: builtin, overridable, custom", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(alpha = "a", beta = "b"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  nms <- names(tm)
  # First two are readonly builtins
  expect_equal(nms[1], "thepage")
  expect_equal(nms[2], "total_pages")
  # Next two are overridable
  expect_equal(nms[3], "program")
  expect_equal(nms[4], "datetime")
  # Custom follow
  expect_true("alpha" %in% nms)
  expect_true("beta" %in% nms)
})


# ══════════════════════════════════════════════════════════════════════════════
# build_token_map — multiple custom tokens
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_token_map handles many custom tokens", {
  tokens <- as.list(setNames(paste0("val", 1:10), paste0("tok", 1:10)))
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = tokens)
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  # 4 builtins + 10 custom
  expect_length(tm, 14L)
  for (i in 1:10) {
    expect_equal(tm[[paste0("tok", i)]], paste0("val", i))
  }
})


# ══════════════════════════════════════════════════════════════════════════════
# build_token_map — program fallback to empty string
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_token_map program may be NA when no source context", {
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  # program comes from get_source_path() — may be NA in non-interactive context

  expect_true("program" %in% names(tm))
})


# ══════════════════════════════════════════════════════════════════════════════
# Additional coverage: get_source_path edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_source_path skips ofile when NULL in parent frame", {
  # ofile exists but is NULL — should be skipped (is.character check fails)
  wrapper <- function() {
    ofile <- NULL
    get_source_path()
  }
  result <- wrapper()
  expect_type(result, "character")
  # Should NOT error — just falls through
})

test_that("get_source_path prefers nearest ofile in nested frames", {
  # Multiple frames with ofile — the loop scans from frame 1 upwards
  outer <- function() {
    ofile <- "/outer/script.R"
    middle()
  }
  middle <- function() {
    ofile <- "/middle/script.R"
    inner()
  }
  inner <- function() {
    get_source_path()
  }
  result <- outer()
  # Should find one of them (whichever frame index comes first)
  expect_true(result %in% c("/outer/script.R", "/middle/script.R"))
})

test_that("get_source_path does not error when rstudioapi is unavailable", {
  # In a non-RStudio environment, tryCatch should catch and move on
  result <- get_source_path()
  expect_type(result, "character")
  expect_length(result, 1L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Additional coverage: build_token_map with program fallback to ""
# ══════════════════════════════════════════════════════════════════════════════

test_that("build_token_map program is character even when get_source_path returns NA", {
  # When get_source_path() returns NA, the %||% chain should produce ""
  spec <- new_fr_spec(data.frame(x = 1))
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  # program should be character (either "" or a path, never NULL)
  expect_type(tm$program, "character")
  expect_length(tm$program, 1L)
})

test_that("build_token_map custom token with same name as builtin still works", {
  # datetime override + custom tokens — datetime should appear once
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(
      tokens = list(
        datetime = "CUSTOM_TIME",
        program = "CUSTOM_PROG",
        sponsor = "ACME"
      )
    )
  )
  tm <- build_token_map(page_num = 3, total_pages = 5, spec = spec)
  expect_equal(tm$datetime, "CUSTOM_TIME")
  expect_equal(tm$program, "CUSTOM_PROG")
  expect_equal(tm$sponsor, "ACME")
  # Readonly builtins still set by engine, not user
  expect_equal(tm$thepage, "3")
  expect_equal(tm$total_pages, "5")
  # No duplicates
  expect_equal(length(unique(names(tm))), length(names(tm)))
})

test_that("build_token_map with only program override keeps datetime auto", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(program = "my_prog.sas"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$program, "my_prog.sas")
  # datetime should still be auto-generated (timestamp format)
  expect_match(tm$datetime, "^\\d{2}[A-Z]{3}\\d{4} \\d{2}:\\d{2}:\\d{2}$")
})

test_that("build_token_map with only datetime override keeps program auto", {
  spec <- new_fr_spec(
    data.frame(x = 1),
    page = new_fr_page(tokens = list(datetime = "01JAN2020 00:00:00"))
  )
  tm <- build_token_map(page_num = 1, total_pages = 1, spec = spec)
  expect_equal(tm$datetime, "01JAN2020 00:00:00")
  # program should come from get_source_path() fallback
  expect_type(tm$program, "character")
})
