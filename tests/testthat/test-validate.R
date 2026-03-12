# ──────────────────────────────────────────────────────────────────────────────
# test-validate.R — Tests for R/validate.R validation helpers
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# match_arg_fr
# ══════════════════════════════════════════════════════════════════════════════

test_that("match_arg_fr returns first choice when x is NULL", {
  result <- match_arg_fr(NULL, c("left", "center", "right"))
  expect_equal(result, "left")
})

test_that("match_arg_fr normalises to lowercase", {
  expect_equal(match_arg_fr("LEFT", c("left", "center", "right")), "left")
  expect_equal(match_arg_fr("Center", c("left", "center", "right")), "center")
  expect_equal(match_arg_fr("RIGHT", c("left", "center", "right")), "right")
})

test_that("match_arg_fr accepts valid lowercase input", {
  expect_equal(match_arg_fr("center", c("left", "center", "right")), "center")
})

test_that("match_arg_fr errors on invalid value", {
  expect_error(
    match_arg_fr("justify", c("left", "center", "right")),
    class = "rlang_error"
  )
})

test_that("match_arg_fr error message includes arg name and choices", {
  expect_error(
    match_arg_fr("bad", c("a", "b"), arg = "myarg"),
    "myarg"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# check_scalar_chr
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_scalar_chr accepts a single string", {
  expect_invisible(check_scalar_chr("hello"))
  expect_equal(check_scalar_chr("hello"), "hello")
})

test_that("check_scalar_chr errors on non-character", {
  expect_error(check_scalar_chr(123), class = "rlang_error")
  expect_error(check_scalar_chr(TRUE), class = "rlang_error")
})

test_that("check_scalar_chr errors on length > 1 character vector", {
  expect_error(check_scalar_chr(c("a", "b")), class = "rlang_error")
})

test_that("check_scalar_chr errors on NULL when allow_null is FALSE", {
  expect_error(check_scalar_chr(NULL), class = "rlang_error")
})

test_that("check_scalar_chr returns NULL invisibly when allow_null is TRUE", {
  result <- check_scalar_chr(NULL, allow_null = TRUE)
  expect_null(result)
})

test_that("check_scalar_chr accepts empty string", {
  expect_invisible(check_scalar_chr(""))
})


# ══════════════════════════════════════════════════════════════════════════════
# check_positive_num
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_positive_num accepts positive numbers", {
  expect_invisible(check_positive_num(1))
  expect_invisible(check_positive_num(0.5))
  expect_invisible(check_positive_num(100))
})

test_that("check_positive_num errors on zero", {
  expect_error(check_positive_num(0), class = "rlang_error")
})

test_that("check_positive_num errors on negative numbers", {
  expect_error(check_positive_num(-1), class = "rlang_error")
  expect_error(check_positive_num(-0.5), class = "rlang_error")
})

test_that("check_positive_num errors on NA", {
  expect_error(check_positive_num(NA_real_), class = "rlang_error")
})

test_that("check_positive_num errors on non-numeric input", {
  expect_error(check_positive_num("10"), class = "rlang_error")
  expect_error(check_positive_num(TRUE), class = "rlang_error")
})

test_that("check_positive_num errors on length > 1 numeric vector", {
  expect_error(check_positive_num(c(1, 2)), class = "rlang_error")
})

test_that("check_positive_num errors on NULL when allow_null is FALSE", {
  expect_error(check_positive_num(NULL), class = "rlang_error")
})

test_that("check_positive_num returns NULL invisibly when allow_null is TRUE", {
  result <- check_positive_num(NULL, allow_null = TRUE)
  expect_null(result)
})


# ══════════════════════════════════════════════════════════════════════════════
# check_scalar_lgl
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_scalar_lgl accepts TRUE and FALSE", {
  expect_invisible(check_scalar_lgl(TRUE))
  expect_invisible(check_scalar_lgl(FALSE))
})

test_that("check_scalar_lgl errors on NA", {
  expect_error(check_scalar_lgl(NA), class = "rlang_error")
})

test_that("check_scalar_lgl errors on non-logical input", {
  expect_error(check_scalar_lgl(1), class = "rlang_error")
  expect_error(check_scalar_lgl("TRUE"), class = "rlang_error")
  expect_error(check_scalar_lgl(NULL), class = "rlang_error")
})

test_that("check_scalar_lgl errors on logical vector length > 1", {
  expect_error(check_scalar_lgl(c(TRUE, FALSE)), class = "rlang_error")
})

test_that("check_scalar_lgl returns NULL invisibly when allow_null is TRUE", {
  result <- check_scalar_lgl(NULL, allow_null = TRUE)
  expect_null(result)
})


# ══════════════════════════════════════════════════════════════════════════════
# check_non_negative_int
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_non_negative_int accepts zero", {
  expect_equal(check_non_negative_int(0), 0L)
})

test_that("check_non_negative_int accepts positive integers", {
  expect_equal(check_non_negative_int(5L), 5L)
  expect_equal(check_non_negative_int(3), 3L) # numeric coerced to integer
})

test_that("check_non_negative_int errors on negative numbers", {
  expect_error(check_non_negative_int(-1), class = "rlang_error")
})

test_that("check_non_negative_int errors on non-integer numeric", {
  expect_error(check_non_negative_int(1.5), class = "rlang_error")
  expect_error(check_non_negative_int(0.1), class = "rlang_error")
})

test_that("check_non_negative_int errors on NA", {
  expect_error(check_non_negative_int(NA_real_), class = "rlang_error")
  expect_error(check_non_negative_int(NA_integer_), class = "rlang_error")
})

test_that("check_non_negative_int errors on non-numeric input", {
  expect_error(check_non_negative_int("5"), class = "rlang_error")
  expect_error(check_non_negative_int(TRUE), class = "rlang_error")
  expect_error(check_non_negative_int(NULL), class = "rlang_error")
})

test_that("check_non_negative_int errors on length > 1", {
  expect_error(check_non_negative_int(c(1, 2)), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# parse_pct_width
# ══════════════════════════════════════════════════════════════════════════════

test_that("parse_pct_width returns NULL for non-percentage strings", {
  expect_null(parse_pct_width("auto"))
  expect_null(parse_pct_width("2.5"))
  expect_null(parse_pct_width("hello"))
})

test_that("parse_pct_width returns NULL for non-character input", {
  expect_null(parse_pct_width(42))
  expect_null(parse_pct_width(TRUE))
})

test_that("parse_pct_width parses valid percentage strings", {
  result <- parse_pct_width("20%")
  expect_s3_class(result, "fr_pct")
  expect_equal(as.numeric(result), 0.20)
})

test_that("parse_pct_width parses decimal percentages", {
  result <- parse_pct_width("50.5%")
  expect_s3_class(result, "fr_pct")
  expect_equal(as.numeric(result), 0.505)
})

test_that("parse_pct_width errors on 0%", {
  expect_error(parse_pct_width("0%"), class = "rlang_error")
})

test_that("parse_pct_width accepts 100%", {
  result <- parse_pct_width("100%")
  expect_s3_class(result, "fr_pct")
  expect_equal(as.numeric(result), 1.0)
})

test_that("parse_pct_width errors on >100%", {
  expect_error(parse_pct_width("150%"), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# validate_n_param — three-form validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("validate_n_param accepts NULL n", {
  expect_silent(validate_n_param(n = NULL))
})

test_that("validate_n_param errors on character scalar (no longer valid)", {
  expect_error(validate_n_param(n = "TRTA"), class = "rlang_error")
})

test_that("validate_n_param errors on unnamed numeric vector", {
  expect_error(validate_n_param(n = c(10, 20)), class = "rlang_error")
})

test_that("validate_n_param accepts named numeric vector", {
  expect_silent(validate_n_param(n = c(placebo = 45, drug = 45)))
})

test_that("validate_n_param errors on unnamed list", {
  expect_error(validate_n_param(n = list(c(a = 10))), class = "rlang_error")
})

test_that("validate_n_param errors on named list with non-named-numeric entry", {
  expect_error(
    validate_n_param(n = list(grp1 = c(10, 20))),
    class = "rlang_error"
  )
})

test_that("validate_n_param errors on non-numeric inner list value", {
  expect_error(
    validate_n_param(n = list(grp1 = "bad")),
    class = "rlang_error"
  )
})

test_that("validate_n_param accepts named list with named numeric entries", {
  expect_silent(validate_n_param(
    n = list(
      grp1 = c(a = 10, b = 20),
      grp2 = c(a = 15, b = 25)
    )
  ))
})

test_that("validate_n_param accepts 2-col data frame", {
  expect_silent(validate_n_param(n = data.frame(trt = "A", n = 10L)))
})

test_that("validate_n_param rejects function form", {
  expect_error(validate_n_param(n = function(d) c(a = 1)), "named numeric")
})

test_that("validate_n_param errors on unsupported type (logical)", {
  expect_error(validate_n_param(n = TRUE), class = "rlang_error")
})

test_that("validate_n_param validates format as character scalar", {
  expect_error(
    validate_n_param(n = c(a = 10), format = 123),
    class = "rlang_error"
  )
  expect_silent(validate_n_param(n = c(a = 10), format = "(N={n})"))
})


# ══════════════════════════════════════════════════════════════════════════════
# has_fr_markup — additional coverage
# ══════════════════════════════════════════════════════════════════════════════

test_that("has_fr_markup returns FALSE for NA input", {
  expect_false(has_fr_markup(NA_character_))
})

test_that("has_fr_markup returns FALSE for length-0 character", {
  expect_false(has_fr_markup(character(0)))
})

test_that("has_fr_markup detects various fr_ markup forms", {
  expect_true(has_fr_markup("text {fr_italic('x')} more"))
  expect_true(has_fr_markup("{fr_dagger()}"))
  expect_true(has_fr_markup("{fr_newline()}"))
})


# ══════════════════════════════════════════════════════════════════════════════
# markup_sentinel
# ══════════════════════════════════════════════════════════════════════════════

test_that("markup_sentinel with empty content produces correct format", {
  s <- markup_sentinel("NEWLINE", "")
  expect_true(grepl("\x01NEWLINE:\x02", s, fixed = TRUE))
})

test_that("markup_sentinel with content produces correct format", {
  s <- markup_sentinel("BOLD", "hello world")
  expect_true(grepl("\x01BOLD:hello world\x02", s, fixed = TRUE))
})
