# Token substitution gate (Stage 3). Before this pass, `.with_chrome()`
# resolved only `{datetime}` / `{program}` / `{program_path}`. Study-
# meta tokens (`{sponsor}`, `{protocol}`, ...) and semantic tokens
# (`{analysis_set}`, `{arm_label}`) rendered as literal placeholders on
# paper. These tests pin the expanded substitution and prove the
# byte-golden precondition (empty theme -> passes through unchanged).

.mk_object <- function(pagehead = NULL, pagefoot = NULL, options = list()) {
  opts <- options
  if (!is.null(pagehead)) {
    opts$pagehead <- pagehead
  }
  if (!is.null(pagefoot)) {
    opts$pagefoot <- pagefoot
  }
  arpillar::object(
    id = "t1",
    type = "summary",
    dataset = "ADSL",
    title = "Test",
    options = opts,
    roles = list()
  )
}

test_that("study-meta tokens substitute from theme$study", {
  obj <- .mk_object(
    pagehead = list(
      left = "{sponsor}",
      center = "{sponsor} - {protocol}",
      right = "{data_date}"
    )
  )
  theme <- list(
    study = list(
      sponsor = "Pfizer",
      protocol = "PA-101",
      data_date = "2026-03-05"
    )
  )
  out <- .with_chrome(obj, theme = theme)
  expect_identical(out@options$pagehead$left, "Pfizer")
  expect_identical(out@options$pagehead$center, "Pfizer - PA-101")
  expect_identical(out@options$pagehead$right, "2026-03-05")
})

test_that("analysis-set token resolves via object > theme default", {
  obj <- .mk_object(
    pagehead = list(left = "{analysis_set}"),
    options = list(population = "safety")
  )
  theme <- list(
    populations = list(safety = list(label = "Safety Population"))
  )
  out <- .with_chrome(obj, theme = theme)
  expect_identical(out@options$pagehead$left, "Safety Population")
})

test_that("analysis-set falls back to theme$default_population", {
  obj <- .mk_object(pagehead = list(left = "{analysis_set}"))
  theme <- list(
    default_population = "itt",
    populations = list(itt = list(label = "ITT Analysis Set"))
  )
  out <- .with_chrome(obj, theme = theme)
  expect_identical(out@options$pagehead$left, "ITT Analysis Set")
})

test_that("arm-label token resolves from theme$arm$label", {
  obj <- .mk_object(pagefoot = list(right = "Arm: {arm_label}"))
  theme <- list(arm = list(label = "Treatment Group"))
  out <- .with_chrome(obj, theme = theme)
  expect_identical(out@options$pagefoot$right, "Arm: Treatment Group")
})

test_that("unknown token passes through unchanged (defensive)", {
  obj <- .mk_object(pagehead = list(left = "{fake_token}"))
  out <- .with_chrome(obj, theme = list())
  expect_identical(out@options$pagehead$left, "{fake_token}")
})

test_that("empty theme + no study-token bands -> only chrome tokens fire", {
  # The byte-golden invariant precondition: an object with only the
  # legacy chrome tokens ({datetime}) sees identical output regardless
  # of theme.
  obj <- .mk_object(pagefoot = list(right = "Report {datetime}"))
  now <- as.POSIXct("2026-07-06 10:00:00", tz = "UTC")
  a <- .with_chrome(obj, now = now, theme = list())
  b <- .with_chrome(obj, now = now, theme = list(study = list(sponsor = "X")))
  expect_identical(a@options$pagefoot$right, b@options$pagefoot$right)
})

test_that("unresolved analysis-set token passes through when no population applies", {
  obj <- .mk_object(pagehead = list(left = "{analysis_set}"))
  out <- .with_chrome(obj, theme = list())
  expect_identical(out@options$pagehead$left, "{analysis_set}")
})

test_that("all tokens across both bands compose without collision", {
  obj <- .mk_object(
    pagehead = list(
      left = "{sponsor}",
      center = "{study} - {indication}",
      right = "{data_date}"
    ),
    pagefoot = list(
      left = "{status} | {analysis_set}",
      center = "Page {page} of {npages}",
      right = "{program} @ {datetime}"
    ),
    options = list(population = "saf")
  )
  now <- as.POSIXct("2026-07-06 12:30:45", tz = "UTC")
  theme <- list(
    study = list(
      sponsor = "Anthropic",
      study = "CV185155",
      indication = "AF",
      data_date = "2026-05-01",
      status = "FINAL"
    ),
    populations = list(saf = list(label = "Safety"))
  )
  out <- .with_chrome(obj, now = now, theme = theme)
  expect_identical(out@options$pagehead$left, "Anthropic")
  expect_identical(out@options$pagehead$center, "CV185155 - AF")
  expect_identical(out@options$pagehead$right, "2026-05-01")
  expect_identical(out@options$pagefoot$left, "FINAL | Safety")
  # {page} / {npages} are backend-resolved field codes; pass through.
  expect_identical(
    out@options$pagefoot$center,
    "Page {page} of {npages}"
  )
  # {program} + {datetime} baked as literals.
  expect_match(out@options$pagefoot$right, "^programs/", perl = TRUE)
  expect_match(out@options$pagefoot$right, "@ [0-9]{2}[A-Z]{3}[0-9]{4}:")
})
