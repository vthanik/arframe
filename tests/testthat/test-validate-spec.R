# ══════════════════════════════════════════════════════════════════════════════
# test-validate-spec.R — Tests for fr_validate() (api-validate.R)
# ══════════════════════════════════════════════════════════════════════════════


# ── Clean spec passes silently ───────────────────────────────────────────────

test_that("fr_validate passes clean spec silently", {
  spec <- tbl_demog |> fr_table() |> fr_hlines("header")
  expect_invisible(fr_validate(spec))
})

test_that("fr_validate returns the spec invisibly", {
  spec <- tbl_demog |> fr_table()
  result <- fr_validate(spec)
  expect_identical(result, spec)
})

test_that("fr_validate strict mode passes clean spec silently", {
  spec <- tbl_demog |> fr_table() |> fr_hlines("header")
  expect_no_error(fr_validate(spec, strict = TRUE))
  expect_invisible(fr_validate(spec, strict = TRUE))
})

test_that("fr_validate is pipeline-friendly", {
  out <- file.path(tempdir(), "validate_pipe_test.rtf")
  on.exit(unlink(out), add = TRUE)
  spec <- tbl_demog |>
    fr_table() |>
    fr_hlines("header") |>
    fr_validate()
  expect_s3_class(spec, "fr_spec")
})


# ── Input validation ────────────────────────────────────────────────────────

test_that("fr_validate rejects non-fr_spec input", {
  expect_error(fr_validate(data.frame(x = 1)), class = "rlang_error")
  expect_error(fr_validate("not a spec"), class = "rlang_error")
  expect_error(fr_validate(NULL), class = "rlang_error")
})

test_that("fr_validate validates strict parameter", {
  spec <- tbl_demog |> fr_table()
  expect_error(fr_validate(spec, strict = "yes"), class = "rlang_error")
  expect_error(fr_validate(spec, strict = 1), class = "rlang_error")
})


# ── Check 1: Column specs reference valid data columns ──────────────────────

test_that("fr_validate warns on bad column in column specs", {
  spec <- tbl_demog |> fr_table()
  spec$columns[["nonexistent"]] <- fr_col("Bad")
  expect_warning(fr_validate(spec), "not found in data")
})

test_that("fr_validate strict errors on bad column in column specs", {
  spec <- tbl_demog |> fr_table()
  spec$columns[["nonexistent"]] <- fr_col("Bad")
  expect_error(fr_validate(spec, strict = TRUE), "validation issue")
})

test_that("fr_validate warns on multiple bad columns in column specs", {
  spec <- tbl_demog |> fr_table()
  spec$columns[["fake_a"]] <- fr_col("Fake A")
  spec$columns[["fake_b"]] <- fr_col("Fake B")
  w <- capture_warnings(fr_validate(spec))
  expect_true(any(grepl("fake_a", w)))
  expect_true(any(grepl("fake_b", w)))
})

test_that("fr_validate ignores gap columns (.__* prefix)", {

  spec <- tbl_demog |> fr_table()
  # Simulate a gap column injected by finalize

  spec$columns[[".__gap_1"]] <- fr_col("")
  expect_invisible(fr_validate(spec))
})


# ── Check 2: Row config columns exist ──────────────────────────────────────

test_that("fr_validate warns on bad page_by column", {
  spec <- tbl_demog |> fr_table()
  spec$body$page_by <- "nonexistent"
  expect_warning(fr_validate(spec), "page_by")
})

test_that("fr_validate warns on bad group_by column", {
  spec <- tbl_demog |> fr_table()
  spec$body$group_by <- "nonexistent"
  expect_warning(fr_validate(spec), "group_by")
})

test_that("fr_validate warns on bad indent_by column", {
  spec <- tbl_demog |> fr_table()
  spec$body$indent_by <- "nonexistent"
  expect_warning(fr_validate(spec), "indent_by")
})

test_that("fr_validate warns on bad blank_after column", {
  spec <- tbl_demog |> fr_table()
  spec$body$blank_after <- "nonexistent"
  expect_warning(fr_validate(spec), "blank_after")
})

test_that("fr_validate strict errors on bad row config column", {
  spec <- tbl_demog |> fr_table()
  spec$body$page_by <- "nonexistent"
  expect_error(fr_validate(spec, strict = TRUE), "validation issue")
})

test_that("fr_validate passes with valid row config columns", {
  # Use actual columns from tbl_demog
  cols <- names(tbl_demog)
  spec <- tbl_demog |> fr_table()
  spec$body$group_by <- cols[1]
  expect_invisible(fr_validate(spec))
})


# ── Check 3: stub_cols exist ───────────────────────────────────────────────

test_that("fr_validate warns on bad stub column", {
  spec <- tbl_demog |> fr_table()
  spec$columns[["nonexistent"]] <- fr_col("Bad", stub = TRUE)
  expect_warning(fr_validate(spec), "not found")
})

test_that("fr_validate strict errors on bad stub column", {
  spec <- tbl_demog |> fr_table()
  spec$columns[["nonexistent"]] <- fr_col("Bad", stub = TRUE)
  expect_error(fr_validate(spec, strict = TRUE), "validation issue")
})

test_that("fr_validate passes with valid stub column", {
  spec <- tbl_demog |> fr_table()
  spec <- fr_cols(spec, characteristic = fr_col("Char", stub = TRUE))
  expect_silent(suppressWarnings(fr_validate(spec)))
})


# ── Check 4: N-count names match column specs ──────────────────────────────

test_that("fr_validate warns when N-count names don't match columns", {
  spec <- tbl_demog |> fr_table()
  spec$header$n <- c(nonexistent = 50L)
  expect_warning(fr_validate(spec), "N-count")
})

test_that("fr_validate strict errors when N-count names don't match columns", {
  spec <- tbl_demog |> fr_table()
  spec$header$n <- c(nonexistent = 50L)
  expect_error(fr_validate(spec, strict = TRUE), "validation issue")
})

test_that("fr_validate passes with valid N-count names matching data columns", {
  spec <- tbl_demog |> fr_table()
  cols <- names(tbl_demog)
  n_vec <- setNames(rep(50L, 2), cols[2:3])
  spec$header$n <- n_vec
  expect_invisible(fr_validate(spec))
})

test_that("fr_validate passes with valid N-count names matching column specs", {
  spec <- tbl_demog |> fr_table() |> fr_cols()
  col_names <- names(spec$columns)
  n_vec <- setNames(rep(50L, 2), col_names[1:2])
  spec$header$n <- n_vec
  expect_invisible(fr_validate(spec))
})

test_that("fr_validate skips N-count check for non-numeric or unnamed n", {
  # Function form — should not trigger the named-numeric check

  spec <- tbl_demog |> fr_table()
  spec$header$n <- function(data, group) 50L
  expect_invisible(fr_validate(spec))

  # Unnamed numeric — also skipped
  spec2 <- tbl_demog |> fr_table()
  spec2$header$n <- 50L
  expect_invisible(fr_validate(spec2))
})


# ── Check 5: Span columns exist ───────────────────────────────────────────

test_that("fr_validate warns on bad span columns", {
  spec <- tbl_demog |> fr_table()
  spec$header$spans <- list(
    list(label = "Bad Span", columns = c("nonexistent_a", "nonexistent_b"))
  )
  expect_warning(fr_validate(spec), "Span.*non-existent")
})

test_that("fr_validate strict errors on bad span columns", {
  spec <- tbl_demog |> fr_table()
  spec$header$spans <- list(
    list(label = "Bad Span", columns = c("nonexistent_a", "nonexistent_b"))
  )
  expect_error(fr_validate(spec, strict = TRUE), "validation issue")
})

test_that("fr_validate passes with valid span columns", {
  cols <- names(tbl_demog)
  spec <- tbl_demog |> fr_table()
  spec$header$spans <- list(
    list(label = "Good Span", columns = cols[2:3])
  )
  expect_invisible(fr_validate(spec))
})

test_that("fr_validate checks multiple spans independently", {
  cols <- names(tbl_demog)
  spec <- tbl_demog |> fr_table()
  spec$header$spans <- list(
    list(label = "Good Span", columns = cols[2:3]),
    list(label = "Bad Span", columns = c("bad_col"))
  )
  expect_warning(fr_validate(spec), "Span 2")
})


# ── Check 6: Column widths vs printable area ──────────────────────────────

test_that("fr_validate warns when column widths exceed 110% of printable area", {
  spec <- tbl_demog |> fr_table()
  # Set all columns to very wide fixed widths
  col_names <- names(spec$columns)
  if (length(col_names) == 0L) {
    spec <- spec |> fr_cols()
    col_names <- names(spec$columns)
  }
  for (nm in col_names) {
    spec$columns[[nm]]$width <- 5.0  # 5 inches each — will blow past printable
  }
  expect_warning(fr_validate(spec), "exceed.*110%|printable area")
})

test_that("fr_validate strict errors when widths exceed printable area", {
  spec <- tbl_demog |> fr_table() |> fr_cols()
  col_names <- names(spec$columns)
  for (nm in col_names) {
    spec$columns[[nm]]$width <- 5.0
  }
  expect_error(fr_validate(spec, strict = TRUE), "validation issue")
})

test_that("fr_validate skips width check when split is enabled", {
  spec <- tbl_demog |> fr_table() |> fr_cols()
  col_names <- names(spec$columns)
  for (nm in col_names) {
    spec$columns[[nm]]$width <- 5.0
  }
  spec$columns_meta$split <- TRUE
  # Should NOT warn because split bypasses this check
  expect_invisible(fr_validate(spec))
})

test_that("fr_validate skips width check when any column has auto width", {
  spec <- tbl_demog |> fr_table() |> fr_cols()
  col_names <- names(spec$columns)
  # Set one column to auto (NULL width or "auto")
  spec$columns[[col_names[1]]]$width <- NULL
  # Other columns very wide
  for (nm in col_names[-1]) {
    spec$columns[[nm]]$width <- 10.0
  }
  # NULL width → is.numeric returns FALSE → NA → any(is.na) TRUE → skipped
  expect_invisible(fr_validate(spec))
})


# ── Check 7: Style row/col indices in range ────────────────────────────────

test_that("fr_validate warns when style row index exceeds data rows", {
  spec <- tbl_demog |> fr_table()
  nr <- nrow(tbl_demog)
  spec$cell_styles <- list(
    list(rows = as.integer(c(nr + 10L)), cols = NULL, bold = TRUE)
  )
  expect_warning(fr_validate(spec), "row index.*exceeds")
})

test_that("fr_validate strict errors when style row index exceeds data rows", {
  spec <- tbl_demog |> fr_table()
  nr <- nrow(tbl_demog)
  spec$cell_styles <- list(
    list(rows = as.integer(c(nr + 10L)), cols = NULL, bold = TRUE)
  )
  expect_error(fr_validate(spec, strict = TRUE), "validation issue")
})

test_that("fr_validate passes when style row indices are in range", {
  spec <- tbl_demog |> fr_table()
  spec$cell_styles <- list(
    list(rows = 1L, cols = NULL, bold = TRUE)
  )
  expect_invisible(fr_validate(spec))
})

test_that("fr_validate skips style check for non-integer rows", {
  spec <- tbl_demog |> fr_table()
  # Character row selectors are resolved later — not checked here
  spec$cell_styles <- list(
    list(rows = NULL, cols = NULL, bold = TRUE)
  )
  expect_invisible(fr_validate(spec))
})


# ── Check 8: Font family recognised ───────────────────────────────────────

test_that("fr_validate warns on unrecognised font family", {
  spec <- tbl_demog |> fr_table()
  spec$page$font_family <- "ComicSansUltra"
  expect_warning(fr_validate(spec), "not recognised")
})

test_that("fr_validate strict errors on unrecognised font family", {
  spec <- tbl_demog |> fr_table()
  spec$page$font_family <- "ComicSansUltra"
  expect_error(fr_validate(spec, strict = TRUE), "validation issue")
})

test_that("fr_validate passes with known font families", {
  spec <- tbl_demog |> fr_table()
  # Default font (NULL) should pass
  expect_invisible(fr_validate(spec))

  # Explicit known fonts should also pass
  for (fam in c("Helvetica", "Times", "Courier")) {
    spec$page$font_family <- fam
    expect_invisible(fr_validate(spec))
  }
})


# ── Check 9: sort_by / repeat_cols columns exist ──────────────────────────

test_that("fr_validate warns on bad sort_by column", {
  spec <- tbl_demog |> fr_table()
  spec$body$sort_by <- "nonexistent_sort"
  expect_warning(fr_validate(spec), "sort_by")
})

test_that("fr_validate strict errors on bad sort_by column", {
  spec <- tbl_demog |> fr_table()
  spec$body$sort_by <- "nonexistent_sort"
  expect_error(fr_validate(spec, strict = TRUE), "validation issue")
})

test_that("fr_validate warns on bad repeat_cols column", {
  spec <- tbl_demog |> fr_table()
  spec$body$repeat_cols <- "nonexistent_repeat"
  expect_warning(fr_validate(spec), "repeat_cols")
})

test_that("fr_validate strict errors on bad repeat_cols column", {
  spec <- tbl_demog |> fr_table()
  spec$body$repeat_cols <- "nonexistent_repeat"
  expect_error(fr_validate(spec, strict = TRUE), "validation issue")
})

test_that("fr_validate passes with valid sort_by columns", {
  cols <- names(tbl_demog)
  spec <- tbl_demog |> fr_table()
  spec$body$sort_by <- cols[1]
  expect_invisible(fr_validate(spec))
})

test_that("fr_validate passes with valid repeat_cols columns", {
  cols <- names(tbl_demog)
  spec <- tbl_demog |> fr_table()
  spec$body$repeat_cols <- cols[1]
  expect_invisible(fr_validate(spec))
})


# ── Multiple issues ───────────────────────────────────────────────────────

test_that("fr_validate reports multiple issues at once", {
  spec <- tbl_demog |> fr_table()
  # Inject bad column spec AND bad page_by
  spec$columns[["fake_col"]] <- fr_col("Fake")
  spec$body$page_by <- "nonexistent_page"
  w <- capture_warnings(fr_validate(spec))
  # Should mention both problems
  combined <- paste(w, collapse = " ")
  expect_true(grepl("fake_col", combined))
  expect_true(grepl("page_by", combined))
})

test_that("fr_validate strict reports issue count in error", {
  spec <- tbl_demog |> fr_table()
  spec$columns[["fake_col"]] <- fr_col("Fake")
  spec$body$page_by <- "nonexistent_page"
  expect_error(fr_validate(spec, strict = TRUE), "2 validation issues")
})

test_that("fr_validate reports single issue count correctly", {
  spec <- tbl_demog |> fr_table()
  spec$columns[["fake_col"]] <- fr_col("Fake")
  # Strict mode

  expect_error(fr_validate(spec, strict = TRUE), "1 validation issue")
})
