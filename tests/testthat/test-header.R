# ──────────────────────────────────────────────────────────────────────────────
# test-header.R — Tests for fr_header() and fr_cols()
# ──────────────────────────────────────────────────────────────────────────────

test_that("fr_header sets align, valign, bold", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(align = "center", valign = "top", bold = FALSE)

  expect_equal(spec$header$align, "center")
  expect_equal(spec$header$valign, "top")
  expect_false(spec$header$bold)
})

test_that("fr_header preserves existing spans", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_spans("Treatment" = c("zom_50mg", "zom_100mg")) |>
    fr_header(align = "center")

  expect_length(spec$header$spans, 1L)
  expect_equal(spec$header$align, "center")
})

test_that("fr_header stores n and format", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(
      n = c(zom_50mg = 45, placebo = 45),
      format = "{name}\n(N={n})"
    )

  expect_equal(spec$header$n, c(zom_50mg = 45, placebo = 45))
  expect_equal(spec$header$format, "{name}\n(N={n})")
})

test_that("fr_header n-count labels resolve during finalize_spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg", align = "right"),
      placebo = fr_col("Placebo", align = "right")
    ) |>
    fr_header(
      n = c(zom_50mg = 45, placebo = 45),
      format = "{name}\n(N={n})"
    )

  # finalize_spec resolves the labels
  fspec <- tlframe:::finalize_spec(spec)

  expect_equal(fspec$columns$zom_50mg$label, "Zomerane 50 mg\n(N=45)")
  expect_equal(fspec$columns$placebo$label, "Placebo\n(N=45)")
})

test_that("fr_header and fr_cols are order-independent", {
  # fr_header first, then fr_cols
  spec1 <- tbl_demog |>
    fr_table() |>
    fr_header(n = c(zom_50mg = 45), format = "{name}\n(N={n})") |>
    fr_cols(zom_50mg = fr_col("Zom 50mg", align = "right"))

  # fr_cols first, then fr_header
  spec2 <- tbl_demog |>
    fr_table() |>
    fr_cols(zom_50mg = fr_col("Zom 50mg", align = "right")) |>
    fr_header(n = c(zom_50mg = 45), format = "{name}\n(N={n})")

  fspec1 <- tlframe:::finalize_spec(spec1)
  fspec2 <- tlframe:::finalize_spec(spec2)

  expect_equal(fspec1$columns$zom_50mg$label, "Zom 50mg\n(N=45)")
  expect_equal(fspec2$columns$zom_50mg$label, "Zom 50mg\n(N=45)")
})

test_that("fr_header sets bg and fg", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(bg = "#E0E0E0", fg = "navy")

  expect_equal(spec$header$bg, "#E0E0E0")
  expect_equal(spec$header$fg, "#000080")
})

test_that("fr_header sets font_size", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(font_size = 8)

  expect_equal(spec$header$font_size, 8)
})

test_that("fr_header sets repeat_on_page", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(repeat_on_page = FALSE)

  expect_false(spec$header$repeat_on_page)
})

test_that("fr_header validates inputs", {
  spec <- tbl_demog |> fr_table()

  expect_error(fr_header(spec, n = c(1, 2)), "named numeric")
  expect_error(fr_header(spec, n = "bad_string"), "named numeric")
  expect_error(fr_header(spec, n = TRUE), "named numeric")
  expect_error(fr_header(spec, bold = "yes"), "TRUE.*FALSE")
  expect_error(fr_header(spec, font_size = -1), "positive")

  # auto requires n_subject
  expect_error(fr_header(spec, n = "auto"), "n_subject")

  # list n must be named
  expect_error(fr_header(spec, n = list(c(a = 1))), "named by page_by")

  # list n elements must be named numeric

  expect_error(fr_header(spec, n = list(grp = c(1, 2))), "named numeric")

  # n_data must be a data frame
  expect_error(fr_header(spec, n = "auto", n_subject = "X", n_data = "bad"),
               "data frame")
})

test_that("fr_header align propagates to columns via finalize_spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(align = "center")

  fspec <- tlframe:::finalize_spec(spec)

  # All columns without per-column header_align should get "center"
  for (nm in names(fspec$columns)) {
    expect_equal(fspec$columns[[nm]]$header_align, "center")
  }
})

test_that("per-column header_align overrides fr_header align", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(zom_50mg = fr_col("Zom", header_align = "right")) |>
    fr_header(align = "center")

  fspec <- tlframe:::finalize_spec(spec)

  # zom_50mg has per-column override

  expect_equal(fspec$columns$zom_50mg$header_align, "right")
  # Others get fr_header default
  expect_equal(fspec$columns$characteristic$header_align, "center")
})

test_that("header_cfg flows to build_header_cell_grid", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(.width = "auto") |>
    fr_header(bold = FALSE, bg = "#E0E0E0", font_size = 8)

  fspec <- tlframe:::finalize_spec(spec)
  vis_cols <- Filter(function(c) !isFALSE(c$visible), fspec$columns)

  h_row <- 1L + length(fspec$header$spans)
  hgrid <- tlframe:::build_header_cell_grid(
    vis_cols, fspec$cell_styles, fspec$page, h_row,
    default_valign = "bottom",
    header_cfg = fspec$header
  )

  expect_false(any(hgrid$bold))
  expect_true(all(hgrid$bg == "#E0E0E0"))
  expect_true(all(hgrid$font_size == 8))
})


# ──────────────────────────────────────────────────────────────────────────────
# Per-group N-count tests
# ──────────────────────────────────────────────────────────────────────────────

test_that("fr_header stores per-group list n", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(
      n = list(
        "Group A" = c(zom_50mg = 42, placebo = 40),
        "Group B" = c(zom_50mg = 45, placebo = 44)
      ),
      format = "{name}\n(N={n})"
    )

  expect_true(is.list(spec$header$n))
  expect_length(spec$header$n, 2L)
  expect_equal(spec$header$n[["Group A"]], c(zom_50mg = 42, placebo = 40))
})

test_that("fr_header stores function n", {
  n_fn <- function(group_data, group_label) c(zom_50mg = 42L)
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(n = n_fn, format = "{name}\n(N={n})")

  expect_true(is.function(spec$header$n))
})

test_that("fr_header stores n_subject and n_data", {
  src <- data.frame(USUBJID = c("S1", "S2"), grp = c("A", "B"))
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(
      n = "auto", n_subject = "USUBJID", n_data = src,
      format = "{name}\n(N={n})"
    )

  expect_equal(spec$header$n, "auto")
  expect_equal(spec$header$n_subject, "USUBJID")
  expect_true(is.data.frame(spec$header$n_data))
})

test_that("per-group list n skips global resolution in finalize_spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg", align = "right"),
      placebo  = fr_col("Placebo", align = "right")
    ) |>
    fr_header(
      n = list("Group A" = c(zom_50mg = 42, placebo = 40)),
      format = "{name}\n(N={n})"
    )

  # finalize_spec should NOT resolve labels for per-group n
  suppressWarnings(fspec <- tlframe:::finalize_spec(spec))

  # Labels should remain unmodified (base labels, no N appended)
  expect_equal(fspec$columns$zom_50mg$label, "Zomerane 50 mg")
  expect_equal(fspec$columns$placebo$label, "Placebo")
})

test_that("resolve_group_labels returns overrides for per-group list", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg"),
      placebo  = fr_col("Placebo")
    ) |>
    fr_header(
      n = list(
        "Group A" = c(zom_50mg = 42, placebo = 40),
        "Group B" = c(zom_50mg = 45, placebo = 44)
      ),
      format = "{name}\n(N={n})"
    )

  fspec <- suppressWarnings(tlframe:::finalize_spec(spec))

  ov_a <- tlframe:::resolve_group_labels(fspec, fspec$data, "Group A")
  expect_equal(ov_a[["zom_50mg"]], "Zomerane 50 mg\n(N=42)")
  expect_equal(ov_a[["placebo"]], "Placebo\n(N=40)")

  ov_b <- tlframe:::resolve_group_labels(fspec, fspec$data, "Group B")
  expect_equal(ov_b[["zom_50mg"]], "Zomerane 50 mg\n(N=45)")
  expect_equal(ov_b[["placebo"]], "Placebo\n(N=44)")
})

test_that("resolve_group_labels returns NULL for missing group key", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(
      n = list("Group A" = c(zom_50mg = 42)),
      format = "{name}\n(N={n})"
    )

  fspec <- suppressWarnings(tlframe:::finalize_spec(spec))
  ov <- tlframe:::resolve_group_labels(fspec, fspec$data, "Missing Group")
  expect_null(ov)
})

test_that("resolve_group_labels returns NULL for global numeric n", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(
      n = c(zom_50mg = 45, placebo = 45),
      format = "{name}\n(N={n})"
    )

  fspec <- tlframe:::finalize_spec(spec)
  ov <- tlframe:::resolve_group_labels(fspec, fspec$data, NULL)
  expect_null(ov)
})

test_that("resolve_group_labels works with function n", {
  n_fn <- function(group_data, group_label) {
    c(zom_50mg = 99L, placebo = 88L)
  }

  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zom"),
      placebo  = fr_col("Placebo")
    ) |>
    fr_header(n = n_fn, format = "{name}\n(N={n})")

  fspec <- tlframe:::finalize_spec(spec)
  ov <- tlframe:::resolve_group_labels(fspec, fspec$data, "SomeGroup")
  expect_equal(ov[["zom_50mg"]], "Zom\n(N=99)")
  expect_equal(ov[["placebo"]], "Placebo\n(N=88)")
})

test_that("resolve_group_labels works with n = auto and n_data", {
  # Source data: 3 subjects for col_a, 2 for col_b (per group)
  src <- data.frame(
    grp = rep(c("G1", "G2"), each = 4),
    SUBJ = c("S1", "S2", "S3", "S3", "S4", "S5", "S5", "S5"),
    col_a = c("x", "x", "x", NA, "x", "x", NA, NA),
    col_b = c("y", "y", NA, NA, "y", "y", "y", NA),
    stringsAsFactors = FALSE
  )

  display <- data.frame(
    grp = c("G1", "G2"),
    col_a = c("summary_a1", "summary_a2"),
    col_b = c("summary_b1", "summary_b2"),
    stringsAsFactors = FALSE
  )

  spec <- display |>
    fr_table() |>
    fr_rows(page_by = "grp") |>
    fr_cols(
      col_a = fr_col("Col A"),
      col_b = fr_col("Col B")
    ) |>
    fr_header(
      n = "auto", n_subject = "SUBJ", n_data = src,
      format = "{name}\n(N={n})"
    )

  fspec <- tlframe:::finalize_spec(spec)

  # G1: col_a has S1, S2, S3 with non-NA -> 3; col_b has S1, S2 -> 2
  ov_g1 <- tlframe:::resolve_group_labels(fspec, display[1, ], "G1")
  expect_equal(ov_g1[["col_a"]], "Col A\n(N=3)")
  expect_equal(ov_g1[["col_b"]], "Col B\n(N=2)")

  # G2: col_a has S4, S5 with non-NA -> 2; col_b has S4, S5 -> 2
  ov_g2 <- tlframe:::resolve_group_labels(fspec, display[2, ], "G2")
  expect_equal(ov_g2[["col_a"]], "Col A\n(N=2)")
  expect_equal(ov_g2[["col_b"]], "Col B\n(N=2)")
})

test_that("n = auto without page_by counts from full data", {
  src <- data.frame(
    SUBJ = c("S1", "S2", "S3"),
    col_a = c("x", "x", NA),
    stringsAsFactors = FALSE
  )

  display <- data.frame(
    col_a = "summary",
    stringsAsFactors = FALSE
  )

  spec <- display |>
    fr_table() |>
    fr_cols(col_a = fr_col("Col A")) |>
    fr_header(
      n = "auto", n_subject = "SUBJ", n_data = src,
      format = "{name}\n(N={n})"
    )

  fspec <- tlframe:::finalize_spec(spec)
  ov <- tlframe:::resolve_group_labels(fspec, display, NULL)
  # S1, S2 have non-NA col_a -> N=2
  expect_equal(ov[["col_a"]], "Col A\n(N=2)")
})

test_that("per-group list n without page_by emits warning", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(
      n = list("A" = c(zom_50mg = 42)),
      format = "{name}\n(N={n})"
    )

  expect_warning(tlframe:::finalize_spec(spec), "page_by")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_spans — tidyselect integration
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_spans with starts_with() resolves columns", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_spans("Zomerane" = starts_with("zom_"))
  expect_length(spec$header$spans, 1L)
  expect_equal(spec$header$spans[[1]]$columns, c("zom_50mg", "zom_100mg"))
})

test_that("fr_spans with contains() resolves columns", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_spans("50 mg Group" = contains("50"))
  expect_length(spec$header$spans, 1L)
  expect_equal(spec$header$spans[[1]]$columns, "zom_50mg")
})

test_that("fr_spans with character vector still works (no regression)", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_spans("Zomerane" = c("zom_50mg", "zom_100mg"))
  expect_length(spec$header$spans, 1L)
  expect_equal(spec$header$spans[[1]]$columns, c("zom_50mg", "zom_100mg"))
})
