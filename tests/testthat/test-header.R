# ──────────────────────────────────────────────────────────────────────────────
# test-header.R — Tests for fr_header() and N-count system (now on fr_cols)
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

test_that("fr_cols stores .n and .n_format on columns_meta", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg"),
      placebo = fr_col("Placebo"),
      .n = c("Zomerane 50 mg" = 45, "Placebo" = 45),
      .n_format = "{label}\n(N={n})"
    )

  expect_equal(spec$columns_meta$n, c("Zomerane 50 mg" = 45, "Placebo" = 45))
  expect_equal(spec$columns_meta$n_format, "{label}\n(N={n})")
})

test_that("per-column n resolves during finalize_spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg", align = "right", n = 45L),
      placebo = fr_col("Placebo", align = "right", n = 45L),
      .n_format = "{label}\n(N={n})"
    )

  fspec <- arframe:::finalize_spec(spec)

  expect_equal(fspec$columns$zom_50mg$label, "Zomerane 50 mg\n(N=45)")
  expect_equal(fspec$columns$placebo$label, "Placebo\n(N=45)")
})

test_that("bulk .n resolves during finalize_spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg", align = "right"),
      placebo = fr_col("Placebo", align = "right"),
      .n = c("Zomerane 50 mg" = 45, "Placebo" = 45),
      .n_format = "{label}\n(N={n})"
    )

  fspec <- arframe:::finalize_spec(spec)
  expect_equal(fspec$columns$zom_50mg$label, "Zomerane 50 mg\n(N=45)")
  expect_equal(fspec$columns$placebo$label, "Placebo\n(N=45)")
})

test_that("fr_cols and fr_header are order-independent", {
  # fr_header first, then fr_cols
  spec1 <- tbl_demog |>
    fr_table() |>
    fr_header(bold = TRUE) |>
    fr_cols(
      zom_50mg = fr_col("Zom 50mg", align = "right", n = 45L),
      .n_format = "{label}\n(N={n})"
    )

  # fr_cols first, then fr_header
  spec2 <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zom 50mg", align = "right", n = 45L),
      .n_format = "{label}\n(N={n})"
    ) |>
    fr_header(bold = TRUE)

  fspec1 <- arframe:::finalize_spec(spec1)
  fspec2 <- arframe:::finalize_spec(spec2)

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

  expect_error(fr_header(spec, bold = "yes"), "TRUE.*FALSE")
  expect_error(fr_header(spec, font_size = -1), "positive")
})

test_that("fr_header align propagates to columns via finalize_spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_header(align = "center")

  fspec <- arframe:::finalize_spec(spec)

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

  fspec <- arframe:::finalize_spec(spec)

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

  fspec <- arframe:::finalize_spec(spec)
  vis_cols <- Filter(function(c) !isFALSE(c$visible), fspec$columns)

  h_row <- 1L + length(fspec$header$spans)
  hgrid <- arframe:::build_header_cell_grid(
    vis_cols,
    fspec$cell_styles,
    fspec$page,
    h_row,
    default_valign = "bottom",
    header_cfg = fspec$header
  )

  expect_false(any(hgrid$bold))
  expect_true(all(hgrid$bg == "#E0E0E0"))
  expect_true(all(hgrid$font_size == 8))
})


# ──────────────────────────────────────────────────────────────────────────────
# Per-column N-count tests (now on fr_col/fr_cols)
# ──────────────────────────────────────────────────────────────────────────────

test_that("fr_col stores n and group", {
  col <- fr_col("Placebo", n = 45L, group = "Treatment")
  expect_equal(col$n, 45L)
  expect_equal(col$group, "Treatment")
})

test_that("fr_col validates n is non-negative integer", {
  expect_error(fr_col("X", n = -1L), "non-negative")
  expect_error(fr_col("X", n = 1.5), "non-negative")
  expect_no_error(fr_col("X", n = 0L))
})

test_that("per-column n takes priority over bulk .n", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg", n = 99L),
      placebo = fr_col("Placebo"),
      .n = c("Zomerane 50 mg" = 50, "Placebo" = 45),
      .n_format = "{label}\n(N={n})"
    )

  fspec <- arframe:::finalize_spec(spec)
  # Per-column n = 99 wins over bulk .n = 50
  expect_equal(fspec$columns$zom_50mg$label, "Zomerane 50 mg\n(N=99)")
  # Bulk .n works for placebo
  expect_equal(fspec$columns$placebo$label, "Placebo\n(N=45)")
})


# ──────────────────────────────────────────────────────────────────────────────
# Per-group N-count tests (now on fr_cols .n)
# ──────────────────────────────────────────────────────────────────────────────

test_that("fr_cols stores per-group list .n", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg"),
      placebo = fr_col("Placebo"),
      .n = list(
        "Group A" = c("Zomerane 50 mg" = 42, "Placebo" = 40),
        "Group B" = c("Zomerane 50 mg" = 45, "Placebo" = 44)
      ),
      .n_format = "{label}\n(N={n})"
    )

  expect_true(is.list(spec$columns_meta$n))
  expect_length(spec$columns_meta$n, 2L)
})

test_that("per-group list .n skips global resolution in finalize_spec", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg", align = "right"),
      placebo = fr_col("Placebo", align = "right"),
      .n = list("Group A" = c("Zomerane 50 mg" = 42, "Placebo" = 40)),
      .n_format = "{label}\n(N={n})"
    )

  # finalize_spec should NOT resolve labels for per-group n
  suppressWarnings(fspec <- arframe:::finalize_spec(spec))

  # Labels should remain unmodified (base labels, no N appended)
  expect_equal(fspec$columns$zom_50mg$label, "Zomerane 50 mg")
  expect_equal(fspec$columns$placebo$label, "Placebo")
})

test_that("resolve_group_labels returns overrides for per-group list", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg"),
      placebo = fr_col("Placebo"),
      .n = list(
        "Group A" = c("Zomerane 50 mg" = 42, "Placebo" = 40),
        "Group B" = c("Zomerane 50 mg" = 45, "Placebo" = 44)
      ),
      .n_format = "{label}\n(N={n})"
    )

  fspec <- suppressWarnings(arframe:::finalize_spec(spec))

  res_a <- arframe:::resolve_group_labels(fspec, fspec$data, "Group A")
  ov_a <- res_a$columns
  expect_equal(ov_a[["zom_50mg"]], "Zomerane 50 mg\n(N=42)")
  expect_equal(ov_a[["placebo"]], "Placebo\n(N=40)")

  res_b <- arframe:::resolve_group_labels(fspec, fspec$data, "Group B")
  ov_b <- res_b$columns
  expect_equal(ov_b[["zom_50mg"]], "Zomerane 50 mg\n(N=45)")
  expect_equal(ov_b[["placebo"]], "Placebo\n(N=44)")
})

test_that("resolve_group_labels returns NULL for missing group key", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      .n = list("Group A" = c("Zomerane 50 mg" = 42)),
      .n_format = "{label}\n(N={n})"
    )

  fspec <- suppressWarnings(arframe:::finalize_spec(spec))
  ov <- arframe:::resolve_group_labels(fspec, fspec$data, "Missing Group")
  expect_null(ov)
})

test_that("resolve_group_labels returns NULL for global numeric .n", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zomerane 50 mg"),
      placebo = fr_col("Placebo"),
      .n = c("Zomerane 50 mg" = 45, "Placebo" = 45),
      .n_format = "{label}\n(N={n})"
    )

  fspec <- arframe:::finalize_spec(spec)
  ov <- arframe:::resolve_group_labels(fspec, fspec$data, NULL)
  expect_null(ov)
})


test_that("per-group list .n without page_by emits warning", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      .n = list("A" = c("Zomerane 50 mg" = 42)),
      .n_format = "{label}\n(N={n})"
    )

  expect_warning(arframe:::finalize_spec(spec), "page_by")
})

test_that("{label} token works in .n_format string", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zom", n = 45L),
      .n_format = "{label}\n(N={n})"
    )

  fspec <- arframe:::finalize_spec(spec)
  expect_equal(fspec$columns$zom_50mg$label, "Zom\n(N=45)")
})


# ══════════════════════════════════════════════════════════════════════════════
# parse_df_n_counts() tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("parse_df_n_counts handles 2-col data frame", {
  spec <- tbl_demog |> fr_table()
  df <- data.frame(trt = c("A", "B"), n = c(10L, 20L))
  result <- arframe:::parse_df_n_counts(df, spec)
  expect_equal(result$type, "global")
  expect_equal(result$counts, c(A = 10L, B = 20L))
})

test_that("parse_df_n_counts handles 3-col data frame", {
  spec <- tbl_demog |> fr_table()
  df <- data.frame(
    grp = c("G1", "G1", "G2"),
    trt = c("A", "B", "A"),
    n = c(10L, 20L, 30L)
  )
  result <- arframe:::parse_df_n_counts(df, spec)
  expect_equal(result$type, "per_group")
  expect_equal(result$page_col, 1L)
  expect_equal(result$trt_col, 2L)
  expect_equal(result$count_col, 3L)
})


# ══════════════════════════════════════════════════════════════════════════════
# match_trt_to_columns() tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("match_trt_to_columns matches case-insensitively to labels", {
  columns <- list(
    col_a = fr_col("Placebo"),
    col_b = fr_col("Zomerane 50mg")
  )
  counts <- c("placebo" = 45L, "zomerane 50mg" = 44L)
  result <- arframe:::match_trt_to_columns(counts, columns)
  expect_equal(result[["col_a"]], 45L)
  expect_equal(result[["col_b"]], 44L)
})

test_that("match_trt_to_columns skips unmatched treatments", {
  columns <- list(col_a = fr_col("Placebo"))
  counts <- c("Placebo" = 45L, "Unknown" = 99L)
  result <- arframe:::match_trt_to_columns(counts, columns)
  expect_equal(length(result), 1L)
  expect_equal(result[["col_a"]], 45L)
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_spans — .gap parameter
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_spans .gap sets span_gap on header", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_spans("Zomerane" = c("zom_50mg", "zom_100mg"), .gap = FALSE)

  expect_false(spec$header$span_gap)
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

test_that("fr_spans errors on overlapping columns at same level", {
  spec <- tbl_demog |> fr_table()

  # Same call: two spans sharing a column at level 1

  expect_error(
    spec |>
      fr_spans(
        "A" = c("zom_50mg", "zom_100mg"),
        "B" = c("zom_100mg", "total")
      ),
    "multiple spans"
  )

  # Across calls: second call introduces overlap at same level
  spec2 <- spec |> fr_spans("A" = c("zom_50mg", "zom_100mg"), .level = 1L)
  expect_error(
    spec2 |> fr_spans("B" = c("zom_100mg", "total"), .level = 1L),
    "multiple spans"
  )

  # Different levels: same column at different levels is fine
  expect_no_error(
    spec |>
      fr_spans("A" = c("zom_50mg", "zom_100mg"), .level = 1L) |>
      fr_spans("B" = c("zom_50mg", "zom_100mg"), .level = 2L)
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# Direct data frame N-count tests (now via fr_cols .n)
# ══════════════════════════════════════════════════════════════════════════════

test_that("validate_n_param accepts 2-col data frame", {
  df <- data.frame(trt = c("A", "B"), n = c(10L, 20L))
  expect_no_error(arframe:::validate_n_param(df))
})

test_that("validate_n_param accepts 3-col data frame", {
  df <- data.frame(grp = "G1", trt = "A", n = 10L)
  expect_no_error(arframe:::validate_n_param(df))
})

test_that("validate_n_param rejects 1-col data frame", {
  df <- data.frame(n = c(10L, 20L))
  expect_error(arframe:::validate_n_param(df), "2 or 3 columns")
})

test_that("validate_n_param rejects 4-col data frame", {
  df <- data.frame(a = 1, b = 2, c = 3, d = 4)
  expect_error(arframe:::validate_n_param(df), "2 or 3 columns")
})

test_that("validate_n_param rejects non-numeric last column", {
  df <- data.frame(trt = "A", n = "ten")
  expect_error(arframe:::validate_n_param(df), "numeric")
})

test_that("2-col data frame resolves globally via finalize_spec", {
  df <- data.frame(
    trt = c("Zom", "Placebo"),
    n = c(99L, 88L)
  )

  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zom"),
      placebo = fr_col("Placebo"),
      .n = df,
      .n_format = "{label}\n(N={n})"
    )

  fspec <- arframe:::finalize_spec(spec)
  expect_equal(fspec$columns$zom_50mg$label, "Zom\n(N=99)")
  expect_equal(fspec$columns$placebo$label, "Placebo\n(N=88)")
})

test_that("3-col data frame resolves per-group via resolve_group_labels", {
  df <- data.frame(
    param = c("G1", "G1", "G2", "G2"),
    trt = c("Col A", "Col B", "Col A", "Col B"),
    n = c(10L, 20L, 30L, 40L)
  )

  display <- data.frame(
    grp = c("G1", "G2"),
    col_a = c("x", "y"),
    col_b = c("x", "y"),
    stringsAsFactors = FALSE
  )

  spec <- display |>
    fr_table() |>
    fr_rows(page_by = "grp") |>
    fr_cols(
      col_a = fr_col("Col A"),
      col_b = fr_col("Col B"),
      .n = df,
      .n_format = "{label}\n(N={n})"
    )

  fspec <- arframe:::finalize_spec(spec)

  # Labels NOT resolved globally (3-col → per-group)
  expect_equal(fspec$columns$col_a$label, "Col A")
  expect_equal(fspec$columns$col_b$label, "Col B")

  # Per-group resolution
  res_g1 <- arframe:::resolve_group_labels(fspec, display[1, ], "G1")
  expect_equal(res_g1$columns[["col_a"]], "Col A\n(N=10)")
  expect_equal(res_g1$columns[["col_b"]], "Col B\n(N=20)")

  res_g2 <- arframe:::resolve_group_labels(fspec, display[2, ], "G2")
  expect_equal(res_g2$columns[["col_a"]], "Col A\n(N=30)")
  expect_equal(res_g2$columns[["col_b"]], "Col B\n(N=40)")
})

test_that("3-col data frame without page_by emits warning", {
  df <- data.frame(grp = "G1", trt = "A", n = 10L)

  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("A"),
      .n = df,
      .n_format = "{label}\n(N={n})"
    )

  expect_warning(arframe:::finalize_spec(spec), "page_by")
})

test_that("2-col data frame matches spanner labels", {
  df <- data.frame(
    trt = c("Zomerane", "Placebo"),
    n = c(89L, 45L)
  )

  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("Zom 50mg"),
      zom_100mg = fr_col("Zom 100mg"),
      placebo = fr_col("Placebo"),
      .n = df,
      .n_format = "{label}\n(N={n})"
    ) |>
    fr_spans("Zomerane" = c("zom_50mg", "zom_100mg"))

  fspec <- arframe:::finalize_spec(spec)

  # Spanner label should be overridden
  expect_equal(fspec$header$spans[[1]]$label, "Zomerane\n(N=89)")
  # Column matched
  expect_equal(fspec$columns$placebo$label, "Placebo\n(N=45)")
})

test_that("unmatched page_by group warns in resolve_group_labels", {
  df <- data.frame(
    param = c("G1", "G1"),
    trt = c("Col A", "Col B"),
    n = c(10L, 20L)
  )

  display <- data.frame(
    grp = c("G1", "G2"),
    col_a = c("x", "y"),
    col_b = c("x", "y"),
    stringsAsFactors = FALSE
  )

  spec <- display |>
    fr_table() |>
    fr_rows(page_by = "grp") |>
    fr_cols(
      col_a = fr_col("Col A"),
      col_b = fr_col("Col B"),
      .n = df,
      .n_format = "{label}\n(N={n})"
    )

  fspec <- arframe:::finalize_spec(spec)

  # G2 not in df → should warn
  expect_warning(
    arframe:::resolve_group_labels(fspec, display[2, ], "G2"),
    "No N-counts found"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# Inline spanning via group=
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_col group generates auto-spans", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      characteristic = fr_col("Characteristic"),
      zom_50mg = fr_col("50 mg", group = "Zomerane"),
      zom_100mg = fr_col("100 mg", group = "Zomerane"),
      placebo = fr_col("Placebo"),
      total = fr_col("Total")
    )

  fspec <- arframe:::finalize_spec(spec)

  # Should have one auto-generated span
  expect_length(fspec$header$spans, 1L)
  expect_equal(fspec$header$spans[[1]]$label, "Zomerane")
  expect_equal(fspec$header$spans[[1]]$columns, c("zom_50mg", "zom_100mg"))
  expect_equal(fspec$header$spans[[1]]$level, 1L)
})

test_that("group auto-span is overridden by explicit fr_spans", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      zom_50mg = fr_col("50 mg", group = "Zomerane"),
      zom_100mg = fr_col("100 mg", group = "Zomerane")
    ) |>
    fr_spans("Zomerane" = c("zom_50mg", "zom_100mg", "placebo"))

  fspec <- arframe:::finalize_spec(spec)

  # Explicit fr_spans should win — 3 columns, not 2
  zom_spans <- Filter(function(s) s$label == "Zomerane", fspec$header$spans)
  expect_length(zom_spans, 1L)
  expect_equal(zom_spans[[1]]$columns, c("zom_50mg", "zom_100mg", "placebo"))
})

test_that("single-column group creates single-column span", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      placebo = fr_col("Placebo", group = "Control")
    )

  fspec <- arframe:::finalize_spec(spec)

  ctrl_spans <- Filter(function(s) s$label == "Control", fspec$header$spans)
  expect_length(ctrl_spans, 1L)
  expect_equal(ctrl_spans[[1]]$columns, "placebo")
})

test_that("bulk .n auto-routes to spans from group=", {
  n_df <- data.frame(
    trt = c("Zomerane", "Placebo"),
    n = c(90L, 45L)
  )

  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      characteristic = fr_col("Characteristic"),
      zom_50mg = fr_col("50 mg", group = "Zomerane"),
      zom_100mg = fr_col("100 mg", group = "Zomerane"),
      placebo = fr_col("Placebo"),
      .n = n_df,
      .n_format = "{label}\n(N={n})"
    )

  fspec <- arframe:::finalize_spec(spec)

  # "Zomerane" matches group → span should get N
  zom_spans <- Filter(
    function(s) grepl("Zomerane", s$label),
    fspec$header$spans
  )
  expect_length(zom_spans, 1L)
  expect_equal(zom_spans[[1]]$label, "Zomerane\n(N=90)")

  # "Placebo" matches column → column should get N
  expect_equal(fspec$columns$placebo$label, "Placebo\n(N=45)")
})


# ══════════════════════════════════════════════════════════════════════════════
# Named list align (tidyselect) in fr_header
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_header align as named list resolves via tidyselect", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      characteristic = fr_col("Characteristic"),
      zom_50mg = fr_col("Zomerane 50 mg"),
      placebo = fr_col("Placebo"),
      total = fr_col("Total")
    ) |>
    fr_header(
      align = list(
        left = "characteristic",
        center = c(starts_with("zom"), "placebo"),
        right = "total"
      )
    )

  expect_null(spec$header$align)
  expect_equal(spec$header$align_map[["characteristic"]], "left")
  expect_equal(spec$header$align_map[["zom_50mg"]], "center")
  expect_equal(spec$header$align_map[["placebo"]], "center")
  expect_equal(spec$header$align_map[["total"]], "right")
})

test_that("align_map overrides scalar align but not fr_col header_align", {
  spec <- tbl_demog |>
    fr_table() |>
    fr_cols(
      characteristic = fr_col("Characteristic", header_align = "left"),
      zom_50mg = fr_col("Zomerane 50 mg"),
      placebo = fr_col("Placebo")
    ) |>
    fr_header(
      align = list(
        center = c("characteristic", "zom_50mg", "placebo")
      )
    )

  fspec <- arframe:::finalize_spec(spec)

  # characteristic has fr_col(header_align = "left") → must win
  expect_equal(fspec$columns$characteristic$header_align, "left")
  # zom_50mg has no per-column override → align_map applies
  expect_equal(fspec$columns$zom_50mg$header_align, "center")
})

test_that("fr_header rejects invalid align list names", {
  spec <- tbl_demog |> fr_table()
  expect_error(
    fr_header(spec, align = list(bad_name = "characteristic")),
    "valid alignments"
  )
})
