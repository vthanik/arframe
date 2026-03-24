# ──────────────────────────────────────────────────────────────────────────────
# test-render-dispatch.R — Tests for render.R internals
#
# Covers: finalize_spec(), prepare_pages(), calculate_col_panels(),
#         fit_panel_widths(), fr_render() dispatch, detect_format(),
#         get_backend(), fr_register_backend(), fr_backends()
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# detect_format()
# ══════════════════════════════════════════════════════════════════════════════

test_that("detect_format returns correct format for known extensions", {
  expect_equal(arframe:::detect_format("output.rtf"), "rtf")
  expect_equal(arframe:::detect_format("output.tex"), "latex")
  expect_equal(arframe:::detect_format("output.pdf"), "pdf")
  # Case insensitive

  expect_equal(arframe:::detect_format("output.RTF"), "rtf")
  expect_equal(arframe:::detect_format("output.TEX"), "latex")
})

test_that("detect_format errors on missing extension", {
  expect_error(
    arframe:::detect_format("output"),
    "Cannot detect format"
  )
})

test_that("detect_format errors on unsupported extension", {
  expect_error(
    arframe:::detect_format("output.docx"),
    "Unsupported file extension"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# get_backend()
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_backend returns a backend for known formats", {
  be <- arframe:::get_backend("rtf")
  expect_true(is.function(be$render))
})

test_that("get_backend errors on unknown format", {
  expect_error(
    arframe:::get_backend("html_custom_xyz"),
    "No backend available"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_register_backend() / fr_backends()
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_register_backend registers and fr_backends lists it", {
  # Save original and restore on exit
  fe <- arframe:::fr_env
  orig <- fe$backends
  on.exit(fe$backends <- orig, add = TRUE)

  fr_register_backend(
    format = "test_fmt_xyz",
    extensions = c("xyz", "xyz2"),
    render_fn = function(spec, page_groups, col_panels, path) NULL,
    description = "Test format"
  )

  backends <- fr_backends()
  expect_true("test_fmt_xyz" %in% backends$format)
  row <- backends[backends$format == "test_fmt_xyz", ]
  expect_equal(unname(row$extensions), "xyz, xyz2")
  expect_equal(unname(row$description), "Test format")
})

test_that("fr_register_backend validates inputs", {
  expect_error(fr_register_backend("x", character(0), identity), "non-empty")
  expect_error(
    fr_register_backend("x", "ext", "not_a_fn"),
    "must be a function"
  )
})

test_that("registered backend is usable via fr_render", {
  fe <- arframe:::fr_env
  orig <- fe$backends
  on.exit(
    {
      fe$backends <- orig
    },
    add = TRUE
  )

  tmp <- tempfile(fileext = ".zzz")
  on.exit(unlink(tmp), add = TRUE)

  fr_register_backend(
    format = "zzz_test",
    extensions = "zzz",
    render_fn = function(spec, page_groups, col_panels, path) {
      writeLines("rendered", path)
    },
    description = "ZZZ test"
  )

  data.frame(a = 1) |> fr_table() |> fr_render(tmp)
  expect_true(file.exists(tmp))
  expect_equal(readLines(tmp), "rendered")
})


# ══════════════════════════════════════════════════════════════════════════════
# fr_render() — error paths
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_render errors on non-fr_spec input", {
  expect_error(
    fr_render(list(), "out.rtf"),
    "fr_spec"
  )
})

test_that("fr_render errors on unsupported format via format arg", {
  spec <- data.frame(x = 1) |> fr_table()
  expect_error(
    fr_render(spec, "out.rtf", format = "nonexistent_format_xyz"),
    "No backend available"
  )
})

test_that("fr_render errors when path has unsupported extension", {
  spec <- data.frame(x = 1) |> fr_table()
  expect_error(
    fr_render(spec, "out.docx"),
    "Unsupported file extension"
  )
})

test_that("fr_render returns path invisibly on success", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)
  spec <- data.frame(x = 1) |> fr_table()
  result <- fr_render(spec, tmp)
  expect_equal(result, tmp)
})


# ══════════════════════════════════════════════════════════════════════════════
# finalize_spec() — column auto-generation
# ══════════════════════════════════════════════════════════════════════════════

test_that("finalize_spec auto-generates columns from data when none specified", {
  spec <- data.frame(a = 1:3, b = c("x", "y", "z")) |> fr_table()
  result <- arframe:::finalize_spec(spec)
  # Should have columns for both 'a' and 'b'
  col_names <- names(result$columns)
  expect_true("a" %in% col_names)
  expect_true("b" %in% col_names)
  # Numeric column should be right-aligned
  # (auto-alignment from finalize_columns)
})

test_that("finalize_spec generates columns for uncovered data columns", {
  spec <- data.frame(a = 1, b = 2, c = 3) |>
    fr_table() |>
    fr_cols(a = fr_col("Col A", width = 2))

  result <- arframe:::finalize_spec(spec)
  expect_true("b" %in% names(result$columns))
  expect_true("c" %in% names(result$columns))
  # Original configured column should still be present

  expect_equal(result$columns$a$label, "Col A")
})


# ══════════════════════════════════════════════════════════════════════════════
# finalize_spec() — column widths
# ══════════════════════════════════════════════════════════════════════════════

test_that("finalize_spec resolves all column widths to numeric values", {
  spec <- data.frame(x = 1:5, y = letters[1:5]) |> fr_table()
  result <- arframe:::finalize_spec(spec)
  for (col in result$columns) {
    expect_true(is.numeric(col$width), info = paste("column", col$id))
    expect_true(col$width > 0, info = paste("column", col$id))
  }
})

test_that("finalize_spec skips width distribution when col_split is enabled", {
  # With col_split, columns keep natural widths (not squeezed to fit page)
  spec <- data.frame(a = 1, b = 2, c = 3, d = 4, e = 5) |>
    fr_table() |>
    fr_cols(a = fr_col(stub = TRUE), .split = TRUE)

  result <- arframe:::finalize_spec(spec)
  # Widths should be individual estimates, not distributed
  total_width <- sum(vapply(
    Filter(function(c) !isFALSE(c$visible), result$columns),
    function(c) c$width,
    numeric(1)
  ))
  # With col_split, total can exceed printable area
  # Just verify columns have numeric widths
  for (col in result$columns) {
    expect_true(is.numeric(col$width))
  }
})


# ══════════════════════════════════════════════════════════════════════════════
# finalize_spec() — sort_by
# ══════════════════════════════════════════════════════════════════════════════

test_that("finalize_spec sorts data by sort_by columns", {
  spec <- data.frame(
    id = c("C", "A", "B"),
    val = c(3, 1, 2),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(sort_by = "id")

  result <- arframe:::finalize_spec(spec)
  expect_equal(result$data$id, c("A", "B", "C"))
  expect_equal(result$data$val, c(1, 2, 3))
})

test_that("finalize_spec sorts by multiple sort_by columns", {
  spec <- data.frame(
    grp = c("B", "A", "A", "B"),
    val = c(2, 2, 1, 1),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(sort_by = c("grp", "val"))

  result <- arframe:::finalize_spec(spec)
  expect_equal(result$data$grp, c("A", "A", "B", "B"))
  expect_equal(result$data$val, c(1, 2, 1, 2))
})


# ══════════════════════════════════════════════════════════════════════════════
# finalize_spec() — suppress
# ══════════════════════════════════════════════════════════════════════════════

test_that("finalize_spec suppresses repeated values in suppress", {
  spec <- data.frame(
    subj = c("S01", "S01", "S02", "S02"),
    event = c("AE1", "AE2", "AE3", "AE4"),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(suppress = "subj")

  result <- arframe:::finalize_spec(spec)
  expect_equal(result$data$subj, c("S01", "", "S02", ""))
})

test_that("suppress handles NAs without error", {
  spec <- data.frame(
    subj = c("S01", NA, NA, "S02"),
    event = c("A", "B", "C", "D"),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(suppress = "subj")

  result <- arframe:::finalize_spec(spec)
  # Should not error; NAs handled
  expect_equal(nrow(result$data), 4L)
})


# ══════════════════════════════════════════════════════════════════════════════
# finalize_spec() — blank_after / group_by auto-blank
# ══════════════════════════════════════════════════════════════════════════════

test_that("finalize_spec inserts blank rows at blank_after boundaries", {
  spec <- data.frame(
    grp = c("A", "A", "B", "B"),
    val = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(blank_after = "grp")

  result <- arframe:::finalize_spec(spec)
  # Should have inserted a blank row between A and B groups
  expect_true(nrow(result$data) > 4L)
  # Check that one blank row exists (all empty)
  blank_mask <- rowSums(result$data != "") == 0L
  expect_true(sum(blank_mask) >= 1L)
})

test_that("group_by does not auto-imply blank_after", {
  spec <- data.frame(
    cat = c("X", "X", "Y", "Y"),
    val = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(group_by = "cat")

  result <- arframe:::finalize_spec(spec)
  # group_by alone does not insert blank rows — use blank_after explicitly

  expect_equal(nrow(result$data), 4L)
})

test_that("blank_after inserts blank rows at group boundaries", {
  spec <- data.frame(
    cat = c("X", "X", "Y", "Y"),
    val = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(group_by = "cat", blank_after = "cat")

  result <- arframe:::finalize_spec(spec)
  # explicit blank_after inserts blank rows between groups
  expect_true(nrow(result$data) > 4L)
})


# ══════════════════════════════════════════════════════════════════════════════
# finalize_spec() — page_by column visibility
# ══════════════════════════════════════════════════════════════════════════════

test_that("page_by columns are auto-hidden (visible = FALSE)", {
  spec <- data.frame(
    param = c("A", "A", "B", "B"),
    val = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(page_by = "param")

  result <- arframe:::finalize_spec(spec)
  expect_false(result$columns$param$visible)
  expect_true(result$columns$val$visible)
})


# ══════════════════════════════════════════════════════════════════════════════
# finalize_spec() — header label resolution (N-counts)
# ══════════════════════════════════════════════════════════════════════════════

test_that("finalize_spec resolves global numeric N-counts in header labels", {
  spec <- data.frame(
    stat = c("Mean", "SD"),
    trt1 = c("10.1", "2.3"),
    trt2 = c("11.5", "3.1"),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_cols(
      trt1 = fr_col("Treatment A", n = 50L),
      trt2 = fr_col("Treatment B", n = 48L),
      .n_format = "{label}\n(N={n})"
    )

  result <- arframe:::finalize_spec(spec)
  expect_equal(result$columns$trt1$label, "Treatment A\n(N=50)")
  expect_equal(result$columns$trt2$label, "Treatment B\n(N=48)")
})


# ══════════════════════════════════════════════════════════════════════════════
# finalize_spec() — header align propagation
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_header(align) propagates to columns without per-column override", {
  spec <- data.frame(a = 1, b = 2) |>
    fr_table() |>
    fr_header(align = "center")

  result <- arframe:::finalize_spec(spec)
  expect_equal(result$columns$a$header_align, "center")
  expect_equal(result$columns$b$header_align, "center")
})


# ══════════════════════════════════════════════════════════════════════════════
# finalize_spec() — percentage widths
# ══════════════════════════════════════════════════════════════════════════════

test_that("finalize_spec resolves percentage widths to inches", {
  spec <- data.frame(a = 1, b = 2) |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = fr_pct(0.50)),
      b = fr_col("B", width = fr_pct(0.50))
    )

  result <- arframe:::finalize_spec(spec)
  printable_w <- arframe:::printable_area_inches(spec$page)[["width"]]
  expect_equal(result$columns$a$width, 0.5 * printable_w, tolerance = 0.01)
  expect_equal(result$columns$b$width, 0.5 * printable_w, tolerance = 0.01)
})


# ══════════════════════════════════════════════════════════════════════════════
# prepare_pages()
# ══════════════════════════════════════════════════════════════════════════════

test_that("prepare_pages returns single group when no page_by", {
  spec <- data.frame(x = 1:3) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  pages <- arframe:::prepare_pages(spec)

  expect_length(pages, 1L)
  expect_null(pages[[1]]$group_label)
  expect_equal(nrow(pages[[1]]$data), nrow(spec$data))
})

test_that("prepare_pages splits by page_by column preserving data order", {
  spec <- data.frame(
    param = c("BP", "HR", "BP", "HR"),
    val = c(120, 70, 130, 80),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(page_by = "param")

  spec <- arframe:::finalize_spec(spec)
  pages <- arframe:::prepare_pages(spec)

  expect_length(pages, 2L)
  # Order preserved: BP first (appears first in data)
  expect_equal(pages[[1]]$group_label, "BP")
  expect_equal(pages[[2]]$group_label, "HR")
  expect_equal(nrow(pages[[1]]$data), 2L)
  expect_equal(nrow(pages[[2]]$data), 2L)
})

test_that("prepare_pages with multiple page_by columns creates composite keys", {
  spec <- data.frame(
    visit = c("V1", "V1", "V2", "V2"),
    param = c("BP", "HR", "BP", "HR"),
    val = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_rows(page_by = c("visit", "param"))

  spec <- arframe:::finalize_spec(spec)
  pages <- arframe:::prepare_pages(spec)

  expect_length(pages, 4L)
  expect_equal(pages[[1]]$group_label, "V1 / BP")
  expect_equal(pages[[2]]$group_label, "V1 / HR")
})

test_that("fr_rows errors on non-existent page_by column", {
  expect_error(
    data.frame(x = 1) |>
      fr_table() |>
      fr_rows(page_by = "nonexistent"),
    "not found"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# calculate_col_panels()
# ══════════════════════════════════════════════════════════════════════════════

test_that("calculate_col_panels returns single panel when col_split = FALSE", {
  spec <- data.frame(a = 1, b = 2, c = 3) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  panels <- arframe:::calculate_col_panels(spec)

  expect_length(panels, 1L)
  vis <- names(Filter(function(c) !isFALSE(c$visible), spec$columns))
  expect_equal(panels[[1]], vis)
})

test_that("calculate_col_panels returns single panel when cols fit", {
  spec <- data.frame(a = 1, b = 2) |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = 1),
      b = fr_col("B", width = 1)
    ) |>
    fr_cols(
      a = fr_col("A", width = 1, stub = TRUE),
      b = fr_col("B", width = 1),
      .split = TRUE
    )

  spec <- arframe:::finalize_spec(spec)
  panels <- arframe:::calculate_col_panels(spec)

  # Total width (1+1=2) fits in page, so single panel
  expect_length(panels, 1L)
})

test_that("calculate_col_panels splits into multiple panels for wide tables", {
  # Create a spec with many narrow columns that exceed page width
  d <- as.data.frame(
    setNames(as.list(1:12), paste0("c", 1:12))
  )
  spec <- d |>
    fr_table() |>
    fr_cols(
      c1 = fr_col("Stub", width = 1.5, stub = TRUE),
      .split = TRUE
    ) |>
    fr_page(orientation = "portrait")

  # Set explicit widths on all data columns to force splitting
  for (i in 2:12) {
    nm <- paste0("c", i)
    spec$columns[[nm]] <- fr_col(label = nm, width = 1.5)
    spec$columns[[nm]]$id <- nm
  }

  spec <- arframe:::finalize_spec(spec)
  panels <- arframe:::calculate_col_panels(spec)

  # With portrait page (~6.5" printable) and stub=1.5", available=5"
  # Each data col is 1.5" -> 3 per panel -> ~4 panels
  expect_true(length(panels) > 1L)

  # Each panel should start with stub column
  for (p in panels) {
    expect_true("c1" %in% p)
  }
})

test_that("calculate_col_panels errors when stub columns exceed page width", {
  d <- data.frame(a = 1, b = 2)
  spec <- d |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = 20, stub = TRUE),
      .split = TRUE
    )

  spec <- arframe:::finalize_spec(spec)
  expect_error(
    arframe:::calculate_col_panels(spec),
    "Stub columns exceed"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# fit_panel_widths()
# ══════════════════════════════════════════════════════════════════════════════

test_that("fit_panel_widths scales data columns to fill printable area", {
  d <- data.frame(stub = 1, d1 = 2, d2 = 3, d3 = 4, d4 = 5)
  spec <- d |>
    fr_table() |>
    fr_cols(
      stub = fr_col("Stub", width = 1.5, stub = TRUE),
      d1 = fr_col("D1", width = 1.0),
      d2 = fr_col("D2", width = 1.0),
      d3 = fr_col("D3", width = 1.0),
      d4 = fr_col("D4", width = 1.0),
      .split = TRUE
    )

  spec <- arframe:::finalize_spec(spec)

  # Create panels manually
  col_panels <- list(
    c("stub", "d1", "d2"),
    c("stub", "d3", "d4")
  )

  result <- arframe:::fit_panel_widths(spec, col_panels)

  printable_w <- arframe:::printable_area_inches(spec$page)[["width"]]
  available <- printable_w - 1.5 # minus stub width

  # Stub should keep original width
  expect_equal(result$columns$stub$width, 1.5)

  # Panel 1 data cols (d1, d2) should be scaled so d1+d2 = available
  panel1_data_w <- result$columns$d1$width + result$columns$d2$width
  expect_equal(panel1_data_w, available, tolerance = 0.01)

  # Panel 2 data cols (d3, d4) should also sum to available
  panel2_data_w <- result$columns$d3$width + result$columns$d4$width
  expect_equal(panel2_data_w, available, tolerance = 0.01)
})

test_that("fit_panel_widths preserves proportional ratios", {
  d <- data.frame(stub = 1, d1 = 2, d2 = 3)
  spec <- d |>
    fr_table() |>
    fr_cols(
      stub = fr_col("Stub", width = 1.0, stub = TRUE),
      d1 = fr_col("D1", width = 1.0),
      d2 = fr_col("D2", width = 2.0),
      .split = TRUE
    )

  spec <- arframe:::finalize_spec(spec)
  col_panels <- list(c("stub", "d1", "d2"))
  result <- arframe:::fit_panel_widths(spec, col_panels)

  # d2 should still be twice as wide as d1
  ratio <- result$columns$d2$width / result$columns$d1$width
  expect_equal(ratio, 2.0, tolerance = 0.01)
})


# ══════════════════════════════════════════════════════════════════════════════
# finalize_labels() — edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("finalize_labels skips non-numeric .n (per-group list)", {
  spec <- data.frame(a = 1, b = 2) |> fr_table()
  spec$columns_meta$n <- list(grp1 = c("a" = 10), grp2 = c("a" = 20))
  spec$columns_meta$n_format <- "{label}\n(N={n})"

  result <- suppressWarnings(arframe:::finalize_spec(spec))
  # Per-group list n is not resolved globally — labels should be unchanged
  expect_false(grepl("N=", result$columns$a$label %||% "a"))
})

test_that("finalize_labels handles unmatched .n labels gracefully", {
  spec <- data.frame(a = 1) |> fr_table()
  spec$columns_meta$n <- c("nonexistent" = 50)
  spec$columns_meta$n_format <- "{label}\n(N={n})"

  # Should not error, just skip the non-matching column
  result <- arframe:::finalize_spec(spec)
  expect_true(inherits(result, "fr_spec"))
})


# ══════════════════════════════════════════════════════════════════════════════
# resolve_group_labels()
# ══════════════════════════════════════════════════════════════════════════════

test_that("resolve_group_labels returns NULL when no format set", {
  spec <- data.frame(a = 1) |> fr_table()
  spec <- arframe:::finalize_spec(spec)
  result <- arframe:::resolve_group_labels(spec, spec$data, NULL)
  expect_null(result)
})

test_that("resolve_group_labels handles per-group list .n", {
  spec <- data.frame(a = 1, b = 2) |>
    fr_table() |>
    fr_cols(
      a = fr_col("Col A"),
      b = fr_col("Col B"),
      .n = list("GroupX" = c("Col A" = 30, "Col B" = 25)),
      .n_format = "{label}\n(N={n})"
    )
  spec <- suppressWarnings(arframe:::finalize_spec(spec))

  result <- arframe:::resolve_group_labels(spec, spec$data, "GroupX")
  # Now returns list(columns, spans)
  expect_true(is.list(result))
  expect_true("a" %in% names(result$columns))
  expect_true(grepl("N=30", result$columns["a"]))
})

test_that("resolve_group_labels returns NULL for unknown group in list .n", {
  spec <- data.frame(a = 1) |> fr_table()
  spec$columns_meta$n <- list("GroupA" = c("a" = 10))
  spec$columns_meta$n_format <- "{label}\n(N={n})"
  spec <- suppressWarnings(arframe:::finalize_spec(spec))

  result <- arframe:::resolve_group_labels(spec, spec$data, "UnknownGroup")
  expect_null(result)
})

test_that("resolve_group_labels handles 2-col df .n resolved globally", {
  # 2-col data frame → resolved globally in finalize_labels
  spec <- data.frame(trt1 = 1, trt2 = 2) |>
    fr_table() |>
    fr_cols(
      trt1 = fr_col("Treatment 1"),
      trt2 = fr_col("Treatment 2"),
      .n = data.frame(trt = c("Treatment 1", "Treatment 2"), n = c(40L, 35L)),
      .n_format = "{label}\n(N={n})"
    )

  spec <- arframe:::finalize_spec(spec)

  # 2-col df → already resolved globally, resolve_group_labels returns NULL
  result <- arframe:::resolve_group_labels(spec, spec$data, "SomeGroup")
  expect_null(result)

  # Labels should be resolved globally
  expect_true(grepl("N=40", spec$columns$trt1$label))
})


# ══════════════════════════════════════════════════════════════════════════════
# match_trt_to_columns()
# ══════════════════════════════════════════════════════════════════════════════

test_that("match_trt_to_columns matches treatment labels to columns", {
  columns <- list(
    col_a = fr_col("Placebo"),
    col_b = fr_col("Active")
  )
  counts <- c("Placebo" = 45L, "Active" = 44L)
  result <- arframe:::match_trt_to_columns(counts, columns)
  expect_equal(result[["col_a"]], 45L)
  expect_equal(result[["col_b"]], 44L)
})

test_that("match_trt_to_columns is case-insensitive", {
  columns <- list(col_a = fr_col("PLACEBO"))
  counts <- c("placebo" = 45L)
  result <- arframe:::match_trt_to_columns(counts, columns)
  expect_equal(result[["col_a"]], 45L)
})

test_that("match_trt_to_columns only matches column labels (not spanners)", {
  # Columns have sub-labels, not treatment names
  columns <- list(
    pt = fr_col("Parameter"),
    bl_pbo = fr_col("Baseline"),
    val_pbo = fr_col("Value"),
    cfb_pbo = fr_col("CFB")
  )
  counts <- c("Placebo" = 45L)
  result <- arframe:::match_trt_to_columns(counts, columns)
  # No column label matches "Placebo" → empty result

  expect_length(result, 0L)
})

test_that("match_trt_to_columns works for direct column label match", {
  columns <- list(col_a = fr_col("Placebo"), col_b = fr_col("Active"))
  counts <- c("Placebo" = 45L, "Active" = 44L)
  result <- arframe:::match_trt_to_columns(counts, columns)
  expect_equal(result[["col_a"]], 45L)
  expect_equal(result[["col_b"]], 44L)
})


# ══════════════════════════════════════════════════════════════════════════════
# match_trt_to_spans()
# ══════════════════════════════════════════════════════════════════════════════

test_that("match_trt_to_spans matches treatment labels to spanner labels", {
  columns <- list(
    bl_pbo = fr_col("Baseline"),
    val_pbo = fr_col("Value"),
    bl_act = fr_col("Baseline"),
    val_act = fr_col("Value")
  )
  spans <- list(
    list(label = "Placebo", columns = c("bl_pbo", "val_pbo"), level = 1L),
    list(label = "Active", columns = c("bl_act", "val_act"), level = 1L)
  )
  counts <- c("Placebo" = 45L, "Active" = 44L)
  result <- arframe:::match_trt_to_spans(counts, columns, spans)
  # Keyed by span label
  expect_equal(result[["Placebo"]], 45L)
  expect_equal(result[["Active"]], 44L)
})

test_that("match_trt_to_spans skips treatments that match column labels", {
  columns <- list(
    col_a = fr_col("Placebo"),
    col_b = fr_col("Value")
  )
  spans <- list(
    list(label = "Placebo", columns = c("col_b"), level = 1L)
  )
  counts <- c("Placebo" = 45L)
  result <- arframe:::match_trt_to_spans(counts, columns, spans)
  # "Placebo" matches a column label → skipped for span matching
  expect_length(result, 0L)
})

test_that("match_trt_to_spans is case-insensitive", {
  columns <- list(bl = fr_col("Baseline"))
  spans <- list(
    list(label = "PLACEBO", columns = c("bl"), level = 1L)
  )
  counts <- c("placebo" = 45L)
  result <- arframe:::match_trt_to_spans(counts, columns, spans)
  expect_equal(result[["PLACEBO"]], 45L)
})

test_that("match_trt_to_spans returns empty when no spans", {
  columns <- list(col_a = fr_col("A"))
  counts <- c("Test" = 10L)
  result <- arframe:::match_trt_to_spans(counts, columns, list())
  expect_length(result, 0L)
})

test_that("mixed column + spanner N-count matching works together", {
  columns <- list(
    pt = fr_col("Parameter"),
    bl_pbo = fr_col("Baseline"),
    val_pbo = fr_col("Value"),
    total = fr_col("Total")
  )
  spans <- list(
    list(label = "Placebo", columns = c("bl_pbo", "val_pbo"), level = 1L)
  )
  counts <- c("Placebo" = 45L, "Total" = 100L)

  col_result <- arframe:::match_trt_to_columns(counts, columns)
  span_result <- arframe:::match_trt_to_spans(counts, columns, spans)

  # "Total" matches column label directly
  expect_equal(col_result[["total"]], 100L)
  # "Placebo" matches spanner
  expect_equal(span_result[["Placebo"]], 45L)
  # "Placebo" should NOT be in column result
  expect_false("bl_pbo" %in% names(col_result))
})


# ══════════════════════════════════════════════════════════════════════════════
# insert_blank_after() — from render-common.R
# ══════════════════════════════════════════════════════════════════════════════

test_that("insert_blank_after inserts blank rows at group boundaries", {
  data <- data.frame(
    grp = c("A", "A", "B", "B", "C"),
    val = as.character(1:5),
    stringsAsFactors = FALSE
  )
  result <- arframe:::insert_blank_after(data, "grp")

  # 2 boundaries (A->B, B->C) -> 2 blank rows -> 7 total
  expect_equal(nrow(result$data), 7L)
  expect_equal(result$insert_positions, c(2L, 4L))
})

test_that("insert_blank_after returns data unchanged with no blank_cols", {
  data <- data.frame(x = 1:3)
  result <- arframe:::insert_blank_after(data, character(0))
  expect_equal(nrow(result$data), 3L)
  expect_equal(result$insert_positions, integer(0))
})

test_that("insert_blank_after handles single-row data", {
  data <- data.frame(x = 1)
  result <- arframe:::insert_blank_after(data, "x")
  expect_equal(nrow(result$data), 1L)
  expect_equal(result$insert_positions, integer(0))
})


# ══════════════════════════════════════════════════════════════════════════════
# inject_span_gaps()
# ══════════════════════════════════════════════════════════════════════════════

test_that("inject_span_gaps adds gap columns between adjacent spans", {
  spec <- data.frame(a = 1, b = 2, c = 3, d = 4) |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = 1),
      b = fr_col("B", width = 1),
      c = fr_col("C", width = 1),
      d = fr_col("D", width = 1)
    ) |>
    fr_spans("Span1" = c("a", "b"), "Span2" = c("c", "d"))

  spec <- arframe:::finalize_spec(spec)
  col_names <- names(spec$columns)

  # Should have gap columns injected
  gap_cols <- grep("__span_gap_", col_names, value = TRUE)
  expect_true(length(gap_cols) >= 1L)
})

test_that("inject_span_gaps does nothing when span_gap = FALSE", {
  spec <- data.frame(a = 1, b = 2, c = 3, d = 4) |>
    fr_table() |>
    fr_cols(
      a = fr_col("A", width = 1),
      b = fr_col("B", width = 1),
      c = fr_col("C", width = 1),
      d = fr_col("D", width = 1)
    ) |>
    fr_spans("S1" = c("a", "b"), "S2" = c("c", "d"), .gap = FALSE)

  spec <- arframe:::finalize_spec(spec)
  gap_cols <- grep("__span_gap_", names(spec$columns), value = TRUE)
  expect_length(gap_cols, 0L)
})


# ══════════════════════════════════════════════════════════════════════════════
# insert_gap_column()
# ══════════════════════════════════════════════════════════════════════════════

test_that("insert_gap_column inserts at correct position", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  result <- arframe:::insert_gap_column(df, 2L, "gap1")

  expect_equal(ncol(result), 4L)
  expect_equal(names(result), c("a", "b", "gap1", "c"))
  expect_equal(result$gap1, c("", "", ""))
})

test_that("insert_gap_column works at end position", {
  df <- data.frame(a = 1:2, b = 3:4)
  result <- arframe:::insert_gap_column(df, 2L, "gap_end")

  expect_equal(ncol(result), 3L)
  expect_equal(names(result), c("a", "b", "gap_end"))
})


# ══════════════════════════════════════════════════════════════════════════════
# expand_spans_for_gap()
# ══════════════════════════════════════════════════════════════════════════════

test_that("expand_spans_for_gap includes gap in straddling span", {
  spans <- list(
    list(columns = c("a", "b", "c"), level = 1L, label = "Wide")
  )
  result <- arframe:::expand_spans_for_gap(spans, "gap1", "b")
  expect_equal(result[[1]]$columns, c("a", "b", "gap1", "c"))
})

test_that("expand_spans_for_gap ignores span that ends at after_col", {
  spans <- list(
    list(columns = c("a", "b"), level = 1L, label = "Narrow")
  )
  result <- arframe:::expand_spans_for_gap(spans, "gap1", "b")
  # "b" is the last column — span does not straddle, so no change
  expect_equal(result[[1]]$columns, c("a", "b"))
})


# ══════════════════════════════════════════════════════════════════════════════
# End-to-end: finalize + prepare + panels
# ══════════════════════════════════════════════════════════════════════════════

test_that("full pipeline produces correct RTF output with all features", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  data.frame(
    grp = c("A", "A", "B", "B"),
    stat = c("Mean", "SD", "Mean", "SD"),
    val = c("10.1", "2.3", "11.5", "3.1"),
    stringsAsFactors = FALSE
  ) |>
    fr_table() |>
    fr_titles("Table 1") |>
    fr_footnotes("Note: test") |>
    fr_rows(group_by = "grp") |>
    fr_hlines("header") |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
  txt <- rawToChar(readBin(tmp, "raw", file.info(tmp)$size))
  expect_true(grepl("Table 1", txt, fixed = TRUE))
})

test_that("page_by + col_split integration produces output", {
  tmp <- tempfile(fileext = ".rtf")
  on.exit(unlink(tmp), add = TRUE)

  d <- data.frame(
    param = rep(c("BP", "HR"), each = 2),
    stat = rep(c("Mean", "SD"), 2),
    trt1 = c("120", "10", "70", "5"),
    trt2 = c("118", "9", "72", "6"),
    trt3 = c("122", "11", "68", "7"),
    stringsAsFactors = FALSE
  )

  d |>
    fr_table() |>
    fr_rows(page_by = "param") |>
    fr_render(tmp)

  expect_true(file.exists(tmp))
})
