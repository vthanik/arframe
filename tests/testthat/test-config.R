# ──────────────────────────────────────────────────────────────────────────────
# test-config.R — Tests for YAML config system
# ──────────────────────────────────────────────────────────────────────────────

test_that("fr_config loads a YAML file and stores in fr_env", {
  # Create a temp config
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(
    "page:",
    "  font_size: 10",
    "  orientation: portrait",
    "rules:",
    "  hlines: booktabs"
  ), tmp)

  fr_config_reset()
  cfg <- fr_config(tmp)

  expect_type(cfg, "list")
  expect_equal(cfg$page$font_size, 10)
  expect_equal(cfg$page$orientation, "portrait")
  expect_equal(cfg$rules$hlines, "booktabs")

  fr_config_reset()
})

test_that("fr_config_get returns loaded config", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("page:\n  font_size: 11", tmp)

  fr_config(tmp)
  cfg <- fr_config_get()
  expect_equal(cfg$page$font_size, 11)

  fr_config_reset()
})

test_that("fr_config_reset clears config", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("page:\n  font_size: 11", tmp)

  fr_config(tmp)
  fr_config_reset()

  # After reset, apply_config should be a no-op
  spec <- tbl_demog |> fr_table()
  # Default font size is 9, not 11

  expect_equal(spec$page$font_size, 9)
})

test_that("apply_config applies page settings to fr_table", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "page:",
    "  font_size: 10",
    "  orientation: portrait",
    "  paper: a4"
  ), tmp)
  fr_config(tmp)

  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$font_size, 10)
  expect_equal(spec$page$orientation, "portrait")
  expect_equal(spec$page$paper, "a4")
})

test_that("config < fr_theme < per-table verbs (precedence)", {
  fr_config_reset()
  fr_theme_reset()

  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); fr_theme_reset(); unlink(tmp) }, add = TRUE)

  # Config: font_size = 10
  writeLines("page:\n  font_size: 10", tmp)
  fr_config(tmp)

  # Theme: font_size = 11 (overrides config)
  fr_theme(font_size = 11)

  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$font_size, 11)

  # Per-table verb: font_size = 12 (overrides theme)
  spec <- spec |> fr_page(font_size = 12)
  expect_equal(spec$page$font_size, 12)
})

test_that("config applies header defaults", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "header:",
    "  align: right",
    "  valign: top",
    "  bold: false"
  ), tmp)
  fr_config(tmp)

  spec <- tbl_demog |> fr_table()
  expect_equal(spec$header$align, "right")
  expect_equal(spec$header$valign, "top")
  expect_false(spec$header$bold)
})

test_that("config applies custom tokens", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "tokens:",
    "  company: Pharma Corp",
    "  study_id: TFRM-001"
  ), tmp)
  fr_config(tmp)

  spec <- tbl_demog |> fr_table()
  expect_equal(spec$page$tokens$company, "Pharma Corp")
  expect_equal(spec$page$tokens$study_id, "TFRM-001")
})

test_that("config applies rules", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "rules:",
    "  hlines: booktabs"
  ), tmp)
  fr_config(tmp)

  spec <- tbl_demog |> fr_table()
  # Should have booktabs rules (3 rules: top, mid, bottom)
  hline_rules <- Filter(function(r) inherits(r, "fr_rule_hline"), spec$rules)
  expect_length(hline_rules, 3L)
})

test_that("merge_config deep merges lists", {
  base <- list(a = 1, b = list(x = 10, y = 20), c = 3)
  over <- list(b = list(x = 99, z = 30), d = 4)
  result <- tlframe:::merge_config(base, over)

  expect_equal(result$a, 1)
  expect_equal(result$b$x, 99)
  expect_equal(result$b$y, 20)
  expect_equal(result$b$z, 30)
  expect_equal(result$c, 3)
  expect_equal(result$d, 4)
})

test_that("find_config finds _tlframe.yml in parent directories", {
  tmp_dir <- tempfile("tlframe_test_")
  dir.create(tmp_dir)
  sub_dir <- file.path(tmp_dir, "sub", "sub2")
  dir.create(sub_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Place config in parent
  writeLines("page:\n  font_size: 7", file.path(tmp_dir, "_tlframe.yml"))

  found <- tlframe:::find_config(sub_dir)
  expect_true(file.exists(found))
  cfg <- yaml::read_yaml(found)
  expect_equal(cfg$page$font_size, 7)
})

test_that("fr_config errors on missing file", {
  expect_error(fr_config("nonexistent_path.yml"), "not found")
})


# ── Spacing ──────────────────────────────────────────────────────────────────

test_that("fr_spacing sets spacing values on spec", {
  spec <- data.frame(a = 1) |> fr_table()

  # Defaults

  expect_equal(spec$spacing$titles_after, 1L)
  expect_equal(spec$spacing$footnotes_before, 1L)
  expect_equal(spec$spacing$pagehead_after, 0L)

  # Override
  spec2 <- spec |> fr_spacing(titles_after = 0L, footnotes_before = 2L)
  expect_equal(spec2$spacing$titles_after, 0L)
  expect_equal(spec2$spacing$footnotes_before, 2L)
  expect_equal(spec2$spacing$pagehead_after, 0L)  # unchanged
})

test_that("fr_spacing rejects negative values", {
  spec <- data.frame(a = 1) |> fr_table()
  expect_error(fr_spacing(spec, titles_after = -1L), "non-negative")
})

test_that("fr_spacing rejects non-integer values", {
  spec <- data.frame(a = 1) |> fr_table()
  expect_error(fr_spacing(spec, titles_after = 1.5), "non-negative integer")
})

test_that("config applies spacing from YAML", {
  tmp <- tempfile(fileext = ".yml")
  on.exit({ unlink(tmp); fr_config_reset() }, add = TRUE)
  writeLines(c(
    "spacing:",
    "  titles_after: 2",
    "  footnotes_before: 0"
  ), tmp)

  fr_config_reset()
  fr_config(tmp)
  spec <- data.frame(a = 1) |> fr_table()

  expect_equal(spec$spacing$titles_after, 2L)
  expect_equal(spec$spacing$footnotes_before, 0L)
  expect_equal(spec$spacing$pagehead_after, 0L)  # default kept
})

test_that("fr_theme applies spacing", {
  on.exit(fr_theme_reset(), add = TRUE)
  fr_theme_reset()
  fr_theme(spacing = list(titles_after = 3L))

  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$spacing$titles_after, 3L)
  expect_equal(spec$spacing$footnotes_before, 1L)
})


test_that("apply_config warns on invalid hlines preset", {
  withr::local_options(list(cli.default_handler = identity))
  fr_config_reset()
  fr_env$config <- list(rules = list(hlines = "nonexistent_preset"))
  expect_warning(
    data.frame(a = 1) |> fr_table(),
    "rules\\.hlines"
  )
  fr_config_reset()
})


# ══════════════════════════════════════════════════════════════════════════════
# Additional coverage tests
# ══════════════════════════════════════════════════════════════════════════════

# ── find_config ──────────────────────────────────────────────────────────────

test_that("find_config falls back to package defaults when no _tlframe.yml exists", {
  # Use a temp dir with no _tlframe.yml anywhere

  tmp_dir <- tempfile("tlframe_empty_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  found <- tlframe:::find_config(tmp_dir)
  # Should resolve to the bundled package default

  expect_true(file.exists(found))
  expect_true(grepl("defaults.*_tlframe\\.yml$", found))
})

test_that("find_config finds _tlframe.yml in the starting dir itself", {
  tmp_dir <- tempfile("tlframe_here_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines("page:\n  font_size: 6", file.path(tmp_dir, "_tlframe.yml"))

  found <- tlframe:::find_config(tmp_dir)
  expect_true(file.exists(found))
  cfg <- yaml::read_yaml(found)
  expect_equal(cfg$page$font_size, 6)
})

test_that("find_config walks up multiple directory levels", {
  tmp_dir <- tempfile("tlframe_walk_")
  deep_dir <- file.path(tmp_dir, "a", "b", "c", "d")
  dir.create(deep_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines("page:\n  font_size: 5", file.path(tmp_dir, "_tlframe.yml"))

  found <- tlframe:::find_config(deep_dir)
  expect_true(file.exists(found))
  cfg <- yaml::read_yaml(found)
  expect_equal(cfg$page$font_size, 5)
})

test_that("find_config picks the closest _tlframe.yml", {
  tmp_dir <- tempfile("tlframe_closest_")
  sub_dir <- file.path(tmp_dir, "sub")
  dir.create(sub_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines("page:\n  font_size: 99", file.path(tmp_dir, "_tlframe.yml"))
  writeLines("page:\n  font_size: 42", file.path(sub_dir, "_tlframe.yml"))

  found <- tlframe:::find_config(sub_dir)
  cfg <- yaml::read_yaml(found)
  expect_equal(cfg$page$font_size, 42)
})


# ── merge_config ─────────────────────────────────────────────────────────────

test_that("merge_config returns base when override is NULL", {
  base <- list(a = 1, b = 2)
  expect_equal(tlframe:::merge_config(base, NULL), base)
})

test_that("merge_config returns override when base is NULL", {
  over <- list(x = 10)
  expect_equal(tlframe:::merge_config(NULL, over), over)
})

test_that("merge_config scalar override replaces base scalar", {
  base <- list(a = 1, b = 2)
  over <- list(a = 99)
  result <- tlframe:::merge_config(base, over)
  expect_equal(result$a, 99)
  expect_equal(result$b, 2)
})

test_that("merge_config override replaces list with scalar", {
  base <- list(a = list(x = 1, y = 2))
  over <- list(a = "flat")
  result <- tlframe:::merge_config(base, over)
  expect_equal(result$a, "flat")
})

test_that("merge_config override replaces scalar with list", {
  base <- list(a = "flat")
  over <- list(a = list(x = 1))
  result <- tlframe:::merge_config(base, over)
  expect_equal(result$a, list(x = 1))
})

test_that("merge_config deeply nested three levels", {
  base <- list(l1 = list(l2 = list(l3 = "base_val", keep = TRUE)))
  over <- list(l1 = list(l2 = list(l3 = "over_val", new = 42)))
  result <- tlframe:::merge_config(base, over)
  expect_equal(result$l1$l2$l3, "over_val")
  expect_true(result$l1$l2$keep)
  expect_equal(result$l1$l2$new, 42)
})

test_that("merge_config with both NULL returns NULL", {
  expect_null(tlframe:::merge_config(NULL, NULL))
})


# ── fr_config error paths ────────────────────────────────────────────────────

test_that("fr_config errors on non-character file argument", {
  expect_error(fr_config(123))
})

test_that("fr_config errors on non-existent path with helpful message", {
  expect_error(
    fr_config("/nonexistent/path/_tlframe.yml"),
    "not found"
  )
})

test_that("fr_config handles empty YAML file gracefully", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)

  # Empty file — yaml::read_yaml returns NULL, converted to list()
  writeLines("", tmp)
  cfg <- fr_config(tmp)
  expect_type(cfg, "list")
  expect_length(cfg, 0L)

  # apply_config should be a no-op on empty config
  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$page$font_size, 9)  # package default
})


# ── fr_config_get auto-discovery ─────────────────────────────────────────────

test_that("fr_config_get auto-loads config when none is loaded", {
  fr_config_reset()
  # fr_config_get() calls fr_config() internally when config is NULL
  cfg <- fr_config_get()
  expect_type(cfg, "list")
  # Should have loaded something (package defaults at minimum)
  fr_config_reset()
})


# ── apply_config: header section ─────────────────────────────────────────────

test_that("apply_config applies header span_gap", {
  fr_config_reset()
  fr_env$config <- list(header = list(span_gap = FALSE))
  spec <- data.frame(a = 1) |> fr_table()
  expect_false(spec$header$span_gap)
  fr_config_reset()
})

test_that("apply_config applies header n and n_format", {
  fr_config_reset()
  fr_env$config <- list(header = list(
    n = "auto",
    n_format = "{name} [N={n}]",
    n_subject = "USUBJID"
  ))
  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$header$n, "auto")
  expect_equal(spec$header$format, "{name} [N={n}]")
  expect_equal(spec$header$n_subject, "USUBJID")
  fr_config_reset()
})

test_that("apply_config skips header when header config is not a list", {

  fr_config_reset()
  fr_env$config <- list(header = "not_a_list")
  # Should not error — just skip the header section
  spec <- data.frame(a = 1) |> fr_table()
  expect_s3_class(spec, "fr_spec")
  fr_config_reset()
})


# ── apply_config: pagehead and pagefoot ──────────────────────────────────────

test_that("apply_config applies pagehead settings", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "pagehead:",
    "  left: 'Company XYZ'",
    "  center: 'Study 001'",
    "  right: 'Page {thepage}'",
    "  font_size: 7",
    "  bold: true"
  ), tmp)
  fr_config(tmp)

  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$pagehead$left, "Company XYZ")
  expect_equal(spec$pagehead$center, "Study 001")
  expect_equal(spec$pagehead$right, "Page {thepage}")
  expect_equal(spec$pagehead$font_size, 7)
  expect_true(spec$pagehead$bold)
})

test_that("apply_config applies pagefoot settings", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "pagefoot:",
    "  left: '{program}'",
    "  right: '{datetime}'",
    "  font_size: 7",
    "  bold: true"
  ), tmp)
  fr_config(tmp)

  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$pagefoot$left, "{program}")
  expect_equal(spec$pagefoot$right, "{datetime}")
  expect_equal(spec$pagefoot$font_size, 7)
  expect_true(spec$pagefoot$bold)
})


# ── apply_config: rules (vlines) ────────────────────────────────────────────

test_that("apply_config applies vlines from config", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "rules:",
    "  vlines: all"
  ), tmp)
  fr_config(tmp)

  spec <- data.frame(a = 1, b = 2) |> fr_table()
  vline_rules <- Filter(function(r) inherits(r, "fr_vline_spec"), spec$rules)
  expect_true(length(vline_rules) > 0L)
})

test_that("apply_config skips vlines when void", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "rules:",
    "  vlines: void"
  ), tmp)
  fr_config(tmp)

  spec <- data.frame(a = 1, b = 2) |> fr_table()
  vline_rules <- Filter(function(r) inherits(r, "fr_vline_spec"), spec$rules)
  expect_length(vline_rules, 0L)
})

test_that("apply_config warns on invalid vlines preset", {
  fr_config_reset()
  fr_env$config <- list(rules = list(vlines = "nonexistent_vline"))
  expect_warning(
    data.frame(a = 1) |> fr_table(),
    "rules\\.vlines"
  )
  fr_config_reset()
})


# ── apply_config: footnotes ──────────────────────────────────────────────────

test_that("apply_config applies footnotes separator", {
  fr_config_reset()
  fr_env$config <- list(footnotes = list(separator = TRUE))
  spec <- data.frame(a = 1) |> fr_table()
  expect_true(spec$meta$footnote_separator)
  fr_config_reset()
})

test_that("apply_config applies footnotes placement", {
  fr_config_reset()
  fr_env$config <- list(footnotes = list(placement = "last"))
  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$meta$footnote_placement, "last")
  fr_config_reset()
})

test_that("apply_config skips footnotes when not a list", {
  fr_config_reset()
  fr_env$config <- list(footnotes = "not_a_list")
  spec <- data.frame(a = 1) |> fr_table()
  expect_s3_class(spec, "fr_spec")
  fr_config_reset()
})


# ── apply_config: titles ─────────────────────────────────────────────────────

test_that("apply_config applies titles config", {
  fr_config_reset()
  fr_env$config <- list(titles = list(
    align = "left",
    bold = TRUE,
    font_size = 12
  ))
  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$meta$title_align, "left")
  expect_true(spec$meta$title_bold)
  expect_equal(spec$meta$title_font_size, 12)
  fr_config_reset()
})

test_that("apply_config skips titles when not a list", {
  fr_config_reset()
  fr_env$config <- list(titles = "not_a_list")
  spec <- data.frame(a = 1) |> fr_table()
  expect_s3_class(spec, "fr_spec")
  fr_config_reset()
})


# ── apply_config: tokens ─────────────────────────────────────────────────────

test_that("apply_config skips tokens when empty", {
  fr_config_reset()
  fr_env$config <- list(tokens = list())
  spec <- data.frame(a = 1) |> fr_table()
  # No error, tokens should be unchanged from defaults
  expect_s3_class(spec, "fr_spec")
  fr_config_reset()
})

test_that("apply_config tokens are low priority (existing tokens win)", {
  fr_config_reset()
  fr_env$config <- list(tokens = list(company = "Config Corp"))
  spec <- data.frame(a = 1) |> fr_table()

  # Now set a token manually via pagehead (higher priority)
  spec$page$tokens$company <- "Manual Corp"
  expect_equal(spec$page$tokens$company, "Manual Corp")
  fr_config_reset()
})


# ── apply_config: page section ───────────────────────────────────────────────

test_that("apply_config applies split and continuation from config", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "columns:",
    "  split: true",
    "page:",
    "  continuation: '(continued)'"
  ), tmp)
  fr_config(tmp)

  spec <- data.frame(a = 1) |> fr_table()
  expect_true(isTRUE(spec$columns_meta$split))
  expect_equal(spec$page$continuation, "(continued)")
})

test_that("apply_config applies font_family from config", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "page:",
    "  font_family: 'Times New Roman'"
  ), tmp)
  fr_config(tmp)

  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$page$font_family, "Times New Roman")
})

test_that("apply_config applies margins from config", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "page:",
    "  margins: [1.5, 1.0, 1.5, 1.0]"
  ), tmp)
  fr_config(tmp)

  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$page$margins, list(top = 1.5, right = 1.0, bottom = 1.5, left = 1.0))
})


# ── apply_config: empty/null config ──────────────────────────────────────────

test_that("apply_config is a no-op when config is NULL", {
  fr_config_reset()
  # config is NULL after reset; fr_table should use defaults

  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$page$font_size, 9)
  expect_equal(spec$page$orientation, "landscape")
})

test_that("apply_config is a no-op when config is empty list", {
  fr_config_reset()
  fr_env$config <- list()
  spec <- data.frame(a = 1) |> fr_table()
  expect_equal(spec$page$font_size, 9)
  fr_config_reset()
})


# ── apply_config: full roundtrip from YAML ───────────────────────────────────

test_that("apply_config roundtrip: comprehensive YAML config", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines(c(
    "page:",
    "  font_size: 8",
    "  orientation: portrait",
    "  paper: a4",
    "  font_family: 'Times New Roman'",
    "header:",
    "  align: center",
    "  valign: top",
    "  bold: true",
    "pagehead:",
    "  left: 'Pharma Co'",
    "  right: 'Page {thepage}'",
    "pagefoot:",
    "  left: '{program}'",
    "rules:",
    "  hlines: booktabs",
    "  vlines: all",
    "footnotes:",
    "  separator: true",
    "  placement: last",
    "spacing:",
    "  titles_after: 2",
    "  footnotes_before: 0",
    "titles:",
    "  align: left",
    "  bold: true",
    "tokens:",
    "  company: 'Test Corp'"
  ), tmp)
  fr_config(tmp)

  spec <- data.frame(a = 1, b = 2) |> fr_table()

  # Page

  expect_equal(spec$page$font_size, 8)
  expect_equal(spec$page$orientation, "portrait")
  expect_equal(spec$page$paper, "a4")
  expect_equal(spec$page$font_family, "Times New Roman")

  # Header

  expect_equal(spec$header$align, "center")
  expect_equal(spec$header$valign, "top")
  expect_true(spec$header$bold)

  # Pagehead

  expect_equal(spec$pagehead$left, "Pharma Co")
  expect_equal(spec$pagehead$right, "Page {thepage}")

  # Pagefoot
  expect_equal(spec$pagefoot$left, "{program}")

  # Rules
  hlines <- Filter(function(r) inherits(r, "fr_rule_hline"), spec$rules)
  expect_true(length(hlines) >= 3L)  # booktabs = 3 rules
  vlines <- Filter(function(r) inherits(r, "fr_vline_spec"), spec$rules)
  expect_true(length(vlines) > 0L)

  # Footnotes
  expect_true(spec$meta$footnote_separator)
  expect_equal(spec$meta$footnote_placement, "last")

  # Spacing
  expect_equal(spec$spacing$titles_after, 2L)
  expect_equal(spec$spacing$footnotes_before, 0L)

  # Titles
  expect_equal(spec$meta$title_align, "left")
  expect_true(spec$meta$title_bold)

  # Tokens
  expect_equal(spec$page$tokens$company, "Test Corp")
})


# ── fr_config_reset restores fr_env fields ───────────────────────────────────

test_that("fr_config_reset clears both config and config_file", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("page:\n  font_size: 7", tmp)

  fr_config(tmp)
  expect_false(is.null(fr_env$config))
  expect_false(is.null(fr_env$config_file))

  fr_config_reset()
  expect_null(fr_env$config)
  expect_null(fr_env$config_file)
})

test_that("fr_config stores the file path in fr_env$config_file", {
  fr_config_reset()
  tmp <- tempfile(fileext = ".yml")
  on.exit({ fr_config_reset(); unlink(tmp) }, add = TRUE)
  writeLines("page:\n  font_size: 7", tmp)

  fr_config(tmp)
  expect_true(grepl("\\.yml$", fr_env$config_file))
  fr_config_reset()
})
