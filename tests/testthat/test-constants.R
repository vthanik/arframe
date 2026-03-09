# ──────────────────────────────────────────────────────────────────────────────
# test-constants.R — Tests for constants.R
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# fr_env existence and structure
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_env exists and is an environment", {
  expect_true(is.environment(fr_env))
})

test_that("fr_env contains all expected members", {
  expected <- c(
    "fonts", "paper", "baseline_ratio",
    "linestyle_rtf", "valid_linestyles", "cell_border_rtf", "para_border_rtf",
    "hline_presets", "line_widths",
    "latex_specials", "rtf_specials", "latex_unicode", "rtf_unicode",
    "named_colors", "builtin_tokens",
    "valid_aligns", "align_to_rtf", "align_to_latex",
    "sentinel_start", "sentinel_end", "sentinel_pattern"
  )
  for (member in expected) {
    expect_true(exists(member, envir = fr_env),
                info = paste0("fr_env$", member, " should exist"))
  }
})


# ══════════════════════════════════════════════════════════════════════════════
# Font tables
# ══════════════════════════════════════════════════════════════════════════════

test_that("os_default_fonts returns a list with mono, sans, serif", {
  fonts <- os_default_fonts()
  expect_type(fonts, "list")
  expect_true("mono" %in% names(fonts))
  expect_true("sans" %in% names(fonts))
  expect_true("serif" %in% names(fonts))
  expect_true(nzchar(fonts$mono))
})

test_that("lookup_font_family resolves known fonts to family keys", {
  # Returns fr_env$fonts keys: "modern", "swiss", "roman"
  expect_equal(lookup_font_family("Courier New"), "modern")
  expect_equal(lookup_font_family("Arial"), "swiss")
  expect_equal(lookup_font_family("Times New Roman"), "roman")
})

test_that("lookup_font_family falls back to modern for unknown fonts", {
  expect_equal(lookup_font_family("NonExistentFont"), "modern")
})

test_that("resolve_afm_name resolves font families to AFM names", {
  expect_equal(resolve_afm_name("modern"), "Courier")
  expect_equal(resolve_afm_name("swiss"), "Helvetica")
  expect_equal(resolve_afm_name("roman"), "Times-Roman")
  expect_equal(resolve_afm_name("modern", bold = TRUE), "Courier-Bold")
  expect_equal(resolve_afm_name("swiss", italic = TRUE), "Helvetica-Oblique")
  expect_equal(resolve_afm_name("roman", bold = TRUE, italic = TRUE), "Times-BoldItalic")
})

test_that("resolve_afm_name resolves font names to AFM names", {
  expect_equal(resolve_afm_name("Courier New"), "Courier")
  expect_equal(resolve_afm_name("Arial"), "Helvetica")
  expect_equal(resolve_afm_name("Times New Roman"), "Times-Roman")
  expect_equal(resolve_afm_name("Arial", bold = TRUE), "Helvetica-Bold")
})

test_that("measure_text_width_twips returns positive values", {
  w <- measure_text_width_twips("Hello", "Helvetica", 10)
  expect_true(is.numeric(w))
  expect_gt(w, 0)
})

test_that("measure_text_width_twips scales with font size", {
  w9 <- measure_text_width_twips("Hello", "Helvetica", 9)
  w12 <- measure_text_width_twips("Hello", "Helvetica", 12)
  expect_gt(w12, w9)
})

test_that("measure_text_width_twips differentiates narrow vs wide chars", {
  w_narrow <- measure_text_width_twips("iiii", "Helvetica", 10)
  w_wide   <- measure_text_width_twips("MMMM", "Helvetica", 10)
  expect_gt(w_wide, w_narrow * 2)  # M is ~3.75x wider than i
})

test_that("measure_text_width_twips is consistent for monospace", {
  w_narrow <- measure_text_width_twips("iiii", "Courier", 10)
  w_wide   <- measure_text_width_twips("MMMM", "Courier", 10)
  expect_equal(w_narrow, w_wide)  # Courier is monospaced
})

test_that("measure_text_width_twips handles empty and NA", {
  expect_equal(measure_text_width_twips("", "Helvetica", 10), 0)
  expect_equal(measure_text_width_twips(NA_character_, "Helvetica", 10), 0)
})

test_that("measure_text_width_twips is vectorized", {
  w <- measure_text_width_twips(c("a", "bb", "ccc"), "Helvetica", 10)
  expect_length(w, 3)
  expect_true(all(w > 0))
  # Longer strings should be wider
  expect_lt(w[1], w[2])
  expect_lt(w[2], w[3])
})

test_that("get_rtf_font_family returns valid RTF family", {
  expect_equal(get_rtf_font_family("Courier New"), "fmodern")
  expect_equal(get_rtf_font_family("Arial"), "fswiss")
  expect_equal(get_rtf_font_family("Times New Roman"), "froman")
})

test_that("get_rtf_font_prq returns valid pitch", {
  expect_equal(get_rtf_font_prq("Courier New"), 1L)
  expect_equal(get_rtf_font_prq("Arial"), 2L)
})

test_that("get_tex_font_cmd returns valid LaTeX font command", {
  expect_match(get_tex_font_cmd("Courier New"), "ttfamily")
  expect_match(get_tex_font_cmd("Arial"), "sffamily")
  expect_match(get_tex_font_cmd("Times New Roman"), "rmfamily")
})


# ══════════════════════════════════════════════════════════════════════════════
# Paper sizes
# ══════════════════════════════════════════════════════════════════════════════

test_that("paper_dims_twips returns named integer vector", {
  dims <- paper_dims_twips("letter", "landscape")
  expect_type(dims, "integer")
  expect_named(dims, c("width", "height"))
  expect_gt(dims[["width"]], 0L)
  expect_gt(dims[["height"]], 0L)
})

test_that("landscape swaps width and height", {
  portrait <- paper_dims_twips("letter", "portrait")
  landscape <- paper_dims_twips("letter", "landscape")
  expect_equal(portrait[["width"]], landscape[["height"]])
  expect_equal(portrait[["height"]], landscape[["width"]])
})

test_that("a4 and legal paper sizes work", {
  a4 <- paper_dims_twips("a4", "portrait")
  legal <- paper_dims_twips("legal", "portrait")
  expect_gt(a4[["width"]], 0L)
  expect_gt(legal[["height"]], a4[["height"]])
})


# ══════════════════════════════════════════════════════════════════════════════
# Unit conversions
# ══════════════════════════════════════════════════════════════════════════════

test_that("inches_to_twips and twips_to_inches are inverses", {
  expect_equal(twips_to_inches(inches_to_twips(1)), 1)
  expect_equal(twips_to_inches(inches_to_twips(8.5)), 8.5)
})

test_that("inches_to_twips returns integer", {
  expect_true(is.integer(inches_to_twips(1)))
  expect_equal(inches_to_twips(1), 1440L)
})

test_that("pt_to_half_pt doubles and rounds", {
  expect_equal(pt_to_half_pt(9), 18L)
  expect_equal(pt_to_half_pt(10.5), 21L)
  expect_true(is.integer(pt_to_half_pt(9)))
})

test_that("pt_to_twips converts correctly (1pt = 20twips)", {
  expect_equal(pt_to_twips(1), 20L)
  expect_equal(pt_to_twips(12), 240L)
  expect_true(is.integer(pt_to_twips(1)))
})


# ══════════════════════════════════════════════════════════════════════════════
# Row height calculation
# ══════════════════════════════════════════════════════════════════════════════

test_that("row_height_twips returns positive integer", {
  h <- row_height_twips(9)
  expect_true(is.integer(h))
  expect_gt(h, 0L)
})

test_that("row_height_twips scales with font size", {
  h9 <- row_height_twips(9)
  h12 <- row_height_twips(12)
  expect_gt(h12, h9)
})

test_that("row_height_twips respects array_stretch", {
  h1 <- row_height_twips(9, array_stretch = 1.0)
  h15 <- row_height_twips(9, array_stretch = 1.5)
  expect_gt(h15, h1)
})

test_that("row_height_twips_exact returns negative value", {
  h <- row_height_twips_exact(9)
  expect_lt(h, 0L)
  expect_equal(h, -row_height_twips(9))
})

test_that("baseline_skip_twips uses 1.2 ratio", {
  bs <- baseline_skip_twips(10)
  expected <- pt_to_twips(1.2 * 10)
  expect_equal(bs, expected)
})


# ══════════════════════════════════════════════════════════════════════════════
# Line styles and hline presets
# ══════════════════════════════════════════════════════════════════════════════

test_that("valid_linestyles contains all required values", {
  expect_true(all(c("solid", "dashed", "dotted", "double", "dashdot") %in%
                    fr_env$valid_linestyles))
})

test_that("linestyle_rtf maps all valid linestyles", {
  for (style in fr_env$valid_linestyles) {
    expect_true(style %in% names(fr_env$linestyle_rtf),
                info = paste0("linestyle_rtf should contain '", style, "'"))
  }
})

test_that("linestyle_rtf maps dashdot to brdrdashd", {
  expect_equal(fr_env$linestyle_rtf[["dashdot"]], "\\brdrdashd")
})

test_that("hline_presets contain all required preset names", {
  expected_presets <- c("header", "open", "void", "above", "below",
                        "hsides", "box", "booktabs")
  for (preset in expected_presets) {
    expect_true(preset %in% names(fr_env$hline_presets),
                info = paste0("hline_presets should contain '", preset, "'"))
  }
})

test_that("hline_presets$header has 1 rule (below header)", {
  p <- fr_env$hline_presets$header
  expect_length(p, 1L)
  expect_equal(p[[1]]$region, "header")
  expect_equal(p[[1]]$side,   "below")
})

test_that("hline_presets$open has 2 rules (above + below header)", {
  p <- fr_env$hline_presets$open
  expect_length(p, 2L)
  expect_equal(p[[1]]$side, "above")
  expect_equal(p[[2]]$side, "below")
})

test_that("hline_presets$void is an empty list", {
  expect_length(fr_env$hline_presets$void, 0)
})

test_that("hline_presets$booktabs has 3 rules (top, mid, bottom)", {
  expect_length(fr_env$hline_presets$booktabs, 3)
})

test_that("line_widths contains named sizes as positive numbers", {
  expect_named(fr_env$line_widths, c("hairline", "thin", "medium", "thick"))
  expect_true(all(fr_env$line_widths > 0))
  expect_equal(fr_env$line_widths[["thin"]],     0.50)
  expect_equal(fr_env$line_widths[["hairline"]], 0.25)
  expect_equal(fr_env$line_widths[["medium"]],   1.00)
  expect_equal(fr_env$line_widths[["thick"]],    1.50)
})

test_that("resolve_line_width returns 0.5 for NULL", {
  expect_equal(resolve_line_width(NULL), 0.5)
})

test_that("resolve_line_width resolves named shorthands", {
  expect_equal(resolve_line_width("thin"),     0.50)
  expect_equal(resolve_line_width("hairline"), 0.25)
  expect_equal(resolve_line_width("medium"),   1.00)
  expect_equal(resolve_line_width("thick"),    1.50)
})

test_that("resolve_line_width passes through positive numeric", {
  expect_equal(resolve_line_width(0.75), 0.75)
  expect_equal(resolve_line_width(2.0),  2.0)
})

test_that("resolve_line_width errors on bad name", {
  expect_error(resolve_line_width("very_thick"), class = "rlang_error")
})

test_that("resolve_line_width errors on non-positive numeric", {
  expect_error(resolve_line_width(0),    class = "rlang_error")
  expect_error(resolve_line_width(-1),   class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# Special character maps
# ══════════════════════════════════════════════════════════════════════════════

test_that("latex_specials maps all dangerous characters", {
  dangerous <- c("\\", "{", "}", "&", "%", "$", "#", "_", "~", "^")
  for (ch in dangerous) {
    expect_true(ch %in% names(fr_env$latex_specials),
                info = paste0("latex_specials should map '", ch, "'"))
  }
})

test_that("rtf_specials maps backslash, braces", {
  expect_true("\\" %in% names(fr_env$rtf_specials))
  expect_true("{" %in% names(fr_env$rtf_specials))
  expect_true("}" %in% names(fr_env$rtf_specials))
})

test_that("latex_unicode maps common symbols", {
  expect_true(length(fr_env$latex_unicode) > 0)
  expect_true("\u2020" %in% names(fr_env$latex_unicode))  # dagger
})

test_that("rtf_unicode maps common symbols", {
  expect_true(length(fr_env$rtf_unicode) > 0)
})


# ══════════════════════════════════════════════════════════════════════════════
# Color utilities
# ══════════════════════════════════════════════════════════════════════════════

test_that("hex_to_rgb parses hex colors correctly", {
  rgb <- hex_to_rgb("#FF0000")
  expect_equal(rgb, c(r = 255L, g = 0L, b = 0L))

  rgb2 <- hex_to_rgb("#003366")
  expect_equal(rgb2, c(r = 0L, g = 51L, b = 102L))
})

test_that("hex_to_rgb handles lowercase and uppercase", {
  expect_equal(hex_to_rgb("#ff0000"), hex_to_rgb("#FF0000"))
})

test_that("rgb_to_rtf_color formats correctly", {
  rtf <- rgb_to_rtf_color(255, 0, 0)
  expect_match(rtf, "\\\\red255")
  expect_match(rtf, "\\\\green0")
  expect_match(rtf, "\\\\blue0")
})

test_that("hex_to_latex_color formats correctly", {
  tex <- hex_to_latex_color("#FF0000")
  expect_true(is.character(tex))
  expect_true(nzchar(tex))
})

test_that("resolve_color returns hex strings as-is", {
  expect_equal(resolve_color("#000000"), "#000000")
  expect_equal(resolve_color("#FFFFFF"), "#FFFFFF")
  # resolve_color normalizes hex to uppercase
  expect_equal(resolve_color("#ff0000"), "#FF0000")
})

test_that("resolve_color handles named colors", {
  result <- resolve_color("black")
  expect_true(is.character(result))
  expect_match(result, "^#")
})

test_that("resolve_color resolves CSS named colors", {
  expect_equal(resolve_color("steelblue"), "#4682B4")
  expect_equal(resolve_color("tomato"), "#FF6347")
  expect_equal(resolve_color("cornflowerblue"), "#6495ED")
  expect_equal(resolve_color("lightgray"), "#D3D3D3")
  expect_equal(resolve_color("SteelBlue"), "#4682B4")  # case-insensitive
})

test_that("resolve_color errors on invalid color with suggestions", {
  expect_error(resolve_color("notacolor"))
  expect_error(resolve_color("steelblu"), "Did you mean")
})


# ══════════════════════════════════════════════════════════════════════════════
# Token resolution
# ══════════════════════════════════════════════════════════════════════════════

test_that("resolve_tokens replaces known tokens", {
  token_map <- list(thepage = "1", total_pages = "5")
  result <- resolve_tokens(
    c("Page {thepage} of {total_pages}"),
    token_map
  )
  expect_equal(result, "Page 1 of 5")
})

test_that("resolve_tokens handles escaped braces", {
  token_map <- list(thepage = "1")
  result <- resolve_tokens("C:{{Users}}{{report}}", token_map)
  expect_equal(result, "C:{Users}{report}")
})

test_that("resolve_tokens skips fr_ patterns", {
  token_map <- list(thepage = "1")
  result <- resolve_tokens("{fr_super(1)} Page {thepage}", token_map)
  expect_match(result, "\\{fr_super\\(1\\)")
  expect_match(result, "Page 1")
})

test_that("resolve_tokens errors on unknown tokens", {
  token_map <- list(thepage = "1")
  expect_error(
    resolve_tokens("{unknown_token}", token_map),
    "unknown_token"
  )
})

test_that("resolve_tokens handles plain text without tokens", {
  token_map <- list(thepage = "1")
  result <- resolve_tokens("Plain text here", token_map)
  expect_equal(result, "Plain text here")
})

test_that("resolve_tokens handles custom tokens", {
  token_map <- list(study = "ABC-001", pop = "ITT")
  result <- resolve_tokens("{study} - {pop}", token_map)
  expect_equal(result, "ABC-001 - ITT")
})


# ══════════════════════════════════════════════════════════════════════════════
# Alignment maps
# ══════════════════════════════════════════════════════════════════════════════

test_that("valid_aligns contains all expected values", {
  expect_equal(fr_env$valid_aligns, c("left", "center", "right", "decimal"))
})

test_that("align_to_rtf maps all valid alignments", {
  for (a in fr_env$valid_aligns) {
    expect_true(a %in% names(fr_env$align_to_rtf))
  }
})

test_that("align_to_latex maps all valid alignments", {
  for (a in fr_env$valid_aligns) {
    expect_true(a %in% names(fr_env$align_to_latex))
  }
})


# ══════════════════════════════════════════════════════════════════════════════
# Validation helpers
# ══════════════════════════════════════════════════════════════════════════════

test_that("match_arg_fr returns valid match", {
  expect_equal(match_arg_fr("left", c("left", "right")), "left")
  expect_equal(match_arg_fr("RIGHT", c("left", "right")), "right")
})

test_that("match_arg_fr errors on invalid value", {
  expect_error(match_arg_fr("up", c("left", "right")), class = "rlang_error")
})

test_that("match_arg_fr returns first choice for NULL", {
  expect_equal(match_arg_fr(NULL, c("left", "right")), "left")
})

test_that("check_scalar_chr accepts valid strings", {
  expect_invisible(check_scalar_chr("hello"))
  expect_invisible(check_scalar_chr(""))
})

test_that("check_scalar_chr rejects non-strings", {
  expect_error(check_scalar_chr(123), class = "rlang_error")
  expect_error(check_scalar_chr(c("a", "b")), class = "rlang_error")
  expect_error(check_scalar_chr(NULL), class = "rlang_error")
})

test_that("check_scalar_chr allow_null works", {
  expect_invisible(check_scalar_chr(NULL, allow_null = TRUE))
})

test_that("check_positive_num accepts valid numbers", {
  expect_invisible(check_positive_num(1))
  expect_invisible(check_positive_num(0.5))
  expect_invisible(check_positive_num(100))
})

test_that("check_positive_num rejects invalid values", {
  expect_error(check_positive_num(0), class = "rlang_error")
  expect_error(check_positive_num(-1), class = "rlang_error")
  expect_error(check_positive_num("a"), class = "rlang_error")
  expect_error(check_positive_num(NA_real_), class = "rlang_error")
  expect_error(check_positive_num(c(1, 2)), class = "rlang_error")
})

test_that("check_scalar_lgl accepts TRUE/FALSE", {
  expect_invisible(check_scalar_lgl(TRUE))
  expect_invisible(check_scalar_lgl(FALSE))
})

test_that("check_scalar_lgl rejects non-logical", {
  expect_error(check_scalar_lgl(1), class = "rlang_error")
  expect_error(check_scalar_lgl("TRUE"), class = "rlang_error")
  expect_error(check_scalar_lgl(NA), class = "rlang_error")
})


# ══════════════════════════════════════════════════════════════════════════════
# Glue-markup detection and sentinel system
# ══════════════════════════════════════════════════════════════════════════════

test_that("has_fr_markup detects {fr_*} patterns", {
  expect_true(has_fr_markup("{fr_super(1)} test"))
  expect_true(has_fr_markup("BMI kg/m{fr_super(2)}"))
  expect_true(has_fr_markup("{fr_bold('Table')}"))
})

test_that("has_fr_markup returns FALSE for plain strings", {
  expect_false(has_fr_markup("plain text"))
  expect_false(has_fr_markup(""))
  expect_false(has_fr_markup("{notfr_super(1)}"))
  expect_false(has_fr_markup("Page {thepage}"))
})

test_that("has_fr_markup returns FALSE for non-character input", {
  expect_false(has_fr_markup(123))
  expect_false(has_fr_markup(NULL))
  expect_false(has_fr_markup(c("a", "b")))
})

test_that("markup_sentinel produces correct format", {
  s <- markup_sentinel("SUPER", "1")
  expect_match(s, "SUPER")
  expect_match(s, "1")
  expect_equal(substr(s, 1, 1), "\x01")
  expect_equal(substr(s, nchar(s), nchar(s)), "\x02")
})

test_that("markup_sentinel roundtrips via sentinel_pattern", {
  s <- markup_sentinel("BOLD", "hello")
  m <- regexec(fr_env$sentinel_pattern, s, perl = TRUE)
  parts <- regmatches(s, m)[[1]]
  expect_length(parts, 3)
  expect_equal(parts[[2]], "BOLD")
  expect_equal(parts[[3]], "hello")
})


# ══════════════════════════════════════════════════════════════════════════════
# Liberation font fallback
# ══════════════════════════════════════════════════════════════════════════════

test_that("liberation_fallback maps all font family types", {
  expect_equal(fr_env$liberation_fallback[["modern"]], "Liberation Mono")
  expect_equal(fr_env$liberation_fallback[["swiss"]],  "Liberation Sans")
  expect_equal(fr_env$liberation_fallback[["roman"]],  "Liberation Serif")
})

test_that("liberation_file_prefix maps all Liberation fonts", {
  expect_equal(fr_env$liberation_file_prefix[["Liberation Mono"]],  "LiberationMono")
  expect_equal(fr_env$liberation_file_prefix[["Liberation Sans"]],  "LiberationSans")
  expect_equal(fr_env$liberation_file_prefix[["Liberation Serif"]], "LiberationSerif")
})

test_that("bundled Liberation font files exist", {
  font_dir <- system.file("fonts", "liberation", package = "tlframe")
  expect_true(nzchar(font_dir))

  expected_files <- c(
    "LiberationMono-Regular.ttf", "LiberationMono-Bold.ttf",
    "LiberationMono-Italic.ttf", "LiberationMono-BoldItalic.ttf",
    "LiberationSans-Regular.ttf", "LiberationSans-Bold.ttf",
    "LiberationSans-Italic.ttf", "LiberationSans-BoldItalic.ttf",
    "LiberationSerif-Regular.ttf", "LiberationSerif-Bold.ttf",
    "LiberationSerif-Italic.ttf", "LiberationSerif-BoldItalic.ttf",
    "SIL-OFL-1.1.txt"
  )
  for (f in expected_files) {
    expect_true(file.exists(file.path(font_dir, f)),
                info = paste0("Missing: ", f))
  }
})

test_that("is_system_font_available returns TRUE for Liberation fonts", {
  expect_true(is_system_font_available("Liberation Mono"))
  expect_true(is_system_font_available("Liberation Sans"))
  expect_true(is_system_font_available("Liberation Serif"))
})

test_that("resolve_latex_font returns system font when available", {
  # Liberation fonts are always available (bundled)
  result <- resolve_latex_font("Liberation Mono")
  expect_equal(result$name, "Liberation Mono")
  expect_null(result$path)
})

test_that("resolve_latex_font falls back for unavailable fonts", {
  # Mock is_system_font_available to return FALSE
  local_mocked_bindings(is_system_font_available = function(font_name) FALSE)

  result <- resolve_latex_font("Courier New")
  expect_equal(result$name, "Liberation Mono")
  expect_true(nzchar(result$path))
  expect_true(dir.exists(result$path))
})

test_that("resolve_latex_font maps families correctly in fallback", {
  local_mocked_bindings(is_system_font_available = function(font_name) FALSE)

  mono  <- resolve_latex_font("Courier New")
  sans  <- resolve_latex_font("Arial")
  serif <- resolve_latex_font("Times New Roman")

  expect_equal(mono$name,  "Liberation Mono")
  expect_equal(sans$name,  "Liberation Sans")
  expect_equal(serif$name, "Liberation Serif")
})


test_that("resolve_color normalizes hex to uppercase", {
  expect_equal(resolve_color("#aabbcc"), "#AABBCC")
  expect_equal(resolve_color("#FF0000"), "#FF0000")
  expect_equal(resolve_color("#abcdef"), "#ABCDEF")
})


# ══════════════════════════════════════════════════════════════════════════════
# Additional coverage tests
# ══════════════════════════════════════════════════════════════════════════════

# ── resolve_afm_name: all family + style combinations ─────────────────────

test_that("resolve_afm_name covers all bold/italic combos for modern", {
  expect_equal(resolve_afm_name("modern"),                              "Courier")
  expect_equal(resolve_afm_name("modern", bold = TRUE),                 "Courier-Bold")
  expect_equal(resolve_afm_name("modern", italic = TRUE),               "Courier-Oblique")
  expect_equal(resolve_afm_name("modern", bold = TRUE, italic = TRUE),  "Courier-BoldOblique")
})

test_that("resolve_afm_name covers all bold/italic combos for swiss", {
  expect_equal(resolve_afm_name("swiss"),                              "Helvetica")
  expect_equal(resolve_afm_name("swiss", bold = TRUE),                 "Helvetica-Bold")
  expect_equal(resolve_afm_name("swiss", italic = TRUE),               "Helvetica-Oblique")
  expect_equal(resolve_afm_name("swiss", bold = TRUE, italic = TRUE),  "Helvetica-BoldOblique")
})

test_that("resolve_afm_name covers all bold/italic combos for roman", {
  expect_equal(resolve_afm_name("roman"),                              "Times-Roman")
  expect_equal(resolve_afm_name("roman", bold = TRUE),                 "Times-Bold")
  expect_equal(resolve_afm_name("roman", italic = TRUE),               "Times-Italic")
  expect_equal(resolve_afm_name("roman", bold = TRUE, italic = TRUE),  "Times-BoldItalic")
})

test_that("resolve_afm_name falls back to modern for unknown font names", {
  expect_equal(resolve_afm_name("UnknownFont"), "Courier")
  expect_equal(resolve_afm_name("UnknownFont", bold = TRUE), "Courier-Bold")
  expect_equal(resolve_afm_name("UnknownFont", italic = TRUE), "Courier-Oblique")
  expect_equal(resolve_afm_name("UnknownFont", bold = TRUE, italic = TRUE), "Courier-BoldOblique")
})

test_that("resolve_afm_name resolves all font names in each family", {
  # Modern family fonts
  for (fn in c("Courier", "Consolas", "Lucida Console", "DejaVu Sans Mono", "Liberation Mono")) {
    expect_equal(resolve_afm_name(fn), "Courier", info = paste0("Font: ", fn))
  }
  # Swiss family fonts
  for (fn in c("Helvetica", "Calibri", "Verdana", "Tahoma", "Segoe UI", "DejaVu Sans", "Liberation Sans")) {
    expect_equal(resolve_afm_name(fn), "Helvetica", info = paste0("Font: ", fn))
  }
  # Roman family fonts
  for (fn in c("Times", "Georgia", "Palatino", "Book Antiqua", "Cambria", "DejaVu Serif", "Liberation Serif")) {
    expect_equal(resolve_afm_name(fn), "Times-Roman", info = paste0("Font: ", fn))
  }
})


# ── get_rtf_font_family and get_rtf_font_prq: all families ───────────────

test_that("get_rtf_font_family returns correct family for all font names", {
  # Modern fonts
  for (fn in c("Courier New", "Courier", "Consolas", "Lucida Console")) {
    expect_equal(get_rtf_font_family(fn), "fmodern", info = paste0("Font: ", fn))
  }
  # Swiss fonts
  for (fn in c("Arial", "Helvetica", "Calibri", "Verdana", "Tahoma")) {
    expect_equal(get_rtf_font_family(fn), "fswiss", info = paste0("Font: ", fn))
  }
  # Roman fonts
  for (fn in c("Times New Roman", "Times", "Georgia", "Palatino")) {
    expect_equal(get_rtf_font_family(fn), "froman", info = paste0("Font: ", fn))
  }
})

test_that("get_rtf_font_family falls back to fmodern for unknown fonts", {
  expect_equal(get_rtf_font_family("NonExistentFont"), "fmodern")
})

test_that("get_rtf_font_prq returns correct pitch for all families", {
  expect_equal(get_rtf_font_prq("Courier New"), 1L)      # modern = fixed
  expect_equal(get_rtf_font_prq("Times New Roman"), 2L)   # roman = variable
  expect_equal(get_rtf_font_prq("Arial"), 2L)             # swiss = variable
})

test_that("get_rtf_font_prq falls back to modern pitch for unknown fonts", {
  expect_equal(get_rtf_font_prq("NonExistentFont"), 1L)
})

test_that("get_tex_font_cmd falls back to ttfamily for unknown fonts", {
  expect_match(get_tex_font_cmd("NonExistentFont"), "ttfamily")
})


# ── paper_dims_twips: all sizes and orientations ─────────────────────────

test_that("paper_dims_twips portrait returns original dimensions", {
  letter <- paper_dims_twips("letter", "portrait")
  expect_equal(letter[["width"]],  12240L)
  expect_equal(letter[["height"]], 15840L)
})

test_that("paper_dims_twips landscape swaps for all paper sizes", {
  for (sz in c("letter", "a4", "legal")) {
    p <- paper_dims_twips(sz, "portrait")
    l <- paper_dims_twips(sz, "landscape")
    expect_equal(p[["width"]],  l[["height"]], info = paste0(sz, " width"))
    expect_equal(p[["height"]], l[["width"]],  info = paste0(sz, " height"))
  }
})

test_that("paper_dims_twips a4 has correct dimensions", {
  a4 <- paper_dims_twips("a4", "portrait")
  expect_equal(a4[["width"]],  11906L)
  expect_equal(a4[["height"]], 16838L)
})

test_that("paper_dims_twips legal has correct dimensions", {
  legal <- paper_dims_twips("legal", "portrait")
  expect_equal(legal[["width"]],  12240L)
  expect_equal(legal[["height"]], 20163L)
})

test_that("paper_dims_twips warns and falls back for unknown paper size", {
  expect_warning(
    dims <- paper_dims_twips("tabloid", "portrait"),
    "Unknown paper size"
  )
  letter <- paper_dims_twips("letter", "portrait")
  expect_equal(dims, letter)
})


# ── is_system_font_available: Windows always TRUE ────────────────────────

test_that("is_system_font_available returns TRUE for standard fonts on Windows", {
  skip_if_not(tolower(Sys.info()[["sysname"]]) == "windows",
              "Test only runs on Windows")
  expect_true(is_system_font_available("Arial"))
  expect_true(is_system_font_available("Courier New"))
  expect_true(is_system_font_available("Times New Roman"))
  expect_true(is_system_font_available("SomeRandomFont"))
})


# ── hex_to_rgb: 3-digit hex and invalid hex ──────────────────────────────

test_that("hex_to_rgb handles 3-digit shorthand", {
  rgb <- hex_to_rgb("#F00")
  expect_equal(rgb, c(r = 255L, g = 0L, b = 0L))

  rgb2 <- hex_to_rgb("#ABC")
  expect_equal(rgb2, c(r = 170L, g = 187L, b = 204L))
})

test_that("hex_to_rgb handles input without # prefix", {
  rgb <- hex_to_rgb("FF0000")
  expect_equal(rgb, c(r = 255L, g = 0L, b = 0L))
})

test_that("hex_to_rgb errors on invalid hex length", {
  expect_error(hex_to_rgb("#FFFF"), "Invalid hex color")
  expect_error(hex_to_rgb("#F"), "Invalid hex color")
  expect_error(hex_to_rgb("#FFFFFFF"), "Invalid hex color")
})


# ── hex_to_latex_color: format ────────────────────────────────────────────

test_that("hex_to_latex_color strips # and uppercases", {
  expect_equal(hex_to_latex_color("#ff0000"), "FF0000")
  expect_equal(hex_to_latex_color("#003366"), "003366")
  expect_equal(hex_to_latex_color("#ABCDEF"), "ABCDEF")
})


# ── resolve_color: NULL and NA ───────────────────────────────────────────

test_that("resolve_color returns NULL for NULL", {
  expect_null(resolve_color(NULL))
})

test_that("resolve_color returns NULL for NA", {
  expect_null(resolve_color(NA))
  expect_null(resolve_color(NA_character_))
})

test_that("resolve_color error on unknown name without close match", {
  expect_error(resolve_color("xyzzy123"), "hex string")
})


# ── resolve_line_width: multi-element character error ────────────────────

test_that("resolve_line_width errors on multi-element character vector", {
  expect_error(resolve_line_width(c("thin", "thick")), "single string")
})


# ── resolve_tokens: NULL and non-character input ─────────────────────────

test_that("resolve_tokens returns NULL for NULL input", {
  expect_null(resolve_tokens(NULL, list(thepage = "1")))
})

test_that("resolve_tokens returns non-character input as-is", {
  expect_equal(resolve_tokens(42, list(thepage = "1")), 42)
})

test_that("resolve_tokens handles vector of multiple strings", {
  token_map <- list(x = "A", y = "B")
  result <- resolve_tokens(c("{x}", "{y}", "plain"), token_map)
  expect_equal(result, c("A", "B", "plain"))
})


# ── os_default_fonts: values are in font tables ─────────────────────────

test_that("os_default_fonts returns fonts that exist in font tables", {
  fonts <- os_default_fonts()
  for (nm in c("mono", "sans", "serif")) {
    fam <- lookup_font_family(fonts[[nm]])
    expect_true(fam %in% c("modern", "swiss", "roman"),
                info = paste0("Font: ", fonts[[nm]]))
  }
})


# ── Grouped sub-lists ────────────────────────────────────────────────────

test_that("fr_env$presets contains hline, linestyles, and line_widths", {
  expect_true(is.list(fr_env$presets))
  expect_true("hline" %in% names(fr_env$presets))
  expect_true("linestyles" %in% names(fr_env$presets))
  expect_true("line_widths" %in% names(fr_env$presets))
  expect_identical(fr_env$presets$hline, fr_env$hline_presets)
  expect_identical(fr_env$presets$linestyles, fr_env$valid_linestyles)
  expect_identical(fr_env$presets$line_widths, fr_env$line_widths)
})

test_that("fr_env$rtf contains linestyle, cell_border, para_border, specials, unicode", {
  expect_true(is.list(fr_env$rtf))
  expect_named(fr_env$rtf, c("linestyle", "cell_border", "para_border", "specials", "unicode"),
               ignore.order = TRUE)
  expect_identical(fr_env$rtf$linestyle, fr_env$linestyle_rtf)
  expect_identical(fr_env$rtf$cell_border, fr_env$cell_border_rtf)
  expect_identical(fr_env$rtf$specials, fr_env$rtf_specials)
})

test_that("fr_env$validation contains aligns, valigns, linestyles", {
  expect_true(is.list(fr_env$validation))
  expect_identical(fr_env$validation$aligns, fr_env$valid_aligns)
  expect_identical(fr_env$validation$valigns, fr_env$valid_valigns)
  expect_identical(fr_env$validation$linestyles, fr_env$valid_linestyles)
})


# ── Rendering constants ─────────────────────────────────────────────────

test_that("LaTeX rendering constants exist and have expected types", {
  expect_true(is.numeric(fr_env$latex_leading_factor))
  expect_true(is.character(fr_env$latex_rowsep))
  expect_true(is.numeric(fr_env$latex_space_width_em))
  expect_true(is.numeric(fr_env$latex_fn_sep_width_pt))
  expect_true(is.numeric(fr_env$points_per_inch))
  expect_equal(fr_env$points_per_inch, 72)
})

test_that("RTF rendering constants exist and have expected types", {
  expect_true(is.numeric(fr_env$rtf_leading_factor))
  expect_true(is.integer(fr_env$rtf_min_headery))
  expect_true(is.integer(fr_env$rtf_decimal_pad))
  expect_true(is.numeric(fr_env$rtf_box_border_wd))
  expect_true(is.integer(fr_env$rtf_spanner_brdrw))
})


# ── Vertical alignment maps ─────────────────────────────────────────────

test_that("valid_valigns contains all expected values", {
  expect_equal(fr_env$valid_valigns, c("top", "middle", "bottom"))
})

test_that("valign_to_rtf maps all valid vertical alignments", {
  for (v in fr_env$valid_valigns) {
    expect_true(v %in% names(fr_env$valign_to_rtf),
                info = paste0("valign_to_rtf should contain '", v, "'"))
  }
  # top maps to empty string (default)
  expect_equal(fr_env$valign_to_rtf[["top"]], "")
  expect_match(fr_env$valign_to_rtf[["middle"]], "clvertalc")
  expect_match(fr_env$valign_to_rtf[["bottom"]], "clvertalb")
})

test_that("valign_to_latex maps all valid vertical alignments", {
  for (v in fr_env$valid_valigns) {
    expect_true(v %in% names(fr_env$valign_to_latex),
                info = paste0("valign_to_latex should contain '", v, "'"))
  }
  expect_equal(fr_env$valign_to_latex[["top"]], "t")
  expect_equal(fr_env$valign_to_latex[["middle"]], "m")
  expect_equal(fr_env$valign_to_latex[["bottom"]], "b")
})


# ── LaTeX linestyle map ──────────────────────────────────────────────────

test_that("linestyle_latex maps all valid linestyles", {
  for (style in fr_env$valid_linestyles) {
    expect_true(style %in% names(fr_env$linestyle_latex),
                info = paste0("linestyle_latex should contain '", style, "'"))
  }
  expect_equal(fr_env$linestyle_latex[["solid"]], "solid")
  expect_equal(fr_env$linestyle_latex[["dashdot"]], "dashed")
})


# ── RTF border map completeness ──────────────────────────────────────────

test_that("cell_border_rtf maps all four sides", {
  expect_named(fr_env$cell_border_rtf, c("top", "bottom", "left", "right"),
               ignore.order = TRUE)
})

test_that("para_border_rtf maps all four sides", {
  expect_named(fr_env$para_border_rtf, c("top", "bottom", "left", "right"),
               ignore.order = TRUE)
})


# ── hline_presets detailed checks ────────────────────────────────────────

test_that("hline_presets$box is the string sentinel 'box'", {
  expect_equal(fr_env$hline_presets$box, "box")
})

test_that("hline_presets$hsides has header above + body below", {
  p <- fr_env$hline_presets$hsides
  expect_length(p, 2L)
  expect_equal(p[[1]]$region, "header")
  expect_equal(p[[1]]$side, "above")
  expect_equal(p[[2]]$region, "body")
  expect_equal(p[[2]]$side, "below")
})

test_that("hline_presets$above has single rule above header", {
  p <- fr_env$hline_presets$above
  expect_length(p, 1L)
  expect_equal(p[[1]]$region, "header")
  expect_equal(p[[1]]$side, "above")
})

test_that("hline_presets$below has single rule below body", {
  p <- fr_env$hline_presets$below
  expect_length(p, 1L)
  expect_equal(p[[1]]$region, "body")
  expect_equal(p[[1]]$side, "below")
})

test_that("hline_presets$booktabs has correct widths (thick/thin/thick)", {
  p <- fr_env$hline_presets$booktabs
  expect_equal(p[[1]]$width, 1.0)  # toprule
  expect_equal(p[[2]]$width, 0.5)  # midrule
  expect_equal(p[[3]]$width, 1.0)  # bottomrule
})


# ── Backend registry ─────────────────────────────────────────────────────

test_that("fr_env$backends is initialized as a list", {
  expect_true(is.list(fr_env$backends))
})


# ── builtin_tokens ───────────────────────────────────────────────────────

test_that("builtin_tokens contains all four token names", {
  expect_equal(
    sort(fr_env$builtin_tokens),
    sort(c("thepage", "total_pages", "program", "datetime"))
  )
})
