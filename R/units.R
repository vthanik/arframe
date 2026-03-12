# ──────────────────────────────────────────────────────────────────────────────
# units.R — Unit conversion functions (twips, points, inches)
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# 1. Unit Conversions
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
inches_to_twips <- function(inches) as.integer(round(inches * 1440))

#' @noRd
twips_to_inches <- function(twips) twips / 1440

#' Points to RTF half-points
#' @noRd
pt_to_half_pt <- function(pt) as.integer(round(pt * 2))

#' Points to twips (1pt = 20 twips)
#' @noRd
pt_to_twips <- function(pt) as.integer(round(pt * 20))


# ══════════════════════════════════════════════════════════════════════════════
# 2. Row Height Calculation
#
# From the LaTeX Companion (3rd ed, p.441):
#   effective_height = arraystretch * (extrarowheight + 0.7 * baselineskip)
#   where baselineskip = 1.2 * font_size (standard ratio)
#
# From the RTF specification (v1.9.1, p.96):
#   \trrhN  Height of a table row in twips.
#           positive = "at least" this height
#           negative = "exact" this height (absolute value used)
#           0        = auto (sufficient for tallest character)
#
#   \slN    Line spacing in twips.
#           positive = "at least" (use if taller than tallest char)
#           negative = "exactly" (absolute value, even if shorter)
#   \slmult1 = multiple of "Single" line spacing
#
# Single line spacing in Word = 1.0 * baselineskip = 1.2 * font_size
# This matches the LaTeX convention exactly.
#
# For pharma tables we use "at-least" row height (positive \trrh) so
# multi-line cells grow naturally. The formula:
#
#   row_height = array_stretch * (extra_row_height + baseline_skip)
#   baseline_skip = 1.2 * font_size
#
# All values in twips (1pt = 20 twips).
# ══════════════════════════════════════════════════════════════════════════════

#' Baseline skip multiplier (from LaTeX Companion / Word default)
#' Single spacing = 1.2 * font_size
#' @noRd
fr_env$baseline_ratio <- 1.2

#' Calculate row height in twips
#'
#' Uses the LaTeX Companion formula adapted for RTF:
#'   row_height = array_stretch * (extra_row_height + baseline_skip)
#'
#' @param font_size_pt Numeric. Font size in points.
#' @param array_stretch Numeric. Row spacing multiplier (1.0 = single, 1.5 = 1.5x).
#'   Equivalent to LaTeX \\arraystretch.
#' @param extra_row_height_pt Numeric. Extra padding above baseline in points.
#'   Equivalent to LaTeX \\extrarowheight.
#' @return Integer. Row height in twips (positive = "at least").
#' @noRd
row_height_twips <- function(
  font_size_pt,
  array_stretch = 1.0,
  extra_row_height_pt = 1.0
) {
  baseline_skip_pt <- fr_env$baseline_ratio * font_size_pt
  height_pt <- array_stretch * (extra_row_height_pt + baseline_skip_pt)
  pt_to_twips(height_pt)
}


#' Calculate baseline skip in twips
#' @noRd
baseline_skip_twips <- function(font_size_pt) {
  pt_to_twips(fr_env$baseline_ratio * font_size_pt)
}
