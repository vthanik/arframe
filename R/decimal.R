# ──────────────────────────────────────────────────────────────────────────────
# decimal.R — Full stat display alignment engine (10-type taxonomy)
#
# Architecture:
#   1. detect_stat_types()      — vectorized type classification across a column
#   2. parse_stat_value()       — extract typed components via regex
#   3. compute_stat_widths()    — column-wide max character widths per component
#   4. rebuild_stat_aligned()   — reconstruct as space-padded fixed-width string
#   5. align_decimal_column()   — master pipeline: detect → parse → widths → rebuild
#   6. compute_all_decimal_geometry() — integration with finalize_spec()
#
# All formatted strings have the same nchar (padded). In monospace fonts
# (Courier New — the pharma standard), space padding produces pixel-perfect
# alignment. Rendered as a single cell with left indent for centering.
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# 1. Type Detection (vectorized)
# ══════════════════════════════════════════════════════════════════════════════

#' Stat display type patterns
#'
#' ORDER MATTERS: most specific patterns first to avoid false matches.
#' E.g. `est_ci` (has comma inside parens) must precede `est_spread`
#' (parens without comma), which must precede `n_pct`.
#' @noRd
stat_type_patterns <- c(
  missing = "^\\s*$|^[-\u2014\u2013]{1,3}$|^(NA|NE|NC|ND|NR)$",
  n_over_N_pct = "^\\s*\\d+\\s*/\\s*\\d+\\s*\\(\\s*[<>]?\\d+\\.?\\d*\\s*%?\\s*\\)\\s*$",
  est_ci = "^\\s*-?\\d+\\.?\\d*\\s*\\(\\s*-?\\d+\\.?\\d*\\s*,\\s*-?\\d+\\.?\\d*\\s*\\)\\s*$",
  n_pct = "^\\s*\\d+\\s*\\(\\s*[<>]?\\d+\\.?\\d*\\s*%?\\s*\\)\\s*$",
  est_spread = "^\\s*-?\\d+\\.?\\d*\\s*\\(\\s*-?\\d+\\.?\\d*\\s*\\)\\s*$",
  int_range = "^\\s*\\d+\\s+[-\u2013\u2014]\\s+\\d+\\s*$",
  range_pair = "^\\s*-?\\d+\\.?\\d*\\s*,\\s*-?\\d+\\.?\\d*\\s*$",
  pvalue = "^\\s*[<>=]\\d+\\.\\d+\\s*$",
  scalar_float = "^\\s*-?\\d+\\.\\d+\\s*$",
  n_only = "^\\s*\\d+\\s*$"
)

#' Type family membership for family-aware dominant type selection
#' @noRd
stat_type_family <- c(
  n_only = "count",
  n_pct = "count",
  n_over_N_pct = "count",
  scalar_float = "float",
  pvalue = "float",
  est_spread = "estimate",
  est_ci = "estimate",
  range_pair = "range",
  int_range = "range"
)

#' Richness rank within each family (higher = more complex structure)
#' @noRd
stat_type_richness <- c(
  n_only = 1L,
  n_pct = 2L,
  n_over_N_pct = 3L,
  scalar_float = 1L,
  pvalue = 2L,
  est_spread = 1L,
  est_ci = 2L,
  range_pair = 1L,
  int_range = 2L
)

#' Tie-breaker priority across families (estimate > range > count > float)
#' @noRd
stat_family_priority <- c(estimate = 4L, range = 3L, count = 2L, float = 1L)


#' Detect the stat display type of a single value
#'
#' @param value Character scalar (trimmed).
#' @return Character scalar — one of the type names, or "unknown".
#' @noRd
detect_stat_type <- function(value) {
  value <- trimws(value)
  for (i in seq_along(stat_type_patterns)) {
    if (grepl(stat_type_patterns[[i]], value, perl = TRUE)) {
      return(names(stat_type_patterns)[[i]])
    }
  }
  "unknown"
}


#' Vectorized type detection across a column
#'
#' Tests all values against each pattern in priority order using
#' `stringi::stri_detect_regex()` (vectorized). Each value is assigned
#' the first matching type.
#'
#' @param content_vec Character vector of trimmed cell values.
#' @return Character vector of type names (same length as input).
#' @noRd
detect_stat_types <- function(content_vec) {
  n <- length(content_vec)
  result <- rep("unknown", n)
  unassigned <- rep(TRUE, n)

  for (i in seq_along(stat_type_patterns)) {
    if (!any(unassigned)) {
      break
    }
    matches <- stringi::stri_detect_regex(content_vec, stat_type_patterns[[i]])
    hits <- unassigned & matches
    if (any(hits)) {
      result[hits] <- names(stat_type_patterns)[[i]]
      unassigned[hits] <- FALSE
    }
  }

  result
}


# ══════════════════════════════════════════════════════════════════════════════
# 2. Typed Parsing
# ══════════════════════════════════════════════════════════════════════════════

#' Parse a stat value into typed components
#'
#' All fields are guaranteed non-NULL (empty string for absent components)
#' so callers never need `%||%` in hot loops.
#'
#' @param value Character scalar.
#' @param type_name Character scalar — detected type.
#' @return Named list with type-specific fields.
#' @noRd
parse_stat_value <- function(value, type_name) {
  switch(
    type_name,

    missing = list(type = "missing", raw = value),

    n_only = list(type = "n_only", n = value, raw = value),

    scalar_float = {
      m <- stringi::stri_match_first_regex(value, "^(-?)(\\d+)\\.(\\d+)$")
      list(
        type = "scalar_float",
        sign = m[1, 2],
        int = m[1, 3],
        dec = m[1, 4],
        raw = value
      )
    },

    pvalue = {
      m <- stringi::stri_match_first_regex(value, "^([<>=])(\\d+)\\.(\\d+)$")
      list(
        type = "pvalue",
        prefix = m[1, 2],
        int = m[1, 3],
        dec = m[1, 4],
        raw = value
      )
    },

    n_pct = {
      m <- stringi::stri_match_first_regex(
        value,
        "^(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)$"
      )
      list(
        type = "n_pct",
        n = m[1, 2],
        pct_prefix = m[1, 3],
        pct_int = m[1, 4],
        pct_dec = m[1, 5],
        pct_sign = m[1, 6],
        raw = value
      )
    },

    n_over_N_pct = {
      m <- stringi::stri_match_first_regex(
        value,
        "^(\\d+)\\s*/\\s*(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)$"
      )
      list(
        type = "n_over_N_pct",
        num = m[1, 2],
        den = m[1, 3],
        pct_prefix = m[1, 4],
        pct_int = m[1, 5],
        pct_dec = m[1, 6],
        pct_sign = m[1, 7],
        raw = value
      )
    },

    est_spread = {
      m <- stringi::stri_match_first_regex(
        value,
        "^(-?)(\\d+)\\.?(\\d*)\\s*\\(\\s*(-?)(\\d+)\\.?(\\d*)\\s*\\)$"
      )
      list(
        type = "est_spread",
        est_sign = m[1, 2],
        est_int = m[1, 3],
        est_dec = m[1, 4],
        sprd_sign = m[1, 5],
        sprd_int = m[1, 6],
        sprd_dec = m[1, 7],
        raw = value
      )
    },

    est_ci = {
      m <- stringi::stri_match_first_regex(
        value,
        "^(-?)(\\d+)\\.?(\\d*)\\s*\\(\\s*(-?)(\\d+)\\.?(\\d*)\\s*,\\s*(-?)(\\d+)\\.?(\\d*)\\s*\\)$"
      )
      list(
        type = "est_ci",
        est_sign = m[1, 2],
        est_int = m[1, 3],
        est_dec = m[1, 4],
        lo_sign = m[1, 5],
        lo_int = m[1, 6],
        lo_dec = m[1, 7],
        hi_sign = m[1, 8],
        hi_int = m[1, 9],
        hi_dec = m[1, 10],
        raw = value
      )
    },

    range_pair = {
      m <- stringi::stri_match_first_regex(
        value,
        "^(-?)(\\d+)\\.?(\\d*)\\s*,\\s*(-?)(\\d+)\\.?(\\d*)$"
      )
      list(
        type = "range_pair",
        l_sign = m[1, 2],
        l_int = m[1, 3],
        l_dec = m[1, 4],
        r_sign = m[1, 5],
        r_int = m[1, 6],
        r_dec = m[1, 7],
        raw = value
      )
    },

    int_range = {
      m <- stringi::stri_match_first_regex(
        value,
        "^(\\d+)\\s+([-\u2013\u2014])\\s+(\\d+)$"
      )
      list(
        type = "int_range",
        left = m[1, 2],
        sep = m[1, 3],
        right = m[1, 4],
        raw = value
      )
    },

    # Fallback for unknown types
    list(type = "unknown", raw = value)
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# 3. Column-Wide Width Computation
# ══════════════════════════════════════════════════════════════════════════════

#' Check if any parsed value has a non-empty field
#' @noRd
has_field <- function(typed, field_name) {
  any(vapply(typed, function(p) nzchar(p[[field_name]]), logical(1)))
}

#' Compute max character widths per component across a column
#'
#' @param parsed_values List of parsed value lists.
#' @param dominant_type Character — the dominant type for this column.
#' @return Named list with max widths and `full_width`.
#' @noRd
compute_stat_widths <- function(parsed_values, dominant_type) {
  # Filter to values of the dominant type for width calculation
  typed <- Filter(function(p) p$type == dominant_type, parsed_values)
  if (length(typed) == 0L) {
    raw_widths <- vapply(parsed_values, function(p) nchar(p$raw), integer(1))
    return(list(full_width = max(0L, raw_widths)))
  }

  maxw <- function(key) {
    vals <- vapply(typed, function(p) nchar(p[[key]]), integer(1))
    max(0L, vals)
  }

  maxw_with_sign <- function(int_key, sign_key) {
    vals <- vapply(
      typed,
      function(p) {
        nchar(p[[sign_key]]) + nchar(p[[int_key]])
      },
      integer(1)
    )
    max(0L, vals)
  }

  switch(
    dominant_type,

    n_only = {
      w_n <- maxw("n")
      list(w_n = w_n, full_width = w_n)
    },

    scalar_float = {
      w_sign_int <- maxw_with_sign("int", "sign")
      w_dec <- maxw("dec")
      list(
        w_sign_int = w_sign_int,
        w_dec = w_dec,
        full_width = w_sign_int + 1L + w_dec
      )
    },

    pvalue = {
      w_prefix <- maxw("prefix")
      w_int <- maxw("int")
      w_dec <- maxw("dec")
      list(
        w_prefix = w_prefix,
        w_int = w_int,
        w_dec = w_dec,
        full_width = w_prefix + w_int + 1L + w_dec
      )
    },

    n_pct = {
      w_n <- maxw("n")
      w_pct_prefix <- maxw("pct_prefix")
      w_pct_int <- maxw("pct_int")
      w_pct_dec <- maxw("pct_dec")
      w_pct_sign <- maxw("pct_sign")
      has_dec <- has_field(typed, "pct_dec")
      fw <- w_n + 2L + w_pct_prefix + w_pct_int
      if (has_dec) {
        fw <- fw + 1L + w_pct_dec
      }
      fw <- fw + w_pct_sign + 1L
      list(
        w_n = w_n,
        w_pct_prefix = w_pct_prefix,
        w_pct_int = w_pct_int,
        w_pct_dec = w_pct_dec,
        w_pct_sign = w_pct_sign,
        has_dec = has_dec,
        full_width = fw
      )
    },

    n_over_N_pct = {
      w_num <- maxw("num")
      w_den <- maxw("den")
      w_pct_prefix <- maxw("pct_prefix")
      w_pct_int <- maxw("pct_int")
      w_pct_dec <- maxw("pct_dec")
      w_pct_sign <- maxw("pct_sign")
      has_dec <- has_field(typed, "pct_dec")
      fw <- w_num + 1L + w_den + 2L + w_pct_prefix + w_pct_int
      if (has_dec) {
        fw <- fw + 1L + w_pct_dec
      }
      fw <- fw + w_pct_sign + 1L
      list(
        w_num = w_num,
        w_den = w_den,
        w_pct_prefix = w_pct_prefix,
        w_pct_int = w_pct_int,
        w_pct_dec = w_pct_dec,
        w_pct_sign = w_pct_sign,
        has_dec = has_dec,
        full_width = fw
      )
    },

    est_spread = {
      w_est_si <- maxw_with_sign("est_int", "est_sign")
      w_est_dec <- maxw("est_dec")
      w_sprd_si <- maxw_with_sign("sprd_int", "sprd_sign")
      w_sprd_dec <- maxw("sprd_dec")
      has_est_dec <- has_field(typed, "est_dec")
      has_sprd_dec <- has_field(typed, "sprd_dec")
      fw <- w_est_si
      if (has_est_dec) {
        fw <- fw + 1L + w_est_dec
      }
      fw <- fw + 2L + w_sprd_si
      if (has_sprd_dec) {
        fw <- fw + 1L + w_sprd_dec
      }
      fw <- fw + 1L
      list(
        w_est_si = w_est_si,
        w_est_dec = w_est_dec,
        w_sprd_si = w_sprd_si,
        w_sprd_dec = w_sprd_dec,
        has_est_dec = has_est_dec,
        has_sprd_dec = has_sprd_dec,
        full_width = fw
      )
    },

    est_ci = {
      w_est_si <- maxw_with_sign("est_int", "est_sign")
      w_est_dec <- maxw("est_dec")
      w_lo_si <- maxw_with_sign("lo_int", "lo_sign")
      w_lo_dec <- maxw("lo_dec")
      w_hi_si <- maxw_with_sign("hi_int", "hi_sign")
      w_hi_dec <- maxw("hi_dec")
      has_est_dec <- has_field(typed, "est_dec")
      has_lo_dec <- has_field(typed, "lo_dec")
      has_hi_dec <- has_field(typed, "hi_dec")
      fw <- w_est_si
      if (has_est_dec) {
        fw <- fw + 1L + w_est_dec
      }
      fw <- fw + 2L + w_lo_si
      if (has_lo_dec) {
        fw <- fw + 1L + w_lo_dec
      }
      fw <- fw + 2L + w_hi_si
      if (has_hi_dec) {
        fw <- fw + 1L + w_hi_dec
      }
      fw <- fw + 1L
      list(
        w_est_si = w_est_si,
        w_est_dec = w_est_dec,
        w_lo_si = w_lo_si,
        w_lo_dec = w_lo_dec,
        w_hi_si = w_hi_si,
        w_hi_dec = w_hi_dec,
        has_est_dec = has_est_dec,
        has_lo_dec = has_lo_dec,
        has_hi_dec = has_hi_dec,
        full_width = fw
      )
    },

    range_pair = {
      w_l_si <- maxw_with_sign("l_int", "l_sign")
      w_l_dec <- maxw("l_dec")
      w_r_si <- maxw_with_sign("r_int", "r_sign")
      w_r_dec <- maxw("r_dec")
      has_l_dec <- has_field(typed, "l_dec")
      has_r_dec <- has_field(typed, "r_dec")
      fw <- w_l_si
      if (has_l_dec) {
        fw <- fw + 1L + w_l_dec
      }
      fw <- fw + 2L + w_r_si
      if (has_r_dec) {
        fw <- fw + 1L + w_r_dec
      }
      list(
        w_l_si = w_l_si,
        w_l_dec = w_l_dec,
        w_r_si = w_r_si,
        w_r_dec = w_r_dec,
        has_l_dec = has_l_dec,
        has_r_dec = has_r_dec,
        full_width = fw
      )
    },

    int_range = {
      w_left <- maxw("left")
      w_right <- maxw("right")
      list(
        w_left = w_left,
        w_right = w_right,
        full_width = w_left + 3L + w_right
      )
    },

    # Fallback
    list(full_width = 0L)
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. String Reconstruction
# ══════════════════════════════════════════════════════════════════════════════

#' Build a float part: sign+int right-aligned, optional ".dec" left-aligned
#' @noRd
pad_float_part <- function(sign, int, dec, w_si, w_dec, has_dec) {
  padded_si <- stringi::stri_pad_left(paste0(sign, int), w_si)
  if (isTRUE(has_dec)) {
    paste0(padded_si, ".", stringi::stri_pad_right(dec, w_dec))
  } else {
    padded_si
  }
}

#' Build a percentage part: prefix + int \[.dec\] + sign
#' @noRd
pad_pct_part <- function(pct_prefix, pct_int, pct_dec, pct_sign, widths) {
  padded_pfx <- stringi::stri_pad_left(pct_prefix, widths$w_pct_prefix)
  padded_int <- stringi::stri_pad_left(pct_int, widths$w_pct_int)
  pct <- paste0(padded_pfx, padded_int)
  if (isTRUE(widths$has_dec)) {
    pct <- paste0(pct, ".", stringi::stri_pad_right(pct_dec, widths$w_pct_dec))
  }
  paste0(pct, stringi::stri_pad_right(pct_sign, widths$w_pct_sign))
}


#' Rebuild a parsed value as a fixed-width aligned string
#'
#' @param parsed Named list from `parse_stat_value()`.
#' @param widths Named list from `compute_stat_widths()`.
#' @param dominant_type Character — the dominant type.
#' @return Character scalar padded to `widths$full_width`.
#' @noRd
rebuild_stat_aligned <- function(parsed, widths, dominant_type) {
  fw <- widths$full_width
  if (fw == 0L) {
    return("")
  }

  # Missing / unknown → pad to full width
  if (parsed$type %in% c("missing", "unknown")) {
    return(strrep(" ", fw))
  }

  # If this value's type doesn't match dominant, try compatible alignment
  if (parsed$type != dominant_type) {
    # --- n_only fallbacks (all families) ---
    if (parsed$type == "n_only") {
      w <- switch(
        dominant_type,
        n_pct = widths$w_n,
        n_over_N_pct = widths$w_num,
        est_spread = ,
        est_ci = widths$w_est_si,
        scalar_float = widths$w_sign_int,
        pvalue = widths$w_int,
        range_pair = widths$w_l_si,
        int_range = widths$w_left,
        NULL
      )
      if (!is.null(w)) {
        padded_n <- stringi::stri_pad_left(parsed$n, w)
        return(stringi::stri_pad_right(padded_n, fw))
      }
    }

    # --- scalar_float fallbacks ---
    if (
      parsed$type == "scalar_float" &&
        dominant_type %in% c("est_spread", "est_ci")
    ) {
      est_part <- pad_float_part(
        parsed$sign,
        parsed$int,
        parsed$dec,
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      return(stringi::stri_pad_right(est_part, fw))
    }

    # --- pvalue fallbacks ---
    if (parsed$type == "pvalue" && dominant_type == "scalar_float") {
      return(pad_float_part(
        parsed$prefix,
        parsed$int,
        parsed$dec,
        widths$w_sign_int,
        widths$w_dec,
        TRUE
      ))
    }
    if (
      parsed$type == "pvalue" &&
        dominant_type %in% c("est_spread", "est_ci")
    ) {
      est_part <- pad_float_part(
        parsed$prefix,
        parsed$int,
        parsed$dec,
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      return(stringi::stri_pad_right(est_part, fw))
    }

    # --- n_pct fallbacks ---
    if (parsed$type == "n_pct" && dominant_type == "n_over_N_pct") {
      padded_n <- stringi::stri_pad_left(parsed$n, widths$w_num)
      slash_den <- strrep(" ", 1L + widths$w_den)
      pct <- pad_pct_part(
        parsed$pct_prefix,
        parsed$pct_int,
        parsed$pct_dec,
        parsed$pct_sign,
        widths
      )
      return(paste0(padded_n, slash_den, " (", pct, ")"))
    }

    # --- est_spread fallbacks ---
    if (parsed$type == "est_spread" && dominant_type == "est_ci") {
      est <- pad_float_part(
        parsed$est_sign,
        parsed$est_int,
        parsed$est_dec,
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      sprd <- pad_float_part(
        parsed$sprd_sign,
        parsed$sprd_int,
        parsed$sprd_dec,
        widths$w_lo_si,
        widths$w_lo_dec,
        widths$has_lo_dec
      )
      return(stringi::stri_pad_right(paste0(est, " (", sprd, ")"), fw))
    }

    # --- range_pair fallbacks ---
    if (
      parsed$type == "range_pair" &&
        dominant_type %in% c("est_spread", "est_ci")
    ) {
      l <- pad_float_part(
        parsed$l_sign,
        parsed$l_int,
        parsed$l_dec,
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      r <- paste0(parsed$r_sign, parsed$r_int)
      if (nzchar(parsed$r_dec)) {
        r <- paste0(r, ".", parsed$r_dec)
      }
      return(stringi::stri_pad_right(paste0(l, ", ", r), fw))
    }

    # Generic fallback: right-pad raw
    return(stringi::stri_pad_right(parsed$raw, fw))
  }

  switch(
    dominant_type,

    n_only = {
      stringi::stri_pad_left(parsed$n, widths$w_n)
    },

    scalar_float = {
      pad_float_part(
        parsed$sign,
        parsed$int,
        parsed$dec,
        widths$w_sign_int,
        widths$w_dec,
        TRUE
      )
    },

    pvalue = {
      padded_pfx <- stringi::stri_pad_left(parsed$prefix, widths$w_prefix)
      padded_int <- stringi::stri_pad_left(parsed$int, widths$w_int)
      padded_dec <- stringi::stri_pad_right(parsed$dec, widths$w_dec)
      paste0(padded_pfx, padded_int, ".", padded_dec)
    },

    n_pct = {
      padded_n <- stringi::stri_pad_left(parsed$n, widths$w_n)
      # R1: n==0 → suppress percentage part
      n_val <- suppressWarnings(as.integer(parsed$n))
      if (!is.na(n_val) && n_val == 0L) {
        return(stringi::stri_pad_right(padded_n, fw))
      }
      pct <- pad_pct_part(
        parsed$pct_prefix,
        parsed$pct_int,
        parsed$pct_dec,
        parsed$pct_sign,
        widths
      )
      paste0(padded_n, " (", pct, ")")
    },

    n_over_N_pct = {
      padded_num <- stringi::stri_pad_left(parsed$num, widths$w_num)
      padded_den <- stringi::stri_pad_right(parsed$den, widths$w_den)
      pct <- pad_pct_part(
        parsed$pct_prefix,
        parsed$pct_int,
        parsed$pct_dec,
        parsed$pct_sign,
        widths
      )
      paste0(padded_num, "/", padded_den, " (", pct, ")")
    },

    est_spread = {
      est <- pad_float_part(
        parsed$est_sign,
        parsed$est_int,
        parsed$est_dec,
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      sprd <- pad_float_part(
        parsed$sprd_sign,
        parsed$sprd_int,
        parsed$sprd_dec,
        widths$w_sprd_si,
        widths$w_sprd_dec,
        widths$has_sprd_dec
      )
      paste0(est, " (", sprd, ")")
    },

    est_ci = {
      est <- pad_float_part(
        parsed$est_sign,
        parsed$est_int,
        parsed$est_dec,
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      lo <- pad_float_part(
        parsed$lo_sign,
        parsed$lo_int,
        parsed$lo_dec,
        widths$w_lo_si,
        widths$w_lo_dec,
        widths$has_lo_dec
      )
      hi <- pad_float_part(
        parsed$hi_sign,
        parsed$hi_int,
        parsed$hi_dec,
        widths$w_hi_si,
        widths$w_hi_dec,
        widths$has_hi_dec
      )
      paste0(est, " (", lo, ", ", hi, ")")
    },

    range_pair = {
      l <- pad_float_part(
        parsed$l_sign,
        parsed$l_int,
        parsed$l_dec,
        widths$w_l_si,
        widths$w_l_dec,
        widths$has_l_dec
      )
      r <- pad_float_part(
        parsed$r_sign,
        parsed$r_int,
        parsed$r_dec,
        widths$w_r_si,
        widths$w_r_dec,
        widths$has_r_dec
      )
      paste0(l, ", ", r)
    },

    int_range = {
      padded_l <- stringi::stri_pad_left(parsed$left, widths$w_left)
      padded_r <- stringi::stri_pad_left(parsed$right, widths$w_right)
      paste0(padded_l, " ", parsed$sep, " ", padded_r)
    },

    # Fallback
    stringi::stri_pad_right(parsed$raw, fw)
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# 5. Master Column Aligner
# ══════════════════════════════════════════════════════════════════════════════

#' Align all values in a decimal column via pre-formatted padding
#'
#' Pipeline: detect types → find dominant type → parse all → compute widths →
#' rebuild all. Returns character vector where all strings have the same nchar.
#'
#' @param content_vec Character vector of cell values.
#' @return Character vector of aligned strings (all same `nchar`).
#' @noRd
align_decimal_column <- function(content_vec) {
  content_vec <- trimws(content_vec)
  n <- length(content_vec)
  if (n == 0L) {
    return(character(0))
  }

  # Vectorized type detection (one stri_detect_regex per pattern, not per value)
  types <- detect_stat_types(content_vec)

  # Find dominant type via family-aware priority
  non_skip <- types[!types %in% c("missing", "unknown")]
  if (length(non_skip) == 0L) {
    return(rep("", n))
  }

  families <- stat_type_family[non_skip]
  fam_counts <- table(families)
  max_count <- max(fam_counts)
  tied_fams <- names(fam_counts)[fam_counts == max_count]
  dominant_family <- tied_fams[which.max(stat_family_priority[tied_fams])]

  family_types <- unique(non_skip[families == dominant_family])
  dominant_type <- family_types[which.max(stat_type_richness[family_types])]

  # Parse all values
  parsed_values <- vector("list", n)
  for (i in seq_len(n)) {
    if (types[i] %in% c("missing", "unknown")) {
      parsed_values[[i]] <- list(type = types[i], raw = content_vec[i])
    } else {
      parsed_values[[i]] <- parse_stat_value(content_vec[i], types[i])
    }
  }

  # Compute column-wide widths from dominant type values
  widths <- compute_stat_widths(parsed_values, dominant_type)

  # Rebuild all values as fixed-width strings
  result <- vapply(
    parsed_values,
    function(p) {
      rebuild_stat_aligned(p, widths, dominant_type)
    },
    character(1),
    USE.NAMES = FALSE
  )

  # Ensure all same nchar (pad any shorter ones on the right)
  max_nc <- max(nchar(result))
  if (max_nc > 0L) {
    result <- stringi::stri_pad_right(result, max_nc)
  }

  result
}


# ══════════════════════════════════════════════════════════════════════════════
# 6. Integration with finalize_spec()
# ══════════════════════════════════════════════════════════════════════════════

#' Pre-compute decimal geometry for all decimal-aligned columns
#'
#' Called during `finalize_spec()` to pre-compute geometry for all columns
#' with `align = "decimal"`. The result is stored on `spec$decimal_geometry`
#' and used by both RTF and LaTeX backends.
#'
#' @param spec An fr_spec object (post-finalize_columns).
#' @return Named list of geometry objects (one per decimal column), or NULL.
#' @noRd
compute_all_decimal_geometry <- function(spec) {
  columns <- spec$columns
  col_names <- names(columns)

  # Early exit: skip loop if no decimal columns
  has_decimal <- vapply(
    columns,
    function(col) identical(col$align, "decimal"),
    logical(1)
  )
  if (!any(has_decimal)) {
    return(NULL)
  }

  result <- list()

  # Hoist invariants outside the column loop
  pb <- spec$body$page_by
  gb <- spec$body$group_by
  align_key <- if (length(pb) > 0L) {
    pb
  } else if (length(gb) > 0L) {
    gb
  } else {
    character(0)
  }
  space_twips <- measure_text_width_twips(
    " ",
    spec$page$font_family,
    spec$page$font_size
  )

  for (nm in col_names) {
    col <- columns[[nm]]
    if (!identical(col$align, "decimal")) {
      next
    }
    if (!nm %in% names(spec$data)) {
      next
    }

    vals <- as.character(spec$data[[nm]])
    vals[is.na(vals)] <- ""

    # Group-aware alignment: split by page_by/group_by for independent alignment
    col_width_twips <- inches_to_twips(col$width)

    if (length(align_key) > 0L && all(align_key %in% names(spec$data))) {
      if (length(align_key) == 1L) {
        group_labels <- as.character(spec$data[[align_key]])
      } else {
        group_labels <- inject(paste(
          !!!spec$data[align_key],
          sep = fr_env$group_sep
        ))
      }
      unique_groups <- unique(group_labels)
      formatted <- character(length(vals))
      group_max_nc <- integer(length(vals))
      for (grp in unique_groups) {
        mask <- group_labels == grp
        grp_formatted <- align_decimal_column(vals[mask])
        formatted[mask] <- grp_formatted
        grp_nc <- if (any(nzchar(grp_formatted))) {
          max(nchar(grp_formatted))
        } else {
          0L
        }
        group_max_nc[mask] <- grp_nc
      }
      # NO cross-group padding — each group keeps its natural nchar

      # Per-row center_offset (vectorized: one offset per group width)
      widths_twips <- as.integer(group_max_nc * space_twips)
      center_offset <- pmax(
        0L,
        as.integer((col_width_twips - widths_twips) / 2)
      )
      max_width_twips <- as.integer(max(group_max_nc) * space_twips)
    } else {
      formatted <- align_decimal_column(vals)

      # Compute scalar offset, replicate to vector for consistency
      if (length(formatted) > 0L && any(nzchar(formatted))) {
        max_chars <- max(nchar(formatted))
        max_width_twips <- as.integer(max_chars * space_twips)
        scalar_offset <- max(
          0L,
          as.integer((col_width_twips - max_width_twips) / 2)
        )
      } else {
        max_width_twips <- 0L
        scalar_offset <- 0L
      }
      center_offset <- rep(scalar_offset, length(formatted))
    }

    result[[nm]] <- list(
      formatted = formatted,
      center_offset = center_offset, # Integer VECTOR (one per row)
      max_width = max_width_twips
    )
  }

  if (length(result) == 0L) NULL else result
}
