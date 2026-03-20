# ──────────────────────────────────────────────────────────────────────────────
# decimal.R — Full stat display alignment engine (18-type taxonomy)
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
# (the submission font), space padding produces pixel-perfect
# alignment. Rendered as a single cell with left indent for centering.
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# 0. Constants & Regex Building Blocks
# ══════════════════════════════════════════════════════════════════════════════

#' Missing token alternation (reused across CI / pvalue patterns)
#' @noRd
MISSING_TOKEN_RE <- "NR|NE|NC|NA|ND|INF|-INF|BLQ|-"

#' Non-capturing numeric-or-token (for detection patterns)
#' @noRd
NUM_OR_TOK_NC <- paste0("(?:-?\\d+\\.?\\d*|", MISSING_TOKEN_RE, ")")

#' Capturing numeric-or-token — 4 groups: (sign)(int)(dec)|(token)
#' @noRd
NUM_OR_TOK_CAP <- paste0(
  "(?:(-?)(\\d+)\\.?(\\d*)|(",
  MISSING_TOKEN_RE,
  "))"
)

#' Capturing pval-or-token — 4 groups: (prefix)(int)(dec)|(token)
#' @noRd
PVAL_OR_TOK_CAP <- paste0(
  "(?:([<>=]?)(\\d+)\\.(\\d+)|(",
  MISSING_TOKEN_RE,
  "))"
)

#' Standard compound gap width (spaces between segments)
#' @noRd
COMPOUND_GAP <- 4L


# ══════════════════════════════════════════════════════════════════════════════
# 1. Type Detection (vectorized)
# ══════════════════════════════════════════════════════════════════════════════

#' Stat type registry -- single source of truth for all type metadata
#'
#' ORDER MATTERS: most specific patterns first to avoid false matches.
#' Compound types first, then standard types in decreasing specificity.
#' @noRd
stat_type_registry <- list(
  # --- Missing (expanded with BLQ/INF/-INF) ---
  missing = list(
    pattern = paste0(
      "^\\s*$|^[-\u2014\u2013]{1,3}$|^(",
      "NA|NE|NC|ND|NR|BLQ|INF|-INF",
      ")$"
    ),
    family = "missing",
    richness = 0L
  ),

  # --- Compound types (most specific first) ---
  est_spread_pct_ci = list(
    pattern = paste0(
      "^\\s*-?\\d+\\.?\\d*\\s*\\(\\s*-?\\d+\\.?\\d*\\s*%\\s*\\)",
      "\\s+",
      "\\(\\s*-?\\d+\\.?\\d*\\s*,\\s*-?\\d+\\.?\\d*\\s*\\)\\s*$"
    ),
    family = "compound",
    richness = 5L
  ),
  est_ci_pval = list(
    pattern = paste0(
      "^\\s*",
      NUM_OR_TOK_NC,
      "\\s*\\(\\s*",
      NUM_OR_TOK_NC,
      "\\s*,\\s*",
      NUM_OR_TOK_NC,
      "\\s*\\)\\s+",
      "(?:[<>=]?\\d+\\.\\d+|",
      MISSING_TOKEN_RE,
      ")\\s*$"
    ),
    family = "compound",
    richness = 4L
  ),
  n_over_N_pct_ci = list(
    pattern = paste0(
      "^\\s*\\d+\\s*/\\s*\\d+\\s*\\(\\s*[<>]?\\d+\\.?\\d*\\s*%?\\s*\\)",
      "\\s+",
      "\\[\\s*-?\\d+\\.?\\d*\\s*,\\s*-?\\d+\\.?\\d*\\s*\\]\\s*$"
    ),
    family = "compound",
    richness = 4L
  ),
  n_pct_rate = list(
    pattern = paste0(
      "^\\s*\\d+\\s*\\(\\s*[<>]?\\d+\\.?\\d*\\s*%?\\s*\\)",
      "\\s+",
      "-?\\d+\\.?\\d*\\s*$"
    ),
    family = "compound",
    richness = 3L
  ),

  # --- Standard types ---
  n_over_N_pct = list(
    pattern = "^\\s*\\d+\\s*/\\s*\\d+\\s*\\(\\s*[<>]?\\d+\\.?\\d*\\s*%?\\s*\\)\\s*$",
    family = "count",
    richness = 3L
  ),
  est_ci = list(
    pattern = paste0(
      "^\\s*",
      NUM_OR_TOK_NC,
      "\\s*\\(\\s*",
      NUM_OR_TOK_NC,
      "\\s*,\\s*",
      NUM_OR_TOK_NC,
      "\\s*\\)\\s*$"
    ),
    family = "estimate",
    richness = 2L
  ),
  est_ci_bracket = list(
    pattern = paste0(
      "^\\s*",
      NUM_OR_TOK_NC,
      "\\s*\\[\\s*",
      NUM_OR_TOK_NC,
      "\\s*,\\s*",
      NUM_OR_TOK_NC,
      "\\s*\\]\\s*$"
    ),
    family = "estimate",
    richness = 2L
  ),
  n_pct = list(
    pattern = "^\\s*\\d+\\s*\\(\\s*[<>]?\\d+\\.?\\d*\\s*%?\\s*\\)\\s*$",
    family = "count",
    richness = 2L
  ),
  est_spread_pct = list(
    pattern = "^\\s*-?\\d+\\.?\\d*\\s*\\(\\s*-?\\d+\\.?\\d*\\s*%\\s*\\)\\s*$",
    family = "estimate",
    richness = 1L
  ),
  est_spread = list(
    pattern = "^\\s*-?\\d+\\.?\\d*\\s*\\(\\s*-?\\d+\\.?\\d*\\s*\\)\\s*$",
    family = "estimate",
    richness = 1L
  ),
  n_over_N = list(
    pattern = "^\\s*\\d+\\s*/\\s*\\d+\\s*$",
    family = "count",
    richness = 2L
  ),
  n_over_float = list(
    pattern = "^\\s*\\d+\\s*/\\s*\\d+\\.\\d+\\s*$",
    family = "count",
    richness = 2L
  ),
  int_range = list(
    pattern = "^\\s*\\d+\\s+[-\u2013\u2014]\\s+\\d+\\s*$",
    family = "range",
    richness = 2L
  ),
  range_pair = list(
    pattern = "^\\s*-?\\d+\\.?\\d*\\s*,\\s*-?\\d+\\.?\\d*\\s*$",
    family = "range",
    richness = 1L
  ),
  pvalue = list(
    pattern = "^\\s*[<>=]\\d+\\.\\d+\\s*$",
    family = "float",
    richness = 2L
  ),
  scalar_float = list(
    pattern = "^\\s*-?\\d+\\.\\d+\\s*$",
    family = "float",
    richness = 1L
  ),
  n_only = list(
    pattern = "^\\s*\\d+\\s*$",
    family = "count",
    richness = 1L
  )
)

#' Derived lookup vectors from stat_type_registry
#' @noRd
stat_type_patterns <- vapply(
  stat_type_registry,
  `[[`,
  character(1),
  "pattern"
)

#' @noRd
stat_type_family <- vapply(
  stat_type_registry,
  `[[`,
  character(1),
  "family"
)
# Remove types without a meaningful family (missing)
stat_type_family <- stat_type_family[
  !stat_type_family %in% "missing"
]

#' @noRd
stat_type_richness <- vapply(
  stat_type_registry,
  `[[`,
  integer(1),
  "richness"
)

#' Tie-breaker priority across families
#' @noRd
stat_family_priority <- c(
  compound = 5L,
  estimate = 4L,
  range = 3L,
  count = 2L,
  float = 1L
)


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
# 2. Parse Regex Constants (precomputed at load time)
# ══════════════════════════════════════════════════════════════════════════════

#' Token-aware est_ci parse regex (12 groups: 4 per position)
#' @noRd
est_ci_parse_re <- paste0(
  "^",
  NUM_OR_TOK_CAP,
  "\\s*\\(\\s*",
  NUM_OR_TOK_CAP,
  "\\s*,\\s*",
  NUM_OR_TOK_CAP,
  "\\s*\\)$"
)

#' Token-aware est_ci_bracket parse regex (12 groups)
#' @noRd
est_ci_bracket_parse_re <- paste0(
  "^",
  NUM_OR_TOK_CAP,
  "\\s*\\[\\s*",
  NUM_OR_TOK_CAP,
  "\\s*,\\s*",
  NUM_OR_TOK_CAP,
  "\\s*\\]$"
)

#' est_ci_pval parse regex (16 groups: 4 × 3 CI + 4 pval)
#' @noRd
est_ci_pval_parse_re <- paste0(
  "^",
  NUM_OR_TOK_CAP,
  "\\s*\\(\\s*",
  NUM_OR_TOK_CAP,
  "\\s*,\\s*",
  NUM_OR_TOK_CAP,
  "\\s*\\)\\s+",
  PVAL_OR_TOK_CAP,
  "$"
)

#' est_spread_pct parse regex (6 groups)
#' @noRd
est_spread_pct_parse_re <- paste0(
  "^(-?)(\\d+)\\.?(\\d*)\\s*\\(\\s*(-?)(\\d+)\\.?(\\d*)\\s*%\\s*\\)$"
)

#' est_spread_pct_ci parse regex (12 groups)
#' @noRd
est_spread_pct_ci_parse_re <- paste0(
  "^(-?)(\\d+)\\.?(\\d*)\\s*\\(\\s*(-?)(\\d+)\\.?(\\d*)\\s*%\\s*\\)",
  "\\s+",
  "\\(\\s*(-?)(\\d+)\\.?(\\d*)\\s*,\\s*(-?)(\\d+)\\.?(\\d*)\\s*\\)$"
)

#' n_pct_rate parse regex (8 groups)
#' @noRd
n_pct_rate_parse_re <- paste0(
  "^(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)",
  "\\s+(-?)(\\d+)\\.?(\\d*)$"
)

#' n_over_N_pct_ci parse regex (12 groups)
#' @noRd
n_over_N_pct_ci_parse_re <- paste0(
  "^(\\d+)\\s*/\\s*(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)",
  "\\s+\\[\\s*(-?)(\\d+)\\.?(\\d*)\\s*,\\s*(-?)(\\d+)\\.?(\\d*)\\s*\\]$"
)


# ══════════════════════════════════════════════════════════════════════════════
# 3. Typed Parsing
# ══════════════════════════════════════════════════════════════════════════════

#' Parse a stat value into typed components
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
      m <- stringi::stri_match_first_regex(value, est_ci_parse_re)
      m[is.na(m)] <- ""
      list(
        type = "est_ci",
        est_sign = m[1, 2],
        est_int = m[1, 3],
        est_dec = m[1, 4],
        est_token = m[1, 5],
        lo_sign = m[1, 6],
        lo_int = m[1, 7],
        lo_dec = m[1, 8],
        lo_token = m[1, 9],
        hi_sign = m[1, 10],
        hi_int = m[1, 11],
        hi_dec = m[1, 12],
        hi_token = m[1, 13],
        raw = value
      )
    },

    est_ci_bracket = {
      m <- stringi::stri_match_first_regex(value, est_ci_bracket_parse_re)
      m[is.na(m)] <- ""
      list(
        type = "est_ci_bracket",
        est_sign = m[1, 2],
        est_int = m[1, 3],
        est_dec = m[1, 4],
        est_token = m[1, 5],
        lo_sign = m[1, 6],
        lo_int = m[1, 7],
        lo_dec = m[1, 8],
        lo_token = m[1, 9],
        hi_sign = m[1, 10],
        hi_int = m[1, 11],
        hi_dec = m[1, 12],
        hi_token = m[1, 13],
        raw = value
      )
    },

    est_spread_pct = {
      m <- stringi::stri_match_first_regex(value, est_spread_pct_parse_re)
      list(
        type = "est_spread_pct",
        est_sign = m[1, 2],
        est_int = m[1, 3],
        est_dec = m[1, 4],
        sprd_sign = m[1, 5],
        sprd_int = m[1, 6],
        sprd_dec = m[1, 7],
        raw = value
      )
    },

    est_ci_pval = {
      m <- stringi::stri_match_first_regex(value, est_ci_pval_parse_re)
      m[is.na(m)] <- ""
      list(
        type = "est_ci_pval",
        est_sign = m[1, 2],
        est_int = m[1, 3],
        est_dec = m[1, 4],
        est_token = m[1, 5],
        lo_sign = m[1, 6],
        lo_int = m[1, 7],
        lo_dec = m[1, 8],
        lo_token = m[1, 9],
        hi_sign = m[1, 10],
        hi_int = m[1, 11],
        hi_dec = m[1, 12],
        hi_token = m[1, 13],
        pv_prefix = m[1, 14],
        pv_int = m[1, 15],
        pv_dec = m[1, 16],
        pv_token = m[1, 17],
        raw = value
      )
    },

    est_spread_pct_ci = {
      m <- stringi::stri_match_first_regex(value, est_spread_pct_ci_parse_re)
      list(
        type = "est_spread_pct_ci",
        est_sign = m[1, 2],
        est_int = m[1, 3],
        est_dec = m[1, 4],
        sprd_sign = m[1, 5],
        sprd_int = m[1, 6],
        sprd_dec = m[1, 7],
        ci_lo_sign = m[1, 8],
        ci_lo_int = m[1, 9],
        ci_lo_dec = m[1, 10],
        ci_hi_sign = m[1, 11],
        ci_hi_int = m[1, 12],
        ci_hi_dec = m[1, 13],
        raw = value
      )
    },

    n_pct_rate = {
      m <- stringi::stri_match_first_regex(value, n_pct_rate_parse_re)
      list(
        type = "n_pct_rate",
        n = m[1, 2],
        pct_prefix = m[1, 3],
        pct_int = m[1, 4],
        pct_dec = m[1, 5],
        pct_sign = m[1, 6],
        rate_sign = m[1, 7],
        rate_int = m[1, 8],
        rate_dec = m[1, 9],
        raw = value
      )
    },

    n_over_N_pct_ci = {
      m <- stringi::stri_match_first_regex(value, n_over_N_pct_ci_parse_re)
      list(
        type = "n_over_N_pct_ci",
        num = m[1, 2],
        den = m[1, 3],
        pct_prefix = m[1, 4],
        pct_int = m[1, 5],
        pct_dec = m[1, 6],
        pct_sign = m[1, 7],
        ci_lo_sign = m[1, 8],
        ci_lo_int = m[1, 9],
        ci_lo_dec = m[1, 10],
        ci_hi_sign = m[1, 11],
        ci_hi_int = m[1, 12],
        ci_hi_dec = m[1, 13],
        raw = value
      )
    },

    n_over_N = {
      m <- stringi::stri_match_first_regex(value, "^(\\d+)\\s*/\\s*(\\d+)$")
      list(
        type = "n_over_N",
        num = m[1, 2],
        den = m[1, 3],
        raw = value
      )
    },

    n_over_float = {
      m <- stringi::stri_match_first_regex(
        value,
        "^(\\d+)\\s*/\\s*(\\d+)\\.(\\d+)$"
      )
      list(
        type = "n_over_float",
        num = m[1, 2],
        den_int = m[1, 3],
        den_dec = m[1, 4],
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


#' Batch-parse stat values by type (vectorized regex)
#'
#' @param values Character vector of trimmed cell values.
#' @param types Character vector of detected types (same length as values).
#' @return List of named lists (same length as values).
#' @noRd
parse_stat_values_batch <- function(values, types) {
  n <- length(values)
  parsed <- vector("list", n)

  for (tp in unique(types)) {
    idx <- which(types == tp)
    vals <- values[idx]

    batch <- switch(
      tp,

      missing = lapply(vals, function(v) list(type = "missing", raw = v)),

      n_only = lapply(vals, function(v) list(type = "n_only", n = v, raw = v)),

      scalar_float = {
        m <- stringi::stri_match_first_regex(vals, "^(-?)(\\d+)\\.(\\d+)$")
        mapply(
          function(sign, int, dec, raw) {
            list(
              type = "scalar_float",
              sign = sign,
              int = int,
              dec = dec,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      pvalue = {
        m <- stringi::stri_match_first_regex(vals, "^([<>=])(\\d+)\\.(\\d+)$")
        mapply(
          function(prefix, int, dec, raw) {
            list(
              type = "pvalue",
              prefix = prefix,
              int = int,
              dec = dec,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      n_pct = {
        m <- stringi::stri_match_first_regex(
          vals,
          "^(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)$"
        )
        mapply(
          function(n_val, pp, pi, pd, ps, raw) {
            list(
              type = "n_pct",
              n = n_val,
              pct_prefix = pp,
              pct_int = pi,
              pct_dec = pd,
              pct_sign = ps,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          m[, 5],
          m[, 6],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      n_over_N_pct = {
        m <- stringi::stri_match_first_regex(
          vals,
          "^(\\d+)\\s*/\\s*(\\d+)\\s*\\(\\s*([<>]?)(\\d+)\\.?(\\d*)\\s*(%?)\\s*\\)$"
        )
        mapply(
          function(num, den, pp, pi, pd, ps, raw) {
            list(
              type = "n_over_N_pct",
              num = num,
              den = den,
              pct_prefix = pp,
              pct_int = pi,
              pct_dec = pd,
              pct_sign = ps,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          m[, 5],
          m[, 6],
          m[, 7],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      est_spread = {
        m <- stringi::stri_match_first_regex(
          vals,
          "^(-?)(\\d+)\\.?(\\d*)\\s*\\(\\s*(-?)(\\d+)\\.?(\\d*)\\s*\\)$"
        )
        mapply(
          function(es, ei, ed, ss, si, sd, raw) {
            list(
              type = "est_spread",
              est_sign = es,
              est_int = ei,
              est_dec = ed,
              sprd_sign = ss,
              sprd_int = si,
              sprd_dec = sd,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          m[, 5],
          m[, 6],
          m[, 7],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      est_ci = {
        m <- stringi::stri_match_first_regex(vals, est_ci_parse_re)
        m[is.na(m)] <- ""
        mapply(
          function(es, ei, ed, et, ls, li, ld, lt, hs, hi, hd, ht, raw) {
            list(
              type = "est_ci",
              est_sign = es,
              est_int = ei,
              est_dec = ed,
              est_token = et,
              lo_sign = ls,
              lo_int = li,
              lo_dec = ld,
              lo_token = lt,
              hi_sign = hs,
              hi_int = hi,
              hi_dec = hd,
              hi_token = ht,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          m[, 5],
          m[, 6],
          m[, 7],
          m[, 8],
          m[, 9],
          m[, 10],
          m[, 11],
          m[, 12],
          m[, 13],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      est_ci_bracket = {
        m <- stringi::stri_match_first_regex(vals, est_ci_bracket_parse_re)
        m[is.na(m)] <- ""
        mapply(
          function(es, ei, ed, et, ls, li, ld, lt, hs, hi, hd, ht, raw) {
            list(
              type = "est_ci_bracket",
              est_sign = es,
              est_int = ei,
              est_dec = ed,
              est_token = et,
              lo_sign = ls,
              lo_int = li,
              lo_dec = ld,
              lo_token = lt,
              hi_sign = hs,
              hi_int = hi,
              hi_dec = hd,
              hi_token = ht,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          m[, 5],
          m[, 6],
          m[, 7],
          m[, 8],
          m[, 9],
          m[, 10],
          m[, 11],
          m[, 12],
          m[, 13],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      est_spread_pct = {
        m <- stringi::stri_match_first_regex(vals, est_spread_pct_parse_re)
        mapply(
          function(es, ei, ed, ss, si, sd, raw) {
            list(
              type = "est_spread_pct",
              est_sign = es,
              est_int = ei,
              est_dec = ed,
              sprd_sign = ss,
              sprd_int = si,
              sprd_dec = sd,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          m[, 5],
          m[, 6],
          m[, 7],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      est_ci_pval = {
        m <- stringi::stri_match_first_regex(vals, est_ci_pval_parse_re)
        m[is.na(m)] <- ""
        mapply(
          function(
            es,
            ei,
            ed,
            et,
            ls,
            li,
            ld,
            lt,
            hs,
            hi,
            hd,
            ht,
            pp,
            pi,
            pd,
            pt,
            raw
          ) {
            list(
              type = "est_ci_pval",
              est_sign = es,
              est_int = ei,
              est_dec = ed,
              est_token = et,
              lo_sign = ls,
              lo_int = li,
              lo_dec = ld,
              lo_token = lt,
              hi_sign = hs,
              hi_int = hi,
              hi_dec = hd,
              hi_token = ht,
              pv_prefix = pp,
              pv_int = pi,
              pv_dec = pd,
              pv_token = pt,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          m[, 5],
          m[, 6],
          m[, 7],
          m[, 8],
          m[, 9],
          m[, 10],
          m[, 11],
          m[, 12],
          m[, 13],
          m[, 14],
          m[, 15],
          m[, 16],
          m[, 17],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      est_spread_pct_ci = {
        m <- stringi::stri_match_first_regex(vals, est_spread_pct_ci_parse_re)
        mapply(
          function(es, ei, ed, ss, si, sd, cls, cli, cld, chs, chi, chd, raw) {
            list(
              type = "est_spread_pct_ci",
              est_sign = es,
              est_int = ei,
              est_dec = ed,
              sprd_sign = ss,
              sprd_int = si,
              sprd_dec = sd,
              ci_lo_sign = cls,
              ci_lo_int = cli,
              ci_lo_dec = cld,
              ci_hi_sign = chs,
              ci_hi_int = chi,
              ci_hi_dec = chd,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          m[, 5],
          m[, 6],
          m[, 7],
          m[, 8],
          m[, 9],
          m[, 10],
          m[, 11],
          m[, 12],
          m[, 13],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      n_pct_rate = {
        m <- stringi::stri_match_first_regex(vals, n_pct_rate_parse_re)
        mapply(
          function(n_val, pp, pi, pd, ps, rs, ri, rd, raw) {
            list(
              type = "n_pct_rate",
              n = n_val,
              pct_prefix = pp,
              pct_int = pi,
              pct_dec = pd,
              pct_sign = ps,
              rate_sign = rs,
              rate_int = ri,
              rate_dec = rd,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          m[, 5],
          m[, 6],
          m[, 7],
          m[, 8],
          m[, 9],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      n_over_N_pct_ci = {
        m <- stringi::stri_match_first_regex(vals, n_over_N_pct_ci_parse_re)
        mapply(
          function(
            num,
            den,
            pp,
            pi,
            pd,
            ps,
            cls,
            cli,
            cld,
            chs,
            chi,
            chd,
            raw
          ) {
            list(
              type = "n_over_N_pct_ci",
              num = num,
              den = den,
              pct_prefix = pp,
              pct_int = pi,
              pct_dec = pd,
              pct_sign = ps,
              ci_lo_sign = cls,
              ci_lo_int = cli,
              ci_lo_dec = cld,
              ci_hi_sign = chs,
              ci_hi_int = chi,
              ci_hi_dec = chd,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          m[, 5],
          m[, 6],
          m[, 7],
          m[, 8],
          m[, 9],
          m[, 10],
          m[, 11],
          m[, 12],
          m[, 13],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      n_over_N = {
        m <- stringi::stri_match_first_regex(vals, "^(\\d+)\\s*/\\s*(\\d+)$")
        mapply(
          function(num, den, raw) {
            list(type = "n_over_N", num = num, den = den, raw = raw)
          },
          m[, 2],
          m[, 3],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      n_over_float = {
        m <- stringi::stri_match_first_regex(
          vals,
          "^(\\d+)\\s*/\\s*(\\d+)\\.(\\d+)$"
        )
        mapply(
          function(num, di, dd, raw) {
            list(
              type = "n_over_float",
              num = num,
              den_int = di,
              den_dec = dd,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      range_pair = {
        m <- stringi::stri_match_first_regex(
          vals,
          "^(-?)(\\d+)\\.?(\\d*)\\s*,\\s*(-?)(\\d+)\\.?(\\d*)$"
        )
        mapply(
          function(ls, li, ld, rs, ri, rd, raw) {
            list(
              type = "range_pair",
              l_sign = ls,
              l_int = li,
              l_dec = ld,
              r_sign = rs,
              r_int = ri,
              r_dec = rd,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          m[, 5],
          m[, 6],
          m[, 7],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      int_range = {
        m <- stringi::stri_match_first_regex(
          vals,
          "^(\\d+)\\s+([-\u2013\u2014])\\s+(\\d+)$"
        )
        mapply(
          function(left, sep, right, raw) {
            list(
              type = "int_range",
              left = left,
              sep = sep,
              right = right,
              raw = raw
            )
          },
          m[, 2],
          m[, 3],
          m[, 4],
          vals,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },

      # unknown fallback
      lapply(vals, function(v) list(type = "unknown", raw = v))
    )

    parsed[idx] <- batch
  }

  parsed
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. Column-Wide Width Computation
# ══════════════════════════════════════════════════════════════════════════════

#' Check if any parsed value has a non-empty field
#' @noRd
has_field <- function(typed, field_name) {
  any(vapply(typed, function(p) nzchar(p[[field_name]]), logical(1)))
}

#' Max width of sign+int OR token (for token-aware positions)
#' @noRd
maxw_si_or_tok <- function(typed, sign_key, int_key, tok_key) {
  vals <- vapply(
    typed,
    function(p) {
      tok <- p[[tok_key]]
      if (nzchar(tok)) {
        nchar(tok)
      } else {
        nchar(p[[sign_key]]) + nchar(p[[int_key]])
      }
    },
    integer(1)
  )
  max(0L, vals)
}

#' Compute max character widths per component across a column
#'
#' @param parsed_values List of parsed value lists.
#' @param dominant_type Character — the dominant type for this column.
#' @return Named list with max widths and `full_width`.
#' @noRd
compute_stat_widths <- function(parsed_values, dominant_type) {
  # Include sibling types with compatible field structures so widths

  # accommodate all values that share the alignment grid.
  # Same-structure siblings (identical fields — include directly)
  same_siblings <- switch(
    dominant_type,
    est_spread_pct = "est_spread",
    est_spread = "est_spread_pct",
    est_ci = "est_ci_bracket",
    est_ci_bracket = "est_ci",
    NULL
  )
  type_set <- c(dominant_type, same_siblings)
  typed <- Filter(function(p) p$type %in% type_set, parsed_values)

  # Subset siblings: adapt with empty fields for missing slots
  if (dominant_type == "n_over_N_pct") {
    sib <- Filter(function(p) p$type == "n_over_N", parsed_values)
    if (length(sib) > 0L) {
      typed <- c(
        typed,
        lapply(sib, function(p) {
          p$pct_prefix <- ""
          p$pct_int <- ""
          p$pct_dec <- ""
          p$pct_sign <- ""
          p
        })
      )
    }
  }

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

    est_spread_pct = {
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
      fw <- fw + 2L # "%)"
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

    est_ci = ,
    est_ci_bracket = {
      w_est_si <- maxw_si_or_tok(typed, "est_sign", "est_int", "est_token")
      w_est_dec <- maxw("est_dec")
      w_lo_si <- maxw_si_or_tok(typed, "lo_sign", "lo_int", "lo_token")
      w_lo_dec <- maxw("lo_dec")
      w_hi_si <- maxw_si_or_tok(typed, "hi_sign", "hi_int", "hi_token")
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

    n_over_N = {
      w_num <- maxw("num")
      w_den <- maxw("den")
      list(
        w_num = w_num,
        w_den = w_den,
        full_width = w_num + 1L + w_den
      )
    },

    n_over_float = {
      w_num <- maxw("num")
      w_den_int <- maxw("den_int")
      w_den_dec <- maxw("den_dec")
      list(
        w_num = w_num,
        w_den_int = w_den_int,
        w_den_dec = w_den_dec,
        full_width = w_num + 1L + w_den_int + 1L + w_den_dec
      )
    },

    est_ci_pval = {
      # CI part (token-aware)
      w_est_si <- maxw_si_or_tok(typed, "est_sign", "est_int", "est_token")
      w_est_dec <- maxw("est_dec")
      w_lo_si <- maxw_si_or_tok(typed, "lo_sign", "lo_int", "lo_token")
      w_lo_dec <- maxw("lo_dec")
      w_hi_si <- maxw_si_or_tok(typed, "hi_sign", "hi_int", "hi_token")
      w_hi_dec <- maxw("hi_dec")
      has_est_dec <- has_field(typed, "est_dec")
      has_lo_dec <- has_field(typed, "lo_dec")
      has_hi_dec <- has_field(typed, "hi_dec")
      ci_fw <- w_est_si
      if (has_est_dec) {
        ci_fw <- ci_fw + 1L + w_est_dec
      }
      ci_fw <- ci_fw + 2L + w_lo_si
      if (has_lo_dec) {
        ci_fw <- ci_fw + 1L + w_lo_dec
      }
      ci_fw <- ci_fw + 2L + w_hi_si
      if (has_hi_dec) {
        ci_fw <- ci_fw + 1L + w_hi_dec
      }
      ci_fw <- ci_fw + 1L
      # Pval part (token-aware)
      w_pv_pi <- maxw_si_or_tok(typed, "pv_prefix", "pv_int", "pv_token")
      w_pv_dec <- maxw("pv_dec")
      has_pv_dec <- has_field(typed, "pv_dec")
      pv_fw <- w_pv_pi
      if (has_pv_dec) {
        pv_fw <- pv_fw + 1L + w_pv_dec
      }
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
        w_pv_pi = w_pv_pi,
        w_pv_dec = w_pv_dec,
        has_pv_dec = has_pv_dec,
        gap = COMPOUND_GAP,
        full_width = ci_fw + COMPOUND_GAP + pv_fw
      )
    },

    n_pct_rate = {
      # n_pct part
      w_n <- maxw("n")
      w_pct_prefix <- maxw("pct_prefix")
      w_pct_int <- maxw("pct_int")
      w_pct_dec <- maxw("pct_dec")
      w_pct_sign <- maxw("pct_sign")
      has_dec <- has_field(typed, "pct_dec")
      npct_fw <- w_n + 2L + w_pct_prefix + w_pct_int
      if (has_dec) {
        npct_fw <- npct_fw + 1L + w_pct_dec
      }
      npct_fw <- npct_fw + w_pct_sign + 1L
      # rate part
      w_rate_si <- maxw_with_sign("rate_int", "rate_sign")
      w_rate_dec <- maxw("rate_dec")
      has_rate_dec <- has_field(typed, "rate_dec")
      rate_fw <- w_rate_si
      if (has_rate_dec) {
        rate_fw <- rate_fw + 1L + w_rate_dec
      }
      list(
        w_n = w_n,
        w_pct_prefix = w_pct_prefix,
        w_pct_int = w_pct_int,
        w_pct_dec = w_pct_dec,
        w_pct_sign = w_pct_sign,
        has_dec = has_dec,
        w_rate_si = w_rate_si,
        w_rate_dec = w_rate_dec,
        has_rate_dec = has_rate_dec,
        gap = COMPOUND_GAP,
        full_width = npct_fw + COMPOUND_GAP + rate_fw
      )
    },

    n_over_N_pct_ci = {
      # n/N (pct) part
      w_num <- maxw("num")
      w_den <- maxw("den")
      w_pct_prefix <- maxw("pct_prefix")
      w_pct_int <- maxw("pct_int")
      w_pct_dec <- maxw("pct_dec")
      w_pct_sign <- maxw("pct_sign")
      has_dec <- has_field(typed, "pct_dec")
      npct_fw <- w_num + 1L + w_den + 2L + w_pct_prefix + w_pct_int
      if (has_dec) {
        npct_fw <- npct_fw + 1L + w_pct_dec
      }
      npct_fw <- npct_fw + w_pct_sign + 1L
      # bracket CI part
      w_ci_lo_si <- maxw_with_sign("ci_lo_int", "ci_lo_sign")
      w_ci_lo_dec <- maxw("ci_lo_dec")
      w_ci_hi_si <- maxw_with_sign("ci_hi_int", "ci_hi_sign")
      w_ci_hi_dec <- maxw("ci_hi_dec")
      has_ci_lo_dec <- has_field(typed, "ci_lo_dec")
      has_ci_hi_dec <- has_field(typed, "ci_hi_dec")
      ci_fw <- 1L + w_ci_lo_si
      if (has_ci_lo_dec) {
        ci_fw <- ci_fw + 1L + w_ci_lo_dec
      }
      ci_fw <- ci_fw + 2L + w_ci_hi_si
      if (has_ci_hi_dec) {
        ci_fw <- ci_fw + 1L + w_ci_hi_dec
      }
      ci_fw <- ci_fw + 1L
      list(
        w_num = w_num,
        w_den = w_den,
        w_pct_prefix = w_pct_prefix,
        w_pct_int = w_pct_int,
        w_pct_dec = w_pct_dec,
        w_pct_sign = w_pct_sign,
        has_dec = has_dec,
        w_ci_lo_si = w_ci_lo_si,
        w_ci_lo_dec = w_ci_lo_dec,
        w_ci_hi_si = w_ci_hi_si,
        w_ci_hi_dec = w_ci_hi_dec,
        has_ci_lo_dec = has_ci_lo_dec,
        has_ci_hi_dec = has_ci_hi_dec,
        gap = 1L,
        full_width = npct_fw + 1L + ci_fw
      )
    },

    est_spread_pct_ci = {
      # est (spread%) part
      w_est_si <- maxw_with_sign("est_int", "est_sign")
      w_est_dec <- maxw("est_dec")
      w_sprd_si <- maxw_with_sign("sprd_int", "sprd_sign")
      w_sprd_dec <- maxw("sprd_dec")
      has_est_dec <- has_field(typed, "est_dec")
      has_sprd_dec <- has_field(typed, "sprd_dec")
      espct_fw <- w_est_si
      if (has_est_dec) {
        espct_fw <- espct_fw + 1L + w_est_dec
      }
      espct_fw <- espct_fw + 2L + w_sprd_si
      if (has_sprd_dec) {
        espct_fw <- espct_fw + 1L + w_sprd_dec
      }
      espct_fw <- espct_fw + 2L # "%)"
      # paren CI part
      w_ci_lo_si <- maxw_with_sign("ci_lo_int", "ci_lo_sign")
      w_ci_lo_dec <- maxw("ci_lo_dec")
      w_ci_hi_si <- maxw_with_sign("ci_hi_int", "ci_hi_sign")
      w_ci_hi_dec <- maxw("ci_hi_dec")
      has_ci_lo_dec <- has_field(typed, "ci_lo_dec")
      has_ci_hi_dec <- has_field(typed, "ci_hi_dec")
      ci_fw <- 1L + w_ci_lo_si
      if (has_ci_lo_dec) {
        ci_fw <- ci_fw + 1L + w_ci_lo_dec
      }
      ci_fw <- ci_fw + 2L + w_ci_hi_si
      if (has_ci_hi_dec) {
        ci_fw <- ci_fw + 1L + w_ci_hi_dec
      }
      ci_fw <- ci_fw + 1L
      list(
        w_est_si = w_est_si,
        w_est_dec = w_est_dec,
        w_sprd_si = w_sprd_si,
        w_sprd_dec = w_sprd_dec,
        has_est_dec = has_est_dec,
        has_sprd_dec = has_sprd_dec,
        w_ci_lo_si = w_ci_lo_si,
        w_ci_lo_dec = w_ci_lo_dec,
        w_ci_hi_si = w_ci_hi_si,
        w_ci_hi_dec = w_ci_hi_dec,
        has_ci_lo_dec = has_ci_lo_dec,
        has_ci_hi_dec = has_ci_hi_dec,
        gap = COMPOUND_GAP,
        full_width = espct_fw + COMPOUND_GAP + ci_fw
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
# 5. String Reconstruction
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

#' Build a percentage part: prefix + int [.dec] + sign
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

#' Pad a numeric-or-token position
#'
#' If token, right-justify in w_si and space-fill the decimal part.
#' If numeric, delegate to pad_float_part.
#' @noRd
pad_token_or_float <- function(token, sign, int, dec, w_si, w_dec, has_dec) {
  if (nzchar(token)) {
    padded <- stringi::stri_pad_left(token, w_si)
    if (isTRUE(has_dec)) {
      paste0(padded, strrep(" ", 1L + w_dec))
    } else {
      padded
    }
  } else {
    pad_float_part(sign, int, dec, w_si, w_dec, has_dec)
  }
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

  # Missing / unknown -> pad to full width
  if (parsed$type %in% c("missing", "unknown")) {
    return(strrep(" ", fw))
  }

  # If this value's type doesn't match dominant, try compatible alignment
  if (parsed$type != dominant_type) {
    # --- n_only fallbacks (all families) ---
    if (parsed$type == "n_only") {
      w <- switch(
        dominant_type,
        n_pct = ,
        n_pct_rate = widths$w_n,
        n_over_N_pct = ,
        n_over_N_pct_ci = ,
        n_over_N = ,
        n_over_float = widths$w_num,
        est_spread = ,
        est_spread_pct = ,
        est_ci = ,
        est_ci_bracket = ,
        est_ci_pval = ,
        est_spread_pct_ci = widths$w_est_si,
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
        dominant_type %in%
          c(
            "est_spread",
            "est_spread_pct",
            "est_ci",
            "est_ci_bracket",
            "est_ci_pval",
            "est_spread_pct_ci"
          )
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
        dominant_type %in%
          c(
            "est_spread",
            "est_spread_pct",
            "est_ci",
            "est_ci_bracket",
            "est_ci_pval",
            "est_spread_pct_ci"
          )
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

    # --- est_ci <-> est_ci_bracket fallback (swap delimiters) ---
    if (
      parsed$type == "est_ci" &&
        dominant_type == "est_ci_bracket" ||
        parsed$type == "est_ci_bracket" && dominant_type == "est_ci"
    ) {
      est <- pad_token_or_float(
        parsed$est_token,
        parsed$est_sign,
        parsed$est_int,
        parsed$est_dec,
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      lo <- pad_token_or_float(
        parsed$lo_token,
        parsed$lo_sign,
        parsed$lo_int,
        parsed$lo_dec,
        widths$w_lo_si,
        widths$w_lo_dec,
        widths$has_lo_dec
      )
      hi <- pad_token_or_float(
        parsed$hi_token,
        parsed$hi_sign,
        parsed$hi_int,
        parsed$hi_dec,
        widths$w_hi_si,
        widths$w_hi_dec,
        widths$has_hi_dec
      )
      if (dominant_type == "est_ci") {
        return(paste0(est, " (", lo, ", ", hi, ")"))
      } else {
        return(paste0(est, " [", lo, ", ", hi, "]"))
      }
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
    if (parsed$type == "est_spread" && dominant_type == "est_ci_bracket") {
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
      return(stringi::stri_pad_right(paste0(est, " [", sprd, "]"), fw))
    }

    # --- est_spread_pct <-> est_spread ---
    if (parsed$type == "est_spread_pct" && dominant_type == "est_spread") {
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
      return(stringi::stri_pad_right(paste0(est, " (", sprd, "%)"), fw))
    }
    if (parsed$type == "est_spread" && dominant_type == "est_spread_pct") {
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
      return(stringi::stri_pad_right(paste0(est, " (", sprd, " )"), fw))
    }

    # --- n_over_N in n_over_N_pct dominant ---
    if (parsed$type == "n_over_N" && dominant_type == "n_over_N_pct") {
      padded_num <- stringi::stri_pad_left(parsed$num, widths$w_num)
      padded_den <- stringi::stri_pad_right(parsed$den, widths$w_den)
      return(stringi::stri_pad_right(
        paste0(padded_num, "/", padded_den),
        fw
      ))
    }

    # --- range_pair fallbacks ---
    if (
      parsed$type == "range_pair" &&
        dominant_type %in%
          c("est_spread", "est_spread_pct", "est_ci", "est_ci_bracket")
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

  # === Same-type rebuild ===
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

    est_spread_pct = {
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
      paste0(est, " (", sprd, "%)")
    },

    est_ci = {
      est <- pad_token_or_float(
        parsed$est_token,
        parsed$est_sign,
        parsed$est_int,
        parsed$est_dec,
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      lo <- pad_token_or_float(
        parsed$lo_token,
        parsed$lo_sign,
        parsed$lo_int,
        parsed$lo_dec,
        widths$w_lo_si,
        widths$w_lo_dec,
        widths$has_lo_dec
      )
      hi <- pad_token_or_float(
        parsed$hi_token,
        parsed$hi_sign,
        parsed$hi_int,
        parsed$hi_dec,
        widths$w_hi_si,
        widths$w_hi_dec,
        widths$has_hi_dec
      )
      paste0(est, " (", lo, ", ", hi, ")")
    },

    est_ci_bracket = {
      est <- pad_token_or_float(
        parsed$est_token,
        parsed$est_sign,
        parsed$est_int,
        parsed$est_dec,
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      lo <- pad_token_or_float(
        parsed$lo_token,
        parsed$lo_sign,
        parsed$lo_int,
        parsed$lo_dec,
        widths$w_lo_si,
        widths$w_lo_dec,
        widths$has_lo_dec
      )
      hi <- pad_token_or_float(
        parsed$hi_token,
        parsed$hi_sign,
        parsed$hi_int,
        parsed$hi_dec,
        widths$w_hi_si,
        widths$w_hi_dec,
        widths$has_hi_dec
      )
      paste0(est, " [", lo, ", ", hi, "]")
    },

    n_over_N = {
      padded_num <- stringi::stri_pad_left(parsed$num, widths$w_num)
      padded_den <- stringi::stri_pad_right(parsed$den, widths$w_den)
      paste0(padded_num, "/", padded_den)
    },

    n_over_float = {
      padded_num <- stringi::stri_pad_left(parsed$num, widths$w_num)
      padded_di <- stringi::stri_pad_left(parsed$den_int, widths$w_den_int)
      padded_dd <- stringi::stri_pad_right(parsed$den_dec, widths$w_den_dec)
      paste0(padded_num, "/", padded_di, ".", padded_dd)
    },

    est_ci_pval = {
      # CI part
      est <- pad_token_or_float(
        parsed$est_token,
        parsed$est_sign,
        parsed$est_int,
        parsed$est_dec,
        widths$w_est_si,
        widths$w_est_dec,
        widths$has_est_dec
      )
      lo <- pad_token_or_float(
        parsed$lo_token,
        parsed$lo_sign,
        parsed$lo_int,
        parsed$lo_dec,
        widths$w_lo_si,
        widths$w_lo_dec,
        widths$has_lo_dec
      )
      hi <- pad_token_or_float(
        parsed$hi_token,
        parsed$hi_sign,
        parsed$hi_int,
        parsed$hi_dec,
        widths$w_hi_si,
        widths$w_hi_dec,
        widths$has_hi_dec
      )
      ci_str <- paste0(est, " (", lo, ", ", hi, ")")
      # Pval part
      pv <- pad_token_or_float(
        parsed$pv_token,
        parsed$pv_prefix,
        parsed$pv_int,
        parsed$pv_dec,
        widths$w_pv_pi,
        widths$w_pv_dec,
        widths$has_pv_dec
      )
      paste0(ci_str, strrep(" ", widths$gap), pv)
    },

    n_pct_rate = {
      # n_pct part
      padded_n <- stringi::stri_pad_left(parsed$n, widths$w_n)
      n_val <- suppressWarnings(as.integer(parsed$n))
      if (!is.na(n_val) && n_val == 0L) {
        npct_str <- stringi::stri_pad_right(padded_n, fw)
        return(npct_str)
      }
      pct <- pad_pct_part(
        parsed$pct_prefix,
        parsed$pct_int,
        parsed$pct_dec,
        parsed$pct_sign,
        widths
      )
      npct_str <- paste0(padded_n, " (", pct, ")")
      # rate part
      rate <- pad_float_part(
        parsed$rate_sign,
        parsed$rate_int,
        parsed$rate_dec,
        widths$w_rate_si,
        widths$w_rate_dec,
        widths$has_rate_dec
      )
      paste0(npct_str, strrep(" ", widths$gap), rate)
    },

    n_over_N_pct_ci = {
      # n/N (pct) part
      padded_num <- stringi::stri_pad_left(parsed$num, widths$w_num)
      padded_den <- stringi::stri_pad_right(parsed$den, widths$w_den)
      pct <- pad_pct_part(
        parsed$pct_prefix,
        parsed$pct_int,
        parsed$pct_dec,
        parsed$pct_sign,
        widths
      )
      npct_str <- paste0(padded_num, "/", padded_den, " (", pct, ")")
      # bracket CI part
      ci_lo <- pad_float_part(
        parsed$ci_lo_sign,
        parsed$ci_lo_int,
        parsed$ci_lo_dec,
        widths$w_ci_lo_si,
        widths$w_ci_lo_dec,
        widths$has_ci_lo_dec
      )
      ci_hi <- pad_float_part(
        parsed$ci_hi_sign,
        parsed$ci_hi_int,
        parsed$ci_hi_dec,
        widths$w_ci_hi_si,
        widths$w_ci_hi_dec,
        widths$has_ci_hi_dec
      )
      paste0(
        npct_str,
        strrep(" ", widths$gap),
        "[",
        ci_lo,
        ", ",
        ci_hi,
        "]"
      )
    },

    est_spread_pct_ci = {
      # est (spread%) part
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
      espct_str <- paste0(est, " (", sprd, "%)")
      # paren CI part
      ci_lo <- pad_float_part(
        parsed$ci_lo_sign,
        parsed$ci_lo_int,
        parsed$ci_lo_dec,
        widths$w_ci_lo_si,
        widths$w_ci_lo_dec,
        widths$has_ci_lo_dec
      )
      ci_hi <- pad_float_part(
        parsed$ci_hi_sign,
        parsed$ci_hi_int,
        parsed$ci_hi_dec,
        widths$w_ci_hi_si,
        widths$w_ci_hi_dec,
        widths$has_ci_hi_dec
      )
      paste0(
        espct_str,
        strrep(" ", widths$gap),
        "(",
        ci_lo,
        ", ",
        ci_hi,
        ")"
      )
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
# 6. Master Column Aligner
# ══════════════════════════════════════════════════════════════════════════════

#' Align all values in a decimal column via pre-formatted padding
#'
#' Pipeline: detect types -> find dominant type -> parse all -> compute widths ->
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

  # Parse all values (vectorized per-type batch)
  parsed_values <- parse_stat_values_batch(content_vec, types)

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
# 7. Integration with finalize_spec()
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
      widths_twips <- round(group_max_nc * space_twips)
      center_offset <- pmax(
        0L,
        round((col_width_twips - widths_twips) / 2)
      )
      max_width_twips <- round(max(group_max_nc) * space_twips)
    } else {
      formatted <- align_decimal_column(vals)

      # Compute scalar offset, replicate to vector for consistency
      if (length(formatted) > 0L && any(nzchar(formatted))) {
        max_chars <- max(nchar(formatted))
        max_width_twips <- round(max_chars * space_twips)
        scalar_offset <- max(
          0L,
          round((col_width_twips - max_width_twips) / 2)
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
