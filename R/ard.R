# ─────────────────────────────────────────────────────────────────────────────
# ard.R — Convert ARD (Analysis Results Data) to wide summary for fr_table()
#
# Preprocessing utility that bridges the cards/pharmaverse ARD format
# into arframe's wide summary format. No dependency on cards — accepts
# any data frame with the standard ARD columns.
#
# Handles all common input shapes:
#   - Raw ard_stack() output (list columns)
#   - After unlist_ard_columns() (atomic columns, same names)
#   - After rename_ard_columns(columns = all_ard_groups("names"))
#   - After rename_ard_columns() (default: groups + variables renamed)
#   - Multi-group .by = c(ARM, SEX) with extra group columns
#   - No .by at all (ungrouped)
#
# Usage:
#   cards::ard_stack(adsl, .by = "ARM", ...) |>
#     fr_wide_ard(statistic = ...) |>
#     fr_table() |> fr_cols(...) |> fr_render("out.pdf")
# ─────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# Normalization helpers — handle list and atomic columns uniformly
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
normalize_ard_chr <- function(col) {
  if (is.list(col)) {
    vapply(
      col,
      function(x) {
        if (is.null(x) || length(x) == 0L) {
          NA_character_
        } else {
          tryCatch(as.character(x[[1L]]), error = function(e) NA_character_)
        }
      },
      character(1L)
    )
  } else if (is.character(col)) {
    col
  } else if (is.factor(col)) {
    as.character(col)
  } else {
    as.character(col)
  }
}

#' @noRd
normalize_ard_num <- function(col) {
  if (is.list(col)) {
    vapply(
      col,
      function(s) {
        if (is.null(s) || length(s) == 0L) {
          NA_real_
        } else if (is.logical(s[[1L]])) {
          as.numeric(s[[1L]])
        } else {
          tryCatch(
            as.numeric(s[[1L]]),
            warning = function(w) {
              cli_warn(c(
                "Non-numeric value in ARD stat column.",
                "i" = "Value {.val {s[[1L]]}} coerced to {.val NA}."
              ), call = caller_env())
              NA_real_
            },
            error = function(e) NA_real_
          )
        }
      },
      numeric(1L)
    )
  } else if (is.numeric(col)) {
    as.double(col)
  } else if (is.logical(col)) {
    as.numeric(col)
  } else {
    suppressWarnings(as.numeric(col))
  }
}


# ══════════════════════════════════════════════════════════════════════════════
# detect_renamed_arm — find arm column in renamed-groups data
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
detect_renamed_arm <- function(df, column, call) {
  # Find non-standard columns (potential group columns)
  computed_cols <- c("arm", "var_level", "stat_val", "stat_chr", "ctx")
  non_std <- setdiff(names(df), c(fr_env$ard_standard_cols, computed_cols))

  # Exclude columns whose name appears in df$variable (hierarchical variable cols)
  if ("variable" %in% names(df)) {
    var_names <- unique(df$variable)
    non_std <- setdiff(non_std, var_names)
  }

  if (length(non_std) == 0L) {
    return(NULL)
  }

  if (!is.null(column)) {
    if (!column %in% non_std) {
      # column might match a value in a non-standard col (raw ARD style)
      return(NULL)
    }
    arm_col <- column
    extra_cols <- setdiff(non_std, arm_col)
  } else if (length(non_std) == 1L) {
    arm_col <- non_std
    extra_cols <- character(0L)
  } else {
    cli_abort(
      c(
        "Multiple potential group columns found: {.val {non_std}}.",
        "i" = "Specify {.arg column} to identify the treatment arm column.",
        "i" = "Example: {.code fr_wide_ard(data, column = \"{non_std[1L]}\")}"
      ),
      call = call
    )
  }

  list(col_name = arm_col, extra_cols = extra_cols)
}


# ══════════════════════════════════════════════════════════════════════════════
# reconstruct_renamed_ard — reverse-engineer fully-renamed ARD
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
reconstruct_renamed_ard <- function(df, column, call) {
  std_cols <- intersect(
    names(df),
    c(
      "context",
      "stat_name",
      "stat_label",
      "stat",
      "fmt_fun",
      "warning",
      "error",
      "stat_fmt"
    )
  )
  non_std <- setdiff(names(df), std_cols)

  if (is.null(column)) {
    cli_abort(
      c(
        "Cannot auto-detect treatment arm column from fully-renamed ARD.",
        "i" = "Specify {.arg column} explicitly.",
        "i" = "Non-standard columns found: {.val {non_std}}."
      ),
      call = call
    )
  }

  if (!column %in% names(df)) {
    cli_abort(
      c(
        "Column {.val {column}} not found in data.",
        "i" = "Available columns: {.val {names(df)}}."
      ),
      call = call
    )
  }

  arm_col <- column
  var_cols <- setdiff(non_std, arm_col)

  # Reconstruct variable and variable_level
  n <- nrow(df)
  variable <- rep(NA_character_, n)
  var_level <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    for (vc in var_cols) {
      val <- df[[vc]][i]
      if (!is.na(val) && nchar(as.character(val)) > 0L) {
        variable[i] <- vc
        var_level[i] <- as.character(val)
        break
      }
    }
  }

  # All-NA rows are continuous variables — assign to always-NA var_cols
  unknown_rows <- is.na(variable)
  if (any(unknown_rows) && length(var_cols) > 0L) {
    always_na <- var_cols[vapply(
      var_cols,
      function(vc) {
        all(is.na(df[[vc]]))
      },
      logical(1L)
    )]
    if (length(always_na) >= 1L) {
      variable[unknown_rows] <- always_na[1L]
    } else {
      # Best effort: find var_cols not used by any categorical row
      used_vars <- unique(variable[!is.na(variable)])
      unused <- setdiff(var_cols, used_vars)
      if (length(unused) >= 1L) {
        variable[unknown_rows] <- unused[1L]
      }
    }
  }

  df$variable <- variable
  df$variable_level <- var_level

  list(df = df, column = arm_col, extra_groups = character(0L))
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_wide_ard — Convert long ARD to wide summary data frame
# ══════════════════════════════════════════════════════════════════════════════

#' Convert ARD (Long) to Wide Summary Data Frame
#'
#' @description
#'
#' Converts Analysis Results Data (ARD) from packages like
#' [cards](https://insightsengineering.github.io/cards/) into a wide summary
#' data frame suitable for [fr_table()]. ARD stores one row per statistic;
#' `fr_wide_ard()` combines statistics using format strings, pivots treatment
#' arms to columns, and returns a display-ready data frame.
#'
#' This function has **no dependency on cards** — it accepts any data frame
#' with the standard ARD columns (`variable`, `stat_name`, `stat`).
#'
#' Handles all common post-`ard_stack()` pipeline shapes:
#' - Raw `ard_stack()` output (list columns)
#' - After `unlist_ard_columns()` (atomic columns)
#' - After `rename_ard_columns(columns = all_ard_groups("names"))` (group
#'   columns renamed to e.g. `ARM`)
#' - After default `rename_ard_columns()` (both groups and variables renamed)
#' - Multi-group `.by = c(ARM, SEX)` — extra groups preserved in output
#' - No `.by` at all (ungrouped)
#'
#' @param data A data frame with ARD columns. At minimum, requires
#'   `stat_name` and `stat`. Standard ARD also has `variable`,
#'   `group1`/`group1_level`, `variable_level`, and `context`.
#'   Renamed ARD (after `rename_ard_columns()`) is also accepted.
#'   Typically produced by `cards::ard_stack()`.
#'
#' @param statistic Format strings for combining statistics into display
#'   cells. Accepts three forms:
#'
#'   **Form 1: Named list by context** (most common)
#'   ```r
#'   list(
#'     categorical = "{n} ({p}%)",
#'     continuous  = c("Mean (SD)" = "{mean} ({sd})", Median = "{median}")
#'   )
#'   ```
#'   Categorical: one format string produces one row per category level.
#'   Continuous: a named character vector produces one row per entry, with
#'   names becoming row labels (e.g., "Mean (SD)", "Median").
#'
#'   **Form 2: Named list by variable** (per-variable override)
#'   ```r
#'   list(
#'     AGE = c(n = "{N}", "Mean (SD)" = "{mean} ({sd})"),
#'     SEX = "{n} ({p}%)",
#'     default = "{n} ({p}%)"
#'   )
#'   ```
#'
#'   **Form 3: Single string** (applied to everything)
#'   ```r
#'   "{n} ({p}%)"
#'   ```
#'
#'   Default: `list(continuous = "{mean} ({sd})", categorical = "{n} ({p}%)")`.
#'
#' @param column Character scalar. The grouping variable that becomes the
#'   display columns (e.g., `"ARM"`, `"DOSGRP"`).
#'   For raw ARD, this is the value in `group1` (auto-detected if `NULL`).
#'   For renamed ARD, this is the column name in the data frame.
#'   If `NULL` (default), auto-detected from the data.
#'
#' @param label Named character vector mapping variable names to display
#'   labels. E.g., `c(AGE = "Age (years)", SEX = "Sex, n (%)")`.
#'   For hierarchical ARD, also renames sentinel variables:
#'   `c("..ard_hierarchical_overall.." = "Patients with Any TEAE")`.
#'   If `NULL`, uses `variable` values as-is.
#'
#' @param overall Character scalar. Column header for overall/total rows
#'   (where `group1` is `NA`). Default `"Total"`. Set to `NULL` to exclude
#'   overall rows.
#'
#' @param decimals Decimal precision for stat formatting. Accepts two forms:
#'
#'   **Global defaults** — a named integer vector:
#'   ```r
#'   c(mean = 1, sd = 2, p = 0, median = 1)
#'   ```
#'
#'   **Per-variable overrides** — a named list:
#'   ```r
#'   list(
#'     AGE  = c(mean = 1, sd = 2),
#'     BMIBL = c(mean = 2, sd = 3),
#'     .default = c(mean = 1, sd = 2, p = 0)
#'   )
#'   ```
#'
#'   Overrides built-in defaults. Default: `NULL`.
#'
#' @param fmt Named list of custom format functions keyed by `stat_name`.
#'   Each function takes a numeric value and returns a character string.
#'   E.g., `list(p.value = function(x) if (x < 0.001) "<0.001" else
#'   sprintf("%.3f", x))`. Default: `NULL`.
#'
#' @param big_n Character scalar. The `stat_name` value used for population
#'   counts (BigN) in the ARD (e.g., `"BigN"`, `"bigN"`, `"popn"`).
#'   When specified, rows with this `stat_name` are extracted from the body
#'   and attached as `attr(result, "n_counts")` — a named numeric vector
#'   suitable for `fr_cols(.n =)`. Default: `NULL` (no extraction).
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{Extra group columns (if multi-group)}{e.g., `SEX`, `PARAMCD`}
#'     \item{`variable`}{Grouping variable (for use with `fr_rows(group_by =)`)}
#'     \item{`stat_label`}{Row label (statistic name or category level)}
#'     \item{One column per treatment arm}{Formatted display values}
#'     \item{`Total` (if overall present)}{Overall column}
#'   }
#'
#'   For hierarchical ARD (SOC/PT), additional columns:
#'   \describe{
#'     \item{`soc`}{System organ class value}
#'     \item{`pt`}{Preferred term value}
#'     \item{`row_type`}{`"soc"` or `"pt"`}
#'   }
#'
#'   If `big_n` is specified, `attr(result, "n_counts")` contains a named
#'   numeric vector of population counts per arm.
#'
#'   Pipe directly to `fr_table()`:
#'   ```r
#'   ard |> fr_wide_ard() |> fr_table() |> fr_cols(...) |> fr_render("out.pdf")
#'   ```
#'
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- cards::ard_stack(
#'     data = arframe::adsl[arframe::adsl$SAFFL == "Y", ],
#'     .by = "ARM",
#'     cards::ard_continuous(variables = "AGE"),
#'     cards::ard_categorical(variables = "SEX"),
#'     .overall = TRUE
#'   )
#'
#'   # --- Scenario 1: Multi-row continuous output ---
#'   wide <- fr_wide_ard(ard,
#'     statistic = list(
#'       continuous = c(
#'         n           = "{N}",
#'         "Mean (SD)" = "{mean} ({sd})",
#'         Median      = "{median}",
#'         "Min, Max"  = "{min}, {max}"
#'       ),
#'       categorical = "{n} ({p}%)"
#'     ),
#'     label = c(AGE = "Age (years)", SEX = "Sex"),
#'     decimals = c(mean = 1, sd = 2, p = 0)
#'   )
#'
#'   print(wide)
#'
#'   # --- Scenario 2: Per-variable decimals ---
#'   wide2 <- fr_wide_ard(ard,
#'     statistic = list(
#'       continuous = c("Mean (SD)" = "{mean} ({sd})"),
#'       categorical = "{n} ({p}%)"
#'     ),
#'     decimals = list(AGE = c(mean = 2, sd = 3), .default = c(p = 1))
#'   )
#'
#'   # --- Scenario 3: Single string statistic ---
#'   cat_ard <- cards::ard_stack(
#'     data = arframe::adsl[arframe::adsl$SAFFL == "Y", ],
#'     .by = "ARM",
#'     cards::ard_categorical(variables = "SEX")
#'   )
#'   wide3 <- fr_wide_ard(cat_ard, statistic = "{n} ({p}%)")
#'
#'   # --- Scenario 4: Pipe into arframe ---
#'   wide |>
#'     fr_table() |>
#'     fr_cols(
#'       variable   = fr_col(visible = FALSE),
#'       stat_label = fr_col("", width = 2.5),
#'       .align = "decimal"
#'     ) |>
#'     fr_rows(group_by = list(cols = "variable", label = "stat_label"))
#' }
#'
#' @seealso [fr_table()] for the pipeline entry point.
#'
#' @export
fr_wide_ard <- function(
  data,
  statistic = list(
    continuous = "{mean} ({sd})",
    categorical = "{n} ({p}%)"
  ),
  column = NULL,
  label = NULL,
  overall = "Total",
  decimals = NULL,
  fmt = NULL,
  big_n = NULL
) {
  call <- caller_env()

  # ── Validate inputs ──────────────────────────────────────────────────────
  if (!is.data.frame(data)) {
    cli_abort(
      c(
        "{.arg data} must be a data frame.",
        "x" = "You supplied {.obj_type_friendly {data}}."
      ),
      call = call
    )
  }

  # Normalize statistic to a named list
  if (is.character(statistic) && length(statistic) == 1L) {
    statistic <- list(continuous = statistic, categorical = statistic)
  }
  if (!is.list(statistic)) {
    cli_abort(
      c(
        "{.arg statistic} must be a named list or a single string.",
        "x" = "You supplied {.obj_type_friendly {statistic}}.",
        "i" = "See {.help fr_wide_ard} for format string examples."
      ),
      call = call
    )
  }

  if (!is.null(fmt) && !is.list(fmt)) {
    cli_abort(
      c(
        "{.arg fmt} must be a named list of functions or {.val NULL}.",
        "x" = "You supplied {.obj_type_friendly {fmt}}."
      ),
      call = call
    )
  }

  if (!is.null(big_n)) {
    check_scalar_chr(big_n, call = call)
  }

  df <- as.data.frame(data, stringsAsFactors = FALSE)
  extra_group_cols <- character(0L)

  # ── Detect input shape and normalize ─────────────────────────────────────
  has_variable <- "variable" %in% names(df)
  has_stat_name <- "stat_name" %in% names(df)
  has_group1_level <- "group1_level" %in% names(df)
  has_group1 <- "group1" %in% names(df)

  if (!has_stat_name || !"stat" %in% names(df)) {
    cli_abort(
      c(
        "ARD data is missing required columns: {.val stat_name} and/or {.val stat}.",
        "i" = "Typically produced by {.fn cards::ard_stack}."
      ),
      call = call
    )
  }

  if (!has_variable && has_stat_name) {
    # ── Shape D: Fully-renamed ARD (no variable column) ──────────────────
    reconstructed <- reconstruct_renamed_ard(df, column, call)
    df <- reconstructed$df
    column_name <- reconstructed$column
    extra_group_cols <- reconstructed$extra_groups
    # Set arm from the column
    df$arm <- as.character(df[[column_name]])
    # Normalize stat
    df$stat_val <- normalize_ard_num(df$stat)
    df$stat_chr <- normalize_ard_chr(df$stat)
    # Normalize var_level
    df$var_level <- normalize_ard_chr(df$variable_level)
    # column refers to the renamed column name
    # Don't filter by group1 since it doesn't exist
  } else if (has_variable && has_group1_level) {
    # ── Shape A: Raw or unlisted ARD (standard columns present) ──────────
    # Normalize stat column (handles both list and atomic)
    df$stat_val <- normalize_ard_num(df$stat)
    df$stat_chr <- normalize_ard_chr(df$stat)

    # Normalize variable_level → var_level
    if ("variable_level" %in% names(df)) {
      df$var_level <- normalize_ard_chr(df$variable_level)
    } else {
      df$var_level <- NA_character_
    }

    # Normalize group1 if list
    if (has_group1 && is.list(df$group1)) {
      df$group1 <- normalize_ard_chr(df$group1)
    }

    # ── Multi-group detection ──────────────────────────────────────────────
    # Find all groupN/groupN_level pairs
    group_pairs <- list()
    for (i in seq_len(6L)) {
      gcol <- paste0("group", i)
      glcol <- paste0("group", i, "_level")
      if (gcol %in% names(df) && glcol %in% names(df)) {
        group_pairs[[i]] <- list(name_col = gcol, level_col = glcol)
      } else {
        break
      }
    }

    # Find which group matches `column`
    arm_group_idx <- 1L
    if (!is.null(column) && length(group_pairs) > 0L) {
      for (gi in seq_along(group_pairs)) {
        gp <- group_pairs[[gi]]
        gvals <- normalize_ard_chr(df[[gp$name_col]])
        if (column %in% gvals) {
          arm_group_idx <- gi
          break
        }
      }
    }

    # Extract arm from the matched group
    if (length(group_pairs) >= arm_group_idx) {
      gp <- group_pairs[[arm_group_idx]]
      df$arm <- normalize_ard_chr(df[[gp$level_col]])
      if (is.null(column)) {
        col_vals <- normalize_ard_chr(df[[gp$name_col]])
        column <- col_vals[!is.na(col_vals)][1L]
      }
    } else {
      df$arm <- NA_character_
    }

    # Extract extra group columns
    for (gi in seq_along(group_pairs)) {
      if (gi == arm_group_idx) {
        next
      }
      gp <- group_pairs[[gi]]
      group_var_names <- normalize_ard_chr(df[[gp$name_col]])
      group_var_name <- unique(group_var_names[!is.na(group_var_names)])
      if (length(group_var_name) == 1L) {
        df[[group_var_name]] <- normalize_ard_chr(df[[gp$level_col]])
        extra_group_cols <- c(extra_group_cols, group_var_name)
      }
    }
  } else if (has_variable && !has_group1_level) {
    # ── Shape B: Groups-renamed, or Shape C: No groups ───────────────────
    # Normalize stat
    df$stat_val <- normalize_ard_num(df$stat)
    df$stat_chr <- normalize_ard_chr(df$stat)

    # Normalize variable_level → var_level
    if ("variable_level" %in% names(df)) {
      df$var_level <- normalize_ard_chr(df$variable_level)
    } else {
      df$var_level <- NA_character_
    }

    arm_info <- detect_renamed_arm(df, column, call)
    if (!is.null(arm_info)) {
      # Shape B: groups were renamed
      df$arm <- as.character(df[[arm_info$col_name]])
      column <- arm_info$col_name
      extra_group_cols <- arm_info$extra_cols
    } else if (!has_group1) {
      # Shape C: No groups at all
      df$arm <- NA_character_
      # Force overall to capture all rows in a single column
      if (is.null(overall)) {
        overall <- "value"
      }
    } else {
      # Has group1 but no group1_level — unusual but handle gracefully
      df$arm <- NA_character_
    }
  } else {
    cli_abort(
      c(
        "Cannot determine ARD structure from the provided data.",
        "i" = "Expected columns: {.val variable}, {.val stat_name}, {.val stat}.",
        "i" = "Typically produced by {.fn cards::ard_stack}."
      ),
      call = call
    )
  }

  # ── Detect hierarchical ARD (SOC/PT) ────────────────────────────────────
  hierarchy <- detect_ard_hierarchy(df)

  # ── Auto-detect column from group1 (raw ARD only) ──────────────────────
  if (is.null(column) && has_group1 && "group1" %in% names(df)) {
    grp_col <- if (hierarchy$is_hierarchical) {
      hierarchy$column_group
    } else {
      "group1"
    }
    if (!is.null(grp_col) && grp_col %in% names(df)) {
      grp1_vals <- df[[grp_col]][!is.na(df[[grp_col]])]
      if (length(grp1_vals) > 0L) {
        column <- grp1_vals[[1L]]
      }
    }
  }

  # ── Fix arm for hierarchical overall rows ────────────────────────────────
  # In hierarchical ARDs with overall = TRUE, PT-level total rows have

  # group1 = <parent_var> (e.g., AEBODSYS) instead of the by-variable
  # (e.g., TRT01A). Their arm was set to the parent level value — correct
  # it to NA so the overall-labelling step converts them to "Total".
  if (
    hierarchy$is_hierarchical && !is.null(column) && "group1" %in% names(df)
  ) {
    g1_names <- if (is.list(df$group1)) {
      normalize_ard_chr(df$group1)
    } else {
      df$group1
    }
    is_overall_child <- !is.na(g1_names) & g1_names != column
    df$arm[is_overall_child] <- NA_character_
  }

  # ── Determine context per row ────────────────────────────────────────────
  if ("context" %in% names(df)) {
    df$ctx <- as.character(df$context)
  } else {
    df$ctx <- ifelse(is.na(df$var_level), "continuous", "categorical")
  }

  # ── Extract BigN rows BEFORE internal row filtering ────────────────────
  # BigN rows often have variable == column (e.g., variable = "ARM"),
  # which would be removed by the internal row filter below.
  n_counts <- NULL
  if (!is.null(big_n)) {
    bign_mask <- df$stat_name == big_n
    if (any(bign_mask)) {
      bign_df <- df[bign_mask, , drop = FALSE]
      bign_vals <- bign_df$stat_val
      bign_names <- bign_df$arm
      keep <- !is.na(bign_names) & !duplicated(bign_names)
      n_counts <- stats::setNames(bign_vals[keep], bign_names[keep])
      df <- df[!bign_mask, , drop = FALSE]
    }
  }

  # ── Filter internal rows ────────────────────────────────────────────────
  # Keep ..ard_hierarchical_overall.. (real display row), filter others
  internal_rows <- (grepl("^\\.\\.", df$variable) &
    !(df$variable %in% fr_env$ard_keep_sentinels)) |
    (df$ctx %in% fr_env$ard_internal_contexts)

  if (!is.null(column)) {
    internal_rows <- internal_rows | df$variable == column
  }
  df <- df[!internal_rows, , drop = FALSE]

  if (nrow(df) == 0L) {
    cli_abort(
      c(
        "No displayable rows remain after filtering internal ARD rows.",
        "i" = "Check that {.arg data} contains actual analysis results."
      ),
      call = call
    )
  }

  # ── Label overall rows ──────────────────────────────────────────────────
  if (!is.null(overall)) {
    df$arm <- ifelse(is.na(df$arm), overall, df$arm)
  } else {
    df <- df[!is.na(df$arm), , drop = FALSE]
  }

  # ── Filter to the target column group (raw ARD with group1) ────────────
  if (!is.null(column) && "group1" %in% names(df)) {
    g1 <- if (is.list(df$group1)) normalize_ard_chr(df$group1) else df$group1
    # Only filter if column matches group1 values.
    # Keep overall rows (arm labelled as the overall value) — these are
    # hierarchical total rows whose group1 is the parent variable.
    if (column %in% g1) {
      is_overall_row <- if (!is.null(overall)) df$arm == overall else FALSE
      df <- df[is.na(g1) | g1 == column | is_overall_row, , drop = FALSE]
    }
  }

  if (nrow(df) == 0L) {
    cli_abort(
      c(
        "No rows remain after filtering.",
        "i" = "Check {.arg column} and {.arg overall} settings."
      ),
      call = call
    )
  }

  # ── Resolve decimals per variable ───────────────────────────────────────
  decimals_resolved <- resolve_ard_decimals(decimals)

  # ── Format stat values ──────────────────────────────────────────────────
  df$stat_fmt <- vapply(
    seq_len(nrow(df)),
    function(i) {
      format_ard_stat(
        df$stat_val[i],
        df$stat_chr[i],
        df$stat_name[i],
        decimals_resolved,
        df$variable[i],
        fmt
      )
    },
    character(1L)
  )

  # ── Handle hierarchical vs flat output ──────────────────────────────────
  if (hierarchy$is_hierarchical) {
    wide <- build_hierarchical_wide(df, statistic, hierarchy, call)
  } else {
    wide <- build_flat_wide(df, statistic, extra_group_cols, call)
  }

  # Arm levels for column ordering
  arm_levels <- unique(df$arm[!is.na(df$arm)])

  # Replace NA cells with empty string
  for (a in arm_levels) {
    if (a %in% names(wide)) {
      wide[[a]][is.na(wide[[a]])] <- ""
    }
  }

  # ── Apply display labels ────────────────────────────────────────────────
  if (!is.null(label)) {
    if ("variable" %in% names(wide)) {
      for (v in names(label)) {
        wide$variable[wide$variable == v] <- label[[v]]
      }
    }
    # Also apply labels to soc and pt columns (hierarchical output)
    if ("soc" %in% names(wide)) {
      for (v in names(label)) {
        wide$soc[wide$soc == v] <- label[[v]]
      }
    }
    if ("pt" %in% names(wide)) {
      for (v in names(label)) {
        wide$pt[wide$pt == v] <- label[[v]]
      }
    }
  }

  # ── Indent stat_label for non-group rows (flat output only) ─────────────
  if (!hierarchy$is_hierarchical && "stat_label" %in% names(wide)) {
    is_level_row <- wide$stat_label != wide$variable
    wide$stat_label[is_level_row] <- paste0(
      "  ",
      wide$stat_label[is_level_row]
    )
  }

  # ── Attach BigN attribute ──────────────────────────────────────────────
  if (!is.null(n_counts)) {
    attr(wide, "n_counts") <- n_counts
  }

  rownames(wide) <- NULL
  wide
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: build flat (non-hierarchical) wide output
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
build_flat_wide <- function(
  df,
  statistic,
  extra_group_cols = character(0L),
  call = NULL
) {
  arm_levels <- unique(df$arm[!is.na(df$arm)])

  # Create composite key for variable + extra groups
  if (length(extra_group_cols) > 0L) {
    eg_vals <- lapply(extra_group_cols, function(ec) {
      as.character(df[[ec]])
    })
    df$var_group_key <- do.call(
      paste,
      c(list(df$variable), eg_vals, list(sep = "\x1f"))
    )
  } else {
    df$var_group_key <- df$variable
  }

  key_order <- unique(df$var_group_key)
  result_rows <- list()
  idx <- 0L

  for (key in key_order) {
    key_df <- df[df$var_group_key == key, , drop = FALSE]
    if (nrow(key_df) == 0L) {
      next
    }

    var_name <- key_df$variable[1L]
    ctx <- key_df$ctx[1L]
    fmt_spec <- resolve_ard_statistic(var_name, ctx, statistic)

    # Validate format references against available stats for this variable
    available <- unique(key_df$stat_name)
    fmt_strs <- if (is_multirow_spec(fmt_spec)) fmt_spec else fmt_spec
    for (fs in fmt_strs) {
      validate_format_stats(fs, available, var_name, call)
    }

    # Capture extra group values for this key
    eg_values <- list()
    for (ec in extra_group_cols) {
      eg_values[[ec]] <- as.character(key_df[[ec]][1L])
    }

    if (is_multirow_spec(fmt_spec)) {
      for (row_label in names(fmt_spec)) {
        fmt_str <- fmt_spec[[row_label]]
        for (arm_val in arm_levels) {
          cell <- interpolate_stats(key_df, arm_val, fmt_str)
          idx <- idx + 1L
          row <- data.frame(
            variable = var_name,
            stat_label = row_label,
            arm = arm_val,
            cell_text = cell,
            stringsAsFactors = FALSE
          )
          for (ec in extra_group_cols) {
            row[[ec]] <- eg_values[[ec]]
          }
          result_rows[[idx]] <- row
        }
      }
    } else {
      fmt_str <- fmt_spec
      levels <- unique(key_df$var_level)

      if (all(is.na(levels))) {
        for (arm_val in arm_levels) {
          cell <- interpolate_stats(key_df, arm_val, fmt_str)
          idx <- idx + 1L
          row <- data.frame(
            variable = var_name,
            stat_label = var_name,
            arm = arm_val,
            cell_text = cell,
            stringsAsFactors = FALSE
          )
          for (ec in extra_group_cols) {
            row[[ec]] <- eg_values[[ec]]
          }
          result_rows[[idx]] <- row
        }
      } else {
        levels <- levels[!is.na(levels)]
        for (lvl in levels) {
          for (arm_val in arm_levels) {
            lvl_df <- key_df[
              !is.na(key_df$var_level) & key_df$var_level == lvl,
              ,
              drop = FALSE
            ]
            cell <- interpolate_stats(lvl_df, arm_val, fmt_str)
            idx <- idx + 1L
            row <- data.frame(
              variable = var_name,
              stat_label = lvl,
              arm = arm_val,
              cell_text = cell,
              stringsAsFactors = FALSE
            )
            for (ec in extra_group_cols) {
              row[[ec]] <- eg_values[[ec]]
            }
            result_rows[[idx]] <- row
          }
        }
      }
    }
  }

  if (idx == 0L) {
    return(data.frame(
      variable = character(0L),
      stat_label = character(0L),
      stringsAsFactors = FALSE
    ))
  }

  long_df <- do.call(rbind, result_rows[seq_len(idx)])
  pivot_ard_wide(long_df, arm_levels, extra_group_cols)
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: build hierarchical (SOC/PT) wide output
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
build_hierarchical_wide <- function(df, statistic, hierarchy, call) {
  arm_levels <- unique(df$arm[!is.na(df$arm)])
  soc_col <- hierarchy$soc_col
  pt_col <- hierarchy$pt_col

  # Extract SOC-level values (handles both list and atomic)
  if (soc_col %in% names(df)) {
    df$soc_val <- normalize_ard_chr(df[[soc_col]])
    # For overall PT rows (arm = NA), group2 is absent but the SOC value
    # lives in group1_level. Fill in missing soc_val from group1_level.
    if ("group1_level" %in% names(df)) {
      g1_val <- normalize_ard_chr(df$group1_level)
      missing_soc <- is.na(df$soc_val) & !is.na(g1_val)
      df$soc_val[missing_soc] <- g1_val[missing_soc]
    }
  } else {
    df$soc_val <- NA_character_
  }

  soc_var <- hierarchy$soc_var
  pt_var <- hierarchy$pt_var

  result_rows <- list()
  idx <- 0L

  # ── Process ..ard_hierarchical_overall.. sentinel FIRST ──────────────────
  overall_df <- df[
    df$variable == "..ard_hierarchical_overall..",
    ,
    drop = FALSE
  ]
  if (nrow(overall_df) > 0L) {
    fmt_str <- resolve_ard_statistic(
      "..ard_hierarchical_overall..",
      "categorical",
      statistic
    )
    if (is_multirow_spec(fmt_str)) {
      fmt_str <- fmt_str[[1L]]
    }
    validate_format_stats(
      fmt_str,
      unique(overall_df$stat_name),
      "..ard_hierarchical_overall..",
      call
    )

    for (arm_val in arm_levels) {
      cell <- interpolate_stats(overall_df, arm_val, fmt_str)
      idx <- idx + 1L
      result_rows[[idx]] <- data.frame(
        soc = "..ard_hierarchical_overall..",
        pt = "..ard_hierarchical_overall..",
        row_type = "overall",
        arm = arm_val,
        cell_text = cell,
        stringsAsFactors = FALSE
      )
    }
  }

  # ── Process SOC/PT rows ────────────────────────────────────────────────
  soc_df <- df[df$variable == soc_var, , drop = FALSE]
  soc_levels <- unique(soc_df$var_level[!is.na(soc_df$var_level)])
  pt_df <- df[df$variable == pt_var, , drop = FALSE]

  # Resolve and validate format strings once (invariant across SOC levels)
  fmt_str <- resolve_ard_statistic(soc_var, "categorical", statistic)
  if (is_multirow_spec(fmt_str)) {
    fmt_str <- fmt_str[[1L]]
  }
  validate_format_stats(
    fmt_str,
    unique(soc_df$stat_name),
    soc_var,
    call
  )

  fmt_str_pt <- resolve_ard_statistic(pt_var, "categorical", statistic)
  if (is_multirow_spec(fmt_str_pt)) {
    fmt_str_pt <- fmt_str_pt[[1L]]
  }
  validate_format_stats(
    fmt_str_pt,
    unique(pt_df$stat_name),
    pt_var,
    call
  )

  for (soc in soc_levels) {
    soc_subset <- soc_df[
      !is.na(soc_df$var_level) & soc_df$var_level == soc,
      ,
      drop = FALSE
    ]

    for (arm_val in arm_levels) {
      cell <- interpolate_stats(soc_subset, arm_val, fmt_str)
      idx <- idx + 1L
      result_rows[[idx]] <- data.frame(
        soc = soc,
        pt = soc,
        row_type = "soc",
        arm = arm_val,
        cell_text = cell,
        stringsAsFactors = FALSE
      )
    }

    # PT rows under this SOC
    if (!is.null(soc_col) && soc_col %in% names(pt_df)) {
      pt_under_soc <- pt_df[
        !is.na(pt_df$soc_val) & pt_df$soc_val == soc,
        ,
        drop = FALSE
      ]
    } else {
      pt_under_soc <- pt_df
    }

    pt_levels <- unique(pt_under_soc$var_level[!is.na(pt_under_soc$var_level)])

    for (pt in pt_levels) {
      pt_subset <- pt_under_soc[
        !is.na(pt_under_soc$var_level) & pt_under_soc$var_level == pt,
        ,
        drop = FALSE
      ]
      for (arm_val in arm_levels) {
        cell <- interpolate_stats(pt_subset, arm_val, fmt_str_pt)
        idx <- idx + 1L
        result_rows[[idx]] <- data.frame(
          soc = soc,
          pt = pt,
          row_type = "pt",
          arm = arm_val,
          cell_text = cell,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (idx == 0L) {
    return(data.frame(
      soc = character(0L),
      pt = character(0L),
      row_type = character(0L),
      stringsAsFactors = FALSE
    ))
  }

  long_df <- do.call(rbind, result_rows[seq_len(idx)])

  # Build row key for pivoting
  long_df$row_key <- paste(
    long_df$soc,
    long_df$pt,
    long_df$row_type,
    sep = "\x1f"
  )

  # Pivot: one row per unique (soc, pt, row_type)
  row_keys <- unique(long_df$row_key)
  wide <- long_df[
    match(row_keys, long_df$row_key),
    c("soc", "pt", "row_type"),
    drop = FALSE
  ]

  for (a in arm_levels) {
    arm_df <- long_df[long_df$arm == a, , drop = FALSE]
    wide[[a]] <- arm_df$cell_text[match(row_keys, arm_df$row_key)]
  }

  rownames(wide) <- NULL
  wide
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: detect hierarchical ARD structure
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
detect_ard_hierarchy <- function(df) {
  result <- list(
    is_hierarchical = FALSE,
    soc_col = NULL,
    pt_col = NULL,
    soc_var = NULL,
    pt_var = NULL,
    column_group = "group1"
  )

  if (!all(c("group1", "variable") %in% names(df))) {
    return(result)
  }

  has_group2 <- "group2" %in% names(df) && "group2_level" %in% names(df)

  if (has_group2) {
    group2_vals <- unique(normalize_ard_chr(df$group2)[
      !is.na(normalize_ard_chr(df$group2))
    ])
    group1_vals <- unique(normalize_ard_chr(df$group1)[
      !is.na(normalize_ard_chr(df$group1))
    ])
    var_vals <- unique(df$variable)

    if (length(group2_vals) > 0L && length(var_vals) > 0L) {
      # Hierarchical: group2 is a variable name (e.g., AEBODSYS) that ALSO
      # appears in the variable column. Multi-group (e.g., SEX) does NOT
      # appear as a variable.
      if (all(group2_vals %in% var_vals)) {
        result$is_hierarchical <- TRUE
        result$soc_col <- "group2_level"
        result$soc_var <- group2_vals[[1L]]
        result$pt_var <- setdiff(
          var_vals,
          c(
            group2_vals,
            group1_vals,
            "..ard_hierarchical_overall.."
          )
        )
        if (length(result$pt_var) > 0L) {
          result$pt_var <- result$pt_var[[1L]]
        } else {
          result$pt_var <- var_vals[[1L]]
        }
        result$column_group <- "group1"
      }
    }
  }

  result
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: resolve format string for a variable
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
resolve_ard_statistic <- function(var_name, context, statistic) {
  statistic[[var_name]] %||%
    statistic[[context]] %||%
    statistic[["default"]] %||%
    "{n}"
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: check if a format spec is multi-row (named vector)
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
is_multirow_spec <- function(fmt_spec) {
  is.character(fmt_spec) && length(fmt_spec) > 1L && !is.null(names(fmt_spec))
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: interpolate stats into a format string for a given arm
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
interpolate_stats <- function(var_df, arm_val, fmt_str) {
  subset <- var_df[var_df$arm == arm_val, , drop = FALSE]
  if (nrow(subset) == 0L) {
    return("")
  }

  stat_vec <- stats::setNames(subset$stat_fmt, subset$stat_name)
  tryCatch(
    as.character(glue::glue_data(
      as.list(stat_vec),
      fmt_str,
      .open = "{",
      .close = "}"
    )),
    error = function(e) ""
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: validate format string references against available stat_names
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
validate_format_stats <- function(fmt_str, available_stats, var_name, call) {
  # Extract stat_name references from glue format string
  refs <- parse_glue_refs(fmt_str)
  if (length(refs) == 0L) {
    return(invisible(NULL))
  }

  missing <- setdiff(refs, available_stats)
  if (length(missing) == 0L) {
    return(invisible(NULL))
  }

  cli_abort(
    c(
      "Format string for {.val {var_name}} references unknown stat{?s}: {.val {missing}}.",
      "i" = "Format string: {.val {fmt_str}}",
      "i" = "Available stat_names for {.val {var_name}}: {.val {sort(available_stats)}}."
    ),
    call = call
  )
}


#' @noRd
parse_glue_refs <- function(fmt_str) {
  # Remove escaped braces (glue's {{literal}} syntax) before matching
  cleaned <- gsub("\\{\\{[^}]*\\}\\}", "", fmt_str)
  m <- gregexpr("\\{([^{}]+)\\}", cleaned)
  refs <- regmatches(cleaned, m)[[1L]]
  if (length(refs) == 0L) {
    return(character(0L))
  }
  # Strip braces
  gsub("^\\{|\\}$", "", refs)
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: pivot long to wide (base R, no tidyr)
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
pivot_ard_wide <- function(
  long_df,
  arm_levels,
  extra_group_cols = character(0L)
) {
  key_cols <- c(extra_group_cols, "variable", "stat_label")
  keep_cols <- c(key_cols, "arm", "cell_text")
  long_df <- unique(long_df[, keep_cols, drop = FALSE])

  # Build row key
  key_parts <- lapply(key_cols, function(k) long_df[[k]])
  long_df$row_key <- do.call(paste, c(key_parts, list(sep = "\x1f")))

  # Skeleton from first arm
  first_arm <- long_df[long_df$arm == arm_levels[1L], , drop = FALSE]
  wide <- first_arm[, key_cols, drop = FALSE]

  # Row key for the wide skeleton
  wide_key_parts <- lapply(key_cols, function(k) wide[[k]])
  wide_key <- do.call(paste, c(wide_key_parts, list(sep = "\x1f")))

  for (a in arm_levels) {
    arm_df <- long_df[long_df$arm == a, , drop = FALSE]
    wide[[a]] <- arm_df$cell_text[match(wide_key, arm_df$row_key)]
  }

  wide
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: resolve decimals to a standard structure
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
resolve_ard_decimals <- function(decimals) {
  if (is.null(decimals)) {
    return(list(global = NULL, per_var = NULL))
  }

  if (is.numeric(decimals) && !is.null(names(decimals))) {
    return(list(global = decimals, per_var = NULL))
  }

  if (is.list(decimals)) {
    global <- decimals[[".default"]]
    per_var <- decimals[setdiff(names(decimals), ".default")]
    return(list(global = global, per_var = per_var))
  }

  list(global = NULL, per_var = NULL)
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal: format a single stat value
# ══════════════════════════════════════════════════════════════════════════════

#' @noRd
format_ard_stat <- function(
  value,
  value_chr,
  stat_name,
  decimals_resolved = NULL,
  variable = NULL,
  fmt = NULL
) {
  # Non-finite numeric → empty
  if (!is.na(value) && !is.finite(value)) {
    return("")
  }

  # Logical stats → format as TRUE/FALSE
  if (stat_name %in% fr_env$ard_logical_stat_names) {
    if (!is.na(value_chr)) {
      return(value_chr)
    }
    return("")
  }

  # Character stats or non-coercible → use character value as-is
  if (is.na(value) && !is.na(value_chr)) {
    return(value_chr)
  }

  # Both NA → empty
  if (is.na(value)) {
    return("")
  }

  # Custom format functions override everything
  if (!is.null(fmt) && stat_name %in% names(fmt)) {
    fn <- fmt[[stat_name]]
    if (is.function(fn)) return(as.character(fn(value)))
  }

  # Per-variable decimals override
  if (!is.null(decimals_resolved) && !is.null(variable)) {
    per_var <- decimals_resolved$per_var
    if (!is.null(per_var) && variable %in% names(per_var)) {
      var_dec <- per_var[[variable]]
      if (stat_name %in% names(var_dec)) {
        d <- as.integer(var_dec[[stat_name]])
        return(format_stat_with_decimals(value, stat_name, d))
      }
    }
  }

  # Global decimals override
  if (!is.null(decimals_resolved)) {
    global <- decimals_resolved$global
    if (!is.null(global) && stat_name %in% names(global)) {
      d <- as.integer(global[[stat_name]])
      return(format_stat_with_decimals(value, stat_name, d))
    }
  }

  # Built-in defaults per stat type
  switch(
    stat_name,
    # cards core
    n = ,
    N = ,
    N_obs = ,
    N_miss = ,
    N_nonmiss = ,
    n_cum = ,
    n_event = ,
    n.risk = sprintf("%d", as.integer(round(value))),
    p = sprintf("%.0f", value * 100),
    p_miss = ,
    p_nonmiss = ,
    p_cum = sprintf("%.1f", value * 100),
    mean = sprintf("%.1f", value),
    sd = sprintf("%.2f", value),
    median = sprintf("%.1f", value),
    min = ,
    max = sprintf("%.1f", value),
    p25 = ,
    p75 = sprintf("%.1f", value),
    # cardx test stats
    p.value = format_p_value(value),
    estimate = ,
    std.error = sprintf("%.4f", value),
    statistic = sprintf("%.2f", value),
    parameter = sprintf("%.1f", value),
    conf.low = ,
    conf.high = ,
    conf.level = sprintf("%.2f", value),
    # Default: 1 decimal
    sprintf("%.1f", value)
  )
}


# ── Internal: format with explicit decimal count, respecting p-scale ──────

#' @noRd
format_stat_with_decimals <- function(value, stat_name, d) {
  is_pct <- stat_name %in% c("p", "p_miss", "p_nonmiss", "p_cum")
  if (is_pct) {
    value <- value * 100
  }
  # Pharma boundary rules for integer percentages (d == 0):
  #   >0 and <1   -> "<1"    (not "0" — subjects exist)
  #   >99 and <100 -> ">99"   (not "100" — not all subjects)
  if (is_pct && d == 0L) {
    if (value > 0 && value < 1) {
      return("<1")
    }
    if (value > 99 && value < 100) return(">99")
  }
  sprintf(paste0("%.", d, "f"), value)
}


# ── Internal: p.value formatting with <0.001 threshold ────────────────────

#' @noRd
format_p_value <- function(value) {
  if (is.na(value)) {
    return("")
  }
  if (value < 0.001) {
    return("<0.001")
  }
  sprintf("%.3f", value)
}
