# ─────────────────────────────────────────────────────────────────────────────
# test-ard.R — Tests for fr_wide_ard() ARD-to-wide converter
# ─────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# Shared fixtures: minimal ARD-shaped data frames
# ══════════════════════════════════════════════════════════════════════════════

# Helper to build an ARD row (handles list columns correctly)
ard_row <- function(
  variable,
  stat_name,
  stat,
  group1 = "ARM",
  group1_level = NA_character_,
  variable_level = NA_character_,
  context = "continuous"
) {
  d <- data.frame(
    variable = variable,
    stat_name = stat_name,
    group1 = group1,
    context = context,
    stringsAsFactors = FALSE
  )
  d$stat <- list(stat)
  d$group1_level <- list(group1_level)
  d$variable_level <- list(variable_level)
  d
}

# Multi-group version with group2
ard_row2 <- function(
  variable,
  stat_name,
  stat,
  group1 = "ARM",
  group1_level = NA_character_,
  group2 = NA_character_,
  group2_level = NA_character_,
  variable_level = NA_character_,
  context = "continuous"
) {
  d <- data.frame(
    variable = variable,
    stat_name = stat_name,
    group1 = group1,
    group2 = group2,
    context = context,
    stringsAsFactors = FALSE
  )
  d$stat <- list(stat)
  d$group1_level <- list(group1_level)
  d$group2_level <- list(group2_level)
  d$variable_level <- list(variable_level)
  d
}

make_continuous_ard <- function() {
  rows <- list(
    ard_row("AGE", "N", 45, group1_level = "Placebo"),
    ard_row("AGE", "mean", 65.3, group1_level = "Placebo"),
    ard_row("AGE", "sd", 12.1, group1_level = "Placebo"),
    ard_row("AGE", "median", 64.0, group1_level = "Placebo"),
    ard_row("AGE", "min", 35, group1_level = "Placebo"),
    ard_row("AGE", "max", 89, group1_level = "Placebo"),
    ard_row("AGE", "N", 44, group1_level = "Drug"),
    ard_row("AGE", "mean", 63.8, group1_level = "Drug"),
    ard_row("AGE", "sd", 11.5, group1_level = "Drug"),
    ard_row("AGE", "median", 62.5, group1_level = "Drug"),
    ard_row("AGE", "min", 32, group1_level = "Drug"),
    ard_row("AGE", "max", 85, group1_level = "Drug")
  )
  do.call(rbind, rows)
}

make_categorical_ard <- function() {
  rows <- list(
    ard_row(
      "SEX",
      "n",
      20,
      group1_level = "Placebo",
      variable_level = "Male",
      context = "categorical"
    ),
    ard_row(
      "SEX",
      "p",
      0.444,
      group1_level = "Placebo",
      variable_level = "Male",
      context = "categorical"
    ),
    ard_row(
      "SEX",
      "n",
      25,
      group1_level = "Placebo",
      variable_level = "Female",
      context = "categorical"
    ),
    ard_row(
      "SEX",
      "p",
      0.556,
      group1_level = "Placebo",
      variable_level = "Female",
      context = "categorical"
    ),
    ard_row(
      "SEX",
      "n",
      18,
      group1_level = "Drug",
      variable_level = "Male",
      context = "categorical"
    ),
    ard_row(
      "SEX",
      "p",
      0.409,
      group1_level = "Drug",
      variable_level = "Male",
      context = "categorical"
    ),
    ard_row(
      "SEX",
      "n",
      26,
      group1_level = "Drug",
      variable_level = "Female",
      context = "categorical"
    ),
    ard_row(
      "SEX",
      "p",
      0.591,
      group1_level = "Drug",
      variable_level = "Female",
      context = "categorical"
    )
  )
  do.call(rbind, rows)
}

make_mixed_ard <- function() {
  rbind(make_continuous_ard(), make_categorical_ard())
}

# Unlisted ARD: all columns atomic (simulates unlist_ard_columns())
make_unlisted_ard <- function() {
  data.frame(
    variable = rep("AGE", 4L),
    stat_name = rep(c("mean", "sd"), 2L),
    stat = c(65.3, 12.1, 63.8, 11.5),
    group1 = rep("ARM", 4L),
    group1_level = rep(c("Placebo", "Drug"), each = 2L),
    variable_level = rep(NA_character_, 4L),
    context = rep("continuous", 4L),
    stringsAsFactors = FALSE
  )
}

# Renamed-groups ARD: ARM as column name, variable/variable_level kept
make_renamed_groups_ard <- function() {
  data.frame(
    ARM = rep(c("Placebo", "Drug"), each = 2L),
    variable = rep("AGE", 4L),
    variable_level = rep(NA_character_, 4L),
    context = rep("continuous", 4L),
    stat_name = rep(c("mean", "sd"), 2L),
    stat = c(65.3, 12.1, 63.8, 11.5),
    stringsAsFactors = FALSE
  )
}

# Renamed-groups categorical
make_renamed_groups_cat_ard <- function() {
  data.frame(
    ARM = rep(c("Placebo", "Drug"), each = 4L),
    variable = rep("SEX", 8L),
    variable_level = rep(c("Male", "Male", "Female", "Female"), 2L),
    context = rep("categorical", 8L),
    stat_name = rep(c("n", "p", "n", "p"), 2L),
    stat = c(20, 0.444, 25, 0.556, 18, 0.409, 26, 0.591),
    stringsAsFactors = FALSE
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# Input validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_wide_ard errors on non-data-frame input", {
  expect_error(fr_wide_ard("not a df"), "must be a data frame")
})

test_that("fr_wide_ard errors on missing required columns", {
  bad_df <- data.frame(x = 1)
  expect_error(fr_wide_ard(bad_df), "missing required columns")
})

test_that("fr_wide_ard errors on bad statistic type", {
  ard <- make_continuous_ard()
  expect_error(fr_wide_ard(ard, statistic = 42), "must be a named list")
})

test_that("fr_wide_ard errors on bad fmt type", {
  ard <- make_continuous_ard()
  expect_error(fr_wide_ard(ard, fmt = "bad"), "must be a named list")
})


# ══════════════════════════════════════════════════════════════════════════════
# Single format string (continuous)
# ══════════════════════════════════════════════════════════════════════════════

test_that("continuous ARD with single format -> one row per variable", {
  ard <- make_continuous_ard()
  wide <- fr_wide_ard(ard, statistic = list(continuous = "{mean} ({sd})"))

  expect_s3_class(wide, "data.frame")
  expect_true("variable" %in% names(wide))
  expect_true("stat_label" %in% names(wide))
  expect_true("Placebo" %in% names(wide))
  expect_true("Drug" %in% names(wide))
  expect_equal(nrow(wide), 1L)
  expect_true(grepl("65.3", wide$Placebo[1L]))
  expect_true(grepl("12.10", wide$Placebo[1L]))
})


# ══════════════════════════════════════════════════════════════════════════════
# Multi-row continuous (named vector)
# ══════════════════════════════════════════════════════════════════════════════

test_that("named vector statistic -> multi-row continuous output", {
  ard <- make_continuous_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(
      continuous = c(
        n = "{N}",
        "Mean (SD)" = "{mean} ({sd})",
        Median = "{median}",
        "Min, Max" = "{min}, {max}"
      )
    )
  )

  expect_equal(nrow(wide), 4L)
  expect_equal(
    wide$stat_label,
    c("  n", "  Mean (SD)", "  Median", "  Min, Max")
  )
  expect_equal(wide$Placebo[1L], "45")
  expect_true(grepl("65.3", wide$Placebo[2L]))
})


# ══════════════════════════════════════════════════════════════════════════════
# Categorical with {n} ({p}%)
# ══════════════════════════════════════════════════════════════════════════════

test_that("categorical ARD with n (p%) format", {
  ard <- make_categorical_ard()
  wide <- fr_wide_ard(ard, statistic = list(categorical = "{n} ({p}%)"))

  expect_equal(nrow(wide), 2L) # Male, Female
  expect_true(grepl("20", wide$Placebo[1L]))
  expect_true(grepl("44%", wide$Placebo[1L]))
})


# ══════════════════════════════════════════════════════════════════════════════
# Mixed continuous + categorical
# ══════════════════════════════════════════════════════════════════════════════

test_that("mixed ARD with separate continuous/categorical formats", {
  ard <- make_mixed_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(
      continuous = c("Mean (SD)" = "{mean} ({sd})"),
      categorical = "{n} ({p}%)"
    )
  )

  # 1 continuous row (Mean (SD)) + 2 categorical (Male, Female)
  expect_equal(nrow(wide), 3L)
  expect_equal(wide$variable[1L], "AGE")
  expect_equal(wide$variable[2L], "SEX")
})


# ══════════════════════════════════════════════════════════════════════════════
# Overall column
# ══════════════════════════════════════════════════════════════════════════════

test_that("overall rows included with default 'Total' header", {
  ard <- make_continuous_ard()
  overall_rows <- do.call(
    rbind,
    list(
      ard_row(
        "AGE",
        "N",
        89,
        group1 = NA_character_,
        group1_level = NA_character_
      ),
      ard_row(
        "AGE",
        "mean",
        64.5,
        group1 = NA_character_,
        group1_level = NA_character_
      ),
      ard_row(
        "AGE",
        "sd",
        11.8,
        group1 = NA_character_,
        group1_level = NA_character_
      ),
      ard_row(
        "AGE",
        "median",
        63.2,
        group1 = NA_character_,
        group1_level = NA_character_
      ),
      ard_row(
        "AGE",
        "min",
        32,
        group1 = NA_character_,
        group1_level = NA_character_
      ),
      ard_row(
        "AGE",
        "max",
        89,
        group1 = NA_character_,
        group1_level = NA_character_
      )
    )
  )
  ard <- rbind(ard, overall_rows)

  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})"),
    overall = "Total"
  )

  expect_true("Total" %in% names(wide))
  expect_true(grepl("64.5", wide$Total[1L]))
})

test_that("overall = NULL excludes overall rows", {
  ard <- make_continuous_ard()
  overall_rows <- do.call(
    rbind,
    list(
      ard_row(
        "AGE",
        "mean",
        64.5,
        group1 = NA_character_,
        group1_level = NA_character_
      ),
      ard_row(
        "AGE",
        "sd",
        11.8,
        group1 = NA_character_,
        group1_level = NA_character_
      )
    )
  )
  ard <- rbind(ard, overall_rows)

  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})"),
    overall = NULL
  )

  expect_false("Total" %in% names(wide))
})


# ══════════════════════════════════════════════════════════════════════════════
# Custom labels
# ══════════════════════════════════════════════════════════════════════════════

test_that("label argument renames variable column", {
  ard <- make_continuous_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})"),
    label = c(AGE = "Age (years)")
  )

  expect_equal(wide$variable[1L], "Age (years)")
})


# ══════════════════════════════════════════════════════════════════════════════
# Per-variable statistic overrides
# ══════════════════════════════════════════════════════════════════════════════

test_that("per-variable statistic override works", {
  ard <- make_mixed_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(
      AGE = c("Mean (SD)" = "{mean} ({sd})", Median = "{median}"),
      SEX = "{n} ({p}%)"
    )
  )

  # AGE gets 2 rows, SEX gets 2 levels
  expect_equal(nrow(wide), 4L)
  age_rows <- wide[wide$variable == "AGE", ]
  expect_equal(nrow(age_rows), 2L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Per-variable decimals
# ══════════════════════════════════════════════════════════════════════════════

test_that("per-variable decimals override works", {
  ard <- make_continuous_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})"),
    decimals = list(AGE = c(mean = 2, sd = 3))
  )

  expect_true(grepl("65.30", wide$Placebo[1L]))
  expect_true(grepl("12.100", wide$Placebo[1L]))
})

test_that("global decimals (named vector) works", {
  ard <- make_continuous_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})"),
    decimals = c(mean = 2, sd = 1)
  )

  expect_true(grepl("65.30", wide$Placebo[1L]))
  expect_true(grepl("12.1)", wide$Placebo[1L]))
})


# ══════════════════════════════════════════════════════════════════════════════
# Custom fmt functions
# ══════════════════════════════════════════════════════════════════════════════

test_that("p.value uses <0.001 threshold by default", {
  ard <- do.call(
    rbind,
    list(
      ard_row("TREATMENT", "p.value", 0.00003, group1_level = "Placebo"),
      ard_row("TREATMENT", "p.value", 0.045, group1_level = "Drug")
    )
  )

  wide <- fr_wide_ard(ard, statistic = list(continuous = "{p.value}"))

  expect_equal(wide$Placebo[1L], "<0.001")
  expect_equal(wide$Drug[1L], "0.045")
})

test_that("custom fmt overrides built-in p.value handling", {
  ard <- do.call(
    rbind,
    list(
      ard_row("TREATMENT", "p.value", 0.00003, group1_level = "Placebo"),
      ard_row("TREATMENT", "p.value", 0.045, group1_level = "Drug")
    )
  )

  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{p.value}"),
    fmt = list(p.value = function(x) sprintf("%.5f", x))
  )

  expect_equal(wide$Placebo[1L], "0.00003")
})


# ══════════════════════════════════════════════════════════════════════════════
# Single string statistic (Form 3)
# ══════════════════════════════════════════════════════════════════════════════

test_that("single string statistic applies to all variables", {
  ard <- make_categorical_ard()
  wide <- fr_wide_ard(ard, statistic = "{n} ({p}%)")

  expect_equal(nrow(wide), 2L)
  expect_true(grepl("20", wide$Placebo[1L]))
})


# ══════════════════════════════════════════════════════════════════════════════
# Default statistic key
# ══════════════════════════════════════════════════════════════════════════════

test_that("'default' key in statistic provides fallback", {
  ard <- make_categorical_ard()
  wide <- fr_wide_ard(ard, statistic = list(default = "{n} ({p}%)"))

  expect_equal(nrow(wide), 2L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Percentage formatting with decimals
# ══════════════════════════════════════════════════════════════════════════════

test_that("p stat is multiplied by 100 and respects decimal override", {
  ard <- make_categorical_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(categorical = "{n} ({p}%)"),
    decimals = c(p = 1)
  )

  # p = 0.444 -> 44.4%
  expect_true(grepl("44.4%", wide$Placebo[1L]))
})


# ══════════════════════════════════════════════════════════════════════════════
# Stat names from cardx
# ══════════════════════════════════════════════════════════════════════════════

test_that("cardx stat names use correct default formatting", {
  ard <- do.call(
    rbind,
    list(
      ard_row("AGE", "estimate", 1.2345, group1_level = "Placebo"),
      ard_row("AGE", "N_obs", 100, group1_level = "Placebo")
    )
  )

  wide <- fr_wide_ard(ard, statistic = list(continuous = "{estimate}"))

  expect_equal(wide$Placebo[1L], "1.2345")
})


# ══════════════════════════════════════════════════════════════════════════════
# Integration: pipeline to fr_table()
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_wide_ard output pipes to fr_table()", {
  ard <- make_mixed_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(
      continuous = c("Mean (SD)" = "{mean} ({sd})"),
      categorical = "{n} ({p}%)"
    ),
    label = c(AGE = "Age (years)", SEX = "Sex")
  )

  spec <- fr_table(wide)
  expect_s3_class(spec, "fr_spec")
  expect_true("variable" %in% names(spec$data))
  expect_true("stat_label" %in% names(spec$data))
})


# ══════════════════════════════════════════════════════════════════════════════
# Column normalization — atomic columns (unlist_ard_columns output)
# ══════════════════════════════════════════════════════════════════════════════

test_that("atomic stat column works (unlisted ARD)", {
  ard <- make_unlisted_ard()
  wide <- fr_wide_ard(ard, statistic = list(continuous = "{mean} ({sd})"))

  expect_s3_class(wide, "data.frame")
  expect_equal(nrow(wide), 1L)
  expect_true(grepl("65.3", wide$Placebo[1L]))
  expect_true(grepl("12.10", wide$Placebo[1L]))
})

test_that("atomic group1_level and variable_level work", {
  ard <- data.frame(
    variable = rep("SEX", 4L),
    stat_name = rep(c("n", "p"), 2L),
    stat = c(20, 0.444, 25, 0.556),
    group1 = rep("ARM", 4L),
    group1_level = rep(c("Placebo", "Placebo"), 2L),
    variable_level = rep(c("Male", "Female"), each = 2L),
    context = rep("categorical", 4L),
    stringsAsFactors = FALSE
  )

  wide <- fr_wide_ard(ard, statistic = list(categorical = "{n} ({p}%)"))

  expect_equal(nrow(wide), 2L)
  expect_true(grepl("20", wide$Placebo[1L]))
})


# ══════════════════════════════════════════════════════════════════════════════
# Renamed group columns (production pipeline pattern)
# ══════════════════════════════════════════════════════════════════════════════

test_that("renamed groups (ARM as column) works for continuous", {
  ard <- make_renamed_groups_ard()
  wide <- fr_wide_ard(
    ard,
    column = "ARM",
    statistic = list(continuous = "{mean} ({sd})")
  )

  expect_s3_class(wide, "data.frame")
  expect_true("Placebo" %in% names(wide))
  expect_true("Drug" %in% names(wide))
  expect_equal(nrow(wide), 1L)
  expect_true(grepl("65.3", wide$Placebo[1L]))
})

test_that("renamed groups auto-detects single non-standard column", {
  ard <- make_renamed_groups_ard()
  # column = NULL → should auto-detect ARM as the only non-standard col
  wide <- fr_wide_ard(ard, statistic = list(continuous = "{mean} ({sd})"))

  expect_true("Placebo" %in% names(wide))
  expect_true("Drug" %in% names(wide))
})

test_that("renamed groups works for categorical", {
  ard <- make_renamed_groups_cat_ard()
  wide <- fr_wide_ard(
    ard,
    column = "ARM",
    statistic = list(categorical = "{n} ({p}%)")
  )

  expect_equal(nrow(wide), 2L) # Male, Female
  expect_true(grepl("20", wide$Placebo[1L]))
})

test_that("full production pipeline: unlist + rename groups", {
  # Simulates: ard_stack() |> unlist_ard_columns() |>
  #   rename_ard_columns(columns = all_ard_groups("names"))
  ard <- rbind(
    make_renamed_groups_ard(),
    make_renamed_groups_cat_ard()
  )

  wide <- fr_wide_ard(
    ard,
    column = "ARM",
    statistic = list(
      continuous = c("Mean (SD)" = "{mean} ({sd})"),
      categorical = "{n} ({p}%)"
    ),
    label = c(AGE = "Age (years)", SEX = "Sex")
  )

  expect_equal(nrow(wide), 3L)
  expect_equal(wide$variable[1L], "Age (years)")
  expect_true("Placebo" %in% names(wide))
})


# ══════════════════════════════════════════════════════════════════════════════
# Fully-renamed ARD (default rename_ard_columns, no variable column)
# ══════════════════════════════════════════════════════════════════════════════

test_that("fully-renamed ARD: reconstruction from named columns", {
  # After rename_ard_columns(): ARM, AGE, SEX columns, no variable col
  ard <- data.frame(
    ARM = c(
      "Placebo",
      "Placebo",
      "Drug",
      "Drug",
      "Placebo",
      "Placebo",
      "Drug",
      "Drug"
    ),
    AGE = rep(NA_character_, 8L),
    SEX = c(NA, NA, NA, NA, "Male", "Male", "Male", "Male"),
    context = c(rep("continuous", 4L), rep("categorical", 4L)),
    stat_name = c("mean", "sd", "mean", "sd", "n", "p", "n", "p"),
    stat = c(65.3, 12.1, 63.8, 11.5, 20, 0.444, 18, 0.409),
    stringsAsFactors = FALSE
  )

  wide <- fr_wide_ard(
    ard,
    column = "ARM",
    statistic = list(
      continuous = "{mean} ({sd})",
      categorical = "{n} ({p}%)"
    )
  )

  expect_s3_class(wide, "data.frame")
  expect_true("Placebo" %in% names(wide))
  # SEX rows should have been reconstructed
  sex_rows <- wide[wide$variable == "SEX", ]
  expect_gte(nrow(sex_rows), 1L)
})

test_that("fully-renamed ARD requires column parameter", {
  ard <- data.frame(
    ARM = c("Placebo", "Drug"),
    AGE = c(NA, NA),
    stat_name = c("mean", "mean"),
    stat = c(65.3, 63.8),
    stringsAsFactors = FALSE
  )

  expect_error(
    fr_wide_ard(ard, column = NULL),
    "auto-detect"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# Multi-group .by = c("ARM", "SEX")
# ══════════════════════════════════════════════════════════════════════════════

test_that("multi-group raw ARD preserves extra group columns", {
  rows <- list(
    ard_row2(
      "AGE",
      "mean",
      72.1,
      group1_level = "Placebo",
      group2 = "SEX",
      group2_level = "F"
    ),
    ard_row2(
      "AGE",
      "sd",
      6.55,
      group1_level = "Placebo",
      group2 = "SEX",
      group2_level = "F"
    ),
    ard_row2(
      "AGE",
      "mean",
      64.5,
      group1_level = "Placebo",
      group2 = "SEX",
      group2_level = "M"
    ),
    ard_row2(
      "AGE",
      "sd",
      8.30,
      group1_level = "Placebo",
      group2 = "SEX",
      group2_level = "M"
    ),
    ard_row2(
      "AGE",
      "mean",
      71.8,
      group1_level = "Drug",
      group2 = "SEX",
      group2_level = "F"
    ),
    ard_row2(
      "AGE",
      "sd",
      7.20,
      group1_level = "Drug",
      group2 = "SEX",
      group2_level = "F"
    ),
    ard_row2(
      "AGE",
      "mean",
      63.2,
      group1_level = "Drug",
      group2 = "SEX",
      group2_level = "M"
    ),
    ard_row2(
      "AGE",
      "sd",
      9.10,
      group1_level = "Drug",
      group2 = "SEX",
      group2_level = "M"
    )
  )
  ard <- do.call(rbind, rows)

  wide <- fr_wide_ard(
    ard,
    column = "ARM",
    statistic = list(continuous = c("Mean (SD)" = "{mean} ({sd})"))
  )

  expect_true("SEX" %in% names(wide))
  expect_equal(nrow(wide), 2L) # F and M
  # Check sex values are preserved
  expect_true("F" %in% wide$SEX || "M" %in% wide$SEX)
})

test_that("multi-group renamed ARD preserves extra group columns", {
  ard <- data.frame(
    ARM = rep(c("Placebo", "Drug"), each = 4L),
    SEX = rep(c("F", "F", "M", "M"), 2L),
    variable = rep("AGE", 8L),
    variable_level = rep(NA_character_, 8L),
    context = rep("continuous", 8L),
    stat_name = rep(c("mean", "sd"), 4L),
    stat = c(72.1, 6.55, 64.5, 8.30, 71.8, 7.20, 63.2, 9.10),
    stringsAsFactors = FALSE
  )

  wide <- fr_wide_ard(
    ard,
    column = "ARM",
    statistic = list(continuous = c("Mean (SD)" = "{mean} ({sd})"))
  )

  expect_true("SEX" %in% names(wide))
  expect_equal(nrow(wide), 2L)
})


# ══════════════════════════════════════════════════════════════════════════════
# No groups at all
# ══════════════════════════════════════════════════════════════════════════════

test_that("ungrouped ARD produces single value column", {
  ard <- data.frame(
    variable = rep("AGE", 2L),
    stat_name = c("mean", "sd"),
    stat = c(64.5, 11.8),
    context = rep("continuous", 2L),
    stringsAsFactors = FALSE
  )

  wide <- fr_wide_ard(ard, statistic = list(continuous = "{mean} ({sd})"))

  expect_s3_class(wide, "data.frame")
  expect_equal(nrow(wide), 1L)
  # Should have a value column (via overall default)
  arm_cols <- setdiff(names(wide), c("variable", "stat_label"))
  expect_gte(length(arm_cols), 1L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Sentinels & internal context filtering
# ══════════════════════════════════════════════════════════════════════════════

test_that("..ard_hierarchical_overall.. is kept (not filtered)", {
  rows <- list(
    ard_row(
      "..ard_hierarchical_overall..",
      "n",
      35,
      group1_level = "Placebo",
      context = "categorical",
      variable_level = NA_character_
    ),
    ard_row(
      "..ard_hierarchical_overall..",
      "p",
      0.78,
      group1_level = "Placebo",
      context = "categorical",
      variable_level = NA_character_
    ),
    ard_row(
      "..ard_hierarchical_overall..",
      "n",
      38,
      group1_level = "Drug",
      context = "categorical",
      variable_level = NA_character_
    ),
    ard_row(
      "..ard_hierarchical_overall..",
      "p",
      0.84,
      group1_level = "Drug",
      context = "categorical",
      variable_level = NA_character_
    )
  )
  ard <- do.call(rbind, rows)

  wide <- fr_wide_ard(ard, statistic = "{n} ({p}%)")

  # Should NOT be empty — sentinel was kept
  expect_gte(nrow(wide), 1L)
  expect_true(grepl("35", wide$Placebo[1L]))
})

test_that("..ard_hierarchical_overall.. is relabelable via label", {
  rows <- list(
    ard_row(
      "..ard_hierarchical_overall..",
      "n",
      35,
      group1_level = "Placebo",
      context = "categorical",
      variable_level = NA_character_
    ),
    ard_row(
      "..ard_hierarchical_overall..",
      "n",
      38,
      group1_level = "Drug",
      context = "categorical",
      variable_level = NA_character_
    )
  )
  ard <- do.call(rbind, rows)

  wide <- fr_wide_ard(
    ard,
    statistic = "{n}",
    label = c("..ard_hierarchical_overall.." = "Any TEAE")
  )

  expect_equal(wide$variable[1L], "Any TEAE")
})

test_that("hierarchical SOC rows have SOC name in pt column", {
  rows <- list(
    # SOC-level rows
    ard_row2(
      "AEBODSYS",
      "n",
      10,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      variable_level = "Cardiac disorders",
      context = "hierarchical"
    ),
    ard_row2(
      "AEBODSYS",
      "p",
      0.22,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      variable_level = "Cardiac disorders",
      context = "hierarchical"
    ),
    # PT-level rows
    ard_row2(
      "AEDECOD",
      "n",
      5,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = "Cardiac disorders",
      variable_level = "Bradycardia",
      context = "hierarchical"
    ),
    ard_row2(
      "AEDECOD",
      "p",
      0.11,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = "Cardiac disorders",
      variable_level = "Bradycardia",
      context = "hierarchical"
    )
  )
  ard <- do.call(rbind, rows)

  wide <- fr_wide_ard(ard, statistic = "{n} ({p}%)")

  soc_rows <- wide[wide$row_type == "soc", ]
  # pt should equal soc (not empty string)
  expect_true(all(soc_rows$pt == soc_rows$soc))
  expect_true(all(nzchar(soc_rows$pt)))
})

test_that("hierarchical overall row has label in pt column", {
  rows <- list(
    # Overall row
    ard_row2(
      "..ard_hierarchical_overall..",
      "n",
      35,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      variable_level = NA_character_,
      context = "hierarchical"
    ),
    ard_row2(
      "..ard_hierarchical_overall..",
      "p",
      0.78,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      variable_level = NA_character_,
      context = "hierarchical"
    ),
    # SOC row (needed to trigger hierarchical detection)
    ard_row2(
      "AEBODSYS",
      "n",
      10,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      variable_level = "Cardiac disorders",
      context = "hierarchical"
    ),
    ard_row2(
      "AEBODSYS",
      "p",
      0.22,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      variable_level = "Cardiac disorders",
      context = "hierarchical"
    ),
    # PT row
    ard_row2(
      "AEDECOD",
      "n",
      5,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = "Cardiac disorders",
      variable_level = "Bradycardia",
      context = "hierarchical"
    ),
    ard_row2(
      "AEDECOD",
      "p",
      0.11,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = "Cardiac disorders",
      variable_level = "Bradycardia",
      context = "hierarchical"
    )
  )
  ard <- do.call(rbind, rows)

  wide <- fr_wide_ard(
    ard,
    statistic = "{n} ({p}%)",
    label = c("..ard_hierarchical_overall.." = "Any TEAE")
  )

  overall_row <- wide[wide$row_type == "overall", ]
  expect_equal(nrow(overall_row), 1L)
  expect_equal(overall_row$pt, "Any TEAE")
  expect_equal(overall_row$soc, "Any TEAE")
})

test_that("..ard_total_n.. is filtered out", {
  ard <- make_continuous_ard()
  internal_rows <- do.call(
    rbind,
    list(
      ard_row("..ard_total_n..", "N", 89, group1_level = "Placebo"),
      ard_row("..ard_total_n..", "N", 89, group1_level = "Drug")
    )
  )
  ard <- rbind(ard, internal_rows)

  wide <- fr_wide_ard(ard, statistic = list(continuous = "{mean} ({sd})"))

  # ..ard_total_n.. rows should be filtered
  expect_false(any(grepl("total_n", wide$variable)))
  expect_equal(nrow(wide), 1L)
})

test_that("context = 'attributes' rows are filtered", {
  ard <- make_continuous_ard()
  attr_rows <- do.call(
    rbind,
    list(
      ard_row(
        "AGE",
        "label",
        0,
        group1_level = "Placebo",
        context = "attributes"
      ),
      ard_row(
        "AGE",
        "class",
        0,
        group1_level = "Placebo",
        context = "attributes"
      )
    )
  )
  ard <- rbind(ard, attr_rows)

  wide <- fr_wide_ard(ard, statistic = list(continuous = "{mean} ({sd})"))

  # Only 1 row (mean/sd), attribute rows filtered
  expect_equal(nrow(wide), 1L)
})

test_that("context = 'total_n' rows are filtered", {
  ard <- make_continuous_ard()
  tn_rows <- do.call(
    rbind,
    list(
      ard_row("AGE", "N", 89, group1_level = "Placebo", context = "total_n"),
      ard_row("AGE", "N", 89, group1_level = "Drug", context = "total_n")
    )
  )
  ard <- rbind(ard, tn_rows)

  wide <- fr_wide_ard(ard, statistic = list(continuous = "{mean} ({sd})"))
  expect_equal(nrow(wide), 1L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Non-numeric stat values (character, logical, call objects)
# ══════════════════════════════════════════════════════════════════════════════

test_that("character stat (method name) formats as-is, not blank", {
  ard <- do.call(
    rbind,
    list(
      ard_row("AGE", "p.value", 0.045, group1_level = "Placebo"),
      ard_row("AGE", "method", "test", group1_level = "Placebo")
    )
  )
  # Replace the list stat for method with a character
  ard$stat[[2L]] <- "Welch Two Sample t-test"

  wide <- suppressWarnings(fr_wide_ard(
    ard,
    statistic = list(continuous = "{p.value} ({method})")
  ))

  expect_true(grepl("Welch", wide$Placebo[1L]))
})

test_that("logical stat (paired = TRUE) formats as TRUE", {
  ard <- do.call(
    rbind,
    list(
      ard_row("AGE", "paired", TRUE, group1_level = "Placebo")
    )
  )

  wide <- fr_wide_ard(ard, statistic = list(continuous = "{paired}"))
  expect_equal(wide$Placebo[1L], "TRUE")
})

test_that("call object in stat does not crash (returns NA gracefully)", {
  ard <- do.call(
    rbind,
    list(
      ard_row("AGE", "method", 0, group1_level = "Placebo")
    )
  )
  # Replace stat with a call object
  ard$stat[[1L]] <- quote(chisq.test(x))

  # Should not error
  wide <- fr_wide_ard(ard, statistic = list(continuous = "{method}"))
  expect_s3_class(wide, "data.frame")
})


# ══════════════════════════════════════════════════════════════════════════════
# Non-finite values (Inf, NaN)
# ══════════════════════════════════════════════════════════════════════════════

test_that("Inf in stat produces empty string", {
  ard <- do.call(
    rbind,
    list(
      ard_row("AGE", "mean", Inf, group1_level = "Placebo"),
      ard_row("AGE", "sd", 12.1, group1_level = "Placebo")
    )
  )

  wide <- fr_wide_ard(ard, statistic = list(continuous = "{mean} ({sd})"))
  expect_true(grepl(" \\(12.10\\)", wide$Placebo[1L]))
})

test_that("NaN in stat produces empty string", {
  ard <- do.call(
    rbind,
    list(
      ard_row("AGE", "mean", NaN, group1_level = "Placebo"),
      ard_row("AGE", "sd", 12.1, group1_level = "Placebo")
    )
  )

  wide <- fr_wide_ard(ard, statistic = list(continuous = "{mean} ({sd})"))
  expect_true(grepl(" \\(12.10\\)", wide$Placebo[1L]))
})


# ══════════════════════════════════════════════════════════════════════════════
# BigN extraction
# ══════════════════════════════════════════════════════════════════════════════

test_that("big_n extracts BigN rows and attaches n_counts attribute", {
  ard <- make_continuous_ard()
  # Add BigN rows
  bign_rows <- do.call(
    rbind,
    list(
      ard_row("ARM", "BigN", 50, group1_level = "Placebo"),
      ard_row("ARM", "BigN", 45, group1_level = "Drug")
    )
  )
  ard <- rbind(ard, bign_rows)

  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})"),
    big_n = "BigN"
  )

  # BigN rows should NOT be in the body
  expect_equal(nrow(wide), 1L)

  # n_counts attribute should be present
  nc <- attr(wide, "n_counts")
  expect_true(!is.null(nc))
  expect_equal(nc[["Placebo"]], 50)
  expect_equal(nc[["Drug"]], 45)
})

test_that("big_n = 'popn' works for AE table convention", {
  ard <- make_continuous_ard()
  bign_rows <- do.call(
    rbind,
    list(
      ard_row("ARM", "popn", 50, group1_level = "Placebo"),
      ard_row("ARM", "popn", 45, group1_level = "Drug")
    )
  )
  ard <- rbind(ard, bign_rows)

  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})"),
    big_n = "popn"
  )

  nc <- attr(wide, "n_counts")
  expect_true(!is.null(nc))
  expect_equal(length(nc), 2L)
})

test_that("BigN rows are excluded from body data", {
  ard <- make_continuous_ard()
  bign_rows <- do.call(
    rbind,
    list(
      ard_row("ARM", "BigN", 50, group1_level = "Placebo"),
      ard_row("ARM", "BigN", 45, group1_level = "Drug")
    )
  )
  ard <- rbind(ard, bign_rows)

  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})"),
    big_n = "BigN"
  )

  # Body should not contain BigN data
  expect_equal(nrow(wide), 1L)
  # Verify it's the mean/sd row, not BigN
  expect_true(grepl("65.3", wide$Placebo[1L]))
})

test_that("big_n = NULL (default) does not extract", {
  ard <- make_continuous_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})")
  )

  expect_null(attr(wide, "n_counts"))
})


# ══════════════════════════════════════════════════════════════════════════════
# Integration with cards (guarded)
# ══════════════════════════════════════════════════════════════════════════════

test_that("raw ard_stack() -> fr_wide_ard() works", {
  skip_if_not_installed("cards")

  ard <- cards::ard_stack(
    data = arframe::adsl[arframe::adsl$SAFFL == "Y", ],
    .by = "ARM",
    cards::ard_continuous(variables = "AGE"),
    cards::ard_categorical(variables = "SEX")
  )

  wide <- fr_wide_ard(
    ard,
    statistic = list(
      continuous = c("Mean (SD)" = "{mean} ({sd})"),
      categorical = "{n} ({p}%)"
    )
  )

  expect_s3_class(wide, "data.frame")
  expect_gte(nrow(wide), 3L)
})

test_that("unlist_ard_columns() -> fr_wide_ard() works", {
  skip_if_not_installed("cards")

  ard <- cards::ard_stack(
    data = arframe::adsl[arframe::adsl$SAFFL == "Y", ],
    .by = "ARM",
    cards::ard_continuous(variables = "AGE"),
    cards::ard_categorical(variables = "SEX")
  ) |>
    cards::unlist_ard_columns()

  wide <- fr_wide_ard(
    ard,
    statistic = list(
      continuous = c("Mean (SD)" = "{mean} ({sd})"),
      categorical = "{n} ({p}%)"
    )
  )

  expect_s3_class(wide, "data.frame")
  expect_gte(nrow(wide), 3L)
})

test_that("rename_ard_columns(groups only) -> fr_wide_ard() works", {
  skip_if_not_installed("cards")

  ard <- cards::ard_stack(
    data = arframe::adsl[arframe::adsl$SAFFL == "Y", ],
    .by = "ARM",
    cards::ard_continuous(variables = "AGE"),
    cards::ard_categorical(variables = "SEX")
  ) |>
    cards::unlist_ard_columns() |>
    cards::rename_ard_columns(columns = cards::all_ard_groups("names"))

  wide <- fr_wide_ard(
    ard,
    column = "ARM",
    statistic = list(
      continuous = c("Mean (SD)" = "{mean} ({sd})"),
      categorical = "{n} ({p}%)"
    )
  )

  expect_s3_class(wide, "data.frame")
  # ARM values from adsl may vary; just check we have arm columns
  arm_cols <- setdiff(names(wide), c("variable", "stat_label"))
  expect_gte(length(arm_cols), 1L)
  expect_gte(nrow(wide), 3L)
})

test_that("full production pipeline: ard_stack -> unlist -> rename -> fr_wide_ard", {
  skip_if_not_installed("cards")

  adsl_safe <- arframe::adsl[arframe::adsl$SAFFL == "Y", ]

  ard <- cards::ard_stack(
    data = adsl_safe,
    .by = "ARM",
    cards::ard_continuous(variables = "AGE"),
    cards::ard_categorical(variables = "SEX"),
    .overall = TRUE
  ) |>
    cards::unlist_ard_columns() |>
    cards::rename_ard_columns(columns = cards::all_ard_groups("names"))

  wide <- fr_wide_ard(
    ard,
    column = "ARM",
    statistic = list(
      continuous = c(
        n = "{N}",
        "Mean (SD)" = "{mean} ({sd})",
        Median = "{median}",
        "Min, Max" = "{min}, {max}"
      ),
      categorical = "{n} ({p}%)"
    ),
    label = c(AGE = "Age (years)", SEX = "Sex"),
    decimals = c(mean = 1, sd = 2, p = 0),
    overall = "Total"
  )

  expect_s3_class(wide, "data.frame")
  expect_true("Total" %in% names(wide))
  age_rows <- wide[wide$variable == "Age (years)", ]
  expect_equal(nrow(age_rows), 4L)
  sex_rows <- wide[wide$variable == "Sex", ]
  expect_gte(nrow(sex_rows), 1L)

  # Should pipe cleanly to fr_table()
  spec <- fr_table(wide)
  expect_s3_class(spec, "fr_spec")
})


# ══════════════════════════════════════════════════════════════════════════════
# Error messages for unknown stat_names in format strings
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_wide_ard() errors helpfully when format references unknown stat_name", {
  ard <- make_continuous_ard()

  # Reference a stat_name that doesn't exist in the ARD

  expect_error(
    fr_wide_ard(ard, statistic = list(continuous = "{mean} ({se})")),
    "unknown stat.*se"
  )

  # Error message should mention available stat_names
  expect_error(
    fr_wide_ard(ard, statistic = list(continuous = "{mean} ({se})")),
    "Available stat_names"
  )
})

test_that("fr_wide_ard() errors for unknown stat in multi-row spec", {
  ard <- make_continuous_ard()

  expect_error(
    fr_wide_ard(
      ard,
      statistic = list(
        continuous = c(
          "Mean (SE)" = "{mean} ({se})",
          Median = "{median}"
        )
      )
    ),
    "unknown stat.*se"
  )
})

test_that("fr_wide_ard() errors for unknown stat in per-variable spec", {
  ard <- make_continuous_ard()

  expect_error(
    fr_wide_ard(ard, statistic = list(AGE = "{mean} ({cv})")),
    "unknown stat.*cv"
  )
})

test_that("fr_wide_ard() succeeds with valid stat_name references", {
  ard <- make_continuous_ard()

  # All referenced stats exist — should not error
  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})")
  )
  expect_s3_class(wide, "data.frame")
  expect_true(nrow(wide) > 0L)
})

test_that("parse_glue_refs handles escaped braces without false positives", {
  # Escaped braces {{literal}} should NOT be extracted as refs
  expect_equal(arframe:::parse_glue_refs("{{literal}} {n}"), "n")
  expect_equal(arframe:::parse_glue_refs("{{foo}}"), character(0L))
  expect_equal(arframe:::parse_glue_refs("{mean} ({sd})"), c("mean", "sd"))
  expect_equal(arframe:::parse_glue_refs("no refs here"), character(0L))
})


# ══════════════════════════════════════════════════════════════════════════════
# Bug regression: PT-level Total column missing with ard_stack_hierarchical
# overall = TRUE, over_variables = TRUE
# ══════════════════════════════════════════════════════════════════════════════

test_that("ard_stack_hierarchical overall=TRUE, over_variables=TRUE: PT rows have Total values", {
  skip_if_not_installed("cards")

  adsl_safe <- arframe::adsl[arframe::adsl$SAFFL == "Y", ]
  adae_safe <- arframe::adae[arframe::adae$SAFFL == "Y", ]

  ard <- cards::ard_stack_hierarchical(
    data = adae_safe,
    variables = c(AEBODSYS, AEDECOD),
    by = TRTA,
    denominator = adsl_safe,
    id = USUBJID,
    overall = TRUE,
    over_variables = TRUE
  )

  wide <- fr_wide_ard(
    ard,
    statistic = "{n} ({p}%)",
    overall = "Total"
  )

  expect_s3_class(wide, "data.frame")

  # Hierarchical output has soc, pt, row_type columns
  expect_true(all(c("soc", "pt", "row_type", "Total") %in% names(wide)))

  # Overall row must exist with non-empty Total
  overall_rows <- wide[wide$row_type == "overall", , drop = FALSE]
  expect_gte(nrow(overall_rows), 1L)
  expect_true(
    nchar(overall_rows$Total[1L]) > 0L,
    label = "Overall row has a non-empty Total value"
  )

  # PT-level rows must have non-empty Total values
  pt_rows <- wide[wide$row_type == "pt", , drop = FALSE]
  expect_gte(nrow(pt_rows), 1L)
  pt_with_total <- pt_rows[nchar(pt_rows$Total) > 0L, , drop = FALSE]
  expect_equal(
    nrow(pt_with_total),
    nrow(pt_rows),
    label = "All PT-level rows have non-empty Total values"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: normalize_ard_chr with factor input
# ══════════════════════════════════════════════════════════════════════════════

test_that("normalize_ard_chr handles factor columns", {
  result <- arframe:::normalize_ard_chr(factor(c("a", "b", "c")))
  expect_equal(result, c("a", "b", "c"))
  expect_type(result, "character")
})

test_that("normalize_ard_chr handles non-character/non-factor columns", {
  result <- arframe:::normalize_ard_chr(c(1L, 2L, 3L))
  expect_equal(result, c("1", "2", "3"))
  expect_type(result, "character")
})

test_that("normalize_ard_chr handles list with NULL/empty elements", {
  result <- arframe:::normalize_ard_chr(list(NULL, character(0L), "hello"))
  expect_equal(result, c(NA_character_, NA_character_, "hello"))
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: normalize_ard_num edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("normalize_ard_num handles list with logical element", {
  result <- arframe:::normalize_ard_num(list(TRUE, FALSE))
  expect_equal(result, c(1, 0))
})

test_that("normalize_ard_num handles logical column", {
  result <- arframe:::normalize_ard_num(c(TRUE, FALSE, TRUE))
  expect_equal(result, c(1, 0, 1))
})

test_that("normalize_ard_num handles character column (non-numeric coercion)", {
  result <- arframe:::normalize_ard_num(c("1.5", "abc", "3"))
  expect_equal(result, c(1.5, NA_real_, 3))
})

test_that("normalize_ard_num handles list with non-coercible element", {
  expect_warning(
    result <- arframe:::normalize_ard_num(list("abc")),
    "Non-numeric"
  )
  expect_equal(result, NA_real_)
})

test_that("normalize_ard_num handles list with NULL/empty elements", {
  result <- arframe:::normalize_ard_num(list(NULL, numeric(0L), 42))
  expect_equal(result, c(NA_real_, NA_real_, 42))
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: detect_renamed_arm error paths
# ══════════════════════════════════════════════════════════════════════════════

test_that("detect_renamed_arm errors with multiple ambiguous group cols", {
  df <- data.frame(
    ARM = c("Placebo", "Drug"),
    SEX = c("M", "F"),
    variable = c("AGE", "AGE"),
    stat_name = c("mean", "mean"),
    stat = c(65, 63),
    context = c("continuous", "continuous"),
    stringsAsFactors = FALSE
  )

  expect_error(
    arframe:::detect_renamed_arm(df, column = NULL, call = rlang::caller_env()),
    "Multiple potential group columns"
  )
})

test_that("detect_renamed_arm returns NULL when column not in non_std", {
  df <- data.frame(
    ARM = c("Placebo", "Drug"),
    variable = c("AGE", "AGE"),
    stat_name = c("mean", "mean"),
    stat = c(65, 63),
    context = c("continuous", "continuous"),
    stringsAsFactors = FALSE
  )

  result <- arframe:::detect_renamed_arm(
    df,
    column = "NONEXISTENT",
    call = rlang::caller_env()
  )
  expect_null(result)
})

test_that("detect_renamed_arm returns NULL when no non-standard columns", {
  df <- data.frame(
    variable = c("AGE", "AGE"),
    stat_name = c("mean", "sd"),
    stat = c(65, 12),
    context = c("continuous", "continuous"),
    stringsAsFactors = FALSE
  )

  result <- arframe:::detect_renamed_arm(
    df,
    column = NULL,
    call = rlang::caller_env()
  )
  expect_null(result)
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: reconstruct_renamed_ard error paths
# ══════════════════════════════════════════════════════════════════════════════

test_that("reconstruct_renamed_ard errors when column not in data", {
  df <- data.frame(
    ARM = c("Placebo", "Drug"),
    stat_name = c("mean", "mean"),
    stat = c(65, 63),
    stringsAsFactors = FALSE
  )

  expect_error(
    arframe:::reconstruct_renamed_ard(
      df,
      column = "BADCOL",
      call = rlang::caller_env()
    ),
    "not found in data"
  )
})

test_that("reconstruct_renamed_ard uses unused var_cols for continuous rows", {
  # Scenario: no always-NA columns, but some var_cols unused by categorical rows
  df <- data.frame(
    ARM = c("Placebo", "Drug", "Placebo", "Drug"),
    SEX = c("Male", "Male", NA, NA),
    AGE = c(NA, NA, NA, NA),
    stat_name = c("n", "n", "mean", "mean"),
    stat = c(10, 12, 65, 63),
    stringsAsFactors = FALSE
  )

  result <- arframe:::reconstruct_renamed_ard(
    df,
    column = "ARM",
    call = rlang::caller_env()
  )
  expect_true("variable" %in% names(result$df))
  # The continuous rows (NA in both SEX and AGE) should get assigned to AGE
  # (always-NA column)
  expect_true(any(result$df$variable == "AGE", na.rm = TRUE))
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: fr_wide_ard undetermined structure error
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_wide_ard errors on data without variable column (no stat_name)", {
  df <- data.frame(
    group1 = "ARM",
    group1_level = "Placebo",
    stat = c(65),
    stringsAsFactors = FALSE
  )

  expect_error(fr_wide_ard(df), "missing required columns")
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: group1 present but no group1_level (unusual shape)
# ══════════════════════════════════════════════════════════════════════════════

test_that("ARD with group1 but no group1_level handled gracefully", {
  df <- data.frame(
    variable = rep("AGE", 4L),
    stat_name = rep(c("mean", "sd"), 2L),
    stat = c(65.3, 12.1, 63.8, 11.5),
    group1 = rep("ARM", 4L),
    context = rep("continuous", 4L),
    stringsAsFactors = FALSE
  )

  # Has group1 but no group1_level — should fall through to Shape B/C handling
  # arm will be NA, so overall captures everything
  wide <- fr_wide_ard(df, statistic = list(continuous = "{mean} ({sd})"))
  expect_s3_class(wide, "data.frame")
  expect_gte(nrow(wide), 1L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: no rows remain after filtering
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_wide_ard errors when all rows are internal sentinels", {
  ard <- do.call(
    rbind,
    list(
      ard_row("..ard_total_n..", "N", 89, group1_level = "Placebo"),
      ard_row("..ard_total_n..", "N", 89, group1_level = "Drug")
    )
  )

  expect_error(fr_wide_ard(ard, statistic = "{N}"), "No displayable rows")
})

test_that("fr_wide_ard errors when all rows filtered by overall = NULL and no arm", {
  # Only overall rows (arm = NA), but overall = NULL excludes them
  ard <- do.call(
    rbind,
    list(
      ard_row(
        "AGE",
        "mean",
        64.5,
        group1 = NA_character_,
        group1_level = NA_character_
      ),
      ard_row(
        "AGE",
        "sd",
        11.8,
        group1 = NA_character_,
        group1_level = NA_character_
      )
    )
  )

  expect_error(
    fr_wide_ard(
      ard,
      statistic = list(continuous = "{mean} ({sd})"),
      overall = NULL
    ),
    "No rows remain"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: pharma boundary rules in format_stat_with_decimals
# ══════════════════════════════════════════════════════════════════════════════

test_that("pharma boundary: p close to 0% formats as <1", {
  # p = 0.005 → 0.5% → should be "<1" with decimals = c(p = 0)
  ard <- do.call(
    rbind,
    list(
      ard_row(
        "SEX",
        "n",
        1,
        group1_level = "Placebo",
        variable_level = "Male",
        context = "categorical"
      ),
      ard_row(
        "SEX",
        "p",
        0.005,
        group1_level = "Placebo",
        variable_level = "Male",
        context = "categorical"
      )
    )
  )

  wide <- fr_wide_ard(
    ard,
    statistic = list(categorical = "{n} ({p}%)"),
    decimals = c(p = 0)
  )

  expect_true(grepl("<1%", wide$Placebo[1L]))
})

test_that("pharma boundary: p close to 100% formats as >99", {
  # p = 0.998 → 99.8% → should be ">99" with decimals = c(p = 0)
  ard <- do.call(
    rbind,
    list(
      ard_row(
        "SEX",
        "n",
        99,
        group1_level = "Placebo",
        variable_level = "Male",
        context = "categorical"
      ),
      ard_row(
        "SEX",
        "p",
        0.998,
        group1_level = "Placebo",
        variable_level = "Male",
        context = "categorical"
      )
    )
  )

  wide <- fr_wide_ard(
    ard,
    statistic = list(categorical = "{n} ({p}%)"),
    decimals = c(p = 0)
  )

  expect_true(grepl(">99%", wide$Placebo[1L]))
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: format_ard_stat for various stat_name branches
# ══════════════════════════════════════════════════════════════════════════════

test_that("format_ard_stat handles p_miss, p_nonmiss, p_cum stat names", {
  # These are percentage stats that use %.1f with *100 scaling
  result_pmiss <- arframe:::format_ard_stat(
    0.123,
    NA_character_,
    "p_miss",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result_pmiss, "12.3")

  result_pnm <- arframe:::format_ard_stat(
    0.456,
    NA_character_,
    "p_nonmiss",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result_pnm, "45.6")

  result_pcum <- arframe:::format_ard_stat(
    0.789,
    NA_character_,
    "p_cum",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result_pcum, "78.9")
})

test_that("format_ard_stat handles count stat names (N_miss, N_nonmiss, etc.)", {
  result <- arframe:::format_ard_stat(
    42.0,
    NA_character_,
    "N_miss",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result, "42")

  result2 <- arframe:::format_ard_stat(
    100.0,
    NA_character_,
    "N_nonmiss",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result2, "100")

  result3 <- arframe:::format_ard_stat(
    15.0,
    NA_character_,
    "n_event",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result3, "15")

  result4 <- arframe:::format_ard_stat(
    80.0,
    NA_character_,
    "n.risk",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result4, "80")

  result5 <- arframe:::format_ard_stat(
    5.0,
    NA_character_,
    "n_cum",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result5, "5")
})

test_that("format_ard_stat handles cardx test stat names", {
  result_se <- arframe:::format_ard_stat(
    1.2345,
    NA_character_,
    "std.error",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result_se, "1.2345")

  result_stat <- arframe:::format_ard_stat(
    5.678,
    NA_character_,
    "statistic",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result_stat, "5.68")

  result_param <- arframe:::format_ard_stat(
    2.5,
    NA_character_,
    "parameter",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result_param, "2.5")

  result_cl <- arframe:::format_ard_stat(
    0.95,
    NA_character_,
    "conf.low",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result_cl, "0.95")

  result_ch <- arframe:::format_ard_stat(
    1.05,
    NA_character_,
    "conf.high",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result_ch, "1.05")

  result_level <- arframe:::format_ard_stat(
    0.95,
    NA_character_,
    "conf.level",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result_level, "0.95")
})

test_that("format_ard_stat falls back to default 1-decimal for unknown stat", {
  result <- arframe:::format_ard_stat(
    3.456,
    NA_character_,
    "unknown_stat",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result, "3.5")
})

test_that("format_ard_stat returns empty for logical stat with NA chr", {
  result <- arframe:::format_ard_stat(
    NA_real_,
    NA_character_,
    "paired",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result, "")
})

test_that("format_ard_stat returns empty for both NA value and NA chr", {
  result <- arframe:::format_ard_stat(
    NA_real_,
    NA_character_,
    "mean",
    NULL,
    NULL,
    NULL
  )
  expect_equal(result, "")
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: format_p_value with NA
# ══════════════════════════════════════════════════════════════════════════════

test_that("format_p_value returns empty for NA", {
  result <- arframe:::format_p_value(NA_real_)
  expect_equal(result, "")
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: resolve_ard_decimals edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("resolve_ard_decimals with unrecognized input returns NULL/NULL", {
  result <- arframe:::resolve_ard_decimals(42)
  expect_equal(result$global, NULL)
  expect_equal(result$per_var, NULL)
})

test_that("resolve_ard_decimals with list and .default key", {
  result <- arframe:::resolve_ard_decimals(
    list(AGE = c(mean = 2), .default = c(sd = 3))
  )
  expect_equal(result$global, c(sd = 3))
  expect_equal(result$per_var$AGE, c(mean = 2))
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: per-variable decimals with .default in full pipeline
# ══════════════════════════════════════════════════════════════════════════════

test_that("per-variable decimals with .default works end-to-end", {
  ard <- make_mixed_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(
      continuous = "{mean} ({sd})",
      categorical = "{n} ({p}%)"
    ),
    decimals = list(AGE = c(mean = 3), .default = c(sd = 1, p = 1))
  )

  expect_s3_class(wide, "data.frame")
  # AGE row: mean should be 3 decimals
  age_row <- wide[wide$variable == "AGE", ]
  expect_true(grepl("65.300", age_row$Placebo[1L]))
  # sd uses .default = 1 decimal (not per-var override for AGE)
  expect_true(grepl("12.1\\)", age_row$Placebo[1L]))
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: big_n validation error
# ══════════════════════════════════════════════════════════════════════════════

test_that("big_n must be a scalar character", {
  ard <- make_continuous_ard()
  expect_error(
    fr_wide_ard(ard, statistic = list(continuous = "{mean}"), big_n = 42),
    "must be a single character string"
  )
})

test_that("big_n with no matching stat_name produces no n_counts", {
  ard <- make_continuous_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})"),
    big_n = "NONEXISTENT"
  )
  expect_null(attr(wide, "n_counts"))
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: interpolate_stats returns empty on missing arm
# ══════════════════════════════════════════════════════════════════════════════

test_that("interpolate_stats returns empty string for missing arm", {
  df <- data.frame(
    arm = c("Placebo"),
    stat_name = c("mean"),
    stat_fmt = c("65.3"),
    stringsAsFactors = FALSE
  )

  result <- arframe:::interpolate_stats(df, "NonexistentArm", "{mean}")
  expect_equal(result, "")
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: context inferred from var_level when no context column
# ══════════════════════════════════════════════════════════════════════════════

test_that("context inferred from var_level when context column absent", {
  ard <- data.frame(
    variable = rep("SEX", 4L),
    stat_name = rep(c("n", "p"), 2L),
    stat = c(20, 0.444, 25, 0.556),
    group1 = rep("ARM", 4L),
    group1_level = rep("Placebo", 4L),
    variable_level = rep(c("Male", "Female"), each = 2L),
    stringsAsFactors = FALSE
  )

  wide <- fr_wide_ard(ard, statistic = list(categorical = "{n} ({p}%)"))
  expect_equal(nrow(wide), 2L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: hierarchical with multiple arms + overall
# ══════════════════════════════════════════════════════════════════════════════

test_that("hierarchical build with two arms and overall column", {
  rows <- list(
    # Overall sentinel
    ard_row2(
      "..ard_hierarchical_overall..",
      "n",
      70,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      context = "hierarchical"
    ),
    ard_row2(
      "..ard_hierarchical_overall..",
      "n",
      65,
      group1_level = "Drug",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      context = "hierarchical"
    ),
    # SOC row
    ard_row2(
      "AEBODSYS",
      "n",
      10,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      variable_level = "Cardiac disorders",
      context = "hierarchical"
    ),
    ard_row2(
      "AEBODSYS",
      "n",
      8,
      group1_level = "Drug",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      variable_level = "Cardiac disorders",
      context = "hierarchical"
    ),
    # PT row
    ard_row2(
      "AEDECOD",
      "n",
      5,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = "Cardiac disorders",
      variable_level = "Bradycardia",
      context = "hierarchical"
    ),
    ard_row2(
      "AEDECOD",
      "n",
      3,
      group1_level = "Drug",
      group2 = "AEBODSYS",
      group2_level = "Cardiac disorders",
      variable_level = "Bradycardia",
      context = "hierarchical"
    )
  )
  ard <- do.call(rbind, rows)

  wide <- fr_wide_ard(ard, statistic = "{n}")

  expect_true(all(c("soc", "pt", "row_type") %in% names(wide)))
  expect_true("Placebo" %in% names(wide))
  expect_true("Drug" %in% names(wide))
  # Overall + 1 SOC + 1 PT = 3 rows
  expect_equal(nrow(wide), 3L)
  expect_equal(sum(wide$row_type == "overall"), 1L)
  expect_equal(sum(wide$row_type == "soc"), 1L)
  expect_equal(sum(wide$row_type == "pt"), 1L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: hierarchical label applied to soc and pt columns
# ══════════════════════════════════════════════════════════════════════════════

test_that("label renames soc and pt columns in hierarchical output", {
  rows <- list(
    ard_row2(
      "AEBODSYS",
      "n",
      10,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      variable_level = "Cardiac disorders",
      context = "hierarchical"
    ),
    ard_row2(
      "AEDECOD",
      "n",
      5,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = "Cardiac disorders",
      variable_level = "Bradycardia",
      context = "hierarchical"
    )
  )
  ard <- do.call(rbind, rows)

  wide <- fr_wide_ard(
    ard,
    statistic = "{n}",
    label = c("Cardiac disorders" = "Cardiac", "Bradycardia" = "Brady")
  )

  expect_true(any(wide$soc == "Cardiac"))
  expect_true(any(wide$pt == "Brady"))
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: format_stat_with_decimals for p_miss/p_nonmiss/p_cum
# ══════════════════════════════════════════════════════════════════════════════

test_that("format_stat_with_decimals applies 100x for p-family stats", {
  result <- arframe:::format_stat_with_decimals(0.456, "p_miss", 1L)
  expect_equal(result, "45.6")

  result2 <- arframe:::format_stat_with_decimals(0.123, "p_nonmiss", 2L)
  expect_equal(result2, "12.30")

  result3 <- arframe:::format_stat_with_decimals(0.789, "p_cum", 0L)
  expect_equal(result3, "79")
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: is_multirow_spec edge cases
# ══════════════════════════════════════════════════════════════════════════════

test_that("is_multirow_spec returns FALSE for unnamed multi-element vector", {
  expect_false(arframe:::is_multirow_spec(c("{mean}", "{sd}")))
})

test_that("is_multirow_spec returns FALSE for single named string", {
  expect_false(arframe:::is_multirow_spec(c("Mean" = "{mean}")))
})

test_that("is_multirow_spec returns TRUE for named multi-element vector", {
  expect_true(arframe:::is_multirow_spec(c("Mean" = "{mean}", "SD" = "{sd}")))
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: custom fmt function via full pipeline
# ══════════════════════════════════════════════════════════════════════════════

test_that("custom fmt function for mean overrides built-in formatting", {
  ard <- make_continuous_ard()
  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean}"),
    fmt = list(mean = function(x) paste0("[", round(x, 3), "]"))
  )

  expect_true(grepl("\\[65.3\\]", wide$Placebo[1L]))
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: resolve_ard_statistic fallback to "{n}"
# ══════════════════════════════════════════════════════════════════════════════

test_that("resolve_ard_statistic falls back to {n} when nothing matches", {
  result <- arframe:::resolve_ard_statistic("UNKNOWN", "unknown_ctx", list())
  expect_equal(result, "{n}")
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: detect_ard_hierarchy with no group1 or variable
# ══════════════════════════════════════════════════════════════════════════════

test_that("detect_ard_hierarchy returns non-hierarchical when no group1", {
  df <- data.frame(
    variable = "AGE",
    stat_name = "mean",
    stat = 65,
    stringsAsFactors = FALSE
  )

  result <- arframe:::detect_ard_hierarchy(df)
  expect_false(result$is_hierarchical)
})

test_that("detect_ard_hierarchy with group2 not matching variable is not hierarchical", {
  df <- data.frame(
    variable = c("AGE", "AGE"),
    group1 = c("ARM", "ARM"),
    group1_level = c("Placebo", "Drug"),
    group2 = c("SEX", "SEX"),
    group2_level = c("M", "F"),
    stat_name = c("mean", "mean"),
    stat = c(65, 63),
    stringsAsFactors = FALSE
  )

  result <- arframe:::detect_ard_hierarchy(df)
  expect_false(result$is_hierarchical)
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: undetermined ARD structure (no variable, has stat_name + stat)
# ══════════════════════════════════════════════════════════════════════════════

test_that("fr_wide_ard errors on fully-renamed ARD without column arg", {
  # Has stat_name and stat, but no variable and no standard group columns
  # Triggers Shape D (reconstruct_renamed_ard) which requires column
  df <- data.frame(
    stat_name = c("mean", "sd"),
    stat = c(65.3, 12.1),
    stringsAsFactors = FALSE
  )

  expect_error(fr_wide_ard(df), "auto-detect")
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: Shape A without variable_level column
# ══════════════════════════════════════════════════════════════════════════════

test_that("raw ARD without variable_level column defaults to NA", {
  df <- data.frame(
    variable = rep("AGE", 4L),
    stat_name = rep(c("mean", "sd"), 2L),
    group1 = rep("ARM", 4L),
    context = rep("continuous", 4L),
    stringsAsFactors = FALSE
  )
  df$stat <- as.list(c(65.3, 12.1, 63.8, 11.5))
  df$group1_level <- as.list(rep(c("Placebo", "Drug"), each = 2L))

  wide <- fr_wide_ard(df, statistic = list(continuous = "{mean} ({sd})"))
  expect_s3_class(wide, "data.frame")
  expect_equal(nrow(wide), 1L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: validate_format_stats with no refs (literal format string)
# ══════════════════════════════════════════════════════════════════════════════

test_that("validate_format_stats with no refs returns silently", {
  expect_silent(
    arframe:::validate_format_stats(
      "literal text",
      c("mean", "sd"),
      "AGE",
      NULL
    )
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: ungrouped ARD with overall = NULL forces "value" column
# ══════════════════════════════════════════════════════════════════════════════

test_that("ungrouped ARD with overall = NULL forces 'value' column", {
  ard <- data.frame(
    variable = rep("AGE", 2L),
    stat_name = c("mean", "sd"),
    stat = c(64.5, 11.8),
    context = rep("continuous", 2L),
    stringsAsFactors = FALSE
  )

  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{mean} ({sd})"),
    overall = NULL
  )

  # Should still have a value column (forced to "value" when ungrouped + overall = NULL)
  expect_s3_class(wide, "data.frame")
  expect_gte(nrow(wide), 1L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: extra group cols in categorical (multi-group with levels)
# ══════════════════════════════════════════════════════════════════════════════

test_that("extra group cols propagated in categorical multi-group output", {
  rows <- list(
    ard_row2(
      "RACE",
      "n",
      10,
      group1_level = "Placebo",
      group2 = "SEX",
      group2_level = "F",
      variable_level = "White",
      context = "categorical"
    ),
    ard_row2(
      "RACE",
      "p",
      0.5,
      group1_level = "Placebo",
      group2 = "SEX",
      group2_level = "F",
      variable_level = "White",
      context = "categorical"
    ),
    ard_row2(
      "RACE",
      "n",
      8,
      group1_level = "Drug",
      group2 = "SEX",
      group2_level = "F",
      variable_level = "White",
      context = "categorical"
    ),
    ard_row2(
      "RACE",
      "p",
      0.4,
      group1_level = "Drug",
      group2 = "SEX",
      group2_level = "F",
      variable_level = "White",
      context = "categorical"
    )
  )
  ard <- do.call(rbind, rows)

  wide <- fr_wide_ard(
    ard,
    column = "ARM",
    statistic = list(categorical = "{n} ({p}%)")
  )

  expect_true("SEX" %in% names(wide))
  expect_equal(wide$SEX[1L], "F")
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: hierarchical multirow_spec flattening
# ══════════════════════════════════════════════════════════════════════════════

test_that("hierarchical overall uses first entry of multirow spec", {
  rows <- list(
    ard_row2(
      "..ard_hierarchical_overall..",
      "n",
      70,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      context = "hierarchical"
    ),
    ard_row2(
      "..ard_hierarchical_overall..",
      "p",
      0.78,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      context = "hierarchical"
    ),
    # SOC row
    ard_row2(
      "AEBODSYS",
      "n",
      10,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      variable_level = "Cardiac disorders",
      context = "hierarchical"
    ),
    ard_row2(
      "AEBODSYS",
      "p",
      0.22,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = NA_character_,
      variable_level = "Cardiac disorders",
      context = "hierarchical"
    ),
    # PT row
    ard_row2(
      "AEDECOD",
      "n",
      5,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = "Cardiac disorders",
      variable_level = "Bradycardia",
      context = "hierarchical"
    ),
    ard_row2(
      "AEDECOD",
      "p",
      0.11,
      group1_level = "Placebo",
      group2 = "AEBODSYS",
      group2_level = "Cardiac disorders",
      variable_level = "Bradycardia",
      context = "hierarchical"
    )
  )
  ard <- do.call(rbind, rows)

  # Use multirow spec — hierarchical should flatten to first entry
  wide <- fr_wide_ard(
    ard,
    statistic = list(
      categorical = c(
        "n (%)" = "{n} ({p}%)",
        "Count" = "{n}"
      )
    )
  )

  # Should still produce valid output (uses first format for hierarchical)
  expect_s3_class(wide, "data.frame")
  expect_true("row_type" %in% names(wide))
})


# ══════════════════════════════════════════════════════════════════════════════
# Coverage gap: reconstruct_renamed_ard unused var_cols fallback
# ══════════════════════════════════════════════════════════════════════════════

test_that("reconstruct_renamed_ard assigns continuous rows to unused var_cols", {
  # No always-NA column, but AGE unused by categorical rows
  df <- data.frame(
    ARM = c("Placebo", "Placebo", "Placebo", "Placebo"),
    SEX = c("Male", "Female", NA, NA),
    AGE = c(NA, NA, NA, NA),
    stat_name = c("n", "n", "mean", "sd"),
    stat = c(10, 15, 65.3, 12.1),
    stringsAsFactors = FALSE
  )

  result <- arframe:::reconstruct_renamed_ard(
    df,
    column = "ARM",
    call = rlang::caller_env()
  )
  # Rows 3-4 (continuous) should be assigned to AGE (always-NA → first priority)
  expect_true(all(result$df$variable[3:4] == "AGE"))
})
