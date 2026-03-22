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

  wide <- fr_wide_ard(
    ard,
    statistic = list(continuous = "{p.value} ({method})")
  )

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

test_that("extract_glue_refs handles escaped braces without false positives", {
  # Escaped braces {{literal}} should NOT be extracted as refs
  expect_equal(arframe:::extract_glue_refs("{{literal}} {n}"), "n")
  expect_equal(arframe:::extract_glue_refs("{{foo}}"), character(0L))
  expect_equal(arframe:::extract_glue_refs("{mean} ({sd})"), c("mean", "sd"))
  expect_equal(arframe:::extract_glue_refs("no refs here"), character(0L))
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
