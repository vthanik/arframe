# Convert ARD (Long) to Wide Summary Data Frame

Converts Analysis Results Data (ARD) from packages like
[cards](https://insightsengineering.github.io/cards/) into a wide
summary data frame suitable for
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).
ARD stores one row per statistic; `fr_wide_ard()` combines statistics
using format strings, pivots treatment arms to columns, and returns a
display-ready data frame.

This function has **no dependency on cards** — it accepts any data frame
with the standard ARD columns (`variable`, `stat_name`, `stat`).

Handles all common post-`ard_stack()` pipeline shapes:

- Raw `ard_stack()` output (list columns)

- After `unlist_ard_columns()` (atomic columns)

- After `rename_ard_columns(columns = all_ard_groups("names"))` (group
  columns renamed to e.g. `ARM`)

- After default `rename_ard_columns()` (both groups and variables
  renamed)

- Multi-group `.by = c(ARM, SEX)` — extra groups preserved in output

- No `.by` at all (ungrouped)

## Usage

``` r
fr_wide_ard(
  data,
  statistic = list(continuous = "{mean} ({sd})", categorical = "{n} ({p}%)"),
  column = NULL,
  label = NULL,
  overall = "Total",
  decimals = NULL,
  fmt = NULL,
  big_n = NULL
)
```

## Arguments

- data:

  A data frame with ARD columns. At minimum, requires `stat_name` and
  `stat`. Standard ARD also has `variable`, `group1`/`group1_level`,
  `variable_level`, and `context`. Renamed ARD (after
  `rename_ard_columns()`) is also accepted. Typically produced by
  [`cards::ard_stack()`](https://insightsengineering.github.io/cards/latest-tag/reference/ard_stack.html).

- statistic:

  Format strings for combining statistics into display cells. Accepts
  three forms:

  **Form 1: Named list by context** (most common)

      list(
        categorical = "{n} ({p}%)",
        continuous  = c("Mean (SD)" = "{mean} ({sd})", Median = "{median}")
      )

  Categorical: one format string produces one row per category level.
  Continuous: a named character vector produces one row per entry, with
  names becoming row labels (e.g., "Mean (SD)", "Median").

  **Form 2: Named list by variable** (per-variable override)

      list(
        AGE = c(n = "{N}", "Mean (SD)" = "{mean} ({sd})"),
        SEX = "{n} ({p}%)",
        default = "{n} ({p}%)"
      )

  **Form 3: Single string** (applied to everything)

      "{n} ({p}%)"

  Default:
  `list(continuous = "{mean} ({sd})", categorical = "{n} ({p}%)")`.

- column:

  Character scalar. The grouping variable that becomes the display
  columns (e.g., `"ARM"`, `"DOSGRP"`). For raw ARD, this is the value in
  `group1` (auto-detected if `NULL`). For renamed ARD, this is the
  column name in the data frame. If `NULL` (default), auto-detected from
  the data.

- label:

  Named character vector mapping variable names to display labels. E.g.,
  `c(AGE = "Age (years)", SEX = "Sex, n (%)")`. For hierarchical ARD,
  also renames sentinel variables:
  `c("..ard_hierarchical_overall.." = "Patients with Any TEAE")`. If
  `NULL`, uses `variable` values as-is.

- overall:

  Character scalar. Column header for overall/total rows (where `group1`
  is `NA`). Default `"Total"`. Set to `NULL` to exclude overall rows.

- decimals:

  Decimal precision for stat formatting. Accepts two forms:

  **Global defaults** — a named integer vector:

      c(mean = 1, sd = 2, p = 0, median = 1)

  **Per-variable overrides** — a named list:

      list(
        AGE  = c(mean = 1, sd = 2),
        BMIBL = c(mean = 2, sd = 3),
        .default = c(mean = 1, sd = 2, p = 0)
      )

  Overrides built-in defaults. Default: `NULL`.

- fmt:

  Named list of custom format functions keyed by `stat_name`. Each
  function takes a numeric value and returns a character string. E.g.,
  `list(p.value = function(x) if (x < 0.001) "<0.001" else sprintf("%.3f", x))`.
  Default: `NULL`.

- big_n:

  Character scalar. The `stat_name` value used for population counts
  (BigN) in the ARD (e.g., `"BigN"`, `"bigN"`, `"popn"`). When
  specified, rows with this `stat_name` are extracted from the body and
  attached as `attr(result, "n_counts")` — a named numeric vector
  suitable for `fr_cols(.n =)`. Default: `NULL` (no extraction).

## Value

A data frame with columns:

- Extra group columns (if multi-group):

  e.g., `SEX`, `PARAMCD`

- `variable`:

  Grouping variable (for use with `fr_rows(group_by =)`)

- `stat_label`:

  Row label (statistic name or category level)

- One column per treatment arm:

  Formatted display values

- `Total` (if overall present):

  Overall column

For hierarchical ARD (SOC/PT), additional columns:

- `soc`:

  System organ class value

- `pt`:

  Preferred term value

- `row_type`:

  `"soc"` or `"pt"`

If `big_n` is specified, `attr(result, "n_counts")` contains a named
numeric vector of population counts per arm.

Pipe directly to
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md):

    ard |> fr_wide_ard() |> fr_table() |> fr_cols(...) |> fr_render("out.pdf")

## See also

[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)
for the pipeline entry point.

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_stack(
    data = arframe::adsl[arframe::adsl$SAFFL == "Y", ],
    .by = "ARM",
    cards::ard_continuous(variables = "AGE"),
    cards::ard_categorical(variables = "SEX"),
    .overall = TRUE
  )

  # --- Scenario 1: Multi-row continuous output ---
  wide <- fr_wide_ard(ard,
    statistic = list(
      continuous = c(
        n           = "{N}",
        "Mean (SD)" = "{mean} ({sd})",
        Median      = "{median}",
        "Min, Max"  = "{min}, {max}"
      ),
      categorical = "{n} ({p}%)"
    ),
    label = c(AGE = "Age (years)", SEX = "Sex"),
    decimals = c(mean = 1, sd = 2, p = 0)
  )

  print(wide)

  # --- Scenario 2: Per-variable decimals ---
  wide2 <- fr_wide_ard(ard,
    statistic = list(
      continuous = c("Mean (SD)" = "{mean} ({sd})"),
      categorical = "{n} ({p}%)"
    ),
    decimals = list(AGE = c(mean = 2, sd = 3), .default = c(p = 1))
  )

  # --- Scenario 3: Single string statistic ---
  cat_ard <- cards::ard_stack(
    data = arframe::adsl[arframe::adsl$SAFFL == "Y", ],
    .by = "ARM",
    cards::ard_categorical(variables = "SEX")
  )
  wide3 <- fr_wide_ard(cat_ard, statistic = "{n} ({p}%)")

  # --- Scenario 4: Pipe into arframe ---
  wide |>
    fr_table() |>
    fr_cols(
      variable   = fr_col(visible = FALSE),
      stat_label = fr_col("", width = 2.5),
      .align = "decimal"
    ) |>
    fr_rows(group_by = list(cols = "variable", label = "stat_label"))
}
#>      variable  stat_label     Placebo Zomerane 100mg Zomerane 50mg       Total
#> 1 Age (years)           n          45             45            45         135
#> 2 Age (years)   Mean (SD) 75.0 (6.75)    75.3 (7.09)   73.1 (8.43) 74.4 (7.46)
#> 3 Age (years)      Median        74.0           73.0          74.0        74.0
#> 4 Age (years)    Min, Max  65.0, 88.0     55.0, 88.0    55.0, 88.0  55.0, 88.0
#> 5         Sex           F    27 (60%)       20 (44%)      28 (62%)    75 (56%)
#> 6         Sex           M    18 (40%)       25 (56%)      17 (38%)    60 (44%)
#> 
#> ── fr_spec: Table 
#> Data: 6 rows x 6 columns
#> Page: landscape letter, 9pt Times New Roman
#> Columns (5 visible of 6):
#> stat_label "" 2.50in decimal
#> Placebo "Placebo" 0.61in decimal
#> Zomerane 100mg "Zomerane 100mg" 1.00in decimal
#> Zomerane 50mg "Zomerane 50mg" 0.93in decimal
#> Total "Total" 0.61in decimal
#> Header: valign=bottom
#> Rows: group_by=variable (label=stat_label), indent_by=stat_label
```
