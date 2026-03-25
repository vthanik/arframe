# Validate a Table Specification Before Rendering

Checks a configured `fr_spec` for common misconfigurations that would
cause errors or unexpected output at render time. Returns the spec
invisibly when valid, making it pipeline-friendly. In strict mode, any
issue raises an error.

## Usage

``` r
fr_validate(x, ...)

# Default S3 method
fr_validate(x, ...)

# S3 method for class 'fr_spec'
fr_validate(x, ..., strict = FALSE)
```

## Arguments

- x:

  An `fr_spec` object from
  [`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md).

- ...:

  Additional arguments (currently unused).

- strict:

  Logical. If `TRUE`, any validation issue raises an error. If `FALSE`
  (default), issues are reported as warnings and the spec is returned
  invisibly.

## Value

Invisibly returns `spec` when valid (or when `strict = FALSE`). Raises
an error in strict mode when issues are found.

## Checks performed

1.  Column names in
    [`fr_cols()`](https://vthanik.github.io/arframe/reference/fr_cols.md)
    exist in data

2.  `page_by`, `group_by`, `indent_by`, `blank_after` columns exist

3.  `stub_cols` exist in data

4.  N-count column names match column specs

5.  Span columns exist and are contiguous

6.  Column widths sum to printable area (warning if \>110%)

7.  Style row/col indices are in valid range

8.  Font family is recognised

9.  `sort_by` / `suppress` columns exist (for listings)

## See also

[`fr_render()`](https://vthanik.github.io/arframe/reference/fr_render.md),
[`fr_table()`](https://vthanik.github.io/arframe/reference/fr_table.md)

## Examples

``` r
## ── Clean spec passes validation silently ──────────────────────────────────

spec <- tbl_demog |>
  fr_table() |>
  fr_hlines("header")

spec |> fr_validate()

## ── Strict mode on a clean spec (no error) ────────────────────────────────

spec |> fr_validate(strict = TRUE)

## ── Spec with a validation issue: bad column in column specs ────────────────

# Manually inject a column spec for a non-existent column
# (fr_cols() validates eagerly, so we set it directly for demo)
bad_spec <- tbl_demog |> fr_table()
bad_spec$columns[["nonexistent"]] <- fr_col("Bad Column")

# Non-strict mode: warns but returns the spec
bad_spec |> fr_validate()
#> Warning: ! 1 validation issue found:
#> • Column in `fr_cols()` not found in data: "nonexistent".

# Strict mode errors (wrapped in tryCatch for safety)
tryCatch(
  bad_spec |> fr_validate(strict = TRUE),
  error = function(e) message("Caught: ", conditionMessage(e))
)
#> Caught: ! 1 validation issue found:
#> • Column in `fr_cols()` not found in data: "nonexistent".

## ── Validate before rendering in a pipeline ───────────────────────────────

out <- file.path(tempdir(), "validated.rtf")
tbl_ae_soc |>
  fr_table() |>
  fr_titles("Table 14.3.1", "AE by SOC") |>
  fr_hlines("header") |>
  fr_validate() |>
  fr_render(out)
unlink(out)
```
