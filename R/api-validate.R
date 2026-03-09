# ──────────────────────────────────────────────────────────────────────────────
# api-validate.R — Pre-render validation: fr_validate()
#
# Catches misconfigurations before render time, giving users instant feedback.
# ──────────────────────────────────────────────────────────────────────────────


#' Validate a Table Specification Before Rendering
#'
#' @description
#'
#' Checks a configured `fr_spec` for common misconfigurations that would cause
#' errors or unexpected output at render time. Returns the spec invisibly when
#' valid, making it pipeline-friendly. In strict mode, any issue raises an
#' error.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param strict Logical. If `TRUE`, any validation issue raises an error.
#'   If `FALSE` (default), issues are reported as warnings and the spec
#'   is returned invisibly.
#'
#' @return Invisibly returns `spec` when valid (or when `strict = FALSE`).
#'   Raises an error in strict mode when issues are found.
#'
#' @section Checks performed:
#' 1. Column names in `fr_cols()` exist in data
#' 2. `page_by`, `group_by`, `indent_by`, `blank_after` columns exist
#' 3. `stub_cols` exist in data
#' 4. N-count column names match column specs
#' 5. Span columns exist and are contiguous
#' 6. Column widths sum to printable area (warning if >110%)
#' 7. Style row/col indices are in valid range
#' 8. Font family is recognised
#' 9. `sort_by` / `repeat_cols` columns exist (for listings)
#'
#' @examples
#' ## ── Clean spec passes validation silently ──────────────────────────────────
#'
#' spec <- tbl_demog |>
#'   fr_table() |>
#'   fr_hlines("header")
#'
#' spec |> fr_validate()
#'
#' ## ── Strict mode on a clean spec (no error) ────────────────────────────────
#'
#' spec |> fr_validate(strict = TRUE)
#'
#' ## ── Spec with a validation issue: bad column in column specs ────────────────
#'
#' # Manually inject a column spec for a non-existent column
#' # (fr_cols() validates eagerly, so we set it directly for demo)
#' bad_spec <- tbl_demog |> fr_table()
#' bad_spec$columns[["nonexistent"]] <- fr_col("Bad Column")
#'
#' # Non-strict mode: warns but returns the spec
#' bad_spec |> fr_validate()
#'
#' # Strict mode errors (wrapped in tryCatch for safety)
#' tryCatch(
#'   bad_spec |> fr_validate(strict = TRUE),
#'   error = function(e) message("Caught: ", conditionMessage(e))
#' )
#'
#' ## ── Validate before rendering in a pipeline ───────────────────────────────
#'
#' out <- file.path(tempdir(), "validated.rtf")
#' tbl_ae_soc |>
#'   fr_table() |>
#'   fr_titles("Table 14.3.1", "AE by SOC") |>
#'   fr_hlines("header") |>
#'   fr_validate() |>
#'   fr_render(out)
#' unlink(out)
#'
#' @seealso [fr_render()], [fr_table()]
#' @export
fr_validate <- function(spec, strict = FALSE) {
  call <- caller_env()
  check_fr_spec(spec, call = call)
  check_scalar_lgl(strict, arg = "strict", call = call)

  issues <- character(0)
  data_names <- names(spec$data)
  nr <- nrow(spec$data)

  # 1. Column specs reference valid data columns
  if (length(spec$columns) > 0L) {
    col_spec_names <- names(spec$columns)
    # Skip gap columns (injected by finalize)
    real_cols <- col_spec_names[!startsWith(col_spec_names, ".__")]
    bad <- setdiff(real_cols, data_names)
    if (length(bad) > 0L) {
      issues <- c(issues, cli::format_inline(
        "Column{?s} in {.fn fr_cols} not found in data: {.val {bad}}."
      ))
    }
  }

  # 2. Row config columns exist
  body <- spec$body
  if (!is.null(body)) {
    for (field in c("page_by", "group_by", "indent_by", "blank_after")) {
      cols <- body[[field]]
      if (length(cols) > 0L) {
        bad <- setdiff(cols, data_names)
        if (length(bad) > 0L) {
          issues <- c(issues, cli::format_inline(
            "{.arg {field}} column{?s} not found in data: {.val {bad}}."
          ))
        }
      }
    }
    # sort_by / repeat_cols (listing params)
    if (length(body$sort_by) > 0L) {
      bad <- setdiff(body$sort_by, data_names)
      if (length(bad) > 0L) {
        issues <- c(issues, cli::format_inline(
          "{.arg sort_by} column{?s} not found in data: {.val {bad}}."
        ))
      }
    }
    if (is.character(body$repeat_cols) && length(body$repeat_cols) > 0L) {
      bad <- setdiff(body$repeat_cols, data_names)
      if (length(bad) > 0L) {
        issues <- c(issues, cli::format_inline(
          "{.arg repeat_cols} column{?s} not found in data: {.val {bad}}."
        ))
      }
    }
  }

  # 3. stub columns exist
  if (length(spec$columns) > 0L) {
    stub_cols <- stub_column_names(spec$columns)
    if (length(stub_cols) > 0L) {
      bad <- setdiff(stub_cols, data_names)
      if (length(bad) > 0L) {
        issues <- c(issues, cli::format_inline(
          "Stub column{?s} not found in data: {.val {bad}}."
        ))
      }
    }
  }

  # 4. N-count names match column specs
  n_counts <- spec$header$n
  if (is.numeric(n_counts) && !is.null(names(n_counts))) {
    col_names <- if (length(spec$columns) > 0L) names(spec$columns) else data_names
    bad <- setdiff(names(n_counts), col_names)
    if (length(bad) > 0L) {
      issues <- c(issues, cli::format_inline(
        "N-count name{?s} don't match column specs: {.val {bad}}."
      ))
    }
  }

  # 5. Span columns exist and are contiguous
  spans <- spec$header$spans %||% list()
  if (length(spans) > 0L) {
    vis_names <- if (length(spec$columns) > 0L) {
      names(visible_columns(spec$columns))
    } else {
      data_names
    }
    for (i in seq_along(spans)) {
      sp <- spans[[i]]
      bad <- setdiff(sp$columns, c(vis_names, data_names))
      if (length(bad) > 0L) {
        issues <- c(issues, cli::format_inline(
          "Span {i} references non-existent column{?s}: {.val {bad}}."
        ))
      }
    }
  }

  # 6. Column widths vs printable area
  split_mode <- spec$columns_meta$split
  if (length(spec$columns) > 0L &&
      (identical(split_mode, FALSE) || is.null(split_mode))) {
    vis_cols <- visible_columns(spec$columns)
    widths <- vapply(vis_cols, function(c) {
      w <- c$width
      if (is.numeric(w) && !is_fr_pct(w)) w else NA_real_
    }, numeric(1))
    if (!any(is.na(widths))) {
      total <- sum(widths)
      printable <- printable_area_inches(spec$page)[["width"]]
      if (total > printable * 1.1) {
        issues <- c(issues, cli::format_inline(
          "Column widths ({round(total, 2)}in) exceed 110% of printable area ({round(printable, 2)}in). Consider {.code fr_cols(.split = TRUE)} or narrower widths."
        ))
      }
    }
  }

  # 7. Style row/col indices in range
  for (i in seq_along(spec$cell_styles)) {
    style <- spec$cell_styles[[i]]
    if (is.integer(style$rows) && any(style$rows > nr)) {
      issues <- c(issues, cli::format_inline(
        "Style {i}: row index {max(style$rows)} exceeds data rows ({nr})."
      ))
    }
  }

  # 8. Font family recognised
  font <- spec$page$font_family
  if (!is.null(font)) {
    known <- unlist(lapply(fr_env$fonts, function(f) f$names), use.names = FALSE)
    if (!font %in% known) {
      issues <- c(issues, cli::format_inline(
        "Font family {.val {font}} not recognised. Metrics may be inaccurate."
      ))
    }
  }

  # Report
  if (length(issues) > 0L) {
    msg <- c(
      "!" = "{length(issues)} validation issue{?s} found:",
      set_names(issues, rep("*", length(issues)))
    )
    if (strict) {
      cli_abort(msg, call = call)
    } else {
      cli_warn(msg)
    }
  }

  invisible(spec)
}
