# ──────────────────────────────────────────────────────────────────────────────
# validate.R — Input validation helpers and sentinel utilities
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# Validation Helpers (rlang::caller_arg / caller_env)
# ══════════════════════════════════════════════════════════════════════════════

#' Validate a string is one of allowed values
#' @noRd
match_arg_fr <- function(x, choices,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (is.null(x)) return(choices[[1L]])
  x_low <- tolower(x)
  if (!(x_low %in% choices)) {
    cli_abort(
      "{.arg {arg}} must be one of {.val {choices}}, not {.val {x}}.",
      arg = arg, call = call
    )
  }
  x_low
}


#' Assert input is a character scalar
#' @noRd
check_scalar_chr <- function(x,
                              arg = caller_arg(x),
                              call = caller_env(),
                              allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(NULL))
  if (!is_string(x)) {
    cli_abort(c("{.arg {arg}} must be a single character string.",
                "x" = "You supplied {.obj_type_friendly {x}}."),
              arg = arg, call = call)
  }
  invisible(x)
}


#' Assert input is a positive numeric scalar
#' @noRd
check_positive_num <- function(x,
                                arg = caller_arg(x),
                                call = caller_env(),
                                allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(NULL))
  if (!(is.numeric(x) && length(x) == 1L && !is.na(x)) || x <= 0) {
    cli_abort(c("{.arg {arg}} must be a single positive number.",
                "x" = "You supplied {.obj_type_friendly {x}}."),
              arg = arg, call = call)
  }
  invisible(x)
}


#' Assert input is a logical scalar
#' @noRd
check_scalar_lgl <- function(x,
                              arg = caller_arg(x),
                              call = caller_env(),
                              allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(NULL))
  if (!is_scalar_logical(x) || is.na(x)) {
    cli_abort(c("{.arg {arg}} must be {.val TRUE} or {.val FALSE}.",
                "x" = "You supplied {.obj_type_friendly {x}}."),
              arg = arg, call = call)
  }
  invisible(x)
}


#' Assert input is a valid color
#' @noRd
check_color <- function(x,
                         arg = caller_arg(x),
                         call = caller_env(),
                         allow_null = TRUE) {
  if (allow_null && is.null(x)) return(invisible(NULL))
  if (!is.character(x) || length(x) != 1L) {
    cli_abort(c("{.arg {arg}} must be a color string (hex or name).",
                "x" = "You supplied {.obj_type_friendly {x}}.",
                "i" = "Example: {.code {arg} = \"#003366\"} or {.code {arg} = \"navy\"}."),
              arg = arg, call = call)
  }
  resolve_color(x, arg = arg, call = call)
  invisible(x)
}


#' Assert input is a non-negative integer scalar
#' @noRd
check_non_negative_int <- function(x,
                                    arg = caller_arg(x),
                                    call = caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0L ||
      x != as.integer(x)) {
    cli_abort(c("{.arg {arg}} must be a non-negative integer.",
                "x" = "You supplied {.val {x}}."),
              arg = arg, call = call)
  }
  as.integer(x)
}




#' Validate that column names exist in a data frame
#'
#' Checks that all column names in `cols` are present in `data_names`.
#' Errors with a helpful message if any are missing.
#'
#' @param cols Character vector of column names to check.
#' @param data_names Character vector of available column names.
#' @param arg Argument name for error messages.
#' @param call Caller environment for backtrace.
#' @noRd
validate_cols_exist <- function(cols, data_names,
                                arg = "cols",
                                call = caller_env()) {
  bad <- setdiff(cols, data_names)
  if (length(bad) > 0L) {
    cli_abort(c(
      "{.arg {arg}}: column{?s} not found in the data: {.val {bad}}.",
      "i" = "Available columns: {.val {data_names}}."
    ), call = call)
  }
  invisible(cols)
}


#' Parse a percentage width string to an fr_pct value
#'
#' Parses strings like `"20%"` or `"50.5%"` into `fr_pct(0.20)`. Returns
#' `NULL` if the string is not a percentage. Errors on invalid values
#' (0% or above 100%). Used by `fr_col()` and `fr_cols()`.
#'
#' @param x Character scalar to test.
#' @param arg Argument name for error messages.
#' @param call Caller environment for backtrace.
#' @return An `fr_pct` value, or `NULL` if `x` is not a percentage string.
#' @noRd
parse_pct_width <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is.character(x) || !grepl("^\\d+(\\.\\d+)?%$", x)) return(NULL)
  pct_val <- as.numeric(sub("%$", "", x)) / 100
  check_positive_num(pct_val, arg = arg, call = call)
  if (pct_val > 1) {
    cli_abort(c("{.arg {arg}} percentage must be between 0% and 100%.",
                "x" = "You supplied {.val {paste0(pct_val * 100, '%')}}."),
              call = call)
  }
  fr_pct(pct_val)
}


#' Validate n and format parameters for fr_header()
#'
#' Validates the three supported N-count forms: named numeric vector,
#' named list of named numeric vectors, and data frame (2-col or 3-col).
#'
#' @param n N-count specification (numeric, list, data frame, or NULL).
#' @param format Optional glue format string for N-count labels.
#' @param call Caller environment for error reporting.
#' @noRd
validate_n_param <- function(n, format = NULL, call = caller_env()) {
  if (!is.null(n)) {
    if (is.data.frame(n)) {
      # Data frame form: 2-col (global) or 3-col (per-group)
      # Must check BEFORE is.list() since data frames are lists
      if (ncol(n) < 2L || ncol(n) > 3L) {
        cli_abort(
          c("{.arg n} as a data frame must have 2 or 3 columns.",
            "x" = "You supplied {ncol(n)} column{?s}.",
            "i" = "2-col: treatment + count (global). 3-col: page_by + treatment + count (per-group)."),
          call = call
        )
      }
      count_col <- ncol(n)
      if (!is.numeric(n[[count_col]])) {
        cli_abort(
          c("Last column of {.arg n} data frame must be numeric (counts).",
            "x" = "Column {.val {names(n)[count_col]}} is {.cls {class(n[[count_col]])}}."),
          call = call
        )
      }
    } else if (is.numeric(n)) {
      if (is.null(names(n))) {
        cli_abort(
          c("{.arg n} must be a named numeric vector, a named list, or a data frame.",
            "x" = "You supplied an unnamed numeric vector.",
            "i" = "Example: {.code c(placebo = 45, zom_50mg = 45)}."),
          call = call
        )
      }
    } else if (is.list(n)) {
      if (is.null(names(n))) {
        cli_abort(
          c("{.arg n} as a list must be named by page_by group values.",
            "x" = "You supplied an unnamed list."),
          call = call
        )
      }
      for (grp in names(n)) {
        v <- n[[grp]]
        if (!is.numeric(v) || is.null(names(v))) {
          cli_abort(
            c("{.arg n}[[{.val {grp}}]] must be a named numeric vector.",
              "i" = "Example: {.code list(\"Group A\" = c(placebo = 42))}."),
            call = call
          )
        }
      }
    } else {
      cli_abort(
        c("{.arg n} must be a named numeric vector, a named list, or a data frame.",
          "x" = "You supplied {.obj_type_friendly {n}}.",
          "i" = "See {.fun fr_header} docs for N-count examples."),
        call = call
      )
    }
  }

  if (!is.null(format)) {
    check_scalar_chr(format, arg = "format", call = call)
  }

  invisible(NULL)
}


# ══════════════════════════════════════════════════════════════════════════════
# Glue-Markup Detection
# ══════════════════════════════════════════════════════════════════════════════

#' Detect whether a string contains {fr_*(...)} markup expressions
#' @noRd
has_fr_markup <- function(x) {
  is.character(x) && length(x) == 1L && grepl("\\{fr_", x, fixed = FALSE)
}

#' Markup sentinel markers for render-time resolution
#' @noRd
fr_env$sentinel_start   <- "\x01"
fr_env$sentinel_end     <- "\x02"
fr_env$sentinel_pattern <- "\x01([A-Z]+):([^\x02]*)\x02"


#' Build a markup sentinel string
#' @noRd
markup_sentinel <- function(type, content = "") {
  paste0(fr_env$sentinel_start, type, ":", content, fr_env$sentinel_end)
}
