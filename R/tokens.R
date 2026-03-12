# ──────────────────────────────────────────────────────────────────────────────
# tokens.R — Token map building and provenance utilities
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# 1. Provenance Utilities
#
# get_source_path() attempts to detect the calling script's file path
# across all common R execution environments:
#   - RStudio interactive (rstudioapi)
#   - source("script.R") (sys.frame "ofile")
#   - Rscript / R CMD BATCH (commandArgs)
#   - knitr / rmarkdown (knitr::current_input)
#   - Interactive (returns NA_character_)
# ══════════════════════════════════════════════════════════════════════════════

#' Get source file path (best effort)
#' @noRd
get_source_path <- function() {
  # 1. RStudio API (interactive in RStudio)
  tryCatch(
    {
      if (
        requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::isAvailable()
      ) {
        ctx <- rstudioapi::getSourceEditorContext()
        if (!is.null(ctx$path) && nzchar(ctx$path)) return(ctx$path)
      }
    },
    error = function(e) NULL
  )

  # 2. source("script.R") — look for "ofile" in parent frames
  for (i in seq_len(sys.nframe())) {
    env <- sys.frame(i)
    if (exists("ofile", envir = env, inherits = FALSE)) {
      ofile <- get("ofile", envir = env, inherits = FALSE)
      if (is.character(ofile) && nzchar(ofile)) return(ofile)
    }
  }

  # 3. Rscript / R CMD BATCH (commandArgs)
  cmd_args <- commandArgs(trailingOnly = FALSE)

  # Long form: --file=script.R
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0L) {
    return(normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = FALSE))
  }

  # Short form: -f script.R
  f_index <- which(cmd_args == "-f")
  if (length(f_index) > 0L && f_index[1L] < length(cmd_args)) {
    return(normalizePath(cmd_args[f_index[1L] + 1L], mustWork = FALSE))
  }

  # 4. knitr (rmarkdown / quarto)
  tryCatch(
    {
      if (requireNamespace("knitr", quietly = TRUE)) {
        input <- knitr::current_input()
        if (!is.null(input) && nzchar(input)) return(input)
      }
    },
    error = function(e) NULL
  )

  NA_character_
}


#' Format timestamp for headers/footers
#'
#' Standard pharma format: DDMONYYYY HH:MM:SS (e.g. "05MAR2026 14:24:25")
#'
#' @return Character scalar.
#' @noRd
get_timestamp <- function() {
  toupper(format(Sys.time(), "%d%b%Y %H:%M:%S"))
}


# ══════════════════════════════════════════════════════════════════════════════
# 2. Token Map Builder
#
# Builds the full token_name → value map by merging built-in tokens
# (thepage, total_pages, program, datetime) with user-supplied custom tokens.
#
# Built-in readonly tokens (thepage, total_pages) are set by the pagination
# engine and cannot be overridden. This is enforced at fr_page() creation
# time by validate_user_tokens() in classes.R.
#
# Built-in overridable tokens (program, datetime) can be set by the user
# via fr_page(tokens = list(program = "my_script.R")).
# ══════════════════════════════════════════════════════════════════════════════

#' Build the complete token map for page header/footer resolution
#'
#' @param page_num Integer. Current page number (set by pagination engine).
#' @param total_pages Integer. Total page count (set by pagination engine).
#' @param spec An fr_spec object.
#' @return Named list of token_name → character value.
#' @noRd
build_token_map <- function(page_num, total_pages, spec) {
  user_tokens <- spec$page$tokens

  # Readonly tokens — always from pagination engine
  builtin <- list(
    thepage = as.character(page_num),
    total_pages = as.character(total_pages)
  )

  # Overridable tokens — user can set via fr_page(tokens = ...)
  src_path <- get_source_path()
  if (is.na(src_path)) {
    src_path <- ""
  }
  overridable <- list(
    program = user_tokens[["program"]] %||% src_path,
    datetime = user_tokens[["datetime"]] %||%
      get_timestamp()
  )

  # Remove overridable token names from user list to get truly custom tokens
  custom_tokens <- user_tokens
  custom_tokens[["program"]] <- NULL
  custom_tokens[["datetime"]] <- NULL

  c(builtin, overridable, custom_tokens)
}
