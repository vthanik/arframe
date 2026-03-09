# ──────────────────────────────────────────────────────────────────────────────
# helpers.R — Error conditions, text utilities, internal helpers
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# 1. Typed Error Conditions
#
# All errors inherit from "tlframe_error" and use cli_abort for pretty
# messages. Backend-specific errors carry the format as metadata.
#
# NOTE on dot-prefix convention (Tidyverse Design Guide §20):
#   The dot-prefix rule applies when `...` is used to create a data
#   structure or is forwarded to a user-supplied function. Here, `...`
#   is forwarded to cli_abort() for dynamic data interpolation
#   (e.g., format = "rtf" available as {format} in the message).
#   We follow rlang::abort()'s own signature convention — which uses
#   `message`, `class`, `call` without dot prefix — for consistency
#   with the rlang/cli ecosystem. These argument names (message, class,
#   call) are extremely unlikely to collide with user data names
#   passed through `...`.
# ══════════════════════════════════════════════════════════════════════════════

#' Raise a tlframe error
#' @noRd
tlframe_error <- function(message, ..., class = NULL, call = caller_env()) {
  cli_abort(message, ..., class = c(class, "tlframe_error"), call = call)
}

#' Render error (RTF or PDF backend)
#' @noRd
tlframe_error_render <- function(message, format, ..., call = caller_env()) {
  tlframe_error(message, ..., format = format,
                class = "tlframe_error_render", call = call)
}

#' Layout error (pagination, column split)
#' @noRd
tlframe_error_layout <- function(message, ..., call = caller_env()) {
  tlframe_error(message, ..., class = "tlframe_error_layout", call = call)
}


# ══════════════════════════════════════════════════════════════════════════════
# 2. Text Normalisation
#
# All text inputs (titles, footnotes, labels, cell data) pass through
# normalise_text() before storage. This validates type and optionally
# evaluates {fr_*()} markup.
# ══════════════════════════════════════════════════════════════════════════════

#' Normalise a text value for storage
#'
#' Accepts a character scalar. If it contains `{fr_*()}` markup expressions,
#' evaluates them via glue to produce sentinel tokens.
#'
#' @param x Character scalar.
#' @param arg Character. Argument name for error messages.
#' @param env Environment for glue evaluation.
#' @param call Caller environment for error messages.
#' @return Character scalar (plain or with embedded sentinels).
#' @noRd
normalise_text <- function(x,
                            arg = "text",
                            env = parent.frame(),
                            call = caller_env()) {
  if (!is.character(x) || length(x) != 1L) {
    cli_abort(
      c("{.arg {arg}} must be a single character string.",
        "x" = "You supplied {.obj_type_friendly {x}}."),
      call = call
    )
  }
  eval_markup(x, env = env)
}


#' Normalise a character vector (multi-line titles/footnotes)
#' @noRd
normalise_text_vec <- function(texts,
                                arg = "...",
                                env = parent.frame(),
                                call = caller_env()) {
  if (!is.character(texts)) {
    cli_abort(
      c("{.arg {arg}} must be character strings.",
        "x" = "You supplied {.obj_type_friendly {texts}}."),
      call = call
    )
  }
  eval_markup_vec(texts, env = env)
}


# ══════════════════════════════════════════════════════════════════════════════
# 3. Row Selector Resolution
#
# Resolves an fr_rows_selector (created by fr_rows_matches()) against the
# actual data frame to produce a plain integer vector of row positions.
# Called eagerly by fr_styles() so that spec$cell_styles always stores
# integer indices, never deferred selectors.
# ══════════════════════════════════════════════════════════════════════════════

#' Resolve an fr_rows_selector to integer row positions
#'
#' @param selector An fr_rows_selector object.
#' @param data Data frame (spec$data).
#' @param call Caller environment for error messages.
#' @return Integer vector of matched row positions (1-based).
#' @noRd
resolve_rows_selector <- function(selector, data, call = caller_env()) {
  col <- selector$col

  if (!col %in% names(data)) {
    cli_abort(
      c("{.fn fr_rows_matches}: column {.val {col}} not found in the data.",
        "i" = "Available columns: {.val {names(data)}}."),
      call = call
    )
  }

  col_vec <- data[[col]]

  if (!is.null(selector$pattern)) {
    # Regex pattern match
    matched <- grep(selector$pattern, as.character(col_vec),
                    ignore.case = selector$ignore.case)
  } else {
    # Exact value match
    matched <- which(col_vec == selector$value)
  }

  as.integer(matched)
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. Plain Text Extraction
# ══════════════════════════════════════════════════════════════════════════════

#' Extract plain text from a label (character with possible sentinels)
#'
#' @param label Character scalar.
#' @return Character scalar with sentinels stripped.
#' @noRd
label_to_plain <- function(label) {
  if (!is.character(label)) return(as.character(label))
  sentinel_to_plain(label)
}


# ══════════════════════════════════════════════════════════════════════════════
# 5. Apply fr_theme() study defaults to a fresh fr_spec
# ══════════════════════════════════════════════════════════════════════════════

#' Apply fr_theme() study-level defaults to a new fr_spec
#'
#' Called by fr_table() immediately after new_fr_spec(). Reads fr_env$theme
#' and applies any stored defaults (page, pagehead, pagefoot, hlines, vlines,
#' footnote_separator) to the spec.
#'
#' @param spec An fr_spec object.
#' @return A modified fr_spec.
#' @noRd
apply_fr_theme <- function(spec) {
  setup <- fr_env$theme
  if (is.null(setup) || length(setup) == 0L) return(spec)

  # Page defaults
  spec <- apply_settings_section(spec, setup, fr_page,
    c("orientation", "paper", "font_family", "font_size", "margins", "tokens",
      "col_gap"))

  # Running header
  spec <- apply_settings_section(spec, setup[["pagehead"]], fr_pagehead,
    c("left", "center", "right", "font_size", "bold"))

  # Running footer
  spec <- apply_settings_section(spec, setup[["pagefoot"]], fr_pagefoot,
    c("left", "center", "right", "font_size", "bold"))

  # Horizontal rules
  if (!is.null(setup[["hlines"]])) {
    spec <- fr_hlines(spec, setup[["hlines"]])
  }

  # Vertical rules
  if (!is.null(setup[["vlines"]])) {
    spec <- fr_vlines(spec, setup[["vlines"]])
  }

  # Spacing
  spec <- apply_settings_section(spec, setup[["spacing"]], fr_spacing,
    c("titles_after", "footnotes_before", "pagehead_after",
      "pagefoot_before", "page_by_after"))

  # Column split/stub defaults (from theme)
  if (!is.null(setup[["split"]])) {
    spec$columns_meta$split <- setup[["split"]]
  }
  if (!is.null(setup[["stub"]])) {
    # Mark named columns as stubs (columns may not be built yet,
    # so store for later application in finalize_columns)
    spec$columns_meta$stub <- setup[["stub"]]
  }

  # Header defaults (from theme)
  if (!is.null(setup[["header"]])) {
    h <- setup[["header"]]
    if (!is.null(h$span_gap))  spec$header$span_gap  <- h$span_gap
  }

  # Footnote separator default
  if (!is.null(setup[["footnote_separator"]])) {
    spec$meta$footnote_separator <- setup[["footnote_separator"]]
  }

  spec
}


#' Apply a config/theme section to a spec via a verb function
#'
#' Shared helper for apply_config() and apply_fr_theme(). Extracts known
#' parameters from a config section and calls the verb function with them.
#'
#' @param spec An fr_spec object.
#' @param cfg_section A named list from config/theme (e.g., cfg$page).
#' @param verb_fn The pipeline verb to call (e.g., fr_page).
#' @param param_names Character vector of parameter names to extract.
#' @return Modified fr_spec.
#' @noRd
apply_settings_section <- function(spec, cfg_section, verb_fn, param_names) {
  if (!is.list(cfg_section)) return(spec)
  args <- cfg_section[intersect(names(cfg_section), param_names)]
  args <- args[!vapply(args, is.null, logical(1))]
  if (length(args) == 0L) return(spec)
  inject(verb_fn(spec, !!!args))
}


# ══════════════════════════════════════════════════════════════════════════════
# 6. Column / Footnote Filter Helpers
#
# Shared predicates to avoid repeating the same Filter(function(c) ...)
# lambdas across render.R, api-validate.R, classes.R, render-rtf.R, and
# render-latex.R.
# ══════════════════════════════════════════════════════════════════════════════

#' Get visible columns from a column list
#' @noRd
visible_columns <- function(columns) {
  Filter(function(c) !isFALSE(c$visible), columns)
}

#' Get names of stub columns from a column list
#' @noRd
stub_column_names <- function(columns) {
  names(Filter(function(c) isTRUE(c$stub), columns))
}

#' Split footnotes by placement
#' @return List with `every` and `last` components.
#' @noRd
split_footnotes <- function(footnotes) {
  list(
    every = Filter(function(fn) fn$placement == "every", footnotes),
    last  = Filter(function(fn) fn$placement == "last", footnotes)
  )
}
