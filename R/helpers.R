# ──────────────────────────────────────────────────────────────────────────────
# helpers.R — Error conditions, text utilities, internal helpers
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# 1. Typed Error Conditions
#
# All errors inherit from "arframe_error" and use cli_abort for pretty
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

#' Raise a arframe error
#' @noRd
arframe_error <- function(message, ..., class = NULL, call = caller_env()) {
  cli_abort(message, ..., class = c(class, "arframe_error"), call = call)
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
normalise_text <- function(
  x,
  arg = "text",
  env = caller_env(),
  call = caller_env()
) {
  if (!is.character(x) || length(x) != 1L) {
    cli_abort(
      c(
        "{.arg {arg}} must be a single character string.",
        "x" = "You supplied {.obj_type_friendly {x}}."
      ),
      call = call
    )
  }
  eval_markup(x, env = env)
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

# S3 generic for row selector resolution.
# Dispatches on selector class. Default method handles fr_rows_selector.
# Custom selector classes can implement resolve_rows_selector.<class>()
# for extensibility (e.g., fr_rows_between, fr_rows_where).
resolve_rows_selector <- function(selector, ...) {
  UseMethod("resolve_rows_selector")
}

#' @noRd
resolve_rows_selector.fr_rows_selector <- function(
  selector,
  data,
  ...,
  call = caller_env()
) {
  col <- selector$col

  if (!col %in% names(data)) {
    cli_abort(
      c(
        "{.fn fr_rows_matches}: column {.val {col}} not found in the data.",
        "i" = "Available columns: {.val {names(data)}}."
      ),
      call = call
    )
  }

  col_vec <- data[[col]]

  if (!is.null(selector$pattern)) {
    matched <- grep(
      selector$pattern,
      as.character(col_vec),
      ignore.case = selector$ignore.case
    )
  } else {
    matched <- which(col_vec == selector$value)
  }

  if (length(matched) == 0L) {
    pattern_desc <- if (!is.null(selector$pattern)) {
      paste0("pattern ", selector$pattern)
    } else {
      paste0("value ", selector$value)
    }
    cli::cli_warn(c(
      "{.fn fr_rows_matches}: no rows matched in column {.val {col}}.",
      "i" = "Selector: {pattern_desc}.",
      "i" = "Styles targeting these rows will have no effect."
    ), call = caller_env())
  }

  as.integer(matched)
}


#' Check if a rows value is a deferred group header selector
#'
#' Returns `TRUE` for `"group_headers"` or `"group_headers:<level>"` strings.
#' These are resolved later in `finalize_rows()` after group header injection.
#' @noRd
is_group_header_selector <- function(rows) {
  is.character(rows) &&
    length(rows) == 1L &&
    startsWith(rows, "group_headers")
}


# ══════════════════════════════════════════════════════════════════════════════
# 4a. Shared Tidyselect Resolution
#
# Central wrapper around tidyselect::eval_select() used by fr_cols(),
# fr_header(), and fr_spans() to resolve column expressions against data.
# ══════════════════════════════════════════════════════════════════════════════

#' Resolve a tidyselect expression against column data
#'
#' Thin wrapper around [tidyselect::eval_select()] that adds consistent
#' error handling. Returns a named integer vector (column positions) on
#' success, or re-raises a contextual error on failure.
#'
#' @param expr A tidyselect expression (quosure, symbol, or call).
#' @param data A data frame or named vector used as the column source for
#'   tidyselect evaluation.
#' @param context Character scalar included in the error message to identify
#'   which caller triggered the failure (e.g. `"span 'Treatment'"`,
#'   `"align 'center'"`, `"formula selector"`).
#' @param call Caller environment for error messages.
#' @return Named integer vector of matched column positions.
#' @noRd
resolve_tidyselect <- function(
  expr,
  data,
  context = NULL,
  call = caller_env()
) {
  tryCatch(
    tidyselect::eval_select(expr, data = data, error_call = call),
    error = function(e) {
      if (is.null(context)) {
        cli_abort(
          c(
            "Failed to resolve tidyselect expression.",
            "x" = conditionMessage(e)
          ),
          call = call,
          parent = e
        )
      }
      cli_abort(
        c(
          "Failed to resolve tidyselect expression for {context}.",
          "x" = conditionMessage(e)
        ),
        call = call,
        parent = e
      )
    }
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# 4b. Tidyselect Column Resolution (Deferred Style Cols)
#
# Supports deferred resolution of tidyselect expressions in cols parameters.
# Style constructors (fr_style, fr_col_style, fr_style_if) capture cols
# via enquo() — character vectors evaluate immediately, tidyselect
# expressions are stored as quosures and resolved in fr_styles() where
# spec$data is available.
# ══════════════════════════════════════════════════════════════════════════════

#' Resolve a cols quosure to a value or store for deferred evaluation
#'
#' If the quosure evaluates to NULL or a character vector, returns as-is.
#' If it errors (typically a tidyselect expression needing data context),
#' returns the quosure for deferred resolution in fr_styles().
#'
#' @param quo A quosure from enquo(cols).
#' @param call Caller environment for error messages.
#' @return NULL, character vector, or quosure (for deferred resolution).
#' @noRd
resolve_cols_expr <- function(quo, call = caller_env()) {
  if (quo_is_null(quo)) {
    return(NULL)
  }

  tryCatch(
    {
      val <- eval_tidy(quo)
      if (is.null(val)) {
        return(NULL)
      }
      if (is.character(val) || is.numeric(val)) {
        return(val)
      }
      cli_abort(
        c(
          "{.arg cols} must be a character vector or tidyselect expression.",
          "x" = "You supplied {.obj_type_friendly {val}}."
        ),
        call = call
      )
    },
    error = function(e) {
      # Defer only errors that indicate a tidyselect expression needing data
      # context: column not found, tidyselect helpers called outside
      # eval_select(), or rlang subset errors. Re-raise everything else
      # (syntax errors, etc.) immediately so the original message isn't lost.
      msg <- conditionMessage(e)
      is_deferred <- grepl("not found", msg, fixed = TRUE) ||
        grepl("could not find function", msg, fixed = TRUE) ||
        grepl("object .+ not found", msg) ||
        inherits(e, "rlang_error") &&
          grepl("can't subset", msg, ignore.case = TRUE)
      if (is_deferred) {
        quo
      } else {
        cli_abort(
          c(
            "Failed to evaluate {.arg cols} expression.",
            "x" = conditionMessage(e)
          ),
          call = call,
          parent = e
        )
      }
    }
  )
}


#' Resolve deferred tidyselect cols in a style object
#'
#' Checks if style$cols is a quosure. If so, resolves it via
#' tidyselect::eval_select() against the data frame.
#'
#' @param style An fr_cell_style object.
#' @param data Data frame (spec$data).
#' @param call Caller environment for error messages.
#' @return The style with cols resolved to a character vector.
#' @noRd
resolve_style_cols <- function(style, data, call = caller_env()) {
  if (is_quosure(style$cols)) {
    pos <- tidyselect::eval_select(style$cols, data = data, error_call = call)
    style$cols <- names(pos)
  }
  style
}


# ══════════════════════════════════════════════════════════════════════════════
# 5. Plain Text Extraction
# ══════════════════════════════════════════════════════════════════════════════

#' Extract plain text from a label (character with possible sentinels)
#'
#' @param label Character scalar.
#' @return Character scalar with sentinels stripped.
#' @noRd
label_to_plain <- function(label) {
  if (!is.character(label)) {
    return(as.character(label))
  }
  sentinel_to_plain(label)
}


# ══════════════════════════════════════════════════════════════════════════════
# 6. Apply fr_theme() study defaults to a fresh fr_spec
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
  if (is.null(setup) || length(setup) == 0L) {
    return(spec)
  }

  # Page defaults
  spec <- apply_settings_section(
    spec,
    setup,
    fr_page,
    c(
      "orientation",
      "paper",
      "font_family",
      "font_size",
      "margins",
      "tokens",
      "col_gap",
      "continuation"
    )
  )

  # Running header
  spec <- apply_settings_section(
    spec,
    setup[["pagehead"]],
    fr_pagehead,
    c("left", "center", "right", "font_size", "bold")
  )

  # Running footer
  spec <- apply_settings_section(
    spec,
    setup[["pagefoot"]],
    fr_pagefoot,
    c("left", "center", "right", "font_size", "bold")
  )

  # Horizontal rules
  if (!is.null(setup[["hlines"]])) {
    spec <- fr_hlines(spec, setup[["hlines"]])
  }

  # Vertical rules
  if (!is.null(setup[["vlines"]])) {
    spec <- fr_vlines(spec, setup[["vlines"]])
  }

  # Spacing
  spec <- apply_settings_section(
    spec,
    setup[["spacing"]],
    fr_spacing,
    c(
      "titles_after",
      "footnotes_before",
      "pagehead_after",
      "pagefoot_before",
      "page_by_after"
    )
  )

  # Column space_mode/split/stub/n_format defaults (from theme)
  if (!is.null(setup[["space_mode"]])) {
    spec$columns_meta$space_mode <- setup[["space_mode"]]
  }
  if (!is.null(setup[["n_format"]])) {
    spec$columns_meta$n_format <- setup[["n_format"]]
  }
  if (!is.null(setup[["split"]])) {
    spec$columns_meta$split <- setup[["split"]]
  }
  if (!is.null(setup[["stub"]])) {
    # Mark named columns as stubs (columns may not be built yet,
    # so store for later application in finalize_columns)
    spec$columns_meta$stub <- setup[["stub"]]
  }

  # Header defaults (from theme)
  spec <- apply_settings_section(
    spec,
    setup[["header"]],
    fr_header,
    c(
      "align",
      "valign",
      "bold",
      "background",
      "color",
      "font_size",
      "repeat_on_page"
    )
  )
  if (!is.null(setup[["header"]]$span_gap)) {
    spec$header$span_gap <- setup[["header"]]$span_gap
  }

  # Row defaults: group_keep, group_style
  if (!is.null(setup[["group_keep"]])) {
    spec$body$group_keep <- setup[["group_keep"]]
  }
  if (!is.null(setup[["group_style"]])) {
    spec$body$group_style <- setup[["group_style"]]
  }
  if (!is.null(setup[["page_by_style"]])) {
    spec$page_by_styles <- list(setup[["page_by_style"]])
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
  if (!is.list(cfg_section)) {
    return(spec)
  }
  args <- cfg_section[intersect(names(cfg_section), param_names)]
  args <- args[!vapply(args, is.null, logical(1))]
  if (length(args) == 0L) {
    return(spec)
  }
  inject(verb_fn(spec, !!!args))
}


# ══════════════════════════════════════════════════════════════════════════════
# 7. Column / Footnote Filter Helpers
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
    last = Filter(function(fn) fn$placement == "last", footnotes)
  )
}
