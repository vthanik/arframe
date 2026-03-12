# ──────────────────────────────────────────────────────────────────────────────
# config.R — YAML config loading, discovery, and merging
#
# tlframe is the first pharma TFL package with file-driven theming.
# A single _tlframe.yml at the project root controls all table appearance.
#
# Three-tier precedence (lowest → highest):
#   1. Package defaults   inst/defaults/_tlframe.yml
#   2. Project config     _tlframe.yml (auto-discovered)
#   3. Session theme      fr_theme() / fr_theme_set()
#   4. Per-table verbs    fr_page(), fr_header(), etc.
# ──────────────────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# Config Discovery
# ══════════════════════════════════════════════════════════════════════════════

#' Find _tlframe.yml by searching up the directory tree
#'
#' Starts from `dir` and walks up parent directories looking for
#' `_tlframe.yml`. Falls back to the package-bundled defaults.
#'
#' @param dir Character scalar. Starting directory (default: working dir).
#' @return Character scalar. Path to the config file found.
#' @noRd
find_config <- function(dir = getwd()) {
  repeat {
    candidate <- file.path(dir, "_tlframe.yml")
    if (file.exists(candidate)) {
      return(normalizePath(candidate, mustWork = FALSE))
    }
    parent <- dirname(dir)
    if (parent == dir) {
      break
    }
    dir <- parent
  }
  # Fallback: package-bundled defaults
  system.file("defaults", "_tlframe.yml", package = "tlframe", mustWork = TRUE)
}


# ══════════════════════════════════════════════════════════════════════════════
# Config Loading
# ══════════════════════════════════════════════════════════════════════════════

#' Load Configuration from a YAML File
#'
#' @description
#'
#' Loads study-level configuration from a `_tlframe.yml` file. If no file
#' path is given, auto-discovers the nearest `_tlframe.yml` by searching
#' up the directory tree from the current working directory.
#'
#' The loaded config is stored internally and automatically applied to
#' every subsequent [fr_table()] call. Config values are the **lowest
#' priority** — they are overridden by [fr_theme()] (session-level) and
#' per-table verbs like [fr_page()] or [fr_header()].
#'
#' **tlframe is the first pharma TFL package with file-driven theming.**
#' Place a single `_tlframe.yml` at your project root and every table in
#' the study inherits the same fonts, margins, headers, footers, and rules
#' without any R code.
#'
#' @param file Character scalar or `NULL`. Path to a `_tlframe.yml` file.
#'   `NULL` (default) triggers auto-discovery: searches up the directory
#'   tree from `getwd()` until a `_tlframe.yml` is found, falling back to
#'   the package-bundled defaults (`inst/defaults/_tlframe.yml`).
#'
#' @return Invisibly returns the parsed config as a named list.
#'
#' @section Four-tier precedence:
#' Settings are resolved from lowest to highest priority:
#' ```
#' Package defaults  < _tlframe.yml  < fr_theme()  < per-table verbs
#' ```
#' Config from `_tlframe.yml` overrides package defaults. [fr_theme()]
#' overrides config. Per-table verbs (e.g. `fr_page(font_size = 10)`)
#' override everything.
#'
#' @section YAML file structure:
#' ```yaml
#' # _tlframe.yml — place at project root
#' page:
#'   paper: letter
#'   orientation: landscape
#'   margins: [1.0, 0.75, 1.0, 0.75]   # top, right, bottom, left
#'   font_family: "Courier New"
#'   font_size: 9
#'   col_gap: 4
#'
#' columns:
#'   split: false           # column splitting for wide tables
#'   spaces: indent         # indent | preserve — leading space handling
#'
#' header:
#'   align: ~               # inherit from column (user decides)
#'   valign: bottom
#'   bold: false
#'
#' pagehead:
#'   left: "{company}"
#'   center: "{study_id}"
#'   right: "Page {thepage} of {total_pages}"
#'   font_size: 8
#'
#' pagefoot:
#'   left: "{program}"
#'   right: "{datetime}"
#'   font_size: 8
#'
#' rules:
#'   hlines: header         # header | booktabs | open | hsides | box | void
#'   vlines: void           # box | all | inner | void
#'
#' footnotes:
#'   separator: true
#'   font_size: 8
#'
#' spacing:                 # blank lines between table sections
#'   titles_after: 1        # after titles, before column header
#'   footnotes_before: 1    # after body, before footnotes
#'   pagehead_after: 0      # after page header, before titles
#'   pagefoot_before: 0     # after footnotes, before page footer
#'   page_by_after: 1       # after page_by label, before col headers
#'
#' tokens:                  # custom token values for pagehead/pagefoot
#'   company: "Pharma Corp"
#'   study_id: "TFRM-2024-001"
#' ```
#'
#' @section Auto-discovery:
#' When `file = NULL`, `fr_config()` searches for `_tlframe.yml` starting
#' from the current working directory and walking up parent directories.
#' This mirrors the behaviour of `.Rprofile`, `_quarto.yml`, and
#' `_pkgdown.yml`. A typical project layout:
#' ```
#' study-root/
#'   _tlframe.yml         <-- found here
#'   R/
#'     t_demog.R
#'   output/
#'     tbl_14_1_1.rtf
#' ```
#'
#' @examples
#' # Load the built-in package defaults config
#' default_cfg <- system.file("defaults/_tlframe.yml", package = "tlframe")
#' fr_config(default_cfg)
#'
#' # Inspect what was loaded
#' fr_config_get()
#'
#' # Create a temporary custom config and load it
#' yml <- file.path(tempdir(), "_tlframe.yml")
#' writeLines(c(
#'   "page:",
#'   "  orientation: landscape",
#'   "  font_size: 8",
#'   "  font_family: 'Courier New'",
#'   "header:",
#'   "  bold: true",
#'   "tokens:",
#'   "  company: 'Pharma Corp'"
#' ), yml)
#' fr_config(yml)
#' fr_config_get()$page$font_size    # 8
#' fr_config_get()$tokens$company    # "Pharma Corp"
#'
#' # Clean up
#' fr_config_reset()
#' unlink(yml)
#'
#' @seealso [fr_config_get()] to inspect, [fr_config_reset()] to clear,
#'   [fr_theme()] for session-level overrides, [fr_page()] for per-table
#'   page layout.
#'
#' @export
fr_config <- function(file = NULL) {
  if (is.null(file)) {
    file <- find_config()
  }
  check_scalar_chr(file, arg = "file")

  if (!file.exists(file)) {
    cli_abort(
      c(
        "Config file not found: {.path {file}}.",
        "i" = "Create a {.file _tlframe.yml} at your project root or pass an explicit path."
      ),
      call = caller_env()
    )
  }

  cfg <- yaml::read_yaml(file)
  if (!is.list(cfg)) {
    cfg <- list()
  }

  fr_env$config <- cfg
  fr_env$config_file <- file
  invisible(cfg)
}


#' Get the Current Configuration
#'
#' @description
#'
#' Returns the current config as a named list. If no config has been loaded
#' yet, auto-discovers and loads the nearest `_tlframe.yml`.
#'
#' @return A named list of config settings. Top-level keys mirror the YAML
#'   structure: `page`, `header`, `pagehead`, `pagefoot`, `rules`,
#'   `footnotes`, `spacing`, `tokens`. Returns an empty list if no config
#'   is loaded and no `_tlframe.yml` is found.
#'
#' @examples
#' # Start clean — no config loaded
#' fr_config_reset()
#'
#' # Load config and inspect top-level keys
#' default_cfg <- system.file("defaults/_tlframe.yml", package = "tlframe")
#' fr_config(default_cfg)
#' cfg <- fr_config_get()
#' names(cfg)                    # page, header, pagehead, etc.
#'
#' # Access nested keys
#' cfg$page$font_size            # font size from config
#' cfg$page$orientation          # page orientation
#' cfg$header$bold               # header bold setting
#'
#' # After reset, fr_config_get() auto-discovers defaults
#' fr_config_reset()
#'
#' # Load a custom config with specific tokens
#' yml <- file.path(tempdir(), "_tlframe.yml")
#' writeLines(c(
#'   "tokens:",
#'   "  study_id: 'DEMO-001'"
#' ), yml)
#' fr_config(yml)
#' fr_config_get()$tokens$study_id   # "DEMO-001"
#'
#' # Clean up
#' fr_config_reset()
#' unlink(yml)
#'
#' @seealso [fr_config()] to load, [fr_config_reset()] to clear,
#'   [fr_theme_get()] for session-level theme inspection.
#'
#' @export
fr_config_get <- function() {
  if (is.null(fr_env$config)) {
    fr_config()
  }
  as.list(fr_env$config)
}


#' Reset Configuration
#'
#' @description
#'
#' Clears the loaded YAML config. After calling `fr_config_reset()`,
#' [fr_table()] uses built-in defaults (or [fr_theme()] if set).
#' Call [fr_config()] again to reload.
#'
#' @return Invisibly `NULL`.
#'
#' @examples
#' # Load a config
#' default_cfg <- system.file("defaults/_tlframe.yml", package = "tlframe")
#' fr_config(default_cfg)
#' fr_config_get()$page$orientation   # has a value
#'
#' # Reset clears everything
#' fr_config_reset()
#'
#' # After reset and re-load, config is back to defaults
#' fr_config(default_cfg)
#' fr_config_get()$page$orientation
#' fr_config_reset()                  # clean up
#'
#' @seealso [fr_config()] to load, [fr_config_get()] to inspect.
#'
#' @export
fr_config_reset <- function() {
  fr_env$config <- NULL
  fr_env$config_file <- NULL
  invisible(NULL)
}


# ══════════════════════════════════════════════════════════════════════════════
# Config Merge
# ══════════════════════════════════════════════════════════════════════════════

#' Deep merge two lists (right wins)
#'
#' Recursively merges `override` into `base`. For scalar and vector
#' values, `override` wins. For nested lists, merging is recursive.
#'
#' @param base List. Lower-priority values.
#' @param override List. Higher-priority values.
#' @return Merged list.
#' @noRd
merge_config <- function(base, override) {
  if (is.null(override)) {
    return(base)
  }
  if (is.null(base)) {
    return(override)
  }
  for (nm in names(override)) {
    if (is.list(base[[nm]]) && is.list(override[[nm]])) {
      base[[nm]] <- merge_config(base[[nm]], override[[nm]])
    } else {
      base[[nm]] <- override[[nm]]
    }
  }
  base
}


# ══════════════════════════════════════════════════════════════════════════════
# Config Application
#
# Called by fr_table() to merge config + theme into a fresh spec.
# ══════════════════════════════════════════════════════════════════════════════

#' Apply config to a fresh fr_spec
#'
#' Only applies if config has been explicitly loaded via fr_config().
#' Does NOT auto-discover — users must call fr_config() to opt in.
#' Called before apply_fr_theme() so that theme overrides config.
#'
#' @param spec An fr_spec object.
#' @return Modified fr_spec.
#' @noRd
apply_config <- function(spec) {
  cfg <- fr_env$config
  if (is.null(cfg) || length(cfg) == 0L) {
    return(spec)
  }

  # Page defaults
  spec <- apply_settings_section(
    spec,
    cfg$page,
    fr_page,
    c(
      "orientation",
      "paper",
      "font_family",
      "font_size",
      "margins",
      "continuation",
      "col_gap"
    )
  )

  # Column split/stub from config
  columns_cfg <- cfg$columns
  if (is.list(columns_cfg)) {
    if (!is.null(columns_cfg$split)) {
      split_val <- columns_cfg$split
      if (isTRUE(split_val) || identical(split_val, FALSE)) {
        spec$columns_meta$split <- split_val
      }
    }
    if (!is.null(columns_cfg$stub) && is.character(columns_cfg$stub)) {
      spec$columns_meta$stub <- columns_cfg$stub
    }
    if (!is.null(columns_cfg$spaces) && is.character(columns_cfg$spaces)) {
      spaces_val <- tryCatch(
        match_arg_fr(columns_cfg$spaces, fr_env$valid_spaces),
        error = function(e) {
          cli_warn(
            "Config {.field columns.spaces} ignored: {conditionMessage(e)}"
          )
          NULL
        }
      )
      if (!is.null(spaces_val)) spec$columns_meta$spaces <- spaces_val
    }
    if (!is.null(columns_cfg$n_format) && is.character(columns_cfg$n_format)) {
      spec$columns_meta$n_format <- columns_cfg$n_format
    }
  }

  # Header defaults (stored on spec$header for render-time use)
  header_cfg <- cfg$header
  if (is.list(header_cfg)) {
    if (!is.null(header_cfg$align)) {
      spec$header$align <- header_cfg$align
    }
    if (!is.null(header_cfg$valign)) {
      spec$header$valign <- header_cfg$valign
    }
    if (!is.null(header_cfg$bold)) {
      spec$header$bold <- header_cfg$bold
    }
    if (!is.null(header_cfg$span_gap)) {
      spec$header$span_gap <- header_cfg$span_gap
    }
    # n_format in header section → route to columns_meta for backward compat
    if (!is.null(header_cfg$n_format) && is.null(spec$columns_meta$n_format)) {
      spec$columns_meta$n_format <- header_cfg$n_format
    }
  }

  # Pagehead
  spec <- apply_settings_section(
    spec,
    cfg$pagehead,
    fr_pagehead,
    c("left", "center", "right", "font_size", "bold")
  )

  # Pagefoot
  spec <- apply_settings_section(
    spec,
    cfg$pagefoot,
    fr_pagefoot,
    c("left", "center", "right", "font_size", "bold")
  )

  # Rules
  rules_cfg <- cfg$rules
  if (is.list(rules_cfg)) {
    if (!is.null(rules_cfg$hlines)) {
      spec <- tryCatch(fr_hlines(spec, rules_cfg$hlines), error = function(e) {
        cli_warn("Config {.field rules.hlines} ignored: {conditionMessage(e)}")
        spec
      })
    }
    if (!is.null(rules_cfg$vlines) && rules_cfg$vlines != "void") {
      spec <- tryCatch(fr_vlines(spec, rules_cfg$vlines), error = function(e) {
        cli_warn("Config {.field rules.vlines} ignored: {conditionMessage(e)}")
        spec
      })
    }
  }

  # Footnotes
  fn_cfg <- cfg$footnotes
  if (is.list(fn_cfg)) {
    if (!is.null(fn_cfg$separator)) {
      spec$meta$footnote_separator <- fn_cfg$separator
    }
    if (!is.null(fn_cfg$placement)) {
      spec$meta$footnote_placement <- fn_cfg$placement
    }
  }

  # Spacing
  spec <- apply_settings_section(
    spec,
    cfg$spacing,
    fr_spacing,
    c(
      "titles_after",
      "footnotes_before",
      "pagehead_after",
      "pagefoot_before",
      "page_by_after"
    )
  )

  # Titles defaults (stored as metadata defaults for title entries)
  titles_cfg <- cfg$titles
  if (is.list(titles_cfg)) {
    if (!is.null(titles_cfg$align)) {
      spec$meta$title_align <- titles_cfg$align
    }
    if (!is.null(titles_cfg$bold)) {
      spec$meta$title_bold <- titles_cfg$bold
    }
    if (!is.null(titles_cfg$font_size)) {
      spec$meta$title_font_size <- titles_cfg$font_size
    }
  }

  # Custom tokens
  if (is.list(cfg$tokens) && length(cfg$tokens) > 0L) {
    existing_tokens <- spec$page$tokens
    spec$page$tokens <- c(cfg$tokens, existing_tokens)
  }

  spec
}
