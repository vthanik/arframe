# ──────────────────────────────────────────────────────────────────────────────
# api-rules.R — Line rule verbs: fr_hlines, fr_vlines, fr_grid
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# Internal helpers for rule filtering
# ──────────────────────────────────────────────────────────────────────────────

#' Keep only vertical rules (filter out horizontal rules and optionally boxes)
#' @param rules List of rule objects.
#' @param keep_box Logical. Whether to keep fr_rule_box objects.
#' @noRd
keep_vertical_rules <- function(rules, keep_box = TRUE) {
  Filter(
    function(r) {
      if (inherits(r, "fr_rule_box")) {
        return(keep_box)
      }
      inherits(r, "fr_vline_spec") ||
        (inherits(r, "fr_rule") && identical(r$direction, "vertical"))
    },
    rules
  )
}

#' Keep only horizontal rules and boxes (filter out vertical rules)
#' @param rules List of rule objects.
#' @noRd
keep_horizontal_rules <- function(rules) {
  Filter(
    function(r) {
      !inherits(r, "fr_rule") ||
        r$direction == "horizontal" ||
        inherits(r, "fr_rule_box")
    },
    rules
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_hlines — Horizontal rule presets and custom rules
# ══════════════════════════════════════════════════════════════════════════════

#' Apply Horizontal Rules
#'
#' @description
#'
#' Sets horizontal rules (lines) for the table using a named preset or
#' custom rule properties. Calling `fr_hlines()` again **replaces** all
#' previously set horizontal rules.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param preset Named rule preset. One of:
#'   * `"header"` — single rule below the column header
#'     only. **Default and most common in TFL outputs** (ICH E3).
#'     No top or bottom border; footnotes follow the last body row directly.
#'   * `"open"` — rule above header + rule below header; no bottom border.
#'     Use when the table bottom is immediately followed by footnotes.
#'   * `"hsides"` — rule at the top (above header) and bottom (below body)
#'     only.
#'   * `"above"` — single rule above the column header only.
#'   * `"below"` — single rule below the last body row only.
#'   * `"box"` — full outer border on all four sides.
#'   * `"booktabs"` — thick top (1 pt) + thin mid (0.5 pt) + thick bottom
#'     (1 pt). The standard for PDF and publication output.
#'   * `"void"` — no horizontal rules. Clears all previously set rules.
#' @param width Rule width. Named shorthand or numeric in points:
#'   * `"hairline"` = 0.25 pt
#'   * `"thin"` = 0.5 pt (default preset width)
#'   * `"medium"` = 1.0 pt
#'   * `"thick"` = 1.5 pt
#'   * Numeric, e.g. `width = 0.75` (positive, in pt)
#'
#'   When supplied, overrides the preset's default widths for **all** rules
#'   created by this call. `NULL` uses each preset's own default widths.
#' @param color Rule colour: hex string (`"#003366"`) or CSS named colour
#'   (`"navy"`). `NULL` = black (`"#000000"`).
#' @param linestyle Rule line style. One of `"solid"` (default), `"dashed"`,
#'   `"dotted"`, `"dashdot"`, or `"double"`.
#'
#' @return A modified `fr_spec`. Rules stored in `spec$rules`.
#'
#' @section Regulatory conventions:
#' Most pharma TFL outputs use `"header"`: a single thin
#' solid rule separating column headers from body. No top or bottom border.
#' Footnotes are placed directly below the last body row with a short
#' separator line drawn by the footnote block itself (controlled by the
#' `.separator` argument of [fr_footnotes()]).
#'
#' `"booktabs"` is the standard for PDF/publication output and matches the
#' LaTeX `\toprule` / `\midrule` / `\bottomrule` pattern.
#'
#' @section Tips:
#' * Combine `fr_hlines()` and `fr_vlines()` independently — each manages its
#'   own set of rules.
#' * `fr_hlines("void")` clears **only** horizontal rules; vertical rules from
#'   `fr_vlines()` are not affected.
#' * `width`, `color`, and `linestyle` apply uniformly to **all** rules created
#'   by this preset call. For mixed widths (e.g. booktabs thick/thin), omit
#'   `width` and let the preset define its own widths.
#'
#' @examples
#' ## ── Common presets ────────────────────────────────────────────────────────
#'
#' # Standard TFL output: single rule under column header
#' tbl_demog |> fr_table() |> fr_hlines("header")
#'
#' # Open: header rules, no bottom border
#' tbl_demog |> fr_table() |> fr_hlines("open")
#'
#' # Booktabs: publication-quality thick/thin/thick
#' tbl_demog |> fr_table() |> fr_hlines("booktabs")
#'
#' # Top and bottom rules only
#' tbl_demog |> fr_table() |> fr_hlines("hsides")
#'
#' # Box: full outer border
#' tbl_demog |> fr_table() |> fr_hlines("box")
#'
#' # Remove all horizontal rules
#' tbl_demog |> fr_table() |> fr_hlines("void")
#'
#' ## ── Custom width ─────────────────────────────────────────────────────────
#'
#' # Hairline rules (0.25 pt)
#' tbl_demog |> fr_table() |> fr_hlines("header", width = "hairline")
#'
#' # Thick rules (1.5 pt)
#' tbl_demog |> fr_table() |> fr_hlines("hsides", width = "thick")
#'
#' # Exact width in points
#' tbl_demog |> fr_table() |> fr_hlines("header", width = 0.75)
#'
#' ## ── Custom colour (hex or CSS named colour) ────────────────────────────────
#'
#' # Hex colour
#' tbl_demog |> fr_table() |> fr_hlines("booktabs", color = "#003366")
#'
#' # CSS named colour
#' tbl_demog |> fr_table() |> fr_hlines("header", color = "steelblue")
#'
#' ## ── Custom linestyle ──────────────────────────────────────────────────────
#'
#' # Dashed rule below header
#' tbl_demog |> fr_table() |> fr_hlines("header", linestyle = "dashed")
#'
#' # Double rule below header
#' tbl_demog |> fr_table() |> fr_hlines("header", linestyle = "double")
#'
#' # Dash-dot (SAS ODS equivalent)
#' tbl_demog |> fr_table() |> fr_hlines("header", linestyle = "dashdot")
#'
#' ## ── Full pipeline ─────────────────────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles("Table 14.1.1 Demographics", "Full Analysis Set") |>
#'   fr_footnotes("[a] Percentages based on N in column header.") |>
#'   fr_hlines("header") |>
#'   fr_vlines("box")
#'
#' @seealso [fr_vlines()] for vertical rules, [fr_styles()] for cell shading
#'   and font overrides, [fr_footnotes()] for the `.separator` control.
#'
#' @export
fr_hlines <- function(
  spec,
  preset = "header",
  width = NULL,
  color = NULL,
  linestyle = NULL
) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  preset <- match_arg_fr(preset, names(fr_env$hline_presets), call = call)

  if (identical(preset, "void")) {
    spec$rules <- keep_vertical_rules(spec$rules, keep_box = TRUE)
    return(spec)
  }

  rule_defs <- fr_env$hline_presets[[preset]]

  if (identical(rule_defs, "box")) {
    new_rules <- list(structure(list(preset = "box"), class = "fr_rule_box"))
  } else {
    resolved_width <- if (!is.null(width)) {
      resolve_line_width(width, call = call)
    } else {
      NULL
    }
    resolved_color <- if (!is.null(color)) {
      resolve_color(color, call = call)
    } else {
      NULL
    }
    resolved_linestyle <- if (!is.null(linestyle)) {
      match_arg_fr(linestyle, fr_env$valid_linestyles, call = call)
    } else {
      NULL
    }

    new_rules <- lapply(rule_defs, function(rd) {
      new_fr_rule(
        direction = "horizontal",
        region = rd$region,
        side = rd$side,
        width = resolved_width %||% rd$width,
        linestyle = resolved_linestyle %||% rd$linestyle,
        fg = resolved_color %||% rd$fg,
        call = call
      )
    })
  }

  # Replace all horizontal rules; preserve only vertical rules
  # Exclude fr_rule_box to prevent accumulation on repeated calls
  is_box <- identical(rule_defs, "box")
  existing_vlines <- keep_vertical_rules(spec$rules, keep_box = !is_box)
  spec$rules <- c(existing_vlines, new_rules)
  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_vlines — Vertical rule presets and custom rules
# ══════════════════════════════════════════════════════════════════════════════

#' Apply Vertical Rules
#'
#' @description
#'
#' Sets vertical rules (column separators) for the table. Calling
#' `fr_vlines()` again **replaces** all previously set vertical rules.
#' Horizontal rules set by [fr_hlines()] are not affected.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param preset Named rule preset or `"void"` (no vertical rules):
#'   * `"box"` — rules on the leftmost and rightmost table edges only (outer
#'     vertical borders). Default.
#'   * `"all"` — rules between every column and on outer edges.
#'   * `"inner"` — rules between columns only; no outer edges.
#'   * `"void"` — no vertical rules. Clears all previously set vlines.
#' @param cols Integer vector of column **gap** positions to rule.
#'   Position `j` draws a rule to the right of column `j`. Use alongside
#'   `preset = "void"` to add individual column separators. `NULL` uses the
#'   preset.
#' @param width Rule width. Named shorthand (`"hairline"`, `"thin"`, `"medium"`,
#'   `"thick"`) or numeric in points. Default `NULL` (thin, 0.5 pt).
#' @param color Rule colour: hex string or CSS named colour. `NULL` = black.
#' @param linestyle One of `"solid"` (default), `"dashed"`, `"dotted"`,
#'   `"dashdot"`, `"double"`.
#' @param abovepos,belowpos Fractions (0–1) controlling partial vertical extent
#'   of the rule within a row. `NULL` = full height (default).
#'
#' @return A modified `fr_spec`. Vertical rules stored in `spec$rules`.
#'
#' @section Preset comparison:
#' | Preset | Left edge | Between cols | Right edge | Use case |
#' |--------|-----------|--------------|------------|----------|
#' | `"box"` | Yes | No | Yes | Clean outer border (most common) |
#' | `"all"` | Yes | Yes | Yes | Full grid with column separators |
#' | `"inner"` | No | Yes | No | Column separators, open sides |
#' | `"void"` | No | No | No | No vertical rules |
#'
#' @section Regulatory conventions:
#' Most pharma TFL outputs use **no vertical rules** (`"void"`) or just an
#' outer box border (`"box"`). Full grids (`"all"`) are uncommon in
#' regulatory submissions but may be used for dense listing tables or
#' shift tables where column boundaries aid readability.
#'
#' @section Tips:
#' * `fr_vlines("box")` is the most common use — a clean outer border.
#' * Combine with `fr_hlines("header")` for the typical pharma style:
#'   box outer border + single rule under the column header.
#' * `cols = c(1L, 3L)` draws a rule to the right of columns 1 and 3 —
#'   useful for separating a stub column from data columns.
#' * `fr_vlines()` and `fr_hlines()` are independent — each manages its
#'   own rules. To set both at once, use [fr_grid()].
#'
#' @examples
#' ## ── Common presets ────────────────────────────────────────────────────────
#'
#' # Outer box border only (most common)
#' tbl_demog |> fr_table() |> fr_vlines("box")
#'
#' # Rules between every column
#' tbl_demog |> fr_table() |> fr_vlines("all")
#'
#' # Inner column separators only (no outer edges)
#' tbl_demog |> fr_table() |> fr_vlines("inner")
#'
#' # Remove all vertical rules
#' tbl_demog |> fr_table() |> fr_vlines("void")
#'
#' ## ── Custom column positions ───────────────────────────────────────────────
#'
#' # Rule after the stub column (column 1) only
#' tbl_demog |>
#'   fr_table() |>
#'   fr_vlines("void") |>
#'   fr_vlines(cols = 1L)
#'
#' ## ── Custom width, style, and colour ────────────────────────────────────────
#'
#' # Thick outer box
#' tbl_demog |> fr_table() |> fr_vlines("box", width = "thick")
#'
#' # Dashed inner separators
#' tbl_demog |> fr_table() |> fr_vlines("all", linestyle = "dashed")
#'
#' # CSS named colour
#' tbl_demog |> fr_table() |> fr_vlines("box", color = "slategray")
#'
#' ## ── Combined with fr_hlines ───────────────────────────────────────────────
#'
#' # Typical regulatory pharma style
#' tbl_demog |>
#'   fr_table() |>
#'   fr_hlines("header") |>
#'   fr_vlines("box")
#'
#' # Full grid: all borders
#' tbl_demog |>
#'   fr_table() |>
#'   fr_hlines("hsides") |>
#'   fr_vlines("all")
#'
#' @seealso [fr_hlines()] for horizontal rules, [fr_grid()] to set both in
#'   one call, [fr_styles()] for cell shading and font styling.
#'
#' @export
fr_vlines <- function(
  spec,
  preset = "box",
  cols = NULL,
  width = NULL,
  color = NULL,
  linestyle = NULL,
  abovepos = NULL,
  belowpos = NULL
) {
  call <- caller_env()
  check_fr_spec(spec, call = call)

  valid_vline_presets <- c("box", "all", "inner", "void")
  preset <- match_arg_fr(preset, valid_vline_presets, call = call)

  # Remove existing vertical rules
  existing_hlines <- keep_horizontal_rules(spec$rules)

  if (identical(preset, "void") && is.null(cols)) {
    spec$rules <- existing_hlines
    return(spec)
  }

  resolved_width <- resolve_line_width(width, call = call)
  resolved_color <- if (!is.null(color)) {
    resolve_color(color, call = call)
  } else {
    "#000000"
  }
  resolved_linestyle <- if (!is.null(linestyle)) {
    match_arg_fr(linestyle, fr_env$valid_linestyles, call = call)
  } else {
    "solid"
  }

  # Validate custom cols
  if (!is.null(cols)) {
    if (!is.numeric(cols) || any(cols < 1L) || any(cols != as.integer(cols))) {
      cli_abort(
        "{.arg cols} must be a vector of positive integers.",
        call = call
      )
    }
    cols <- as.integer(cols)
  }

  new_vlines <- list(
    structure(
      list(
        preset = preset,
        cols = cols,
        width = resolved_width,
        linestyle = resolved_linestyle,
        fg = resolved_color,
        abovepos = abovepos,
        belowpos = belowpos
      ),
      class = c("fr_vline_spec", "fr_rule")
    )
  )

  spec$rules <- c(existing_hlines, new_vlines)
  spec
}


# ══════════════════════════════════════════════════════════════════════════════
# fr_grid — Apply horizontal and vertical rules in one step
# ══════════════════════════════════════════════════════════════════════════════

#' Apply Horizontal and Vertical Rules Together
#'
#' @description
#'
#' A convenience wrapper that calls [fr_hlines()] and [fr_vlines()] in one
#' step. Use `fr_grid()` when you want to set both rule directions at once
#' with consistent styling (same width, colour, and linestyle).
#'
#' Prefer `fr_grid()` over chaining `fr_hlines() |> fr_vlines()` when:
#' * You want uniform width/colour/linestyle for both directions.
#' * You use the slash shorthand for concise one-liners: `fr_grid("header/box")`.
#'
#' Prefer separate `fr_hlines()` + `fr_vlines()` calls when:
#' * Horizontal and vertical rules need different widths, colours, or styles.
#' * You need `fr_vlines(cols = ...)` for custom column positions.
#'
#' @param spec An `fr_spec` object from [fr_table()].
#' @param hpreset Horizontal rule preset passed to [fr_hlines()]. Default
#'   `"header"` (single rule below column header — standard TFL output).
#'
#'   Alternatively, pass a **single slash-separated string** combining both
#'   presets: `"header/box"`, `"booktabs/all"`, etc. When a `/` is detected,
#'   the left side is used as `hpreset` and the right side as `vpreset`.
#' @param vpreset Vertical rule preset passed to [fr_vlines()]. Default
#'   `"box"` (outer border only). Ignored when `hpreset` contains a `/`.
#' @param width Rule width applied to **both** horizontal and vertical rules.
#'   Named shorthand (`"hairline"`, `"thin"`, `"medium"`, `"thick"`) or
#'   numeric in points. `NULL` uses each preset's own default.
#' @param color Rule colour for both directions. `NULL` = black.
#' @param linestyle Line style for both directions. `NULL` = `"solid"`.
#'
#' @return A modified `fr_spec` with both horizontal and vertical rules set.
#'
#' @section Common combinations:
#' | Call | Horizontal | Vertical | Use case |
#' |------|------------|----------|----------|
#' | `fr_grid()` | header | box | Pharma standard (most common) |
#' | `fr_grid("booktabs/box")` | booktabs | box | Publication style |
#' | `fr_grid("hsides/all")` | top+bottom | all columns | Full grid |
#' | `fr_grid("open/box")` | above+below header | box | Header-framed |
#' | `fr_grid("void/void")` | none | none | Clear all rules |
#'
#' @examples
#' ## ── Standard pharma output (header + box) ─────────────────────────────
#'
#' tbl_demog |> fr_table() |> fr_grid()
#'
#' ## ── Slash shorthand ───────────────────────────────────────────────────
#'
#' # Equivalent to fr_grid("booktabs", "box")
#' tbl_demog |> fr_table() |> fr_grid("booktabs/box")
#'
#' ## ── Full grid (top/bottom + all column separators) ────────────────────
#'
#' tbl_demog |> fr_table() |> fr_grid("hsides/all")
#'
#' ## ── Custom width and colour applied uniformly ─────────────────────────
#'
#' tbl_demog |> fr_table() |> fr_grid("header/box", width = "medium")
#'
#' # CSS named colour for both h and v rules
#' tbl_demog |> fr_table() |> fr_grid("header/box", color = "navy")
#'
#' ## ── Full pipeline ─────────────────────────────────────────────────────
#'
#' tbl_demog |>
#'   fr_table() |>
#'   fr_titles("Table 14.1.1 Demographics") |>
#'   fr_grid("header/box") |>
#'   fr_page(orientation = "landscape")
#'
#' @seealso [fr_hlines()] for horizontal rules only, [fr_vlines()] for
#'   vertical rules only.
#'
#' @export
fr_grid <- function(
  spec,
  hpreset = "header",
  vpreset = "box",
  width = NULL,
  color = NULL,
  linestyle = NULL
) {
  # Single-string shorthand: "hpreset/vpreset"
  if (
    is.character(hpreset) &&
      length(hpreset) == 1L &&
      grepl("/", hpreset, fixed = TRUE)
  ) {
    parts <- strsplit(hpreset, "/", fixed = TRUE)[[1L]]
    hpreset <- parts[1L]
    vpreset <- parts[2L]
  }

  spec |>
    fr_hlines(hpreset, width = width, color = color, linestyle = linestyle) |>
    fr_vlines(vpreset, width = width, color = color, linestyle = linestyle)
}
