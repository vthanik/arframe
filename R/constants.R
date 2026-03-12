# ──────────────────────────────────────────────────────────────────────────────
# constants.R — Single source of truth for all backend-independent lookup data
#
# DESIGN DECISIONS:
#
#   1. TOKEN SYNTAX: {thepage}, {total_pages}, {program}, {datetime}
#      Same single-brace syntax used for inline markup ({fr_super(1)}).
#      Pagehead/pagefoot strings never pass through glue::glue() — tokens
#      are resolved by simple find-and-replace at render time.
#      This is how Python's string.Template, Rust's format!(), and
#      JavaScript's template literals all work: {name} → value.
#      Literal brace in output: "{{" → "{" (standard glue escaping).
#
#   2. PRIVATE ENVIRONMENT: All constants live inside `fr_env`, a private
#      environment (pattern from ggplot2's `ggplot_global`). Nothing leaks
#      into the package namespace.
#
#   3. rlang: All validation helpers use rlang::caller_arg() and
#      rlang::caller_env() instead of deparse(substitute()).
#
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# Private environment — like ggplot2's ggplot_global
# ══════════════════════════════════════════════════════════════════════════════

#' Private environment for tlframe internal constants and state.
#' Not exported. Other packages should not manipulate this directly.
#' @noRd
fr_env <- new.env(parent = emptyenv())


# ══════════════════════════════════════════════════════════════════════════════
# 1. Font Family Classification
# ══════════════════════════════════════════════════════════════════════════════

fr_env$fonts <- list(

  modern = list(
    names          = c("Courier New", "Courier", "Consolas", "Lucida Console",
                        "DejaVu Sans Mono", "Liberation Mono",
                        "Latin Modern Mono"),
    rtf_family     = "fmodern",
    rtf_prq        = 1L,
    tex_cmd        = "\\ttfamily",
    afm_name       = "Courier",
    afm_bold       = "Courier-Bold",
    afm_italic     = "Courier-Oblique",
    afm_bolditalic = "Courier-BoldOblique"
  ),

  swiss = list(
    names          = c("Arial", "Helvetica", "Calibri", "Verdana", "Tahoma",
                        "Segoe UI", "DejaVu Sans", "Liberation Sans",
                        "Latin Modern Sans"),
    rtf_family     = "fswiss",
    rtf_prq        = 2L,
    tex_cmd        = "\\sffamily",
    afm_name       = "Helvetica",
    afm_bold       = "Helvetica-Bold",
    afm_italic     = "Helvetica-Oblique",
    afm_bolditalic = "Helvetica-BoldOblique"
  ),

  roman = list(
    names          = c("Times New Roman", "Times", "Georgia", "Palatino",
                        "Book Antiqua", "Cambria", "DejaVu Serif",
                        "Liberation Serif", "Latin Modern Roman"),
    rtf_family     = "froman",
    rtf_prq        = 2L,
    tex_cmd        = "\\rmfamily",
    afm_name       = "Times-Roman",
    afm_bold       = "Times-Bold",
    afm_italic     = "Times-Italic",
    afm_bolditalic = "Times-BoldItalic"
  )
)


# Latin Modern fallback map: family type -> Latin Modern font name
# Latin Modern is always available in any tinytex/texlive installation
fr_env$lm_fallback <- c(
  modern = "Latin Modern Mono",
  swiss  = "Latin Modern Sans",
  roman  = "Latin Modern Roman"
)

# CTAN packages required by the LaTeX/PDF backend (preamble + tabularray)
fr_env$required_latex_pkgs <- c(
  "tabularray", "fontspec", "geometry", "xcolor",
  "fancyhdr", "lastpage", "booktabs"
)


#' Get custom font directory from environment variable
#'
#' Reads `TLFRAME_FONT_DIR` and returns the normalized path if the directory
#' exists and contains `.ttf` or `.otf` files. Returns `NULL` otherwise.
#' No caching — `Sys.getenv()` + `dir.exists()` are trivially fast, and
#' avoids stale cache if the user changes the env var mid-session.
#'
#' @return Character scalar (normalized path) or `NULL`.
#' @noRd
get_font_dir <- function() {
  dir <- Sys.getenv("TLFRAME_FONT_DIR", unset = "")
  if (!nzchar(dir) || !dir.exists(dir)) return(NULL)
  files <- list.files(dir, pattern = "\\.(ttf|otf)$", ignore.case = TRUE)
  if (length(files) == 0L) return(NULL)
  normalizePath(dir, mustWork = TRUE)
}


#' Check if a system font is available for XeLaTeX
#'
#' Latin Modern fonts always return `TRUE` (built into tinytex/texlive).
#' If `TLFRAME_FONT_DIR` is set and contains font files, returns `TRUE`
#' (trust the user — XeLaTeX resolves names via `OSFONTDIR`).
#' On Windows, standard fonts are always present. On Linux/macOS, checks
#' via `fc-list`.
#'
#' @param font_name Character scalar. Font name to check.
#' @return Logical scalar.
#' @noRd
is_system_font_available <- function(font_name) {

  # Latin Modern fonts are always available in tinytex/texlive
  if (font_name %in% fr_env$lm_fallback) return(TRUE)

  # Custom font directory set — trust the user
  if (!is.null(get_font_dir())) return(TRUE)

  os <- tolower(Sys.info()[["sysname"]])

  # Windows always has standard fonts (Courier New, Arial, Times New Roman)
  if (os == "windows") return(TRUE)

  # Linux/macOS: use fc-list to check (cached per session)
  fonts <- get_system_font_list()
  # TRUE sentinel means "can't determine, assume available"
  if (isTRUE(fonts)) return(TRUE)
  font_name %in% fonts
}


#' Get cached list of system font family names (Linux/macOS only)
#'
#' Calls `fc-list` once per session and caches the result in `fr_env`.
#' Returns a character vector of available font family names.
#'
#' @return Character vector of font family names, or `NULL` if fc-list
#'   is unavailable (callers treat NULL as "assume all fonts available").
#' @noRd
get_system_font_list <- function() {
  if (!is.null(fr_env$system_fonts)) return(fr_env$system_fonts)

  fc_list <- Sys.which("fc-list")
  if (!nzchar(fc_list)) {
    # Can't check — return sentinel that matches everything
    fr_env$system_fonts <- TRUE
    return(TRUE)
  }

  result <- tryCatch(
    system2("fc-list", args = c(":", "family"), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )

  if (length(result) == 0L) {
    fr_env$system_fonts <- TRUE
    return(TRUE)
  }

  # fc-list returns "family1,family2" per line; extract all family names
  all_families <- unique(trimws(unlist(
    strsplit(result, ",", fixed = TRUE)
  )))
  fr_env$system_fonts <- all_families
  all_families
}


#' Resolve a font for LaTeX rendering, with Latin Modern fallback
#'
#' If the requested font is available on the system, returns it as-is.
#' Otherwise, falls back to the equivalent Latin Modern font (always
#' available in tinytex/texlive).
#'
#' @param font_name Character scalar. Requested font name.
#' @return Character scalar. The resolved font name for fontspec.
#' @noRd
resolve_latex_font <- function(font_name) {

  if (is_system_font_available(font_name)) {
    return(font_name)
  }

  # Determine family type and map to Latin Modern equivalent
  fam <- lookup_font_family(font_name)
  lm_name <- fr_env$lm_fallback[[fam]]

  cli::cli_inform(c(
    "i" = "Font {.val {font_name}} not found on system.",
    "*" = "Falling back to {.val {lm_name}} (tinytex built-in)."
  ))

  lm_name
}


#' Resolve a font for RTF rendering, with OS-appropriate fallback
#'
#' If the requested font is available on the system, returns it as-is.
#' Otherwise, falls back to the OS default font for the same family type
#' (e.g., Courier New, Arial, or Times New Roman on Windows).
#'
#' @param font_name Character scalar. Requested font name.
#' @return Character scalar. The resolved font name for RTF.
#' @noRd
resolve_rtf_font <- function(font_name) {

  if (is_system_font_available(font_name)) {
    return(font_name)
  }

  # Map to OS default of same family type
  fam <- lookup_font_family(font_name)
  defaults <- os_default_fonts()
  fallback <- switch(fam,
    modern = defaults$mono,
    swiss  = defaults$sans,
    roman  = defaults$serif
  )

  cli::cli_inform(c(
    "i" = "Font {.val {font_name}} not found on system.",
    "*" = "Falling back to {.val {fallback}} for RTF output."
  ))

  fallback
}


#' OS-aware default font triplet (mono, sans, serif)
#' @return Named list: `mono`, `sans`, `serif`
#' @noRd
os_default_fonts <- function() {
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "windows") {
    list(mono = "Courier New", sans = "Arial",     serif = "Times New Roman")
  } else if (os == "darwin") {
    list(mono = "Courier New", sans = "Helvetica",  serif = "Times New Roman")
  } else {
    # Prefer Microsoft fonts if installed; fall back to Latin Modern
    if (is_system_font_available("Courier New")) {
      list(mono = "Courier New", sans = "Arial", serif = "Times New Roman")
    } else {
      list(mono = "Latin Modern Mono", sans = "Latin Modern Sans",
           serif = "Latin Modern Roman")
    }
  }
}


#' Lookup font family type from font name
#' @param font_name Character scalar. e.g. "Courier New"
#' @return Character: "modern", "swiss", or "roman"
#' @noRd
lookup_font_family <- function(font_name) {
  for (fam in names(fr_env$fonts)) {
    if (font_name %in% fr_env$fonts[[fam]]$names) return(fam)
  }
  "modern"
}


#' Resolve AFM font name from font family + bold/italic
#'
#' @param font_family Character. Family type ("modern"/"swiss"/"roman") or
#'   a font name like "Courier New".
#' @param bold Logical.
#' @param italic Logical.
#' @return Character. AFM font name (e.g. "Helvetica-Bold").
#' @noRd
resolve_afm_name <- function(font_family, bold = FALSE, italic = FALSE) {
  fam <- if (font_family %in% names(fr_env$fonts)) {
    font_family
  } else {
    lookup_font_family(font_family)
  }
  info <- fr_env$fonts[[fam]]
  if (bold && italic) return(info$afm_bolditalic)
  if (bold)           return(info$afm_bold)
  if (italic)         return(info$afm_italic)
  info$afm_name
}


#' Get RTF font family keyword
#' @noRd
get_rtf_font_family <- function(font_name) {
  fam <- lookup_font_family(font_name)
  fr_env$fonts[[fam]]$rtf_family
}


#' Get RTF pitch value
#' @noRd
get_rtf_font_prq <- function(font_name) {
  fam <- lookup_font_family(font_name)
  fr_env$fonts[[fam]]$rtf_prq
}


#' Get LaTeX font family command
#' @noRd
get_tex_font_cmd <- function(font_name) {
  fam <- lookup_font_family(font_name)
  fr_env$fonts[[fam]]$tex_cmd
}


# ══════════════════════════════════════════════════════════════════════════════
# 2. Paper Dimensions (twips; 1 inch = 1440 twips)
# ══════════════════════════════════════════════════════════════════════════════

fr_env$paper <- list(
  letter = c(width = 12240L, height = 15840L),
  a4     = c(width = 11906L, height = 16838L),
  legal  = c(width = 12240L, height = 20163L)
)


#' Get paper dimensions, accounting for orientation
#' @noRd
paper_dims_twips <- function(paper = "letter", orientation = "landscape") {
  dims <- fr_env$paper[[paper]]
  if (is.null(dims)) {
    cli_warn("Unknown paper size {.val {paper}}, using {.val letter}.")
    dims <- fr_env$paper[["letter"]]
  }
  if (orientation == "landscape") {
    c(width = unname(dims[["height"]]), height = unname(dims[["width"]]))
  } else {
    dims
  }
}


# ══════════════════════════════════════════════════════════════════════════════
# 3. RTF Border Styles + hline Presets
#
# linestyle values follow CSS <line-style> (MDN / W3C):
#   solid | dashed | dotted | double
#
# Preset names follow HTML <table frame=> (RFC 1942 / W3C):
#   void    — no outer rules
#   above   — top rule only
#   below   — bottom rule only
#   hsides  — top and bottom rules
#   box     — all four sides
# Plus booktabs from the LaTeX Companion (3rd ed, §6.6.5).
# ══════════════════════════════════════════════════════════════════════════════

#' RTF border linestyle control words
#'
#' Maps tlframe linestyle names to RTF border style control words.
#' Ref: Word 2007 RTF Specification §2.6.6.3.
#' @noRd
fr_env$linestyle_rtf <- list(
  solid   = "\\brdrs",
  dashed  = "\\brdrdash",
  dotted  = "\\brdrdot",
  double  = "\\brdrdb",
  dashdot = "\\brdrdashd"
)

#' Valid linestyle values
#'
#' Follows CSS <line-style> vocabulary (MDN / W3C) plus "dashdot" from
#' SAS ODS BORDERSTYLE. Used in fr_hlines(), fr_vlines(), and new_fr_rule().
#' @noRd
fr_env$valid_linestyles <- c("solid", "dashed", "dotted", "double", "dashdot")

#' Named line width constants (in points)
#'
#' Used by resolve_line_width() to translate named widths to numeric pt values.
#' Pharma convention: thin (0.5pt) is the standard rule weight.
#' @noRd
fr_env$line_widths <- c(
  hairline = 0.25,
  thin     = 0.50,
  medium   = 1.00,
  thick    = 1.50
)


#' Resolve a line width to a numeric pt value
#'
#' Accepts:
#'   - A named shorthand: "hairline" (0.25pt), "thin" (0.5pt),
#'     "medium" (1pt), "thick" (1.5pt)
#'   - A positive numeric value in points (e.g. 0.75)
#'   - NULL — returns the default width (0.5pt, "thin")
#'
#' @param wd Character shorthand or positive numeric (pt). NULL → 0.5.
#' @return Numeric. Width in points.
#' @noRd
resolve_line_width <- function(wd,
                                arg  = caller_arg(wd),
                                call = caller_env()) {
  if (is.null(wd)) return(0.5)

  if (is.character(wd)) {
    if (length(wd) != 1L) {
      cli_abort("{.arg {arg}} must be a single string or number.", call = call)
    }
    w <- unname(fr_env$line_widths[wd])
    if (is.na(w)) {
      cli_abort(
        c("{.arg {arg}} {.val {wd}} is not a recognised width name.",
          "i" = "Named options: {.val {names(fr_env$line_widths)}}.",
          "i" = "Or pass a positive number in points, e.g. {.code wd = 0.75}."),
        arg = arg, call = call
      )
    }
    return(w)
  }

  check_positive_num(wd, arg = arg, call = call)
  wd
}


fr_env$cell_border_rtf <- list(
  top    = "\\clbrdrt",
  bottom = "\\clbrdrb",
  left   = "\\clbrdrl",
  right  = "\\clbrdrr"
)

fr_env$para_border_rtf <- list(
  top    = "\\brdrt",
  bottom = "\\brdrb",
  left   = "\\brdrl",
  right  = "\\brdrr"
)


# ── Grouped sub-list: presets ──────────────────────────────────────────────
#' Preset constants for rules and line widths.
#' Access via fr_env$presets$hline, fr_env$presets$line_widths, etc.
#' Legacy flat accessors (fr_env$hline_presets, fr_env$line_widths) are
#' preserved below for backward compatibility.
#' @noRd
fr_env$presets <- list()

#' hline preset definitions
#'
#' Names:
#'   header     — single rule below column header (most common in TFL outputs)
#'   open       — rule above header + below header; no bottom border
#'   hsides     — top and bottom rules, no mid-rule (HTML frame="hsides")
#'   above      — top rule only (HTML frame="above")
#'   below      — bottom rule only (HTML frame="below")
#'   box        — full outer border on all four sides
#'   booktabs   — thick top + thin mid + thick bottom (LaTeX booktabs package)
#'   void       — no rules at all
#'
#' Each rule definition: region, side, width (pt), linestyle, fg (hex)
#' box is a special sentinel — backends render all four borders separately.
#' @noRd
fr_env$hline_presets <- list(

  # header — single thin rule below column header only.
  # The most common style in TFL outputs (ICH E3).
  # No top border, no bottom border; footnotes follow the last body row directly.
  header = list(
    list(region = "header", side = "below", width = 0.5,
         linestyle = "solid", fg = "#000000")
  ),

  # open — rule above header + rule below header; no bottom border.
  # Use when the table bottom is immediately followed by footnotes.
  open = list(
    list(region = "header", side = "above", width = 0.5,
         linestyle = "solid", fg = "#000000"),
    list(region = "header", side = "below", width = 0.5,
         linestyle = "solid", fg = "#000000")
  ),

  # hsides — top and bottom rules only, no mid-rule.
  hsides = list(
    list(region = "header", side = "above", width = 0.5,
         linestyle = "solid", fg = "#000000"),
    list(region = "body",   side = "below", width = 0.5,
         linestyle = "solid", fg = "#000000")
  ),

  # above — single rule above column header only.
  above = list(
    list(region = "header", side = "above", width = 0.5,
         linestyle = "solid", fg = "#000000")
  ),

  # below — single rule below last body row only.
  below = list(
    list(region = "body", side = "below", width = 0.5,
         linestyle = "solid", fg = "#000000")
  ),

  # box — all four outer sides (sentinel; backends expand to 4 borders).
  box = "box",

  # booktabs — from LaTeX Companion §6.6.5:
  #   \toprule    heavyrulewidth ≈ 1.0pt
  #   \midrule    lightrulewidth ≈ 0.5pt
  #   \bottomrule heavyrulewidth ≈ 1.0pt
  booktabs = list(
    list(region = "header", side = "above", width = 1.0,
         linestyle = "solid", fg = "#000000"),
    list(region = "header", side = "below", width = 0.5,
         linestyle = "solid", fg = "#000000"),
    list(region = "body",   side = "below", width = 1.0,
         linestyle = "solid", fg = "#000000")
  ),

  # void — no rules.
  void = list()
)

# Grouped sub-list aliases for organized access
fr_env$presets$hline       <- fr_env$hline_presets
fr_env$presets$linestyles  <- fr_env$valid_linestyles
fr_env$presets$line_widths <- fr_env$line_widths


# ══════════════════════════════════════════════════════════════════════════════
# 4. LaTeX Special Character Escaping + Unicode Map
# ══════════════════════════════════════════════════════════════════════════════

fr_env$latex_specials <- c(
  "\\" = "\\textbackslash{}",
  "&"  = "\\&",
  "%"  = "\\%",
  "$"  = "\\$",
  "#"  = "\\#",
  "_"  = "\\_",
  "{"  = "\\{",
  "}"  = "\\}",
  "~"  = "\\textasciitilde{}",
  "^"  = "\\textasciicircum{}"
)

fr_env$rtf_specials <- c(
  "\\" = "\\\\",
  "{"  = "\\{",
  "}"  = "\\}"
)

#' Unicode → LaTeX command map
#'
#' Each entry: "unicode_char" = "latex_command"
#' Inline comments show the original character for maintainability.
#' @noRd
fr_env$latex_unicode <- c(
  # ── Math operators & relations ──
  "\u00b1" = "\\ensuremath{\\pm}",            # ± plus-minus
  "\u2264" = "\\ensuremath{\\leq}",           # ≤ less-than-or-equal
  "\u2265" = "\\ensuremath{\\geq}",           # ≥ greater-than-or-equal
  "\u2260" = "\\ensuremath{\\neq}",           # ≠ not-equal
  "\u00d7" = "\\ensuremath{\\times}",         # × multiplication
  "\u00f7" = "\\ensuremath{\\div}",           # ÷ division
  "\u221e" = "\\ensuremath{\\infty}",         # ∞ infinity
  "\u2248" = "\\ensuremath{\\approx}",        # ≈ approximately
  "\u221a" = "\\ensuremath{\\sqrt{}}",        # √ square root

  # ── Superscript digits ──
  "\u00b9" = "\\textsuperscript{1}",          # ¹ superscript one
  "\u00b2" = "\\textsuperscript{2}",          # ² superscript two
  "\u00b3" = "\\textsuperscript{3}",          # ³ superscript three

  # ── Greek letters (common in clinical statistics) ──
  "\u03b1" = "\\ensuremath{\\alpha}",         # α alpha
  "\u03b2" = "\\ensuremath{\\beta}",          # β beta
  "\u03b3" = "\\ensuremath{\\gamma}",         # γ gamma
  "\u03b4" = "\\ensuremath{\\delta}",         # δ delta
  "\u03bb" = "\\ensuremath{\\lambda}",        # λ lambda
  "\u03bc" = "\\ensuremath{\\mu}",            # μ mu
  "\u03c0" = "\\ensuremath{\\pi}",            # π pi
  "\u03c3" = "\\ensuremath{\\sigma}",         # σ sigma
  "\u03c7" = "\\ensuremath{\\chi}",           # χ chi

  # ── Typographic symbols ──
  "\u2020" = "\\textdagger{}",                # † dagger
  "\u2021" = "\\textdaggerdbl{}",             # ‡ double dagger
  "\u00a7" = "\\textsection{}",               # § section sign
  "\u00b6" = "\\textparagraph{}",             # ¶ pilcrow / paragraph
  "\u2022" = "\\textbullet{}",                # • bullet
  "\u2013" = "\\textendash{}",                # – en-dash
  "\u2014" = "\\textemdash{}",                # — em-dash
  "\u2018" = "`",                             # ' left single quote
  "\u2019" = "'",                             # ' right single quote
  "\u201c" = "``",                            # " left double quote
  "\u201d" = "''",                            # " right double quote
  "\u2026" = "\\ldots{}",                     # … horizontal ellipsis

  # ── Legal / trademark ──
  "\u00a9" = "\\textcopyright{}",             # © copyright
  "\u00ae" = "\\textregistered{}",            # ® registered
  "\u2122" = "\\texttrademark{}",             # ™ trademark

  # ── Arrows (shift tables, forest plots) ──
  "\u2190" = "\\ensuremath{\\leftarrow}",     # ← left arrow
  "\u2191" = "\\ensuremath{\\uparrow}",       # ↑ up arrow
  "\u2192" = "\\ensuremath{\\rightarrow}",    # → right arrow
  "\u2193" = "\\ensuremath{\\downarrow}",     # ↓ down arrow

  # ── Fractions & degree ──
  "\u00b0" = "\\textdegree{}",                # ° degree
  "\u00bc" = "\\textonequarter{}",            # ¼ one quarter
  "\u00bd" = "\\textonehalf{}",               # ½ one half
  "\u00be" = "\\textthreequarters{}"          # ¾ three quarters
)

#' Unicode → RTF control word map
#'
#' RTF handles Unicode via \\uN ? where N = decimal codepoint,
#' ? = ANSI fallback. Entries below use ANSI shorthand where available.
#' @noRd
fr_env$rtf_unicode <- c(
  "\u2020" = "\\'86",                         # † dagger
  "\u2021" = "\\'87",                         # ‡ double dagger
  "\u2022" = "\\'95",                         # • bullet
  "\u2013" = "\\'96",                         # – en-dash
  "\u2014" = "\\'97",                         # — em-dash
  "\u2018" = "\\'91",                         # ' left single quote
  "\u2019" = "\\'92",                         # ' right single quote
  "\u201c" = "\\'93",                         # " left double quote
  "\u201d" = "\\'94"                          # " right double quote
)


# ══════════════════════════════════════════════════════════════════════════════
# 5. Color Utilities
# ══════════════════════════════════════════════════════════════════════════════

# Full CSS Color Level 4 named colors (148 colors)
# Standard across CSS, ggplot2, matplotlib, D3, and all modern dev tools.
# See: https://www.w3.org/TR/css-color-4/#named-colors
fr_env$named_colors <- c(
  aliceblue            = "#F0F8FF",
  antiquewhite         = "#FAEBD7",
  aqua                 = "#00FFFF",
  aquamarine           = "#7FFFD4",
  azure                = "#F0FFFF",
  beige                = "#F5F5DC",
  bisque               = "#FFE4C4",
  black                = "#000000",
  blanchedalmond       = "#FFEBCD",
  blue                 = "#0000FF",
  blueviolet           = "#8A2BE2",
  brown                = "#A52A2A",
  burlywood            = "#DEB887",
  cadetblue            = "#5F9EA0",
  chartreuse           = "#7FFF00",
  chocolate            = "#D2691E",
  coral                = "#FF7F50",
  cornflowerblue       = "#6495ED",
  cornsilk             = "#FFF8DC",
  crimson              = "#DC143C",
  cyan                 = "#00FFFF",
  darkblue             = "#00008B",
  darkcyan             = "#008B8B",
  darkgoldenrod        = "#B8860B",
  darkgray             = "#A9A9A9",
  darkgreen            = "#006400",
  darkgrey             = "#A9A9A9",
  darkkhaki            = "#BDB76B",
  darkmagenta          = "#8B008B",
  darkolivegreen       = "#556B2F",
  darkorange           = "#FF8C00",
  darkorchid           = "#9932CC",
  darkred              = "#8B0000",
  darksalmon           = "#E9967A",
  darkseagreen         = "#8FBC8F",
  darkslateblue        = "#483D8B",
  darkslategray        = "#2F4F4F",
  darkslategrey        = "#2F4F4F",
  darkturquoise        = "#00CED1",
  darkviolet           = "#9400D3",
  deeppink             = "#FF1493",
  deepskyblue          = "#00BFFF",
  dimgray              = "#696969",
  dimgrey              = "#696969",
  dodgerblue           = "#1E90FF",
  firebrick            = "#B22222",
  floralwhite          = "#FFFAF0",
  forestgreen          = "#228B22",
  fuchsia              = "#FF00FF",
  gainsboro            = "#DCDCDC",
  ghostwhite           = "#F8F8FF",
  gold                 = "#FFD700",
  goldenrod            = "#DAA520",
  gray                 = "#808080",
  green                = "#008000",
  greenyellow          = "#ADFF2F",
  grey                 = "#808080",
  honeydew             = "#F0FFF0",
  hotpink              = "#FF69B4",
  indianred            = "#CD5C5C",
  indigo               = "#4B0082",
  ivory                = "#FFFFF0",
  khaki                = "#F0E68C",
  lavender             = "#E6E6FA",
  lavenderblush        = "#FFF0F5",
  lawngreen            = "#7CFC00",
  lemonchiffon         = "#FFFACD",
  lightblue            = "#ADD8E6",
  lightcoral           = "#F08080",
  lightcyan            = "#E0FFFF",
  lightgoldenrodyellow = "#FAFAD2",
  lightgray            = "#D3D3D3",
  lightgreen           = "#90EE90",
  lightgrey            = "#D3D3D3",
  lightpink            = "#FFB6C1",
  lightsalmon          = "#FFA07A",
  lightseagreen        = "#20B2AA",
  lightskyblue         = "#87CEFA",
  lightslategray       = "#778899",
  lightslategrey       = "#778899",
  lightsteelblue       = "#B0C4DE",
  lightyellow          = "#FFFFE0",
  lime                 = "#00FF00",
  limegreen            = "#32CD32",
  linen                = "#FAF0E6",
  magenta              = "#FF00FF",
  maroon               = "#800000",
  mediumaquamarine     = "#66CDAA",
  mediumblue           = "#0000CD",
  mediumorchid         = "#BA55D3",
  mediumpurple         = "#9370DB",
  mediumseagreen       = "#3CB371",
  mediumslateblue      = "#7B68EE",
  mediumspringgreen    = "#00FA9A",
  mediumturquoise      = "#48D1CC",
  mediumvioletred      = "#C71585",
  midnightblue         = "#191970",
  mintcream            = "#F5FFFA",
  mistyrose            = "#FFE4E1",
  moccasin             = "#FFE4B5",
  navajowhite          = "#FFDEAD",
  navy                 = "#000080",
  oldlace              = "#FDF5E6",
  olive                = "#808000",
  olivedrab            = "#6B8E23",
  orange               = "#FFA500",
  orangered            = "#FF4500",
  orchid               = "#DA70D6",
  palegoldenrod        = "#EEE8AA",
  palegreen            = "#98FB98",
  paleturquoise        = "#AFEEEE",
  palevioletred        = "#DB7093",
  papayawhip           = "#FFEFD5",
  peachpuff            = "#FFDAB9",
  peru                 = "#CD853F",
  pink                 = "#FFC0CB",
  plum                 = "#DDA0DD",
  powderblue           = "#B0E0E6",
  purple               = "#800080",
  rebeccapurple        = "#663399",
  red                  = "#FF0000",
  rosybrown            = "#BC8F8F",
  royalblue            = "#4169E1",
  saddlebrown          = "#8B4513",
  salmon               = "#FA8072",
  sandybrown           = "#F4A460",
  seagreen             = "#2E8B57",
  seashell             = "#FFF5EE",
  sienna               = "#A0522D",
  silver               = "#C0C0C0",
  skyblue              = "#87CEEB",
  slateblue            = "#6A5ACD",
  slategray            = "#708090",
  slategrey            = "#708090",
  snow                 = "#FFFAFA",
  springgreen          = "#00FF7F",
  steelblue            = "#4682B4",
  tan                  = "#D2B48C",
  teal                 = "#008080",
  thistle              = "#D8BFD8",
  tomato               = "#FF6347",
  turquoise            = "#40E0D0",
  violet               = "#EE82EE",
  wheat                = "#F5DEB3",
  white                = "#FFFFFF",
  whitesmoke           = "#F5F5F5",
  yellow               = "#FFFF00",
  yellowgreen          = "#9ACD32"
)


#' Parse hex color string to integer RGB triple
#' @noRd
hex_to_rgb <- function(hex) {
  hex <- sub("^#", "", hex)
  if (nchar(hex) == 3L) {
    hex <- paste0(substr(hex, 1, 1), substr(hex, 1, 1),
                  substr(hex, 2, 2), substr(hex, 2, 2),
                  substr(hex, 3, 3), substr(hex, 3, 3))
  }
  if (nchar(hex) != 6L) {
    cli_abort(c(
      "Invalid hex color {.val {paste0('#', hex)}}. Expected 3 or 6 hex digits.",
      "i" = "Example: {.code \"#003366\"} or {.code \"#036\"}."
    ))
  }
  c(
    r = strtoi(substr(hex, 1, 2), 16L),
    g = strtoi(substr(hex, 3, 4), 16L),
    b = strtoi(substr(hex, 5, 6), 16L)
  )
}


#' Format RGB as RTF color definition
#' @noRd
rgb_to_rtf_color <- function(r, g, b) {
  paste0("\\red", r, "\\green", g, "\\blue", b)
}


#' Format hex as LaTeX xcolor value
#' @noRd
hex_to_latex_color <- function(hex) {
  toupper(sub("^#", "", hex))
}


#' Resolve color: accepts hex strings or CSS named colors
#' @noRd
resolve_color <- function(color, arg = caller_arg(color), call = caller_env()) {
  if (is.null(color) || is.na(color)) return(NULL)
  if (startsWith(color, "#")) {
    if (!grepl("^#[0-9A-Fa-f]{6}$", color)) {
      cli_abort(c(
        "Invalid hex color {.val {color}}.",
        "i" = "Hex colors must be 6-digit format: {.val #RRGGBB}."
      ), arg = arg, call = call)
    }
    return(toupper(color))
  }
  key <- tolower(color)
  named <- fr_env$named_colors[key]
  if (is.na(named)) {
    # Suggest closest matches via agrep
    all_names <- names(fr_env$named_colors)
    close <- agrep(key, all_names, max.distance = 0.3, value = TRUE)
    hint <- if (length(close) > 0L) {
      paste0("Did you mean: ", paste0("{.val ", close[seq_len(min(5L, length(close)))], "}", collapse = ", "), "?")
    } else {
      "Use a hex string (e.g. {.val #003366}) or any CSS named colour (e.g. {.val steelblue}, {.val tomato})."
    }
    cli_abort(c(
      "Unknown color name {.val {color}}.",
      "i" = hint
    ), arg = arg, call = call)
  }
  unname(named)
}


# ══════════════════════════════════════════════════════════════════════════════
# 6. Token System for pagehead / pagefoot
#
# SYNTAX:  {thepage}  {total_pages}  {program}  {datetime}
#
# Same single-brace syntax as inline markup ({fr_super(1)}).
# Consistent with Python format(), Rust format!(), JS template literals.
#
# Why this works without collision:
#   - Inline markup uses {fr_*(...)} — always starts with "fr_"
#   - Tokens use {name} — never starts with "fr_"
#   - Literal brace: "{{" → "{" (standard glue escaping)
#
# Custom tokens via fr_page(tokens = list(study = "ABC-001")):
#   fr_pagehead(left = "{study}")
#
# ══════════════════════════════════════════════════════════════════════════════

fr_env$builtin_tokens <- c("thepage", "total_pages", "program", "datetime")


#' Resolve {token} placeholders in a string
#'
#' Scans for `{name}` patterns where name is NOT prefixed with "fr_"
#' (those are markup, not tokens). Replaces with values from token_map.
#' Literal "{{" → "{" and "}}" → "}" (standard escaping).
#'
#' @param text Character vector (supports multi-line headers).
#' @param token_map Named list of token_name → value.
#' @param context Character. For error messages.
#' @return Character vector with tokens resolved.
#' @noRd
resolve_tokens <- function(text, token_map, context = "page header/footer") {
  if (is.null(text) || !is.character(text)) return(text)
  vapply(text, function(txt) {
    resolve_tokens_single(txt, token_map, context)
  }, character(1), USE.NAMES = FALSE)
}


#' Resolve tokens in a single string
#' @noRd
resolve_tokens_single <- function(text, token_map, context) {
  # Protect escaped braces: {{ → sentinel, }} → sentinel
  text <- gsub("\\{\\{", "\x01LBRACE\x02", text, fixed = FALSE)
  text <- gsub("\\}\\}", "\x01RBRACE\x02", text, fixed = FALSE)

  # Find all {name} tokens that are NOT {fr_*(...)} markup
  # Token pattern: { word_chars } where word doesn't start with "fr_"
  pattern <- "\\{([A-Za-z_][A-Za-z0-9_]*)\\}"
  m <- gregexpr(pattern, text, perl = TRUE)
  tokens_found <- regmatches(text, m)[[1]]

  if (length(tokens_found) > 0L) {
    token_names <- sub("^\\{", "", sub("\\}$", "", tokens_found))

    # Skip any that look like markup (start with "fr_")
    is_markup <- startsWith(token_names, "fr_")

    for (i in seq_along(token_names)) {
      if (is_markup[[i]]) next
      nm <- token_names[[i]]
      val <- token_map[[nm]]
      if (is.null(val)) {
        cli_abort(c(
          "Unknown token {.val {{{nm}}}} in {context}.",
          "i" = "Available tokens: {.val {names(token_map)}}.",
          "i" = "Example: {.code fr_pagehead(left = \"{{program}}\")}"
        ))
      }
      text <- sub(paste0("{", nm, "}"), as.character(val), text, fixed = TRUE)
    }
  }

  # Restore escaped braces
  text <- gsub("\x01LBRACE\x02", "{", text, fixed = TRUE)
  text <- gsub("\x01RBRACE\x02", "}", text, fixed = TRUE)
  text
}


# ══════════════════════════════════════════════════════════════════════════════
# 7. Alignment Map
# ══════════════════════════════════════════════════════════════════════════════

fr_env$valid_aligns <- c("left", "center", "right", "decimal")

fr_env$align_to_rtf <- c(
  left    = "\\ql",
  center  = "\\qc",
  right   = "\\qr",
  decimal = "\\ql"
)

fr_env$align_to_latex <- c(
  left    = "L",
  center  = "C",
  right   = "R",
  decimal = "L"
)

# Vertical alignment (cell-level property, orthogonal to horizontal align)
# RTF: \clvertalt (top, default), \clvertalc (center), \clvertalb (bottom)
# LaTeX: t (top), m (middle), b (bottom) — tabularray valign key
fr_env$valid_valigns <- c("top", "middle", "bottom")

fr_env$valid_spaces <- c("indent", "preserve")

fr_env$valign_to_rtf <- c(
  top    = "",
  middle = "\\clvertalc",
  bottom = "\\clvertalb"
)

fr_env$valign_to_latex <- c(
  top    = "t",
  middle = "m",
  bottom = "b"
)


# ══════════════════════════════════════════════════════════════════════════════
# 8. LaTeX Border Linestyle Map
# ══════════════════════════════════════════════════════════════════════════════

#' tabularray border linestyle names
#' @noRd
# ── LaTeX spacing constants ──────────────────────────────────────────────
fr_env$latex_leading_factor <- 1.15
fr_env$latex_rowsep <- "0pt"

# ── Group key separator ─────────────────────────────────────────────────
# Used to build composite group keys when page_by/group_by has multiple columns.
# Unit separator (U+001F) is safe because it never appears in data content.
fr_env$group_sep <- "\x1f"

# ── RTF rendering constants ────────────────────────────────────────────────
fr_env$rtf_leading_factor    <- 1.4
fr_env$rtf_min_headery       <- 360L
fr_env$rtf_decimal_pad       <- 36L
fr_env$rtf_box_border_wd     <- 0.5
fr_env$rtf_spanner_brdrw     <- 10L

# ── Page break / keep-together defaults ───────────────────────────────────
fr_env$default_orphan_min      <- 3L
fr_env$default_widow_min       <- 3L

# ── LaTeX rendering constants (additional) ────────────────────────────────
fr_env$latex_space_width_em    <- 0.55
fr_env$latex_fn_sep_width_pt   <- 0.4
fr_env$points_per_inch         <- 72

fr_env$linestyle_latex <- c(
  solid   = "solid",
  dashed  = "dashed",
  dotted  = "dotted",
  double  = "double",
  dashdot = "dashed"
)


# ── Grouped sub-list: rtf ──────────────────────────────────────────────────
#' RTF-specific constants for convenient grouped access.
#' @noRd
fr_env$rtf <- list(
  linestyle   = fr_env$linestyle_rtf,
  cell_border = fr_env$cell_border_rtf,
  para_border = fr_env$para_border_rtf,
  specials    = fr_env$rtf_specials,
  unicode     = fr_env$rtf_unicode
)

# ── Grouped sub-list: validation ────────────────────────────────────────────
#' Validation-related constants.
#' @noRd
fr_env$validation <- list(
  aligns     = fr_env$valid_aligns,
  valigns    = fr_env$valid_valigns,
  linestyles = fr_env$valid_linestyles
)


# ══════════════════════════════════════════════════════════════════════════════
# 9. Backend Registry
#
# Initialised here with built-in backends. Render functions are assigned
# in .onLoad() since they don't exist yet when constants.R is sourced.
# Third-party packages can register additional backends via
# fr_register_backend().
# ══════════════════════════════════════════════════════════════════════════════

fr_env$backends <- list()
