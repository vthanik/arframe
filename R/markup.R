# ──────────────────────────────────────────────────────────────────────────────
# markup.R — Inline rich text via glue-string expressions
#
# DESIGN:
#   Users embed {fr_super(1)}, {fr_bold("text")}, etc. directly in strings.
#   When the string contains {fr_*(...)} expressions, the pipeline evaluates
#   them via glue::glue(). Each fr_* helper returns an fr_markup S3 object
#   whose format() / as.character() method emits a sentinel token:
#
#     "\x01SUPER:1\x02"
#
#   At render time, backend-specific resolvers replace sentinels with
#   RTF control words or LaTeX commands.
#
#   Plain strings (no {fr_} patterns) pass through with zero overhead.
#
# EXAMPLES:
#   fr_footnotes("{fr_super(1)} Pearson chi-square test")
#   fr_titles("{fr_bold('Table 14.1.1')}")
#   fr_col("BMI kg/m{fr_super(2)}", width = 1.5)
#   num <- 1; fr_footnotes("{fr_super(num)} test")
#
# ──────────────────────────────────────────────────────────────────────────────


# ══════════════════════════════════════════════════════════════════════════════
# 1. S3 Constructor (internal)
# ══════════════════════════════════════════════════════════════════════════════

#' Create an inline markup node
#'
#' Low-level constructor. Users typically call the helpers
#' (fr_super, fr_bold, etc.) which call this internally.
#'
#' @param type Character. Markup type: "SUPER", "SUB", "BOLD", "ITALIC",
#'   "UNDERLINE", "UNICODE", "NEWLINE".
#' @param content Character. Text content of the markup node.
#' @return An S3 object of class `fr_markup`.
#' @noRd
new_fr_markup <- function(type, content = "") {
  structure(
    list(type = type, content = as.character(content)),
    class = "fr_markup"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# 2. format / as.character — Emit sentinel tokens for glue interpolation
#
# When glue::glue("{fr_super(1)}") evaluates fr_super(1), it gets an
# fr_markup object. Glue then calls format() on it, which produces:
#   "\x01SUPER:1\x02"
# This sentinel survives string concatenation and is resolved at render time.
# ══════════════════════════════════════════════════════════════════════════════

#' @export
format.fr_markup <- function(x, ...) {
  markup_sentinel(x$type, x$content)
}

#' @export
as.character.fr_markup <- function(x, ...) {
  format.fr_markup(x, ...)
}


# ══════════════════════════════════════════════════════════════════════════════
# 3. print — Human-readable preview
# ══════════════════════════════════════════════════════════════════════════════

#' @export
print.fr_markup <- function(x, ...) {
  preview <- switch(toupper(x$type),
    "SUPER"     = paste0("^{", x$content, "}"),
    "SUB"       = paste0("_{", x$content, "}"),
    "BOLD"      = paste0("**", x$content, "**"),
    "ITALIC"    = paste0("_",  x$content, "_"),
    "UNDERLINE" = paste0("__", x$content, "__"),
    "UNICODE"   = x$content,
    "NEWLINE"   = "<newline>",
    paste0("<", x$type, ":", x$content, ">")
  )
  cat("<fr_markup> ", preview, "\n", sep = "")
  invisible(x)
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. User-Facing Markup Helpers
#
# These are designed to be called INSIDE glue strings:
#   "{fr_super(1)} Pearson chi-square test"
#
# They can also be used standalone:
#   node <- fr_super(1)
#   format(node)  # "\x01SUPER:1\x02"
# ══════════════════════════════════════════════════════════════════════════════

#' Superscript Markup
#'
#' @description
#'
#' Renders content as superscript in the final output. Use inside
#' glue-string expressions in titles, footnotes, column labels, and cell
#' values. At render time the backend converts the markup to the
#' appropriate format (RTF control words or LaTeX `\textsuperscript{}`).
#'
#' The most common use in regulatory tables is for footnote markers:
#' `"[a]{fr_super('a')}"` in a cell value, paired with
#' `"[a] Percentages based on N."` in [fr_footnotes()].
#'
#' @param x Character or numeric. Content to render as superscript.
#'   Coerced to character internally.
#'
#' @return An `fr_markup` object. When interpolated inside a glue string
#'   (e.g. `"{fr_super(1)}"`), produces a sentinel token
#'   (`\x01SUPER:1\x02`) that is resolved at render time.
#'
#' @examples
#' # Footnote marker
#' fr_super("a")
#'
#' # Unit with exponent: kg/m²
#' fr_col("BMI kg/m{fr_super(2)}", width = 1.5)
#'
#' # Standalone usage (inspect the sentinel)
#' format(fr_super(1))
#'
#' # In a pipeline:
#' spec <- tbl_demog |> fr_table()
#' n <- 1
#' spec |> fr_footnotes("{fr_super(n)} Pearson chi-square test")
#'
#' @seealso [fr_sub()] for subscript, [fr_bold()] and [fr_italic()] for
#'   font style, [fr_dagger()] for the † symbol.
#'
#' @export
fr_super <- function(x) new_fr_markup("SUPER", x)


#' Subscript Markup
#'
#' @description
#'
#' Renders content as subscript in the final output. Use inside
#' glue-string expressions in titles, footnotes, column labels, and cell
#' values. Common in clinical statistics for chemical formulas (H₂O),
#' statistical notation (beta-1), and indexed variables (x_i).
#'
#' @param x Character or numeric. Content to render as subscript.
#'   Coerced to character internally.
#'
#' @return An `fr_markup` object. When interpolated inside a glue string,
#'   produces a sentinel token resolved at render time.
#'
#' @section Rendering:
#' In RTF output, subscript uses `\sub` (half the font size, lowered).
#' In future LaTeX output, it maps to `\textsubscript{}`. The subscript
#' text renders at roughly half the surrounding font size.
#'
#' @examples
#' # Chemical formula: H₂O
#' fr_col("H{fr_sub(2)}O", width = 1.0)
#'
#' # Statistical subscript: x_i
#' fr_col("x{fr_sub('i')}", width = 1.0)
#'
#' # In a footnote:
#' spec <- tbl_demog |> fr_table()
#' spec |> fr_footnotes("{fr_sub('n')} = number of subjects with data")
#'
#' @seealso [fr_super()] for superscript, [fr_unicode()] for arbitrary
#'   Unicode characters.
#'
#' @export
fr_sub <- function(x) new_fr_markup("SUB", x)


#' Bold Markup
#'
#' @description
#'
#' Renders a text span as bold in the final output. Use inside
#' glue-string expressions to apply bold formatting to a **portion** of a
#' title, footnote, column label, or cell value — without bolding the
#' entire line.
#'
#' **When to use `fr_bold()` vs `bold = TRUE`:**
#' * `fr_bold("text")` — inline markup for partial bold within a string.
#' * `fr_row_style(bold = TRUE)` — bold the entire row uniformly.
#' * `fr_header(bold = TRUE)` — bold all column headers.
#'
#' @param x Character. Text to render in bold.
#'
#' @return An `fr_markup` object. When interpolated inside a glue string,
#'   produces a sentinel token resolved at render time.
#'
#' @section Rendering:
#' In RTF output, bold uses `\b` ... `\b0` control words. In future
#' LaTeX output, it maps to `\textbf{}`. The bold weight matches the
#' surrounding font family.
#'
#' @examples
#' # Bold a label keyword
#' fr_col("{fr_bold('Total')} Subjects", width = 1.5)
#'
#' # Bold just the table number in a title:
#' spec <- tbl_demog |> fr_table()
#' spec |> fr_titles("{fr_bold('Table 14.1.1')} Summary of Demographics")
#'
#' @seealso [fr_italic()] for italic, [fr_underline()] for underline,
#'   [fr_row_style()] for bolding entire rows, [fr_header()] for bolding
#'   all column headers.
#'
#' @export
fr_bold <- function(x) new_fr_markup("BOLD", x)


#' Italic Markup
#'
#' @description
#'
#' Renders a text span as italic in the final output. Use inside
#' glue-string expressions to italicize a portion of a title, footnote,
#' column label, or cell value.
#'
#' Common in regulatory tables for:
#' * **P-value annotations**: `"{fr_italic('P')}-value"` renders as *P*-value.
#' * **Statistical method notes**: Fisher's exact, Cochran-Mantel-Haenszel.
#' * **Latin abbreviations**: *vs.*, *et al.*, *in vitro*.
#'
#' @param x Character. Text to render in italic.
#'
#' @return An `fr_markup` object. When interpolated inside a glue string,
#'   produces a sentinel token resolved at render time.
#'
#' @section Rendering:
#' In RTF output, italic uses `\i` ... `\i0` control words. In future
#' LaTeX output, it maps to `\textit{}`.
#'
#' @examples
#' # Italic P-value annotation
#' fr_italic("P")
#'
#' # In footnotes:
#' spec <- tbl_demog |> fr_table()
#' spec |> fr_footnotes("[a] {fr_italic('P')}-value from Fisher's exact test.")
#' spec |> fr_footnotes("Comparison {fr_italic('vs.')} placebo.")
#'
#' @seealso [fr_bold()] for bold, [fr_underline()] for underline.
#'
#' @export
fr_italic <- function(x) new_fr_markup("ITALIC", x)


#' Underline Markup
#'
#' @description
#'
#' Renders a text span as underlined in the final output. Use inside
#' glue-string expressions. Underline is less common in regulatory tables
#' but is sometimes used for:
#' * **Confidentiality marks**: "CONFIDENTIAL" in title headers.
#' * **Hyperlink-style emphasis** in electronic submissions.
#'
#' For most emphasis needs, prefer [fr_bold()] — it is the standard in
#' pharma TFL outputs.
#'
#' @param x Character. Text to underline.
#'
#' @return An `fr_markup` object. When interpolated inside a glue string,
#'   produces a sentinel token resolved at render time.
#'
#' @section Rendering:
#' In RTF output, underline uses `\ul` ... `\ulnone` control words. In
#' future LaTeX output, it maps to `\underline{}`.
#'
#' @examples
#' # Confidentiality marker
#' fr_underline("CONFIDENTIAL")
#'
#' # In a title:
#' spec <- tbl_demog |> fr_table()
#' spec |> fr_titles("{fr_underline('CONFIDENTIAL')} - Do Not Distribute")
#'
#' @seealso [fr_bold()] for bold, [fr_italic()] for italic.
#'
#' @export
fr_underline <- function(x) new_fr_markup("UNDERLINE", x)


#' Unicode Character by Codepoint
#'
#' @description
#'
#' Inserts a Unicode character by its numeric codepoint. At render time
#' the backend converts to the appropriate escape sequence (RTF `\uN`
#' or LaTeX command). Use this for symbols not available on the keyboard
#' or for cross-platform portability.
#'
#' For the most commonly used symbols in clinical tables, prefer the
#' dedicated helpers: [fr_dagger()], [fr_ddagger()].
#'
#' @param codepoint Integer. Unicode codepoint in hex (e.g. `0x00B1`
#'   for ±) or decimal (e.g. `177L`). Must be a single value.
#'
#' @return An `fr_markup` object containing the UTF-8 character.
#'
#' @section Common codepoints for clinical tables:
#' | Symbol | Hex | Description |
#' |--------|-----|-------------|
#' | ± | `0x00B1` | Plus-minus (lab ranges) |
#' | ≤ | `0x2264` | Less-than-or-equal |
#' | ≥ | `0x2265` | Greater-than-or-equal |
#' | × | `0x00D7` | Multiplication (dosing) |
#' | ° | `0x00B0` | Degree (temperature) |
#' | α | `0x03B1` | Alpha (significance level) |
#' | β | `0x03B2` | Beta (type II error) |
#' | μ | `0x03BC` | Mu (mean) |
#' | † | `0x2020` | Dagger (use [fr_dagger()]) |
#' | ‡ | `0x2021` | Double dagger (use [fr_ddagger()]) |
#'
#' @examples
#' # Plus-minus symbol in a label
#' fr_col("Mean {fr_unicode(0x00B1)} SD", width = 1.5)
#'
#' # Degree symbol
#' fr_col("Temperature ({fr_unicode(0x00B0)}C)", width = 1.5)
#'
#' # In a footnote:
#' spec <- tbl_demog |> fr_table()
#' spec |> fr_footnotes("[a] P {fr_unicode(0x2264)} 0.05 considered significant.")
#'
#' @seealso [fr_dagger()] and [fr_ddagger()] for common symbol shortcuts,
#'   [fr_super()] for superscript notation.
#'
#' @export
fr_unicode <- function(codepoint) {
  if (!is.numeric(codepoint) || length(codepoint) != 1L) {
    cli_abort(
      c("{.arg codepoint} must be a single integer (e.g., {.code 0x00B1}).",
        "x" = "You supplied {.obj_type_friendly {codepoint}}."),
      call = caller_env()
    )
  }
  # Store as the actual UTF-8 character — backends handle conversion
  new_fr_markup("UNICODE", intToUtf8(as.integer(codepoint)))
}


#' Dagger Symbol
#'
#' @description
#'
#' Inserts the dagger symbol (U+2020). Shorthand for
#' `fr_unicode(0x2020)`.
#'
#' In regulatory tables the dagger is used to mark treatment-related
#' adverse events or to indicate a secondary footnote series when the
#' `[a]`/`[b]` series is exhausted.
#'
#' @return An `fr_markup` object containing the dagger character.
#'
#' @section Symbol conventions in pharma TLFs:
#' | Symbol | Markup | Common use |
#' |--------|--------|------------|
#' | `[a]`, `[b]` | superscript | Primary footnote markers |
#' | dagger | `fr_dagger()` | Treatment-related AE |
#' | double dagger | `fr_ddagger()` | Serious AE |
#' | asterisk | `*` | Significance indicator |
#'
#' @examples
#' # Standalone dagger symbol
#' fr_dagger()
#'
#' # In a footnote:
#' spec <- tbl_demog |> fr_table()
#' spec |> fr_footnotes("{fr_dagger()} Treatment-related adverse event.")
#'
#' # Combined dagger and double dagger:
#' spec |> fr_footnotes(
#'   "{fr_dagger()} Treatment-related adverse event.",
#'   "{fr_ddagger()} Serious adverse event."
#' )
#'
#' @seealso [fr_ddagger()] for the double dagger,
#'   [fr_unicode()] for arbitrary Unicode characters,
#'   [fr_super()] for superscript footnote markers.
#'
#' @export
fr_dagger <- function() fr_unicode(0x2020L)


#' Double Dagger Symbol
#'
#' @description
#'
#' Inserts the double dagger symbol (U+2021). Shorthand for
#' `fr_unicode(0x2021)`.
#'
#' In regulatory tables the double dagger marks **serious adverse events**
#' or acts as a secondary marker after the single dagger. See
#' [fr_dagger()] for the full symbol convention table.
#'
#' @return An `fr_markup` object containing the double dagger character.
#'
#' @examples
#' # Standalone double dagger symbol
#' fr_ddagger()
#'
#' # In footnotes:
#' spec <- tbl_demog |> fr_table()
#' spec |> fr_footnotes(
#'   "{fr_dagger()} Treatment-related adverse event.",
#'   "{fr_ddagger()} Serious adverse event."
#' )
#'
#' @seealso [fr_dagger()] for the single dagger and symbol conventions,
#'   [fr_unicode()] for arbitrary Unicode characters.
#'
#' @export
fr_ddagger <- function() fr_unicode(0x2021L)


#' Em Dash
#'
#' @description
#'
#' Inserts an em dash (U+2014, "—"). Shorthand for `fr_unicode(0x2014)`.
#'
#' The em dash is used in regulatory table titles and footnotes to
#' separate clauses, e.g. "Safety Population — All Randomized Subjects".
#' Using `fr_emdash()` instead of a literal "—" character guarantees
#' correct rendering in both RTF and PDF output regardless of file
#' encoding.
#'
#' @return An `fr_markup` object containing the em dash character.
#'
#' @examples
#' # In a title:
#' spec <- tbl_demog |> fr_table()
#' spec |> fr_titles("Safety Population {fr_emdash()} FAS")
#'
#' # In a footnote:
#' spec |> fr_footnotes("Source: ADSL {fr_emdash()} Safety Population")
#'
#' @seealso [fr_endash()] for the shorter en dash,
#'   [fr_unicode()] for arbitrary Unicode characters.
#'
#' @export
fr_emdash <- function() fr_unicode(0x2014L)


#' En Dash
#'
#' @description
#'
#' Inserts an en dash (U+2013, "–"). Shorthand for `fr_unicode(0x2013)`.
#'
#' The en dash is used in regulatory tables for numeric ranges
#' (e.g. "18–65 years"), date ranges, and confidence intervals.
#' Using `fr_endash()` instead of a literal "–" character guarantees
#' correct rendering in both RTF and PDF output.
#'
#' @return An `fr_markup` object containing the en dash character.
#'
#' @examples
#' # Numeric range in a footnote:
#' spec <- tbl_demog |> fr_table()
#' spec |> fr_footnotes("[a] Age range: 18{fr_endash()}65 years.")
#'
#' # In a column label:
#' fr_col("95% CI{fr_newline()}(Lower{fr_endash()}Upper)", width = 1.5)
#'
#' @seealso [fr_emdash()] for the longer em dash,
#'   [fr_unicode()] for arbitrary Unicode characters.
#'
#' @export
fr_endash <- function() fr_unicode(0x2013L)


#' Line Break Within a Text Element
#'
#' @description
#'
#' Inserts a line break within a single title line, footnote line,
#' or column label. Use this when you need a multi-line label that
#' is logically one element (e.g. a column header that wraps to two
#' lines).
#'
#' In most cases you can use a literal `"\\n"` in your string instead.
#' `fr_newline()` is provided for use inside glue expressions where a
#' literal newline would be awkward or ambiguous.
#'
#' @return An `fr_markup` object that resolves to a line break at
#'   render time.
#'
#' @section Rendering:
#' In RTF output, the newline resolves to `\\line` (a soft line break
#' within the same paragraph). In future LaTeX output, it maps to `\\\\`.
#' The layout engine accounts for newlines when calculating row height.
#'
#' @examples
#' # Multi-line column header via newline markup
#' fr_col("Treatment{fr_newline()}Arm", width = 1.5)
#'
#' # Equivalent using literal newline (simpler — preferred)
#' fr_col("Treatment\nArm", width = 1.5)
#'
#' @seealso [fr_col()] for column labels, [fr_titles()] for title lines.
#'
#' @export
fr_newline <- function() new_fr_markup("NEWLINE", "")


# ══════════════════════════════════════════════════════════════════════════════
# 5. Glue Evaluation Engine
#
# When a string contains {fr_*(...)} patterns, we evaluate it with
# glue::glue() so the markup helpers produce sentinel tokens inline.
# The result is a plain character string with embedded sentinels.
# ══════════════════════════════════════════════════════════════════════════════

#' Evaluate glue-string markup in a text value
#'
#' If `text` contains `{fr_` patterns, evaluate via glue::glue().
#' Otherwise return as-is (zero overhead for plain strings).
#'
#' @param text Character scalar.
#' @param env Environment for glue evaluation (default: caller's environment).
#' @return Character scalar with sentinel tokens embedded.
#' @noRd
eval_markup <- function(text, env = parent.frame()) {
  if (!is.character(text) || length(text) != 1L) return(text)
  if (!has_fr_markup(text)) return(text)
  as.character(glue(text, .envir = env, .open = "{", .close = "}"))
}


#' Evaluate markup across a character vector
#'
#' Applies eval_markup to each element. Used for titles, footnotes,
#' column labels, and cell data.
#'
#' @param texts Character vector.
#' @param env Environment for glue evaluation.
#' @return Character vector with sentinels.
#' @noRd
eval_markup_vec <- function(texts, env = parent.frame()) {
  vapply(texts, function(t) eval_markup(t, env = env), character(1),
         USE.NAMES = FALSE)
}


# ══════════════════════════════════════════════════════════════════════════════
# 6. Sentinel Resolution (called by backends at render time)
# ══════════════════════════════════════════════════════════════════════════════

#' Check if a string contains unresolved markup sentinels
#' @noRd
has_sentinel <- function(text) {
  grepl(fr_env$sentinel_start, text, fixed = TRUE)
}


#' Resolve sentinel tokens to backend-specific output
#'
#' @param text Character scalar containing sentinels.
#' @param resolver Function(type, content) → character. Backend-specific.
#' @return Character scalar with sentinels replaced.
#' @noRd
resolve_sentinels <- function(text, resolver) {
  if (!has_sentinel(text)) return(text)
  pattern <- fr_env$sentinel_pattern
  m <- gregexpr(pattern, text, perl = TRUE)
  tokens <- regmatches(text, m)[[1]]
  if (length(tokens) == 0L) return(text)

  for (tok in tokens) {
    parts <- regmatches(tok, regexec(pattern, tok, perl = TRUE))[[1]]
    type <- parts[[2]]
    content <- parts[[3]]
    replacement <- resolver(type, content)
    text <- sub(tok, replacement, text, fixed = TRUE)
  }
  text
}


#' Resolve sentinels across a character vector
#' @noRd
resolve_sentinels_vec <- function(texts, resolver) {
  vapply(texts, function(t) resolve_sentinels(t, resolver), character(1),
         USE.NAMES = FALSE)
}


# ══════════════════════════════════════════════════════════════════════════════
# 7. Plain Text Extraction (for row height estimation)
# ══════════════════════════════════════════════════════════════════════════════

#' Strip all markup sentinels, keeping only the content text
#'
#' Used by the layout engine to estimate row heights from plain text widths.
#'
#' @param text Character scalar possibly containing sentinels.
#' @return Character scalar with sentinels replaced by their content.
#' @noRd
sentinel_to_plain <- function(text) {
  if (!has_sentinel(text)) return(text)
  resolve_sentinels(text, function(type, content) {
    if (toupper(type) == "NEWLINE") return("\n")
    content
  })
}

#' Strip sentinels from a character vector
#' @noRd
sentinel_to_plain_vec <- function(texts) {
  vapply(texts, sentinel_to_plain, character(1), USE.NAMES = FALSE)
}


# ══════════════════════════════════════════════════════════════════════════════
