# The workspace icon system — the ONE home for every icon the R side
# renders. Moved out of utils_atoms.R so no module ever hardcodes SVG
# markup inline again:
#
#   * `.fa_names` / `.icon()`      — semantic name -> Font Awesome 6 glyph,
#                                    for generic chrome (plus/close/trash/...).
#   * `.TYPE_ICONS` / `.type_icon()` — per-GENERATOR house SVGs (summary/
#                                    crosstab/occurrence/...), CLAUDE.md #4.
#   * `.CHROME_GLYPHS` / `.glyph()`  — one-off house SVG glyphs that are
#                                    neither FA nor generator-keyed (the
#                                    inspector panel toggle pair, ...).
#
# House SVG style (design system #4): 16x16 viewBox, `currentColor`,
# 1.3 stroke, round caps/joins. `.svg_tag()` is the shared wrapper so the
# style lives in exactly one sprintf. The JS-side icons (srcjs/toolbar.js
# run/rtf glyphs, srcjs/bridge.js picker type chips) intentionally stay in
# the JS bundle — Preact/selectize render them client-side.

# Semantic name -> Font Awesome 6 icon id.
.fa_names <- c(
  plus = "plus",
  close = "xmark",
  pin = "thumbtack",
  pencil = "pen",
  grip = "grip-vertical",
  kebab = "ellipsis-vertical",
  search = "magnifying-glass",
  table = "table",
  figure = "chart-line",
  listing = "list",
  database = "database",
  import = "file-import",
  export = "download",
  code = "code",
  check = "check",
  warn = "triangle-exclamation",
  undo = "arrow-rotate-left",
  redo = "arrow-rotate-right",
  open = "folder-open",
  save = "floppy-disk",
  trash = "trash-can",
  copy = "copy",
  folder_plus = "folder-plus",
  package = "box-archive",
  arrow_right = "arrow-right",
  calendar = "calendar",
  eye = "eye",
  chevrons_left = "angles-left",
  chevrons_right = "angles-right",
  chevron_right = "chevron-right",
  play = "play",
  info = "circle-info",
  # Mode-nav glyphs (the top app bar).
  report = "file-lines",
  logs = "terminal",
  gear = "gear",
  review = "clipboard-check"
)

#' An inline workspace icon by semantic name (HTML to drop into a label/button).
#'
#' fontawesome's `fa()` defaults `margin-left`/`margin-right` to `auto`, which
#' shoves the icon around inside any full-width flex row (a TOC entry, a card
#' header); pin both to `0px`.
#' @noRd
.icon <- function(name, size = 16) {
  if (!name %in% names(.fa_names)) {
    .abort_app(
      c(
        "Unknown icon name {.val {name}}.",
        "i" = "See {.code .fa_names} for the registered set."
      ),
      call = rlang::caller_env()
    )
  }
  fontawesome::fa(
    unname(.fa_names[name]),
    height = paste0(size, "px"),
    fill = "currentColor",
    margin_left = "0px",
    margin_right = "0px"
  )
}

#' The shared house-style SVG wrapper: every inline SVG the R side renders
#' goes through this one sprintf, so the design-system constants (16x16
#' viewBox, currentColor, 1.3 stroke, round caps/joins, aria-hidden) are
#' written exactly once.
#' @param inner *The SVG body markup.* `<character(1)>`.
#' @param class *The svg element's class attribute.* `<character(1)>`.
#' @param size *Rendered width/height in px.* `<integer(1)>`.
#' @noRd
.svg_tag <- function(inner, class, size) {
  shiny::HTML(sprintf(
    '<svg class="%s" width="%d" height="%d" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.3" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">%s</svg>',
    class,
    size,
    size,
    inner
  ))
}

# Generator id -> inline SVG body (16x16 viewBox, stroke-only house style).
# Keyed by TYPE (the 7 `arpillar::generators()` ids: summary/crosstab/
# occurrence/listing/km/line/box), not by `kind` ("table"/"figure"/
# "listing") — every preset sharing a generator (e.g. every AE occurrence
# preset) reads the same glyph, so the icon signals output FAMILY while the
# row label still distinguishes the specific table/figure/listing.
.TYPE_ICONS <- list(
  summary = '<circle cx="3" cy="4.3" r="0.7"/><path d="M5.5 4.3 H13"/><circle cx="3" cy="8" r="0.7"/><path d="M5.5 8 H13"/><circle cx="3" cy="11.7" r="0.7"/><path d="M5.5 11.7 H10.5"/>',
  crosstab = '<rect x="2" y="2.5" width="12" height="11" rx="1.4"/><path d="M6.3 2.5 V13.5 M10.1 2.5 V13.5 M2 6.2 H14 M2 9.8 H14"/>',
  occurrence = '<path d="M2.6 3.2 H9"/><path d="M3.8 3.2 V11.3"/><path d="M3.8 7.25 H5.4"/><path d="M6.6 7.25 H13.4"/><path d="M3.8 11.3 H5.4"/><path d="M6.6 11.3 H13.4"/>',
  # A page of raw rows: document outline + per-row rules, the appendix-16.2
  # data-listing motif (distinct from summary's stat bullets).
  listing = '<rect x="3" y="2" width="10" height="12" rx="1.4"/><path d="M5.2 5 H10.8 M5.2 7.4 H10.8 M5.2 9.8 H10.8 M5.2 12.2 H8.6"/>',
  km = '<path d="M2.4 2 V13.6 H14"/><path d="M3 3.6 H6 V7 H9.2 V10.3 H12.8"/>',
  line = '<path d="M2.4 2 V13.6 H14"/><path d="M3.4 11.2 L6.6 6.6 L9.6 9 L13 4.4"/><circle cx="6.6" cy="6.6" r="0.5" fill="currentColor" stroke="none"/><circle cx="13" cy="4.4" r="0.5" fill="currentColor" stroke="none"/>',
  box = '<rect x="5.4" y="5" width="5.2" height="6" rx="0.8"/><path d="M5.4 8 H10.6"/><path d="M8 5 V2.6 M6.6 2.6 H9.4"/><path d="M8 11 V13.4 M6.6 13.4 H9.4"/>'
)

#' An inline per-generator-type SVG icon (HTML), keyed by the render TYPE
#' (`"summary"`/`"crosstab"`/`"occurrence"`/`"listing"`/`"km"`/`"line"`/
#' `"box"`) rather than `kind` ("table"/"figure"/"listing") — falls back to
#' the `summary` glyph for an unrecognized type so a generator not yet wired
#' to a glyph still renders something instead of erroring.
#' @noRd
.type_icon <- function(type, size = 16L) {
  inner <- .TYPE_ICONS[[type]] %||% .TYPE_ICONS[["summary"]]
  .svg_tag(inner, class = "ar-type-icon", size = size)
}

# Glyph name -> inline SVG body, for house glyphs that are neither Font
# Awesome nor generator-keyed. Each entry documents where it renders and
# what state it signals. The rendered class is `ar-glyph ar-glyph-<name>`
# (underscores -> dashes), which is what any state-flipping CSS keys on.
.CHROME_GLYPHS <- list(
  # The inspector panel toggle pair (mod_toolbar.R): a sidebar rectangle
  # whose right column is FILLED while the inspector is open (click =
  # close) and outline-only while collapsed (click = open). CSS keyed off
  # `.ar-insp-collapsed` shows one at a time (arframe.css, 07a).
  panel_close = paste0(
    '<rect x="1.5" y="2.5" width="13" height="11" rx="2"/>',
    '<path d="M10.5 2.5 H12.5 A2 2 0 0 1 14.5 4.5 V11.5 A2 2 0 0 1 12.5 13.5 H10.5 Z" fill="currentColor" stroke="none"/>'
  ),
  panel_open = paste0(
    '<rect x="1.5" y="2.5" width="13" height="11" rx="2"/>',
    '<path d="M10.5 2.5 V13.5"/>'
  )
)

#' An inline chrome glyph (HTML) by `.CHROME_GLYPHS` name. Unknown names
#' error loudly — a chrome glyph is always author-chosen, so a miss is a
#' typo, never data.
#' @noRd
.glyph <- function(name, size = 16L) {
  inner <- .CHROME_GLYPHS[[name]]
  if (is.null(inner)) {
    .abort_app(
      c(
        "Unknown chrome glyph {.val {name}}.",
        "i" = "See {.code .CHROME_GLYPHS} for the registered set."
      ),
      call = rlang::caller_env()
    )
  }
  .svg_tag(
    inner,
    class = paste0("ar-glyph ar-glyph-", gsub("_", "-", name, fixed = TRUE)),
    size = size
  )
}
