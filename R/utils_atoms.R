# The Galley chrome atoms: the workspace icon set (Font Awesome 6 via
# fontawesome::fa), the uppercase micro-label, the letterpress status stamp,
# the variable type chip, and the btn-default-free Shiny action button. See
# docs/superpowers/specs/2026-07-02-arframe-galley-design-system.md #2, #5.

#' Abort with the app's single input-error class.
#'
#' Every app-side error in arframe is a classed `arframe_error_input`
#' condition (via `cli::cli_abort`), so callers can `tryCatch()` on one class
#' regardless of which atom or module raised it.
#' @param msg *The cli message vector.* `<character>: required`. Passed
#'   through to `cli::cli_abort()` unchanged (headline + `x`/`i` bullets).
#' @param ... Forwarded to `cli::cli_abort()` (e.g. `call =`).
#' @param .envir *Where to evaluate the message's glue `{}` expressions.*
#'   `<environment>: default parent.frame()`. `cli_abort()` otherwise
#'   evaluates glue expressions against THIS function's frame, not the
#'   caller's, so a `{.val {x}}` referencing the caller's local `x` would not
#'   resolve; default to one frame up so every call site's variables are
#'   visible without repeating this at each call site.
#' @noRd
.abort_app <- function(msg, ..., .envir = parent.frame()) {
  cli::cli_abort(msg, ..., class = "arframe_error_input", .envir = .envir)
}

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
  play = "play",
  info = "circle-info",
  # Activity-bar glyphs (the far-left mode rail, mockup piece A).
  report = "file-lines",
  logs = "terminal",
  gear = "gear",
  # Inspector tab glyphs (the far-right icon strip).
  roles = "table-list",
  options = "sliders",
  filters = "filter",
  ranks = "arrow-down-wide-short"
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

#' A small info icon plus its detail rendered inline. The icon is chrome
#' -- it signals "info about the selection" -- and the detail sits right
#' next to it so users never have to hover-and-wait for a tooltip. The
#' native `title` still carries the same text for screen-reader parity.
#' @noRd
.info_icon <- function(detail) {
  headline <- strsplit(detail, "\n", fixed = TRUE)[[1]][1]
  shiny::tags$span(
    class = "ar-info-icon",
    tabindex = "0",
    title = detail,
    `aria-label` = detail,
    .icon("info", 12),
    shiny::tags$span(class = "ar-info-text", headline)
  )
}

#' A small uppercase tracked micro-label (the chrome signature).
#' @noRd
.label <- function(text) {
  shiny::div(class = "ar-label", text)
}

#' A data-item type chip: measure (# / violet), date (calendar / amber), or
#' category (A / blue).
#' @param role_type *One of `"measure"`, `"date"`, `"category"`.*
#'   `<character(1)>: required`.
#' @noRd
.type_chip <- function(role_type) {
  if (identical(role_type, "measure")) {
    return(shiny::span(class = "ar-chip ar-chip-meas", "#"))
  }
  if (identical(role_type, "date")) {
    return(shiny::span(class = "ar-chip ar-chip-date", .icon("calendar", 11)))
  }
  if (identical(role_type, "category")) {
    return(shiny::span(class = "ar-chip ar-chip-cat", "A"))
  }
  .abort_app(
    c(
      "Unknown {.arg role_type} {.val {role_type}}.",
      "i" = "Use one of {.val measure}, {.val date}, {.val category}."
    ),
    call = rlang::caller_env()
  )
}

# status -> c(word, css modifier suffix, aria-label sentence). The word/class
# pair mirrors arpillar::output_status()'s three states 1:1 ("ready", "draft",
# "needs_data"); "broken" (render failed) and "stale" (heavy edit awaiting
# Run, decision #8) are app-side flags layered on top, not part of the
# engine oracle.
.stamp_specs <- list(
  ready = c("READY", "ready", "Ready to render."),
  draft = c("DRAFT", "draft", "Draft: some inputs are still unmet."),
  needs_data = c("NO DATA", "needs_data", "No dataset bound."),
  broken = c("ERROR", "broken", "Render failed."),
  stale = c("STALE", "stale", "Proof is stale: run to re-typeset.")
)

#' A letterpress status stamp: mono caps, colored 1px border, transparent fill.
#'
#' Maps the oracle's status vocabulary to the five Galley stamps. Colour never
#' carries the signal alone -- the word is always present, and `aria-label`
#' repeats it as a full sentence for screen readers.
#' @param status *One of `"ready"`, `"draft"`, `"needs_data"`, `"broken"`,
#'   `"stale"`.* `<character(1)>: required`.
#' @noRd
.stamp <- function(status) {
  spec <- .stamp_specs[[status]]
  if (is.null(spec)) {
    .abort_app(
      c(
        "Unknown stamp status {.val {status}}.",
        "i" = "Use one of {.val ready}, {.val draft}, {.val needs_data}, {.val broken}, {.val stale}."
      ),
      call = rlang::caller_env()
    )
  }
  shiny::span(
    class = paste0("ar-stamp ar-stamp-", spec[[2]], " ar-mono"),
    `aria-label` = spec[[3]],
    spec[[1]]
  )
}

# Generator id -> inline SVG body (16x16 viewBox, stroke-only house style).
# Keyed by TYPE (the 6 `arpillar::generators()` ids: summary/crosstab/
# occurrence/km/line/box), not by `kind` ("table"/"figure") -- every preset
# sharing a generator (e.g. every AE occurrence preset) reads the same
# glyph, so the icon signals output FAMILY while the row label still
# distinguishes the specific table/figure.
.TYPE_ICONS <- list(
  summary = '<circle cx="3" cy="4.3" r="0.7"/><path d="M5.5 4.3 H13"/><circle cx="3" cy="8" r="0.7"/><path d="M5.5 8 H13"/><circle cx="3" cy="11.7" r="0.7"/><path d="M5.5 11.7 H10.5"/>',
  crosstab = '<rect x="2" y="2.5" width="12" height="11" rx="1.4"/><path d="M6.3 2.5 V13.5 M10.1 2.5 V13.5 M2 6.2 H14 M2 9.8 H14"/>',
  occurrence = '<path d="M2.6 3.2 H9"/><path d="M3.8 3.2 V11.3"/><path d="M3.8 7.25 H5.4"/><path d="M6.6 7.25 H13.4"/><path d="M3.8 11.3 H5.4"/><path d="M6.6 11.3 H13.4"/>',
  km = '<path d="M2.4 2 V13.6 H14"/><path d="M3 3.6 H6 V7 H9.2 V10.3 H12.8"/>',
  line = '<path d="M2.4 2 V13.6 H14"/><path d="M3.4 11.2 L6.6 6.6 L9.6 9 L13 4.4"/><circle cx="6.6" cy="6.6" r="0.5" fill="currentColor" stroke="none"/><circle cx="13" cy="4.4" r="0.5" fill="currentColor" stroke="none"/>',
  box = '<rect x="5.4" y="5" width="5.2" height="6" rx="0.8"/><path d="M5.4 8 H10.6"/><path d="M8 5 V2.6 M6.6 2.6 H9.4"/><path d="M8 11 V13.4 M6.6 13.4 H9.4"/>'
)

#' An inline per-generator-type SVG icon (HTML), keyed by the render TYPE
#' (`"summary"`/`"crosstab"`/`"occurrence"`/`"km"`/`"line"`/`"box"`) rather
#' than `kind` ("table"/"figure") -- falls back to the `summary` glyph for
#' an unrecognized type so a generator not yet wired to a glyph still
#' renders something instead of erroring.
#' @noRd
.type_icon <- function(type, size = 16L) {
  inner <- .TYPE_ICONS[[type]] %||% .TYPE_ICONS[["summary"]]
  shiny::HTML(sprintf(
    '<svg class="ar-type-icon" width="%d" height="%d" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.3" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">%s</svg>',
    size,
    size,
    inner
  ))
}

#' A Shiny action button that uses Bootstrap variant classes (`btn-primary`,
#' `btn-outline-secondary`, `btn-danger`, ...) WITHOUT shiny's `btn-default` --
#' so the variant owns the colour and there is no dark default hover/active
#' fill to fight. Shiny binds any `.action-button` by its id, so this behaves
#' exactly like `shiny::actionButton()`.
#' @noRd
.action_btn <- function(
  id,
  label,
  variant = "outline-secondary",
  class = NULL,
  style = NULL,
  disabled = FALSE
) {
  btn <- shiny::tags$button(
    id = id,
    type = "button",
    class = paste("btn", paste0("btn-", variant), "action-button", class),
    style = style,
    label
  )
  if (disabled) {
    btn <- shiny::tagAppendAttributes(btn, disabled = "disabled")
  }
  btn
}

#' A filesystem-safe slug for an output's download filename:
#' `t-14-1-1-demographics.rtf` -- kind letter + number + title, lowercased,
#' non-alnum runs collapsed to `-`. Shared by the canvas toolbar's .rtf
#' download (mod_toolbar.R) and the paper's code bar (mod_paper.R).
#' @noRd
.output_slug <- function(object) {
  label <- object@options$number_label %||% "Table"
  kind <- tolower(substr(label, 1, 1))
  raw <- paste(kind, object@options$number %||% "", object@title)
  slug <- tolower(gsub("[^a-zA-Z0-9]+", "-", trimws(raw)))
  gsub("^-+|-+$", "", slug)
}
