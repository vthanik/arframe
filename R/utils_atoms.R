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
  arrow_right = "arrow-right",
  calendar = "calendar",
  eye = "eye"
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
# "needs_data"); "broken" is an app-side render-failed flag layered on top,
# not part of the engine oracle.
.stamp_specs <- list(
  ready = c("READY", "ready", "Ready to render."),
  draft = c("DRAFT", "draft", "Draft: some inputs are still unmet."),
  needs_data = c("NO DATA", "needs_data", "No dataset bound."),
  broken = c("ERROR", "broken", "Render failed.")
)

#' A letterpress status stamp: mono caps, colored 1px border, transparent fill.
#'
#' Maps the oracle's status vocabulary to the four Galley stamps. Colour never
#' carries the signal alone -- the word is always present, and `aria-label`
#' repeats it as a full sentence for screen readers.
#' @param status *One of `"ready"`, `"draft"`, `"needs_data"`, `"broken"`.*
#'   `<character(1)>: required`.
#' @noRd
.stamp <- function(status) {
  spec <- .stamp_specs[[status]]
  if (is.null(spec)) {
    .abort_app(
      c(
        "Unknown stamp status {.val {status}}.",
        "i" = "Use one of {.val ready}, {.val draft}, {.val needs_data}, {.val broken}."
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
