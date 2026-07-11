# The Galley chrome atoms: the workspace icon set (Font Awesome 6 via
# fontawesome::fa), the uppercase micro-label, the status pill, the variable
# type chip, the btn-default-free Shiny action button, and the dashboard
# surfaces (card / stat tile / presence avatar). See
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

# The icon system (`.fa_names`/`.icon`, `.TYPE_ICONS`/`.type_icon`,
# `.CHROME_GLYPHS`/`.glyph`) lives in utils_icons.R.

#' A small info icon plus its detail rendered inline. The icon is chrome
#' — it signals "info about the selection" — and the detail sits right
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

#' A status pill: a mono-caps word with a leading status dot on a soft-fill
#' capsule (the dot + fill are CSS — `.ar-stamp::before` / `.ar-stamp-*`).
#'
#' Maps the oracle's status vocabulary to the five Galley states. Colour never
#' carries the signal alone — the word is always present, and `aria-label`
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

#' A Shiny action button that uses Bootstrap variant classes (`btn-primary`,
#' `btn-outline-secondary`, `btn-danger`, ...) WITHOUT shiny's `btn-default` —
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

#' A "confirm delete" modal (the non-blocking Shiny-native confirm the user
#' asked for — NOT a `window.confirm()`). A Cancel button dismisses; the
#' danger "Delete" posts to `confirm_id`, which the caller wires to the actual
#' removal. Shared by Data (datasets) and the Report LoC (outputs).
#' @param confirm_id *The namespaced id of the confirm button.*
#'   `<character(1)>`. From `session$ns("...")`.
#' @param n *How many items will be deleted.* `<integer(1)>`.
#' @param noun *Singular noun for the item ("dataset" / "output").*
#'   `<character(1)>`.
#' @param detail *One sentence on what delete does + how to undo.*
#'   `<character(1)>`.
#' @noRd
.confirm_delete_modal <- function(confirm_id, n, noun, detail) {
  plural <- if (n == 1L) noun else paste0(noun, "s")
  m <- shiny::modalDialog(
    title = sprintf("Delete %d %s?", n, plural),
    shiny::p(detail),
    size = "s",
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      .action_btn(confirm_id, "Delete", variant = "danger", class = "ex-btn-sm")
    )
  )
  # `ar-confirm-modal` scopes the Galley modal skin (accent title, rounded
  # borderless card) so it never touches datasetviewer's own modals.
  shiny::tagAppendAttributes(m, class = "ar-confirm-modal")
}

#' A dashboard surface card: white, elevated, rounded, padded. Optional
#' header row (a title plus a right-aligned action slot) above the body.
#'
#' The generic container of the dashboard redesign. NB the inspector root
#' already owns `.ar-card`, so this uses `.ar-panel`.
#' @param ... *Body content* (tags).
#' @param title *Card heading.* `<character(1)> | NULL`. `NULL` -> no header.
#' @param action *Right-aligned header slot (a button/link tag) `| NULL`.*
#' @param class *Extra classes on the panel root.* `<character> | NULL`.
#' @noRd
.card <- function(..., title = NULL, action = NULL, class = NULL) {
  header <- if (!is.null(title) || !is.null(action)) {
    shiny::div(
      class = "ar-panel-head",
      shiny::div(class = "ar-panel-title", title),
      action
    )
  }
  shiny::div(
    class = paste("ar-panel", class),
    header,
    shiny::div(class = "ar-panel-body", ...)
  )
}

#' A stat tile: a big mono value with a small label, an optional signed
#' delta, and an optional leading icon.
#'
#' The value renders in the mono "instrument" face — stat tiles carry
#' machine facts (subject/record counts, dataset dims), never prose.
#' @param value *The headline figure.* `<character(1)>`.
#' @param label *What it measures.* `<character(1)>`.
#' @param delta *Signed-change chip (e.g. `"+3 today"`), `| NULL`.*
#' @param trend *One of `"up"`/`"down"`/`"flat"`; colours the delta.*
#'   `<character(1)> default "flat"`.
#' @param icon *Leading icon name (see `.fa_names`), `| NULL`.*
#' @noRd
.stat_tile <- function(
  value,
  label,
  delta = NULL,
  trend = "flat",
  icon = NULL
) {
  shiny::div(
    class = "ar-stat-tile",
    if (!is.null(icon)) {
      shiny::span(class = "ar-stat-tile-icon", .icon(icon, 18))
    },
    shiny::div(
      class = "ar-stat-tile-main",
      shiny::div(class = "ar-stat-tile-value", value),
      shiny::div(class = "ar-stat-tile-label", label)
    ),
    if (!is.null(delta)) {
      shiny::span(class = paste0("ar-stat-tile-delta ar-trend-", trend), delta)
    }
  )
}

#' A team presence avatar: initials on a coloured disc, ringed when the
#' member is live (within the heartbeat window).
#'
#' Initials-only (no image dependency); colour + initials come from the
#' roster (`fct_team.R`). Colour is passed to CSS via the `--ar-avatar-bg`
#' custom property so the class carries all other styling.
#' @param initials *One or two characters.* `<character(1)>`.
#' @param colour *Disc background colour.* `<character(1)> | NULL`.
#' @param name *Full name for tooltip / aria-label.* `<character(1)> | NULL`.
#' @param live *Ring it (member active).* `<logical(1)> default FALSE`.
#' @noRd
.avatar <- function(initials, colour = NULL, name = NULL, live = FALSE) {
  shiny::span(
    class = paste("ar-avatar", if (isTRUE(live)) "ar-avatar-live"),
    style = if (!is.null(colour)) paste0("--ar-avatar-bg:", colour, ";"),
    title = name,
    `aria-label` = name %||% initials,
    initials
  )
}

#' The shared variable/param picker: a `selectizeInput` whose per-option
#' render (type-chip + NAME + muted CDISC label) is defined ONCE in the JS
#' bundle (srcjs/bridge.js `window.arframePickerOption` / `arframePickerItem`)
#' and referenced here by name — no render markup lives in the R modules.
#' Each choice's LABEL is the packed `"name\x1ftype\x1flabel"` string the
#' render splits; the VALUE is whatever the server consumes.
#'
#' @param choices *Named-vector choices.* `<character>`, names = packed labels,
#'   values = what the server reads (packed, clean id, or bare column name).
#' @param selected *Pre-selected value(s).* `<character>`. When empty, the
#'   picker force-clears on init (no selectize auto-pick of the first option).
#' @param onchange *A selectize `onChange` JS statement* (add-row pickers post
#'   `{i, value, nonce}` to a shared observer). `<character(1)> | NULL`.
#' @param class *Extra wrapper class* beside the base `ar-picker`.
#' @noRd
.PICKER_RENDER <-
  "{ option: window.arframePickerOption, item: window.arframePickerItem }"

.ar_picker_select <- function(
  ns,
  input_id,
  choices,
  selected = character(0),
  placeholder = "",
  onchange = NULL,
  class = NULL
) {
  opts <- list(
    placeholder = placeholder,
    render = I(.PICKER_RENDER),
    searchField = list("label")
  )
  if (!is.null(onchange)) {
    opts$onChange <- I(onchange)
  }
  if (length(selected) == 0L) {
    # selectize otherwise auto-selects the first option; force empty.
    opts$onInitialize <- I("function() { this.setValue(''); }")
  }
  shiny::div(
    class = paste(c("ar-picker", class), collapse = " "),
    shiny::selectizeInput(
      ns(input_id),
      label = NULL,
      choices = choices,
      selected = selected,
      options = opts
    )
  )
}

#' A fold/unfold inspector-pane section: a native `<details>`/`<summary>`
#' accordion, no JS library. Mirrors `.opt_section()`'s elision (NULL/empty
#' body collapses to NULL) so it drops into the same call sites.
#'
#' The summary row packs an optional soft-tinted icon chip, the uppercase
#' `.ar-label`, an optional muted count chip, a flex spacer, an optional
#' pre-built help tag (e.g. Task 10's `.help_icon()`), and a trailing chevron
#' that rotates on `[open]` (CSS).
#' @param label *Section heading.* `<character(1)>: required`.
#' @param body *Section content.* A tag, or a list of tags/`NULL` (elided).
#' @param icon *Leading chip icon name (see `.fa_names`), `| NULL`.*
#' @param open *Start expanded.* `<logical(1)> default TRUE`.
#' @param help *A pre-built help tag slotted before the chevron, `| NULL`.*
#' @param count *A muted mono count/summary chip, `| NULL`.*
#' @noRd
.accordion_section <- function(
  label,
  body,
  icon = NULL,
  open = TRUE,
  help = NULL,
  count = NULL
) {
  # A bare shiny.tag IS a list (name/attribs/children), so the is.list()
  # branch below would strip its class and splice those three parts in as
  # loose body elements (rendering as literal text, not markup) — wrap it
  # first. A tagList is class shiny.tag.list and stays on the list path.
  if (inherits(body, "shiny.tag")) {
    body <- list(body)
  }
  body <- Filter(Negate(is.null), if (is.list(body)) body else list(body))
  if (length(body) == 0L) {
    return(NULL)
  }
  args <- list(
    class = "ar-acc",
    shiny::tags$summary(
      class = "ar-acc-head",
      if (!is.null(icon)) {
        shiny::tags$span(class = "ar-acc-chip", .icon(icon, 13))
      },
      shiny::tags$span(class = "ar-label ar-acc-label", label),
      if (!is.null(count)) {
        shiny::tags$span(class = "ar-acc-count ar-mono", count)
      },
      shiny::tags$span(class = "ar-bar-spacer"),
      help,
      .icon("chevron_right", 12)
    ),
    shiny::tags$div(class = "ar-acc-body", body)
  )
  if (isTRUE(open)) {
    args$open <- NA
  }
  do.call(shiny::tags$details, args)
}
