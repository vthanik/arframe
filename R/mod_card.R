# The galley card (design spec #4/#8): the summonable, pinnable inspector
# that opens routed to a paper region -- `columns`/`rows`/`axes` show the
# Roles editor (mod_card_roles.R, this task); `title`/`footnotes`/`series`/
# `legend` and `filters` show a "coming" stub until Tasks 11/12 fill them
# in. The frame owns summon/pin/close chrome only; it never itself decides
# what a region means -- that is `.CARD_REGION_GROUP` below, one row per
# region token the paper/ghost/error-summary surfaces already emit (see
# utils_ghost.R's `.GHOST_REGION_MAP` and design spec #4's region table).

# ---- region routing -----------------------------------------------------

#' Region token -> which card CONTENT group renders it: `"roles"` (this
#' task), `"options"` (Task 11: title/footnotes/stat+figure options), or
#' `"filters"` (Task 12). A region with no row here still falls back to
#' `"options"` in `.card_region_group()` -- the frame always shows SOME
#' content rather than a blank pane for a future region token.
#' @noRd
.CARD_REGION_GROUP <- c(
  columns = "roles",
  rows = "roles",
  axes = "roles",
  title = "options",
  footnotes = "options",
  series = "options",
  legend = "options",
  filters = "filters"
)

#' The content group for `region`, defaulting to `"options"` for an
#' unmapped/`NULL` token.
#' @noRd
.card_region_group <- function(region) {
  if (is.null(region)) {
    return("options")
  }
  hit <- unname(.CARD_REGION_GROUP[region])
  if (is.na(hit)) "options" else hit
}

# The region micro-label shown in the card header -- the same word the
# region token names, uppercased for the `.ar-label` treatment.
.REGION_LABELS <- c(
  columns = "COLUMNS",
  rows = "ROWS",
  axes = "AXES",
  title = "TITLE",
  footnotes = "FOOTNOTES",
  series = "SERIES",
  legend = "LEGEND",
  filters = "FILTERS"
)

#' The card header's region label, falling back to the raw (uppercased)
#' token for a region this map has not seen.
#' @noRd
.region_label <- function(region) {
  if (is.null(region)) {
    return("")
  }
  hit <- unname(.REGION_LABELS[region])
  if (is.na(hit)) toupper(region) else hit
}

# ---- header ---------------------------------------------------------------

#' The card header: region micro-label, pin toggle (`aria-pressed`, no
#' visible label change -- the icon + pressed state carry it), close.
#' `pinned` is read fresh on every header render (server-rendered, see
#' `mod_card_server()`) so the pin icon's pressed state never drifts from
#' `rv$pinned`.
#' @noRd
.card_header <- function(ns, region, pinned) {
  shiny::tags$div(
    class = "ar-card-header",
    shiny::tags$div(class = "ar-label ar-card-region-label", .region_label(region)),
    shiny::tags$div(class = "ar-card-header-spacer"),
    shiny::tags$button(
      id = ns("pin"),
      type = "button",
      class = paste(
        "ar-icon-btn ar-card-pin action-button",
        if (isTRUE(pinned)) "ar-card-pin-active" else NULL
      ),
      `aria-pressed` = if (isTRUE(pinned)) "true" else "false",
      `aria-label` = "Pin card open",
      .icon("pin", 13)
    ),
    .action_btn(
      ns("close"),
      .icon("close", 13),
      variant = "link",
      class = "ar-icon-btn"
    )
  )
}

# ---- coming stub ------------------------------------------------------

#' A quiet "coming" stub for a region group not yet implemented (Options:
#' Task 11, Filters: Task 12) -- shows the region label plus a one-line
#' note so the card never renders visibly empty for a region a click
#' legitimately routed to.
#' @noRd
.card_coming_stub <- function(region, note) {
  shiny::tags$div(
    class = "ar-card-coming",
    shiny::tags$p(class = "ar-mono", note)
  )
}

# ---- UI ---------------------------------------------------------------

#' The galley card UI: the `--ar-card` frame (header + a body slot hosting
#' every region-content module, class-flipped by `rv$region`) plus the two
#' not-yet-built region stubs (`options`/`filters`). `mod_card_roles_ui()`
#' is mounted unconditionally -- CSS visibility, not conditional mounting,
#' picks which content shows (matching `mod_frame_ui()`'s "all bodies
#' mount, CSS picks one" pattern), so a role edit made, card closed, then
#' reopened on a DIFFERENT region never loses its own state.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_card_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    id = ns("card"),
    class = "ar-card",
    shiny::uiOutput(ns("card_slot"))
  )
}

# ---- server -------------------------------------------------------------

#' The galley card server: renders the frame (or nothing, when
#' `rv$card` is `FALSE`) off `rv$card`/`rv$region`/`rv$pinned`, mounts
#' `mod_card_roles_server()` once, and wires close/pin. The floating vs
#' pinned CLASS flip is a `session$sendCustomMessage()`, not a `renderUI`
#' dependency on `rv$pinned` alone -- re-rendering the whole card body on a
#' pin toggle would remount (and so reset any in-progress picker state of)
#' the roles editor for no reason; only the frame's outer CSS class needs
#' to change.
#' @param id *The module namespace, matching `mod_card_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_card_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_card_roles_server("roles", store)

    # The whole card body: header + the one content group the CURRENT
    # region maps to. Gated on the three pointers a summon/route/pin cycle
    # actually touches -- `rv$report`/`rv$selected` are NOT read here (the
    # roles submodule reads those itself for its own content), so a report
    # edit never remounts this frame's header/chrome.
    output$card_slot <- shiny::renderUI({
      if (!isTRUE(store$rv$card)) {
        return(NULL)
      }
      region <- store$rv$region
      group <- .card_region_group(region)
      shiny::tagList(
        .card_header(ns, region, store$rv$pinned),
        shiny::div(
          class = "ar-card-body",
          # `mod_card_roles_ui()` mounts unconditionally; `display: none`
          # (via the `ar-card-group-hidden` class, not React-style
          # conditional mounting) hides it when a non-roles region is
          # active, so its own internal state (an open picker, say) is
          # preserved across a region switch back and forth.
          shiny::tagAppendAttributes(
            mod_card_roles_ui(ns("roles")),
            class = if (!identical(group, "roles")) "ar-card-group-hidden"
          ),
          if (identical(group, "options")) {
            .card_coming_stub(
              region,
              "Title, footnotes, and display options arrive next."
            )
          },
          if (identical(group, "filters")) {
            .card_coming_stub(
              region,
              "Population and subset filters arrive next."
            )
          }
        )
      )
    }) |>
      shiny::bindEvent(store$rv$card, store$rv$region, store$rv$pinned)

    shiny::observeEvent(input$close, {
      close_card(store)
    })

    shiny::observeEvent(input$pin, {
      toggle_pin(store)
    })

    # The floating/pinned class flip: `.ar-pinned` on the card root. Sent
    # on every `rv$pinned` change (including the one this module's own
    # `toggle_pin()` just made) so a future OTHER pin trigger (e.g. a
    # keyboard shortcut, Task 17) stays correct without this observer
    # needing to know who changed it.
    shiny::observe({
      session$sendCustomMessage(
        "ar-card-pin",
        list(id = ns("card"), pinned = isTRUE(store$rv$pinned))
      )
    }) |>
      shiny::bindEvent(store$rv$pinned)

    invisible(NULL)
  })
}
