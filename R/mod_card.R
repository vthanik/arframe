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

# ---- inspector tabs (2026-07-10, pill-strip consolidation) -----------------

#' The three inspector tabs, in display order. Ranks left this map --
#' `mod_card_ranks.R`'s content was relocated into the Options pane's
#' ORDER section and the file deleted; this frame no longer mounts or
#' references it.
#' @noRd
.INSP_TABS <- c(
  roles = "Roles",
  options = "Options",
  filters = "Filters"
)

#' One inspector tab pill (text label, no icon). The ACTIVE tab is styled
#' by a pure CSS rule keyed off the `ar-insp-tab-*` class on the card root
#' (set by the "ar-insp-tab" message), mirroring the frame's mode-button
#' pattern -- a tab switch never re-renders the panel body.
#' @noRd
.insp_tab_btn <- function(ns, tab) {
  shiny::tags$button(
    id = ns(paste0("tab_", tab)),
    type = "button",
    class = "ar-insp-tab action-button",
    `data-ar-insp-tab` = tab,
    .INSP_TABS[[tab]]
  )
}

# ---- UI ---------------------------------------------------------------

#' The docked inspector UI: a fixed-width right panel -- a horizontal
#' segmented pill strip (Roles/Options/Filters) at the TOP of the panel,
#' then the pane stack (every pane mounts once, the `ar-insp-tab-*` class
#' on the card root picks which shows -- matching `mod_frame_ui()`'s
#' pattern, so a role edit made on one tab survives any amount of tab
#' switching), then the telemetry line. When the workspace carries
#' `ar-insp-collapsed` (frame-owned, see `toggle_insp()`) the whole card
#' folds -- the re-open affordance is the toolbar's `panel_toggle` button
#' (mod_toolbar.R), not a persistent rail. The action footer moved to the
#' canvas toolbar (mod_toolbar.R, 2026-07-04).
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_card_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    id = ns("card"),
    class = "ar-card ar-insp-tab-roles",
    # A drag handle on the rail's left edge -- arframe.js resizes the rail
    # width as it is dragged (client-side; the width persists for the session).
    shiny::tags$div(
      class = "ar-insp-resize",
      `data-ar-resize` = "insp",
      `aria-hidden` = "true"
    ),
    shiny::div(
      class = "ar-insp-full",
      shiny::div(
        class = "ar-insp-main",
        shiny::div(
          class = "ar-insp-strip",
          lapply(names(.INSP_TABS), function(tab) .insp_tab_btn(ns, tab))
        ),
        shiny::div(
          class = "ar-insp-body",
          shiny::div(
            class = "ar-insp-pane ar-insp-pane-roles",
            mod_card_roles_ui(ns("roles"))
          ),
          shiny::div(
            class = "ar-insp-pane ar-insp-pane-options",
            mod_card_options_ui(ns("options"))
          ),
          shiny::div(
            class = "ar-insp-pane ar-insp-pane-filters",
            mod_card_filters_ui(ns("filters"))
          )
        ),
        shiny::uiOutput(ns("telemetry"), class = "ar-insp-tel ar-mono")
      )
    )
  )
}

# ---- server -------------------------------------------------------------

#' The docked inspector server: mounts the three panes once, routes tab
#' clicks into `rv$insp_tab` (region clicks route via `open_card()`), and
#' mirrors the tab to the card root's `ar-insp-tab-*` class (a message,
#' never a `renderUI` -- switching tabs must not remount pane state).
#' Run / .rtf / code-view moved to the canvas toolbar (mod_toolbar.R).
#' @param id *The module namespace, matching `mod_card_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_card_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod_card_roles_server("roles", store)
    mod_card_options_server("options", store)
    mod_card_filters_server("filters", store)

    # The strip now lives INSIDE the folding pane (2026-07-10) -- a tab click
    # only switches (or re-opens when collapsed); the click-active-tab-to-
    # collapse toggle moved to the toolbar's explicit `panel_toggle` button
    # (mod_toolbar.R). `insp_collapsed` is mirrored to the client via the
    # frame's own `ar-collapse` message (any session may send it -- it
    # targets the workspace class), so the CSS folds/unfolds the card.
    lapply(names(.INSP_TABS), function(tab) {
      shiny::observeEvent(input[[paste0("tab_", tab)]], {
        # Nothing selected = nothing to inspect: a tab click never opens an
        # empty pane.
        if (is.null(store$rv$selected)) {
          return()
        }
        # A direct tab click is navigation, NOT region routing: clear any
        # stale region focus so a pane never renders a region-narrowed (or
        # empty) subset after the user moved on from a jump-link.
        store$rv$region <- NULL
        was_collapsed <- isTRUE(store$rv$insp_collapsed)
        store$rv$insp_tab <- tab
        store$rv$insp_collapsed <- FALSE
        if (was_collapsed) {
          session$sendCustomMessage(
            "ar-collapse",
            list(
              rail = isTRUE(store$rv$rail_collapsed),
              loc_rail = isTRUE(store$rv$loc_rail_collapsed),
              insp = isTRUE(store$rv$insp_collapsed)
            )
          )
        }
      })
    })

    # Selection drives the pane's very existence (2026-07-04): no output
    # selected = inspector collapsed to its rail; selecting one opens it.
    shiny::observe({
      store$rv$insp_collapsed <- is.null(store$rv$selected)
      session$sendCustomMessage(
        "ar-collapse",
        list(
          rail = isTRUE(store$rv$rail_collapsed),
          loc_rail = isTRUE(store$rv$loc_rail_collapsed),
          insp = isTRUE(store$rv$insp_collapsed)
        )
      )
    }) |>
      shiny::bindEvent(store$rv$selected, ignoreNULL = FALSE)

    # Tab -> class flip on the card root. Covers BOTH writers (a direct
    # tab click above, and `open_card()`'s region routing).
    shiny::observe({
      session$sendCustomMessage(
        "ar-insp-tab",
        list(id = ns("card"), tab = store$rv$insp_tab)
      )
    }) |>
      shiny::bindEvent(store$rv$insp_tab)

    # Telemetry (2026-07-04): a small info icon whose native `title` tooltip
    # carries the detail (dataset, matched/total, filter status). The full
    # line got noisy at the bottom of the inspector; the icon keeps the
    # information one hover away without eating a whole row.
    output$telemetry <- shiny::renderUI({
      obj <- selected_object(store)
      if (is.null(obj)) {
        return(.info_icon("no output selected"))
      }
      counts <- tryCatch(
        arpillar::filter_count(store$con, obj@dataset, obj@filters),
        error = function(e) NULL
      )
      if (is.null(counts)) {
        return(.info_icon(sprintf("Dataset: %s", tolower(obj@dataset))))
      }
      filtered <- counts$total - counts$matched
      # Headline (visible next to the icon) stays compact -- the fuller
      # detail (with filtered-out count) goes into the hover title.
      headline <- sprintf(
        "%s \u00b7 %s of %s records",
        tolower(obj@dataset),
        format(counts$matched, big.mark = ","),
        format(counts$total, big.mark = ",")
      )
      detail <- paste(
        c(
          headline,
          if (filtered > 0L) {
            sprintf("Filtered out: %s", format(filtered, big.mark = ","))
          }
        ),
        collapse = "\n"
      )
      .info_icon(detail)
    }) |>
      shiny::bindEvent(store$rv$report, store$rv$selected)
    # Born hidden (the app opens in Data mode, 2026-07-04).
    shiny::outputOptions(output, "telemetry", suspendWhenHidden = FALSE)

    invisible(NULL)
  })
}
