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

# ---- inspector tabs (v5, decision #8) ---------------------------------------

#' The four inspector tabs, in display order. Ranks is a disabled-coming
#' row per the plan (v1.1), but the tab exists so the frame never reflows
#' when it arrives.
#' @noRd
.INSP_TABS <- c(
  roles = "Roles",
  options = "Options",
  filters = "Filters",
  ranks = "Ranks"
)

#' One inspector tab button. The ACTIVE tab is styled by a pure CSS rule
#' keyed off the `ar-insp-tab-*` class on the card root (set by the
#' "ar-insp-tab" message), mirroring the frame's mode-button pattern -- a
#' tab switch never re-renders the panel body.
#' @noRd
.insp_tab_btn <- function(ns, tab) {
  shiny::tags$button(
    id = ns(paste0("tab_", tab)),
    type = "button",
    class = "ar-insp-tab action-button",
    `data-ar-insp-tab` = tab,
    .icon(tab, 15),
    shiny::tags$span(class = "ar-insp-tab-lbl", .INSP_TABS[[tab]])
  )
}

# ---- UI ---------------------------------------------------------------

#' The docked inspector UI: a fixed-width right panel -- the pane stack
#' (every pane mounts once, the `ar-insp-tab-*` class on the card root
#' picks which shows -- matching `mod_frame_ui()`'s pattern, so a role
#' edit made on one tab survives any amount of tab switching), the
#' telemetry line, and the explorer-style labeled tab rail on the far
#' right edge (Roles/Options/Filters/Ranks). When the workspace carries
#' `ar-insp-collapsed` (frame-owned, see `toggle_insp()`) CSS hides only
#' `.ar-insp-main` -- the tab rail itself is the collapsed strip. The
#' action footer moved to the canvas toolbar (mod_toolbar.R, 2026-07-04).
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
          ),
          shiny::div(
            class = "ar-insp-pane ar-insp-pane-ranks",
            mod_card_ranks_ui(ns("ranks"))
          )
        ),
        shiny::uiOutput(ns("telemetry"), class = "ar-insp-tel ar-mono")
      ),
      # Explorer-style labeled tab rail on the inspector's FAR RIGHT edge
      # (2026-07-04): the rail itself is the persistent slim strip when the
      # pane is collapsed, so no chevrons and no separate slim div remain.
      shiny::div(
        class = "ar-insp-tabs",
        lapply(names(.INSP_TABS), function(tab) .insp_tab_btn(ns, tab))
      )
    )
  )
}

# ---- server -------------------------------------------------------------

#' The docked inspector server: mounts the four panes once, routes tab
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
    mod_card_ranks_server("ranks", store)

    # The tab strip doubles as a show/hide toggle: clicking the ACTIVE tab
    # while the pane is open collapses it (the strip stays); clicking any tab
    # while collapsed re-opens it on that tab; clicking a different tab while
    # open just switches. `insp_collapsed` is mirrored to the client via the
    # frame's own `ar-collapse` message (any session may send it -- it targets
    # the workspace class), so the CSS folds/unfolds the pane.
    lapply(names(.INSP_TABS), function(tab) {
      shiny::observeEvent(input[[paste0("tab_", tab)]], {
        # Nothing selected = nothing to inspect: the rail stays a rail
        # (2026-07-04) -- a tab click never opens an empty pane.
        if (is.null(store$rv$selected)) {
          return()
        }
        # A direct tab click is navigation, NOT region routing: clear any
        # stale region focus so a pane never renders a region-narrowed (or
        # empty) subset after the user moved on from a jump-link.
        store$rv$region <- NULL
        if (isTRUE(store$rv$insp_collapsed)) {
          store$rv$insp_collapsed <- FALSE
          store$rv$insp_tab <- tab
        } else if (identical(store$rv$insp_tab, tab)) {
          store$rv$insp_collapsed <- TRUE
        } else {
          store$rv$insp_tab <- tab
        }
        session$sendCustomMessage(
          "ar-collapse",
          list(
            rail = isTRUE(store$rv$rail_collapsed),
            insp = isTRUE(store$rv$insp_collapsed)
          )
        )
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
