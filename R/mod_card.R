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

# ---- coming stub ------------------------------------------------------

#' A quiet "coming" stub for a pane not yet implemented (Filters: Task 12;
#' Ranks: deliberately deferred to the AE hierarchy work) -- the note plus
#' a `coming` tag, so the card never renders visibly empty for a tab a
#' click legitimately routed to.
#' @noRd
.card_coming_stub <- function(region, note) {
  shiny::tags$div(
    class = "ar-card-coming",
    shiny::tags$p(
      class = "ar-mono",
      note,
      shiny::tags$span(class = "ar-tag-coming ar-mono", "coming")
    )
  )
}

# ---- UI ---------------------------------------------------------------

#' The docked inspector UI (v5, decision #8): a fixed-width right panel --
#' tab strip (Roles/Options/Filters/Ranks + collapse chevron), the pane
#' stack (every pane mounts once, the `ar-insp-tab-*` class on the card
#' root picks which shows -- matching `mod_frame_ui()`'s pattern, so a
#' role edit made on one tab survives any amount of tab switching), the
#' action footer (Run / .rtf / code), and the telemetry line. A second,
#' slim strip renders when the workspace carries `ar-insp-collapsed`
#' (frame-owned, see `toggle_insp()`).
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
      # v5 refinement: the tab strip is VERTICAL on the rail's side (not a
      # horizontal strip on top), so the editing panes get the full height.
      shiny::div(
        class = "ar-insp-tabs",
        lapply(names(.INSP_TABS), function(tab) .insp_tab_btn(ns, tab)),
        shiny::tags$div(class = "ar-insp-tabs-spacer"),
        shiny::tags$button(
          type = "button",
          class = "ar-icon-btn ar-insp-cv",
          `data-ar-collapse` = "insp",
          `aria-label` = "Collapse inspector",
          .icon("chevrons_right", 13)
        )
      ),
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
            .card_coming_stub(
              "ranks",
              "Top-N and incidence cutoffs arrive with the AE hierarchy table"
            )
          )
        ),
        shiny::div(
          class = "ar-insp-act",
          shiny::tags$button(
            id = ns("run"),
            type = "button",
            class = "ar-insp-run action-button",
            .icon("play", 11),
            "Run",
            # U+2318 PLACE OF INTEREST SIGN + U+21B5 CARRIAGE RETURN -- \u
            # escapes keep R/ ASCII-clean (R CMD check portability rule).
            shiny::span(class = "ar-insp-kbd ar-mono", "\u2318\u21b5")
          ),
          shiny::downloadLink(
            ns("rtf"),
            label = shiny::tagList(.icon("export", 12), ".rtf"),
            class = "ar-insp-dl"
          ),
          shiny::tags$button(
            id = ns("code"),
            type = "button",
            class = "ar-insp-dl action-button",
            `aria-label` = "View reproduction code",
            .icon("code", 12)
          )
        ),
        shiny::uiOutput(ns("telemetry"), class = "ar-insp-tel ar-mono")
      )
    ),
    shiny::div(
      class = "ar-insp-slim",
      shiny::tags$button(
        type = "button",
        class = "ar-icon-btn",
        `data-ar-collapse` = "insp",
        `aria-label` = "Expand inspector",
        .icon("chevrons_left", 13)
      )
    )
  )
}

# ---- server -------------------------------------------------------------

#' A filesystem-safe slug for the selected output's download filename:
#' `t-14-1-1-demographics.rtf` -- kind letter + number + title, lowercased,
#' non-alnum runs collapsed to `-`.
#' @noRd
.output_slug <- function(object) {
  label <- object@options$number_label %||% "Table"
  kind <- tolower(substr(label, 1, 1))
  raw <- paste(kind, object@options$number %||% "", object@title)
  slug <- tolower(gsub("[^a-zA-Z0-9]+", "-", trimws(raw)))
  gsub("^-+|-+$", "", slug)
}

#' The docked inspector server (v5): mounts the roles editor once, routes
#' tab clicks into `rv$insp_tab` (region clicks route via `open_card()`),
#' mirrors the tab to the card root's `ar-insp-tab-*` class (a message,
#' never a `renderUI` -- switching tabs must not remount pane state), and
#' owns the action footer: Run (drops the ARD memo and bumps `run_nonce`
#' so the paper re-typesets fresh), the per-output `.rtf` download, and
#' the code-view toggle (S4).
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

    lapply(names(.INSP_TABS), function(tab) {
      shiny::observeEvent(input[[paste0("tab_", tab)]], {
        store$rv$insp_tab <- tab
      })
    })

    # Tab -> class flip on the card root. Covers BOTH writers (a direct
    # tab click above, and `open_card()`'s region routing).
    shiny::observe({
      session$sendCustomMessage(
        "ar-insp-tab",
        list(id = ns("card"), tab = store$rv$insp_tab)
      )
    }) |>
      shiny::bindEvent(store$rv$insp_tab)

    # Run: drop every memoized ARD so the rebuild is honest (a stale
    # upstream parquet re-collects rather than replaying the memo), clear
    # the stale-proof flags (decision #8 -- Run IS the re-typeset), then
    # bump the nonce the paper's renderers bind to.
    shiny::observeEvent(input$run, {
      keys <- grep("^ard::", ls(store$cache), value = TRUE)
      rm(list = keys, envir = store$cache)
      store$rv$stale <- character(0)
      store$rv$run_nonce <- store$rv$run_nonce + 1L
      log_line(store, "run: re-typeset requested")
    })

    # The code button toggles the desk's code view (mod_paper owns the
    # panel DOM; this only flips the shared store flag).
    shiny::observeEvent(input$code, {
      store$rv$code_view <- !isTRUE(store$rv$code_view)
    })

    # The per-output RTF -- the SAME render seam as export (decision #7's
    # one-spec rule): tables through render_rtf, figures through
    # render_figure_rtf.
    output$rtf <- shiny::downloadHandler(
      filename = function() {
        obj <- selected_object(store)
        if (is.null(obj)) "output.rtf" else paste0(.output_slug(obj), ".rtf")
      },
      content = function(file) {
        obj <- selected_object(store)
        if (is.null(obj)) {
          .abort_app("No output is selected.")
        }
        if (.is_figure_type(obj@type)) {
          arpillar::render_figure_rtf(store$con, obj, file)
        } else {
          ard <- cached_ard(store, obj)
          arpillar::render_rtf(ard, obj, file)
        }
      }
    )

    # Telemetry (the teal steal): dataset + retained/total records through
    # the engine's own count -- no DBI call lives in this module.
    output$telemetry <- shiny::renderUI({
      obj <- selected_object(store)
      if (is.null(obj)) {
        return(shiny::span("no output selected"))
      }
      counts <- tryCatch(
        arpillar::filter_count(store$con, obj@dataset, obj@filters),
        error = function(e) NULL
      )
      if (is.null(counts)) {
        return(shiny::span(tolower(obj@dataset)))
      }
      shiny::span(sprintf(
        "%s \u00b7 %s of %s records",
        tolower(obj@dataset),
        format(counts$matched, big.mark = ","),
        format(counts$total, big.mark = ",")
      ))
    }) |>
      shiny::bindEvent(store$rv$report, store$rv$selected)

    invisible(NULL)
  })
}
