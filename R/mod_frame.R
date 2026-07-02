# The Galley frame: the whole 100vh workspace -- app bar, the three mounted
# mode bodies (report/data/qc), and the status bar (design spec #3). Layout
# only: every body is handed in by the caller as opaque tag content and all
# three stay MOUNTED at once (draft state lives in the store, never the DOM --
# see the suspend-contract regression in test-fct_store.R). CSS shows only the
# one matching `store$rv$mode` via the `ar-mode-*` class on `.ar-workspace`,
# set by arframe.js's "ar-mode" custom message handler.

#' The Galley frame UI: app bar, the three mode bodies, status bar.
#'
#' `report_body`/`data_body`/`qc_body` are opaque tag content -- this module
#' does not know what is inside them, only which `.ar-body-*` container each
#' sits in. The report body is itself the three-region contents/desk/card row;
#' the caller composes that (see [arframe()]).
#' @param id *The module namespace.* `<character(1)>: required`.
#' @param report_body *Report-mode body content.* `<tag/tagList>: required`.
#' @param data_body *Data-mode body content.* `<tag/tagList>: required`.
#' @param qc_body *QC-mode body content.* `<tag/tagList>: required`.
#' @noRd
mod_frame_ui <- function(id, report_body, data_body, qc_body) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "ar-workspace ar-mode-report",
    .frame_bar(ns),
    shiny::div(
      class = "ar-body",
      shiny::div(class = "ar-body-report", report_body),
      shiny::div(class = "ar-body-data", data_body),
      shiny::div(class = "ar-body-qc", qc_body)
    ),
    .frame_statusbar(ns)
  )
}

#' The 42px app bar: wordmark, report title (click-to-edit), mode buttons,
#' the (inert) command-palette hint, undo/redo, Export package.
#' @noRd
.frame_bar <- function(ns) {
  shiny::div(
    class = "ar-bar",
    shiny::span(class = "ar-bar-mark ar-mono", "arframe"),
    shiny::div(class = "ar-bar-divider"),
    .frame_title(ns),
    shiny::div(class = "ar-bar-spacer"),
    .action_btn(
      ns("undo_btn"),
      .icon("undo", 14),
      variant = "link",
      class = "ar-icon-btn"
    ),
    .action_btn(
      ns("redo_btn"),
      .icon("redo", 14),
      variant = "link",
      class = "ar-icon-btn"
    ),
    .mode_btn(ns("mode_data"), "data", "Data"),
    .mode_btn(ns("mode_qc"), "qc", "QC"),
    shiny::span(class = "ar-bar-hint ar-mono", "\u2318K"),
    .action_btn(
      ns("export_btn"),
      "Export package",
      variant = "link",
      class = "ar-btn-ink"
    )
  )
}

#' One quiet mode-switch button (`Data` / `QC`). No "active" class is set
#' here -- the active state is a pure CSS rule keyed off the `.ar-mode-*`
#' class on the workspace root (see arframe.css 02 frame), so switching mode
#' never needs a second server round-trip just to restyle the buttons.
#' `data-ar-mode` is what arframe.js's delegated click handler reads to fire
#' `input$mode`.
#' @noRd
.mode_btn <- function(id, mode, label) {
  shiny::tagAppendAttributes(
    .action_btn(id, label, variant = "link", class = "ar-bar-mode"),
    `data-ar-mode` = mode
  )
}

#' The click-to-edit report title: a static span with a pencil affordance
#' that JS flips to a text input (`ar-title-editing` class on the wrapper,
#' CSS-driven visibility so no inline styles are toggled from JS); Enter/blur
#' hands the value back through `input$name`.
#' @noRd
.frame_title <- function(ns) {
  shiny::div(
    id = ns("title_wrap"),
    class = "ar-title-wrap",
    shiny::tagAppendAttributes(
      shiny::textOutput(ns("title_display"), inline = TRUE),
      class = "ar-title"
    ),
    .icon("pencil", 12),
    shiny::div(
      class = "ar-title-input",
      shiny::textInput(ns("name"), label = NULL, value = "")
    )
  )
}

#' The 26px mono status bar: output/ready counts, active dataset, saved time.
#' Filled in by later tasks (Contents owns the counts, Data mode the active
#' dataset); this task ships the empty structural shell.
#' @noRd
.frame_statusbar <- function(ns) {
  shiny::div(
    class = "ar-statusbar ar-mono",
    shiny::span(id = ns("status_counts")),
    shiny::div(class = "ar-statusbar-spacer"),
    shiny::span(id = ns("status_dataset")),
    shiny::span(id = ns("status_saved"))
  )
}

#' The Galley frame server: mode switching, undo/redo, report-title edit.
#'
#' All three concerns write through `store` only -- `rv$mode` for mode
#' switching (mirrored to the client via the `ar-mode` message so CSS can show
#' the right `.ar-body-*`), `commit()` for the title (a direct rename of the
#' report itself, not an object -- `rename_output()`'s sibling), and
#' `undo()`/`redo()` for the history buttons.
#' @param id *The module namespace, matching `mod_frame_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_frame_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    output$title_display <- shiny::renderText(store$rv$report@name)

    # A mode button toggles: clicking the ACTIVE mode returns to Report, so
    # Report (home) is always reachable without a separate affordance the
    # two-button bar does not have.
    shiny::observeEvent(input$mode, {
      new_mode <- if (identical(store$rv$mode, input$mode)) {
        "report"
      } else {
        input$mode
      }
      store$rv$mode <- new_mode
      session$sendCustomMessage("ar-mode", new_mode)
    })

    shiny::observeEvent(input$name, {
      title <- trimws(input$name)
      if (!nzchar(title) || identical(title, store$rv$report@name)) {
        return()
      }
      commit(
        store,
        S7::set_props(store$rv$report, name = title),
        label = "rename report"
      )
    })

    shiny::observeEvent(input$undo_btn, undo(store))
    shiny::observeEvent(input$redo_btn, redo(store))

    # store$undo is a plain (non-reactive) environment; store$rv$report is
    # the reactive proxy every commit()/undo()/redo() writes last, so reading
    # it here is what gives this observer its invalidation trigger.
    shiny::observe({
      store$rv$report
      session$sendCustomMessage(
        "ar-disable",
        list(id = session$ns("undo_btn"), disabled = !can_undo(store))
      )
      session$sendCustomMessage(
        "ar-disable",
        list(id = session$ns("redo_btn"), disabled = !can_redo(store))
      )
    })

    invisible(NULL)
  })
}
