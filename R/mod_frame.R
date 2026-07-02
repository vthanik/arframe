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

#' The 42px app bar (v5, decision #8): wordmark, the Data/Report
#' segmented toggle (top-LEFT -- modes are peers, state reads before
#' actions), report title (click-to-edit), then the right action cluster:
#' undo/redo, QC, the command-palette hint, Export package.
#' @noRd
.frame_bar <- function(ns) {
  shiny::div(
    class = "ar-bar",
    shiny::span(class = "ar-bar-mark ar-mono", "arframe"),
    .frame_seg(ns),
    shiny::div(class = "ar-bar-divider"),
    .frame_title(ns),
    shiny::div(class = "ar-bar-spacer"),
    shiny::tagAppendAttributes(
      .action_btn(
        ns("undo_btn"),
        .icon("undo", 14),
        variant = "link",
        class = "ar-icon-btn"
      ),
      `aria-label` = "Undo"
    ),
    shiny::tagAppendAttributes(
      .action_btn(
        ns("redo_btn"),
        .icon("redo", 14),
        variant = "link",
        class = "ar-icon-btn"
      ),
      `aria-label` = "Redo"
    ),
    .mode_btn(ns("mode_qc"), "qc", "QC"),
    # Empty on the server -- arframe.js fills it per the CLIENT's OS
    # (navigator.platform: Mac -> the Command glyph, else "Ctrl K"). The server
    # cannot know the browser's OS, so this cannot be decided in R.
    shiny::span(class = "ar-bar-hint ar-mono"),
    # Async export (Task 16): a plain action button kicks the render onto the
    # daemon pool; the hidden download link beside it is clicked by the server
    # (`ar-click`) once the zip is assembled -- a browser download must be
    # initiated by an <a>, but the render must NOT block the request.
    shiny::tags$button(
      id = ns("export_btn"),
      type = "button",
      class = "ar-btn-ink action-button",
      "Export package"
    ),
    shiny::tagAppendAttributes(
      shiny::downloadLink(
        ns("export_dl"),
        label = NULL,
        class = "ar-hidden-dl"
      ),
      `aria-hidden` = "true",
      tabindex = "-1"
    )
  )
}

#' The Data/Report segmented toggle. Both segments are plain mode
#' buttons under one `.ar-seg` border; the ACTIVE segment is styled by a
#' pure CSS rule keyed off the workspace `ar-mode-*` class (see `.mode_btn`),
#' so switching never round-trips just to restyle.
#' @noRd
.frame_seg <- function(ns) {
  shiny::div(
    class = "ar-seg",
    .mode_btn(ns("mode_data"), "data", "Data"),
    .mode_btn(ns("mode_report"), "report", "Report")
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

    # v5 semantics: the Data/Report SEGMENTS are idempotent (clicking the
    # active segment is a no-op -- a segmented control names both states).
    # Only QC keeps the quiet-toggle behavior: clicking the active QC
    # returns to Report, since the right cluster has no "Report" button.
    shiny::observeEvent(input$mode, {
      new_mode <- if (
        identical(input$mode, "qc") && identical(store$rv$mode, "qc")
      ) {
        "report"
      } else {
        input$mode
      }
      store$rv$mode <- new_mode
      session$sendCustomMessage("ar-mode", new_mode)
    })

    # Panel collapse (decision #8): the chevrons render inside the contents
    # rail and the inspector, but they post here through arframe.js's
    # delegated `[data-ar-collapse]` handler -- collapse state is
    # frame-owned in the store (never the DOM; the ar-collapse message
    # mirrors it to workspace classes for CSS).
    shiny::observeEvent(input$collapse, {
      if (identical(input$collapse, "rail")) {
        toggle_rail(store)
      } else {
        toggle_insp(store)
      }
      session$sendCustomMessage(
        "ar-collapse",
        list(
          rail = isTRUE(store$rv$rail_collapsed),
          insp = isTRUE(store$rv$insp_collapsed)
        )
      )
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

    # Export package (decision #8, async per Task 16): render every ready
    # output on the daemon pool through the export-identical seam, then
    # assemble outputs/ + programs/ + report.json + manifest.csv on the main
    # process and hand back the zip. Draft/query and render-failed outputs are
    # skipped and reported in the log (the honest-incompleteness rule); the QC
    # sheet is the pre-flight for this action. The button stays non-blocking:
    # the render runs off-process, the galley stays live, and the completed
    # zip is delivered by clicking the hidden download link.
    export <- export_task()
    # The in-flight export's staging dir + assembled zip path. A plain env
    # (not reactive) -- only the click and status observers touch them, in
    # order, and neither should invalidate anything downstream.
    ex <- new.env(parent = emptyenv())

    shiny::observeEvent(input$export_btn, {
      report <- store$rv$report
      ex$dir <- file.path(tempdir(), .report_slug(report))
      unlink(ex$dir, recursive = TRUE)
      dir.create(
        file.path(ex$dir, "outputs"),
        recursive = TRUE,
        showWarnings = FALSE
      )
      log_line(store, "export: rendering outputs...")
      session$sendCustomMessage(
        "ar-disable",
        list(id = session$ns("export_btn"), disabled = TRUE)
      )
      export$invoke(
        arpillar::report_to_json(report),
        .export_dataset_paths(store, report),
        file.path(ex$dir, "outputs"),
        .export_names(report)
      )
    })

    shiny::observeEvent(export$status(), {
      st <- export$status()
      if (identical(st, "success")) {
        rendered <- as.list(export$result())
        stamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
        res <- .build_export_package(store, ex$dir, stamp, rendered = rendered)
        ex$zip <- file.path(tempdir(), paste0(basename(ex$dir), ".zip"))
        .zip_export(ex$dir, ex$zip)
        log_line(
          store,
          sprintf(
            "export: %d ready, %d skipped",
            length(res$ready),
            length(res$skipped)
          )
        )
        session$sendCustomMessage(
          "ar-disable",
          list(id = session$ns("export_btn"), disabled = FALSE)
        )
        session$sendCustomMessage(
          "ar-click",
          list(id = session$ns("export_dl"))
        )
        shiny::showNotification(
          sprintf("Export ready: %d output(s).", length(res$ready)),
          type = "message"
        )
      } else if (identical(st, "error")) {
        emsg <- tryCatch(
          {
            export$result()
            "unknown error"
          },
          error = function(e) conditionMessage(e)
        )
        log_line(store, paste0("export failed: ", emsg))
        session$sendCustomMessage(
          "ar-disable",
          list(id = session$ns("export_btn"), disabled = FALSE)
        )
        shiny::showNotification("Export failed.", type = "error")
      }
    })

    output$export_dl <- shiny::downloadHandler(
      filename = function() paste0(.report_slug(store$rv$report), ".zip"),
      content = function(file) file.copy(ex$zip, file, overwrite = TRUE)
    )

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
