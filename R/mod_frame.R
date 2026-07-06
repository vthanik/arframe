# The Galley frame: the whole 100vh workspace -- app bar, the activity bar
# (the far-left mode rail, mockup piece A), the four mounted mode bodies
# (report/data/qc/logs), and the status bar (design spec #3). Layout only:
# every body is handed in by the caller as opaque tag content and all four
# stay MOUNTED at once (draft state lives in the store, never the DOM --
# see the suspend-contract regression in test-fct_store.R). CSS shows only the
# one matching `store$rv$mode` via the `ar-mode-*` class on `.ar-workspace`,
# set by arframe.js's "ar-mode" custom message handler.

#' The Galley frame UI: app bar, activity bar, the four mode bodies,
#' status bar.
#'
#' `report_body`/`data_body`/`qc_body`/`logs_body` are opaque tag content --
#' this module does not know what is inside them, only which `.ar-body-*`
#' container each sits in. The report body is itself the three-region
#' contents/desk/card row; the caller composes that (see [arframe()]).
#' @param id *The module namespace.* `<character(1)>: required`.
#' @param report_body *Report-mode body content.* `<tag/tagList>: required`.
#' @param data_body *Data-mode body content.* `<tag/tagList>: required`.
#' @param qc_body *QC-mode body content.* `<tag/tagList>: required`.
#' @param logs_body *Logs-mode body content.* `<tag/tagList>: required`.
#' @noRd
mod_frame_ui <- function(id, report_body, data_body, qc_body, logs_body, setup_body = NULL) {
  ns <- shiny::NS(id)
  shiny::div(
    # Opens in Setup mode -- study configuration is the first stop.
    class = "ar-workspace ar-mode-setup",
    .frame_bar(ns),
    shiny::div(
      class = "ar-main",
      .frame_actbar(ns),
      shiny::div(
        class = "ar-body",
        shiny::div(class = "ar-body-report", report_body),
        shiny::div(class = "ar-body-data", data_body),
        shiny::div(class = "ar-body-qc", qc_body),
        shiny::div(class = "ar-body-logs", logs_body),
        shiny::div(class = "ar-body-setup", setup_body)
      )
    ),
    .frame_statusbar(ns)
  )
}

#' The 42px app bar (mockup piece A supersedes the v5 segmented toggle):
#' wordmark, report title (click-to-edit), then the right action cluster:
#' undo/redo, the command-palette hint, Export package. Mode switching
#' lives in the activity bar (`.frame_actbar()`).
#' @noRd
.frame_bar <- function(ns) {
  shiny::div(
    class = "ar-bar",
    shiny::span(class = "ar-bar-mark ar-mono", "arframe"),
    shiny::div(class = "ar-bar-divider"),
    .frame_title(ns),
    # Save-state chip -- updated via `ar-save-state` message. Idle default
    # so the user sees the affordance without waiting for a first save.
    shiny::span(
      id = ns("save_chip"),
      class = "ar-save-chip",
      `data-state` = "idle",
      shiny::span(class = "ar-save-chip-lbl", "Saved")
    ),
    shiny::div(class = "ar-bar-spacer"),
    # Open folder (project switcher). shinyDirButton returns a tagList, so
    # tagAppendAttributes leaks extra attrs as text -- keep it plain and let
    # the label ("Open") stand.
    shinyFiles::shinyDirButton(
      ns("open_project"),
      label = "Open",
      title = "Open project folder",
      class = "ar-icon-btn ar-icon-btn-labeled ar-icon-btn-open"
    ),
    # Refresh: rescan on-disk state (scan_and_merge)
    shiny::tagAppendAttributes(
      .action_btn(
        ns("refresh_btn"),
        .icon("redo", 14),
        variant = "link",
        class = "ar-icon-btn"
      ),
      `aria-label` = "Refresh"
    ),
    # Undo / Redo -- kept until the top-bar Preact stage supersedes them.
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
    # Command palette hint (arframe.js fills the glyph per navigator.platform).
    shiny::span(class = "ar-bar-hint ar-mono"),
    # Package -- ships the report tree as a submission-ready zip.
    shiny::tags$button(
      id = ns("export_btn"),
      type = "button",
      class = "ar-btn-ink action-button",
      .icon("package", 13),
      shiny::span("Package"),
      shiny::span(class = "ar-btn-kbd ar-mono", shiny::HTML("&#8984;&#8679;E"))
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

#' The activity bar (mockup piece A): a narrow far-left vertical rail of
#' icon buttons, one per mode destination -- Report, Data, QC, Logs. The
#' ACTIVE button is a pure CSS rule keyed off the workspace `ar-mode-*`
#' class, so switching never round-trips just to restyle. `data-ar-mode`
#' is what arframe.js's delegated click handler reads to fire `input$mode`.
#' Distinct from the collapsible CONTENTS rail (`rv$rail_collapsed`) --
#' the activity bar never collapses.
#' @noRd
.frame_actbar <- function(ns) {
  shiny::div(
    class = "ar-actbar",
    # Setup leads the rail (user decision 2026-07-06): the study configuration
    # is the first stop; Data / Report follow. Startup mode still opens on
    # whichever `store$rv$mode` was seeded with by the caller.
    .act_btn(ns("mode_setup"), "setup", "gear", "Setup"),
    .act_btn(ns("mode_data"), "data", "database", "Data"),
    .act_btn(ns("mode_report"), "report", "report", "Report"),
    .act_btn(ns("mode_qc"), "qc", "check", "Review"),
    .act_btn(ns("mode_logs"), "logs", "logs", "Logs")
  )
}

#' One activity-bar button: icon with its label visible BELOW it
#' (explorer-style rail, GOV.UK "visible label over tooltip" principle).
#' @noRd
.act_btn <- function(id, mode, icon, label) {
  shiny::tagAppendAttributes(
    .action_btn(
      id,
      shiny::tagList(
        .icon(icon, 16),
        shiny::span(class = "ar-act-lbl", label)
      ),
      variant = "link",
      class = "ar-act-btn"
    ),
    `data-ar-mode` = mode,
    `aria-label` = label
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

    # Activity-bar semantics: clicking another mode switches; clicking the
    # ACTIVE mode's item again toggles the adjacent panel (the contents
    # rail) -- the explorer-style show/hide the user asked for. Collapse
    # state stays frame-owned in the store, mirrored via ar-collapse.
    shiny::observeEvent(input$mode, {
      if (identical(input$mode, store$rv$mode)) {
        toggle_rail(store)
        session$sendCustomMessage(
          "ar-collapse",
          list(
            rail = isTRUE(store$rv$rail_collapsed),
            insp = isTRUE(store$rv$insp_collapsed)
          )
        )
        return()
      }
      store$rv$mode <- input$mode
      session$sendCustomMessage("ar-mode", input$mode)
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

    # Open project (shinyFiles). Root at ~ so the picker starts local.
    open_volumes <- c(home = path.expand("~"), root = "/")
    shinyFiles::shinyDirChoose(
      input,
      "open_project",
      roots = open_volumes
    )
    shiny::observeEvent(input$open_project, {
      dir <- shinyFiles::parseDirPath(open_volumes, input$open_project)
      if (length(dir) == 1L && nzchar(dir)) {
        tryCatch(
          open_project(store, dir),
          error = function(e) {
            log_line(store, sprintf("open failed: %s", conditionMessage(e)))
            shiny::showNotification(
              paste("Could not open project:", conditionMessage(e)),
              type = "error"
            )
          }
        )
      }
    })

    # Refresh: rescan on-disk state (bring in other-session edits).
    shiny::observeEvent(input$refresh_btn, {
      tryCatch(
        scan_and_merge(store),
        error = function(e) {
          log_line(store, sprintf("refresh failed: %s", conditionMessage(e)))
        }
      )
    })

    # Save-state chip driver. States:
    #   readonly  = no project folder mounted
    #   saving    = dirty flag on
    #   idle      = clean, `saved_at` populated
    shiny::observe({
      state <- if (is.null(store$rv$path)) {
        list(state = "readonly", label = "No folder — Open one")
      } else if (isTRUE(store$rv$dirty)) {
        list(state = "saving", label = "Saving…")
      } else if (!is.null(store$rv$saved_at)) {
        list(state = "idle", label = "Saved")
      } else {
        list(state = "idle", label = "Ready")
      }
      session$sendCustomMessage(
        "ar-save-state",
        list(id = session$ns("save_chip"), state = state$state, label = state$label)
      )
    })

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
      # Export-time source injection: the daemon sees only this JSON, and
      # the success handler assembles the package from the SAME copy -- the
      # store's live report never carries a stamped date.
      report <- .report_with_source(store$rv$report)
      ex$report <- report
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
        res <- .build_export_package(
          store,
          ex$dir,
          stamp,
          rendered = rendered,
          report = ex$report
        )
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
    # See mod_toolbar.R: hidden-link download outputs must stay unsuspended.
    shiny::outputOptions(output, "export_dl", suspendWhenHidden = FALSE)

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
