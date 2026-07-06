# The Galley frame: a 46px .ex-appbar on top of five mounted mode bodies
# (report/data/qc/logs/setup). Layout only: every body is handed in by the
# caller as opaque tag content and all five stay MOUNTED at once (draft
# state lives in the store, never the DOM -- see the suspend-contract
# regression in test-fct_store.R). CSS shows only the one matching
# `store$rv$mode` via the `ar-mode-*` class on `.ar-workspace`, set by
# arframe.js's "ar-mode" custom message handler.

#' The Galley frame UI: a single-row appbar over the five mounted mode bodies.
#'
#' Mode switching lives in the header's `.ex-section-switch` segmented control
#' (Stage 2 rebuild); the earlier far-left `.ar-actbar` rail is gone. Each
#' segment carries `data-ar-mode`, picked up by arframe.js's delegated click
#' handler which fires `input$mode` -- the observer at `mod_frame_server()`
#' still owns the mode-toggle semantics.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @param report_body,data_body,qc_body,logs_body,setup_body *Per-mode body
#'   content.* `<tag/tagList>: required`. Opaque to this module.
#' @noRd
mod_frame_ui <- function(
  id,
  report_body,
  data_body,
  qc_body,
  logs_body,
  setup_body = NULL
) {
  ns <- shiny::NS(id)
  shiny::div(
    # Opens in Setup mode -- study configuration is the first stop.
    class = "ar-workspace ar-mode-setup",
    .frame_bar(ns),
    shiny::div(
      class = "ar-body",
      shiny::div(class = "ar-body-setup", setup_body),
      shiny::div(class = "ar-body-data", data_body),
      shiny::div(class = "ar-body-report", report_body),
      shiny::div(class = "ar-body-qc", qc_body),
      shiny::div(class = "ar-body-logs", logs_body)
    )
  )
}

#' The 46px `.ex-appbar`: wordmark, segmented mode switch, click-to-edit
#' title (natively centered in the title slot via flex), then the actions
#' cluster (Open \u00b7 save chip \u00b7 Refresh \u00b7 Undo/Redo \u00b7 palette hint \u00b7
#' Package). Presence avatars are wired in Stage 3.
#' @noRd
.frame_bar <- function(ns) {
  shiny::div(
    class = "ex-appbar",
    shiny::span(class = "ex-appbar-brand ar-mono", "arframe"),
    .frame_section_switch(),
    .frame_title(ns),
    shiny::div(
      class = "ex-appbar-actions",
      # Open folder (project switcher). shinyDirButton returns a tagList.
      shiny::div(
        class = "ar-picker",
        shinyFiles::shinyDirButton(
          ns("open_project"),
          label = "Open",
          title = "Open project folder",
          class = "ex-btn-sm btn btn-outline-secondary"
        )
      ),
      # Save-state chip -- updated via `ar-save-state` message.
      shiny::span(
        id = ns("save_chip"),
        class = "ar-save-chip",
        `data-state` = "idle",
        shiny::span(class = "ar-save-chip-lbl", "Saved")
      ),
      shiny::span(class = "ex-tb-sep"),
      shiny::tagAppendAttributes(
        .action_btn(
          ns("refresh_btn"),
          .icon("redo", 14),
          variant = "link",
          class = "ex-icon-btn"
        ),
        `aria-label` = "Refresh"
      ),
      shiny::tagAppendAttributes(
        .action_btn(
          ns("undo_btn"),
          .icon("undo", 14),
          variant = "link",
          class = "ex-icon-btn"
        ),
        `aria-label` = "Undo"
      ),
      shiny::tagAppendAttributes(
        .action_btn(
          ns("redo_btn"),
          .icon("redo", 14),
          variant = "link",
          class = "ex-icon-btn"
        ),
        `aria-label` = "Redo"
      ),
      # Command palette hint (arframe.js fills the glyph per navigator.platform).
      shiny::span(class = "ar-bar-hint ar-mono"),
      shiny::span(class = "ex-tb-sep"),
      # Package -- ships the report tree as a submission-ready zip.
      shiny::tags$button(
        id = ns("export_btn"),
        type = "button",
        class = "ar-btn-ink action-button",
        .icon("package", 13),
        shiny::span("Package"),
        shiny::span(
          class = "ar-btn-kbd ar-mono",
          shiny::HTML("&#8984;&#8679;E")
        )
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
  )
}

#' The segmented mode switch inside the appbar. Setup / Data / Report /
#' Review / Logs are peers; the ACTIVE segment is a pure CSS rule keyed
#' off the workspace `ar-mode-*` class (no server round-trip on switch).
#' Each segment carries `data-ar-mode` for arframe.js's delegated click
#' handler, which posts `input$mode`.
#' @noRd
.frame_section_switch <- function() {
  shiny::div(
    class = "ex-section-switch",
    role = "tablist",
    .seg("setup", "Setup"),
    .seg("data", "Data"),
    .seg("report", "Report"),
    .seg("qc", "Review"),
    .seg("logs", "Logs")
  )
}

#' One appbar segment: a plain <button> that the delegated click handler
#' reads via `data-ar-mode`. No Shiny action-button wrapper -- the input
#' is posted from JS, so a bare <button> keeps the DOM minimal.
#' @noRd
.seg <- function(mode, label) {
  shiny::tags$button(
    type = "button",
    class = "ex-seg",
    role = "tab",
    `data-ar-mode` = mode,
    `aria-label` = label,
    label
  )
}

#' The click-to-edit report title: a static span with a pencil affordance
#' that JS flips to a text input (`ar-title-editing` class on the wrapper,
#' CSS-driven visibility so no inline styles are toggled from JS); Enter/blur
#' hands the value back through `input$name`. Sits inside `.ex-appbar-title`
#' which centers the interior in the title slot via flex.
#' @noRd
.frame_title <- function(ns) {
  shiny::div(
    class = "ex-appbar-title",
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

    # Refresh: rescan on-disk state (bring in other-session edits) via
    # the consolidated `.refresh_all()` helper. Both the manual button
    # and the tab-focus event route through the same call site so any
    # additional refresh step lands in one place.
    shiny::observeEvent(input$refresh_btn, {
      tryCatch(
        .refresh_all(store),
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
        list(state = "readonly", label = "No folder \u2014 Open one")
      } else if (isTRUE(store$rv$dirty)) {
        list(state = "saving", label = "Saving\u2026")
      } else if (!is.null(store$rv$saved_at)) {
        list(state = "idle", label = "Saved")
      } else {
        list(state = "idle", label = "Ready")
      }
      session$sendCustomMessage(
        "ar-save-state",
        list(
          id = session$ns("save_chip"),
          state = state$state,
          label = state$label
        )
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
