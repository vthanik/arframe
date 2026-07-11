# The real launcher: opens the arpillar catalog, registers any named data
# paths, seeds the injected store (from a saved project when given), and
# mounts the Galley frame. One store per launch — this is a local-first,
# single-session desktop app (design spec's "report-as-project" paradigm),
# not a multi-user Shiny deployment.

#' Launch the arframe report builder
#'
#' Opens the local-first Shiny application: a submission-native report
#' builder over the `arpillar` engine. Every module communicates only through
#' one injected store (design spec 5.1); this function is the sole place that
#' constructs it.
#'
#' @param project *A saved project to reopen.* `<character(1)> | NULL:
#'   default NULL`. A JSON file previously written by the app's Export/Save
#'   (read via [arpillar::report_from_json()]). `NULL` starts a fresh
#'   "Untitled report".
#' @param data *Datasets to register before launch.* `<character>: default
#'   NULL`. A named vector/list of paths to `.parquet`/`.xpt`/`.json` files;
#'   each name becomes the catalog-visible dataset name (e.g. `c(ADSL =
#'   "adsl.parquet")`). `NULL` opens an empty catalog.
#' @param folders *Study folders to mount before launch.* `<character>:
#'   default NULL`. Paths to directories of dataset files; every recognized
#'   file (`.parquet`/`.xpt`/`.json`) is registered under a library node
#'   named for the folder, and appears in Data mode's SOURCES tree. This is
#'   the folder-first on-ramp — point at an ADaM directory and the whole
#'   catalog populates.
#' @param daemons *Background render workers for async export.* `<integer(1)>:
#'   default 2`. The size of the mirai daemon pool the Export package button
#'   renders on, so a multi-output export never freezes the galley (Task 16).
#'   Set to `0` to disable the pool entirely (export then requires the pool,
#'   so it is only useful for headless/test launches that never export).
#'
#' @return Called for its side effect of running the Shiny application; does
#'   not return a value.
#' @export
arframe <- function(project = NULL, data = NULL, folders = NULL, daemons = 2L) {
  con <- arpillar::engine_open()
  if (!is.null(data)) {
    for (nm in names(data)) {
      arpillar::register_dataset(con, nm, data[[nm]])
    }
  }
  report <- NULL
  project_dir <- NULL
  if (!is.null(project)) {
    # `project` accepts either a report.json file (v0 single-file) OR a project
    # folder (v1 folder format). A folder wins when both would resolve.
    if (dir.exists(project)) {
      project_dir <- normalizePath(project)
      report <- arpillar::report_from_folder(project_dir)
    } else {
      report <- arpillar::report_from_json(project)
    }
  }
  store <- new_store(con, report = report)
  if (!is.null(project_dir)) {
    # Both writes to `rv$` AND the mtime scan (which reads `rv$path`) must run
    # under isolate(): we are outside any reactive context at package load.
    shiny::isolate({
      store$rv$path <- project_dir
      store$rv$saved_at <- Sys.time()
      # Mount the theme's ADaM folder so a arframe(project=) launch populates
      # the catalog like the Open button (open_project()) does.
      .mount_theme_adam(store, report, project_dir)
      .refresh_mtimes(store)
    })
  }

  # The async-export daemon pool (Task 16): a per-launch, named compute
  # profile (NEVER set at package load — that would spawn processes on
  # `library(arframe)`). Torn down in `onStop()` below so a closed session
  # leaves no orphan daemons.
  if (daemons > 0L) {
    mirai::daemons(daemons, .compute = "arframe")
  }

  ui <- bslib::page_fillable(
    theme = ar_theme(),
    padding = 0,
    gap = 0,
    .head_assets(),
    # A `position: relative` wrapper around the whole frame — the Add-output
    # overlay (mod_add_output.R) is an absolutely-positioned `inset: 0`
    # sibling of `mod_frame_ui()`'s output, so it needs a same-size
    # positioning ancestor. `.ar-workspace` (inside the frame) is the
    # explicit `height: 100vh` box; this wrapper has no height of its own,
    # it simply grows to contain that box, so it never has to duplicate
    # `100vh` and never fights bslib's fill-item sizing.
    shiny::div(
      class = "ar-app-root",
      mod_frame_ui(
        "frame",
        # Report mode is the full-width List-of-Contents surface plus a
        # sibling desk that the `ar-report-open` class reveals on drill
        # (mirrors Data's list<->grid). The breadcrumb sits above
        # the paper|inspector panes; `contents-crumb` is rendered by the
        # contents module.
        report_body = shiny::tagList(
          mod_contents_ui("contents"),
          shiny::div(
            class = "ar-report-desk",
            shiny::uiOutput("contents-crumb"),
            shiny::div(
              class = "ar-report-panes",
              mod_paper_ui("paper"),
              mod_card_ui("card")
            )
          )
        ),
        data_body = mod_data_ui("data"),
        qc_body = mod_qc_ui("qc"),
        logs_body = mod_logs_ui("qc"),
        setup_body = mod_setup_ui("setup")
      ),
      mod_add_output_ui("add_output")
    )
  )

  server <- function(input, output, session) {
    # Folder mounts run through the store (they read + bump the catalog
    # nonce and append to the log), so they must fire inside a live session
    # AND under `isolate()` — the server body is not itself a reactive
    # consumer, so a bare `rv$` read there aborts. Mounting once at session
    # start is exactly right for a one-session-per-launch local app; the
    # nonce bump invalidates Data mode's first render.
    if (!is.null(folders)) {
      shiny::isolate(
        for (dir in folders) {
          .mount_folder(store, dir)
        }
      )
    }

    mod_frame_server("frame", store)
    mod_contents_server("contents", store)
    mod_paper_server("paper", store)
    mod_card_server("card", store)
    mod_add_output_server("add_output", store)
    mod_data_server("data", store)
    mod_qc_server("qc", store)
    mod_setup_server("setup", store)

    # Auto-save observer: writes to disk 500ms after any dirty flip, when a
    # project folder is bound.
    install_autosave(store)

    # Tab-focus refresh: bridge.js posts `ar_refresh` on visibilitychange;
    # `.refresh_all()` rescans the outputs folder AND garbage-collects
    # stale presence files. (The manual Refresh button was removed;
    # visibilitychange is now the only trigger.)
    shiny::observeEvent(
      input[["ar_refresh"]],
      {
        .refresh_all(store)
      },
      ignoreInit = TRUE
    )

    # Team presence heartbeat: writes `.arframe/presence/<user>.json`
    # every 30s so teammates opening the same folder see who's live.
    # `.heartbeat()` is a no-op when there is no project OR the user is
    # generic; the observer stays cheap either way.
    presence_tick <- function() {
      # `later` fires outside the reactive graph; `isolate()` lets us
      # read the store fields without an "outside of reactive consumer"
      # abort. `.heartbeat()` is a no-op when `path` is NULL, so the
      # tick stays cheap even for un-opened projects.
      tryCatch(
        shiny::isolate(
          .heartbeat(
            store$rv$path,
            .who_am_i(),
            mode = store$rv$mode,
            current_output = store$rv$selected
          )
        ),
        error = function(e) NULL
      )
      later::later(presence_tick, delay = 30)
    }
    later::later(presence_tick, delay = 5)
  }

  shiny::onStop(function() {
    if (daemons > 0L) {
      mirai::daemons(0, .compute = "arframe")
    }
    arpillar::engine_close(con)
  })
  shiny::shinyApp(ui, server)
}
