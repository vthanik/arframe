# The real launcher: opens the arpillar catalog, registers any named data
# paths, seeds the injected store (from a saved project when given), and
# mounts the Galley frame. One store per launch -- this is a local-first,
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
#'
#' @return Called for its side effect of running the Shiny application; does
#'   not return a value.
#' @export
arframe <- function(project = NULL, data = NULL) {
  con <- arpillar::engine_open()
  if (!is.null(data)) {
    for (nm in names(data)) {
      arpillar::register_dataset(con, nm, data[[nm]])
    }
  }
  report <- if (!is.null(project)) arpillar::report_from_json(project) else NULL
  store <- new_store(con, report = report)

  ui <- bslib::page_fillable(
    theme = ar_theme(),
    padding = 0,
    gap = 0,
    .head_assets(),
    # A `position: relative` wrapper around the whole frame -- the Add-output
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
        report_body = shiny::tagList(
          mod_contents_ui("contents"),
          shiny::div(class = "ar-slot-placeholder", "Paper"),
          shiny::div(class = "ar-slot-placeholder", "Card")
        ),
        data_body = shiny::div(class = "ar-slot-placeholder", "Data mode"),
        qc_body = shiny::div(class = "ar-slot-placeholder", "QC sheet")
      ),
      mod_add_output_ui("add_output")
    )
  )

  server <- function(input, output, session) {
    mod_frame_server("frame", store)
    mod_contents_server("contents", store)
    mod_add_output_server("add_output", store)
  }

  shiny::onStop(function() arpillar::engine_close(con))
  shiny::shinyApp(ui, server)
}
