# Async export (design spec 5.3, plan Task 16): the whole-package render walks
# every ready output through the engine -- each a fresh DuckDB collect plus an
# RTF typeset, heavy enough to freeze a single-process Shiny app for seconds.
# `export_task()` moves it onto a mirai daemon so the galley stays live.
#
# A DuckDB connection NEVER crosses the daemon boundary -- it is a C handle
# bound to THIS process. The daemon receives only the report JSON string and
# the dataset file PATHS, and opens its OWN engine. The daemon expression
# references base + arpillar only; arframe is not loaded there (so nothing
# arframe-side has to serialise, and dev `load_all()` works too).

#' The named dataset file paths every ready output in `report` reads from,
#' resolved off the live catalog (`arpillar::dataset_path()`). A named list
#' `list(<dataset> = <path>)` -- the daemon re-registers these before
#' rendering. Only ready outputs contribute (a draft has nothing to render),
#' and each dataset appears once.
#' @noRd
.export_dataset_paths <- function(store, report) {
  ready <- Filter(
    function(o) identical(arpillar::output_status(o), "ready"),
    .all_objects(report)
  )
  datasets <- unique(vapply(ready, function(o) o@dataset, character(1)))
  stats::setNames(
    lapply(datasets, function(nm) arpillar::dataset_path(store$con, nm)),
    datasets
  )
}

#' The `id -> "<slug>.rtf"` filename map for every ready output. Computed on
#' the main process (where `.output_slug()` lives) and handed to the daemon
#' so the RTFs it writes carry the SAME filenames the synchronous export tree
#' uses -- the daemon never needs any arframe naming logic.
#' @noRd
.export_names <- function(report) {
  ready <- Filter(
    function(o) identical(arpillar::output_status(o), "ready"),
    .all_objects(report)
  )
  stats::setNames(
    lapply(ready, function(o) paste0(.output_slug(o), ".rtf")),
    vapply(ready, function(o) o@id, character(1))
  )
}

#' Build the export mirai: render every ready output in the serialised report
#' to `out_dir`, returning a NAMED character vector (`id -> path`) of the RTFs
#' actually written. Self-contained -- the daemon opens its own engine from
#' `paths` and uses only arpillar, so no connection (and nothing arframe-side)
#' has to serialise. A per-output render error is swallowed so one bad output
#' does not sink the whole batch; the main process sees it missing from the
#' result and reports it skipped (fail loud, not fatal).
#' @noRd
export_mirai <- function(report_json, paths, out_dir, names = list()) {
  mirai::mirai(
    {
      # `arpillar::`-qualified (never `library(arpillar)`) so R CMD check's
      # static analysis of this quoted daemon expression stays clean; the
      # qualification also auto-loads the arpillar namespace in the fresh
      # daemon process, so no attach step is needed at runtime either.
      con <- arpillar::engine_open()
      on.exit(arpillar::engine_close(con))
      for (nm in base::names(paths)) {
        arpillar::register_dataset(con, nm, paths[[nm]])
      }
      report <- arpillar::report_from_json(report_json)
      gens <- arpillar::generators()
      objs <- unlist(
        lapply(report@pages, function(pg) pg@objects),
        recursive = FALSE
      )
      files <- character(0)
      for (obj in objs) {
        if (!identical(arpillar::output_status(obj), "ready")) {
          next
        }
        g <- gens[[obj@type]]
        is_fig <- !is.null(g) && identical(g$kind, "figure")
        fname <- names[[obj@id]]
        if (is.null(fname)) {
          fname <- paste0(obj@id, ".rtf")
        }
        path <- file.path(out_dir, fname)
        # Thread the study theme (Setup) into the export render so a
        # study-level default resolves identically to the live screen
        # (mod_paper passes report@theme too) -- without it, export and
        # screen silently diverge on any theme-set display option.
        theme <- report@theme
        ok <- tryCatch(
          {
            if (is_fig) {
              arpillar::render_figure_rtf(con, obj, path, theme = theme)
            } else {
              arpillar::render_rtf(
                arpillar::build_ard(con, obj),
                obj,
                path,
                theme = theme
              )
            }
            TRUE
          },
          error = function(e) FALSE
        )
        if (ok) {
          files <- c(files, stats::setNames(path, obj@id))
        }
      }
      files
    },
    report_json = report_json,
    paths = paths,
    out_dir = out_dir,
    names = names,
    .compute = "arframe"
  )
}

#' The export ExtendedTask: wraps `export_mirai()` so the Export button can
#' fire it non-blocking and react to `$status()` (running/success/error).
#' Constructed once per session in `mod_frame_server()`; the daemon pool it
#' runs on is set up in `arframe()`.
#' @noRd
export_task <- function() {
  shiny::ExtendedTask$new(function(report_json, paths, out_dir, names) {
    export_mirai(report_json, paths, out_dir, names)
  })
}
