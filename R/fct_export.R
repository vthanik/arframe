# Export package (design spec 5.3, decision #8): the whole-deliverable
# action. Walks every output in the report, renders each READY one through
# the SAME render_rtf/render_figure_rtf seam the screen and per-output .rtf
# use, and assembles the submission-grade tree:
#
#   <report>/
#     outputs/     one .rtf per ready output (draft/query skipped + reported)
#     programs/    one .R per output (emit_code) + run-all.R (emit_report_code)
#     report.json  the full project spec (report_to_json) -- arframe re-opens it
#     manifest.csv file, number, title, dataset, status, timestamp
#
# The programs/ folder is the reproducibility record: hand it to a regulator
# or an independent QC programmer and they regenerate the package from bare
# arpillar, no arframe/Shiny in the loop.

#' A filesystem-safe base name for a report: lowercased, non-alnum runs to
#' `-`. Falls back to `report` for an all-punctuation name.
#' @noRd
.report_slug <- function(report) {
  slug <- tolower(gsub("[^a-zA-Z0-9]+", "-", trimws(report@name)))
  slug <- gsub("^-+|-+$", "", slug)
  if (nzchar(slug)) slug else "report"
}

#' Render one output's RTF into `outputs/`, returning `TRUE` on success. A
#' status-ready output whose render throws (the static-oracle gap) is
#' reported as a failure, not a silent drop.
#' @noRd
.export_render_one <- function(store, object, out_dir) {
  path <- file.path(out_dir, paste0(.output_slug(object), ".rtf"))
  tryCatch(
    {
      if (.is_figure_type(object@type)) {
        arpillar::render_figure_rtf(store$con, object, path)
      } else {
        ard <- cached_ard(store, object)
        arpillar::render_rtf(ard, object, path)
      }
      TRUE
    },
    error = function(e) FALSE
  )
}

#' The manifest row for one output: file (or `NA` when skipped), number,
#' label, title, dataset, status.
#' @noRd
.manifest_row <- function(object, status, file) {
  data.frame(
    file = file %||% NA_character_,
    number = object@options$number %||% NA_character_,
    label = object@options$number_label %||% NA_character_,
    title = object@title,
    dataset = object@dataset,
    status = status,
    stringsAsFactors = FALSE
  )
}

#' Build the export package tree under `dir` (created if absent), returning
#' a summary `list(ready, skipped, dir, manifest)`. `stamp` is the ISO
#' timestamp written into the manifest -- passed in (never `Sys.time()`
#' inside) so a test can pin it.
#'
#' `rendered` selects the render leg. `NULL` (the default) renders every
#' ready output synchronously here (the standalone/download path). A named
#' `list(<id> = <path>)` -- the result of the async daemon task
#' (`export_mirai()`) -- means the RTFs are ALREADY written into `outputs/`;
#' this function then only classifies (present in the map -> ready, absent ->
#' skipped/error) and assembles the cheap parts (programs, report.json,
#' manifest). Either way the outputs/ filenames are the `.output_slug()`
#' slugs, so the manifest linkage is identical.
#'
#' `report` lets the export click hand in its source-injected copy
#' (`.report_with_source()`) so outputs/, programs/ (emit_code embeds
#' options) and report.json stay self-consistent with what the daemon
#' rendered; `NULL` falls back to the store's live report.
#' @noRd
.build_export_package <- function(
  store,
  dir,
  stamp,
  rendered = NULL,
  report = NULL
) {
  report <- report %||% store$rv$report
  out_dir <- file.path(dir, "outputs")
  prog_dir <- file.path(dir, "programs")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(prog_dir, recursive = TRUE, showWarnings = FALSE)

  objects <- .all_objects(report)
  rows <- list()
  ready <- character(0)
  skipped <- character(0)

  for (obj in objects) {
    status <- arpillar::output_status(obj)
    slug <- .output_slug(obj)
    # Every output gets its reproduction program, ready or not -- the
    # program IS the record, and a draft's program documents intent.
    prog <- tryCatch(
      arpillar::emit_code(
        store$con,
        obj,
        path = file.path(prog_dir, paste0(slug, ".R"))
      ),
      error = function(e) NULL
    )
    # Sync leg renders here; async leg trusts the daemon's already-written
    # file (present in `rendered` -> it rendered ok).
    render_ok <- if (is.null(rendered)) {
      identical(status, "ready") && .export_render_one(store, obj, out_dir)
    } else {
      obj@id %in% names(rendered)
    }
    if (render_ok) {
      ready <- c(ready, obj@id)
      rows[[length(rows) + 1L]] <- .manifest_row(
        obj,
        "ready",
        paste0("outputs/", slug, ".rtf")
      )
    } else {
      # A ready-but-render-failed output is reported as "error", a
      # not-ready one by its status -- both land in the skipped set.
      reason <- if (identical(status, "ready")) "error" else status
      skipped <- c(skipped, obj@id)
      rows[[length(rows) + 1L]] <- .manifest_row(obj, reason, NA_character_)
    }
  }

  # The whole-report program + the project JSON.
  tryCatch(
    arpillar::emit_report_code(
      store$con,
      report,
      path = file.path(prog_dir, "run-all.R")
    ),
    error = function(e) NULL
  )
  arpillar::report_to_json(report, path = file.path(dir, "report.json"))

  manifest <- if (length(rows) > 0L) {
    m <- do.call(rbind, rows)
    m$timestamp <- stamp
    m
  } else {
    data.frame()
  }
  utils::write.csv(
    manifest,
    file.path(dir, "manifest.csv"),
    row.names = FALSE,
    na = ""
  )

  list(ready = ready, skipped = skipped, dir = dir, manifest = manifest)
}

#' Zip the export package `dir` into `zipfile`, rooted at the package
#' folder so the archive extracts to a single `<report>/` directory.
#' `.arframe/` (team state -- roster, activity log, presence) is stripped
#' from the staging directory before zipping so team churn never leaks
#' into the sponsor deliverable.
#' @noRd
.zip_export <- function(dir, zipfile) {
  unlink(file.path(dir, ".arframe"), recursive = TRUE, force = TRUE)
  zip::zipr(zipfile, files = dir)
  invisible(zipfile)
}
