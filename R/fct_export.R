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
#' reported as a failure, not a silent drop. `slug` is the caller's
#' already-computed `arpillar::output_slugs()` entry for `object`.
#' @noRd
.export_render_one <- function(store, object, out_dir, slug) {
  path <- file.path(out_dir, paste0(slug, ".rtf"))
  # Thread the study theme so the sync export resolves study-level defaults
  # identically to the screen and the async daemon (fct_async.R). Stamp the
  # running-band chrome tokens ({datetime}, study meta) to literals first --
  # the engine rejects them, so without this the render throws and the output
  # is silently dropped from the package.
  theme <- .with_band_chrome(store$rv$report@theme, object)
  tryCatch(
    {
      if (.is_figure_type(object@type)) {
        arpillar::render_figure_rtf(store$con, object, path, theme = theme)
      } else {
        ard <- cached_ard(store, object)
        arpillar::render_rtf(ard, object, path, theme = theme)
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
    number = object@options[["number"]] %||% NA_character_,
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
#' manifest). Either way the outputs/ filenames are the
#' `arpillar::output_slug()` slugs, so the manifest linkage is identical.
#'
#' `report` lets the export click hand in its render-prepared copy
#' (`.report_for_export()`) so outputs/, programs/ (emit_code embeds
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
  slugs <- arpillar::output_slugs(report)
  rows <- list()
  ready <- character(0)
  skipped <- character(0)

  for (obj in objects) {
    status <- arpillar::output_status(obj)
    slug <- slugs[[obj@id]]
    # Every output gets its reproduction program, ready or not -- the
    # program IS the record, and a draft's program documents intent.
    prog <- tryCatch(
      arpillar::emit_code(
        store$con,
        obj,
        path = file.path(prog_dir, paste0(slug, ".R")),
        theme = report@theme
      ),
      error = function(e) NULL
    )
    # Sync leg renders here; async leg trusts the daemon's already-written
    # file (present in `rendered` -> it rendered ok).
    render_ok <- if (is.null(rendered)) {
      identical(status, "ready") &&
        .export_render_one(store, obj, out_dir, slug)
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
      path = file.path(prog_dir, "run-all.R"),
      theme = report@theme
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

#' Sync this pass's rendered RTFs into the project's PERSISTENT output dir
#' (Setup > Paths `output_rtf_dir`, default `./output/` under the project
#' root -- resolved exactly like `.emit_programs()` resolves programs_dir),
#' then prune `*.rtf` there against the EXPECTED name set: every CURRENT
#' output's slug, ready or not, never "what this pass rendered". So a
#' per-output render failure, or an output flipping ready -> draft/error,
#' keeps its last-known-good RTF; only a renamed-away or deleted output
#' loses its file (the Task-4 `.emit_programs` prune semantics). `files` is
#' the daemon's `id -> path` result; each is copied in (overwrite). A
#' project-less session (`store$rv$path` NULL) is a silent no-op -- there
#' is no persistent dir to sync.
#' @noRd
.sync_output_dir <- function(store, files) {
  if (is.null(store$rv$path)) {
    return(invisible(NULL))
  }
  paths <- store$rv$report@theme$paths %||% list()
  out_dir <- .path_or_default(paths$output_rtf_dir, "./output/")
  if (!.is_absolute_path(out_dir)) {
    out_dir <- file.path(store$rv$path, sub("^\\./", "", out_dir))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (p in unlist(files)) {
    if (is.character(p) && file.exists(p)) {
      file.copy(p, file.path(out_dir, basename(p)), overwrite = TRUE)
    }
  }
  slugs <- arpillar::output_slugs(store$rv$report)
  expected <- paste0(unname(slugs), ".rtf")
  stale <- setdiff(list.files(out_dir, pattern = "\\.rtf$"), expected)
  unlink(file.path(out_dir, stale))
  invisible(out_dir)
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
