# Project-folder workspace layer. The folder IS the format (design decision
# 2026-07-06): setup.yml + outputs/*.json + manifest.csv. arframe wires the
# store's `path`/`dirty`/`saved_at` fields; the load-bearing invariants are
# atomic writes (delegated to arpillar::object_to_json) and no orphan deletion
# (a colleague's output that isn't in our in-memory report survives).

# ---- open_project --------------------------------------------------------

#' Open a project folder and seed the store from it
#'
#' Reads the folder's `setup.yml` into `report@theme`, loads every
#' `outputs/*.json` into the report tree, mounts any parquet the theme's
#' `data.adam_dir` points at, and records mtimes for the refresh scan.
#'
#' Sets `store$rv$path <- dir`, `store$rv$dirty <- FALSE`,
#' `store$rv$saved_at <- Sys.time()`.
#'
#' @noRd
open_project <- function(store, dir) {
  dir <- normalizePath(dir, mustWork = TRUE)
  report <- arpillar::report_from_folder(dir)
  store$rv$report <- report
  store$rv$path <- dir
  store$rv$dirty <- FALSE
  store$rv$saved_at <- Sys.time()
  # Mount ADaM data when the theme names it.
  adam_dir <- report@theme$data$adam_dir
  if (is.character(adam_dir) && length(adam_dir) == 1L && nzchar(adam_dir)) {
    resolved <- if (utils::file_test("-d", adam_dir)) {
      adam_dir
    } else {
      file.path(dir, adam_dir)
    }
    if (utils::file_test("-d", resolved)) {
      .mount_folder(store, resolved, folder = "adam")
    }
  }
  # Record output mtimes for scan_and_merge.
  .refresh_mtimes(store)
  log_line(store, sprintf("opened project %s", basename(dir)))
  invisible(dir)
}

#' Create a new project from an existing one
#'
#' Copies ONLY `setup.yml` from `existing_dir` into `target_dir`; the outputs
#' folder starts empty. `_meta.created_at` and `_meta.reviewed` in the copied
#' theme are cleared and re-stamped for the new user.
#' @noRd
new_project_from <- function(store, existing_dir, target_dir) {
  existing <- normalizePath(existing_dir, mustWork = TRUE)
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)

  src_yml <- file.path(existing, "setup.yml")
  if (file.exists(src_yml)) {
    theme <- arpillar::theme_from_yaml(src_yml)
    # Restamp meta for the new project.
    if (is.list(theme[["_meta"]])) {
      theme[["_meta"]] <- NULL
    }
    arpillar::theme_to_yaml(theme, file.path(target_dir, "setup.yml"))
  }
  dir.create(file.path(target_dir, "outputs"), showWarnings = FALSE)
  open_project(store, target_dir)
}

# ---- save_touched --------------------------------------------------------

#' Save any dirty state to the project folder
#'
#' Called by the auto-save observer (or a manual Save action). Rewrites
#' `setup.yml` + every `outputs/*.json` via the arpillar folder codec, then
#' updates `saved_at` and clears `dirty`. Skipped when no path is set.
#' @noRd
save_touched <- function(store) {
  if (is.null(store$rv$path)) {
    return(invisible(NULL))
  }
  # Ponytail: v1 rewrites the whole folder on every save -- simple, correct,
  # relies on arpillar's atomic per-file writes so no reader sees a partial
  # state. A later stage can diff and write only the changed output.
  arpillar::report_to_folder(store$rv$report, store$rv$path)
  # Emit per-output .R programs + a run-all.R (SAS-pharma convention: every
  # output has a program; a teammate can reproduce the package from the CLI
  # with `Rscript programs/run-all.R`). Path comes from setup.yml's
  # `paths.programs_dir` block, defaulting to `./programs/` relative to the
  # project root.
  .emit_programs(store)
  # One activity line per debounce batch, and a team-roster ensure so the
  # first save of a fresh project seeds `.arframe/team.json`.
  user <- .who_am_i()
  ids <- vapply(.all_objects(store$rv$report), function(o) o@id, character(1))
  tryCatch(
    .log_activity(store$rv$path, user, "edited", ids),
    error = function(e) NULL
  )
  tryCatch(
    .ensure_team_member(store$rv$path, user),
    error = function(e) NULL
  )
  store$rv$dirty <- FALSE
  store$rv$saved_at <- Sys.time()
  .refresh_mtimes(store)
  invisible(NULL)
}

#' Emit `programs/<id>.R` per output and a `programs/run-all.R` that runs
#' them in numbering order. Path from `theme$paths$programs_dir`; default
#' `./programs/` relative to the project root. Absolute paths pass through.
#' Errors are swallowed per-output so one failing generator does not block
#' the save.
#' @noRd
.emit_programs <- function(store) {
  if (is.null(store$rv$path)) {
    return(invisible(NULL))
  }
  paths <- store$rv$report@theme$paths %||% list()
  prog_dir <- paths$programs_dir %||% "./programs/"
  if (!.is_absolute_path(prog_dir)) {
    prog_dir <- file.path(store$rv$path, sub("^\\./", "", prog_dir))
  }
  dir.create(prog_dir, recursive = TRUE, showWarnings = FALSE)
  # Thread the study theme so the emitted programs reproduce the population
  # filter, summaries, decimals, and chrome -- not just the roles/dataset.
  theme <- store$rv$report@theme
  objs <- .all_objects(store$rv$report)
  for (obj in objs) {
    out <- file.path(prog_dir, paste0(obj@id, ".R"))
    tryCatch(
      arpillar::emit_code(store$con, obj, path = out, theme = theme),
      error = function(e) NULL
    )
  }
  tryCatch(
    arpillar::emit_report_code(
      store$con,
      store$rv$report,
      path = file.path(prog_dir, "run-all.R"),
      theme = theme
    ),
    error = function(e) NULL
  )
  invisible(prog_dir)
}

#' TRUE for POSIX absolute paths (`/`), Windows drive paths (`C:\`), and
#' UNC paths (`\\server`). Used to decide whether a Setup > Paths entry
#' resolves against the project root.
#' @noRd
.is_absolute_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    return(FALSE)
  }
  grepl("^([/~]|[A-Za-z]:[\\\\/]|\\\\\\\\)", path)
}

#' The one refresh path (Stage 3 consolidation).
#'
#' Rescans on-disk outputs (`scan_and_merge()`), garbage-collects stale
#' presence files, and bumps `catalog_nonce` so every reactive that
#' watches team state (the Setup > Team feed, the Sources list) picks up
#' the change. Debounced upstream by the caller -- both the manual
#' refresh button and the tab-focus event route through this helper so
#' there is one place to add or reorder steps.
#' @noRd
.refresh_all <- function(store) {
  scan_and_merge(store)
  tryCatch(.gc_presence(store$rv$path), error = function(e) NULL)
  store$rv$catalog_nonce <- store$rv$catalog_nonce + 1L
  invisible(NULL)
}

#' Refresh `store$mtimes` from the outputs/ folder
#' @noRd
.refresh_mtimes <- function(store) {
  if (is.null(store$mtimes)) {
    store$mtimes <- new.env(parent = emptyenv())
  } else {
    rm(list = ls(store$mtimes), envir = store$mtimes)
  }
  if (is.null(store$rv$path)) {
    return(invisible(NULL))
  }
  outputs_dir <- file.path(store$rv$path, "outputs")
  if (!dir.exists(outputs_dir)) {
    return(invisible(NULL))
  }
  files <- list.files(
    outputs_dir,
    pattern = "\\.json$",
    full.names = TRUE
  )
  for (f in files) {
    info <- file.info(f)
    if (!is.na(info$mtime)) {
      store$mtimes[[basename(f)]] <- as.numeric(info$mtime)
    }
  }
  invisible(NULL)
}

# ---- scan_and_merge (refresh handler) ------------------------------------

#' Rescan the outputs folder and pull in changes from other users
#'
#' Called on tab-focus (via the `visibilitychange` bridge) or by a manual
#' Refresh button. Compares current mtimes on disk against `store$mtimes` and
#' reloads any file whose mtime moved forward. `.tmp` siblings from an in-flight
#' write are ignored.
#'
#' Newly-appeared outputs (files present on disk but absent from our
#' in-memory report) are added to the first page. Deleted outputs (in mtimes
#' but no longer on disk) log a soft banner; the object is left in the report
#' until the user confirms removal.
#' @noRd
scan_and_merge <- function(store) {
  if (is.null(store$rv$path)) {
    return(invisible(NULL))
  }
  outputs_dir <- file.path(store$rv$path, "outputs")
  if (!dir.exists(outputs_dir)) {
    return(invisible(NULL))
  }
  disk_files <- list.files(
    outputs_dir,
    pattern = "\\.json$",
    full.names = TRUE
  )
  known <- if (is.null(store$mtimes)) character(0) else ls(store$mtimes)
  seen_on_disk <- basename(disk_files)

  changed_ids <- character(0)
  new_objs <- list()
  for (f in disk_files) {
    fname <- basename(f)
    info <- file.info(f)
    disk_mtime <- as.numeric(info$mtime)
    old_mtime <- store$mtimes[[fname]] %||% -Inf
    if (disk_mtime > old_mtime) {
      obj <- tryCatch(
        arpillar::object_from_json(f),
        error = function(e) NULL
      )
      if (!is.null(obj)) {
        new_objs[[obj@id]] <- obj
        changed_ids <- c(changed_ids, obj@id)
      }
      store$mtimes[[fname]] <- disk_mtime
    }
  }

  # Files vanished from disk since we last scanned.
  vanished <- setdiff(known, seen_on_disk)
  for (v in vanished) {
    rm(list = v, envir = store$mtimes)
  }
  if (length(vanished) > 0L) {
    log_line(
      store,
      sprintf("%d output file(s) removed from disk", length(vanished))
    )
  }

  # Merge any changed objects into the in-memory report.
  if (length(new_objs) > 0L) {
    r <- store$rv$report
    existing_ids <- vapply(
      .all_objects(r),
      function(o) o@id,
      character(1)
    )
    for (id in changed_ids) {
      obj <- new_objs[[id]]
      if (id %in% existing_ids) {
        r <- .replace_object(r, id, obj)
      } else {
        pages <- r@pages
        first <- pages[[1L]]
        pages[[1L]] <- S7::set_props(
          first,
          objects = c(first@objects, list(obj))
        )
        r <- S7::set_props(r, pages = pages)
      }
    }
    store$rv$report <- r
    log_line(
      store,
      sprintf("refreshed %d output(s) from disk", length(new_objs))
    )
  }
  invisible(NULL)
}
