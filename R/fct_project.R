# Project-folder workspace layer. The folder IS the format:
# setup.yml + outputs/*.json + manifest.csv. arframe wires the
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
  # Mount ADaM data the theme names — shared with the arframe(project=)
  # startup so both entry points populate the catalog identically.
  .mount_theme_adam(store, report, dir)
  # Record output mtimes for scan_and_merge.
  .refresh_mtimes(store)
  log_line(store, sprintf("opened project %s", basename(dir)))
  invisible(dir)
}

#' Mount the theme's ADaM folder into the catalog when it names a valid
#' directory. Shared by `open_project()` and the `arframe(project=)` startup so
#' a launched project populates the catalog exactly like the Open button.
#' `base_dir` resolves a relative `adam_dir`; an absent or invalid path is
#' silent (a data-source hint must never abort a launch).
#' @noRd
.mount_theme_adam <- function(store, report, base_dir) {
  adam_dir <- report@theme$data$adam_dir
  if (!is.character(adam_dir) || length(adam_dir) != 1L || !nzchar(adam_dir)) {
    return(invisible())
  }
  resolved <- if (utils::file_test("-d", adam_dir)) {
    adam_dir
  } else {
    file.path(base_dir, adam_dir)
  }
  if (utils::file_test("-d", resolved)) {
    tryCatch(
      .mount_folder(store, resolved, folder = "adam"),
      error = function(e) NULL
    )
  }
  invisible()
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
  # Ponytail: v1 rewrites the whole folder on every save — simple, correct,
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

#' Emit `programs/<slug>.R` per output and a `programs/run-all.R` that runs
#' them in numbering order. Path from `theme$paths$programs_dir`; default
#' `./programs/` relative to the project root. Absolute paths pass through.
#' Errors are swallowed per-output so one failing generator does not block
#' the save; a failing output KEEPS its last-known-good program. Only files
#' no current output claims (renamed/removed since the last emit) are
#' pruned; `run-all.R` never is.
#' @noRd
.emit_programs <- function(store) {
  if (is.null(store$rv$path)) {
    return(invisible(NULL))
  }
  paths <- store$rv$report@theme$paths %||% list()
  prog_dir <- .path_or_default(paths$programs_dir, "./programs/")
  if (!.is_absolute_path(prog_dir)) {
    prog_dir <- file.path(store$rv$path, sub("^\\./", "", prog_dir))
  }
  dir.create(prog_dir, recursive = TRUE, showWarnings = FALSE)
  # Thread the study theme so the emitted programs reproduce the population
  # filter, summaries, decimals, and chrome — not just the roles/dataset.
  theme <- store$rv$report@theme
  objs <- .all_objects(store$rv$report)
  # Slug filenames, not `<id>.R` — must match the `{program}` chrome token
  # (`.study_tokens()` in mod_paper.R) so a printed program path resolves to
  # a real file on disk.
  slugs <- arpillar::output_slugs(store$rv$report)
  for (obj in objs) {
    out <- file.path(prog_dir, paste0(slugs[[obj@id]], ".R"))
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
  # Prune stale programs — a rename/re-slug leaves the OLD file behind
  # otherwise. Prune against the EXPECTED set: every current output's slug
  # PLUS every surviving outputs/*.json basename on disk — the spec GC is
  # ownership-scoped (a concurrent teammate's spec survives our save), so
  # its surviving basenames are exactly the programs that must survive too
  # (review finding: pruning against our slugs alone deleted teammates'
  # programs). Never against what this pass managed to write: a failed emit
  # for a still-present output keeps its last-known-good program ("the
  # program IS the record"). run-all.R never prunes.
  outputs_dir <- file.path(store$rv$path, "outputs")
  disk_specs <- sub(
    "\\.json$",
    "",
    list.files(outputs_dir, pattern = "\\.json$")
  )
  expected <- c(
    paste0(unname(slugs), ".R"),
    paste0(disk_specs, ".R"),
    "run-all.R"
  )
  stale <- setdiff(list.files(prog_dir, pattern = "\\.R$"), expected)
  unlink(file.path(prog_dir, stale))
  invisible(prog_dir)
}

#' Unlink the on-disk spec + program files of DELETED outputs. The save-time
#' GC is ownership-scoped (it never removes an id it does not know — that
#' file may be a concurrent teammate's), so deletion cleans its own files
#' here: scan outputs/*.json for the deleted ids, drop each match plus its
#' same-basename programs/<base>.R. Undo-safe: Cmd-Z restores the object in
#' the store and the next autosave re-emits both files. An unreadable spec
#' is left alone (never delete on a parse error).
#' @noRd
.unlink_output_files <- function(store, ids) {
  root <- store$rv$path
  if (is.null(root) || length(ids) == 0L) {
    return(invisible(NULL))
  }
  outputs_dir <- file.path(root, "outputs")
  paths <- store$rv$report@theme$paths %||% list()
  prog_dir <- .path_or_default(paths$programs_dir, "./programs/")
  if (!.is_absolute_path(prog_dir)) {
    prog_dir <- file.path(root, sub("^\\./", "", prog_dir))
  }
  for (f in list.files(outputs_dir, pattern = "\\.json$")) {
    fid <- tryCatch(
      {
        j <- jsonlite::read_json(file.path(outputs_dir, f))
        # The spec envelope nests the object (`$object$id`); fall back to a
        # bare top-level id for any pre-envelope file.
        as.character(j$object$id %||% j$id)
      },
      error = function(e) character(0)
    )
    if (length(fid) == 1L && fid %in% ids) {
      unlink(file.path(outputs_dir, f))
      unlink(file.path(prog_dir, sub("\\.json$", ".R", f)))
    }
  }
  invisible(NULL)
}

#' A Setup > Paths entry, or a default when it is unset OR blank.
#'
#' Setup > Paths serialises an empty field as `""` (not NULL), so
#' `paths$x %||% default` keeps the blank and resolves it against the
#' project root — spilling programs / renders into the root instead of
#' `programs/` / `output/`. Treat a blank string as unset.
#' @noRd
.path_or_default <- function(path, default) {
  if (is.null(path) || !is.character(path) || !nzchar(trimws(path[[1]]))) {
    default
  } else {
    path
  }
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
#' presence files, and — when there is anything to pick up — bumps
#' `catalog_nonce` so every reactive that watches team state (the Setup >
#' Team feed, the Sources list) sees the change. Debounced upstream by the
#' caller — both the manual refresh button and the tab-focus event route
#' through this helper so there is one place to add or reorder steps.
#' @noRd
.refresh_all <- function(store) {
  changed <- isTRUE(scan_and_merge(store))
  tryCatch(.gc_presence(store$rv$path), error = function(e) NULL)
  # Bump only when something can actually be new: the nonce re-renders every
  # catalog-gated surface at once (all of Setup's section cards, the LoC,
  # Data), so an unconditional bump made the page visibly blink every time
  # the browser tab regained focus — worst on a fresh no-project launch,
  # where the scan is a guaranteed no-op. With a project bound the bump
  # stays unconditional because it is also what refreshes the Team
  # feed/presence, which scan_and_merge() does not track.
  if (changed || !is.null(store$rv$path)) {
    store$rv$catalog_nonce <- store$rv$catalog_nonce + 1L
  }
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
  if (!is.null(store$fsizes)) {
    rm(list = ls(store$fsizes), envir = store$fsizes)
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
      store$fsizes[[basename(f)]] <- as.numeric(info$size)
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
#'
#' Returns (invisibly) TRUE when the scan merged or dropped anything, FALSE
#' when disk and memory already agreed — `.refresh_all()` uses this to skip
#' the catalog-nonce bump (and so the wholesale re-render) on a no-op scan.
#' @noRd
scan_and_merge <- function(store) {
  if (is.null(store$rv$path)) {
    return(invisible(FALSE))
  }
  outputs_dir <- file.path(store$rv$path, "outputs")
  if (!dir.exists(outputs_dir)) {
    return(invisible(FALSE))
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
    disk_size <- as.numeric(info$size)
    old_mtime <- store$mtimes[[fname]] %||% -Inf
    old_size <- store$fsizes[[fname]] %||% NA_real_
    # `>` alone misses a write landing within the filesystem's timestamp
    # granularity (review finding); a same-tick write almost always moves
    # the byte size, so compare both.
    changed <- disk_mtime > old_mtime ||
      (disk_mtime == old_mtime && !identical(disk_size, old_size))
    if (changed) {
      obj <- tryCatch(
        arpillar::object_from_json(f),
        error = function(e) NULL
      )
      if (!is.null(obj)) {
        new_objs[[obj@id]] <- obj
        changed_ids <- c(changed_ids, obj@id)
      }
      store$mtimes[[fname]] <- disk_mtime
      store$fsizes[[fname]] <- disk_size
    }
  }

  # Files vanished from disk since we last scanned.
  vanished <- setdiff(known, seen_on_disk)
  for (v in vanished) {
    rm(list = v, envir = store$mtimes)
    if (
      !is.null(store$fsizes) &&
        exists(v, envir = store$fsizes, inherits = FALSE)
    ) {
      rm(list = v, envir = store$fsizes)
    }
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
  invisible(length(new_objs) > 0L || length(vanished) > 0L)
}
