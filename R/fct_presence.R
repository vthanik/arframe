# Team presence: per-user JSON heartbeat under `.arframe/presence/`. Each
# session writes ONLY its own file. Readers filter by mtime so a crashed
# session's file drops off within 60s without an explicit unregister.

.PRESENCE_SUBDIR <- ".arframe/presence"

#' Absolute path to the per-user presence file inside a project.
#' Creates the parent folder on demand. Returns NULL for a NULL project.
#' @noRd
.presence_path <- function(project_dir, user) {
  if (is.null(project_dir) || !nzchar(project_dir)) {
    return(NULL)
  }
  base <- file.path(project_dir, .PRESENCE_SUBDIR)
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  file.path(base, paste0(.team_slug(user), ".json"))
}

#' Write / touch this user's presence file.
#'
#' Rewrites the JSON body only when `mode` or `current_output` changed
#' since the last heartbeat; otherwise touches the file's mtime via
#' `Sys.setFileTime()`. Small file writes on network drives (Dropbox,
#' SMB) trigger sync notifications, so touching mtime is the cheap path
#' for the common no-change case.
#'
#' Silently skips when the effective user is generic so an anonymous
#' session cannot appear in the presence rail.
#' @noRd
.heartbeat <- function(project_dir, user, mode = NULL, current_output = NULL) {
  if (is.null(project_dir) || !nzchar(project_dir)) {
    return(invisible(NULL))
  }
  if (.user_is_generic(user)) {
    return(invisible(NULL))
  }
  path <- .presence_path(project_dir, user)
  if (is.null(path)) {
    return(invisible(NULL))
  }
  body <- list(
    ts = .now_iso(),
    user = user,
    mode = mode %||% "",
    current_output = current_output %||% ""
  )
  changed <- TRUE
  if (file.exists(path)) {
    prev <- tryCatch(
      jsonlite::fromJSON(path, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.null(prev)) {
      changed <- !identical(prev$mode %||% "", body$mode) ||
        !identical(prev$current_output %||% "", body$current_output)
    }
  }
  if (changed) {
    tmp <- paste0(path, ".tmp")
    writeLines(
      jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
      tmp
    )
    file.rename(tmp, path)
  } else {
    tryCatch(Sys.setFileTime(path, Sys.time()), error = function(e) NULL)
  }
  invisible(path)
}

#' Read every fresh presence entry (mtime within `since_s`).
#'
#' Stale files from crashed sessions are ignored automatically. Malformed
#' JSON is skipped, not fatal.
#' @noRd
.presence_list <- function(project_dir, since_s = 60L) {
  if (is.null(project_dir) || !nzchar(project_dir)) {
    return(list())
  }
  base <- file.path(project_dir, .PRESENCE_SUBDIR)
  if (!dir.exists(base)) {
    return(list())
  }
  files <- list.files(base, pattern = "\\.json$", full.names = TRUE)
  if (length(files) == 0L) {
    return(list())
  }
  info <- file.info(files)
  cutoff <- as.numeric(Sys.time()) - as.numeric(since_s)
  fresh <- files[as.numeric(info$mtime) >= cutoff]
  events <- lapply(fresh, function(p) {
    tryCatch(
      jsonlite::fromJSON(p, simplifyVector = FALSE),
      error = function(e) NULL
    )
  })
  events[!vapply(events, is.null, logical(1))]
}

#' Garbage-collect presence files older than `max_age_s`. Cheap to call on
#' every refresh; a no-op when the folder does not exist.
#' @noRd
.gc_presence <- function(project_dir, max_age_s = 86400L) {
  if (is.null(project_dir) || !nzchar(project_dir)) {
    return(invisible(NULL))
  }
  base <- file.path(project_dir, .PRESENCE_SUBDIR)
  if (!dir.exists(base)) {
    return(invisible(NULL))
  }
  files <- list.files(base, pattern = "\\.json$", full.names = TRUE)
  if (length(files) == 0L) {
    return(invisible(NULL))
  }
  info <- file.info(files)
  cutoff <- as.numeric(Sys.time()) - as.numeric(max_age_s)
  stale <- files[as.numeric(info$mtime) < cutoff]
  for (f in stale) {
    tryCatch(file.remove(f), error = function(e) NULL)
  }
  invisible(NULL)
}
