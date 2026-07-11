# Team activity log: append-only, per-user JSONL file. Concurrency: each
# session writes ONLY to its own `.arframe/activity/<slug>.jsonl` — no
# shared file, no flock, no platform branching. Readers glob the folder
# and merge on read.

.ACTIVITY_SUBDIR <- ".arframe/activity"

#' Absolute path to the per-user activity file inside a project.
#' Creates the parent folder on demand. Returns NULL for a NULL project.
#' @noRd
.activity_path <- function(project_dir, user) {
  if (is.null(project_dir) || !nzchar(project_dir)) {
    return(NULL)
  }
  base <- file.path(project_dir, .ACTIVITY_SUBDIR)
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  file.path(base, paste0(.team_slug(user), ".jsonl"))
}

#' Append one activity line to the current user's per-user file.
#'
#' `targets` is a character vector of output ids touched in the debounce
#' batch. A single line captures the batch so the feed stays dense with
#' signal rather than one line per touched object.
#'
#' Silently skips when the effective user is generic (see
#' `.user_is_generic()`) so an anonymous session cannot churn the feed.
#' @noRd
.log_activity <- function(project_dir, user, action, targets = character(0)) {
  if (is.null(project_dir) || !nzchar(project_dir)) {
    return(invisible(NULL))
  }
  if (.user_is_generic(user)) {
    return(invisible(NULL))
  }
  path <- .activity_path(project_dir, user)
  if (is.null(path)) {
    return(invisible(NULL))
  }
  entry <- list(
    ts = .now_iso(),
    user = user,
    action = as.character(action),
    targets = as.character(targets)
  )
  line <- jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null")
  # Append is buffered but atomic per-write on POSIX for lines below PIPE_BUF
  # (~4KB); our lines are far smaller. Per-user files guarantee no
  # cross-writer interleaving anyway.
  con <- file(path, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(line, con)
  invisible(path)
}

#' Read the most recent activity events across every user in the project.
#'
#' Globs `.arframe/activity/*.jsonl`, parses one entry per line, sorts by
#' `ts` descending, keeps the top `tail_n`. Malformed lines are silently
#' skipped (a rotated / half-written file cannot break the reader).
#' @noRd
.read_activity <- function(project_dir, tail_n = 50L) {
  if (is.null(project_dir) || !nzchar(project_dir)) {
    return(list())
  }
  base <- file.path(project_dir, .ACTIVITY_SUBDIR)
  if (!dir.exists(base)) {
    return(list())
  }
  files <- list.files(base, pattern = "\\.jsonl$", full.names = TRUE)
  events <- unlist(lapply(files, .read_activity_file), recursive = FALSE)
  if (length(events) == 0L) {
    return(list())
  }
  ts <- vapply(events, function(e) e$ts %||% "", character(1))
  ord <- order(ts, decreasing = TRUE)
  events <- events[ord]
  events[seq_len(min(length(events), as.integer(tail_n)))]
}

#' Parse one JSONL activity file; skip malformed lines.
#' @noRd
.read_activity_file <- function(path) {
  raw <- tryCatch(readLines(path, warn = FALSE), error = function(e) {
    character(0)
  })
  raw <- raw[nzchar(raw)]
  if (length(raw) == 0L) {
    return(list())
  }
  parsed <- lapply(raw, function(ln) {
    tryCatch(
      jsonlite::fromJSON(ln, simplifyVector = FALSE),
      error = function(e) NULL
    )
  })
  parsed[!vapply(parsed, is.null, logical(1))]
}
