# Team roster: `.arframe/team.json`. A shared, editable list of members
# (name, email, initials, colour). Auto-seeded from the current user on
# first save so a fresh project always has one entry.

.TEAM_FILE <- ".arframe/team.json"

#' A filesystem-safe slug from a user identifier. Lowercased, non-alnum
#' collapsed to `-`, trimmed. `"Alice Smith"` -> `"alice-smith"`.
#' @noRd
.team_slug <- function(user) {
  if (!is.character(user) || length(user) == 0L || !nzchar(user)) {
    return("unknown")
  }
  s <- tolower(user[[1L]])
  s <- gsub("[^a-z0-9]+", "-", s)
  s <- gsub("^-|-$", "", s)
  if (!nzchar(s)) "unknown" else s
}

#' Is `user` a generic OS / CI account we should never emit activity for?
#'
#' Prevents anonymous churn on shared boxes and CI runners. Users with
#' generic names see a "Set your name" banner in Setup > Team; presence
#' and activity stay silent until they configure a real identity.
#' @noRd
.user_is_generic <- function(user) {
  if (!is.character(user) || length(user) == 0L) {
    return(TRUE)
  }
  s <- tolower(user[[1L]])
  if (!nzchar(s)) {
    return(TRUE)
  }
  s %in% c("root", "www-data", "runner", "user", "unknown", "shiny")
}

#' Read the team roster. Returns a list of members; NULL for a NULL / new
#' project. Malformed JSON is treated as empty (never fatal).
#' @noRd
.read_team <- function(project_dir) {
  if (is.null(project_dir) || !nzchar(project_dir)) {
    return(list())
  }
  path <- file.path(project_dir, .TEAM_FILE)
  if (!file.exists(path)) {
    return(list())
  }
  data <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) list()
  )
  data$members %||% list()
}

#' Persist the team roster atomically.
#' @noRd
.write_team <- function(project_dir, members) {
  if (is.null(project_dir) || !nzchar(project_dir)) {
    return(invisible(NULL))
  }
  base <- file.path(project_dir, ".arframe")
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(project_dir, .TEAM_FILE)
  tmp <- paste0(path, ".tmp")
  writeLines(
    jsonlite::toJSON(list(members = members), auto_unbox = TRUE, null = "null"),
    tmp
  )
  file.rename(tmp, path)
  invisible(path)
}

#' Ensure the current user has an entry in `.arframe/team.json`.
#'
#' Called on every save so the first save of a fresh project seeds the
#' roster with the current user, and joining a project from a new machine
#' adds an entry without ceremony. No-op when the effective user is
#' generic.
#' @noRd
.ensure_team_member <- function(project_dir, user) {
  if (is.null(project_dir) || !nzchar(project_dir)) {
    return(invisible(NULL))
  }
  if (.user_is_generic(user)) {
    return(invisible(NULL))
  }
  members <- .read_team(project_dir)
  slugs <- vapply(members, function(m) .team_slug(m$name %||% ""), character(1))
  if (.team_slug(user) %in% slugs) {
    return(invisible(NULL))
  }
  members <- c(
    members,
    list(list(
      name = user,
      slug = .team_slug(user),
      joined = .now_iso()
    ))
  )
  .write_team(project_dir, members)
  invisible(NULL)
}
