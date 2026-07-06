# User identity + audit-trail meta stamping. No auth, no accounts -- the OS
# username plus an optional ARFRAME_USER env var covers every real-world case
# a folder-shared workspace needs (OneDrive / SharePoint / SAN mount).

#' Resolve the current user's short id
#'
#' Priority: `ARFRAME_USER` env var if set; else `Sys.info()[["user"]]` (the
#' OS login name); else `"unknown"` as a last-resort placeholder.
#' @noRd
.who_am_i <- function() {
  env <- Sys.getenv("ARFRAME_USER", unset = "")
  if (nzchar(env)) {
    return(env)
  }
  osu <- tryCatch(Sys.info()[["user"]], error = function(e) "")
  if (is.character(osu) && length(osu) == 1L && nzchar(osu)) {
    return(osu)
  }
  "unknown"
}

#' Current UTC ISO 8601 timestamp
#' @noRd
.now_iso <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Stamp an audit-trail action on an S7 object's `@meta` slot
#'
#' `action` is one of `"create"` (sets created_by/at AND modified_by/at),
#' `"modify"` (sets modified_by/at, clears any prior `reviewed` block --
#' every edit invalidates a QC signature), or `"review"` (sets reviewed = {by,
#' at, notes}). Returns a NEW object via `S7::set_props()`; the caller commits
#' it into the store.
#'
#' `action = "unreview"` clears the review stamp without touching modified_at
#' (used when the user unchecks the Reviewed toggle without editing anything).
#'
#' @noRd
.stamp_meta <- function(object, action = c("create", "modify", "review", "unreview"), notes = NULL) {
  action <- match.arg(action)
  meta <- object@meta
  if (!is.list(meta)) meta <- list()
  now <- .now_iso()
  me <- .who_am_i()

  if (action == "create") {
    meta$created_at <- now
    meta$created_by <- me
    meta$modified_at <- now
    meta$modified_by <- me
  } else if (action == "modify") {
    if (is.null(meta$created_at)) {
      meta$created_at <- now
      meta$created_by <- me
    }
    meta$modified_at <- now
    meta$modified_by <- me
    # Every edit clears the QC signature; the review can be redone.
    meta$reviewed <- NULL
  } else if (action == "review") {
    r <- list(by = me, at = now)
    if (!is.null(notes) && is.character(notes) && nzchar(notes)) {
      r$notes <- notes
    }
    meta$reviewed <- r
  } else {
    # unreview
    meta$reviewed <- NULL
  }
  S7::set_props(object, meta = meta)
}
