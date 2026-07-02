# Pure walkers over the S7 document tree (report -> page -> object). Every
# function here rebuilds the tree with S7::set_props() -- S7 properties are
# immutable, so a "mutation" is always a brand-new report returned to the
# caller, never an in-place x@prop <- value. fct_store.R's mutators are thin
# wrappers over these plus a commit().

# ---- lookup -----------------------------------------------------------

#' Find one object by id anywhere in the report, or NULL.
#' @noRd
.find_object <- function(report, id) {
  for (pg in report@pages) {
    for (obj in pg@objects) {
      if (identical(obj@id, id)) {
        return(obj)
      }
    }
  }
  NULL
}

#' Every object across every page, in page then within-page order.
#' @noRd
.all_objects <- function(report) {
  unlist(
    lapply(report@pages, function(pg) pg@objects),
    recursive = FALSE
  )
}

# ---- rebuild ------------------------------------------------------------

#' Replace the object with `id` with `obj`, rebuilding the tree.
#' @noRd
.replace_object <- function(report, id, obj) {
  pages <- lapply(report@pages, function(pg) {
    objects <- lapply(pg@objects, function(o) {
      if (identical(o@id, id)) obj else o
    })
    S7::set_props(pg, objects = objects)
  })
  S7::set_props(report, pages = pages)
}

#' Drop the object with `id` from whichever page holds it.
#' @noRd
.remove_object <- function(report, id) {
  pages <- lapply(report@pages, function(pg) {
    objects <- Filter(function(o) !identical(o@id, id), pg@objects)
    S7::set_props(pg, objects = objects)
  })
  S7::set_props(report, pages = pages)
}

#' Move the object with `id` to position `to` within its own page.
#' @noRd
.move_object <- function(report, id, to) {
  pages <- lapply(report@pages, function(pg) {
    idx <- which(vapply(
      pg@objects,
      function(o) identical(o@id, id),
      logical(1)
    ))
    if (length(idx) == 0L) {
      return(pg)
    }
    objects <- pg@objects
    obj <- objects[[idx]]
    rest <- objects[-idx]
    to <- min(max(to, 1L), length(objects))
    objects <- append(rest, list(obj), after = to - 1L)
    S7::set_props(pg, objects = objects)
  })
  S7::set_props(report, pages = pages)
}

# ---- construction ---------------------------------------------------------

#' Build a fresh object from a template entry, bound to `dataset`, with `id`.
#' @noRd
.object_from_template <- function(tpl, dataset, id) {
  arpillar::object(
    id = id,
    type = tpl$type,
    title = tpl$title %||% "",
    dataset = dataset,
    footnotes = as.character(tpl$footnotes %||% character(0))
  )
}

#' The next monotonic object id, `sprintf("out%03d", n)`, over existing ids.
#' @noRd
.next_id <- function(report) {
  ids <- vapply(.all_objects(report), function(o) o@id, character(1))
  nums <- suppressWarnings(as.integer(sub("^out", "", ids)))
  nums <- nums[!is.na(nums)]
  n <- if (length(nums) == 0L) 0L else max(nums)
  sprintf("out%03d", n + 1L)
}

# `%||%` is imported from rlang package-wide (see arframe-package.R).
