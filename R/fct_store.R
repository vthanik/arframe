# The injected structured store (design spec 5.1): one S7 report + galley
# pointers + undo/redo + a two-stage ARD cache. Created once per session and
# handed to every module server; modules communicate ONLY through it, never
# with each other. ALL draft/edit state lives in `store$rv`, never in the
# DOM -- a folded, lazy-mounted, or unmounted pane can never silently drop
# configuration (the suspend-contract regression test proves this end to
# end, with zero Shiny session mounted).

# ---- construction -----------------------------------------------------

#' Create the injected structured store.
#'
#' `con` is the arpillar catalog handle; `report` seeds the live document
#' (defaults to a fresh one-page "Untitled report"). Returns a plain list
#' bundling the catalog, the reactive pointer set every module reads/writes,
#' a plain-environment undo/redo stack, and a plain-environment memo cache
#' (`cached_ard()`'s two-stage seam).
#' @noRd
new_store <- function(con, report = NULL) {
  if (is.null(report)) {
    report <- arpillar::report(
      id = "report1",
      name = "Untitled report",
      pages = list(arpillar::page(id = "p1"))
    )
  }
  undo <- new.env(parent = emptyenv())
  undo$stack <- list()
  undo$redo <- list()
  list(
    con = con,
    rv = shiny::reactiveValues(
      report = report,
      selected = NULL,
      region = NULL,
      card = FALSE,
      pinned = FALSE,
      mode = "report",
      dataset = NULL,
      bridge_dataset = NULL,
      adding = FALSE,
      filter_draft = list(),
      path = NULL,
      dirty = FALSE,
      saved_at = NULL,
      broken = character(0),
      log = character(0),
      catalog_nonce = 0L,
      rail_collapsed = FALSE,
      insp_collapsed = FALSE
    ),
    undo = undo,
    cache = new.env(parent = emptyenv())
  )
}

# ---- commit / undo / redo ----------------------------------------------

#' Commit a new report to the store, pushing the prior one onto the undo
#' stack (capped at 50) and clearing the redo stack.
#' @noRd
commit <- function(store, new_report, label = "") {
  store$undo$stack <- c(store$undo$stack, list(store$rv$report))
  n <- length(store$undo$stack)
  if (n > 50L) {
    store$undo$stack <- store$undo$stack[(n - 49L):n]
  }
  store$undo$redo <- list()
  store$rv$report <- new_report
  store$rv$dirty <- TRUE
  invisible(new_report)
}

#' Pop the last undo entry, pushing the current report onto redo.
#' @noRd
undo <- function(store) {
  n <- length(store$undo$stack)
  if (n == 0L) {
    return(invisible(NULL))
  }
  store$undo$redo <- c(store$undo$redo, list(store$rv$report))
  prior <- store$undo$stack[[n]]
  store$undo$stack <- store$undo$stack[-n]
  store$rv$report <- prior
  store$rv$dirty <- TRUE
  invisible(prior)
}

#' Pop the last redo entry, pushing the current report back onto undo.
#' @noRd
redo <- function(store) {
  n <- length(store$undo$redo)
  if (n == 0L) {
    return(invisible(NULL))
  }
  store$undo$stack <- c(store$undo$stack, list(store$rv$report))
  next_report <- store$undo$redo[[n]]
  store$undo$redo <- store$undo$redo[-n]
  store$rv$report <- next_report
  store$rv$dirty <- TRUE
  invisible(next_report)
}

#' Is there an undo entry to pop?
#' @noRd
can_undo <- function(store) {
  length(store$undo$stack) > 0L
}

#' Is there a redo entry to pop?
#' @noRd
can_redo <- function(store) {
  length(store$undo$redo) > 0L
}

# ---- selection / editing ------------------------------------------------

#' The currently selected object, or NULL when nothing is selected.
#' @noRd
selected_object <- function(store) {
  if (is.null(store$rv$selected)) {
    return(NULL)
  }
  .find_object(store$rv$report, store$rv$selected)
}

#' Apply `fn(object) -> object` to the object with `id`, then commit the
#' rebuilt report. Siblings are untouched.
#' @noRd
update_object <- function(store, id, fn, label = "") {
  obj <- .find_object(store$rv$report, id)
  if (is.null(obj)) {
    return(invisible(NULL))
  }
  new_obj <- fn(obj)
  new_report <- .replace_object(store$rv$report, id, new_obj)
  commit(store, new_report, label = label)
}

#' Append `obj` to the first page, commit, and select it. Shared tail of
#' `add_from_preset()`/`add_from_generator()`.
#' @noRd
.append_and_select <- function(store, obj, label) {
  pages <- store$rv$report@pages
  first <- pages[[1]]
  pages[[1]] <- S7::set_props(first, objects = c(first@objects, list(obj)))
  new_report <- S7::set_props(store$rv$report, pages = pages)
  commit(store, new_report, label = label)
  store$rv$selected <- obj@id
  obj@id
}

#' Add a new output pre-filled from a named preset (`arpillar::preset()`),
#' bind it to `dataset`, select it, and return its freshly minted id.
#'
#' `roles` are rebuilt off `dataset`'s own catalog entry, not copied
#' verbatim from the preset -- a preset var absent from `dataset` still
#' gets a `data_item()` (default `role_type = "category"`), it is never
#' silently dropped, so a mismatched dataset (e.g. an AE-domain preset
#' applied to a dataset without `AEDECOD`) surfaces at validate/render
#' time rather than vanishing here.
#' @noRd
add_from_preset <- function(store, preset_id, dataset) {
  pr <- arpillar::preset(preset_id)
  id <- .next_id(store$rv$report)
  obj <- .object_from_preset(store$con, pr, dataset, id)
  .append_and_select(store, obj, label = "add output from preset")
}

#' Add a bare new output from a generator (`arpillar::generator()`), bind
#' it to `dataset`, select it, and return its freshly minted id.
#'
#' No roles are filled in -- the "blank slate" counterpart to
#' `add_from_preset()`. `options$number` is auto-suggested as the next
#' free `<prefix>.<n>` within the generator's kind group (see
#' `.next_number()`); `options$number_label` is the kind's TLF label
#' ("Table"/"Figure"/"Listing").
#' @noRd
add_from_generator <- function(store, generator_id, dataset) {
  gen <- arpillar::generator(generator_id)
  id <- .next_id(store$rv$report)
  existing_numbers <- vapply(
    .all_objects(store$rv$report),
    function(o) o@options$number %||% "",
    character(1)
  )
  obj <- .object_from_generator(gen, dataset, id, existing_numbers)
  .append_and_select(store, obj, label = "add output from generator")
}

#' Remove the output with `id`; clear a now-dangling selection.
#' @noRd
remove_output <- function(store, id) {
  new_report <- .remove_object(store$rv$report, id)
  commit(store, new_report, label = "remove output")
  if (identical(store$rv$selected, id)) {
    store$rv$selected <- NULL
  }
  invisible(NULL)
}

#' Move the output with `id` to position `to` within its page.
#' @noRd
move_output <- function(store, id, to) {
  new_report <- .move_object(store$rv$report, id, to)
  commit(store, new_report, label = "move output")
  invisible(NULL)
}

#' Rename the output with `id`.
#' @noRd
rename_output <- function(store, id, title) {
  update_object(
    store,
    id,
    function(o) S7::set_props(o, title = title),
    label = "rename output"
  )
  invisible(NULL)
}

# ---- ARD cache: the two-stage seam ---------------------------------------

#' A keyed [arpillar::build_ard()] memo -- the two-stage render seam.
#'
#' The cache key is `dataset + type + roles + filters` -- deliberately
#' EXCLUDING `options`, so a display-only edit (decimals, cutoffs, ...)
#' reuses the already-built ARD instead of recollecting from DuckDB. Keys
#' are prefixed `"ard::"`: the one cache environment is shared with future
#' Add-output recommendation memos (`"rec::"`) and Data-mode profiles
#' (`"profile::"`), so a caller counting ARD entries must filter on the
#' prefix, never take a bare `ls()` length.
#' @noRd
cached_ard <- function(store, object) {
  roles <- lapply(object@roles, function(r) {
    list(
      slot = r@slot,
      items = lapply(r@items, function(it) {
        list(name = it@name, label = it@label, role_type = it@role_type)
      })
    )
  })
  key <- paste0(
    "ard::",
    rlang::hash(list(
      object@dataset,
      object@type,
      roles,
      object@filters
    ))
  )
  hit <- store$cache[[key]]
  if (!is.null(hit)) {
    return(hit)
  }
  ard <- arpillar::build_ard(store$con, object)
  store$cache[[key]] <- ard
  ard
}

# ---- logging ------------------------------------------------------------

#' Append a timestamped line onto `rv$log`.
#' @noRd
log_line <- function(store, msg) {
  ts <- format(Sys.time(), "%H:%M:%S")
  store$rv$log <- c(store$rv$log, paste0("[", ts, "] ", msg))
  invisible(NULL)
}

# ---- galley card -----------------------------------------------------

#' Open the galley card on `region`.
#' @noRd
open_card <- function(store, region) {
  store$rv$region <- region
  store$rv$card <- TRUE
  invisible(NULL)
}

#' Close the galley card -- a no-op while the card is pinned.
#' @noRd
close_card <- function(store) {
  if (isTRUE(store$rv$pinned)) {
    return(invisible(NULL))
  }
  store$rv$card <- FALSE
  invisible(NULL)
}

#' Toggle whether the galley card is docked open.
#' @noRd
toggle_pin <- function(store) {
  store$rv$pinned <- !isTRUE(store$rv$pinned)
  invisible(NULL)
}

# ---- v5 panel collapse (decision #8) --------------------------------------

#' Toggle the contents rail between full and the status-dot strip.
#' @noRd
toggle_rail <- function(store) {
  store$rv$rail_collapsed <- !isTRUE(store$rv$rail_collapsed)
  invisible(NULL)
}

#' Toggle the docked inspector between full and the icon strip.
#' @noRd
toggle_insp <- function(store) {
  store$rv$insp_collapsed <- !isTRUE(store$rv$insp_collapsed)
  invisible(NULL)
}
