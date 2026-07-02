# Pure walkers over the S7 document tree (report -> page -> object). Every
# function here rebuilds the tree with S7::set_props() -- S7 properties are
# immutable, so a "mutation" is always a brand-new report returned to the
# caller, never an in-place x@prop <- value. fct_store.R's mutators are thin
# wrappers over these plus a commit(). Construction (.object_from_preset /
# .object_from_generator) reads arpillar::preset()/generator() -- the
# arpillar::template()/templates() API they replaced is gone.

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

#' Resolve one variable's `role_type` (measure/category/date) off the
#' catalog, defaulting to "category" when the variable is absent from
#' `dataset` -- construction never silently drops a preset-listed var, it
#' still builds a `data_item()` for it (fail loud belongs to the caller /
#' validate step, not to a swallowed role).
#' @noRd
.resolve_role_type <- function(con, dataset, var) {
  items <- arpillar::data_items(con, dataset)
  hit <- items$type[items$name == var]
  if (length(hit) == 1L) hit else "category"
}

#' Build the `roles` list for a preset: one [arpillar::role()] per slot,
#' each holding one [arpillar::data_item()] per variable name in
#' `preset$roles[[slot]]`, with `role_type` resolved per-var off the
#' catalog (see `.resolve_role_type()`).
#' @noRd
.roles_from_preset <- function(con, dataset, preset_roles) {
  lapply(names(preset_roles), function(slot) {
    vars <- preset_roles[[slot]]
    arpillar::role(
      slot = slot,
      items = lapply(vars, function(v) {
        arpillar::data_item(
          name = v,
          role_type = .resolve_role_type(con, dataset, v)
        )
      })
    )
  })
}

#' Build a fresh object from a preset entry (`arpillar::preset()`), bound to
#' `dataset`, with `id`. Copies `type` (the preset's `generator`), `title`,
#' `footnotes`, `filters`, `options` (carries `number`/`number_label` and,
#' for occurrence presets, `population`) verbatim; `roles` are rebuilt off
#' the catalog via `.roles_from_preset()` so `role_type` reflects `dataset`,
#' not the preset author's assumed source dataset.
#' @noRd
.object_from_preset <- function(con, preset, dataset, id) {
  arpillar::object(
    id = id,
    type = preset$generator,
    title = preset$title %||% "",
    dataset = dataset,
    roles = .roles_from_preset(con, dataset, preset$roles),
    filters = preset$filters %||% list(),
    options = preset$options %||% list(),
    footnotes = as.character(preset$footnotes %||% character(0))
  )
}

#' Build a bare object from a generator entry (`arpillar::generator()`),
#' bound to `dataset`, with `id`, and no roles filled -- the "blank slate"
#' path (as opposed to `.object_from_preset()`'s pre-filled one). `options`
#' seeds `number` (auto-suggested via `.next_number()`) and `number_label`
#' (`kind` title-cased: table -> "Table", figure -> "Figure",
#' listing -> "Listing").
#' @noRd
.object_from_generator <- function(generator, dataset, id, existing_numbers) {
  arpillar::object(
    id = id,
    type = generator$id,
    title = generator$label %||% "",
    dataset = dataset,
    options = list(
      number = .next_number(generator$kind, existing_numbers),
      number_label = .kind_number_label(generator$kind)
    ),
    footnotes = character(0)
  )
}

#' The kind -> TLF number_label used in the paper title block.
#' @noRd
.kind_number_label <- function(kind) {
  switch(
    kind,
    table = "Table",
    figure = "Figure",
    listing = "Listing",
    "Table"
  )
}

#' The next free `<prefix>.<n>` TLF number for `kind`, one past the highest
#' `.n` suffix already present in `existing_numbers` that shares the kind's
#' prefix (so a mix of preset-seeded and generator-suggested numbers in the
#' same document never collides). Starts at `.1` when none exist yet. The
#' prefix comes from `.TOC_GROUPS` (`mod_contents.R`) -- the single table
#' mapping kind -> TOC numbering prefix, not duplicated here.
#' @noRd
.next_number <- function(kind, existing_numbers) {
  prefix <- .TOC_GROUPS[[kind]]$prefix %||% "14.1"
  pat <- paste0("^", gsub("\\.", "\\\\.", prefix), "\\.(\\d+)$")
  hits <- grep(pat, existing_numbers, value = TRUE)
  nums <- as.integer(sub(pat, "\\1", hits))
  n <- if (length(nums) == 0L) 0L else max(nums)
  paste0(prefix, ".", n + 1L)
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
