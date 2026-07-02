# The Contents column: the TOC that IS navigation (design spec #3) -- there
# are no canvas tabs, so this list is the only way to switch which output the
# desk shows. Groups outputs TABLES/FIGURES/LISTINGS (kind read off the
# type->generator map; an empty group is omitted entirely), numbers each row
# by its preset-seeded/auto-suggested `options$number` (falling back to the
# old kind-scoped document-order index only when absent), stamps its status,
# and drives every mutation (reorder/rename/duplicate/remove/select) straight
# through the injected store -- this module holds no draft state of its own.

# ---- kind lookup ------------------------------------------------------

#' The kind ("table"/"figure") for each renderable `object@type`.
#'
#' `arpillar::generators()` is keyed by engine TYPE (`"summary"`,
#' `"crosstab"`, `"occurrence"`, `"km"`, `"line"`, `"box"`), which IS the
#' render `type` an `object` actually carries -- so this is a direct
#' name -> `$kind` projection, not a reverse index (the old
#' `arpillar::templates()` was keyed by preset/template id instead, which
#' needed one). No generator currently has `kind == "listing"`; the
#' LISTINGS group is always empty until one is registered, which is
#' exactly why callers must omit empty groups rather than assume the
#' three-group set is complete.
#' @noRd
.kind_by_type <- function() {
  g <- arpillar::generators()
  stats::setNames(vapply(g, `[[`, "", "kind"), names(g))
}

#' The TOC's kind-scoped group key -> display label + numbering prefix.
#'
#' An `object@type` outside the known map (a generator not yet wired to a
#' render leg) falls back to the `listing` group rather than being silently
#' dropped from the TOC -- every output the report holds must appear
#' somewhere (see `.toc_rows()`'s `%||% "listing"` fallback). The `prefix`
#' also backs the fallback auto-number (`.toc_rows()`) and
#' `.next_number()`'s (`utils_report.R`) auto-suggest for a
#' generator-seeded (preset-less) new output.
#' @noRd
.TOC_GROUPS <- list(
  table = list(label = "TABLES", prefix = "14.1"),
  figure = list(label = "FIGURES", prefix = "14.2"),
  listing = list(label = "LISTINGS", prefix = "16.2")
)

# ---- row model ----------------------------------------------------------

#' Build one row per object: id, title, kind, type, group label, TLF
#' number, and status.
#'
#' `number` prefers `obj@options$number` -- the number a preset seeded or
#' `add_from_generator()` auto-suggested -- falling back to the old
#' kind-scoped 1-based document-order index only when that option is
#' absent or blank (e.g. an object built by hand, outside either add
#' path). `status` folds in `rv$broken` ahead of the oracle -- a broken id
#' always shows ERROR regardless of what `output_status()` would otherwise
#' report, matching the stamp table's "app-side render-failed flag"
#' precedence. `type` is `obj@type` verbatim (the render TYPE, e.g.
#' `"occurrence"`) -- the `.type_icon()` glyph key, distinct from `kind`
#' which only splits rows into the coarse TABLES/FIGURES/LISTINGS groups.
#' @noRd
.toc_rows <- function(report, broken) {
  objs <- .all_objects(report)
  if (length(objs) == 0L) {
    return(list())
  }
  by_type <- .kind_by_type()
  kinds <- vapply(
    objs,
    function(o) by_type[[o@type]] %||% "listing",
    character(1)
  )
  counters <- stats::setNames(integer(length(.TOC_GROUPS)), names(.TOC_GROUPS))
  lapply(seq_along(objs), function(i) {
    obj <- objs[[i]]
    kind <- kinds[[i]]
    grp <- .TOC_GROUPS[[kind]]
    counters[[kind]] <<- counters[[kind]] + 1L
    seeded_number <- obj@options$number
    number <- if (length(seeded_number) == 1L && nzchar(seeded_number)) {
      seeded_number
    } else {
      paste0(grp$prefix, ".", counters[[kind]])
    }
    status <- if (obj@id %in% broken) "broken" else arpillar::output_status(obj)
    list(
      id = obj@id,
      title = obj@title,
      kind = kind,
      type = obj@type,
      group_label = grp$label,
      number = number,
      status = status
    )
  })
}

#' Split rows into ordered (label, rows) groups, dropping empty groups.
#' @noRd
.toc_groups <- function(rows) {
  order <- c("table", "figure", "listing")
  present <- unique(vapply(rows, `[[`, character(1), "kind"))
  order <- order[order %in% present]
  lapply(order, function(k) {
    list(
      label = .TOC_GROUPS[[k]]$label,
      rows = Filter(function(r) identical(r$kind, k), rows)
    )
  })
}

# ---- UI ---------------------------------------------------------------

#' The Contents column UI: the `CONTENTS` label, the grouped TOC (server-
#' rendered), and the `+ Add output` footer action.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_contents_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "ar-toc",
    .label("CONTENTS"),
    shiny::uiOutput(ns("toc")),
    .action_btn(
      ns("add_output"),
      shiny::tagList(.icon("plus", 12), "Add output"),
      variant = "link",
      class = "ar-toc-add"
    )
  )
}

# ---- row/group rendering ------------------------------------------------

#' One TOC row: grip (drag handle) + type glyph + mono number + sans title +
#' dotted leader + status stamp + hover-revealed kebab. Laid out as a
#' 6-column CSS grid (`[grip 14px] [type icon 14px] [number] [title 1fr]
#' [stamp] [kebab 18px]`) -- the title's grid cell is itself a flex pair of
#' the title text (fixed to its content, shrinking to ellipsis under real
#' pressure) and the leader (`flex: 1`), so the leader always fills exactly
#' the gap between a SHORT title and the stamp, while a long title still
#' gets first claim on the shared `1fr` column instead of losing the space
#' to a leader that grows unconditionally. The whole row is clickable
#' (posts `row_click` via an inline `onclick`, namespaced through `ns` so
#' this module never depends on a fixed, hardcoded module id) except the
#' kebab and its popovers, which stop propagation so opening a menu never
#' also re-selects the row underneath it.
#' @noRd
.toc_row <- function(ns, row, active) {
  click_js <- sprintf(
    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
    ns("row_click"),
    row$id
  )
  shiny::tags$div(
    class = paste(
      "ar-toc-row",
      if (active) "ar-toc-row-active" else NULL
    ),
    `data-ar-id` = row$id,
    onclick = click_js,
    shiny::tags$span(class = "ar-toc-grip", .icon("grip", 12)),
    .type_icon(row$type, 13),
    shiny::tags$span(class = "ar-toc-number ar-mono", row$number),
    shiny::tags$span(
      class = "ar-toc-title-wrap",
      # `title=` gives a truncated entry its native tooltip -- a real
      # clinical TLF title routinely outgrows a 280px column even after
      # ellipsis, so the full text stays one hover away rather than lost.
      shiny::tags$span(class = "ar-toc-title", title = row$title, row$title),
      shiny::tags$span(class = "ar-toc-leader")
    ),
    .stamp(row$status),
    .toc_kebab(ns, row)
  )
}

#' The hover-revealed kebab: a toggle button plus its two `.ar-pop`
#' popovers (rename: labelled input + Apply; remove: destructive confirm).
#' Duplicate needs no input, so it fires directly off the menu item -- open
#' state is pure client-side class-toggling (the same pattern as the
#' app-bar title edit), never server round-tripped, since it is ephemeral
#' UI chrome, not document state.
#' @noRd
.toc_kebab <- function(ns, row) {
  stop_js <- "event.stopPropagation()"
  dup_js <- sprintf(
    "%s; Shiny.setInputValue('%s', '%s', {priority: 'event'})",
    stop_js,
    ns("duplicate"),
    row$id
  )
  remove_js <- sprintf(
    "%s; Shiny.setInputValue('%s', '%s', {priority: 'event'})",
    stop_js,
    ns("remove"),
    row$id
  )
  rename_js <- sprintf(
    "%s; Shiny.setInputValue('%s', {id: '%s', title: this.previousElementSibling.value}, {priority: 'event'})",
    stop_js,
    ns("rename"),
    row$id
  )
  shiny::tags$div(
    class = "ar-toc-kebab-wrap",
    onclick = stop_js,
    shiny::tags$button(
      type = "button",
      class = "ar-icon-btn ar-toc-kebab",
      `aria-label` = "Output actions",
      onclick = "this.closest('.ar-toc-kebab-wrap').classList.toggle('ar-pop-menu-open')",
      .icon("kebab", 12)
    ),
    shiny::tags$div(
      class = "ar-pop-menu",
      shiny::tags$button(
        type = "button",
        class = "ar-pop-menu-item",
        onclick = "this.closest('.ar-toc-kebab-wrap').classList.remove('ar-pop-menu-open'); this.closest('.ar-toc-kebab-wrap').classList.add('ar-pop-rename-open')",
        "Rename"
      ),
      shiny::tags$button(
        type = "button",
        class = "ar-pop-menu-item",
        onclick = paste0(
          "this.closest('.ar-toc-kebab-wrap').classList.remove('ar-pop-menu-open');",
          dup_js
        ),
        "Duplicate"
      ),
      shiny::tags$button(
        type = "button",
        class = "ar-pop-menu-item ar-pop-menu-item-danger",
        onclick = "this.closest('.ar-toc-kebab-wrap').classList.remove('ar-pop-menu-open'); this.closest('.ar-toc-kebab-wrap').classList.add('ar-pop-remove-open')",
        "Remove"
      )
    ),
    shiny::tags$div(
      class = "ar-pop ar-pop-rename",
      shiny::tags$input(
        type = "text",
        class = "form-control",
        value = row$title
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-outline-secondary ar-pop-apply",
        onclick = paste0(
          rename_js,
          "; this.closest('.ar-toc-kebab-wrap').classList.remove('ar-pop-rename-open')"
        ),
        "Apply"
      )
    ),
    shiny::tags$div(
      class = "ar-pop ar-pop-remove",
      shiny::tags$span("Remove this output?"),
      shiny::tags$button(
        type = "button",
        class = "btn btn-danger ar-pop-apply",
        onclick = remove_js,
        "Remove"
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-outline-secondary",
        onclick = "this.closest('.ar-toc-kebab-wrap').classList.remove('ar-pop-remove-open')",
        "Cancel"
      )
    )
  )
}

#' One group: a faint micro-label header + its sortable row list. The
#' sortable container spans the WHOLE toc (see `.toc_ui`), not one per
#' group -- SortableJS reorders a single flat DOM list; grouping is a
#' rendering concern, reorder always operates on the full document order.
#' @noRd
.toc_group_block <- function(ns, group, selected) {
  shiny::tagList(
    shiny::div(class = "ar-label ar-toc-group-label", group$label),
    lapply(group$rows, function(row) {
      .toc_row(ns, row, active = identical(row$id, selected))
    })
  )
}

#' The full TOC: one `data-ar-sortable` container wrapping every group's
#' rows back to back, so drag can move a row across the visual boundary
#' between two group headers (which stay static, non-draggable content
#' inside the same flex column but outside `[data-ar-sortable-item]`).
#' @noRd
.toc_ui <- function(ns, groups, selected) {
  shiny::tags$div(
    class = "ar-toc-list",
    `data-ar-sortable` = "true",
    `data-ar-sortable-handle` = ".ar-toc-grip",
    `data-ar-sortable-item` = ".ar-toc-row",
    `data-ar-sortable-attr` = "data-ar-id",
    `data-ar-sortable-input` = ns("reorder"),
    lapply(groups, function(g) .toc_group_block(ns, g, selected))
  )
}

# ---- server -------------------------------------------------------------

#' The Contents server: renders the grouped TOC and wires every row action
#' (select / reorder / rename / duplicate / remove / add) through the
#' injected store.
#' @param id *The module namespace, matching `mod_contents_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_contents_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # This DOES re-render on order change: `.toc_rows()` reads
    # `store$rv$report`, and `move_output()`/`commit()` reassign that
    # reactive on every reorder, so a drop always re-derives rows fresh from
    # the store's own object list (never from the DOM) and re-numbers them.
    # That is correct, not merely tolerated -- SortableJS only owns the DOM
    # order WHILE a drag is physically in progress (`onEnd` posts the order
    # once, on drop), and `arInitSortables()`'s `_arSortable` guard cleanly
    # re-binds a fresh Sortable instance to the replaced rows after this
    # renderUI runs, so numbering stays correct across repeated drags. The
    # one residual risk is a DIFFERENT module committing to `rv$report`
    # while a drag is physically mid-gesture (a renderUI mid-drag would
    # swap out the very DOM nodes the user's mouse is dragging) -- today
    # nothing else can commit to this store concurrently, so it cannot
    # happen; Task 9/10 introduces the first concurrent mutator and should
    # revisit this (see `document.body.dataset.arDragging` in arframe.js).
    output$toc <- shiny::renderUI({
      rows <- .toc_rows(store$rv$report, store$rv$broken)
      groups <- .toc_groups(rows)
      .toc_ui(ns, groups, store$rv$selected)
    })

    shiny::observeEvent(input$row_click, {
      store$rv$selected <- input$row_click
    })

    # Reconcile the posted DOM order against the store's own id set (a
    # stale id from a since-removed row is dropped; an id the drag never
    # touched -- e.g. a row in a different group -- is appended at the end,
    # never lost). `move_output()` is the only reorder primitive the store
    # exposes, so a multi-row drag walks it position by position; skipping
    # already-correct slots keeps a same-group drag to the ~1-2 calls (and
    # matching undo-stack entries) the gesture actually implies, rather than
    # one call per row regardless of how much moved.
    shiny::observeEvent(input$reorder, {
      ord <- vapply(input$reorder$order, as.character, character(1))
      ids <- vapply(
        .all_objects(store$rv$report),
        function(o) o@id,
        character(1)
      )
      full_order <- c(intersect(ord, ids), setdiff(ids, ord))
      for (i in seq_along(full_order)) {
        current_ids <- vapply(
          .all_objects(store$rv$report),
          function(o) o@id,
          character(1)
        )
        if (!identical(current_ids[[i]], full_order[[i]])) {
          move_output(store, full_order[[i]], i)
        }
      }
    })

    shiny::observeEvent(input$rename, {
      title <- trimws(input$rename$title)
      if (!nzchar(title)) {
        return()
      }
      rename_output(store, input$rename$id, title)
    })

    shiny::observeEvent(input$duplicate, {
      .duplicate_output(store, input$duplicate)
    })

    shiny::observeEvent(input$remove, {
      remove_output(store, input$remove)
    })

    shiny::observeEvent(input$add_output, {
      store$rv$adding <- TRUE
    })

    invisible(NULL)
  })
}

#' Clone the object with `id`: a fresh id, everything else copied verbatim,
#' appended after the original, selected. Siblings are untouched.
#' @noRd
.duplicate_output <- function(store, id) {
  obj <- .find_object(store$rv$report, id)
  if (is.null(obj)) {
    return(invisible(NULL))
  }
  new_id <- .next_id(store$rv$report)
  clone <- S7::set_props(obj, id = new_id)
  pages <- store$rv$report@pages
  for (i in seq_along(pages)) {
    idx <- which(vapply(
      pages[[i]]@objects,
      function(o) identical(o@id, id),
      logical(1)
    ))
    if (length(idx) == 1L) {
      pages[[i]] <- S7::set_props(
        pages[[i]],
        objects = c(pages[[i]]@objects, list(clone))
      )
      break
    }
  }
  new_report <- S7::set_props(store$rv$report, pages = pages)
  commit(store, new_report, label = "duplicate output")
  store$rv$selected <- new_id
  invisible(new_id)
}
