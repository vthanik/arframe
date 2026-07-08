# The Report mode List-of-Contents (LoC): a full-width, editable table of
# every output that DRILLS into the paper + inspector (2026-07-08, mirrors
# Data mode's list->grid; supersedes the old 280px TOC rail). Groups outputs
# TABLES/FIGURES/LISTINGS (kind read off the type->generator map; an empty
# group is omitted), sorts each group by its `options$number` (the canonical
# TLF order, so there is no manual drag-reorder), stamps its status, and
# edits NUMBER / LABEL / TITLE / POPULATION inline. A row single-click
# selects; double-click / Enter drills (`drill_open()`); the breadcrumb / Esc
# return (`drill_close()`). Every mutation routes through the injected store
# -- this module holds no draft state of its own.

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

#' The kind-scoped group key -> display label + numbering prefix.
#'
#' An `object@type` outside the known map (a generator not yet wired to a
#' render leg) falls back to the `listing` group rather than being silently
#' dropped -- every output the report holds must appear somewhere (see
#' `.toc_rows()`'s `%||% "listing"` fallback). The `prefix` also backs the
#' fallback auto-number (`.toc_rows()`) and `.next_number()`'s
#' (`utils_report.R`) auto-suggest for a generator-seeded (preset-less) new
#' output.
#' @noRd
.TOC_GROUPS <- list(
  table = list(label = "TABLES", prefix = "14.1"),
  figure = list(label = "FIGURES", prefix = "14.2"),
  listing = list(label = "LISTINGS", prefix = "16.2")
)

# ---- row model ----------------------------------------------------------

#' Build one row per object: id, title, kind, type, group label, TLF
#' number, status, and the two inline-editable option values (number_label,
#' population).
#'
#' `number` prefers `obj@options$number` -- the number a preset seeded or
#' `add_from_generator()` auto-suggested -- falling back to the kind-scoped
#' 1-based document-order index only when that option is absent or blank
#' (e.g. an object built by hand, outside either add path). `status` folds
#' in `rv$broken` ahead of the oracle -- a broken id always shows ERROR
#' regardless of what `output_status()` would otherwise report -- then
#' `rv$stale` (a heavy edit awaiting Run). `type` is `obj@type` verbatim
#' (the `.type_icon()` glyph key), distinct from `kind` which only splits
#' rows into the coarse groups. `number_label` / `population` carry the raw
#' `obj@options` values (NA when unset) so the inline LABEL / POPULATION
#' selects can seed their current state without a second object lookup.
#' @noRd
.toc_rows <- function(report, broken, stale = character(0)) {
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
    # Exact `[[` -- `$number` partial-matches `number_label` when a user has
    # cleared the number but kept the label (R's dollar partial matching).
    seeded_number <- obj@options[["number"]]
    number <- if (length(seeded_number) == 1L && nzchar(seeded_number)) {
      seeded_number
    } else {
      paste0(grp$prefix, ".", counters[[kind]])
    }
    status <- if (obj@id %in% broken) {
      "broken"
    } else if (obj@id %in% stale) {
      # The stale flag (run semantics, decision #8) outranks the oracle --
      # the config is ready, but its proof awaits a Run.
      "stale"
    } else {
      arpillar::output_status(obj)
    }
    list(
      id = obj@id,
      title = obj@title,
      kind = kind,
      type = obj@type,
      group_label = grp$label,
      number = number,
      status = status,
      number_label = obj@options$number_label %||% NA_character_,
      population = obj@options$population %||% NA_character_
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

#' A version-aware sort key for a TLF number: each dotted numeric segment is
#' zero-padded to a fixed width so `"14.1.10"` sorts after `"14.1.2"` (a
#' plain string sort would put "10" before "2"). Non-numeric segments are
#' kept verbatim.
#' @noRd
.number_key <- function(number) {
  parts <- strsplit(number, ".", fixed = TRUE)[[1]]
  padded <- vapply(
    parts,
    function(p) {
      if (grepl("^[0-9]+$", p)) {
        formatC(as.integer(p), width = 6L, flag = "0")
      } else {
        p
      }
    },
    character(1)
  )
  paste(padded, collapse = ".")
}

#' Sort a group's rows by their TLF number (the canonical document order).
#' @noRd
.loc_sort_rows <- function(rows) {
  rows[order(vapply(rows, function(r) .number_key(r$number), character(1)))]
}

#' The output ids in the exact order the LoC displays them (group order, then
#' number within group) -- so keyboard Up/Down walks the SAME order the eye
#' sees, not raw document order.
#' @noRd
.loc_ordered_ids <- function(
  report,
  broken = character(0),
  stale = character(0)
) {
  rows <- .toc_rows(report, broken, stale)
  if (length(rows) == 0L) {
    return(character(0))
  }
  ids <- unlist(
    lapply(.toc_groups(rows), function(g) {
      vapply(.loc_sort_rows(g$rows), `[[`, character(1), "id")
    }),
    use.names = FALSE
  )
  ids %||% character(0)
}

# ---- UI ---------------------------------------------------------------

#' The List-of-Contents UI: a section label, the stat strip, the
#' `+ Add output` toolbar, and the server-rendered editable table. Full
#' width -- the desk + inspector live in a sibling `.ar-report-desk` that the
#' `ar-report-open` class reveals on drill.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_contents_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "ar-loc",
    .label("LIST OF CONTENTS"),
    shiny::uiOutput(ns("stats")),
    shiny::div(
      class = "ar-loc-bar",
      shiny::div(class = "ar-bar-spacer"),
      .action_btn(
        ns("add_output"),
        shiny::tagList(.icon("plus", 13), "Add output"),
        variant = "link",
        class = "ar-dx-tb ar-dx-tb-pri"
      )
    ),
    shiny::uiOutput(ns("table"))
  )
}

# ---- table rendering ----------------------------------------------------

#' The stat strip: total outputs + a count per broad status. First live
#' caller of `.stat_tile()`.
#' @noRd
.loc_stats <- function(rows) {
  st <- vapply(rows, `[[`, character(1), "status")
  shiny::div(
    class = "ar-loc-stats",
    .stat_tile(length(rows), "Outputs"),
    .stat_tile(sum(st == "ready"), "Ready"),
    .stat_tile(sum(st %in% c("draft", "needs_data")), "In progress"),
    .stat_tile(sum(st %in% c("broken", "stale")), "Needs attention")
  )
}

.LABEL_CHOICES <- c("Table", "Figure", "Listing")

#' The inline LABEL (`options$number_label`) native select. Seeds to the
#' explicit label, else the kind's natural label (Table/Figure/Listing).
#' @noRd
.loc_label_select <- function(row, onchange) {
  kind_default <- switch(
    row$kind,
    figure = "Figure",
    listing = "Listing",
    "Table"
  )
  sel <- if (
    length(row$number_label) == 1L &&
      !is.na(row$number_label) &&
      nzchar(row$number_label)
  ) {
    row$number_label
  } else {
    kind_default
  }
  shiny::tags$select(
    class = "ar-input-flat ar-loc-sel",
    onchange = onchange,
    lapply(.LABEL_CHOICES, function(v) {
      shiny::tags$option(
        value = v,
        selected = if (identical(v, sel)) "selected" else NULL,
        v
      )
    })
  )
}

#' The inline POPULATION (`options$population`) native select, off the study
#' `theme$populations` library. A leading em-dash option clears the binding;
#' an orphan current value (a legacy dataset name or a since-deleted set) is
#' appended so it still shows and can be re-pointed.
#' @noRd
.loc_pop_select <- function(row, pops, onchange) {
  cur <- if (
    length(row$population) == 1L &&
      !is.na(row$population) &&
      nzchar(row$population)
  ) {
    row$population
  } else {
    ""
  }
  ids <- names(pops)
  opts <- list(shiny::tags$option(
    value = "",
    selected = if (!nzchar(cur)) "selected" else NULL,
    "\u2014"
  ))
  for (pid in ids) {
    opts <- c(
      opts,
      list(shiny::tags$option(
        value = pid,
        selected = if (identical(pid, cur)) "selected" else NULL,
        pops[[pid]]$label %||% pid
      ))
    )
  }
  if (nzchar(cur) && !(cur %in% ids)) {
    opts <- c(
      opts,
      list(shiny::tags$option(
        value = cur,
        selected = "selected",
        paste0(cur, " (unknown)")
      ))
    )
  }
  shiny::tags$select(
    class = "ar-input-flat ar-loc-sel",
    onchange = onchange,
    opts
  )
}

#' The hover-revealed inline actions: Duplicate + Remove. Both stop
#' propagation (belt to the delegated row handler's `closest("button")`
#' guard) so acting on a row never also selects/drills it. Removal is
#' undo-able (`commit()` pushes the undo stack), so it needs no confirm
#' popover -- Cmd-Z is the safety net.
#' @noRd
.loc_actions <- function(ns, row) {
  stop_js <- "event.stopPropagation()"
  dup_js <- sprintf(
    "%s; Shiny.setInputValue('%s', '%s', {priority: 'event'})",
    stop_js,
    ns("duplicate"),
    row$id
  )
  rm_js <- sprintf(
    "%s; Shiny.setInputValue('%s', '%s', {priority: 'event'})",
    stop_js,
    ns("remove"),
    row$id
  )
  shiny::tagList(
    shiny::tags$button(
      type = "button",
      class = "ar-icon-btn ar-loc-act",
      title = "Duplicate",
      `aria-label` = "Duplicate output",
      onclick = dup_js,
      .icon("copy", 13)
    ),
    shiny::tags$button(
      type = "button",
      class = "ar-icon-btn ar-loc-act ar-loc-act-danger",
      title = "Remove",
      `aria-label` = "Remove output",
      onclick = rm_js,
      .icon("trash", 13)
    )
  )
}

#' One LoC table row: type glyph + four inline cells (NUMBER / LABEL / TITLE
#' / POPULATION) + status stamp + hover actions. Each editable cell is a
#' native `<input>`/`<select>` with NO Shiny id -- its `onchange` posts one
#' `cell_edit {id, field, value, nonce}` to the shared server observer, the
#' `arm_edit`/`.dec_select` idiom (commit on blur/change, never per
#' keystroke). The row carries `data-ar-id` for the delegated select/drill
#' handlers in bridge.js.
#' @noRd
.loc_row <- function(ns, row, pops, selected) {
  is_sel <- identical(row$id, selected)
  cell_js <- function(field) {
    sprintf(
      "Shiny.setInputValue('%s', {id: '%s', field: '%s', value: this.value, nonce: Date.now()}, {priority: 'event'})",
      ns("cell_edit"),
      row$id,
      field
    )
  }
  shiny::tags$tr(
    class = paste("ar-dx-row ar-loc-row", if (is_sel) "ar-dx-row-sel"),
    `data-ar-id` = row$id,
    shiny::tags$td(class = "ar-loc-type", .type_icon(row$type, 15)),
    shiny::tags$td(shiny::tags$input(
      type = "text",
      class = "ar-input-flat ar-mono ar-loc-in",
      value = row$number,
      `aria-label` = "TLF number",
      onchange = cell_js("number")
    )),
    shiny::tags$td(.loc_label_select(row, cell_js("number_label"))),
    shiny::tags$td(shiny::tags$input(
      type = "text",
      class = "ar-input-flat ar-loc-in ar-loc-title",
      value = row$title,
      `aria-label` = "Output title",
      onchange = cell_js("title")
    )),
    shiny::tags$td(.loc_pop_select(row, pops, cell_js("population"))),
    shiny::tags$td(class = "ar-loc-stamp", .stamp(row$status)),
    shiny::tags$td(class = "ar-loc-acts", .loc_actions(ns, row))
  )
}

#' One group block: a faint micro-label + a `.ar-dx-table` of its rows,
#' number-sorted.
#' @noRd
.loc_group <- function(ns, group, pops, selected) {
  rows <- .loc_sort_rows(group$rows)
  shiny::tagList(
    shiny::div(class = "ar-label ar-loc-group-label", group$label),
    shiny::tags$table(
      class = "ar-dx-table ar-loc-table",
      shiny::tags$thead(shiny::tags$tr(
        lapply(
          c("", "NUMBER", "LABEL", "TITLE", "POPULATION", "STATUS", ""),
          function(h) shiny::tags$th(h)
        )
      )),
      shiny::tags$tbody(
        lapply(rows, function(row) .loc_row(ns, row, pops, selected))
      )
    )
  )
}

#' The drill breadcrumb, rendered into the desk (`contents-crumb` in app.R).
#' Empty until a drill is open; shows a Contents back-link + the drilled
#' output's title.
#' @noRd
.loc_crumb <- function(ns, obj) {
  shiny::div(
    class = "ar-loc-crumb ar-mono",
    .action_btn(
      ns("back"),
      shiny::tagList(.icon("chevrons_left", 12), "List of Contents"),
      variant = "link",
      class = "ar-dx-bk"
    ),
    shiny::tags$span(class = "ar-dx-sep", "/"),
    shiny::tags$span(class = "ar-loc-crumb-title", obj@title)
  )
}

# ---- server -------------------------------------------------------------

#' The List-of-Contents server: renders the stat strip, the grouped editable
#' table, and the drill breadcrumb, and wires every row action (select /
#' drill / inline edit / duplicate / remove / add / keyboard nav) through the
#' injected store.
#' @param id *The module namespace, matching `mod_contents_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_contents_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$stats <- shiny::renderUI({
      .loc_stats(.toc_rows(store$rv$report, store$rv$broken, store$rv$stale))
    })

    output$table <- shiny::renderUI({
      rows <- .toc_rows(store$rv$report, store$rv$broken, store$rv$stale)
      if (length(rows) == 0L) {
        return(shiny::div(
          class = "ar-dx-empty",
          "No outputs yet \u2014 add one to begin."
        ))
      }
      pops <- store$rv$report@theme$populations %||% list()
      shiny::tagList(lapply(
        .toc_groups(rows),
        function(g) .loc_group(ns, g, pops, store$rv$selected)
      ))
    })

    output$crumb <- shiny::renderUI({
      id_open <- store$rv$report_open
      if (is.null(id_open)) {
        return(NULL)
      }
      obj <- .find_object(store$rv$report, id_open)
      if (is.null(obj)) {
        return(NULL)
      }
      .loc_crumb(ns, obj)
    })

    # The report body is hidden in other modes, and the LoC surface is hidden
    # while drilled (the desk shows instead) -- keep all three computing so a
    # mode switch or a breadcrumb-return never lands on a blank surface.
    shiny::outputOptions(output, "stats", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "table", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "crumb", suspendWhenHidden = FALSE)

    # Mirror the store's drill pointer to the workspace `ar-report-open`
    # class (the layout gate). ignoreNULL = FALSE so a drill_close (-> NULL)
    # still drops the class.
    shiny::observeEvent(
      store$rv$report_open,
      {
        session$sendCustomMessage(
          "ar-report-open",
          list(on = !is.null(store$rv$report_open))
        )
      },
      ignoreNULL = FALSE
    )

    shiny::observeEvent(input$row_click, {
      store$rv$selected <- input$row_click
    })

    # Double-click a row -> drill into its paper + inspector.
    shiny::observeEvent(input$open, {
      drill_open(store, input$open)
    })

    # The breadcrumb Contents link (and Esc, via bridge.js) -> back to list.
    shiny::observeEvent(input$back, {
      drill_close(store)
    })

    # One channel for every inline cell edit. title -> rename_output (blank is
    # a no-op); number / number_label / population -> the standing per-output
    # override (blank clears it, so the value falls back to Setup / the
    # engine default). A population change is a heavy edit (it subsets the
    # data), so update_object marks the proof STALE via its .ard_key oracle.
    shiny::observeEvent(input$cell_edit, {
      e <- input$cell_edit
      id_edit <- e$id
      field <- as.character(e$field %||% "")
      if (is.null(id_edit) || !nzchar(field)) {
        return()
      }
      value <- trimws(as.character(e$value %||% ""))
      if (identical(field, "title")) {
        if (nzchar(value)) {
          rename_output(store, id_edit, value)
        }
        return()
      }
      if (!field %in% c("number", "number_label", "population")) {
        return()
      }
      val <- if (nzchar(value)) value else NULL
      update_object(
        store,
        id_edit,
        function(o) {
          opts <- o@options
          opts[[field]] <- val
          S7::set_props(o, options = opts)
        },
        label = paste("set", field)
      )
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

    # Keyboard nav (Task 17): Up/Down walk the selection through the ids in
    # DISPLAY order (group order, number-sorted -- matching the eye), posted
    # by bridge.js's report-mode keydown map; the first arrow with nothing
    # selected picks the first output. The payload nonce makes a repeated
    # same-direction press a fresh event.
    shiny::observeEvent(input$nav, {
      dir <- input$nav$dir
      ids <- .loc_ordered_ids(
        store$rv$report,
        store$rv$broken,
        store$rv$stale
      )
      if (length(ids) == 0L) {
        return()
      }
      if (is.null(store$rv$selected)) {
        store$rv$selected <- ids[[1]]
        return()
      }
      cur <- match(store$rv$selected, ids)
      if (is.na(cur)) {
        cur <- 1L
      }
      nxt <- if (identical(dir, "up")) {
        max(1L, cur - 1L)
      } else {
        min(length(ids), cur + 1L)
      }
      store$rv$selected <- ids[[nxt]]
    })

    # Enter drills into the selected output (the keyboard twin of a
    # double-click). A no-op when nothing is selected.
    shiny::observeEvent(input$activate, {
      if (!is.null(store$rv$selected)) {
        drill_open(store, store$rv$selected)
      }
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
