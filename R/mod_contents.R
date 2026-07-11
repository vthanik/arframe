# The Report mode List-of-Contents (LoC): a Data-mode mirror (decision
# #12.3). A CONTENTS rail (left) filters the flat editable table by
# kind (TABLES/FIGURES/LISTINGS, read off the type->generator map); the table
# lists every output in flat kind-rank + `options$number` order (the canonical
# TLF order, so there is no manual drag-reorder), stamps its status, shows its
# file mtime, and edits NUMBER / LABEL / TITLE / POPULATION inline. A row
# single-click selects; double-click / Enter (or the toolbar Edit) drills
# (`drill_open()`); the breadcrumb / Esc return (`drill_close()`). The toolbar
# Duplicate / Delete + the text filter round out the manage surface. Every
# mutation routes through the injected store — this module holds no draft
# state of its own.

# ---- kind lookup ------------------------------------------------------

#' The kind ("table"/"figure") for each renderable `object@type`.
#'
#' `arpillar::generators()` is keyed by engine TYPE (`"summary"`,
#' `"crosstab"`, `"occurrence"`, `"km"`, `"line"`, `"box"`), which IS the
#' render `type` an `object` actually carries — so this is a direct
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
#' dropped — every output the report holds must appear somewhere (see
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
#' `number` prefers `obj@options$number` — the number a preset seeded or
#' `add_from_generator()` auto-suggested — falling back to the kind-scoped
#' 1-based document-order index only when that option is absent or blank
#' (e.g. an object built by hand, outside either add path). `status` folds
#' in `rv$broken` ahead of the oracle — a broken id always shows ERROR
#' regardless of what `output_status()` would otherwise report — then
#' `rv$stale` (a heavy edit awaiting Run). `type` is `obj@type` verbatim
#' (the `.type_icon()` glyph key), distinct from `kind` which only splits
#' rows into the coarse groups. `number_label` / `population` carry the raw
#' `obj@options` values (NA when unset) so the inline LABEL / POPULATION
#' selects can seed their current state without a second object lookup.
#' @noRd
.toc_rows <- function(report, broken, stale = character(0), mtimes = NULL) {
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
    # Exact `[[` — `$number` partial-matches `number_label` when a user has
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
      # The stale flag (run semantics, decision #8) outranks the oracle —
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
      population = obj@options$population %||% NA_character_,
      modified = .loc_modified_str(mtimes, obj@id)
    )
  })
}

# kind -> flat display rank (the fixed TLF order). Rows across kinds share one
# flat table now (the rail filters instead of inline group headers), so the
# eye-order is kind-rank then TLF number within kind.
.LOC_KIND_RANK <- c(table = 1L, figure = 2L, listing = 3L)

#' The "MODIFIED" cell for one output: its `outputs/<id>.json` file mtime
#' (`store$mtimes`, populated by open_project()/save_touched()), formatted
#' `YYYY-MM-DD HH:MM`. An em-dash when no project is on disk yet (in-memory
#' session) or the file has not been written — there is no other per-output
#' timestamp to fall back to (the activity log is batch-level).
#' @noRd
.loc_modified_str <- function(mtimes, id) {
  if (is.null(mtimes)) {
    return("\u2014")
  }
  val <- mtimes[[paste0(id, ".json")]]
  if (is.null(val) || is.na(val)) {
    return("\u2014")
  }
  format(as.POSIXct(val, origin = "1970-01-01"), "%Y-%m-%d %H:%M")
}

#' Sort ALL rows into one flat display order: kind rank, then TLF number
#' within kind. Replaces the per-group split now the rail owns kind-filtering.
#' @noRd
.loc_sort_all <- function(rows) {
  if (length(rows) == 0L) {
    return(rows)
  }
  rank <- vapply(
    rows,
    function(r) .LOC_KIND_RANK[[r$kind]] %||% 99L,
    integer(1)
  )
  key <- vapply(rows, function(r) .number_key(r$number), character(1))
  rows[order(rank, key)]
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

#' The output ids in the exact order the LoC displays them (flat kind-rank,
#' then number within kind) — so keyboard Up/Down walks the SAME order the eye
#' sees. Walks ALL outputs regardless of the active rail filter; a filtered-out
#' row can still be reached by arrow (a minor edge, kept lazy).
#' @noRd
.loc_ordered_ids <- function(
  report,
  broken = character(0),
  stale = character(0)
) {
  rows <- .loc_sort_all(.toc_rows(report, broken, stale))
  if (length(rows) == 0L) {
    return(character(0))
  }
  vapply(rows, `[[`, character(1), "id")
}

# ---- UI ---------------------------------------------------------------

#' The List-of-Contents UI: a Data-mode mirror (decision #12.3). A CONTENTS
#' rail (left) filters the main table by kind; the manage toolbar carries a
#' text filter plus Edit / Duplicate / Delete (acting on the selected row) and
#' `+ Add output`; the server-rendered editable table fills the rest. `.ar-loc`
#' is the flex ROW that holds the two columns — the desk + inspector live in a
#' sibling `.ar-report-desk` that the `ar-report-open` class reveals on drill.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_contents_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "ar-loc",
    shiny::div(
      class = "ar-data-rail",
      `data-ar-resizable` = "left",
      shiny::tags$div(
        class = "ar-rail-resize",
        `data-ar-resize` = "left",
        `aria-hidden` = "true"
      ),
      shiny::uiOutput(ns("rail"))
    ),
    shiny::div(
      class = "ar-data-main",
      shiny::div(
        class = "ar-dx-bar",
        shiny::tags$input(
          id = ns("filter"),
          type = "text",
          class = "ar-dx-filter ar-search",
          placeholder = "Filter outputs"
        ),
        shiny::div(class = "ar-bar-spacer"),
        .action_btn(
          ns("edit"),
          shiny::tagList(.icon("eye", 13), "Edit"),
          variant = "primary",
          class = "ex-btn-sm ar-dx-tb"
        ),
        .action_btn(
          ns("duplicate_sel"),
          shiny::tagList(.icon("copy", 13), "Duplicate"),
          variant = "outline-secondary",
          class = "ex-btn-sm ar-dx-tb"
        ),
        .action_btn(
          ns("delete_sel"),
          shiny::tagList(.icon("trash", 13), "Delete"),
          variant = "outline-danger",
          class = "ex-btn-sm ar-dx-tb"
        ),
        .action_btn(
          ns("add_output"),
          shiny::tagList(.icon("plus", 13), "Add output"),
          variant = "primary",
          class = "ex-btn-sm ar-dx-tb ar-dx-tb-pri"
        )
      ),
      shiny::uiOutput(ns("table"))
    )
  )
}

# ---- contents rail ------------------------------------------------------

#' One rail output row: type glyph + mono number + (truncated) title + status
#' stamp — the compact twin of a main-table row, and the Report-mode twin of
#' Data's `.src_dataset_row()`. Carries `data-ar-id`; a single click posts
#' `input$open` through the delegated `.ar-loc-nav` handler, so clicking any
#' output (from the list OR while already drilled) opens/switches it into edit
#' mode. The currently-open output gets the selected class.
#' @noRd
.contents_nav_row <- function(row, open) {
  sel <- if (!is.null(open) && identical(open, row$id)) {
    "ar-data-ds-sel"
  } else {
    NULL
  }
  shiny::tags$div(
    class = paste("ar-data-src ar-loc-nav", sel),
    `data-ar-id` = row$id,
    title = paste0(row$number, "  ", row$title),
    shiny::tags$span(class = "ar-loc-nav-ic", .type_icon(row$type, 14)),
    shiny::tags$span(class = "ar-mono ar-loc-nav-num", row$number),
    shiny::tags$span(class = "ar-loc-nav-title", row$title),
    .stamp(row$status)
  )
}

#' The CONTENTS rail (Data-mirror, decision #12.3): a kind FOLDER tree of one
#' collapsible folder per PRESENT kind (TABLES / FIGURES / LISTINGS, chevron +
#' count), each nesting its output rows (icon + number + title + status).
#' Clicking a folder body filters the main table to that kind (`input$group`);
#' re-clicking the active folder clears the filter (the "All outputs" root that
#' used to clear it was removed). The chevron collapses the folder's
#' outputs (`input$loc_toggle`); a nested output opens/switches it into edit
#' mode (`input$open`, the drill input) — so you can hop between outputs while
#' drilled. `open` (`store$rv$report_open`) lights the row in edit; `collapsed`
#' holds the folders whose outputs are hidden.
#' @noRd
.contents_rail <- function(
  ns,
  rows,
  active,
  open = NULL,
  collapsed = character(0)
) {
  kinds <- vapply(rows, `[[`, character(1), "kind")
  order <- c("table", "figure", "listing")
  present <- order[order %in% kinds]
  icon_for <- function(k) {
    switch(
      k,
      figure = .icon("figure", 14),
      listing = .icon("listing", 14),
      .icon("table", 14)
    )
  }
  folders <- lapply(present, function(k) {
    kids <- .loc_sort_all(Filter(function(r) identical(r$kind, k), rows))
    sel <- if (identical(active, k)) "ar-toc-row-sel ar-data-ds-sel" else NULL
    is_collapsed <- k %in% collapsed
    # Chevron (collapse) and folder body (filter) are separate hit zones, so a
    # toggle click never also filters — the Data `.sources_tree()` idiom.
    shiny::tags$div(
      class = paste("ar-src-group", if (is_collapsed) "ar-src-collapsed"),
      shiny::tags$div(
        class = "ar-src-folder-row",
        shiny::tags$button(
          class = "ar-src-toggle",
          type = "button",
          `data-ar-loc-toggle` = k,
          `aria-label` = paste("Toggle", .TOC_GROUPS[[k]]$label),
          .icon("chevron_right", 11)
        ),
        shiny::tags$div(
          class = paste("ar-data-src ar-toc-row", sel),
          `data-ar-loc-group` = k,
          icon_for(k),
          shiny::tags$span(class = "ar-data-src-name", .TOC_GROUPS[[k]]$label),
          shiny::tags$span(class = "ar-mono ar-data-src-n", length(kids))
        )
      ),
      if (!is_collapsed) {
        lapply(kids, function(r) .contents_nav_row(r, open))
      }
    )
  })
  shiny::tagList(
    shiny::tags$div(
      class = "ar-src-head",
      shiny::tags$div(class = "ar-label", "Contents")
    ),
    folders
  )
}

# ---- table rendering ----------------------------------------------------

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
.loc_pop_select <- function(row, pops, default_pop, onchange) {
  set <- length(row$population) == 1L &&
    !is.na(row$population) &&
    nzchar(row$population)
  # Unset -> show the study default population (`theme$default_population`, the
  # same set the engine falls back to), NOT an em-dash: an output always runs
  # on SOME population, so the default is what the reader should see.
  cur <- if (set) row$population else (default_pop %||% "")
  ids <- names(pops)
  opts <- lapply(ids, function(pid) {
    shiny::tags$option(
      value = pid,
      selected = if (identical(pid, cur)) "selected" else NULL,
      pops[[pid]]$label %||% pid
    )
  })
  # An orphan current value (a legacy dataset name or a since-deleted set) is
  # appended so it still shows and can be re-pointed.
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
  # No populations defined at all -> a single em-dash placeholder so the
  # control is not empty (Setup has not seeded the library yet).
  if (length(opts) == 0L) {
    opts <- list(shiny::tags$option(
      value = "",
      selected = "selected",
      "\u2014"
    ))
  }
  shiny::tags$select(
    class = "ar-input-flat ar-loc-sel",
    onchange = onchange,
    opts
  )
}

#' The lower-cased haystack for the client-side text filter: NUMBER + LABEL +
#' TITLE + POPULATION packed into one string. The edit cells are `<input>`s
#' whose values are NOT in the row's `textContent`, so the filter reads this
#' `data-ar-hay` attribute instead (bridge.js prefers it over `data-ar-name`).
#' @noRd
.loc_row_hay <- function(row) {
  parts <- c(
    row$number,
    if (!is.na(row$number_label)) row$number_label,
    row$title,
    if (!is.na(row$population)) row$population
  )
  tolower(paste(parts, collapse = " "))
}

#' One LoC table row: NUMBER (a small type glyph inlined ahead of the mono
#' number input) + three more inline cells (LABEL / TITLE / POPULATION) +
#' status stamp + MODIFIED. Each editable cell is a native `<input>`/`<select>`
#' with NO Shiny id — its `onchange` posts one `cell_edit {id, field, value,
#' nonce}` to the shared server observer (commit on blur/change, never per
#' keystroke). The row carries `data-ar-id` for the delegated select/drill
#' handlers and `data-ar-hay` for the text filter (both in bridge.js).
#' @noRd
.loc_row <- function(ns, row, pops, default_pop, selected) {
  is_sel <- row$id %in% selected
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
    `data-ar-hay` = .loc_row_hay(row),
    shiny::tags$td(shiny::tags$div(
      class = "ar-loc-num-cell",
      shiny::tags$span(class = "ar-loc-glyph", .type_icon(row$type, 14)),
      shiny::tags$input(
        type = "text",
        class = "ar-input-flat ar-mono ar-loc-in",
        value = row$number,
        `aria-label` = "TLF number",
        onchange = cell_js("number")
      )
    )),
    shiny::tags$td(.loc_label_select(row, cell_js("number_label"))),
    shiny::tags$td(shiny::tags$input(
      type = "text",
      class = "ar-input-flat ar-loc-in ar-loc-title",
      value = row$title,
      `aria-label` = "Output title",
      onchange = cell_js("title")
    )),
    shiny::tags$td(.loc_pop_select(
      row,
      pops,
      default_pop,
      cell_js("population")
    )),
    shiny::tags$td(class = "ar-loc-stamp", .stamp(row$status)),
    shiny::tags$td(class = "ar-mono ar-dx-dim ar-loc-mod", row$modified)
  )
}

#' The single flat LoC table: one `.ar-dx-table` header + one `.loc_row()` per
#' row (already kind-sorted + rail-filtered by the caller). Replaces the old
#' per-group tables now the rail owns kind grouping.
#' @noRd
.loc_table <- function(ns, rows, pops, default_pop, selected) {
  shiny::tags$table(
    class = "ar-dx-table ar-loc-table",
    shiny::tags$thead(shiny::tags$tr(
      lapply(
        c("NUMBER", "LABEL", "TITLE", "POPULATION", "STATUS", "MODIFIED"),
        function(h) shiny::tags$th(h)
      )
    )),
    shiny::tags$tbody(
      lapply(rows, function(row) .loc_row(ns, row, pops, default_pop, selected))
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
      shiny::tagList(.icon("chevrons_left", 12), "Contents"),
      variant = "link",
      class = "ar-dx-bk"
    ),
    shiny::tags$span(class = "ar-dx-sep", "/"),
    shiny::tags$span(class = "ar-loc-crumb-title", obj@title),
    shiny::tags$div(class = "ar-bar-spacer"),
    # X closes the drill back to the List-of-Contents (same as the Contents
    # link + Esc) — the Report twin of Data's open-dataset close button.
    shiny::tags$button(
      id = ns("drill_close"),
      type = "button",
      class = "ar-icon-btn ar-dx-close action-button",
      `aria-label` = "Close output",
      .icon("close", 14)
    )
  )
}

# ---- server -------------------------------------------------------------

#' The List-of-Contents server: renders the CONTENTS rail, the flat editable
#' table, and the drill breadcrumb, and wires every action (rail kind-filter /
#' row select / drill / inline edit / toolbar edit-duplicate-delete / add /
#' keyboard nav) through the injected store.
#' @param id *The module namespace, matching `mod_contents_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_contents_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$rail <- shiny::renderUI({
      rows <- .toc_rows(store$rv$report, store$rv$broken, store$rv$stale)
      .contents_rail(
        ns,
        rows,
        store$rv$loc_group,
        store$rv$report_open,
        store$rv$loc_collapsed
      )
    })

    output$table <- shiny::renderUI({
      rows <- .toc_rows(
        store$rv$report,
        store$rv$broken,
        store$rv$stale,
        store$mtimes
      )
      if (length(rows) == 0L) {
        return(shiny::div(
          class = "ar-dx-empty",
          "No outputs yet \u2014 add one to begin."
        ))
      }
      grp <- store$rv$loc_group
      if (!is.null(grp)) {
        rows <- Filter(function(r) identical(r$kind, grp), rows)
      }
      pops <- store$rv$report@theme$populations %||% list()
      default_pop <- store$rv$report@theme$default_population %||% ""
      # Read loc_selected under isolate() so a pure SELECTION never re-renders
      # the table (that round-trip dimmed the page + lagged the click). The
      # highlight is applied client-side instead (bridge.js `ar-loc-select`);
      # this isolated read only stamps the selected rows when a REAL data change
      # (edit / add / delete) re-renders the table.
      .loc_table(
        ns,
        .loc_sort_all(rows),
        pops,
        default_pop,
        shiny::isolate(store$rv$loc_selected)
      )
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
    # while drilled (the desk shows instead) — keep all three computing so a
    # mode switch or a breadcrumb-return never lands on a blank surface.
    shiny::outputOptions(output, "rail", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "table", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "crumb", suspendWhenHidden = FALSE)

    # A CONTENTS folder body narrows the rail's output list AND the main table
    # to that kind; re-clicking the active folder clears back to all (the "All
    # outputs" root that used to clear it was removed). Does NOT
    # close an open drill — the rail persists, so filtering it is just "find
    # another output to switch to".
    shiny::observeEvent(input$group, {
      k <- input$group
      store$rv$loc_group <- if (
        nzchar(k) && !identical(k, store$rv$loc_group)
      ) {
        k
      } else {
        NULL
      }
    })

    # A folder chevron collapses/expands its nested outputs (server-held so a
    # re-render keeps the state) — the Report twin of Data's `src_toggle`.
    shiny::observeEvent(input$loc_toggle, {
      k <- input$loc_toggle
      cur <- store$rv$loc_collapsed
      store$rv$loc_collapsed <- if (k %in% cur) setdiff(cur, k) else c(cur, k)
    })

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

    # A main-table row click carries `{id, shift, meta, nonce}`. Plain click
    # single-selects; Cmd/Ctrl toggles; Shift range-selects from the anchor
    # (`selected`) through the VISIBLE (rail-filtered) display order.
    # `loc_selected` is what Delete acts on; `selected` is the anchor (drill /
    # Edit / Duplicate target), moving only on a non-shift click.
    shiny::observeEvent(input$row_click, {
      e <- input$row_click
      id <- if (is.list(e)) e$id else e
      if (is.null(id)) {
        return()
      }
      shift <- is.list(e) && isTRUE(e$shift)
      meta <- is.list(e) && isTRUE(e$meta)
      rows <- .toc_rows(store$rv$report, store$rv$broken, store$rv$stale)
      grp <- store$rv$loc_group
      if (!is.null(grp)) {
        rows <- Filter(function(r) identical(r$kind, grp), rows)
      }
      ordered <- vapply(.loc_sort_all(rows), `[[`, character(1), "id")
      store$rv$loc_selected <- .select_update(
        id,
        store$rv$selected,
        ordered,
        store$rv$loc_selected,
        shift = shift,
        meta = meta
      )
      if (!shift) {
        store$rv$selected <- id
      }
    })

    # Mirror the selection to the DOM client-side (no table re-render -> no
    # recalculating dim / click lag). The store stays the source of truth; this
    # echoes it whenever it changes (click, keyboard nav, or a programmatic
    # clear) so shift/meta ranges + arrow-key moves stay in sync. A plain click
    # also flips the class optimistically in bridge.js, so the highlight is
    # instant and this echo just reconciles.
    shiny::observeEvent(
      store$rv$loc_selected,
      {
        session$sendCustomMessage(
          "ar-loc-select",
          list(ids = as.list(store$rv$loc_selected))
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # Double-click a row -> drill into its paper + inspector.
    shiny::observeEvent(input$open, {
      drill_open(store, input$open)
    })

    # The breadcrumb Contents link + X (and Esc, via bridge.js) -> back to list.
    shiny::observeEvent(input$back, {
      drill_close(store)
    })
    shiny::observeEvent(input$drill_close, {
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

    # Toolbar Edit / Duplicate / Delete act on the SELECTED row (the Data-mode
    # View/Delete idiom: always enabled, a no-op when nothing is selected).
    # Edit drills into the paper + inspector — Data's "View data" twin.
    shiny::observeEvent(input$edit, {
      if (!is.null(store$rv$selected)) {
        drill_open(store, store$rv$selected)
      }
    })

    shiny::observeEvent(input$duplicate_sel, {
      if (!is.null(store$rv$selected)) {
        .duplicate_output(store, store$rv$selected)
      }
    })

    # Delete: confirm, then remove every SELECTED output (single- or
    # multi-select) in one commit so a single Cmd-Z undoes the whole batch.
    shiny::observeEvent(input$delete_sel, {
      sel <- store$rv$loc_selected
      if (length(sel) == 0L) {
        return()
      }
      shiny::showModal(.confirm_delete_modal(
        session$ns("confirm_delete_loc"),
        length(sel),
        "output",
        "They are removed from the report. Undo with Cmd-Z."
      ))
    })

    shiny::observeEvent(input$confirm_delete_loc, {
      ids <- store$rv$loc_selected
      new_report <- store$rv$report
      for (id in ids) {
        new_report <- .remove_object(new_report, id)
      }
      commit(
        store,
        new_report,
        label = sprintf(
          "remove %d output%s",
          length(ids),
          if (length(ids) == 1L) "" else "s"
        )
      )
      if (!is.null(store$rv$selected) && store$rv$selected %in% ids) {
        store$rv$selected <- NULL
      }
      store$rv$loc_selected <- character(0)
      shiny::removeModal()
    })

    shiny::observeEvent(input$add_output, {
      store$rv$adding <- TRUE
    })

    # Keyboard nav (Task 17): Up/Down walk the selection through the ids in
    # DISPLAY order (group order, number-sorted — matching the eye), posted
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
      # Arrow keys single-select (collapse any multi-select to the moved row).
      if (is.null(store$rv$selected)) {
        store$rv$selected <- ids[[1]]
        store$rv$loc_selected <- ids[[1]]
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
      store$rv$loc_selected <- ids[[nxt]]
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
  store$rv$loc_selected <- new_id
  invisible(new_id)
}
