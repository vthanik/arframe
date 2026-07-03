# The Ranks pane (design spec #8's fourth tab): ordering semantics, in one
# place, per generator --
#
#   summary/crosstab  the summarize items' ROW-BLOCK order (drag list;
#                     commits through the SAME `.reorder_slot()` helper the
#                     Roles pane uses, so the two panes can never disagree)
#   occurrence        `options$hier_sort` -- SOC/PT incidence order
#                     (frequency vs alphabetical), default-elided
#   line/box          `options$x_order` -- the x level order (the sortable
#                     level list relocated from the Options pane; the
#                     `.RANKS_KEYS` filter there is this pane's other half)
#   km                nothing to rank (time orders itself) -- an honest
#                     empty state, never a dead control
#
# Same contracts as the sibling panes: every commit through
# `update_object()`, state in the store (never the DOM), one shared
# sortable JS seam, always-mounted + `suspendWhenHidden = FALSE`.

# ---- content builders -------------------------------------------------------

#' The pane's honest empty state: name why there is nothing to rank (or
#' what to assign first) instead of a blank sheet.
#' @noRd
.ranks_empty <- function(text) {
  shiny::tags$div(
    class = "ar-insp-empty",
    shiny::tags$p(class = "ar-insp-empty-text", text)
  )
}

#' The row-block order editor (summary/crosstab): one grip row per
#' summarize item, dragging posts `rank_items` and commits through
#' `.reorder_slot()` -- identical semantics to reordering inside the Roles
#' fieldset, surfaced here because order IS the rank for a summary table.
#' @noRd
.ranks_items_section <- function(ns, object) {
  role <- .role_for_slot(object, "summarize")
  items <- if (is.null(role)) list() else role@items
  if (length(items) == 0L) {
    return(.ranks_empty(paste0(
      "Assign variables in Roles first ",
      "\u2014 their row-block order is ranked here."
    )))
  }
  .opt_section(
    "ROW BLOCKS",
    list(
      do.call(
        shiny::tags$div,
        c(
          list(
            class = "ar-opt-levels ar-rank-items",
            `data-ar-sortable` = "true",
            `data-ar-sortable-item` = ".ar-opt-level",
            `data-ar-sortable-attr` = "data-ar-item",
            `data-ar-sortable-input` = ns("rank_items")
          ),
          lapply(items, function(it) {
            shiny::tags$div(
              class = "ar-opt-level",
              `data-ar-item` = it@name,
              .icon("grip", 11),
              shiny::tags$span(class = "ar-rank-name ar-mono", it@name),
              if (nzchar(it@label %||% "")) {
                shiny::tags$span(class = "ar-rank-sub", it@label)
              }
            )
          })
        )
      ),
      shiny::tags$p(
        class = "ar-opt-hint",
        "Drag to set the order the blocks appear in the table."
      )
    )
  )
}

#' The SOC/PT incidence order control (occurrence): the engine's
#' `hier_sort` choice with humanized labels over the EXACT engine values.
#' @noRd
.ranks_hier_section <- function(ns, object) {
  current <- object@options$hier_sort %||% "freq"
  .opt_section(
    "INCIDENCE ORDER",
    list(
      shiny::radioButtons(
        ns("hier_sort"),
        label = NULL,
        choices = c(
          "Most frequent first" = "freq",
          "Alphabetical" = "alpha"
        ),
        selected = if (current %in% c("freq", "alpha")) current else "freq"
      ),
      shiny::tags$p(
        class = "ar-opt-hint",
        "Frequency ranks by pooled-arm incidence, ties alphabetical."
      )
    )
  )
}

#' The x level order control (line/box): the relocated `x_order` sortable
#' -- `.opt_levels_control()` unchanged, seeded from the committed order
#' else the x variable's distinct values.
#' @noRd
.ranks_xorder_section <- function(con, ns, object) {
  schema <- tryCatch(
    arpillar::option_schema(object@type),
    error = function(e) NULL
  )
  row <- if (is.null(schema)) {
    NULL
  } else {
    schema[schema$key == "x_order", , drop = FALSE]
  }
  if (is.null(row) || nrow(row) != 1L) {
    return(NULL)
  }
  control <- .opt_levels_control(con, ns, object, row)
  if (is.null(control)) {
    return(.ranks_empty(
      "Assign an X variable in Roles first \u2014 its levels are ranked here."
    ))
  }
  .opt_section(
    "X LEVEL ORDER",
    list(
      control,
      shiny::tags$p(
        class = "ar-opt-hint",
        "Drag to set the axis order; the engine default is data order."
      )
    )
  )
}

# ---- UI ---------------------------------------------------------------

#' The Ranks pane UI: server-rendered -- the content depends on the
#' selected object's generator.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_card_ranks_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pane"))
}

# ---- server -------------------------------------------------------------

#' The Ranks pane server. Redraws on selection + roles digest (the item
#' list and the x seed both hang off roles); a drag inside the pane leaves
#' the DOM already in the committed order, so no self-redraw is needed.
#' (Same known ceiling as the Options pane: an undo/redo while open can
#' leave control VALUES stale until the next redraw trigger.)
#' @param id *The module namespace, matching `mod_card_ranks_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_card_ranks_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$pane <- shiny::renderUI({
      obj <- selected_object(store)
      if (is.null(obj)) {
        return(shiny::tags$div(
          class = "ar-insp-empty",
          shiny::tags$p(
            class = "ar-insp-empty-text",
            paste0(
              "No output selected. Choose one in Contents, ",
              "or add one with the + button."
            )
          )
        ))
      }
      switch(
        obj@type,
        summary = ,
        crosstab = .ranks_items_section(ns, obj),
        occurrence = .ranks_hier_section(ns, obj),
        line = ,
        box = .ranks_xorder_section(store$con, ns, obj),
        km = .ranks_empty(
          "Kaplan\u2013Meier ranks itself along time \u2014 nothing to order."
        ),
        NULL
      )
    }) |>
      shiny::bindEvent(
        store$rv$selected,
        .roles_digest(selected_object(store))
      )

    # Row-block order (summary/crosstab): the SAME reconcile-and-commit the
    # Roles fieldset drag uses -- one helper, two surfaces, zero drift.
    shiny::observeEvent(input$rank_items, {
      obj_id <- store$rv$selected
      if (is.null(obj_id)) {
        return()
      }
      order <- vapply(input$rank_items$order, as.character, character(1))
      update_object(
        store,
        obj_id,
        function(o) .reorder_slot(o, "summarize", order),
        label = "rank row blocks"
      )
    })

    # Occurrence incidence order: engine values only, default elided.
    shiny::observeEvent(input$hier_sort, {
      obj <- selected_object(store)
      if (is.null(obj) || !input$hier_sort %in% c("freq", "alpha")) {
        return()
      }
      value <- if (identical(input$hier_sort, "freq")) NULL else "alpha"
      if (identical(value, obj@options$hier_sort)) {
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) {
          opts <- o@options
          opts$hier_sort <- value
          S7::set_props(o, options = opts)
        },
        label = "set hier_sort"
      )
    })

    # X level order (line/box): a drop commits the explicit order (the
    # NULL engine default never matches an explicit order, so no elision
    # branch is needed here).
    shiny::observeEvent(input$opt_reorder_x_order, {
      obj <- selected_object(store)
      if (is.null(obj)) {
        return()
      }
      order <- vapply(
        input$opt_reorder_x_order$order,
        as.character,
        character(1)
      )
      if (identical(order, obj@options$x_order)) {
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) {
          opts <- o@options
          opts$x_order <- order
          S7::set_props(o, options = opts)
        },
        label = "reorder x levels"
      )
    })

    # Always-mounted pane contract (see mod_card_options/mod_card_filters).
    shiny::outputOptions(output, "pane", suspendWhenHidden = FALSE)

    invisible(NULL)
  })
}
