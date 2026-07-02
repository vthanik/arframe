# The Filters pane (design spec #8, plan Task 12): the Filters-tab content
# of the docked inspector. Presets FIRST ("Safety population" when the
# bound dataset carries SAFFL; "Full set" clears), then builder rows --
# column (rich picker) - op (the EXACT engine set) - value (multi selectize
# over distinct values for category/date, numeric text for measures, hidden
# for null-tests) - include-missing - remove. Rows live in the store-side
# draft (`rv$filter_draft`, seeded from `object@filters` on selection
# change; never the DOM); a row commits ONLY when complete -- the engine's
# `.filter_one` is drop-tolerant and would silently skip an incomplete
# predicate, so the pane shows an honest `incomplete` badge instead of
# letting it vanish. Every commit is a HEAVY edit (filters key the ARD):
# on a proofed output it marks the proof STALE (fct_store.R's run
# semantics); the live count beside the pane label stays live regardless --
# `filter_count()` is a bare DuckDB COUNT, not a re-typeset.

# ---- engine contract -------------------------------------------------------

# The EXACT op set arpillar's .filter_one compiles (plan Task 12). Order is
# display order in the op select.
.FILTER_OPS <- c("==", "!=", "%in%", ">", "<", ">=", "<=", "is.na", "not.na")

# The canonical safety-population predicate the preset writes -- also what
# the paper tag recognizes (`.filters_tag_label`).
.SAFETY_FILTER <- list(column = "SAFFL", op = "==", value = "Y")

# The selectize token standing in for a real NA level ("(missing)") --
# selectize cannot carry NA itself.
.NA_TOKEN <- "__NA__"

# ponytail: fixed 12-row observer pool (bounded static registration, the
# Roles-module pattern); dynamic registration if anyone ever files >12.
.FILTER_MAX_ROWS <- 12L

#' Is this draft row compilable by the engine? Mirrors
#' `arpillar:::.filter_one`'s own drop rules: a known column + a known op,
#' and (unless a null-test) at least one real value OR the include-missing
#' fold.
#' @noRd
.filter_complete <- function(f) {
  if (!nzchar(f$column %||% "") || !((f$op %||% "") %in% .FILTER_OPS)) {
    return(FALSE)
  }
  if (f$op %in% c("is.na", "not.na")) {
    return(TRUE)
  }
  vals <- f$value
  isTRUE(f$include_missing) ||
    (!is.null(vals) && anyNA(vals)) ||
    length(vals[!is.na(vals)]) > 0L
}

#' The minimal committed shape for one complete row: no `value` on a
#' null-test, no `include_missing` key when FALSE -- so a hand-built
#' safety row and the preset's canonical shape are `identical()`.
#' @noRd
.filter_normalize <- function(f) {
  out <- list(column = f$column, op = f$op)
  if (!f$op %in% c("is.na", "not.na")) {
    out$value <- f$value
    if (isTRUE(f$include_missing)) {
      out$include_missing <- TRUE
    }
  }
  out
}

#' Draft rows from committed predicates (selection-change seeding).
#' @noRd
.seed_draft <- function(filters) {
  lapply(filters, function(f) {
    list(
      column = f$column %||% "",
      op = f$op %||% "",
      value = f$value,
      include_missing = isTRUE(f$include_missing)
    )
  })
}

#' The paper tag's label: the preset's name when the committed set IS the
#' canonical safety predicate, else an honest count.
#' @noRd
.filters_tag_label <- function(filters) {
  if (identical(filters, list(.SAFETY_FILTER))) {
    return("Safety population")
  }
  sprintf(
    "%d filter%s",
    length(filters),
    if (length(filters) == 1L) "" else "s"
  )
}

# ---- row UI -----------------------------------------------------------

#' The value control for one row: hidden for null-tests, numeric text for
#' a measure comparison, multi selectize over
#' `distinct_values(include_missing = TRUE)` otherwise (the NA level shows
#' as "(missing)" via `.NA_TOKEN`).
#' @noRd
.flt_value_control <- function(con, ns, i, row, type, dataset) {
  if (row$op %in% c("is.na", "not.na")) {
    return(NULL)
  }
  input_id <- paste0("f_val_", i)
  if (identical(type, "measure")) {
    val <- row$value
    return(shiny::textInput(
      ns(input_id),
      label = NULL,
      value = if (is.null(val)) "" else as.character(val[[1]]),
      placeholder = "value"
    ))
  }
  levels <- tryCatch(
    arpillar::distinct_values(con, dataset, row$column, include_missing = TRUE),
    error = function(e) character(0)
  )
  choices <- stats::setNames(
    ifelse(is.na(levels), .NA_TOKEN, levels),
    ifelse(is.na(levels), "(missing)", levels)
  )
  selected <- row$value
  if (!is.null(selected)) {
    selected <- ifelse(is.na(selected), .NA_TOKEN, selected)
  }
  shiny::selectizeInput(
    ns(input_id),
    label = NULL,
    choices = choices,
    selected = selected %||% character(0),
    multiple = TRUE,
    options = list(placeholder = "values")
  )
}

#' One builder row: column picker, then (once a column is set) op select,
#' value control, include-missing checkbox, and the remove button; an
#' incomplete row wears the honest badge.
#' @noRd
.flt_row <- function(con, ns, items, i, row, dataset) {
  has_col <- nzchar(row$column %||% "")
  type <- if (has_col) {
    hit <- items$type[items$name == row$column]
    if (length(hit) == 1L) hit else "category"
  } else {
    "category"
  }
  # The picker's choices are packed with the raw SQL type (that is what
  # `.eligible_picker()` builds and what search matches on) -- the
  # re-seeded selection must be packed the SAME way or selectize ignores
  # it and the bind-post resets the row to the first column.
  selected <- if (has_col) {
    sql_hit <- items$sql_type[items$name == row$column]
    .pack_item_choice(row$column, if (length(sql_hit) == 1L) sql_hit else "")
  } else {
    character(0)
  }

  shiny::tags$div(
    class = "ar-flt-row",
    shiny::tags$div(
      class = "ar-flt-row-main",
      .eligible_picker(
        ns,
        paste0("f_col_", i),
        items,
        selected = selected,
        placeholder = "Column"
      ),
      if (has_col) {
        shiny::selectInput(
          ns(paste0("f_op_", i)),
          label = NULL,
          choices = .FILTER_OPS,
          selected = row$op,
          selectize = FALSE,
          width = "84px"
        )
      },
      if (has_col) .flt_value_control(con, ns, i, row, type, dataset),
      shiny::tags$button(
        id = ns(paste0("f_rm_", i)),
        type = "button",
        class = "ar-icon-btn ar-flt-rm action-button",
        `aria-label` = paste0("Remove filter ", i),
        .icon("close", 11)
      )
    ),
    shiny::tags$div(
      class = "ar-flt-row-meta",
      if (has_col && !row$op %in% c("is.na", "not.na")) {
        shiny::checkboxInput(
          ns(paste0("f_miss_", i)),
          label = "include missing",
          value = isTRUE(row$include_missing)
        )
      },
      if (!.filter_complete(row)) {
        shiny::tags$span(class = "ar-flt-badge ar-mono", "incomplete")
      }
    )
  )
}

# ---- UI ---------------------------------------------------------------

#' The Filters pane UI: a server-rendered stack (presets depend on the
#' bound dataset's columns; rows on the draft).
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_card_filters_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pane"))
}

# ---- server -------------------------------------------------------------

#' Commit the draft's COMPLETE rows to the selected object -- a no-op when
#' the committed set already matches (bind-time reposts and incomplete
#' edits never push an undo entry).
#' @noRd
.commit_filters <- function(store) {
  obj <- selected_object(store)
  if (is.null(obj)) {
    return(invisible(NULL))
  }
  complete <- lapply(
    Filter(.filter_complete, store$rv$filter_draft),
    .filter_normalize
  )
  if (identical(complete, obj@filters)) {
    return(invisible(NULL))
  }
  update_object(
    store,
    obj@id,
    function(o) S7::set_props(o, filters = complete),
    label = "edit filters"
  )
  invisible(NULL)
}

#' The Filters pane server: seeds the draft on selection change, renders
#' presets + builder rows, and wires the fixed observer pool. The pane
#' redraws on any draft reassignment (rows are few; a redraw re-seeds each
#' control from the draft, so no state lives in the DOM).
#' @param id *The module namespace, matching `mod_card_filters_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_card_filters_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Selection change re-seeds the draft from the committed predicates --
    # a draft never survives across outputs.
    shiny::observe({
      obj <- selected_object(store)
      store$rv$filter_draft <- if (is.null(obj)) {
        list()
      } else {
        .seed_draft(obj@filters)
      }
    }) |>
      shiny::bindEvent(store$rv$selected, ignoreNULL = FALSE)

    output$pane <- shiny::renderUI({
      obj <- selected_object(store)
      if (is.null(obj)) {
        return(shiny::div(class = "ar-flt-empty"))
      }
      items <- tryCatch(
        arpillar::data_items(store$con, obj@dataset),
        error = function(e) NULL
      )
      if (is.null(items)) {
        return(shiny::div(class = "ar-flt-empty"))
      }
      draft <- store$rv$filter_draft
      shiny::tagList(
        shiny::tags$div(
          class = "ar-flt-head",
          shiny::tags$span(class = "ar-label", "POPULATION"),
          shiny::uiOutput(ns("count"), inline = TRUE)
        ),
        shiny::tags$div(
          class = "ar-flt-presets",
          if ("SAFFL" %in% items$name) {
            .action_btn(
              ns("preset_safety"),
              "Safety population",
              class = "ar-flt-preset"
            )
          },
          .action_btn(ns("preset_full"), "Full set", class = "ar-flt-preset")
        ),
        lapply(seq_along(draft), function(i) {
          .flt_row(store$con, ns, items, i, draft[[i]], obj@dataset)
        }),
        if (length(draft) < .FILTER_MAX_ROWS) {
          .action_btn(
            ns("f_add"),
            "+ Add filter",
            variant = "link",
            class = "ar-flt-add"
          )
        }
      )
    }) |>
      shiny::bindEvent(store$rv$selected, store$rv$filter_draft)

    # The live count: filter_count() over the COMPLETE predicates only,
    # debounced 300ms -- a half-built row never fires a query.
    complete_preds <- shiny::reactive({
      lapply(Filter(.filter_complete, store$rv$filter_draft), .filter_normalize)
    })
    complete_deb <- shiny::debounce(complete_preds, 300)
    output$count <- shiny::renderUI({
      obj <- selected_object(store)
      if (is.null(obj)) {
        return(NULL)
      }
      counts <- tryCatch(
        arpillar::filter_count(store$con, obj@dataset, complete_deb()),
        error = function(e) NULL
      )
      if (is.null(counts)) {
        return(NULL)
      }
      shiny::tags$span(
        class = "ar-flt-count ar-mono",
        sprintf(
          "%s of %s",
          format(counts$matched, big.mark = ","),
          format(counts$total, big.mark = ",")
        )
      )
    })

    # The inspector tab flip is a pure client-side class change the server
    # never sees (the mod_data lesson): a hidden pane's outputs must keep
    # computing or the Filters tab opens blank.
    shiny::outputOptions(output, "pane", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "count", suspendWhenHidden = FALSE)

    # ---- presets ----
    shiny::observeEvent(input$preset_safety, {
      store$rv$filter_draft <- .seed_draft(list(.SAFETY_FILTER))
      .commit_filters(store)
    })

    shiny::observeEvent(input$preset_full, {
      store$rv$filter_draft <- list()
      .commit_filters(store)
    })

    shiny::observeEvent(input$f_add, {
      store$rv$filter_draft <- c(
        store$rv$filter_draft,
        list(list(column = "", op = "", value = NULL, include_missing = FALSE))
      )
    })

    # ---- the row observer pool ----
    # Which f_val_<i> inputs have posted a real value at least once: the
    # value observer must accept NULL ("user removed every selection")
    # but NOT the NULL every input carries before its control first binds
    # -- processing that init-NULL would wipe a freshly seeded draft value.
    val_seen <- new.env(parent = emptyenv())

    for (row_i in seq_len(.FILTER_MAX_ROWS)) {
      local({
        ii <- row_i

        shiny::observeEvent(input[[paste0("f_col_", ii)]], {
          choice <- input[[paste0("f_col_", ii)]]
          draft <- store$rv$filter_draft
          if (is.null(choice) || !nzchar(choice) || ii > length(draft)) {
            return()
          }
          name <- .unpack_item_name(choice)
          row <- draft[[ii]]
          if (identical(row$column, name)) {
            return()
          }
          # The packed half is the raw SQL type; the op default needs the
          # ROLE type (measure vs category/date) -- look it up.
          obj <- selected_object(store)
          if (is.null(obj)) {
            return()
          }
          items <- arpillar::data_items(store$con, obj@dataset)
          hit <- items$type[items$name == name]
          type <- if (length(hit) == 1L) hit else "category"
          row$column <- name
          # The op default follows the column's shape: set-membership for
          # category/date, comparison for a measure.
          row$op <- if (identical(type, "measure")) "==" else "%in%"
          row$value <- NULL
          row$include_missing <- FALSE
          draft[[ii]] <- row
          store$rv$filter_draft <- draft
          .commit_filters(store)
        })

        shiny::observeEvent(input[[paste0("f_op_", ii)]], {
          op <- input[[paste0("f_op_", ii)]]
          draft <- store$rv$filter_draft
          if (
            is.null(op) ||
              !op %in% .FILTER_OPS ||
              ii > length(draft) ||
              identical(draft[[ii]]$op, op)
          ) {
            return()
          }
          draft[[ii]]$op <- op
          if (op %in% c("is.na", "not.na")) {
            draft[[ii]]$value <- NULL
            draft[[ii]]$include_missing <- FALSE
          }
          store$rv$filter_draft <- draft
          .commit_filters(store)
        })

        shiny::observeEvent(
          input[[paste0("f_val_", ii)]],
          {
            raw <- input[[paste0("f_val_", ii)]]
            if (is.null(raw) && !isTRUE(val_seen[[as.character(ii)]])) {
              return()
            }
            if (!is.null(raw)) {
              val_seen[[as.character(ii)]] <- TRUE
            }
            draft <- store$rv$filter_draft
            if (ii > length(draft)) {
              return()
            }
            row <- draft[[ii]]
            obj <- selected_object(store)
            if (is.null(obj) || !nzchar(row$column %||% "")) {
              return()
            }
            items <- arpillar::data_items(store$con, obj@dataset)
            hit <- items$type[items$name == row$column]
            type <- if (length(hit) == 1L) hit else "category"
            value <- if (identical(type, "measure")) {
              v <- suppressWarnings(as.numeric(trimws(raw %||% "")))
              if (length(v) == 1L && !is.na(v)) v else NULL
            } else if (length(raw) == 0L) {
              NULL
            } else {
              # Map the "(missing)" token back to a real NA level.
              out <- as.character(raw)
              out[out == .NA_TOKEN] <- NA_character_
              out
            }
            if (identical(row$value, value)) {
              return()
            }
            draft[[ii]]$value <- value
            store$rv$filter_draft <- draft
            .commit_filters(store)
          },
          ignoreNULL = FALSE
        )

        shiny::observeEvent(input[[paste0("f_miss_", ii)]], {
          flag <- isTRUE(input[[paste0("f_miss_", ii)]])
          draft <- store$rv$filter_draft
          if (
            ii > length(draft) || identical(draft[[ii]]$include_missing, flag)
          ) {
            return()
          }
          draft[[ii]]$include_missing <- flag
          store$rv$filter_draft <- draft
          .commit_filters(store)
        })

        shiny::observeEvent(input[[paste0("f_rm_", ii)]], {
          draft <- store$rv$filter_draft
          if (ii > length(draft)) {
            return()
          }
          draft[[ii]] <- NULL
          store$rv$filter_draft <- draft
          .commit_filters(store)
        })
      })
    }

    invisible(NULL)
  })
}
