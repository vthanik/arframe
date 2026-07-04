# The Filters pane (chips + editor, 2026-07-04 redesign): the Filters-tab
# content of the docked inspector. Presets FIRST (one chip per `*FL`
# population flag in the dataset, CDISC-mapped names, selected state when
# the committed set IS that flag; "Full set" clears), then one compact
# chip per predicate (`SAFFL = Y ×`, `AGE > 65 ×`) and a `+ Filter` chip.
# Clicking a chip opens THE editor card (GOV.UK stacked labelled controls:
# variable -> condition -> type-aware value with real level counts /
# range hint -> include missing); only one editor exists at a time, so
# the old 12-slot per-row observer pool collapsed to one set. Rows live
# in the store-side draft (`rv$filter_draft`, seeded from `object@filters`
# on selection change; the open chip index is `rv$filter_open` -- never
# the DOM); a row commits ONLY when complete -- the engine's `.filter_one`
# is drop-tolerant and would silently skip an incomplete predicate, so
# the chip wears an honest `incomplete` badge instead of letting it
# vanish. Every commit is a HEAVY edit (filters key the ARD): on a
# proofed output it marks the proof STALE (fct_store.R's run semantics);
# the live count beside the pane label stays live regardless --
# `filter_count()` is a bare DuckDB COUNT, not a re-typeset.

# ---- engine contract -------------------------------------------------------

# The EXACT op set arpillar's .filter_one compiles (plan Task 12). Order is
# display order in the op select.
.FILTER_OPS <- c("==", "!=", "%in%", ">", "<", ">=", "<=", "is.na", "not.na")

# The operator select shows these HUMANIZED names over the EXACT engine
# values -- the posted value is always a member of .FILTER_OPS, never a
# display string.
.FILTER_OP_LABELS <- c(
  "is" = "==",
  "is not" = "!=",
  "is any of" = "%in%",
  ">" = ">",
  "<" = "<",
  "\u2265" = ">=",
  "\u2264" = "<=",
  "is missing" = "is.na",
  "is present" = "not.na"
)

# CDISC population-flag conventions: flag column -> chip label. Any OTHER
# `*FL` category column in the dataset still gets a quick chip, labeled
# by its bare predicate ("DISCFL = Y").
.POPULATION_FLAGS <- c(
  SAFFL = "Safety population",
  ITTFL = "ITT population",
  EFFFL = "Efficacy population",
  FASFL = "Full analysis set",
  PPROTFL = "Per-protocol",
  RANDFL = "Randomized",
  ENRLFL = "Enrolled",
  COMPLFL = "Completers"
)

#' The canonical flag predicate a population chip writes.
#' @noRd
.flag_filter <- function(column) {
  list(column = column, op = "==", value = "Y")
}

#' The chip label for a flag column: the CDISC name when mapped, else the
#' bare predicate.
#' @noRd
.flag_label <- function(column) {
  lbl <- .POPULATION_FLAGS[column]
  if (length(lbl) == 1L && !is.na(lbl)) {
    return(unname(lbl))
  }
  paste0(column, " = Y")
}

#' Every population-flag candidate in the dataset: category columns ending
#' in `FL`, mapped flags first (in .POPULATION_FLAGS order), the rest
#' alphabetical.
#' @noRd
.population_flags <- function(items) {
  flags <- items$name[items$type == "category" & grepl("FL$", items$name)]
  c(
    intersect(names(.POPULATION_FLAGS), flags),
    sort(setdiff(flags, names(.POPULATION_FLAGS)))
  )
}

# The canonical safety-population predicate (the SAFFL chip's own output)
# -- kept named because tests and the paper tag pin it.
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

#' The paper tag's label: the population name when the committed set is a
#' single canonical flag predicate (`<FL> == "Y"`), else an honest count.
#' @noRd
.filters_tag_label <- function(filters) {
  if (length(filters) == 1L) {
    f <- filters[[1L]]
    col <- f$column %||% ""
    if (
      identical(f, .flag_filter(col)) &&
        grepl("FL$", col)
    ) {
      return(.flag_label(col))
    }
  }
  sprintf(
    "%d filter%s",
    length(filters),
    if (length(filters) == 1L) "" else "s"
  )
}

# ---- chips + editor UI (2026-07-04 redesign) ---------------------------

#' The compact chip text for one draft row: `SAFFL = Y`, `AGE > 65`,
#' `RACE in 3 values`, `AEDECOD is missing`, or `New filter` before a
#' column is picked.
#' @noRd
.filter_chip_label <- function(row) {
  col <- row$column %||% ""
  if (!nzchar(col)) {
    return("New filter")
  }
  op <- row$op %||% ""
  if (op %in% c("is.na", "not.na")) {
    word <- if (identical(op, "is.na")) "is missing" else "is present"
    return(paste(col, word))
  }
  vals <- row$value
  vals_chr <- as.character(vals[!is.na(vals)])
  shown <- if (anyNA(vals)) c(vals_chr, "(missing)") else vals_chr
  val_txt <- if (length(shown) == 0L) {
    "\u2026"
  } else if (length(shown) == 1L) {
    shown
  } else if (length(shown) == 2L) {
    paste(shown, collapse = ", ")
  } else {
    sprintf("%d values", length(shown))
  }
  op_txt <- if (identical(op, "%in%")) {
    if (length(shown) > 1L) "in" else "="
  } else if (identical(op, "==")) {
    "="
  } else if (identical(op, "!=")) {
    "\u2260"
  } else {
    op
  }
  paste(col, op_txt, val_txt)
}

#' One filter chip: the predicate summary, an honest `incomplete` badge
#' when the engine would drop the row, and the inline remove button.
#' Clicking the chip opens it in the editor card (`rv$filter_open`); the
#' remove button stops propagation so it never also opens the chip.
#' @noRd
.flt_chip <- function(ns, i, row, open) {
  open_js <- sprintf(
    "Shiny.setInputValue('%s', {i: %d, nonce: Date.now()}, {priority: 'event'})",
    ns("chip_open"),
    i
  )
  rm_js <- sprintf(
    "event.stopPropagation(); Shiny.setInputValue('%s', {i: %d, nonce: Date.now()}, {priority: 'event'})",
    ns("chip_rm"),
    i
  )
  shiny::tags$button(
    type = "button",
    class = paste0(
      "ar-flt-chip",
      if (isTRUE(open)) " ar-flt-chip-on",
      if (!.filter_complete(row)) " ar-flt-chip-bad"
    ),
    onclick = open_js,
    shiny::tags$span(class = "ar-mono", .filter_chip_label(row)),
    if (!.filter_complete(row)) {
      shiny::tags$span(class = "ar-flt-badge ar-mono", "incomplete")
    },
    shiny::tags$span(
      class = "ar-flt-chip-x",
      role = "button",
      `aria-label` = paste0("Remove filter ", i),
      onclick = rm_js,
      .icon("close", 10)
    )
  )
}

#' The value control for the OPEN row: hidden for null-tests, numeric
#' input with a live range hint for a measure comparison, multi selectize
#' over the dataset's real levels labeled with their row counts otherwise
#' (the NA level shows as "(missing)" via `.NA_TOKEN`).
#' @noRd
.flt_value_control <- function(con, ns, row, type, dataset) {
  if (row$op %in% c("is.na", "not.na")) {
    return(NULL)
  }
  if (identical(type, "measure")) {
    val <- row$value
    rng <- tryCatch(
      arpillar::column_range(con, dataset, row$column),
      error = function(e) NULL
    )
    return(shiny::tagList(
      shiny::textInput(
        ns("f_val"),
        label = NULL,
        value = if (is.null(val)) "" else as.character(val[[1]]),
        placeholder = "value"
      ),
      if (!is.null(rng)) {
        shiny::tags$div(
          class = "ar-flt-hint ar-mono",
          sprintf("Range %s to %s in %s", rng[[1]], rng[[2]], tolower(dataset))
        )
      }
    ))
  }
  counts <- tryCatch(
    arpillar::value_counts(con, dataset, row$column),
    error = function(e) NULL
  )
  levels <- tryCatch(
    arpillar::distinct_values(con, dataset, row$column, include_missing = TRUE),
    error = function(e) character(0)
  )
  labels <- vapply(
    levels,
    function(lv) {
      base <- if (is.na(lv)) "(missing)" else lv
      n <- if (!is.null(counts) && !is.na(lv)) counts[lv] else NA
      if (length(n) == 1L && !is.na(n)) sprintf("%s (%s) ", base, n) else base
    },
    character(1)
  )
  choices <- stats::setNames(ifelse(is.na(levels), .NA_TOKEN, levels), labels)
  selected <- row$value
  if (!is.null(selected)) {
    selected <- ifelse(is.na(selected), .NA_TOKEN, selected)
  }
  shiny::selectizeInput(
    ns("f_val"),
    label = NULL,
    choices = choices,
    selected = selected %||% character(0),
    multiple = TRUE,
    options = list(placeholder = "values")
  )
}

#' The editor card for the open chip (GOV.UK-style stacked labelled
#' controls): column picker, condition, type-aware value, include-missing,
#' Done. Rendered inline below the chip row -- the open chip is
#' highlighted, so no floating-popover positioning JS is needed and the
#' narrow rail never clips it.
#' @noRd
.flt_editor <- function(con, ns, items, row, dataset) {
  has_col <- nzchar(row$column %||% "")
  type <- if (has_col) {
    hit <- items$type[items$name == row$column]
    if (length(hit) == 1L) hit else "category"
  } else {
    "category"
  }
  # Packed NAME\x1fTYPE\x1fLABEL -- the re-seeded selection must match
  # `.eligible_picker()`'s packing or selectize resets the row.
  selected <- if (has_col) {
    lab_hit <- items$label[items$name == row$column]
    .pack_item_choice(
      row$column,
      type,
      if (length(lab_hit) == 1L) lab_hit else NA_character_
    )
  } else {
    character(0)
  }
  shiny::tags$div(
    class = "ar-flt-pop",
    shiny::tags$div(
      class = "ar-flt-field",
      shiny::tags$label(class = "ar-flt-lbl", "Variable"),
      .eligible_picker(
        ns,
        "f_col",
        items,
        selected = selected,
        placeholder = "Column"
      )
    ),
    if (has_col) {
      shiny::tags$div(
        class = "ar-flt-field",
        shiny::tags$label(class = "ar-flt-lbl", "Condition"),
        shiny::selectInput(
          ns("f_op"),
          label = NULL,
          choices = .FILTER_OP_LABELS,
          selected = row$op,
          selectize = FALSE
        )
      )
    },
    if (has_col && !row$op %in% c("is.na", "not.na")) {
      shiny::tags$div(
        class = "ar-flt-field",
        shiny::tags$label(class = "ar-flt-lbl", "Value"),
        .flt_value_control(con, ns, row, type, dataset)
      )
    },
    if (has_col && !row$op %in% c("is.na", "not.na")) {
      shiny::checkboxInput(
        ns("f_miss"),
        label = "Include missing",
        value = isTRUE(row$include_missing)
      )
    },
    .action_btn(
      ns("f_done"),
      shiny::tagList(.icon("check", 11), "Done"),
      class = "ar-flt-done"
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
    # a draft never survives across outputs, and neither does an open
    # editor (a stale filter_open would index into the wrong draft).
    shiny::observe({
      obj <- selected_object(store)
      store$rv$filter_draft <- if (is.null(obj)) {
        list()
      } else {
        .seed_draft(obj@filters)
      }
      store$rv$filter_open <- NULL
    }) |>
      shiny::bindEvent(store$rv$selected, ignoreNULL = FALSE)

    # The open row, bounds-guarded: NULL when no editor is open OR the
    # index outlived the draft it pointed into.
    open_row <- function() {
      i <- store$rv$filter_open
      draft <- store$rv$filter_draft
      if (is.null(i) || i < 1L || i > length(draft)) {
        return(NULL)
      }
      i
    }

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
      open_i <- open_row()
      committed <- lapply(Filter(.filter_complete, draft), .filter_normalize)
      shiny::tagList(
        shiny::tags$div(
          class = "ar-flt-head",
          shiny::tags$span(class = "ar-label", "POPULATION"),
          shiny::uiOutput(ns("count"), inline = TRUE)
        ),
        do.call(
          shiny::tags$div,
          c(
            list(class = "ar-flt-presets"),
            # One chip per population-flag candidate (`*FL` category
            # columns): CDISC-mapped names first, the rest by their bare
            # predicate. Dynamic set -> ONE shared `preset_flag` input.
            # A preset wears the selected state when the complete draft
            # IS exactly its canonical predicate (one chip language,
            # 2026-07-04).
            lapply(.population_flags(items), function(fl) {
              js <- sprintf(
                "Shiny.setInputValue('%s', {column: '%s', nonce: Date.now()}, {priority: 'event'})",
                ns("preset_flag"),
                fl
              )
              on <- identical(committed, list(.flag_filter(fl)))
              shiny::tags$button(
                type = "button",
                class = paste0(
                  "ar-flt-preset btn btn-default action-button",
                  if (on) " ar-flt-preset-on"
                ),
                onclick = js,
                if (on) .icon("check", 10),
                .flag_label(fl)
              )
            }),
            list(
              .action_btn(
                ns("preset_full"),
                shiny::tagList(
                  if (length(committed) == 0L) .icon("check", 10),
                  "Full set"
                ),
                class = paste0(
                  "ar-flt-preset",
                  if (length(committed) == 0L) " ar-flt-preset-on"
                )
              )
            )
          )
        ),
        shiny::tags$div(class = "ar-label ar-flt-sec", "FILTERS"),
        do.call(
          shiny::tags$div,
          c(
            list(class = "ar-flt-chips"),
            lapply(seq_along(draft), function(i) {
              .flt_chip(ns, i, draft[[i]], open = identical(open_i, i))
            }),
            list(
              if (length(draft) < .FILTER_MAX_ROWS) {
                .action_btn(
                  ns("f_add"),
                  shiny::tagList(.icon("plus", 10), "Filter"),
                  variant = "link",
                  class = "ar-flt-add"
                )
              }
            )
          )
        ),
        if (!is.null(open_i)) {
          .flt_editor(store$con, ns, items, draft[[open_i]], obj@dataset)
        }
      )
    }) |>
      shiny::bindEvent(
        store$rv$selected,
        store$rv$filter_draft,
        store$rv$filter_open,
        ignoreNULL = FALSE
      )

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
    shiny::observeEvent(input$preset_flag, {
      col <- input$preset_flag$column
      if (is.null(col) || !nzchar(col)) {
        return()
      }
      store$rv$filter_draft <- .seed_draft(list(.flag_filter(col)))
      .commit_filters(store)
    })

    shiny::observeEvent(input$preset_full, {
      store$rv$filter_draft <- list()
      .commit_filters(store)
    })

    # + Filter appends a blank row AND opens it in the editor.
    shiny::observeEvent(input$f_add, {
      store$rv$filter_draft <- c(
        store$rv$filter_draft,
        list(list(column = "", op = "", value = NULL, include_missing = FALSE))
      )
      store$rv$filter_open <- length(store$rv$filter_draft)
    })

    # ---- chips ----
    shiny::observeEvent(input$chip_open, {
      # JS posts numbers as double; the draft index is stored integer.
      i <- as.integer(input$chip_open$i %||% 0L)
      if (i < 1L || i > length(store$rv$filter_draft)) {
        return()
      }
      # Clicking the open chip closes the editor; another chip moves it.
      store$rv$filter_open <- if (identical(store$rv$filter_open, i)) {
        NULL
      } else {
        i
      }
    })

    shiny::observeEvent(input$chip_rm, {
      i <- as.integer(input$chip_rm$i %||% 0L)
      draft <- store$rv$filter_draft
      if (i < 1L || i > length(draft)) {
        return()
      }
      draft[[i]] <- NULL
      store$rv$filter_draft <- draft
      open_i <- store$rv$filter_open
      if (!is.null(open_i)) {
        store$rv$filter_open <- if (identical(open_i, i)) {
          NULL
        } else if (open_i > i) {
          open_i - 1L
        } else {
          open_i
        }
      }
      .commit_filters(store)
    })

    shiny::observeEvent(input$f_done, {
      store$rv$filter_open <- NULL
    })

    # ---- the editor observer set ----
    # ONE set of observers (f_col/f_op/f_val/f_miss) targets the single
    # open row -- only one editor exists at a time (2026-07-04; replaces
    # the old 12-slot per-row pool). `val_seen` guards the value observer
    # against the init-NULL every freshly bound input posts once: NULL is
    # only meaningful ("user removed every selection") after a real post,
    # and the guard resets whenever a different row opens.
    val_seen <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(
      store$rv$filter_open,
      val_seen(FALSE),
      ignoreNULL = FALSE
    )

    shiny::observeEvent(input$f_col, {
      choice <- input$f_col
      ii <- open_row()
      if (is.null(choice) || !nzchar(choice) || is.null(ii)) {
        return()
      }
      draft <- store$rv$filter_draft
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

    shiny::observeEvent(input$f_op, {
      op <- input$f_op
      ii <- open_row()
      draft <- store$rv$filter_draft
      if (
        is.null(op) ||
          !op %in% .FILTER_OPS ||
          is.null(ii) ||
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
      input$f_val,
      {
        raw <- input$f_val
        if (is.null(raw) && !isTRUE(val_seen())) {
          return()
        }
        if (!is.null(raw)) {
          val_seen(TRUE)
        }
        ii <- open_row()
        if (is.null(ii)) {
          return()
        }
        draft <- store$rv$filter_draft
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

    shiny::observeEvent(input$f_miss, {
      flag <- isTRUE(input$f_miss)
      ii <- open_row()
      draft <- store$rv$filter_draft
      if (is.null(ii) || identical(draft[[ii]]$include_missing, flag)) {
        return()
      }
      draft[[ii]]$include_missing <- flag
      store$rv$filter_draft <- draft
      .commit_filters(store)
    })

    invisible(NULL)
  })
}
