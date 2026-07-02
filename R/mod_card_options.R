# The Options pane (design spec #8, plan Task 11): the Options-tab content
# of the docked inspector. Three stacked sections -- TITLE (editable TLF
# number + label word + title, per the addendum's editable-numbering
# decision), FOOTNOTES (line editor; line 1 is the population statement by
# convention), and the schema-generated option rows
# (`arpillar::option_schema(type)`), grouped under micro-labels by the
# paper region each key belongs to. Every commit goes through
# `update_object()`; a value equal to the engine default REMOVES the key
# (default-elision -- keeps report.json and `emit_code()` output minimal).
#
# v5 note: unlike the Roles pane, this pane never narrows on `rv$region` --
# all four option-owning regions (title/footnotes/series/legend) route to
# this one tab, and the full stack is small enough to show whole. A `title`
# region click additionally focuses the Title input (`ar-focus`).

# ---- key -> paper-region grouping ----------------------------------------

#' Which paper region an option key belongs to -- the plan's routing table
#' (Task 11), used here as SECTION grouping labels, not as filters. A key
#' this map has not seen lands in the trailing "options" group.
#' @noRd
.OPT_REGION <- c(
  decimals = "rows",
  error_type = "axes",
  ci_level = "axes",
  mean_diamond = "axes",
  event_value = "axes",
  ci = "axes",
  risk_table = "axes",
  censor_marks = "axes",
  time_breaks = "axes",
  x_order = "axes",
  x_label = "axes",
  y_label = "axes",
  palette = "series",
  legend_position = "legend"
)

# Display order + micro-label for the option-row groups.
.OPT_SECTIONS <- c(
  rows = "ROWS",
  axes = "AXES",
  series = "SERIES",
  legend = "LEGEND",
  options = "OPTIONS"
)

# ---- parse / defaults -----------------------------------------------------

#' Parse one typed option value by schema kind. Returns `list(ok, value)`:
#' `ok = FALSE` means invalid input (show the inline message, commit
#' nothing); `ok = TRUE, value = NULL` means "remove the key" (an emptied
#' input falls back to the engine default).
#' @noRd
.opt_parse <- function(kind, raw) {
  raw <- trimws(raw %||% "")
  if (!nzchar(raw)) {
    return(list(ok = TRUE, value = NULL))
  }
  if (identical(kind, "int")) {
    if (!grepl("^-?[0-9]+$", raw)) {
      return(list(ok = FALSE, value = NULL))
    }
    return(list(ok = TRUE, value = as.integer(raw)))
  }
  if (identical(kind, "numvec")) {
    parts <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0L) {
      return(list(ok = TRUE, value = NULL))
    }
    vals <- suppressWarnings(as.numeric(parts))
    if (anyNA(vals)) {
      return(list(ok = FALSE, value = NULL))
    }
    return(list(ok = TRUE, value = sort(vals)))
  }
  # text
  list(ok = TRUE, value = raw)
}

#' The schema default for one option row, normalized to the type the
#' parsed/committed value will carry (the schema stores km's int defaults
#' as doubles -- `identical()` against a parsed `2L` needs the coercion).
#' @noRd
.opt_default <- function(row) {
  d <- row$default[[1]]
  if (is.null(d)) {
    return(NULL)
  }
  switch(
    row$kind,
    int = as.integer(d),
    flag = isTRUE(d),
    numvec = as.numeric(d),
    d
  )
}

#' The effective current value for one option row: the committed option,
#' else the engine default.
#' @noRd
.opt_current <- function(object, row) {
  object@options[[row$key]] %||% .opt_default(row)
}

# ---- controls ---------------------------------------------------------

#' A text input carrying `inputmode="numeric"` -- the plan's int control
#' (mobile numeric keyboard, no spinner chrome).
#' @noRd
.opt_numeric_text <- function(ns, key, value) {
  ti <- shiny::textInput(ns(paste0("opt_", key)), label = NULL, value = value)
  htmltools::tagQuery(ti)$find("input")$addAttrs(
    inputmode = "numeric"
  )$allTags()
}

#' The sortable level list for a `levels`-kind key, seeded from the
#' committed order else the x variable's own distinct values. Reuses the
#' `data-ar-sortable` JS contract the Contents TOC and Roles slots already
#' bind. Returns `NULL` (row skipped) while the x slot is unfilled -- there
#' is nothing to order yet.
#' @noRd
.opt_levels_control <- function(con, ns, object, row) {
  x_role <- .role_for_slot(object, "x")
  if (is.null(x_role) || length(x_role@items) == 0L) {
    return(NULL)
  }
  x_var <- x_role@items[[1]]@name
  seed <- object@options[[row$key]] %||%
    tryCatch(
      arpillar::distinct_values(con, object@dataset, x_var),
      error = function(e) NULL
    )
  if (is.null(seed)) {
    return(NULL)
  }
  shiny::tags$div(
    class = "ar-opt-levels",
    `data-ar-sortable` = "true",
    `data-ar-sortable-item` = ".ar-opt-level",
    `data-ar-sortable-attr` = "data-ar-item",
    `data-ar-sortable-input` = ns(paste0("opt_reorder_", row$key)),
    lapply(seed, function(lv) {
      shiny::tags$div(
        class = "ar-opt-level ar-mono",
        `data-ar-item` = lv,
        .icon("grip", 11),
        shiny::tags$span(lv)
      )
    })
  )
}

#' One option row: label + the kind-matched control (`int` numeric text,
#' `choice` radios with the engine default preselected, `flag` checkbox,
#' `text`/`numvec` text input, `levels` sortable list). Returns `NULL` for
#' a levels row whose seed variable is not assigned yet.
#' @noRd
.opt_control <- function(con, ns, object, row) {
  key <- row$key
  current <- .opt_current(object, row)
  input_id <- paste0("opt_", key)
  control <- switch(
    row$kind,
    int = .opt_numeric_text(
      ns,
      key,
      if (is.null(current)) "" else as.character(current)
    ),
    numvec = shiny::textInput(
      ns(input_id),
      label = NULL,
      value = if (is.null(current)) "" else paste(current, collapse = ", ")
    ),
    text = shiny::textInput(
      ns(input_id),
      label = NULL,
      value = current %||% ""
    ),
    flag = shiny::checkboxInput(
      ns(input_id),
      label = NULL,
      value = isTRUE(current)
    ),
    choice = shiny::radioButtons(
      ns(input_id),
      label = NULL,
      choices = row$choices[[1]],
      selected = current,
      inline = TRUE
    ),
    levels = .opt_levels_control(con, ns, object, row),
    NULL
  )
  if (is.null(control)) {
    return(NULL)
  }
  block <- identical(row$kind, "levels")
  shiny::tags$div(
    class = paste0("ar-opt-row", if (block) " ar-opt-row-block"),
    shiny::tags$span(class = "ar-opt-label", row$label),
    control
  )
}

# ---- sections ---------------------------------------------------------

#' One pane section: micro-label + content rows.
#' @noRd
.opt_section <- function(label, rows) {
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(NULL)
  }
  shiny::tags$div(
    class = "ar-opt-sec",
    shiny::tags$span(class = "ar-label ar-opt-sec-label", label),
    rows
  )
}

#' The TITLE section: number + label word (both editable, addendum
#' decision -- numbering is SAP-shell driven metadata, never re-derived)
#' and the title line.
#' @noRd
.opt_title_section <- function(ns, object) {
  labels <- c("Table", "Figure", "Listing")
  current_label <- object@options$number_label %||% ""
  .opt_section(
    "TITLE",
    list(
      shiny::tags$div(
        class = "ar-opt-number ar-mono",
        shiny::selectInput(
          ns("number_label"),
          label = NULL,
          choices = labels,
          selected = if (current_label %in% labels) current_label else "Table",
          selectize = FALSE,
          width = "90px"
        ),
        shiny::textInput(
          ns("number"),
          label = NULL,
          value = object@options$number %||% "",
          placeholder = "14.1.1"
        )
      ),
      shiny::textInput(
        ns("title"),
        label = NULL,
        value = object@title,
        placeholder = "Output title"
      )
    )
  )
}

#' One footnote line: the population tag on line 1 (the convention the
#' paper's `.population_line()` reads), a plain text input posting through
#' the shared `fn_edit` input on change (blur/Enter -- footnotes are
#' sentences, not live-typed previews), and a remove button posting
#' `fn_remove`. Dynamic per-line controls use the same
#' single-shared-input pattern as `.assigned_row()`/`.toc_kebab()`.
#' @noRd
.fn_row <- function(ns, i, value) {
  edit_js <- sprintf(
    "Shiny.setInputValue('%s', {i: %d, value: this.value, nonce: Date.now()}, {priority: 'event'})",
    ns("fn_edit"),
    i
  )
  remove_js <- sprintf(
    "Shiny.setInputValue('%s', {i: %d, nonce: Date.now()}, {priority: 'event'})",
    ns("fn_remove"),
    i
  )
  shiny::tags$div(
    class = "ar-fn-row",
    shiny::tags$input(
      type = "text",
      class = "ar-fn-input",
      value = value,
      onchange = edit_js,
      `aria-label` = paste0("Footnote ", i)
    ),
    if (i == 1L) {
      shiny::tags$span(class = "ar-fn-pop ar-mono", "population")
    },
    shiny::tags$button(
      type = "button",
      class = "ar-icon-btn ar-fn-remove",
      `aria-label` = paste0("Remove footnote ", i),
      onclick = remove_js,
      .icon("close", 11)
    )
  )
}

#' The FOOTNOTES section: one row per line + the add button.
#' @noRd
.opt_footnotes_section <- function(ns, object) {
  fns <- object@footnotes
  .opt_section(
    "FOOTNOTES",
    c(
      lapply(seq_along(fns), function(i) .fn_row(ns, i, fns[[i]])),
      list(.action_btn(
        ns("fn_add"),
        "+ Add footnote",
        variant = "link",
        class = "ar-fn-add"
      ))
    )
  )
}

#' The schema-generated option sections, grouped by `.OPT_REGION` in
#' `.OPT_SECTIONS` order.
#' @noRd
.opt_schema_sections <- function(con, ns, object, schema) {
  if (is.null(schema) || nrow(schema) == 0L) {
    return(NULL)
  }
  region <- unname(.OPT_REGION[schema$key])
  region[is.na(region)] <- "options"
  lapply(names(.OPT_SECTIONS), function(sec) {
    idx <- which(region == sec)
    if (length(idx) == 0L) {
      return(NULL)
    }
    .opt_section(
      .OPT_SECTIONS[[sec]],
      lapply(idx, function(i) {
        .opt_control(con, ns, object, schema[i, , drop = FALSE])
      })
    )
  })
}

# ---- UI ---------------------------------------------------------------

#' The Options pane UI: a server-rendered section stack -- the option rows
#' depend on the selected object's generator schema, so nothing here can
#' be static.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_card_options_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pane"))
}

# ---- server -------------------------------------------------------------

#' Footnote count for the pane's redraw trigger -- add/remove changes the
#' row set (needs a redraw); typing inside a line does not.
#' @noRd
.fn_count <- function(object) {
  if (is.null(object)) 0L else length(object@footnotes)
}

#' Commit one schema-row option value: parse by kind, surface an invalid
#' input as the inline message (NOT committed -- the last good value
#' stands), elide a value equal to the engine default, and skip a no-op
#' (the value a freshly-rendered control posts on bind must never push an
#' undo entry).
#' @noRd
.commit_opt <- function(store, rv_err, object, row, raw) {
  kind <- row$kind
  if (kind %in% c("int", "numvec", "text")) {
    parsed <- .opt_parse(kind, as.character(raw %||% ""))
    if (!parsed$ok) {
      reason <- if (identical(kind, "int")) {
        "is not a whole number"
      } else {
        "is not a comma-separated list of numbers"
      }
      rv_err(sprintf('%s: "%s" %s.', row$label, raw, reason))
      return(invisible(NULL))
    }
    value <- parsed$value
  } else if (identical(kind, "flag")) {
    value <- isTRUE(raw)
  } else {
    value <- as.character(raw)
  }
  if (!is.null(value) && identical(value, .opt_default(row))) {
    value <- NULL
  }
  if (identical(value, object@options[[row$key]])) {
    rv_err(NULL)
    return(invisible(NULL))
  }
  key <- row$key
  update_object(
    store,
    object@id,
    function(o) {
      opts <- o@options
      opts[[key]] <- value
      S7::set_props(o, options = opts)
    },
    label = paste0("set ", key)
  )
  rv_err(NULL)
  invisible(NULL)
}

#' The Options pane server: renders the section stack for the selected
#' object and wires every commit observer. The pane redraws on selection,
#' roles digest (the levels seed + option row set can change), and
#' footnote count -- deliberately NOT on every report commit, so typing in
#' a text input never redraws the input mid-edit. (Known ceiling: an
#' undo/redo while the pane is open can leave control VALUES stale until
#' the next redraw trigger -- the store stays authoritative, only the
#' control display lags.)
#' @param id *The module namespace, matching `mod_card_options_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_card_options_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv_err <- shiny::reactiveVal(NULL)

    output$pane <- shiny::renderUI({
      obj <- selected_object(store)
      if (is.null(obj)) {
        return(shiny::div(class = "ar-opt-empty"))
      }
      schema <- tryCatch(
        arpillar::option_schema(obj@type),
        error = function(e) NULL
      )
      shiny::tagList(
        .opt_title_section(ns, obj),
        .opt_footnotes_section(ns, obj),
        .opt_schema_sections(store$con, ns, obj, schema),
        shiny::uiOutput(ns("opt_msg"))
      )
    }) |>
      shiny::bindEvent(
        store$rv$selected,
        .roles_digest(selected_object(store)),
        .fn_count(selected_object(store))
      )

    output$opt_msg <- shiny::renderUI({
      msg <- rv_err()
      if (is.null(msg)) {
        return(NULL)
      }
      shiny::tags$p(
        class = "ar-opt-err ar-mono",
        .icon("warn", 11),
        shiny::tags$span(msg)
      )
    })

    # The inspector tab flip is a pure client-side class change the server
    # never sees (the mod_data lesson): a hidden pane's outputs must keep
    # computing or the Options tab opens blank.
    shiny::outputOptions(output, "pane", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "opt_msg", suspendWhenHidden = FALSE)

    # ---- title section commits ----
    shiny::observeEvent(input$title, {
      obj <- selected_object(store)
      if (is.null(obj) || identical(input$title, obj@title)) {
        return()
      }
      val <- input$title
      update_object(
        store,
        obj@id,
        function(o) S7::set_props(o, title = val),
        label = "retitle output"
      )
    })

    shiny::observeEvent(input$number, {
      obj <- selected_object(store)
      if (is.null(obj)) {
        return()
      }
      val <- trimws(input$number)
      value <- if (nzchar(val)) val else NULL
      if (identical(value, obj@options$number)) {
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) {
          opts <- o@options
          opts$number <- value
          S7::set_props(o, options = opts)
        },
        label = "renumber output"
      )
    })

    shiny::observeEvent(input$number_label, {
      obj <- selected_object(store)
      if (
        is.null(obj) || identical(input$number_label, obj@options$number_label)
      ) {
        return()
      }
      val <- input$number_label
      update_object(
        store,
        obj@id,
        function(o) {
          opts <- o@options
          opts$number_label <- val
          S7::set_props(o, options = opts)
        },
        label = "relabel output number"
      )
    })

    # A `title` region click (open_card) focuses the Title input -- the
    # inspector tab flip is store-driven, so by the time the message lands
    # the Options pane is the visible one.
    shiny::observe({
      if (
        identical(store$rv$region, "title") &&
          identical(store$rv$insp_tab, "options")
      ) {
        session$sendCustomMessage("ar-focus", list(id = ns("title")))
      }
    }) |>
      shiny::bindEvent(store$rv$region, store$rv$insp_tab)

    # ---- footnote commits ----
    shiny::observeEvent(input$fn_add, {
      obj <- selected_object(store)
      if (is.null(obj)) {
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) S7::set_props(o, footnotes = c(o@footnotes, "")),
        label = "add footnote"
      )
    })

    shiny::observeEvent(input$fn_edit, {
      obj <- selected_object(store)
      i <- as.integer(input$fn_edit$i)
      val <- as.character(input$fn_edit$value)
      if (
        is.null(obj) ||
          is.na(i) ||
          i < 1L ||
          i > length(obj@footnotes) ||
          identical(obj@footnotes[[i]], val)
      ) {
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) {
          fns <- o@footnotes
          fns[[i]] <- val
          S7::set_props(o, footnotes = fns)
        },
        label = "edit footnote"
      )
    })

    shiny::observeEvent(input$fn_remove, {
      obj <- selected_object(store)
      i <- as.integer(input$fn_remove$i)
      if (is.null(obj) || is.na(i) || i < 1L || i > length(obj@footnotes)) {
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) S7::set_props(o, footnotes = o@footnotes[-i]),
        label = "remove footnote"
      )
    })

    # ---- schema-row commits ----
    # One observer per known option key across every generator -- the same
    # bounded static-registration pattern as the Roles module's slot
    # observers. Each observer looks the row up in the SELECTED object's
    # own schema (defaults differ per generator) and no-ops when the key
    # is not part of it.
    known <- unique(unlist(lapply(names(arpillar::generators()), function(t) {
      arpillar::option_schema(t)$key
    })))

    for (opt_key in known) {
      local({
        k <- opt_key
        input_id <- paste0("opt_", k)
        reorder_id <- paste0("opt_reorder_", k)

        # No `ignoreInit`: when the first reactive flush after mount is
        # itself the first input event, ignoreInit would swallow that real
        # event. The bind-time post of a freshly-rendered control is
        # handled by `.commit_opt()`'s no-op guard instead (seeded value
        # == current value -> no commit).
        shiny::observeEvent(input[[input_id]], {
          obj <- selected_object(store)
          if (is.null(obj)) {
            return()
          }
          schema <- tryCatch(
            arpillar::option_schema(obj@type),
            error = function(e) NULL
          )
          if (is.null(schema)) {
            return()
          }
          row <- schema[schema$key == k, , drop = FALSE]
          if (nrow(row) != 1L) {
            return()
          }
          .commit_opt(store, rv_err, obj, row, input[[input_id]])
        })

        # A levels drop posts its own reorder input (the sortable JS
        # contract); an explicit order always commits verbatim.
        shiny::observeEvent(input[[reorder_id]], {
          obj <- selected_object(store)
          if (is.null(obj)) {
            return()
          }
          order <- vapply(
            input[[reorder_id]]$order,
            as.character,
            character(1)
          )
          update_object(
            store,
            obj@id,
            function(o) {
              opts <- o@options
              opts[[k]] <- order
              S7::set_props(o, options = opts)
            },
            label = paste0("reorder ", k)
          )
        })
      })
    }

    invisible(NULL)
  })
}
