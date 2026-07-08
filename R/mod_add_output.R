# The Add-output overlay (design spec #4, "Add output"): a centred dialog
# shown when `store$rv$adding` is TRUE. Two sections -- the searchable
# preset library grouped by domain, and a bare-generator "start from
# scratch" path -- both end at either `add_from_preset()` or
# `add_from_generator()` (fct_store.R), which already append + select +
# close nothing: THIS module clears `rv$adding` itself once the mutation
# lands.

# ---- preset library -------------------------------------------------------

#' Every registered preset, tagged with its generator's `kind` (for the row
#' glyph) and `label` (for the trailing "<generator label>" caption),
#' grouped by `domain` in registry order within each group. Domain order is
#' fixed (`Safety`, `Efficacy`, `PK`, `General`) rather than
#' first-seen-in-registry, so the library's section order never depends on
#' `presets()`'s own iteration order.
#' @noRd
.PRESET_DOMAINS <- c("Safety", "Efficacy", "PK", "General")

#' One preset library row: id/label/domain/kind/generator id/generator
#' label/number. `generator` is the row glyph key (`.type_icon()`) -- every
#' preset sharing a generator (e.g. every AE occurrence preset) reads the
#' same icon, distinct from `kind` ("table"/"figure") which only splits
#' presets into the two coarse TOC groups.
#' @noRd
.library_rows <- function() {
  gens <- arpillar::generators()
  ps <- arpillar::presets()
  lapply(names(ps), function(id) {
    p <- ps[[id]]
    gen <- gens[[p$generator]]
    list(
      id = id,
      label = p$label,
      domain = p$domain,
      kind = gen$kind,
      generator = p$generator,
      generator_label = gen$label,
      number = p$options[["number"]] %||% ""
    )
  })
}

#' Split library rows into ordered (domain, rows) groups, dropping empty
#' domains and applying a case-insensitive substring `search` filter on
#' `label` first.
#' @noRd
.library_groups <- function(rows, search = "") {
  if (nzchar(trimws(search))) {
    needle <- tolower(trimws(search))
    rows <- Filter(
      function(r) grepl(needle, tolower(r$label), fixed = TRUE),
      rows
    )
  }
  groups <- lapply(.PRESET_DOMAINS, function(d) {
    list(
      domain = d,
      rows = Filter(function(r) identical(r$domain, d), rows)
    )
  })
  Filter(function(g) length(g$rows) > 0L, groups)
}

# ---- dataset matching -------------------------------------------------------

#' The unique set of CDISC variable names a preset's `roles` reference
#' across every slot.
#' @noRd
.preset_vars <- function(preset) {
  unique(unlist(preset$roles, use.names = FALSE))
}

#' `preset`'s role vars absent from `ds_cols`, with the CDISC arm-var
#' swap: a preset-listed arm var (presets hard-code "TRT01P") counts as
#' satisfied whenever the dataset carries any of `.ARM_VAR_ALTS`, because
#' `.roles_from_preset()` swaps to whichever is present. Without this,
#' every BDS dataset (ADAE / ADCM / ADLB / ADVS) falsely reports "missing
#' TRT01P" even though TRTA covers the treatment slot.
#' @noRd
.effective_missing <- function(vars, ds_cols) {
  vars <- if (any(.ARM_VAR_ALTS %in% ds_cols)) {
    setdiff(vars, .ARM_VAR_ALTS)
  } else {
    vars
  }
  setdiff(vars, ds_cols)
}

#' The catalog dataset whose columns cover the MOST of `preset`'s role
#' vars, or `NULL` when no catalog dataset covers even one. Ties break by
#' `catalog_grid()`'s own (insertion) order -- the first-registered
#' best-covering dataset wins, so the default is stable across a
#' re-render.
#' @noRd
.best_dataset <- function(con, preset) {
  vars <- .preset_vars(preset)
  grid <- arpillar::catalog_grid(con)
  if (nrow(grid) == 0L || length(vars) == 0L) {
    return(NULL)
  }
  coverage <- vapply(
    grid$name,
    function(ds) {
      length(vars) -
        length(.effective_missing(vars, arpillar::data_items(con, ds)$name))
    },
    integer(1)
  )
  if (max(coverage) == 0L) {
    return(NULL)
  }
  grid$name[[which.max(coverage)]]
}

#' The subset of `preset`'s role vars absent from `dataset`'s columns, or
#' `character(0)` when every var is present (or `dataset` is `""`/`NULL`,
#' e.g. before a picker selection is made).
#' @noRd
.missing_vars <- function(con, preset, dataset) {
  if (is.null(dataset) || !nzchar(dataset)) {
    return(character(0))
  }
  .effective_missing(
    .preset_vars(preset),
    arpillar::data_items(con, dataset)$name
  )
}

# ---- UI ---------------------------------------------------------------

#' The Add-output overlay UI: an in-flow backdrop + a `--ar-card` dialog,
#' server-rendered end to end (`uiOutput`) so it can react to `rv$adding`
#' without a second visibility mechanism -- the caller composes this
#' directly into the workspace (see `arframe()`), and it renders nothing
#' (`NULL`) while `rv$adding` is `FALSE`.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_add_output_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("overlay"), class = "ar-add-overlay-slot")
}

#' One preset library row: kind glyph + label + generator caption, opens
#' the dataset-picker panel for `row$id` on click (a plain
#' `actionLink`-style row, posting `input$pick_preset`).
#' @noRd
.library_row_ui <- function(ns, row, active) {
  click_js <- sprintf(
    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
    ns("pick_preset"),
    row$id
  )
  shiny::tags$div(
    class = paste(
      "ar-add-lib-row",
      if (active) "ar-add-lib-row-active" else NULL
    ),
    onclick = click_js,
    .type_icon(row$generator, 13),
    shiny::tags$span(class = "ar-add-lib-label", row$label),
    shiny::tags$span(class = "ar-add-lib-caption", row$generator_label)
  )
}

#' One generator row (the "start from a generator" section): kind glyph +
#' label + description, opens the same dataset-picker panel shape as a
#' preset row, posting `input$pick_generator`.
#' @noRd
.generator_row_ui <- function(ns, gen, active = FALSE) {
  click_js <- sprintf(
    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
    ns("pick_generator"),
    gen$id
  )
  shiny::tags$div(
    class = paste(
      "ar-add-gen-row",
      if (active) "ar-add-lib-row-active" else NULL
    ),
    onclick = click_js,
    .type_icon(gen$id, 13),
    shiny::tags$div(
      shiny::tags$span(class = "ar-add-lib-label", gen$label),
      shiny::tags$div(class = "ar-add-gen-desc", gen$description)
    )
  )
}

#' The dataset-picker panel shown once a preset or generator is picked: an
#' empty-by-default selectize (`onInitialize` forces no auto-selected first
#' option, per the app's selection-input rule) UNLESS `preselect` names a
#' dataset (the preset path's best-match default, or a live
#' `rv$bridge_dataset`), a server-rendered warning slot that reacts to the
#' LIVE picker value (see `output$picker_warning`, not built statically
#' here -- a warning computed once off `preselect` would go stale the
#' moment the user overrides the dropdown away from the auto-suggested
#' dataset), and the primary Add action.
#' @noRd
.dataset_picker_ui <- function(
  ns,
  choices,
  preselect,
  add_input_id,
  add_label = "Add to report"
) {
  shiny::tagList(
    shiny::selectizeInput(
      ns("picker_dataset"),
      label = "Dataset",
      choices = choices,
      selected = preselect %||% "",
      options = if (is.null(preselect) || !nzchar(preselect)) {
        list(
          placeholder = "Select a dataset",
          onInitialize = I("function() { this.setValue(''); }")
        )
      } else {
        list(placeholder = "Select a dataset")
      }
    ),
    shiny::uiOutput(ns("picker_warning")),
    .action_btn(
      ns(add_input_id),
      shiny::tagList(.icon("plus", 12), add_label),
      variant = "primary"
    )
  )
}

#' The whole overlay: backdrop + header (label + close) + the three
#' sections. `picked` is `NULL` (no preset/generator chosen yet, dataset
#' picker hidden) or `list(kind = "preset"|"generator", id = <chr>)`.
#' `live_dataset` is the caller's tracked "user override for the CURRENT
#' pick" value (`NULL` before the user has touched the dropdown, or the
#' instant a NEW preset/generator is picked -- see
#' `mod_add_output_server()`'s `override_dataset` reactiveVal). Threading
#' it through here lets a re-render triggered by something else (a search
#' keystroke) preserve an in-progress manual dataset choice instead of
#' recomputing the auto-suggested default and silently reverting it.
#'
#' `search` (used to FILTER the library rows) and `search_seed` (used to
#' SEED the search `textInput`'s `value=`) are deliberately two different
#' reads of the same underlying `input$search`, not one -- the caller
#' passes an UN-ISOLATED read for `search` (so a keystroke re-filters the
#' list) and an `isolate()`d read for `search_seed` (so re-rendering the
#' textInput widget for any OTHER reason does not seed it from the very
#' value the client just sent, which would re-mount the widget, echo that
#' value straight back to the server, and self-trigger a SECOND renderUI
#' cycle for `output$overlay` -- verified: this was silently swallowing
#' the open-focus message, because the second cycle replaced the dialog
#' DOM node the first cycle's focus command was about to land on).
#' @noRd
.overlay_ui <- function(
  store,
  ns,
  search,
  picked,
  live_dataset = NULL,
  search_seed = search
) {
  groups <- .library_groups(.library_rows(), search)
  gens <- arpillar::generators()

  picker <- NULL
  if (!is.null(picked)) {
    if (identical(picked$kind, "preset")) {
      pr <- arpillar::preset(picked$id)
      preselect <- live_dataset %||%
        store$rv$bridge_dataset %||%
        .best_dataset(store$con, pr)
      picker <- .dataset_picker_ui(
        ns,
        choices = arpillar::catalog_grid(store$con)$name,
        preselect = preselect,
        add_input_id = "add_preset"
      )
    } else {
      preselect <- live_dataset %||% store$rv$bridge_dataset
      picker <- .dataset_picker_ui(
        ns,
        choices = arpillar::catalog_grid(store$con)$name,
        preselect = preselect,
        add_input_id = "add_generator"
      )
    }
  }

  shiny::tags$div(
    class = "ar-add-backdrop",
    onclick = sprintf(
      "if (event.target === this) Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
      ns("dismiss")
    ),
    shiny::tags$div(
      class = "ar-add-card",
      id = ns("dialog"),
      tabindex = "-1",
      shiny::tags$div(
        class = "ar-add-header",
        .label("ADD OUTPUT"),
        .action_btn(
          ns("close"),
          .icon("close", 14),
          variant = "link",
          class = "ar-icon-btn"
        )
      ),
      shiny::tags$div(
        class = "ar-add-body",
        .label("Preset library"),
        shiny::div(
          class = "ar-search-host",
          shiny::textInput(
            ns("search"),
            label = NULL,
            # `search_seed`, NOT `search` -- see `.overlay_ui()`'s own
            # doc comment for why these two must stay different reads.
            value = search_seed,
            placeholder = "Search presets"
          )
        ),
        shiny::tags$div(
          class = "ar-add-lib-list",
          lapply(groups, function(g) {
            shiny::tagList(
              shiny::tags$div(class = "ar-label ar-add-lib-domain", g$domain),
              lapply(g$rows, function(row) {
                active <- identical(picked$kind, "preset") &&
                  identical(picked$id, row$id)
                # The picker renders INLINE, right after the row the user
                # just clicked -- not appended after the whole (up to 20-
                # row) library list, which would strand it far below the
                # trigger the user's eye is still on.
                shiny::tagList(
                  .library_row_ui(ns, row, active),
                  if (active) picker
                )
              })
            )
          })
        ),
        shiny::tags$details(
          class = "ar-add-gen-section",
          # Force-open whenever a generator is the active pick -- a bare
          # <details> defaults to closed, and a re-render triggered by
          # something else (a search keystroke) rebuilds this element from
          # scratch, which would otherwise re-collapse it and hide the open
          # dataset picker underneath.
          open = if (identical(picked$kind, "generator")) NA,
          shiny::tags$summary(.label("Start from a generator")),
          lapply(gens, function(gen) {
            active <- identical(picked$kind, "generator") &&
              identical(picked$id, gen$id)
            shiny::tagList(
              .generator_row_ui(ns, gen, active),
              if (active) picker
            )
          })
        )
      )
    )
  )
}

# ---- server -------------------------------------------------------------

#' The Add-output overlay server: renders the overlay (or nothing) off
#' `store$rv$adding`, tracks which preset/generator is picked as local
#' module state (`picked`, `search` -- ephemeral UI state, not document
#' state, so it lives in a moduleServer reactiveVal rather than the
#' store), and wires every Add/close/dismiss path.
#' @param id *The module namespace, matching `mod_add_output_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_add_output_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    picked <- shiny::reactiveVal(NULL)
    # The picker's live value, tracked SEPARATELY from `input$picker_dataset`
    # itself: an override the user has actively made for the CURRENT pick,
    # reset to NULL the instant a NEW preset/generator is picked (see the
    # `pick_preset`/`pick_generator` observers below) so switching presets
    # never carries a stale dataset choice from the PREVIOUS one's picker
    # into the freshly recomputed best-match default.
    override_dataset <- shiny::reactiveVal(NULL)

    # Reset the ephemeral pick/search state and hand focus back to the
    # `+ Add output` trigger (mod_contents.R's namespace + input id are
    # fixed at the app-composition level, like `.frame_bar()`'s reach into
    # `contents-add_output` via `arframe.js`'s "ar-focus" handler) -- every
    # close path (X, backdrop, Esc, or a successful Add) goes through this
    # one function so none of the four can leave stale pick/search state
    # for the next open.
    .close_overlay <- function() {
      store$rv$adding <- FALSE
      store$rv$bridge_dataset <- NULL
      picked(NULL)
      override_dataset(NULL)
      session$sendCustomMessage("ar-focus", list(id = "contents-add_output"))
    }

    # Track the user's live override WITHOUT making `output$overlay` depend
    # on `input$picker_dataset` directly (that would re-render, and so
    # recreate/churn, the selectize widget on every dataset pick -- exactly
    # what `output$picker_warning` exists to avoid needing).
    shiny::observeEvent(input$picker_dataset, {
      override_dataset(input$picker_dataset)
    })

    output$overlay <- shiny::renderUI({
      if (!isTRUE(store$rv$adding)) {
        return(NULL)
      }
      .overlay_ui(
        store,
        ns,
        input$search %||% "",
        picked(),
        override_dataset(),
        search_seed = shiny::isolate(input$search) %||% ""
      )
    })

    # The missing-vars warning, split out of `output$overlay` into its own
    # `uiOutput` so it can react to the LIVE `input$picker_dataset` value --
    # `output$overlay` deliberately does NOT depend on `picker_dataset`
    # (re-rendering the whole dialog, and so the selectize widget, on every
    # dataset pick would churn/reset that control); this output alone
    # re-derives the warning whenever the user changes the dropdown away
    # from the auto-suggested default, and clears it for the generator path
    # (a bare generator has no prefilled `roles`, so there is nothing to
    # check vars against).
    output$picker_warning <- shiny::renderUI({
      p <- picked()
      dataset <- input$picker_dataset
      if (is.null(p) || !identical(p$kind, "preset")) {
        return(NULL)
      }
      pr <- arpillar::preset(p$id)
      missing <- .missing_vars(store$con, pr, dataset)
      if (length(missing) == 0L) {
        return(NULL)
      }
      shiny::tags$div(
        class = "ar-add-warn",
        .icon("warn", 12),
        shiny::tags$span(
          sprintf(
            "%s is missing %s; the output will render once those columns are available or you re-assign roles.",
            dataset,
            paste(missing, collapse = ", ")
          )
        )
      )
    })

    # Opening: focus is moved into the dialog CLIENT-SIDE (arframe.js's
    # MutationObserver on `.ar-add-card` appearing under `.ar-add-overlay-
    # slot`), NOT from a server observer here. `output$overlay`'s first
    # mount of the search `textInput` triggers Shiny's normal "a freshly
    # bound input posts its own current value back once" behavior, which
    # (since this render depends on `input$search` for live filtering)
    # causes ONE extra, unavoidable renderUI cycle right after open --
    # verified empirically. A server-sent "ar-focus" message from a sibling
    # observer has no ordering guarantee against that extra cycle
    # replacing the very dialog DOM node the message is targeting, so the
    # client watches for the element's OWN appearance instead of being
    # told when to look for it.

    shiny::observeEvent(input$close, {
      .close_overlay()
    })
    shiny::observeEvent(input$dismiss, {
      .close_overlay()
    })

    shiny::observeEvent(input$pick_preset, {
      picked(list(kind = "preset", id = input$pick_preset))
      override_dataset(NULL)
    })

    shiny::observeEvent(input$pick_generator, {
      picked(list(kind = "generator", id = input$pick_generator))
      override_dataset(NULL)
    })

    shiny::observeEvent(input$add_preset, {
      p <- picked()
      dataset <- input$picker_dataset
      if (is.null(p) || is.null(dataset) || !nzchar(dataset)) {
        return()
      }
      # Auto-drill: a fresh output opens straight into its paper + inspector
      # so the user lands where they configure it, not back on the list.
      drill_open(store, add_from_preset(store, p$id, dataset))
      .close_overlay()
    })

    shiny::observeEvent(input$add_generator, {
      p <- picked()
      dataset <- input$picker_dataset
      if (is.null(p) || is.null(dataset) || !nzchar(dataset)) {
        return()
      }
      drill_open(store, add_from_generator(store, p$id, dataset))
      .close_overlay()
    })

    invisible(NULL)
  })
}
