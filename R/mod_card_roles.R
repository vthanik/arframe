# The Roles editor (design spec #8's primary tab): content for the
# `columns`/`rows`/`axes` card regions. Which slots show depends on the
# region AND the selected object's generator (`arpillar::generator(type)
# $slots`, the single contract the render legs/oracle already key off --
# see fct_generators.R/fct_status.R in arpillar). Every commit goes through
# `update_object()` (fct_store.R), which re-derives `output_status()` and
# invalidates `rv$report` -- so filling a slot here is the whole "ghost
# fills into a real table" loop; this module never itself renders a paper.

# ---- region -> slot filter -----------------------------------------------

#' Which of a generator's slots the CURRENT region shows.
#'
#' `columns` is the treatment/arm slot; `rows` is the table-content slot
#' (`summarize` OR `hierarchy` -- a generator has at most one of the two);
#' `axes` is every remaining (figure) slot. This mirrors
#' `utils_ghost.R`'s `.GHOST_REGION_MAP` in reverse: that map sends a
#' `validate_output()` control_id TO a region; this filter sends a region
#' back to the generator's OWN slot list, so a ghost click and a real card
#' open land on the identical slot set.
#' @noRd
.region_slots <- function(region, slots) {
  # v5: the docked Roles TAB is shown with no region focus (`region` NULL) --
  # `switch()` aborts on a length-0 EXPR, and the right content there is the
  # FULL role-slot editor, not a region-filtered subset. Only an explicit
  # role-region (columns/rows/axes) narrows the set.
  if (is.null(region) || length(region) != 1L) {
    return(slots)
  }
  ids <- vapply(slots, `[[`, "", "slot")
  keep <- switch(
    region,
    columns = ids %in% "treatment",
    rows = ids %in% c("summarize", "hierarchy"),
    axes = !ids %in% c("treatment", "summarize", "hierarchy"),
    ids %in% character(0)
  )
  slots[keep]
}

# ---- slot alias resolution (mirrors the render legs / status oracle) ------

#' The role-slot ALIAS set a canonical slot name accepts when searching
#' `object@roles` -- deliberately pinned to the exact sets the render legs
#' read (`fct_render_ard.R` `.arm_var`/`.summarize_items`,
#' `fct_render_ggplot.R` `.figure_roles`, `fct_render_km.R` `.km_roles`)
#' and `fct_status.R`'s `.SLOT_REQS`, so a variable this module drops onto
#' `"treatment"` is found by the SAME alias walk the render leg uses, never
#' a second, drifting definition.
#' @noRd
.SLOT_ALIASES <- list(
  treatment = c("treatment", "group"),
  summarize = c("summarize", "row"),
  hierarchy = "hierarchy",
  x = "x",
  y = "y",
  group = c("group", "treatment", "strata"),
  time = "time",
  censor = "censor"
)

#' The alias set for `slot`, falling back to `slot` itself when this table
#' has no entry (a future generator slot with no known alias still
#' resolves to searching its own bare name).
#' @noRd
.slot_aliases <- function(slot) {
  .SLOT_ALIASES[[slot]] %||% slot
}

#' The first role on `object` whose `@slot` is in `slot`'s alias set, or
#' `NULL`. Mirrors `arpillar:::.find_role`/`.slot_items` exactly (first
#' match in role order).
#' @noRd
.role_for_slot <- function(object, slot) {
  aliases <- .slot_aliases(slot)
  for (r in object@roles) {
    if (r@slot %in% aliases) {
      return(r)
    }
  }
  NULL
}

# ---- eligible variables ---------------------------------------------------

#' The dataset's `data_items()` filtered to what `slot$accepts` allows,
#' minus names already assigned to `slot` on `object` -- the "only eligible
#' variables" contract (design spec #8: "Treatment arms won't offer a
#' numeric"). `data_items()` is called fresh (not memoized here) -- it is
#' metadata-only (a single `DESCRIBE`), cheap at demo/dev scale, and a
#' cross-request memo would risk showing a stale column list right after
#' an Add-output/import bumped `catalog_nonce`.
#' @noRd
.eligible_items <- function(con, dataset, slot, assigned_names) {
  items <- arpillar::data_items(con, dataset)
  items <- items[items$type %in% slot$accepts, , drop = FALSE]
  items[!items$name %in% assigned_names, , drop = FALSE]
}

# ---- picker ---------------------------------------------------------------

#' Pack one eligible-variable option as `"NAME\x1fTYPE"` -- `\x1f` (unit
#' separator) cannot appear in a column name or a DuckDB type string, so
#' splitting on it always recovers both fields cleanly; packing both into
#' the selectize VALUE (not just the label) is what lets `render` search
#' match on either the name or the type.
#' @noRd
.pack_item_choice <- function(name, sql_type) {
  paste0(name, "\x1f", sql_type)
}

#' The variable name half of a packed `"NAME\x1fTYPE"` choice value.
#' @noRd
.unpack_item_name <- function(choice) {
  strsplit(choice, "\x1f", fixed = TRUE)[[1]][[1]]
}

#' A two-line rich-picker `selectizeInput`: each option shows the type
#' chip + name on line 1, the raw `sql_type` (muted, mono) on line 2 --
#' built via `options$render` (a JS template string), the same technique
#' `selectize.js` itself recommends for custom option markup. Choices are
#' packed `"NAME\x1fTYPE"` (`.pack_item_choice()`) so free-text search
#' matches the type string too (e.g. typing "double" surfaces every
#' measure). Empty by default (no auto-selected first option, per the
#' app's selection-input rule already followed in `mod_add_output.R`'s
#' dataset picker); the Filters pane passes `selected` to re-seed a
#' committed row's column.
#' @noRd
.eligible_picker <- function(
  ns,
  input_id,
  items,
  selected = character(0),
  placeholder = "Add a variable"
) {
  choices <- vapply(
    seq_len(nrow(items)),
    function(i) .pack_item_choice(items$name[[i]], items$sql_type[[i]]),
    character(1)
  )
  # One JS template per row: `item.value.split(...)` recovers name/type
  # client-side from the packed choice string -- selectize's `render`
  # callback only ever sees the (value, label) pair it was given, so the
  # chip/type-line markup is regenerated in JS, not pre-rendered per row
  # server-side (which would need one `option` template per possible
  # item, an unbounded set).
  render_js <- I(paste0(
    "{ option: function(item, escape) {",
    "  var parts = item.value.split('\\u001f');",
    "  var nm = escape(parts[0]); var ty = escape(parts[1] || '');",
    "  var cls = { measure: 'ar-chip ar-chip-meas', ",
    "              date: 'ar-chip ar-chip-date', ",
    "              category: 'ar-chip ar-chip-cat' };",
    "  var glyph = { measure: '#', date: '\\uD83D\\uDCC5', category: 'A' };",
    "  var t = (parts[1] === 'measure' || parts[1] === 'date') ? parts[1] : 'category';",
    "  return '<div class=\"ar-picker-option\">' +",
    "    '<div class=\"ar-picker-option-line1\">' +",
    "    '<span class=\"' + cls[t] + '\">' + glyph[t] + '</span>' +",
    "    '<span class=\"ar-picker-option-name\">' + nm + '</span>' +",
    "    '</div>' +",
    "    '<div class=\"ar-picker-option-type ar-mono\">' + ty + '</div>' +",
    "    '</div>';",
    "}, item: function(item, escape) {",
    # The SELECTED display (closed control) shows the bare name -- the
    # two-line option template overflows a closed, card-width control.
    "  var nm = escape(item.value.split('\\u001f')[0]);",
    "  return '<div class=\"ar-picker-item\">' + nm + '</div>';",
    "} }"
  ))
  opts <- list(
    placeholder = placeholder,
    render = render_js,
    searchField = list("value")
  )
  if (length(selected) == 0L) {
    # Force-empty on bind -- selectize otherwise auto-picks the first
    # option; only wanted when nothing is meant to be selected.
    opts$onInitialize <- I("function() { this.setValue(''); }")
  }
  shiny::selectizeInput(
    ns(input_id),
    label = NULL,
    choices = stats::setNames(choices, choices),
    selected = selected,
    options = opts
  )
}

# ---- assigned-item rows ---------------------------------------------------

#' One assigned-item row: chip + name + a visible, focusable remove button.
#' `grip` (the drag handle) is shown only when the slot is multi-item
#' (`slot$max > 1`) -- a single-item slot has nothing to reorder.
#'
#' The remove button is a per-item DYNAMIC control (any dataset column
#' name), so it posts through ONE shared `ns("remove")` input via an
#' inline onclick -- `Shiny.setInputValue({slot, name})` -- exactly
#' `mod_contents.R`'s `.toc_kebab()` pattern for its own dynamically-named
#' row actions (`dup_js`/`remove_js`), rather than a per-item
#' `observeEvent` that would need re-registering on every render.
#' @noRd
.assigned_row <- function(ns, slot_id, item, multi) {
  remove_js <- sprintf(
    "Shiny.setInputValue('%s', {slot: '%s', name: '%s', nonce: Date.now()}, {priority: 'event'})",
    ns("remove"),
    slot_id,
    item@name
  )
  shiny::tags$div(
    class = "ar-role-row",
    `data-ar-item` = item@name,
    if (multi) shiny::tags$span(class = "ar-role-grip", .icon("grip", 11)),
    .type_chip(item@role_type),
    shiny::tags$span(class = "ar-role-name", item@label %||% item@name),
    shiny::tags$button(
      type = "button",
      class = "ar-icon-btn ar-role-remove",
      `aria-label` = paste0("Remove ", item@name, " from ", slot_id),
      onclick = remove_js,
      .icon("close", 11)
    )
  )
}

# ---- one slot's fieldset --------------------------------------------------

#' One slot's whole fieldset: legend (the slot label -- the micro-label IS
#' the legend, per the brief), assigned rows (sortable when `max > 1`),
#' the dashed "+ Add variable" picker row, and an inline `validate_output`
#' message when this slot's control_id is among the object's unmet
#' requirements (message text IDENTICAL to the oracle's own -- never
#' reworded, so the ghost hint, the error summary, and this inline message
#' always agree).
#' @noRd
.slot_fieldset <- function(con, ns, object, slot, problems) {
  role <- .role_for_slot(object, slot$slot)
  items <- if (is.null(role)) list() else role@items
  assigned_names <- vapply(items, function(it) it@name, character(1))
  multi <- slot$max > 1L

  sortable_attrs <- if (multi) {
    list(
      `data-ar-sortable` = "true",
      `data-ar-sortable-handle` = ".ar-role-grip",
      `data-ar-sortable-item` = ".ar-role-row",
      `data-ar-sortable-attr` = "data-ar-item",
      `data-ar-sortable-input` = ns(paste0("reorder_", slot$slot)),
      `data-ar-sortable-extra` = sprintf('{"slot":"%s"}', slot$slot)
    )
  } else {
    list()
  }

  problem <- problems[[slot$slot]]

  shiny::tags$fieldset(
    class = "ar-role-slot",
    do.call(shiny::tags$legend, list(class = "ar-label", slot$label)),
    do.call(
      shiny::tags$div,
      c(
        list(class = "ar-role-assigned"),
        sortable_attrs,
        lapply(items, function(it) .assigned_row(ns, slot$slot, it, multi))
      )
    ),
    .eligible_picker(
      ns,
      paste0("add_", slot$slot),
      .eligible_items(con, object@dataset, slot, assigned_names)
    ),
    if (!is.null(problem)) {
      shiny::tags$p(
        class = "ar-role-problem ar-mono",
        .icon("warn", 11),
        shiny::span(problem)
      )
    }
  )
}

# ---- UI ---------------------------------------------------------------

#' The Roles editor UI: a server-rendered slot list (`uiOutput`) -- the
#' set of slots shown depends on both the region and the selected
#' object's generator, so it cannot be built statically here.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_card_roles_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("slots"))
}

# ---- server -------------------------------------------------------------

#' A stable digest of `object@roles` -- the assigned slot/item set, order-
#' sensitive (so a reorder also invalidates the render). Deliberately
#' EXCLUDES everything else on the object (`filters`, `options`, `title`)
#' so an edit made in a DIFFERENT card region never forces this module to
#' re-render its own slot list.
#' @noRd
.roles_digest <- function(object) {
  if (is.null(object)) {
    return(NULL)
  }
  rlang::hash(lapply(object@roles, function(r) {
    list(
      slot = r@slot,
      items = vapply(r@items, function(it) it@name, character(1))
    )
  }))
}

#' Append `item` to the alias-matched existing role on `object`, or create
#' a fresh `role(slot = slot)` holding it when no role matches any alias
#' yet. Mirrors `.role_for_slot()`'s own alias walk so "found an existing
#' role" and "committed to that same role" can never disagree.
#' @noRd
.add_item_to_slot <- function(object, slot, item) {
  aliases <- .slot_aliases(slot)
  roles <- object@roles
  for (i in seq_along(roles)) {
    if (roles[[i]]@slot %in% aliases) {
      roles[[i]] <- S7::set_props(
        roles[[i]],
        items = c(roles[[i]]@items, list(item))
      )
      return(S7::set_props(object, roles = roles))
    }
  }
  new_role <- arpillar::role(slot = slot, items = list(item))
  S7::set_props(object, roles = c(roles, list(new_role)))
}

#' Drop the item named `item_name` from the alias-matched role on `object`.
#' A no-op (returns `object` unchanged) when no such role/item exists.
#' @noRd
.remove_item_from_slot <- function(object, slot, item_name) {
  aliases <- .slot_aliases(slot)
  roles <- object@roles
  for (i in seq_along(roles)) {
    if (roles[[i]]@slot %in% aliases) {
      roles[[i]] <- S7::set_props(
        roles[[i]],
        items = Filter(
          function(it) !identical(it@name, item_name),
          roles[[i]]@items
        )
      )
      return(S7::set_props(object, roles = roles))
    }
  }
  object
}

#' Reorder the alias-matched role's items on `object` to `order` (a
#' character vector of item names in the new order). Any name in `order`
#' absent from the role's current items is dropped; any current item
#' absent from `order` is appended at the end -- the same reconcile
#' discipline `mod_contents.R`'s own `input$reorder` observer uses, so a
#' stale/partial drop payload never loses an item.
#' @noRd
.reorder_slot <- function(object, slot, order) {
  aliases <- .slot_aliases(slot)
  roles <- object@roles
  for (i in seq_along(roles)) {
    if (roles[[i]]@slot %in% aliases) {
      current <- roles[[i]]@items
      names_now <- vapply(current, function(it) it@name, character(1))
      full_order <- c(intersect(order, names_now), setdiff(names_now, order))
      reordered <- lapply(full_order, function(nm) {
        current[[match(nm, names_now)]]
      })
      roles[[i]] <- S7::set_props(roles[[i]], items = reordered)
      return(S7::set_props(object, roles = roles))
    }
  }
  object
}

#' Named list of `slot -> message` for every `validate_output()` row whose
#' `control_id` maps to one of `slots`' own slot names (`"roles-<slot>"`),
#' keyed so `.slot_fieldset()` can look up its own inline message by a
#' plain `problems[[slot$slot]]`.
#' @noRd
.slot_problems <- function(object, slots) {
  v <- arpillar::validate_output(object)
  if (nrow(v) == 0L) {
    return(list())
  }
  ids <- vapply(slots, `[[`, "", "slot")
  out <- list()
  for (i in seq_len(nrow(v))) {
    cid <- v$control_id[[i]]
    slot_id <- sub("^roles-", "", cid)
    if (slot_id %in% ids) {
      out[[slot_id]] <- v$message[[i]]
    }
  }
  out
}

#' The Roles editor server: renders the region-filtered slot list for the
#' selected object's generator, and wires every add/remove/reorder input.
#' Region content re-renders on `(rv$selected, roles digest)` ONLY -- never
#' on a drag (which posts its own `reorder_<slot>` input, handled by a
#' dedicated observer that commits directly without touching this
#' `renderUI`'s own trigger set until the COMMIT lands, at which point the
#' digest itself changes and the list legitimately redraws with the new
#' order).
#' @param id *The module namespace, matching `mod_card_roles_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_card_roles_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$slots <- shiny::renderUI({
      obj <- selected_object(store)
      if (is.null(obj)) {
        return(NULL)
      }
      gen <- tryCatch(arpillar::generator(obj@type), error = function(e) NULL)
      if (is.null(gen)) {
        return(NULL)
      }
      slots <- .region_slots(store$rv$region, gen$slots)
      if (length(slots) == 0L) {
        return(NULL)
      }
      problems <- .slot_problems(obj, slots)
      shiny::tagList(lapply(slots, function(s) {
        .slot_fieldset(store$con, ns, obj, s, problems)
      }))
    }) |>
      shiny::bindEvent(
        store$rv$selected,
        store$rv$region,
        .roles_digest(selected_object(store))
      )

    # One shared observer per generator-defined slot id would need a
    # dynamically-registered set (slots differ per generator/region); a
    # bounded, STATIC set of possible slot ids across every generator is
    # small and fixed (the union of every `.SLOT()` in
    # `arpillar::generators()`), so one `observeEvent` per known slot id
    # is registered ONCE at module-mount time -- each observer is a no-op
    # unless its own `add_<slot>`/`reorder_<slot>` input actually fires.
    known_slot_ids <- unique(unlist(
      lapply(arpillar::generators(), function(g) {
        vapply(g$slots, `[[`, "", "slot")
      })
    ))

    for (slot_id in known_slot_ids) {
      local({
        sid <- slot_id
        add_input <- paste0("add_", sid)
        reorder_input <- paste0("reorder_", sid)

        shiny::observeEvent(
          input[[add_input]],
          {
            choice <- input[[add_input]]
            if (is.null(choice) || !nzchar(choice)) {
              return()
            }
            name <- .unpack_item_name(choice)
            obj_id <- store$rv$selected
            if (is.null(obj_id)) {
              return()
            }
            obj <- selected_object(store)
            item_row <- arpillar::data_items(store$con, obj@dataset)
            hit <- item_row$type[item_row$name == name]
            role_type <- if (length(hit) == 1L) hit else "category"
            item <- arpillar::data_item(name = name, role_type = role_type)
            update_object(
              store,
              obj_id,
              function(o) .add_item_to_slot(o, sid, item),
              label = paste0("assign ", sid)
            )
            # No explicit picker-clear needed: `update_object()` reassigns
            # `store$rv$report`, which changes `.roles_digest()`, which
            # re-triggers `output$slots`' own `renderUI` -- a FRESH
            # `.eligible_picker()` is built from scratch (empty selection,
            # the just-added name already excluded from `choices`), so the
            # old widget is simply replaced rather than needing to be
            # reset in place.
          },
          ignoreInit = TRUE
        )

        shiny::observeEvent(
          input[[reorder_input]],
          {
            obj_id <- store$rv$selected
            if (is.null(obj_id)) {
              return()
            }
            order <- vapply(
              input[[reorder_input]]$order,
              as.character,
              character(1)
            )
            update_object(
              store,
              obj_id,
              function(o) .reorder_slot(o, sid, order),
              label = paste0("reorder ", sid)
            )
          },
          ignoreInit = TRUE
        )
      })
    }

    # One shared observer for every remove click, regardless of slot/item
    # -- `.assigned_row()` posts `{slot, name}` straight off its own inline
    # onclick (the `mod_contents.R` `.toc_kebab()` pattern), so this is a
    # single registration rather than one per (slot, item) pair that would
    # need re-registering on every render.
    shiny::observeEvent(input$remove, {
      obj_id <- store$rv$selected
      if (is.null(obj_id)) {
        return()
      }
      req <- input$remove
      update_object(
        store,
        obj_id,
        function(o) .remove_item_from_slot(o, req$slot, req$name),
        label = paste0("remove ", req$name, " from ", req$slot)
      )
    })

    invisible(NULL)
  })
}
