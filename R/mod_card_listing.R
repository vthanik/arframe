# The listing generator's structured-option editors (Options pane): the
# listing schema carries three structured kinds the generic schema rows skip
# -- `sort` (ORDER BY keys), `transpose` (BDS PIVOT), `stacks` (multi-line
# glued display columns). Cell recodes live in the Roles LEVELS editor
# (item @levels -- one place, not two); the decode footnote is the user's
# own Footnotes line. Scalar `limit` auto-surfaces through the generic int
# row; `column_specs` / `column_order` have no pane UI yet. Everything here
# renders the COMMITTED
# `object@options` (server-authoritative -- the store is the only state
# carrier); every dynamic per-row control posts through a SHARED input via
# an inline `Shiny.setInputValue({i, ..., nonce})` (the `.assigned_row()` /
# `.toc_kebab()` pattern), never a per-row observer inside a renderUI.

# ---- engine contract -------------------------------------------------------

# The EXACT aggregate set the engine's transpose PIVOT compiles
# (fct_render_listing.R `.pivot_agg_fn`). Order is display order.
.LISTING_AGGS <- c(
  "First (default)" = "first",
  "Mean" = "mean",
  "Max" = "max",
  "Min" = "min"
)

# The per-entry glue fields a stack line carries beside its vars.
.STACK_ENTRY_FIELDS <- c("delim", "prefix", "suffix")

#' A committed listing option as a plain list -- `NULL`/non-list reads as
#' empty, so every editor renders off one shape.
#' @noRd
.listing_opt_list <- function(object, key) {
  x <- object@options[[key]]
  if (is.list(x)) x else list()
}

#' The items-meta rows for the listing's SELECTED variables (id first,
#' then the list variables, role order) -- the choice set the transpose
#' and stack editors offer, so a pick can only reference a displayed
#' column. `extra` re-admits a committed value the user has since
#' deselected, so a stale select still shows it. Sort keys keep the FULL
#' dataset meta (ORDER BY needs no display presence).
#' @noRd
.listing_selected_items <- function(object, items, extra = character(0)) {
  slot_names <- function(slot) {
    r <- .role_for_slot(object, slot)
    if (is.null(r)) {
      return(character(0))
    }
    vapply(r@items, function(it) it@name, character(1))
  }
  sel <- unique(c(slot_names("id"), slot_names("columns"), extra))
  items[match(intersect(sel, items$name), items$name), , drop = FALSE]
}

# ---- shared controls -------------------------------------------------------

#' A per-row "add variable" picker for the DYNAMIC listing blocks: the
#' shared chip + name + muted-label render, always empty (it only ADDS).
#' On pick it posts `{i[, j], value, nonce}` to the SHARED `target_input`
#' observer and clears -- the row indices are baked in here, so no per-row
#' Shiny observer leaks inside the renderUI (the `.rich_picker()` idiom).
#' @noRd
.listing_add_picker <- function(
  ns,
  target_input,
  items,
  i,
  j = NULL,
  placeholder = "Add a variable"
) {
  packed <- vapply(
    seq_len(nrow(items)),
    function(k) {
      .pack_item_choice(items$name[[k]], items$type[[k]], items$label[[k]])
    },
    character(1)
  )
  keys <- if (is.null(j)) {
    sprintf("i: %d", as.integer(i))
  } else {
    sprintf("i: %d, j: %d", as.integer(i), as.integer(j))
  }
  onchange <- sprintf(
    paste0(
      "function(value) { if (value) { ",
      "Shiny.setInputValue('%s', {%s, value: value, nonce: Date.now()}, ",
      "{priority: 'event'}); this.setValue(''); } }"
    ),
    ns(target_input),
    keys
  )
  .ar_picker_select(
    ns = ns,
    input_id = paste0(
      target_input,
      "_",
      i,
      if (!is.null(j)) paste0("_", j) else ""
    ),
    choices = stats::setNames(packed, packed),
    placeholder = placeholder,
    onchange = onchange,
    class = "ar-add-picker"
  )
}

#' A blur/Enter-commit text field for one DYNAMIC listing row: posts
#' `{i[, j], field, value, nonce}` to the shared `target_input` on change
#' -- typing never commits per keystroke, and a plain text commit never
#' redraws the pane mid-edit (the `.opt_change_input()` contract).
#' @noRd
.listing_field_input <- function(
  ns,
  target_input,
  i,
  field,
  value,
  placeholder = NULL,
  j = NULL,
  width = NULL
) {
  keys <- if (is.null(j)) {
    sprintf("i: %d", as.integer(i))
  } else {
    sprintf("i: %d, j: %d", as.integer(i), as.integer(j))
  }
  js <- sprintf(
    "Shiny.setInputValue('%s', {%s, field: '%s', value: this.value, nonce: Date.now()}, {priority: 'event'})",
    ns(target_input),
    keys,
    field
  )
  shiny::tags$input(
    type = "text",
    class = "ar-fn-input",
    value = value %||% "",
    placeholder = placeholder,
    onchange = js,
    style = if (!is.null(width)) paste0("width:", width, ";"),
    `aria-label` = paste(field, "for row", i)
  )
}

#' A remove X posting `{i[, j], nonce}` to the shared `target_input` --
#' the same X every assigned row / filter chip wears.
#' @noRd
.listing_rm_btn <- function(ns, target_input, i, label, j = NULL) {
  keys <- if (is.null(j)) {
    sprintf("i: %d", as.integer(i))
  } else {
    sprintf("i: %d, j: %d", as.integer(i), as.integer(j))
  }
  shiny::tags$button(
    type = "button",
    class = "ar-icon-btn ar-fn-remove",
    `aria-label` = label,
    onclick = sprintf(
      "Shiny.setInputValue('%s', {%s, nonce: Date.now()}, {priority: 'event'})",
      ns(target_input),
      keys
    ),
    .icon("close", 11)
  )
}

#' An inline "+ <label>" link button posting `{i?, nonce}` to a shared
#' input (the `span_add` idiom -- plain onclick, no per-block actionButton).
#' @noRd
.listing_add_btn <- function(ns, target_input, label, i = NULL) {
  keys <- if (is.null(i)) "" else sprintf("i: %d, ", as.integer(i))
  shiny::tags$button(
    type = "button",
    class = "btn btn-link ar-fn-add",
    onclick = sprintf(
      "Shiny.setInputValue('%s', {%snonce: Date.now()}, {priority: 'event'})",
      ns(target_input),
      keys
    ),
    label
  )
}

# ---- SORT ------------------------------------------------------------------

#' One committed sort-key row: the column (mono), an asc|desc toggle (the
#' `.retype_control()` two-button idiom), and a remove X. Toggle + remove
#' post `{i, ...}` shared inputs.
#' @noRd
.srt_row <- function(ns, i, s) {
  dir <- if (identical(tolower(s$dir %||% "asc"), "desc")) "desc" else "asc"
  dir_btn <- function(d) {
    active <- identical(dir, d)
    shiny::tags$button(
      type = "button",
      class = paste0("ar-peek-type-btn", if (active) " ar-peek-type-on"),
      `aria-pressed` = if (active) "true" else "false",
      onclick = sprintf(
        "Shiny.setInputValue('%s', {i: %d, dir: '%s', nonce: Date.now()}, {priority: 'event'})",
        ns("srt_dir"),
        i,
        d
      ),
      d
    )
  }
  shiny::tags$div(
    class = "ar-fn-row",
    shiny::tags$span(class = "ar-role-name ar-mono", s$col %||% ""),
    shiny::tags$div(
      class = "ar-peek-type",
      role = "group",
      `aria-label` = paste0("Sort direction for ", s$col %||% ""),
      dir_btn("asc"),
      dir_btn("desc")
    ),
    .listing_rm_btn(ns, "srt_rm", i, paste0("Remove sort key ", s$col %||% ""))
  )
}

#' The SORT section: one row per committed key + a trailing add-picker.
#' Sort keys may be ANY dataset column (displayed or not) -- the engine's
#' ORDER BY needs no display presence, so the picker is unfiltered.
#' @noRd
.listing_sort_section <- function(ns, object, items) {
  srt <- .listing_opt_list(object, "sort")
  .opt_section(
    "SORT",
    list(
      lapply(seq_along(srt), function(i) .srt_row(ns, i, srt[[i]])),
      .eligible_picker(ns, "srt_add", items, placeholder = "Add a sort key"),
      shiny::tags$p(
        class = "ar-opt-hint ar-mono",
        "Keys may be any dataset column \u2014 displayed or not."
      )
    )
  )
}

# ---- TRANSPOSE -------------------------------------------------------------

#' The TRANSPOSE section: Parameter / Value / On-duplicates selects with
#' static ids (there is only one transpose editor). The server re-derives
#' the WHOLE option from the three inputs on every change; the key commits
#' only while param and value are both set and distinct, else it is absent.
#' Choices are the SELECTED list variables (id + columns roles), never the
#' whole dataset -- a transpose can only consume displayed columns.
#' @noRd
.listing_transpose_section <- function(ns, object, items) {
  tr <- object@options$transpose
  tr <- if (is.list(tr)) tr else list()
  items <- .listing_selected_items(
    object,
    items,
    extra = as.character(c(
      tr$param %||% character(0),
      tr$value %||% character(0)
    ))
  )
  sel_row <- function(input_id, label, choices, selected) {
    shiny::tags$div(
      class = "ar-opt-row",
      shiny::tags$span(class = "ar-opt-label", label),
      shiny::selectInput(
        ns(input_id),
        label = NULL,
        choices = choices,
        selected = selected,
        selectize = FALSE,
        width = "170px"
      )
    )
  }
  numeric_cols <- items$name[items$type %in% "measure"]
  .opt_section(
    "TRANSPOSE",
    list(
      sel_row(
        "tr_param",
        "Parameter",
        c("(none)" = "", items$name),
        as.character(tr$param %||% "")
      ),
      sel_row(
        "tr_value",
        "Value",
        c("(none)" = "", numeric_cols),
        as.character(tr$value %||% "")
      ),
      sel_row(
        "tr_agg",
        "On duplicates",
        .LISTING_AGGS,
        as.character(tr$agg %||% "first")
      ),
      shiny::tags$p(
        class = "ar-opt-hint ar-mono",
        paste(
          "Spreads the parameter's levels to columns \u2014 needs both",
          "Parameter and Value. Choices are the selected list variables."
        )
      )
    )
  )
}

# ---- STACKED COLUMNS -------------------------------------------------------

#' One stack entry line: its vars as removable chips, an add-var picker,
#' small delim/prefix/suffix fields, and a remove-line X. An entry with no
#' vars is skipped by the engine, so it can exist transiently while being
#' built.
#' @noRd
.stack_entry_row <- function(ns, i, j, entry, items) {
  vars <- as.character(unlist(entry$vars %||% character(0)))
  chips <- lapply(vars, function(v) {
    shiny::tags$span(
      class = "ar-flt-chip",
      shiny::tags$span(class = "ar-mono", v),
      shiny::tags$span(
        class = "ar-flt-chip-x",
        role = "button",
        `aria-label` = paste0("Remove ", v, " from stack line"),
        onclick = sprintf(
          "Shiny.setInputValue('%s', {i: %d, j: %d, name: '%s', nonce: Date.now()}, {priority: 'event'})",
          ns("stk_var_rm"),
          i,
          j,
          v
        ),
        .icon("close", 10)
      )
    )
  })
  shiny::tags$div(
    class = "ar-fn-row ar-lst-entry",
    shiny::tags$div(class = "ar-flt-chips", chips),
    .listing_add_picker(ns, "stk_var_add", items, i, j),
    .listing_field_input(
      ns,
      "stk_entry_field",
      i,
      "delim",
      entry$delim,
      placeholder = "/",
      j = j,
      width = "48px"
    ),
    .listing_field_input(
      ns,
      "stk_entry_field",
      i,
      "prefix",
      entry$prefix,
      placeholder = "(",
      j = j,
      width = "48px"
    ),
    .listing_field_input(
      ns,
      "stk_entry_field",
      i,
      "suffix",
      entry$suffix,
      placeholder = ")",
      j = j,
      width = "48px"
    ),
    .listing_rm_btn(
      ns,
      "stk_line_rm",
      i,
      paste0("Remove stack ", i, " line ", j),
      j = j
    )
  )
}

#' One stack block: name field, Indent toggle, remove-stack X, its entry
#' lines, and "+ Add line". Each stack is ONE output column whose entries
#' stack as lines in the cell.
#' @noRd
.stack_block <- function(ns, i, st, items) {
  entries <- if (is.list(st$entries)) st$entries else list()
  shiny::tags$div(
    class = "ar-opt-row ar-opt-row-block",
    shiny::tags$div(
      class = "ar-fn-row",
      .listing_field_input(
        ns,
        "stk_field",
        i,
        "name",
        st$name,
        placeholder = "Header label (\\n = line break)"
      ),
      shiny::tags$label(
        class = "ar-lst-indent",
        shiny::tags$input(
          type = "checkbox",
          checked = if (isTRUE(st$indent)) "checked",
          onchange = sprintf(
            "Shiny.setInputValue('%s', {i: %d, on: this.checked, nonce: Date.now()}, {priority: 'event'})",
            ns("stk_indent"),
            i
          ),
          `aria-label` = paste0("Indent continuation lines of stack ", i)
        ),
        shiny::tags$span("Indent")
      ),
      .listing_rm_btn(ns, "stk_rm", i, paste0("Remove stack ", i))
    ),
    lapply(seq_along(entries), function(j) {
      .stack_entry_row(ns, i, j, entries[[j]], items)
    }),
    .listing_add_btn(ns, "stk_line_add", "+ Add line", i = i)
  )
}

#' The STACKED COLUMNS section: one block per committed stack + "+ Add
#' stack". Pickers offer only the SELECTED variables (id + list
#' variables) -- a stack glues displayed columns, never free text.
#' @noRd
.listing_stacks_section <- function(ns, object, items) {
  st <- .listing_opt_list(object, "stacks")
  sel <- .listing_selected_items(object, items)
  .opt_section(
    "STACKED COLUMNS",
    list(
      lapply(seq_along(st), function(i) .stack_block(ns, i, st[[i]], sel)),
      .listing_add_btn(ns, "stk_add", "+ Add stack"),
      shiny::tags$p(
        class = "ar-opt-hint ar-mono",
        paste(
          "Each stack is one column; its lines stack inside the cell.",
          "Pick from the selected variables; line 2+ wraps in",
          "parentheses by default."
        )
      )
    )
  )
}

# ---- entry: sections -------------------------------------------------------

#' The listing structured-option sections (SORT / TRANSPOSE / STACKED
#' COLUMNS) for the Options pane -- `NULL` for any other generator.
#' Renders the COMMITTED `object@options` every time; no draft state
#' lives outside the store. Cell recodes live in the Roles LEVELS editor.
#' @noRd
.opt_listing_sections <- function(con, ns, object) {
  if (!identical(object@type, "listing")) {
    return(NULL)
  }
  items <- .items_meta_for(con, object)
  shiny::tagList(
    .listing_sort_section(ns, object, items),
    .listing_transpose_section(ns, object, items),
    .listing_stacks_section(ns, object, items)
  )
}

# ---- server: observers -----------------------------------------------------

#' Commit one listing option key on the SELECTED object: an emptied list
#' removes the key entirely (the engine's own defaults then apply), and an
#' identical value is a no-op -- a bind-time repost never pushes an undo
#' entry. Returns TRUE when a commit landed.
#' @noRd
.commit_listing_key <- function(store, obj, key, value, label) {
  if (is.list(value) && length(value) == 0L) {
    value <- NULL
  }
  if (identical(value, obj@options[[key]])) {
    return(invisible(FALSE))
  }
  update_object(
    store,
    obj@id,
    function(o) {
      opts <- o@options
      opts[[key]] <- value
      S7::set_props(o, options = opts)
    },
    label = label
  )
  invisible(TRUE)
}

#' Register the listing-option commit observers ONCE (module scope, from
#' `mod_card_options_server`). Structural commits (row add/remove/toggle,
#' a var pick) bump `pane_redraw` so the pane
#' repaints with fresh row keys; plain text-field commits never do --
#' typing must not redraw the input mid-edit.
#' @noRd
.listing_option_observers <- function(
  input,
  output,
  session,
  store,
  pane_redraw
) {
  # The selected object, but only when it IS a listing -- every observer
  # below is inert for any other generator (a stale client post from a
  # previously selected listing never touches a summary's options).
  lst_obj <- function() {
    obj <- selected_object(store)
    if (is.null(obj) || !identical(obj@type, "listing")) {
      return(NULL)
    }
    obj
  }
  redraw <- function() pane_redraw(pane_redraw() + 1L)
  # A shared bounds guard: the 1-based row index off a `{i, ...}` payload,
  # or NA when it outlived the list it points into.
  row_i <- function(payload, n) {
    i <- as.integer(payload$i %||% 0L)
    if (is.na(i) || i < 1L || i > n) NA_integer_ else i
  }

  # ---- sort ----
  # No `ignoreInit` (the schema-row observers' own rule): the empty
  # add-picker's bind-time post is "" and the guard drops it.
  shiny::observeEvent(input$srt_add, {
    choice <- input$srt_add
    obj <- lst_obj()
    if (is.null(obj) || is.null(choice) || !nzchar(choice)) {
      return()
    }
    srt <- .listing_opt_list(obj, "sort")
    srt[[length(srt) + 1L]] <- list(
      col = .unpack_item_name(choice),
      dir = "asc"
    )
    .commit_listing_key(store, obj, "sort", srt, "add sort key")
    redraw()
  })

  shiny::observeEvent(input$srt_dir, {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    srt <- .listing_opt_list(obj, "sort")
    i <- row_i(input$srt_dir, length(srt))
    dir <- as.character(input$srt_dir$dir %||% "")
    if (is.na(i) || !dir %in% c("asc", "desc")) {
      return()
    }
    srt[[i]]$dir <- dir
    if (.commit_listing_key(store, obj, "sort", srt, "flip sort direction")) {
      redraw()
    }
  })

  shiny::observeEvent(input$srt_rm, {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    srt <- .listing_opt_list(obj, "sort")
    i <- row_i(input$srt_rm, length(srt))
    if (is.na(i)) {
      return()
    }
    .commit_listing_key(store, obj, "sort", srt[-i], "remove sort key")
    redraw()
  })

  # ---- transpose ----
  # Re-derive the WHOLE option from the three inputs (Shiny holds each
  # select's last post, so a half-built pick needs no store draft):
  # committed only while param and value are both set and DISTINCT, else
  # the key is absent -- the engine's own completeness rule.
  tr_commit <- function() {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    p <- as.character(input$tr_param %||% "")
    v <- as.character(input$tr_value %||% "")
    a <- as.character(input$tr_agg %||% "first")
    if (!a %in% .LISTING_AGGS) {
      a <- "first"
    }
    value <- if (nzchar(p) && nzchar(v) && !identical(p, v)) {
      list(param = p, value = v, agg = a)
    } else {
      NULL
    }
    .commit_listing_key(store, obj, "transpose", value, "set transpose")
  }
  shiny::observeEvent(input$tr_param, tr_commit())
  shiny::observeEvent(input$tr_value, tr_commit())
  shiny::observeEvent(input$tr_agg, tr_commit())

  # ---- stacks ----
  shiny::observeEvent(input$stk_add, {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    st <- .listing_opt_list(obj, "stacks")
    # Seed line 1 so the new block opens on a picker, not a bare name
    # field (typing "AGE/SEX/RACE" into the name was the error-prone path).
    st[[length(st) + 1L]] <- list(
      name = NULL,
      indent = FALSE,
      entries = list(list(vars = character(0)))
    )
    .commit_listing_key(store, obj, "stacks", st, "add stacked column")
    redraw()
  })

  shiny::observeEvent(input$stk_rm, {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    st <- .listing_opt_list(obj, "stacks")
    i <- row_i(input$stk_rm, length(st))
    if (is.na(i)) {
      return()
    }
    .commit_listing_key(store, obj, "stacks", st[-i], "remove stacked column")
    redraw()
  })

  shiny::observeEvent(input$stk_field, {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    st <- .listing_opt_list(obj, "stacks")
    i <- row_i(input$stk_field, length(st))
    if (is.na(i) || !identical(input$stk_field$field, "name")) {
      return()
    }
    val <- trimws(as.character(input$stk_field$value %||% ""))
    st[[i]]$name <- if (nzchar(val)) val else NULL
    .commit_listing_key(store, obj, "stacks", st, "rename stacked column")
  })

  shiny::observeEvent(input$stk_indent, {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    st <- .listing_opt_list(obj, "stacks")
    i <- row_i(input$stk_indent, length(st))
    if (is.na(i)) {
      return()
    }
    st[[i]]$indent <- isTRUE(input$stk_indent$on)
    .commit_listing_key(store, obj, "stacks", st, "toggle stack indent")
  })

  shiny::observeEvent(input$stk_line_add, {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    st <- .listing_opt_list(obj, "stacks")
    i <- row_i(input$stk_line_add, length(st))
    if (is.na(i)) {
      return()
    }
    entries <- if (is.list(st[[i]]$entries)) st[[i]]$entries else list()
    # An entry with no vars is skipped by the engine -- it exists
    # transiently while the user builds the line. A continuation line
    # (2+) prefills the DPP paren wrap: "USUBJID" over "(Age/Sex/Race)".
    entries[[length(entries) + 1L]] <- if (length(entries) >= 1L) {
      list(vars = character(0), prefix = "(", suffix = ")")
    } else {
      list(vars = character(0))
    }
    st[[i]]$entries <- entries
    .commit_listing_key(store, obj, "stacks", st, "add stack line")
    redraw()
  })

  shiny::observeEvent(input$stk_line_rm, {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    st <- .listing_opt_list(obj, "stacks")
    i <- row_i(input$stk_line_rm, length(st))
    if (is.na(i)) {
      return()
    }
    entries <- if (is.list(st[[i]]$entries)) st[[i]]$entries else list()
    j <- as.integer(input$stk_line_rm$j %||% 0L)
    if (is.na(j) || j < 1L || j > length(entries)) {
      return()
    }
    st[[i]]$entries <- entries[-j]
    .commit_listing_key(store, obj, "stacks", st, "remove stack line")
    redraw()
  })

  shiny::observeEvent(input$stk_var_add, {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    st <- .listing_opt_list(obj, "stacks")
    i <- row_i(input$stk_var_add, length(st))
    choice <- as.character(input$stk_var_add$value %||% "")
    if (is.na(i) || !nzchar(choice)) {
      return()
    }
    entries <- if (is.list(st[[i]]$entries)) st[[i]]$entries else list()
    j <- as.integer(input$stk_var_add$j %||% 0L)
    if (is.na(j) || j < 1L || j > length(entries)) {
      return()
    }
    name <- .unpack_item_name(choice)
    vars <- as.character(unlist(entries[[j]]$vars %||% character(0)))
    entries[[j]]$vars <- unique(c(vars, name))
    st[[i]]$entries <- entries
    .commit_listing_key(store, obj, "stacks", st, "add stack variable")
    redraw()
  })

  shiny::observeEvent(input$stk_var_rm, {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    st <- .listing_opt_list(obj, "stacks")
    i <- row_i(input$stk_var_rm, length(st))
    if (is.na(i)) {
      return()
    }
    entries <- if (is.list(st[[i]]$entries)) st[[i]]$entries else list()
    j <- as.integer(input$stk_var_rm$j %||% 0L)
    if (is.na(j) || j < 1L || j > length(entries)) {
      return()
    }
    vars <- as.character(unlist(entries[[j]]$vars %||% character(0)))
    entries[[j]]$vars <- setdiff(vars, as.character(input$stk_var_rm$name))
    st[[i]]$entries <- entries
    .commit_listing_key(store, obj, "stacks", st, "remove stack variable")
    redraw()
  })

  shiny::observeEvent(input$stk_entry_field, {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    st <- .listing_opt_list(obj, "stacks")
    i <- row_i(input$stk_entry_field, length(st))
    field <- as.character(input$stk_entry_field$field %||% "")
    if (is.na(i) || !field %in% .STACK_ENTRY_FIELDS) {
      return()
    }
    entries <- if (is.list(st[[i]]$entries)) st[[i]]$entries else list()
    j <- as.integer(input$stk_entry_field$j %||% 0L)
    if (is.na(j) || j < 1L || j > length(entries)) {
      return()
    }
    val <- as.character(input$stk_entry_field$value %||% "")
    # An emptied glue field falls back to the engine default -- keep the
    # committed entry minimal (no empty-string keys).
    entries[[j]][[field]] <- if (nzchar(val)) val else NULL
    st[[i]]$entries <- entries
    .commit_listing_key(store, obj, "stacks", st, "edit stack line glue")
  })

  invisible(NULL)
}
