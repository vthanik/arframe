# The listing generator's structured-option editors (Options pane): the
# listing schema carries three structured kinds the generic schema rows skip
# — `sort` (ORDER BY keys), `transpose` (BDS PIVOT), `stacks` (multi-line
# glued display columns). Cell recodes live in the Roles LEVELS editor
# (item @levels — one place, not two); the decode footnote is the user's
# own Footnotes line. Scalar `limit` auto-surfaces through the generic int
# row; `column_specs$format` edits through the DATE FORMATS section; the
# other `column_specs` fields / `column_order` have no pane UI yet. Everything here
# renders the COMMITTED
# `object@options` (server-authoritative — the store is the only state
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

# Preset choices for the glue selects. Values are what commits ("" = engine
# default); an off-list committed value is injected as its own option.
.STACK_GLUE_CHOICES <- list(
  delim = c("/" = "", "," = ",", "space" = " ", "-" = "-", "new line" = "\\n"),
  prefix = c("none" = "", "(" = "(", "[" = "[", "{" = "{"),
  suffix = c("none" = "", ")" = ")", "]" = "]", "}" = "}")
)

# Common date/time formats offered by the DATE FORMATS datalist, keyed by
# the column's actual sub-taxonomy so a DATE column only sees date formats,
# a TIME column only time formats, and a TIMESTAMP column only datetime
# formats -- a plain date column should never advertise `time8.` (nothing
# to render) and a time column should never advertise `date9.`. SAS names
# and picture patterns the engine's `.datetime_strftime()` resolves.
.FMT_PRESETS <- list(
  date = c(
    "date9.",
    "date11.",
    "yymmdd10.",
    "mmddyy10.",
    "ddmmyy10.",
    "mm/dd/yyyy",
    "dd-mon-yyyy"
  ),
  time = c("time8.", "time5."),
  datetime = c("datetime20.")
)

#' Classify one column's raw DuckDB `sql_type` into `"date"` / `"time"` /
#' `"datetime"` -- the DATE FORMATS section shows only the presets that
#' actually render on that column. Anything unrecognised falls back to
#' `"datetime"` (the widest option set) rather than dropping the row.
#' @noRd
.date_subtype <- function(sql_type) {
  t <- toupper(as.character(sql_type %||% ""))
  if (grepl("^TIMESTAMP", t)) {
    return("datetime")
  }
  if (grepl("^TIME", t)) {
    return("time")
  }
  if (grepl("^DATE", t)) {
    return("date")
  }
  "datetime"
}

#' A committed listing option as a plain list — `NULL`/non-list reads as
#' empty, so every editor renders off one shape.
#' @noRd
.listing_opt_list <- function(object, key) {
  x <- object@options[[key]]
  if (is.list(x)) x else list()
}

#' The items-meta rows for the listing's SELECTED variables (id first,
#' then the list variables, role order) — the choice set the transpose
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
#' observer and clears — the row indices are baked in here, so no per-row
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
#' — typing never commits per keystroke, and a plain text commit never
#' redraws the pane mid-edit (the `.opt_change_input()` contract).
#' @noRd
.listing_field_input <- function(
  ns,
  target_input,
  i,
  field,
  value,
  placeholder = NULL,
  j = NULL
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
    `aria-label` = paste(field, "for row", i)
  )
}

#' A remove X posting `{i[, j], nonce}` to the shared `target_input` —
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
#' input (the `span_add` idiom — plain onclick, no per-block actionButton).
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
    class = "ar-fn-row ar-srt-row",
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
#' Sort keys may be ANY dataset column (displayed or not) — the engine's
#' ORDER BY needs no display presence, so the picker is unfiltered.
#' @noRd
.listing_sort_section <- function(ns, object, items) {
  srt <- .listing_opt_list(object, "sort")
  .opt_section(
    "SORT",
    help = .help_icon(ns, "listing_sort"),
    rows = list(
      lapply(seq_along(srt), function(i) .srt_row(ns, i, srt[[i]])),
      .eligible_picker(ns, "srt_add", items, placeholder = "Add a sort key")
    )
  )
}

# ---- TRANSPOSE -------------------------------------------------------------

#' The TRANSPOSE section: Parameter / Value / On-duplicates selects with
#' static ids (there is only one transpose editor). The server re-derives
#' the WHOLE option from the three inputs on every change; the key commits
#' only while param and value are both set and distinct, else it is absent.
#' Choices are the SELECTED list variables (id + columns roles), never the
#' whole dataset — a transpose can only consume displayed columns.
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
  # Id-less native selects (the last known real-id exception, closed
  # 2026-07-18): a drill-switch rebind can never replay a stale value —
  # posts ride the same `tr_*` channels, so the observers are untouched.
  sel_row <- function(input_id, label, choices, selected) {
    shiny::tags$div(
      class = "ar-opt-row",
      shiny::tags$span(class = "ar-opt-label", label),
      .opt_native_select(ns, input_id, choices, selected)
    )
  }
  numeric_cols <- items$name[items$type %in% "measure"]
  .opt_section(
    "TRANSPOSE",
    help = .help_icon(ns, "listing_transpose"),
    rows = list(
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
      )
    )
  )
}

# ---- DATE FORMATS ----------------------------------------------------------

#' The DATE FORMATS section: one row per SELECTED date/time variable with a
#' free-text-plus-datalist combo (native `<input list>`) committing
#' `column_specs[[var]]$format` — SAS names (date9.), picture patterns
#' (mm/dd/yyyy), or raw strftime, resolved engine-side by
#' `.datetime_strftime()`. `NULL` when no date variable is selected.
#' @noRd
.listing_formats_section <- function(ns, object, items) {
  sel <- .listing_selected_items(object, items)
  dates <- sel[sel$type %in% "date", , drop = FALSE]
  if (nrow(dates) == 0L) {
    return(NULL)
  }
  specs <- .listing_opt_list(object, "column_specs")
  subtype <- vapply(
    seq_len(nrow(dates)),
    function(k) .date_subtype(dates$sql_type[[k]]),
    character(1)
  )
  # Placeholder mirrors the row's subtype so an empty field hints at the
  # right shape (date9. / time8. / datetime20.) rather than always "date9.".
  placeholders <- c(date = "date9.", time = "time8.", datetime = "datetime20.")
  rows <- lapply(seq_len(nrow(dates)), function(k) {
    nm <- dates$name[[k]]
    js <- sprintf(
      "Shiny.setInputValue('%s', {name: '%s', value: this.value, nonce: Date.now()}, {priority: 'event'})",
      ns("fmt_set"),
      nm
    )
    shiny::tags$div(
      class = "ar-opt-row",
      shiny::tags$span(class = "ar-role-name ar-mono", nm),
      shiny::tags$input(
        type = "text",
        class = "ar-lst-fmt ar-mono",
        list = ns(paste0("fmt_presets_", subtype[[k]])),
        value = as.character(specs[[nm]]$format %||% ""),
        placeholder = placeholders[[subtype[[k]]]],
        onchange = js,
        `aria-label` = paste("Display format for", nm)
      )
    )
  })
  # One datalist per subtype that is actually present -- browsers cache the
  # first `<datalist>` with a given id, so only emit ones the rows point at.
  used <- unique(subtype)
  datalists <- lapply(used, function(sub) {
    shiny::tags$datalist(
      id = ns(paste0("fmt_presets_", sub)),
      lapply(.FMT_PRESETS[[sub]], function(f) shiny::tags$option(value = f))
    )
  })
  .opt_section(
    "DATE FORMATS",
    help = .help_icon(ns, "listing_formats"),
    rows = list(rows, datalists)
  )
}

# ---- STACKED COLUMNS -------------------------------------------------------

#' One glue select (delim / prefix / suffix) for a stack line: preset
#' choices, the committed off-list value injected, posting `{i, j, field,
#' value}` to the SAME shared `stk_entry_field` input the old text fields
#' used — the server observer is unchanged.
#' @noRd
.stack_glue_select <- function(ns, i, j, field, value) {
  value <- as.character(value %||% "")
  choices <- .STACK_GLUE_CHOICES[[field]]
  if (!value %in% choices) {
    choices <- c(stats::setNames(value, value), choices)
  }
  js <- sprintf(
    "Shiny.setInputValue('%s', {i: %d, j: %d, field: '%s', value: this.value, nonce: Date.now()}, {priority: 'event'})",
    ns("stk_entry_field"),
    i,
    j,
    field
  )
  shiny::tags$label(
    class = "ar-lst-glue-field",
    shiny::tags$span(c(
      delim = "Delimiter",
      prefix = "Prefix",
      suffix = "Suffix"
    )[[field]]),
    shiny::tags$select(
      class = "ar-lst-glue-sel",
      onchange = js,
      `aria-label` = sprintf("%s for stack %d line %d", field, i, j),
      lapply(seq_along(choices), function(k) {
        shiny::tags$option(
          value = choices[[k]],
          selected = if (identical(choices[[k]], value)) "selected",
          names(choices)[[k]]
        )
      })
    )
  )
}

#' One stack entry line, all on ONE row: its vars as removable chips with a
#' compact add-picker inline, a mono glue-preview button that reveals the
#' Delimiter/Prefix/Suffix selects (hidden by default), and a hover-shown
#' remove-line X. An entry with no vars is skipped by the engine, so it can
#' exist transiently while being built.
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
  glue_preview <- paste0(
    entry$prefix %||% "",
    entry$delim %||% "/",
    entry$suffix %||% ""
  )
  shiny::tags$div(
    class = "ar-lst-line",
    shiny::tags$div(
      class = "ar-lst-line-main",
      shiny::tags$div(
        class = "ar-flt-chips ar-lst-vars",
        chips,
        .listing_add_picker(ns, "stk_var_add", items, i, j, placeholder = "+")
      ),
      shiny::tags$button(
        type = "button",
        class = "ar-lst-glue-btn ar-mono",
        `aria-expanded` = "false",
        `aria-label` = sprintf("Edit separators for stack %d line %d", i, j),
        title = "Delimiter / prefix / suffix",
        onclick = paste0(
          "this.setAttribute('aria-expanded', ",
          "this.closest('.ar-lst-line').classList.toggle('ar-lst-glue-open'));"
        ),
        glue_preview
      ),
      .listing_rm_btn(
        ns,
        "stk_line_rm",
        i,
        paste0("Remove stack ", i, " line ", j),
        j = j
      )
    ),
    shiny::tags$div(
      class = "ar-lst-glue",
      .stack_glue_select(ns, i, j, "delim", entry$delim),
      .stack_glue_select(ns, i, j, "prefix", entry$prefix),
      .stack_glue_select(ns, i, j, "suffix", entry$suffix)
    )
  )
}

#' One stack block: a single header row (name field, Indent pill toggle,
#' remove-stack X), its entry lines, and "+ Add line". Each stack is ONE
#' output column whose entries stack as lines in the cell; Indent steps
#' each line two spaces deeper than the one above (engine-side).
#' The pill flips its own class client-side (the commit never redraws).
#' @noRd
.stack_block <- function(ns, i, st, items) {
  entries <- if (is.list(st$entries)) st$entries else list()
  on <- isTRUE(st$indent)
  shiny::tags$div(
    class = "ar-opt-row ar-opt-row-block",
    shiny::tags$div(
      class = "ar-fn-row ar-lst-head",
      .listing_field_input(
        ns,
        "stk_field",
        i,
        "name",
        st$name,
        placeholder = "Header label (\\n = line break)"
      ),
      shiny::tags$button(
        type = "button",
        class = paste0(
          "ar-peek-type-btn ar-lst-indent",
          if (on) " ar-peek-type-on"
        ),
        `aria-pressed` = if (on) "true" else "false",
        title = "Step each line two more spaces",
        onclick = sprintf(
          paste0(
            "var on = this.classList.toggle('ar-peek-type-on'); ",
            "this.setAttribute('aria-pressed', on); ",
            "Shiny.setInputValue('%s', {i: %d, on: on, nonce: Date.now()}, ",
            "{priority: 'event'});"
          ),
          ns("stk_indent"),
          i
        ),
        "Indent"
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
#' variables) — a stack glues displayed columns, never free text.
#' @noRd
.listing_stacks_section <- function(ns, object, items) {
  st <- .listing_opt_list(object, "stacks")
  sel <- .listing_selected_items(object, items)
  .opt_section(
    "STACKED COLUMNS",
    help = .help_icon(ns, "listing_stack"),
    rows = list(
      lapply(seq_along(st), function(i) .stack_block(ns, i, st[[i]], sel)),
      .listing_add_btn(ns, "stk_add", "+ Add stack")
    )
  )
}

# ---- entry: sections -------------------------------------------------------

#' The listing structured-option sections (SORT / TRANSPOSE / STACKED
#' COLUMNS) for the Options pane — `NULL` for any other generator.
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
    .listing_formats_section(ns, object, items),
    .listing_stacks_section(ns, object, items)
  )
}

# ---- server: observers -----------------------------------------------------

#' Commit one listing option key on the SELECTED object: an emptied list
#' removes the key entirely (the engine's own defaults then apply), and an
#' identical value is a no-op — a bind-time repost never pushes an undo
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
#' repaints with fresh row keys; plain text-field commits never do —
#' typing must not redraw the input mid-edit.
#' @noRd
.listing_option_observers <- function(
  input,
  output,
  session,
  store,
  pane_redraw
) {
  # The selected object, but only when it IS a listing — every observer
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
  # A module-local draft holds the half-built pick, RESEEDED from the
  # committed spec on every selection change — so a drill switch can never
  # bleed one output's param/value into another's (the old bound selects
  # relied on Shiny's lingering input state, the admitted staleness hole).
  # Each unbound post updates ONE dimension; the others come from the
  # draft. Committed only while param and value are both set and DISTINCT,
  # else the key is absent — the engine's own completeness rule.
  tr_draft <- shiny::reactiveVal(list())
  shiny::observeEvent(
    store$rv$selected,
    {
      obj <- lst_obj()
      tr_draft(if (is.null(obj)) list() else obj@options$transpose %||% list())
    },
    ignoreNULL = FALSE
  )
  tr_field <- function(field, raw) {
    obj <- lst_obj()
    if (is.null(obj)) {
      return()
    }
    d <- tr_draft()
    d[[field]] <- as.character(raw %||% "")
    tr_draft(d)
    p <- as.character(d$param %||% "")
    v <- as.character(d$value %||% "")
    a <- as.character(d$agg %||% "first")
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
  shiny::observeEvent(input$tr_param, tr_field("param", input$tr_param))
  shiny::observeEvent(input$tr_value, tr_field("value", input$tr_value))
  shiny::observeEvent(input$tr_agg, tr_field("agg", input$tr_agg))

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
    # An entry with no vars is skipped by the engine — it exists
    # transiently while the user builds the line. No glue is seeded
    # User call: prefix/suffix stay empty until set.
    entries[[length(entries) + 1L]] <- list(vars = character(0))
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

  # ---- date formats ----
  # A plain text commit (never redraws); an emptied field removes the
  # format key, and an emptied spec entry removes the column entirely.
  shiny::observeEvent(input$fmt_set, {
    obj <- lst_obj()
    nm <- as.character(input$fmt_set$name %||% "")
    if (is.null(obj) || !nzchar(nm)) {
      return()
    }
    specs <- .listing_opt_list(obj, "column_specs")
    val <- trimws(as.character(input$fmt_set$value %||% ""))
    if (nzchar(val)) {
      specs[[nm]]$format <- val
    } else {
      specs[[nm]]$format <- NULL
      if (!length(specs[[nm]])) {
        specs[[nm]] <- NULL
      }
    }
    .commit_listing_key(store, obj, "column_specs", specs, "set date format")
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
    # An emptied glue field falls back to the engine default — keep the
    # committed entry minimal (no empty-string keys).
    entries[[j]][[field]] <- if (nzchar(val)) val else NULL
    st[[i]]$entries <- entries
    .commit_listing_key(store, obj, "stacks", st, "edit stack line glue")
  })

  invisible(NULL)
}
