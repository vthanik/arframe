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
  # role-region (columns/rows/axes) narrows the set; ANY other region token
  # ("title", "footnotes", a future one) falls back to the full set -- a
  # stale non-roles region must never filter the pane down to nothing (the
  # empty-Roles regression).
  if (is.null(region) || length(region) != 1L) {
    return(slots)
  }
  ids <- vapply(slots, `[[`, "", "slot")
  keep <- switch(
    region,
    columns = ids %in% "treatment",
    # `by` is a row-grouping dimension (nested stub bands), so it belongs with
    # the rows region alongside summarize/hierarchy. A listing's table content
    # is its `id` + `columns` slots -- same region.
    rows = ids %in% c("summarize", "hierarchy", "by", "id", "columns"),
    axes = !ids %in%
      c("treatment", "summarize", "hierarchy", "by", "id", "columns"),
    rep(TRUE, length(slots))
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

#' The dataset's column metadata (`data_items()`), memoized in the store
#' cache PER CATALOG GENERATION -- the `catalog_nonce` in the key means an
#' Add-output/import/delete gets a fresh column list on its next render,
#' while the many readers of one render cycle (source row, every slot's
#' picker, every assigned row's label line) share one `DESCRIBE`. Returns
#' an empty frame (never NULL) when the dataset is unreachable, so every
#' consumer degrades to "no metadata" without its own guard.
#' @noRd
.items_meta <- function(store, dataset) {
  key <- paste0("items::", dataset, "::", store$rv$catalog_nonce)
  if (!exists(key, envir = store$cache)) {
    items <- tryCatch(
      arpillar::data_items(store$con, dataset),
      error = function(e) NULL
    )
    if (is.null(items)) {
      items <- data.frame(
        name = character(0),
        type = character(0),
        sql_type = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
    }
    assign(key, items, envir = store$cache)
  }
  get(key, envir = store$cache)
}

#' One column's metadata row from `.items_meta()`, or `NULL` when absent.
#' @noRd
.item_meta_row <- function(items, name) {
  i <- match(name, items$name)
  if (is.na(i)) {
    return(NULL)
  }
  items[i, , drop = FALSE]
}

#' `items` (a `.items_meta()` frame) filtered to what `slot$accepts`
#' allows, minus names already assigned to `slot` on `object` -- the "only
#' eligible variables" contract (design spec #8: "Treatment arms won't
#' offer a numeric").
#' @noRd
.eligible_items <- function(items, slot, assigned_names) {
  items <- items[items$type %in% slot$accepts, , drop = FALSE]
  items[!items$name %in% assigned_names, , drop = FALSE]
}

# ---- picker ---------------------------------------------------------------

#' Pack one eligible-variable option as `"NAME\x1fTYPE\x1fLABEL"` -- `\x1f`
#' (unit separator) cannot appear in a column name, a DuckDB type string,
#' or a CDISC label, so splitting on it always recovers the fields cleanly;
#' packing everything into the selectize VALUE (not just the label) is what
#' lets `render` search match on the name, the type, OR the label (typing
#' "treatment" surfaces TRT01P by its label).
#' @noRd
.pack_item_choice <- function(name, sql_type, label = NA_character_) {
  lab <- if (length(label) == 1L && !is.na(label)) label else ""
  paste0(name, "\x1f", sql_type, "\x1f", lab)
}

#' The variable name half of a packed `"NAME\x1fTYPE"` choice value.
#' @noRd
.unpack_item_name <- function(choice) {
  strsplit(choice, "\x1f", fixed = TRUE)[[1]][[1]]
}

#' The single-pick variable picker: a raw `<select data-ar-picker>` the JS
#' bridge upgrades to a Tom Select showing the shared type-chip + name +
#' muted-label line. Choices are packed `"NAME\x1fTYPE\x1fLABEL"`
#' (`.pack_item_choice()`) so free-text search matches the type and label too;
#' the value stays packed so the server recovers the name via
#' `.unpack_item_name()`. `mode = "add"` (Roles slot, subject-id) fires the
#' pick and clears; `mode = "bind"` (Filters) posts on change and re-seeds a
#' committed row via `selected`. The render lives once in srcjs/bridge.js.
#' @noRd
.eligible_picker <- function(
  ns,
  input_id,
  items,
  selected = character(0),
  placeholder = "Add a variable",
  bare_value = FALSE
) {
  # Each choice's LABEL is the packed `name\x1ftype\x1flabel` string the shared
  # bundle render splits into chip + name + muted label. The VALUE is the same
  # packed string by default (so the server recovers the name via
  # `.unpack_item_name`), or the bare column name when `bare_value = TRUE`
  # (Treatment reads `input$treatment_trtvar` as a plain column name). Empty
  # `selected` => the picker force-clears on init and simply ADDS (Roles slot,
  # subject-id, re-rendered empty after each pick); a non-empty `selected`
  # re-seeds a committed control (Filters, Treatment).
  packed <- vapply(
    seq_len(nrow(items)),
    function(i) {
      .pack_item_choice(
        items$name[[i]],
        items$type[[i]],
        items$label[[i]]
      )
    },
    character(1)
  )
  values <- if (isTRUE(bare_value)) items$name else packed
  .ar_picker_select(
    ns = ns,
    input_id = input_id,
    choices = stats::setNames(values, packed),
    selected = selected,
    placeholder = placeholder
  )
}

# ---- assigned-item rows ---------------------------------------------------

#' One assigned-item row: a flex action line (grip when the slot is
#' multi-item, the role-type chip, the variable name over its CDISC label,
#' the peek toggle, remove) with the variable-peek panel expanded BELOW the
#' line when this item is in `store$rv$peek`. The panel lives INSIDE the
#' row element so a Sortable drag moves the whole unit.
#'
#' Every per-item control is DYNAMIC (any dataset column name), so each
#' posts through ONE shared input (`remove`/`peek`) via an inline onclick
#' -- `Shiny.setInputValue({slot, name, nonce})` -- exactly
#' `mod_contents.R`'s `.toc_kebab()` pattern, rather than per-item
#' `observeEvent`s that would need re-registering on every render.
#' @noRd
.assigned_row <- function(store, ns, object, slot, item, multi, meta) {
  slot_id <- slot$slot
  open <- item@name %in% store$rv$peek
  remove_js <- sprintf(
    "Shiny.setInputValue('%s', {slot: '%s', name: '%s', nonce: Date.now()}, {priority: 'event'})",
    ns("remove"),
    slot_id,
    item@name
  )
  peek_js <- sprintf(
    "Shiny.setInputValue('%s', {name: '%s', nonce: Date.now()}, {priority: 'event'})",
    ns("peek"),
    item@name
  )
  engine_label <- if (!is.null(meta) && !is.na(meta$label)) meta$label else NULL
  sub <- if (nzchar(item@label %||% "")) item@label else engine_label
  shiny::tags$div(
    class = paste0("ar-role-row", if (open) " ar-role-row-open"),
    `data-ar-item` = item@name,
    shiny::tags$div(
      class = "ar-role-line",
      if (multi) shiny::tags$span(class = "ar-role-grip", .icon("grip", 11)),
      .type_chip(item@role_type),
      shiny::tags$div(
        class = "ar-role-id",
        shiny::tags$span(class = "ar-role-name ar-mono", item@name),
        if (!is.null(sub)) shiny::tags$span(class = "ar-role-sub", sub)
      ),
      shiny::tags$button(
        type = "button",
        class = paste0(
          "ar-icon-btn ar-role-peek-btn",
          if (open) " ar-role-peek-on"
        ),
        `aria-label` = paste0(
          if (open) "Hide details for " else "Show details for ",
          item@name
        ),
        `aria-expanded` = if (open) "true" else "false",
        onclick = peek_js,
        .icon("eye", 11)
      ),
      shiny::tags$button(
        type = "button",
        class = "ar-icon-btn ar-role-remove",
        `aria-label` = paste0("Remove ", item@name, " from ", slot_id),
        onclick = remove_js,
        .icon("close", 11)
      )
    ),
    if (open) .peek_panel(store, ns, object, slot, item, meta)
  )
}

# ---- the variable peek ------------------------------------------------------

#' The engine facts behind one peek panel, memoized per catalog generation:
#' a measure column gets its min/median/max landmarks + observed decimal
#' precision; anything else gets its per-value counts. All pushed-down
#' aggregates (`column_range`/`column_precision`/`value_counts`) -- the
#' column itself never leaves DuckDB. A failure yields an UNCACHED
#' `kind = "error"` record (the next render retries -- a transient
#' catalog error must never poison the whole generation) and logs the
#' condition so the field failure is diagnosable from the console strip.
#' @noRd
.peek_facts <- function(store, dataset, column, meta) {
  key <- paste0("peek::", dataset, "::", column, "::", store$rv$catalog_nonce)
  if (exists(key, envir = store$cache)) {
    return(get(key, envir = store$cache))
  }
  numeric_col <- !is.null(meta) && identical(meta$type, "measure")
  facts <- tryCatch(
    {
      if (numeric_col) {
        list(
          kind = "range",
          range = arpillar::column_range(store$con, dataset, column),
          precision = arpillar::column_precision(store$con, dataset, column)
        )
      } else {
        list(
          kind = "counts",
          counts = arpillar::value_counts(store$con, dataset, column)
        )
      }
    },
    error = function(e) {
      log_line(
        store,
        sprintf(
          "peek failed: %s$%s (%s)",
          dataset,
          column,
          conditionMessage(e)
        )
      )
      list(kind = "error", message = conditionMessage(e))
    }
  )
  if (!identical(facts$kind, "error")) {
    assign(key, facts, envir = store$cache)
  }
  facts
}

# The number of distribution bars a category peek shows before folding the
# tail into "+ n more values".
.PEEK_BARS <- 6L

#' The peek panel body: the display-label editor (a cheap commit -- labels
#' are display-only), the treat-as toggle when the slot accepts more than
#' one role type AND the column is numeric (text can only ever be a
#' category), and the live distribution -- value-count bars for a category,
#' min/median/max + precision for a measure.
#' @noRd
.peek_panel <- function(store, ns, object, slot, item, meta) {
  relabel_js <- sprintf(
    "Shiny.setInputValue('%s', {slot: '%s', name: '%s', value: this.value, nonce: Date.now()}, {priority: 'event'})",
    ns("relabel"),
    slot$slot,
    item@name
  )
  can_retype <- length(slot$accepts) > 1L &&
    !is.null(meta) &&
    identical(meta$type, "measure")
  facts <- .peek_facts(store, object@dataset, item@name, meta)
  shiny::tags$div(
    class = "ar-role-peek",
    shiny::tags$div(
      class = "ar-peek-row",
      shiny::tags$span(class = "ar-peek-key", "Label"),
      shiny::tags$input(
        type = "text",
        class = "ar-peek-label",
        value = item@label,
        placeholder = "Display label (defaults to the name)",
        onchange = relabel_js,
        `aria-label` = paste0("Display label for ", item@name)
      )
    ),
    if (can_retype) .retype_control(ns, slot$slot, item),
    .peek_distribution(facts),
    # A measure gets a per-variable decimal-places knob (the "variable level"
    # of the study -> output -> variable precision cascade); a category has no
    # numeric precision.
    if (!is.null(meta) && identical(meta$type, "measure")) {
      .peek_dp_control(
        ns,
        slot$slot,
        item,
        .var_dp(store$rv$report@theme, item@name)
      )
    },
    if (!is.null(facts) && identical(facts$kind, "counts")) {
      .levels_editor(ns, slot$slot, item, facts)
    }
  )
}

# The levels editor renders up to this many rows; a higher-cardinality
# category (SITEID, USUBJID-ish) keeps the distribution peek only.
# ponytail: flat cap, no paging -- revisit if a real table needs more.
.LEVELS_EDITOR_CAP <- 24L

# The reserved `value` for the synthetic "Missing" level: a per-variable
# override of the study `show_missing` rule, edited in the LEVELS pane as one
# more row with an include checkbox. Kept in sync with arpillar's
# `.MISSING_LEVEL_VALUE` (the engine reads it off `data_item@levels`).
.MISSING_LEVEL_VALUE <- "__ARF_MISSING__"

#' The per-variable LEVELS editor inside a category peek: drag order,
#' include checkbox, DISPLAY AS recode, and "Add expected level" for
#' dummy levels the data never observes (they render as zero rows). All
#' edits are CHEAP (display-only) -- they re-render live off the cached
#' ARD, never marking the proof stale.
#' @noRd
.levels_editor <- function(ns, slot_id, item, facts) {
  observed <- names(facts$counts %||% integer(0))
  meta <- item@levels
  # Split the synthetic Missing override out of the ordinary level metadata:
  # it is NOT a data level, so it never joins the sortable list -- it renders
  # as its own row below, reusing the include checkbox to force the Missing row
  # ON/OFF for this variable (absent = the study `show_missing` rule).
  is_missing <- vapply(
    meta,
    function(m) identical(as.character(m$value), .MISSING_LEVEL_VALUE),
    logical(1)
  )
  miss_meta <- if (any(is_missing)) meta[[which(is_missing)[[1L]]]] else NULL
  meta <- meta[!is_missing]
  declared <- vapply(meta, function(m) as.character(m$value), character(1))
  if (length(observed) > .LEVELS_EDITOR_CAP) {
    return(shiny::tags$p(
      class = "ar-peek-none ar-mono",
      sprintf("%d levels - too many to edit here.", length(observed))
    ))
  }
  all_vals <- c(declared, setdiff(observed, declared))
  if (length(all_vals) == 0L) {
    return(NULL)
  }
  post <- function(input, value, extra = "") {
    sprintf(
      "Shiny.setInputValue('%s', {slot: '%s', name: '%s', value: '%s'%s, nonce: Date.now()}, {priority: 'event'})",
      ns(input),
      slot_id,
      item@name,
      value,
      extra
    )
  }
  rows <- lapply(all_vals, function(v) {
    i <- match(v, declared)
    m <- if (is.na(i)) NULL else meta[[i]]
    included <- !isFALSE(m$include)
    unobserved <- !(v %in% observed)
    display <- as.character(m$display %||% "")
    shiny::tags$div(
      class = "ar-level-row",
      `data-ar-item` = v,
      shiny::tags$span(class = "ar-level-grip", .icon("grip", 11)),
      shiny::tags$input(
        type = "checkbox",
        class = "ar-level-include",
        checked = if (included) "checked",
        onchange = post("lvl_include", v, ", on: this.checked"),
        `aria-label` = paste0("Include level ", v)
      ),
      shiny::tags$span(
        class = paste0("ar-level-val ar-mono", if (unobserved) " ar-level-exp"),
        v
      ),
      shiny::tags$input(
        type = "text",
        class = "ar-level-display",
        value = display,
        placeholder = "(keep)",
        onchange = post("lvl_display", v, ", display: this.value"),
        `aria-label` = paste0("Display ", v, " as")
      ),
      if (unobserved) {
        shiny::tags$button(
          type = "button",
          class = "ar-icon-btn ar-level-rm",
          `aria-label` = paste0("Remove expected level ", v),
          onclick = post("lvl_rm", v),
          .icon("close", 10)
        )
      }
    )
  })
  add_js <- sprintf(
    "var inp = this.previousElementSibling; var v = inp.value.trim(); if (v) { Shiny.setInputValue('%s', {slot: '%s', name: '%s', value: v, nonce: Date.now()}, {priority: 'event'}); inp.value = ''; }",
    ns("lvl_add"),
    slot_id,
    item@name
  )
  # The synthetic Missing row: below the data levels, no grip/reorder and no
  # display-as (it is not a data level). Its include checkbox posts the reserved
  # sentinel through the SAME `lvl_include` observer -- no new server wiring.
  missing_row <- shiny::tags$div(
    class = "ar-level-row ar-level-missing",
    shiny::tags$span(class = "ar-level-grip"),
    shiny::tags$input(
      type = "checkbox",
      class = "ar-level-include",
      checked = if (isTRUE(miss_meta$include)) "checked",
      onchange = post(
        "lvl_include",
        .MISSING_LEVEL_VALUE,
        ", on: this.checked"
      ),
      `aria-label` = paste0("Show the Missing row for ", item@name)
    ),
    shiny::tags$span(class = "ar-level-val", "Missing"),
    shiny::tags$span(class = "ar-level-hint ar-mono", "show empty / NA")
  )
  shiny::tags$div(
    class = "ar-levels-editor",
    shiny::tags$span(class = "ar-label ar-levels-label", "Levels"),
    shiny::tags$div(
      class = "ar-levels-list",
      `data-ar-sortable` = "true",
      `data-ar-sortable-handle` = ".ar-level-grip",
      `data-ar-sortable-item` = ".ar-level-row",
      `data-ar-sortable-attr` = "data-ar-item",
      `data-ar-sortable-input` = ns("lvl_reorder"),
      # ponytail: hand-rolled JSON -- slot ids and CDISC column names are
      # [A-Za-z0-9_.], no escaping needed; avoids a jsonlite Import.
      `data-ar-sortable-extra` = sprintf(
        '{"slot":"%s","name":"%s"}',
        slot_id,
        item@name
      ),
      rows
    ),
    missing_row,
    shiny::tags$div(
      class = "ar-levels-add",
      shiny::tags$input(
        type = "text",
        class = "ar-level-display",
        placeholder = "Add expected level\u2026",
        `aria-label` = paste0("Add expected level to ", item@name)
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-link ar-fn-add",
        onclick = add_js,
        "+ Add"
      )
    )
  )
}

#' The treat-as segmented toggle: a numeric column can be summarized as a
#' measure (stats) or grouped as a category (counts); flipping it is a
#' HEAVY edit (the ARD changes), so the existing stale/Run semantics apply
#' untouched.
#' @noRd
.retype_control <- function(ns, slot_id, item) {
  btn <- function(rt) {
    active <- identical(item@role_type, rt)
    js <- sprintf(
      "Shiny.setInputValue('%s', {slot: '%s', name: '%s', role_type: '%s', nonce: Date.now()}, {priority: 'event'})",
      ns("retype"),
      slot_id,
      item@name,
      rt
    )
    shiny::tags$button(
      type = "button",
      class = paste0("ar-peek-type-btn", if (active) " ar-peek-type-on"),
      `aria-pressed` = if (active) "true" else "false",
      onclick = js,
      rt
    )
  }
  shiny::tags$div(
    class = "ar-peek-row",
    shiny::tags$span(class = "ar-peek-key", "Treat as"),
    shiny::tags$div(
      class = "ar-peek-type",
      role = "group",
      `aria-label` = paste0("Treat ", item@name, " as"),
      btn("measure"),
      btn("category")
    )
  )
}

#' The per-variable decimal-places control on a measure peek. Writes THIS
#' variable's raw precision into the study Decimals-by register
#' (`theme$decimals_by`, `V|<name>`) -- the same knob Setup > Summaries
#' exposes, given top precedence by the engine's `.stat_dp()` over the
#' study/output base (the "variable level" of the precision cascade). Blank =
#' inherit. Study-wide by variable (not per-output; that needs engine work).
#' @noRd
.peek_dp_control <- function(ns, slot_id, item, cur_dp) {
  js <- sprintf(
    "Shiny.setInputValue('%s', {slot: '%s', name: '%s', value: this.value, nonce: Date.now()}, {priority: 'event'})",
    ns("dp_change"),
    slot_id,
    item@name
  )
  shiny::tags$div(
    class = "ar-peek-row",
    shiny::tags$span(class = "ar-peek-key", "Decimal"),
    shiny::tags$input(
      type = "number",
      class = "ar-peek-dp",
      min = "0",
      max = "6",
      step = "1",
      value = if (length(cur_dp) == 1L && !is.na(cur_dp)) {
        as.character(cur_dp)
      } else {
        ""
      },
      placeholder = "inherit",
      onchange = js,
      `aria-label` = paste0("Decimal places for ", item@name)
    )
  )
}

#' The raw decimal-places THIS variable currently carries in the study
#' Decimals-by register (`theme$decimals_by`, `V|`/`P|` keys), or `NA` when it
#' inherits the base. Mirrors the engine's `.flatten_decimals_by()` read.
#' @noRd
.var_dp <- function(theme, name) {
  rules <- theme$decimals_by %||% list()
  keys <- c(paste0("V|", name), paste0("P|", name))
  for (r in rules) {
    if (any(keys %in% as.character(r$names %||% character(0)))) {
      dp <- suppressWarnings(as.integer(r$dp))
      if (length(dp) == 1L && !is.na(dp)) {
        return(dp)
      }
    }
  }
  NA_integer_
}

#' Upsert THIS variable's raw decimals in the Decimals-by register: drop the
#' `V|<name>` key from every existing rule, then (unless `dp` is `NA`) append a
#' single-name rule for it -- so editing one variable never disturbs the dp of
#' others it may have shared a rule with. Returns the new rule list.
#' @noRd
.set_var_dp <- function(rules, name, dp) {
  key <- paste0("V|", name)
  rules <- lapply(rules, function(r) {
    r$names <- setdiff(as.character(r$names %||% character(0)), key)
    r
  })
  rules <- Filter(function(r) length(r$names) > 0L, rules)
  if (length(dp) == 1L && !is.na(dp)) {
    rules <- c(rules, list(list(names = key, dp = dp)))
  }
  rules
}

#' Render the distribution half of a peek panel from `.peek_facts()`.
#' @noRd
.peek_distribution <- function(facts) {
  if (is.null(facts) || identical(facts$kind, "error")) {
    return(htmltools::tagList(
      shiny::tags$p(
        class = "ar-peek-none ar-mono",
        "Distribution unavailable."
      ),
      if (!is.null(facts$message)) {
        shiny::tags$p(class = "ar-peek-why ar-mono", facts$message)
      }
    ))
  }
  if (identical(facts$kind, "range")) {
    r <- facts$range
    line <- if (anyNA(unlist(r))) {
      "all values missing"
    } else {
      sprintf(
        "min %s \u00b7 median %s \u00b7 max %s",
        format(r$min, trim = TRUE),
        format(r$median, trim = TRUE),
        format(r$max, trim = TRUE)
      )
    }
    return(shiny::tags$div(
      class = "ar-peek-facts ar-mono",
      shiny::tags$div(class = "ar-peek-range", line),
      shiny::tags$div(
        class = "ar-peek-precision",
        sprintf("observed precision: %d dp", facts$precision)
      )
    ))
  }
  counts <- facts$counts
  if (length(counts) == 0L) {
    return(shiny::tags$p(class = "ar-peek-none ar-mono", "No values."))
  }
  top <- utils::head(counts, .PEEK_BARS)
  peak <- max(top)
  bars <- lapply(seq_along(top), function(i) {
    pct <- round(100 * top[[i]] / peak)
    shiny::tags$div(
      class = "ar-peek-bar-row",
      shiny::tags$span(class = "ar-peek-val", names(top)[[i]]),
      shiny::tags$div(
        class = "ar-peek-bar",
        shiny::tags$div(
          class = "ar-peek-bar-fill",
          style = paste0("width:", max(pct, 2), "%;")
        )
      ),
      shiny::tags$span(
        class = "ar-peek-n ar-mono",
        format(top[[i]], big.mark = ",")
      )
    )
  })
  more <- length(counts) - length(top)
  shiny::tags$div(
    class = "ar-peek-facts",
    bars,
    if (more > 0L) {
      shiny::tags$div(
        class = "ar-peek-more ar-mono",
        sprintf("+ %d more value%s", more, if (more == 1L) "" else "s")
      )
    }
  )
}

# ---- one slot's fieldset --------------------------------------------------

#' The legend's cardinality hint, straight from the generator's own
#' `min`/`max` slot contract -- the engine's requirement, not a UI guess.
#' @noRd
.cardinality_hint <- function(slot) {
  if (slot$max == 1L) {
    return(if (slot$min >= 1L) "required" else "optional")
  }
  if (is.infinite(slot$max)) {
    return(if (slot$min >= 1L) "1 or more" else "any number")
  }
  sprintf("%d\u2013%d", slot$min, slot$max)
}

#' One slot's whole fieldset: legend (the slot label + the cardinality
#' hint), assigned rows (sortable when `max > 1`, each with its peek
#' panel), the "+ Add variable" picker row, and an inline
#' `validate_output` message when this slot's control_id is among the
#' object's unmet requirements (message text IDENTICAL to the oracle's own
#' -- never reworded, so the ghost hint, the error summary, and this
#' inline message always agree).
#' @noRd
.slot_fieldset <- function(store, ns, object, slot, problems, items_meta) {
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
    shiny::tags$legend(
      class = "ar-label",
      slot$label,
      shiny::tags$span(class = "ar-role-card-hint", .cardinality_hint(slot))
    ),
    # The generator's own slot hint (arpillar `.SLOT(hint=)`) was shown here
    # as inline gray text; the Roles help topic (the section `?` icon, wired
    # with the accordion in the follow-up task) now carries the slot guidance.
    do.call(
      shiny::tags$div,
      c(
        list(class = "ar-role-assigned"),
        sortable_attrs,
        lapply(items, function(it) {
          .assigned_row(
            store,
            ns,
            object,
            slot,
            it,
            multi,
            .item_meta_row(items_meta, it@name)
          )
        })
      )
    ),
    .eligible_picker(
      ns,
      paste0("add_", slot$slot),
      .eligible_items(items_meta, slot, assigned_names)
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

# ---- pane header: source row + orphan problems -----------------------------

#' The dataset facts behind the SOURCE row, memoized per catalog
#' generation: the ADaM structure heuristic + the catalog's row/col counts.
#' `NULL` when the catalog is unreachable (the row simply shows the name).
#' @noRd
.source_facts <- function(store, dataset) {
  key <- paste0("structure::", dataset, "::", store$rv$catalog_nonce)
  if (!exists(key, envir = store$cache)) {
    facts <- tryCatch(
      {
        grid <- arpillar::catalog_grid(store$con)
        row <- grid[grid$name == dataset, , drop = FALSE]
        list(
          structure = arpillar::detect_structure(store$con, dataset),
          rows = if (nrow(row) == 1L) row$rows[[1L]] else NA,
          cols = if (nrow(row) == 1L) row$cols[[1L]] else NA
        )
      },
      error = function(e) NULL
    )
    assign(key, facts, envir = store$cache)
  }
  get(key, envir = store$cache)
}

#' The SOURCE row at the top of the Roles pane: the dataset this output
#' reads, its detected ADaM structure, and its dimensions -- the provenance
#' the roles below are editing against. Read-only here; the dataset is
#' chosen at Add-output time.
#' @noRd
.roles_source_row <- function(store, object) {
  facts <- .source_facts(store, object@dataset)
  shiny::tags$div(
    class = "ar-role-src",
    shiny::tags$span(class = "ar-label", "Source"),
    shiny::tags$span(
      class = "ar-role-src-ds ar-mono",
      toupper(object@dataset)
    ),
    if (!is.null(facts)) {
      shiny::tags$span(class = "ar-role-src-kind", facts$structure)
    },
    if (!is.null(facts) && !is.na(facts$rows) && !is.na(facts$cols)) {
      shiny::tags$span(
        class = "ar-role-src-dims ar-mono",
        sprintf(
          "%s \u00d7 %s",
          format(facts$rows, big.mark = ","),
          format(facts$cols, big.mark = ",")
        )
      )
    }
  )
}

#' `validate_output()` messages NOT owned by any rendered slot fieldset
#' (dataset-level problems, a future non-roles control) -- the checklist
#' strip shows these, the slot-owned ones render inline in their own
#' fieldsets, and nothing is ever double-reported.
#' @noRd
.orphan_problems <- function(object, slots) {
  v <- arpillar::validate_output(object)
  if (nrow(v) == 0L) {
    return(character(0))
  }
  ids <- vapply(slots, `[[`, "", "slot")
  owned <- sub("^roles-", "", v$control_id) %in% ids
  v$message[!owned]
}

#' The problems strip: one warn-tagged line per orphan oracle message.
#' `NULL` when the output is clean (no empty congratulation box).
#' @noRd
.roles_problem_strip <- function(problems) {
  if (length(problems) == 0L) {
    return(NULL)
  }
  shiny::tags$div(
    class = "ar-role-checklist",
    lapply(problems, function(msg) {
      shiny::tags$p(
        class = "ar-role-problem ar-mono",
        .icon("warn", 11),
        shiny::span(msg)
      )
    })
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

#' A stable digest of `object@roles` -- the assigned slot/item set with
#' each item's `label` and `role_type`, order-sensitive (so a reorder, a
#' relabel, or a treat-as flip all invalidate the render). Deliberately
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
      items = lapply(r@items, function(it) {
        list(
          name = it@name,
          label = it@label,
          role_type = it@role_type,
          # Level edits (order/include/display-as/expected) must repaint
          # the pane; they stay OUT of `.ard_key()` (display-only).
          levels = it@levels
        )
      })
    )
  }))
}

#' Apply `fn` to the single item named `item_name` inside the
#' alias-matched role for `slot` -- the shared walk behind relabel and
#' retype, mirroring `.remove_item_from_slot()`'s own discipline. A no-op
#' when no such role/item exists.
#' @noRd
.update_item <- function(object, slot, item_name, fn) {
  aliases <- .slot_aliases(slot)
  roles <- object@roles
  for (i in seq_along(roles)) {
    if (roles[[i]]@slot %in% aliases) {
      items <- roles[[i]]@items
      for (j in seq_along(items)) {
        if (identical(items[[j]]@name, item_name)) {
          items[[j]] <- fn(items[[j]])
          roles[[i]] <- S7::set_props(roles[[i]], items = items)
          return(S7::set_props(object, roles = roles))
        }
      }
      return(object)
    }
  }
  object
}

#' Set an item's display label (CHEAP: labels are display-only, excluded
#' from the ARD key -- the proof re-renders live).
#' @noRd
.relabel_item <- function(object, slot, item_name, label) {
  .update_item(object, slot, item_name, function(it) {
    S7::set_props(it, label = label)
  })
}

#' Set an item's role_type (HEAVY: the ARD changes; `update_object()`'s
#' stale semantics fire untouched).
#' @noRd
.retype_item <- function(object, slot, item_name, role_type) {
  .update_item(object, slot, item_name, function(it) {
    S7::set_props(it, role_type = role_type)
  })
}

# ---- level metadata edits (all CHEAP: display-only) ------------------------

#' Ensure a `@levels` entry for `value` exists, then apply `fn` to it.
#' Entries stay once created (no trivial-entry elision -- an all-default
#' entry only pins a position, which is itself information).
#' @noRd
.set_level_meta <- function(levels, value, fn) {
  vals <- vapply(levels, function(m) as.character(m$value), character(1))
  i <- match(value, vals)
  if (is.na(i)) {
    levels[[length(levels) + 1L]] <- fn(list(value = value))
  } else {
    levels[[i]] <- fn(levels[[i]])
  }
  levels
}

#' Rebuild `@levels` in the dragged order, preserving each entry's fields;
#' a value new to the metadata gets a bare entry (its position is now
#' declared). Declared values missing from a stale/partial payload keep
#' their entries, appended -- the `.reorder_slot()` reconcile discipline.
#' @noRd
.reorder_level_meta <- function(levels, order) {
  vals <- vapply(levels, function(m) as.character(m$value), character(1))
  out <- lapply(order, function(v) {
    i <- match(v, vals)
    if (is.na(i)) list(value = v) else levels[[i]]
  })
  c(out, levels[!vals %in% order])
}

#' Apply a `@levels` transform to one item -- every lvl_* observer's
#' shared commit path.
#' @noRd
.edit_item_levels <- function(store, object, slot, item_name, label, fn) {
  update_object(
    store,
    object@id,
    function(o) {
      .update_item(o, slot, item_name, function(it) {
        S7::set_props(it, levels = fn(it@levels))
      })
    },
    label = label
  )
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

    # Shared help observer for the Roles pane `?` icon (wired with the
    # accordion header in the follow-up task; the topic already exists here).
    shiny::observeEvent(input$help_open, {
      .show_help(input$help_open$topic)
    })

    output$slots <- shiny::renderUI({
      obj <- selected_object(store)
      if (is.null(obj)) {
        return(shiny::tags$p(
          class = "ar-insp-directive",
          "No output selected \u2014 pick one from the report contents."
        ))
      }
      gen <- tryCatch(arpillar::generator(obj@type), error = function(e) NULL)
      if (is.null(gen)) {
        return(NULL)
      }
      slots <- .region_slots(store$rv$region, gen$slots)
      if (length(slots) == 0L) {
        # A region narrowing that matches nothing on THIS generator (a stale
        # "axes" from a figure carried onto a table) shows the full editor,
        # never an empty pane.
        slots <- gen$slots
      }
      if (length(slots) == 0L) {
        return(NULL)
      }
      problems <- .slot_problems(obj, slots)
      items_meta <- .items_meta(store, obj@dataset)
      shiny::tagList(
        .roles_problem_strip(.orphan_problems(obj, slots)),
        .roles_source_row(store, obj),
        lapply(slots, function(s) {
          .slot_fieldset(store, ns, obj, s, problems, items_meta)
        })
      )
    }) |>
      shiny::bindEvent(
        store$rv$selected,
        store$rv$region,
        store$rv$peek,
        store$rv$catalog_nonce,
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
            meta <- .item_meta_row(.items_meta(store, obj@dataset), name)
            role_type <- if (!is.null(meta)) meta$type else "category"
            label <- if (!is.null(meta) && !is.na(meta$label)) {
              meta$label
            } else {
              ""
            }
            item <- arpillar::data_item(
              name = name,
              label = label,
              role_type = role_type
            )
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

    # Peek toggle: membership in `rv$peek` is the ONLY open/closed state
    # (never the DOM), so a digest redraw or a Sortable re-init restores
    # every open panel exactly.
    shiny::observeEvent(input$peek, {
      nm <- input$peek$name
      cur <- store$rv$peek
      store$rv$peek <- if (nm %in% cur) setdiff(cur, nm) else c(cur, nm)
    })

    # Display-label edit (cheap: label is display-only, so the proof
    # re-renders live off the memoized ARD).
    shiny::observeEvent(input$relabel, {
      obj_id <- store$rv$selected
      if (is.null(obj_id)) {
        return()
      }
      req <- input$relabel
      update_object(
        store,
        obj_id,
        function(o) .relabel_item(o, req$slot, req$name, req$value %||% ""),
        label = paste0("relabel ", req$name)
      )
    })

    # Treat-as flip (heavy: the ARD changes; update_object marks the proof
    # stale through the existing oracle, Run re-typesets).
    shiny::observeEvent(input$retype, {
      obj_id <- store$rv$selected
      if (is.null(obj_id)) {
        return()
      }
      req <- input$retype
      if (!req$role_type %in% c("measure", "category")) {
        return()
      }
      update_object(
        store,
        obj_id,
        function(o) .retype_item(o, req$slot, req$name, req$role_type),
        label = paste0("treat ", req$name, " as ", req$role_type)
      )
    })

    # Per-variable decimals (CHEAP: display-only; the engine applies precision
    # at render, so the paper re-typesets live off the theme change, no Run).
    # Writes the study Decimals-by register (`theme$decimals_by`, `V|<name>`),
    # the same knob Setup > Summaries exposes -- study-wide by variable.
    shiny::observeEvent(input$dp_change, {
      req <- input$dp_change
      name <- req$name
      if (is.null(name) || !nzchar(name)) {
        return()
      }
      raw <- trimws(as.character(req$value %||% ""))
      dp <- if (!nzchar(raw)) {
        NA_integer_
      } else {
        suppressWarnings(as.integer(raw))
      }
      # Reject non-integer / negative input (leave the register untouched).
      if (nzchar(raw) && (is.na(dp) || dp < 0L)) {
        return()
      }
      r <- store$rv$report
      theme <- r@theme
      rules <- theme$decimals_by %||% list()
      new_rules <- .set_var_dp(rules, name, dp)
      if (identical(new_rules, rules)) {
        return()
      }
      theme$decimals_by <- new_rules
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = paste0("decimals ", name)
      )
    })

    # ---- level metadata edits (all CHEAP: display-only) ----
    # Shared commit: look the selected object up, apply the @levels
    # transform through .edit_item_levels(); the pane repaints via the
    # roles digest (levels are part of it).
    .lvl_commit <- function(req, label, fn) {
      obj <- selected_object(store)
      if (is.null(obj) || is.null(req$slot) || is.null(req$name)) {
        return()
      }
      .edit_item_levels(store, obj, req$slot, req$name, label, fn)
    }

    shiny::observeEvent(input$lvl_include, {
      req <- input$lvl_include
      v <- as.character(req$value)
      on <- isTRUE(req$on)
      .lvl_commit(req, paste0("toggle level ", v), function(lv) {
        .set_level_meta(lv, v, function(m) {
          m$include <- on
          m
        })
      })
    })

    shiny::observeEvent(input$lvl_display, {
      req <- input$lvl_display
      v <- as.character(req$value)
      dsp <- trimws(as.character(req$display %||% ""))
      .lvl_commit(req, paste0("relabel level ", v), function(lv) {
        .set_level_meta(lv, v, function(m) {
          m$display <- if (nzchar(dsp)) dsp else NULL
          m
        })
      })
    })

    shiny::observeEvent(input$lvl_add, {
      req <- input$lvl_add
      v <- trimws(as.character(req$value))
      if (!nzchar(v)) {
        return()
      }
      .lvl_commit(req, paste0("add expected level ", v), function(lv) {
        .set_level_meta(lv, v, function(m) {
          m$expected <- TRUE
          m
        })
      })
    })

    shiny::observeEvent(input$lvl_rm, {
      req <- input$lvl_rm
      v <- as.character(req$value)
      .lvl_commit(req, paste0("remove level ", v), function(lv) {
        vals <- vapply(lv, function(m) as.character(m$value), character(1))
        lv[vals != v]
      })
    })

    shiny::observeEvent(input$lvl_reorder, {
      req <- input$lvl_reorder
      order <- vapply(req$order, as.character, character(1))
      if (length(order) == 0L) {
        return()
      }
      .lvl_commit(req, "reorder levels", function(lv) {
        .reorder_level_meta(lv, order)
      })
    })

    # The panes are always mounted and CSS-toggled (never remounted), so the
    # slot editor must keep computing while hidden -- a suspended output
    # would show a STALE (or empty) editor after a pure class-flip tab
    # switch. Same contract as mod_card_options/mod_card_filters.
    shiny::outputOptions(output, "slots", suspendWhenHidden = FALSE)

    invisible(NULL)
  })
}
