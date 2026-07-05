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
  stats = "rows",
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

# Keys whose semantics are ORDERING, not display: they render in the Ranks
# pane (mod_card_ranks) so ordering lives in one place, and never here.
.RANKS_KEYS <- c("hier_sort", "x_order")

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

#' A blur/Enter-commit text input: a RAW <input> whose onchange posts the
#' namespaced option input, so typing never commits per keystroke (audit
#' note 2026-07-04) -- the value lands when the field loses focus or the
#' user presses Enter. Shiny's own textInput would post on every keyup.
#' @noRd
.opt_change_input <- function(
  ns,
  input_id,
  value,
  placeholder = NULL,
  width = NULL,
  numeric = FALSE
) {
  js <- sprintf(
    "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
    ns(input_id)
  )
  shiny::tags$input(
    type = "text",
    class = "form-control ar-opt-input",
    value = value,
    placeholder = placeholder,
    onchange = js,
    inputmode = if (numeric) "numeric",
    style = if (!is.null(width)) paste0("width:", width, ";")
  )
}

# Multi-line variant for header cells that legitimately span rows (the
# tabular renderer honors literal "\n" as a line break inside a stub /
# spanner header). Same blur/Enter commit contract as .opt_change_input.
.opt_change_textarea <- function(
  ns,
  input_id,
  value,
  placeholder = NULL,
  width = NULL,
  rows = 2
) {
  js <- sprintf(
    "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
    ns(input_id)
  )
  shiny::tags$textarea(
    id = ns(paste0(input_id, "_ta")),
    class = "form-control ar-opt-input",
    rows = rows,
    placeholder = placeholder,
    onchange = js,
    style = if (!is.null(width)) paste0("width:", width, ";"),
    value
  )
}

#' A text input carrying `inputmode="numeric"`#' A text input carrying `inputmode="numeric"` -- the plan's int control
#' (mobile numeric keyboard, no spinner chrome; blur/Enter commit).
#' @noRd
.opt_numeric_text <- function(ns, key, value) {
  .opt_change_input(
    ns,
    paste0("opt_", key),
    value,
    width = "56px",
    numeric = TRUE
  )
}

#' An int row's stepper: minus / numeric text / plus. The buttons post the
#' shared `opt_step` input ({key, dir}); the observer reads the CURRENT
#' committed value (never the DOM), steps it, and commits through the
#' standard option path -- typing in the text input still works unchanged.
#' @noRd
.opt_int_control <- function(ns, key, value) {
  step_js <- function(dir) {
    sprintf(
      "Shiny.setInputValue('%s', {key: '%s', dir: %d, nonce: Date.now()}, {priority: 'event'})",
      ns("opt_step"),
      key,
      dir
    )
  }
  shiny::tags$div(
    class = "ar-opt-stepper",
    shiny::tags$button(
      type = "button",
      class = "ar-icon-btn ar-opt-step",
      `aria-label` = paste0("Decrease ", key),
      onclick = step_js(-1L),
      "\u2212"
    ),
    .opt_numeric_text(ns, key, value),
    shiny::tags$button(
      type = "button",
      class = "ar-icon-btn ar-opt-step",
      `aria-label` = paste0("Increase ", key),
      onclick = step_js(1L),
      "+"
    )
  )
}

#' The statistics membership + order editor for the summary `stats` option:
#' one grip row per selected statistic (drag to reorder through the same
#' sortable contract; x posts the shared `opt_stat_rm`), plus an add-back
#' select over the deselected remainder. Every commit rides the standard
#' `.commit_opt()` path, so the full default set stays elided (goldens and
#' emitted code never carry it).
#' @noRd
.opt_stats_control <- function(ns, object, row) {
  all_stats <- as.character(row$choices[[1]])
  current <- intersect(
    as.character(object@options[[row$key]] %||% .opt_default(row)),
    all_stats
  )
  if (length(current) == 0L) {
    current <- all_stats
  }
  missing <- setdiff(all_stats, current)
  rows <- lapply(current, function(sv) {
    rm_js <- sprintf(
      "Shiny.setInputValue('%s', {value: '%s', nonce: Date.now()}, {priority: 'event'})",
      ns("opt_stat_rm"),
      sv
    )
    shiny::tags$div(
      class = "ar-opt-level ar-opt-stat",
      `data-ar-item` = sv,
      .icon("grip", 11),
      shiny::tags$span(class = "ar-opt-stat-lbl", sv),
      shiny::tags$button(
        type = "button",
        class = "ar-icon-btn ar-opt-stat-rm",
        `aria-label` = paste0("Remove statistic ", sv),
        onclick = rm_js,
        .icon("close", 10)
      )
    )
  })
  shiny::tags$div(
    class = "ar-opt-levels ar-opt-stats",
    `data-ar-sortable` = "true",
    `data-ar-sortable-item` = ".ar-opt-level",
    `data-ar-sortable-attr` = "data-ar-item",
    `data-ar-sortable-input` = ns("opt_reorder_stats"),
    rows,
    if (length(missing) > 0L) {
      shiny::selectInput(
        ns("opt_stat_add"),
        label = NULL,
        choices = stats::setNames(
          c("", missing),
          c("Add statistic\u2026", missing)
        ),
        selectize = FALSE
      )
    }
  )
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
    int = .opt_int_control(
      ns,
      key,
      if (is.null(current)) "" else as.character(current)
    ),
    numvec = .opt_change_input(
      ns,
      input_id,
      if (is.null(current)) "" else paste(current, collapse = ", ")
    ),
    text = .opt_change_input(ns, input_id, current %||% ""),
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
    levels = if (identical(key, "stats")) {
      .opt_stats_control(ns, object, row)
    } else {
      .opt_levels_control(con, ns, object, row)
    },
    NULL
  )
  if (is.null(control)) {
    return(NULL)
  }
  block <- identical(row$kind, "levels")
  row_tag <- shiny::tags$div(
    class = paste0("ar-opt-row", if (block) " ar-opt-row-block"),
    shiny::tags$span(class = "ar-opt-label", row$label),
    control
  )
  if (!identical(key, "decimals")) {
    return(row_tag)
  }
  # The derived-precision contract, spelled out where the knob lives: the
  # engine renders mean at d, SD at d+1, percentages always at 1 dp.
  d <- suppressWarnings(as.integer(current %||% 1L))
  if (length(d) != 1L || is.na(d)) {
    d <- 1L
  }
  shiny::tagList(
    row_tag,
    shiny::tags$p(
      class = "ar-opt-hint ar-mono",
      sprintf("mean %d dp \u00b7 SD %d dp \u00b7 %% always 1 dp", d, d + 1L)
    )
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
#' decision -- numbering is SAP-shell driven metadata, never re-derived),
#' the title line, and the Appendix-I continuation title lines
#' (`options$titles` -- "Title 2 .. Title X", centered under the main
#' title on paper).
#' @noRd
.opt_title_section <- function(ns, object) {
  labels <- c("Table", "Figure", "Listing")
  current_label <- object@options$number_label %||% ""
  # Continuation title lines are a TABLE-leg feature (arpillar's
  # .rtf_titles); a figure never shows the editor, so nothing silently
  # no-ops.
  is_table <- !.is_figure_type(object@type)
  extra <- if (is_table) {
    as.character(object@options$titles %||% character(0))
  } else {
    character(0)
  }
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
      ),
      lapply(seq_along(extra), function(i) {
        edit_js <- sprintf(
          "Shiny.setInputValue('%s', {i: %d, value: this.value, nonce: Date.now()}, {priority: 'event'})",
          ns("tl_edit"),
          i
        )
        remove_js <- sprintf(
          "Shiny.setInputValue('%s', {i: %d, nonce: Date.now()}, {priority: 'event'})",
          ns("tl_remove"),
          i
        )
        shiny::tags$div(
          class = "ar-fn-row ar-tl-row",
          shiny::tags$input(
            type = "text",
            class = "ar-fn-input",
            value = extra[[i]],
            placeholder = paste0("Title ", i + 1L),
            onchange = edit_js,
            `aria-label` = paste0("Title line ", i + 1L)
          ),
          shiny::tags$button(
            type = "button",
            class = "ar-icon-btn ar-fn-remove",
            `aria-label` = paste0("Remove title line ", i + 1L),
            onclick = remove_js,
            .icon("close", 11)
          )
        )
      }),
      if (is_table) {
        .action_btn(
          ns("tl_add"),
          shiny::tagList(.icon("plus", 11), "Add title line"),
          variant = "link",
          class = "ar-fn-add"
        )
      }
    )
  )
}

#' One footnote line: a drag grip (footnote order is part of the output), a
#' plain text input posting through the shared `fn_edit` input on change
#' (blur/Enter -- footnotes are sentences, not live-typed previews), and a
#' remove button posting `fn_remove`. Footnote 1 renders ONLY as a
#' footnote (the old promotion into the title block was removed
#' 2026-07-04 -- the canvas shows what the options carry, nothing more).
#' Dynamic per-line controls use the same single-shared-input pattern as
#' `.assigned_row()`/`.toc_kebab()`.
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
    `data-ar-item` = as.character(i),
    shiny::tags$span(class = "ar-fn-grip", .icon("grip", 11)),
    shiny::tags$input(
      type = "text",
      class = "ar-fn-input",
      value = value,
      onchange = edit_js,
      `aria-label` = paste0("Footnote ", i)
    ),
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
    list(
      # Footnote ORDER is part of the output (line 1 doubles as the paper's
      # population subtitle), so the rows drag-reorder through the same
      # sortable contract the Roles slots use; rows are keyed by index and
      # the pane redraws after a drop so the keys never go stale.
      do.call(
        shiny::tags$div,
        c(
          list(
            class = "ar-fn-list",
            `data-ar-sortable` = "true",
            `data-ar-sortable-handle` = ".ar-fn-grip",
            `data-ar-sortable-item` = ".ar-fn-row",
            `data-ar-sortable-attr` = "data-ar-item",
            `data-ar-sortable-input` = ns("fn_reorder")
          ),
          lapply(seq_along(fns), function(i) .fn_row(ns, i, fns[[i]]))
        )
      ),
      .action_btn(
        ns("fn_add"),
        shiny::tagList(.icon("plus", 11), "Add footnote"),
        variant = "link",
        class = "ar-fn-add"
      )
    )
  )
}

#' The schema-generated option sections, grouped by `.OPT_REGION` in
#' `.OPT_SECTIONS` order. Ordering keys (`.RANKS_KEYS`) are the Ranks
#' pane's content and are filtered out here.
#' @noRd
.opt_schema_sections <- function(con, ns, object, schema) {
  if (is.null(schema) || nrow(schema) == 0L) {
    return(NULL)
  }
  schema <- schema[!schema$key %in% .RANKS_KEYS, , drop = FALSE]
  if (nrow(schema) == 0L) {
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

# ---- layout sections (global-requirements parity) -------------------------

# Display-name maps for the layout choice knobs: the stored value is the
# engine generic ("mono"), the label the term a statistician expects
# ("Courier New" -- the face Word actually substitutes for the generic).
.LAYOUT_CHOICES <- list(
  orientation = c(Landscape = "landscape", Portrait = "portrait"),
  paper = c("US Letter" = "letter", A4 = "a4"),
  page_n = c("Off" = "off", "In arm headers" = "headers"),
  width_mode = c(
    "Auto-fit contents" = "content",
    "Window (fill page)" = "window",
    "Fixed" = "fixed"
  ),
  font_family = c(
    "Courier New" = "mono",
    "Arial" = "sans",
    "Times New Roman" = "serif"
  )
)

# The layout keys whose commits ride the generic `.commit_opt()` path
# (text / int / choice kinds); lines, band, and margins have dedicated
# observers.
.LAYOUT_GENERIC_KEYS <- c(
  "header_n",
  "total",
  "orientation",
  "paper",
  "font_family",
  "font_size",
  "width_mode",
  "page_by",
  "page_n",
  "page_banner",
  "panels",
  "group_skip",
  "stub_label"
)

#' One layout schema row by key (the layout twin of the option_schema
#' lookup).
#' @noRd
.layout_row <- function(key) {
  sch <- arpillar::layout_schema()
  sch[sch$key == key, , drop = FALSE]
}

#' One running-band editor row: Left / Center / Right text inputs plus the
#' remove button, posting the shared `band_edit` / `band_rm` inputs keyed
#' by band + slot + row index.
#' @noRd
.band_row <- function(ns, band_key, i, values) {
  slot_input <- function(slot, ph) {
    edit_js <- sprintf(
      "Shiny.setInputValue('%s', {band: '%s', slot: '%s', i: %d, value: this.value, nonce: Date.now()}, {priority: 'event'})",
      ns("band_edit"),
      band_key,
      slot,
      i
    )
    shiny::tags$input(
      type = "text",
      class = "ar-band-input",
      value = values[[slot]] %||% "",
      placeholder = ph,
      onchange = edit_js,
      `aria-label` = paste0(band_key, " row ", i, " ", slot)
    )
  }
  remove_js <- sprintf(
    "Shiny.setInputValue('%s', {band: '%s', i: %d, nonce: Date.now()}, {priority: 'event'})",
    ns("band_rm"),
    band_key,
    i
  )
  shiny::tags$div(
    class = "ar-band-row",
    slot_input("left", "Left"),
    slot_input("center", "Center"),
    slot_input("right", "Right"),
    shiny::tags$button(
      type = "button",
      class = "ar-icon-btn ar-fn-remove",
      `aria-label` = paste0("Remove ", band_key, " row ", i),
      onclick = remove_js,
      .icon("close", 11)
    )
  )
}

#' One band editor (HEADER ROWS / FOOTER ROWS): the existing rows + add.
#' @noRd
.band_editor <- function(ns, object, band_key, label) {
  b <- object@options[[band_key]]
  n <- if (is.list(b)) max(lengths(b), 0L) else 0L
  add_js <- sprintf(
    "Shiny.setInputValue('%s', {band: '%s', nonce: Date.now()}, {priority: 'event'})",
    ns("band_add"),
    band_key
  )
  shiny::tags$div(
    class = "ar-band",
    shiny::tags$span(class = "ar-label ar-band-label", label),
    # Display rows in the same top-to-bottom order the canvas prints them
    # -- arpillar renders row [[N]] at the top of the header band and row
    # [[1]] at the bottom, so a top-to-bottom Options list must iterate
    # `rev(seq_len(n))`. The stored index passed to `.band_row()` stays
    # the raw i so band_edit / band_rm still address the right row.
    lapply(rev(seq_len(n)), function(i) {
      vals <- lapply(b, function(v) if (i <= length(v)) v[[i]] else "")
      .band_row(ns, band_key, i, vals)
    }),
    shiny::tags$button(
      type = "button",
      class = "btn btn-link ar-fn-add",
      onclick = add_js,
      "+ Add row"
    )
  )
}

#' The dataset's column metadata straight off the engine (no store cache
#' here -- the options pane redraws rarely), empty frame on failure.
#' @noRd
.items_meta_for <- function(con, object) {
  tryCatch(
    arpillar::data_items(con, object@dataset),
    error = function(e) {
      data.frame(
        name = character(0),
        type = character(0),
        sql_type = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
    }
  )
}

#' The arm-column choices a spanning band can cover: the treatment
#' variable's distinct values (engine pushdown) plus "Total" when the
#' pooled column is on. Empty when the treatment slot is unfilled or the
#' catalog is unreachable -- the section then shows its waiting hint.
#' @noRd
.span_arm_choices <- function(con, object) {
  r <- .role_for_slot(object, "treatment")
  if (is.null(r) || length(r@items) == 0L) {
    return(character(0))
  }
  arms <- tryCatch(
    arpillar::distinct_values(con, object@dataset, r@items[[1]]@name),
    error = function(e) character(0)
  )
  if (isTRUE(object@options$total)) {
    arms <- c(arms, "Total")
  }
  as.character(arms)
}

#' One spanning-band row: label input + arm multi-select + remove, all
#' posting shared inputs keyed by row index.
#' @noRd
.span_row <- function(ns, i, band, arms, claimed = character(0)) {
  label_js <- sprintf(
    "Shiny.setInputValue('%s', {i: %d, value: this.value, nonce: Date.now()}, {priority: 'event'})",
    ns("span_label"),
    i
  )
  # A per-row delegated posting shape: on any checkbox change inside this
  # row's box, gather the currently-checked arm names and post them. Cheaper
  # and more discoverable than the native <select multiple> (which needed a
  # Cmd/Ctrl-click users never guess).
  cols_js <- sprintf(
    "(function(box){Shiny.setInputValue('%s', {i: %d, value: Array.prototype.map.call(box.querySelectorAll('input[type=checkbox]:checked'), function(c){return c.value;}), nonce: Date.now()}, {priority: 'event'})})(this.closest('.ar-span-cols'))",
    ns("span_cols"),
    i
  )
  remove_js <- sprintf(
    "Shiny.setInputValue('%s', {i: %d, nonce: Date.now()}, {priority: 'event'})",
    ns("span_rm"),
    i
  )
  selected <- as.character(unlist(band$cols %||% character(0)))
  shiny::tags$div(
    class = "ar-span-row",
    shiny::tags$input(
      type = "text",
      class = "ar-band-input",
      value = band$label %||% "",
      placeholder = "Band label",
      onchange = label_js,
      `aria-label` = paste0("Spanning band ", i, " label")
    ),
    shiny::tags$div(
      class = "ar-span-cols",
      `aria-label` = paste0("Spanning band ", i, " columns"),
      lapply(arms, function(a) {
        # An arm claimed by an EARLIER band cannot go into this band --
        # tabular's `headers()` errors when the same column sits under
        # two bands. Render greyed + disabled + unchecked so the user
        # cannot compose the conflict at all.
        is_claimed <- a %in% claimed
        shiny::tags$label(
          class = paste(
            "ar-span-col",
            if (is_claimed) "ar-span-col-disabled"
          ),
          shiny::tags$input(
            type = "checkbox",
            value = a,
            checked = if (!is_claimed && a %in% selected) "checked",
            disabled = if (is_claimed) "disabled",
            onchange = cols_js
          ),
          shiny::tags$span(a)
        )
      })
    ),
    shiny::tags$button(
      type = "button",
      class = "ar-icon-btn ar-fn-remove",
      `aria-label` = paste0("Remove spanning band ", i),
      onclick = remove_js,
      .icon("close", 11)
    )
  )
}

#' The SPANNING HEADER section: one row per band + add. Unset = the
#' engine's default single "Treatment Group" band.
#' @noRd
.opt_spans_section <- function(con, ns, object) {
  arms <- .span_arm_choices(con, object)
  sp <- object@options$spans
  sp <- if (is.list(sp)) sp else list()
  add_js <- sprintf(
    "Shiny.setInputValue('%s', {nonce: Date.now()}, {priority: 'event'})",
    ns("span_add")
  )
  .opt_section(
    "SPANNING HEADER",
    list(
      if (length(arms) == 0L) {
        shiny::tags$p(
          class = "ar-opt-hint ar-mono",
          "Assign a treatment variable first."
        )
      } else {
        # Threaded `claimed` set: each band's row sees the arms already
        # taken by EARLIER bands and disables their checkboxes, so the
        # user cannot compose a two-band-per-column conflict.
        claimed <- character(0)
        rows <- lapply(seq_along(sp), function(i) {
          row <- .span_row(ns, i, sp[[i]], arms, claimed = claimed)
          own <- as.character(unlist(sp[[i]]$cols %||% character(0)))
          claimed <<- unique(c(claimed, setdiff(own, claimed)))
          row
        })
        shiny::tagList(
          rows,
          shiny::tags$button(
            type = "button",
            class = "btn btn-link ar-fn-add",
            onclick = add_js,
            "+ Add band"
          ),
          shiny::tags$p(
            class = "ar-opt-hint ar-mono",
            "No bands = one Treatment Group band over every arm."
          )
        )
      }
    )
  )
}

#' The layout sections (COLUMNS / PAGE & OUTPUT / SPANNING HEADER /
#' RUNNING HEADER & FOOTER), rendered off `arpillar::layout_schema()` for
#' TABLE outputs only -- the figure legs ignore every layout key, so a
#' figure never shows dead knobs.
#' @noRd
.opt_layout_sections <- function(con, ns, object) {
  if (.is_figure_type(object@type)) {
    return(NULL)
  }
  sch <- arpillar::layout_schema()
  row_of <- function(key) sch[sch$key == key, , drop = FALSE]
  cur <- function(key) .opt_current(object, row_of(key))
  choice_row <- function(key, width = "170px") {
    shiny::tags$div(
      class = "ar-opt-row",
      shiny::tags$span(class = "ar-opt-label", row_of(key)$label),
      shiny::selectInput(
        ns(paste0("opt_", key)),
        label = NULL,
        choices = .LAYOUT_CHOICES[[key]],
        selected = cur(key),
        selectize = FALSE,
        width = width
      )
    )
  }
  margins <- object@options$margins
  list(
    .opt_section(
      "COLUMNS",
      list(
        shiny::tags$div(
          class = "ar-opt-row ar-opt-row-wide",
          shiny::tags$span(class = "ar-opt-label", "Header N"),
          .opt_change_input(
            ns,
            "opt_header_n",
            cur("header_n") %||% "",
            placeholder = "(N={n})",
            width = "140px"
          )
        ),
        shiny::tags$p(
          class = "ar-opt-hint ar-mono",
          "{n} = the arm's population N; blank = no N line."
        ),
        shiny::tags$div(
          class = "ar-opt-row ar-opt-row-wide",
          shiny::tags$span(class = "ar-opt-label", "Stub column header"),
          # Textarea, left-aligned: the header often wraps across two lines
          # (e.g. "Baseline\nCharacteristics"). Enter inside the field
          # inserts a real newline; blur/Enter+Ctrl commits.
          .opt_change_textarea(
            ns,
            "opt_stub_label",
            cur("stub_label") %||% "",
            placeholder = "e.g. Parameter",
            width = "150px",
            rows = 2
          )
        ),
        shiny::tags$div(
          class = "ar-opt-row",
          shiny::tags$span(
            class = "ar-opt-label",
            "Blank row between blocks"
          ),
          shiny::checkboxInput(
            ns("opt_group_skip"),
            label = NULL,
            value = !identical(cur("group_skip"), FALSE)
          )
        ),
        shiny::tags$div(
          class = "ar-opt-row",
          shiny::tags$span(class = "ar-opt-label", "Total column"),
          shiny::checkboxInput(
            ns("opt_total"),
            label = NULL,
            value = isTRUE(cur("total"))
          )
        ),
        shiny::tags$p(
          class = "ar-opt-hint ar-mono",
          "Pooled across arms; a heavy edit \u2014 Run re-collects."
        )
      )
    ),
    .opt_section(
      "PAGE & OUTPUT",
      list(
        shiny::tags$div(
          class = "ar-opt-row",
          shiny::tags$span(class = "ar-opt-label", "Orientation"),
          shiny::radioButtons(
            ns("opt_orientation"),
            label = NULL,
            choices = .LAYOUT_CHOICES$orientation,
            selected = cur("orientation"),
            inline = TRUE
          )
        ),
        choice_row("paper"),
        choice_row("width_mode"),
        choice_row("font_family"),
        shiny::tags$div(
          class = "ar-opt-row",
          shiny::tags$span(class = "ar-opt-label", "Font size"),
          .opt_int_control(
            ns,
            "font_size",
            as.character(cur("font_size") %||% "")
          )
        ),
        shiny::tags$div(
          class = "ar-opt-row ar-opt-row-wide",
          shiny::tags$span(class = "ar-opt-label", "Margins (in)"),
          .opt_change_input(
            ns,
            "opt_margins",
            if (is.null(margins)) {
              ""
            } else {
              paste(margins, collapse = ", ")
            },
            placeholder = "1, 1, 1, 1"
          )
        ),
        shiny::tags$p(
          class = "ar-opt-hint ar-mono",
          "top, right, bottom, left \u00b7 one value = all sides"
        )
      )
    ),
    .opt_spans_section(con, ns, object),
    .opt_section(
      "SUBGROUP / PAGE BY",
      list(
        {
          # Pageable columns: the dataset's categories, minus the arm var.
          items <- .items_meta_for(con, object)
          arm <- {
            r <- .role_for_slot(object, "treatment")
            if (!is.null(r) && length(r@items) > 0L) r@items[[1]]@name else ""
          }
          cats <- setdiff(items$name[items$type %in% "category"], arm)
          shiny::tags$div(
            class = "ar-opt-row",
            shiny::tags$span(
              class = "ar-opt-label",
              "Page by (one table per level)"
            ),
            shiny::selectInput(
              ns("opt_page_by"),
              label = NULL,
              choices = c("None" = "", cats),
              selected = cur("page_by") %||% "",
              selectize = FALSE,
              width = "170px"
            )
          )
        },
        choice_row("page_n"),
        shiny::tags$div(
          class = "ar-opt-row ar-opt-row-wide",
          shiny::tags$span(class = "ar-opt-label", "Banner label"),
          .opt_change_input(
            ns,
            "opt_page_banner",
            cur("page_banner") %||% "",
            placeholder = "e.g. Sex: {SEX}",
            width = "170px"
          )
        ),
        shiny::tags$p(
          class = "ar-opt-hint ar-mono",
          "Banner tokens are the page-by column name, e.g. {SEX}. Blank = auto."
        ),
        shiny::tags$div(
          class = "ar-opt-row",
          shiny::tags$span(class = "ar-opt-label", "Panels (column groups)"),
          .opt_int_control(
            ns,
            "panels",
            as.character(cur("panels") %||% "")
          )
        )
      )
    ),
    .opt_section(
      "RUNNING HEADER & FOOTER",
      list(
        .band_editor(ns, object, "pagehead", "HEADER ROWS"),
        .band_editor(ns, object, "pagefoot", "FOOTER ROWS"),
        shiny::tags$p(
          class = "ar-opt-hint ar-mono",
          "Tokens: {page}, {npages}, {program}, {datetime}."
        )
      )
    )
  )
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
    # Structural edits made FROM this pane whose controls must repaint
    # (footnote reorder re-keys the rows; a stats add/remove changes the
    # row set; a stepper click must show the stepped value). Text commits
    # never bump it -- typing must not redraw the input mid-edit.
    pane_redraw <- shiny::reactiveVal(0L)

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
      schema <- tryCatch(
        arpillar::option_schema(obj@type),
        error = function(e) NULL
      )
      shiny::tagList(
        .opt_title_section(ns, obj),
        .opt_footnotes_section(ns, obj),
        .opt_schema_sections(store$con, ns, obj, schema),
        .opt_layout_sections(store$con, ns, obj),
        shiny::uiOutput(ns("opt_msg"))
      )
    }) |>
      shiny::bindEvent(
        store$rv$selected,
        .roles_digest(selected_object(store)),
        .fn_count(selected_object(store)),
        pane_redraw()
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

    # A footnote drop: reconcile the dragged index order against the
    # current lines (the Roles reorder discipline -- a stale/partial
    # payload never loses a line), commit, and redraw so the index keys
    # match the new order.
    shiny::observeEvent(input$fn_reorder, {
      obj <- selected_object(store)
      if (is.null(obj)) {
        return()
      }
      order <- suppressWarnings(as.integer(unlist(input$fn_reorder$order)))
      order <- order[!is.na(order)]
      idx <- c(
        intersect(order, seq_along(obj@footnotes)),
        setdiff(seq_along(obj@footnotes), order)
      )
      if (identical(idx, seq_along(obj@footnotes))) {
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) S7::set_props(o, footnotes = o@footnotes[idx]),
        label = "reorder footnotes"
      )
      pane_redraw(pane_redraw() + 1L)
    })

    # ---- continuation title lines (options$titles) ----
    .commit_titles <- function(obj, lines, label) {
      value <- as.character(lines)
      if (length(value) == 0L) {
        value <- NULL
      }
      if (identical(value, obj@options$titles)) {
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) {
          opts <- o@options
          opts$titles <- value
          S7::set_props(o, options = opts)
        },
        label = label
      )
    }

    shiny::observeEvent(input$tl_add, {
      obj <- selected_object(store)
      if (is.null(obj)) {
        return()
      }
      .commit_titles(
        obj,
        c(as.character(obj@options$titles %||% character(0)), ""),
        "add title line"
      )
      pane_redraw(pane_redraw() + 1L)
    })

    shiny::observeEvent(input$tl_edit, {
      obj <- selected_object(store)
      i <- as.integer(input$tl_edit$i)
      lines <- as.character(obj@options$titles %||% character(0))
      if (is.null(obj) || is.na(i) || i < 1L || i > length(lines)) {
        return()
      }
      lines[[i]] <- as.character(input$tl_edit$value)
      .commit_titles(obj, lines, "edit title line")
    })

    shiny::observeEvent(input$tl_remove, {
      obj <- selected_object(store)
      i <- as.integer(input$tl_remove$i)
      lines <- as.character(obj@options$titles %||% character(0))
      if (is.null(obj) || is.na(i) || i < 1L || i > length(lines)) {
        return()
      }
      .commit_titles(obj, lines[-i], "remove title line")
      pane_redraw(pane_redraw() + 1L)
    })

    # ---- margins (unsorted numvec: top, right, bottom, left) ----
    shiny::observeEvent(input$opt_margins, {
      obj <- selected_object(store)
      if (is.null(obj) || .is_figure_type(obj@type)) {
        return()
      }
      raw <- trimws(input$opt_margins %||% "")
      value <- NULL
      if (nzchar(raw)) {
        parts <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
        parts <- parts[nzchar(parts)]
        vals <- suppressWarnings(as.numeric(parts))
        # Order matters (top/right/bottom/left) -- NEVER the sorted
        # `.opt_parse()` numvec path.
        if (anyNA(vals) || !length(vals) %in% c(1L, 4L) || any(vals < 0)) {
          rv_err(sprintf(
            'Margins: "%s" is not 1 or 4 non-negative numbers.',
            raw
          ))
          return()
        }
        value <- vals
        if (all(value == 1)) {
          value <- NULL # the engine default -- elide
        }
      }
      if (identical(value, obj@options$margins)) {
        rv_err(NULL)
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) {
          opts <- o@options
          opts$margins <- value
          S7::set_props(o, options = opts)
        },
        label = "set margins"
      )
      rv_err(NULL)
    })

    # ---- running header / footer bands ----
    # Normalize a band to three equal-length slot vectors so row indexes
    # stay aligned across left/center/right.
    .band_norm <- function(b, n = NULL) {
      slots <- c("left", "center", "right")
      b <- if (is.list(b)) b else list()
      n <- n %||% max(c(lengths(b[intersect(names(b), slots)]), 0L))
      out <- lapply(slots, function(s) {
        v <- as.character(b[[s]] %||% character(0))
        length(v) <- n
        v[is.na(v)] <- ""
        v
      })
      stats::setNames(out, slots)
    }

    .commit_band <- function(obj, band_key, value, label) {
      if (identical(value, obj@options[[band_key]])) {
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) {
          opts <- o@options
          opts[[band_key]] <- value
          S7::set_props(o, options = opts)
        },
        label = label
      )
    }

    shiny::observeEvent(input$band_add, {
      obj <- selected_object(store)
      band_key <- as.character(input$band_add$band)
      if (is.null(obj) || !band_key %in% c("pagehead", "pagefoot")) {
        return()
      }
      b <- .band_norm(obj@options[[band_key]])
      b <- lapply(b, function(v) c(v, ""))
      .commit_band(obj, band_key, b, paste0("add ", band_key, " row"))
      pane_redraw(pane_redraw() + 1L)
    })

    shiny::observeEvent(input$band_rm, {
      obj <- selected_object(store)
      band_key <- as.character(input$band_rm$band)
      i <- as.integer(input$band_rm$i)
      if (is.null(obj) || !band_key %in% c("pagehead", "pagefoot")) {
        return()
      }
      b <- .band_norm(obj@options[[band_key]])
      n <- length(b$left)
      if (is.na(i) || i < 1L || i > n) {
        return()
      }
      b <- lapply(b, function(v) v[-i])
      if (length(b$left) == 0L) {
        b <- NULL # the last row went -- elide the whole band
      }
      .commit_band(obj, band_key, b, paste0("remove ", band_key, " row"))
      pane_redraw(pane_redraw() + 1L)
    })

    shiny::observeEvent(input$band_edit, {
      obj <- selected_object(store)
      band_key <- as.character(input$band_edit$band)
      slot <- as.character(input$band_edit$slot)
      i <- as.integer(input$band_edit$i)
      if (
        is.null(obj) ||
          !band_key %in% c("pagehead", "pagefoot") ||
          !slot %in% c("left", "center", "right")
      ) {
        return()
      }
      if (is.na(i) || i < 1L) {
        return()
      }
      b <- .band_norm(obj@options[[band_key]])
      if (i > length(b$left)) {
        # A row index beyond the stored band (stale DOM after an undo):
        # pad rather than truncate.
        b <- .band_norm(b, n = i)
      }
      b[[slot]][[i]] <- as.character(input$band_edit$value)
      if (all(vapply(b, function(v) all(!nzchar(v)), logical(1)))) {
        b <- NULL # every cell blank -- elide
      }
      .commit_band(obj, band_key, b, paste0("edit ", band_key))
    })

    # ---- spanning header bands ----
    .commit_spans <- function(obj, spans, label) {
      if (length(spans) == 0L) {
        spans <- NULL # no bands = the engine's default band -- elide
      }
      if (identical(spans, obj@options$spans)) {
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) {
          opts <- o@options
          opts$spans <- spans
          S7::set_props(o, options = opts)
        },
        label = label
      )
    }

    shiny::observeEvent(input$span_add, {
      obj <- selected_object(store)
      if (is.null(obj)) {
        return()
      }
      sp <- obj@options$spans
      sp <- if (is.list(sp)) sp else list()
      sp[[length(sp) + 1L]] <- list(label = "", cols = character(0))
      .commit_spans(obj, sp, "add spanning band")
      pane_redraw(pane_redraw() + 1L)
    })

    shiny::observeEvent(input$span_rm, {
      obj <- selected_object(store)
      i <- as.integer(input$span_rm$i)
      sp <- if (is.null(obj)) list() else obj@options$spans
      sp <- if (is.list(sp)) sp else list()
      if (is.null(obj) || is.na(i) || i < 1L || i > length(sp)) {
        return()
      }
      .commit_spans(obj, sp[-i], "remove spanning band")
      pane_redraw(pane_redraw() + 1L)
    })

    shiny::observeEvent(input$span_label, {
      obj <- selected_object(store)
      i <- as.integer(input$span_label$i)
      sp <- if (is.null(obj)) list() else obj@options$spans
      sp <- if (is.list(sp)) sp else list()
      if (is.null(obj) || is.na(i) || i < 1L || i > length(sp)) {
        return()
      }
      sp[[i]]$label <- as.character(input$span_label$value)
      .commit_spans(obj, sp, "relabel spanning band")
    })

    shiny::observeEvent(input$span_cols, {
      obj <- selected_object(store)
      i <- as.integer(input$span_cols$i)
      sp <- if (is.null(obj)) list() else obj@options$spans
      sp <- if (is.list(sp)) sp else list()
      if (is.null(obj) || is.na(i) || i < 1L || i > length(sp)) {
        return()
      }
      # Defensive: drop any incoming arm already claimed by an EARLIER
      # band, in case a stale saved report or an out-of-order client
      # message sneaks past the UI's disabled-checkbox guard. tabular's
      # `headers()` would abort the render otherwise.
      incoming <- as.character(unlist(input$span_cols$value))
      claimed_by_others <- unlist(lapply(sp[seq_len(i - 1L)], function(b) {
        as.character(unlist(b$cols %||% character(0)))
      }))
      sp[[i]]$cols <- setdiff(incoming, claimed_by_others)
      .commit_spans(obj, sp, "set spanning band columns")
    })

    # ---- stepper + statistics membership ----
    shiny::observeEvent(input$opt_step, {
      obj <- selected_object(store)
      if (is.null(obj)) {
        return()
      }
      schema <- tryCatch(
        arpillar::option_schema(obj@type),
        error = function(e) NULL
      )
      row <- if (is.null(schema)) {
        NULL
      } else {
        schema[schema$key == input$opt_step$key, , drop = FALSE]
      }
      if (is.null(row) || nrow(row) != 1L) {
        # A layout stepper (font_size) is not in the generator schema.
        row <- .layout_row(input$opt_step$key)
      }
      if (nrow(row) != 1L || !identical(row$kind, "int")) {
        return()
      }
      current <- suppressWarnings(as.integer(.opt_current(obj, row) %||% 0L))
      if (length(current) != 1L || is.na(current)) {
        current <- 0L
      }
      stepped <- max(0L, current + as.integer(input$opt_step$dir))
      .commit_opt(store, rv_err, obj, row, as.character(stepped))
      pane_redraw(pane_redraw() + 1L)
    })

    .stats_row <- function(obj) {
      schema <- tryCatch(
        arpillar::option_schema(obj@type),
        error = function(e) NULL
      )
      if (is.null(schema)) {
        return(NULL)
      }
      row <- schema[schema$key == "stats", , drop = FALSE]
      if (nrow(row) != 1L) {
        return(NULL)
      }
      row
    }

    shiny::observeEvent(input$opt_stat_rm, {
      obj <- selected_object(store)
      row <- if (is.null(obj)) NULL else .stats_row(obj)
      if (is.null(row)) {
        return()
      }
      current <- as.character(.opt_current(obj, row))
      kept <- setdiff(current, input$opt_stat_rm$value)
      # Never commit an empty set -- the engine would fall back to the full
      # block anyway, which reads as "remove did nothing" in the UI.
      if (length(kept) == 0L || identical(kept, current)) {
        return()
      }
      .commit_opt(store, rv_err, obj, row, kept)
      pane_redraw(pane_redraw() + 1L)
    })

    shiny::observeEvent(input$opt_stat_add, {
      v <- input$opt_stat_add
      if (is.null(v) || !nzchar(v)) {
        return()
      }
      obj <- selected_object(store)
      row <- if (is.null(obj)) NULL else .stats_row(obj)
      if (is.null(row)) {
        return()
      }
      current <- as.character(.opt_current(obj, row))
      if (v %in% current) {
        return()
      }
      .commit_opt(store, rv_err, obj, row, c(current, v))
      pane_redraw(pane_redraw() + 1L)
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
        # contract). The order commits through `.commit_opt()` so a drag
        # back to the engine default order ELIDES the key (x_order's NULL
        # default never matches; stats' full default does).
        shiny::observeEvent(input[[reorder_id]], {
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
          order <- vapply(
            input[[reorder_id]]$order,
            as.character,
            character(1)
          )
          .commit_opt(store, rv_err, obj, row, order)
        })
      })
    }

    # The universal layout keys (text / int / choice kinds) commit through
    # the same `.commit_opt()` path, with the row looked up in
    # `layout_schema()` instead of the generator schema. Tables only --
    # the layout sections never render for a figure.
    for (lay_key in .LAYOUT_GENERIC_KEYS) {
      local({
        k <- lay_key
        input_id <- paste0("opt_", k)
        shiny::observeEvent(input[[input_id]], {
          obj <- selected_object(store)
          if (is.null(obj) || .is_figure_type(obj@type)) {
            return()
          }
          row <- .layout_row(k)
          if (nrow(row) != 1L) {
            return()
          }
          raw <- input[[input_id]]
          # Stub label ergonomics: users type either a real Enter (the
          # textarea honors it) or the two-character literal `\n`. Both
          # must render as a line break in the tabular stub column, so
          # translate the literal on commit.
          if (identical(k, "stub_label") && is.character(raw)) {
            raw <- gsub("\\n", "\n", raw, fixed = TRUE)
          }
          .commit_opt(store, rv_err, obj, row, raw)
        })
      })
    }

    invisible(NULL)
  })
}
