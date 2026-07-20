# The Options pane (design spec #8, plan Task 11): the Options-tab content
# of the docked inspector. Three stacked sections — TITLE (editable TLF
# number + label word + title, per the addendum's editable-numbering
# decision), FOOTNOTES (line editor; line 1 is the population statement by
# convention), and the schema-generated option rows
# (`arpillar::option_schema(type)`), grouped under micro-labels by the
# paper region each key belongs to. Every commit goes through
# `update_object()`; a value equal to the engine default REMOVES the key
# (default-elision — keeps report.json and `emit_code()` output minimal).
#
# v5 note: unlike the Roles pane, this pane never narrows on `rv$region` —
# all four option-owning regions (title/footnotes/series/legend) route to
# this one tab, and the full stack is small enough to show whole. A `title`
# region click additionally focuses the Title input (`ar-focus`).

# ---- key -> paper-region grouping ----------------------------------------

#' Which paper region an option key belongs to — the plan's routing table
#' (Task 11), used here as SECTION grouping labels, not as filters. A key
#' this map has not seen lands in the trailing "options" group.
#' @noRd
.OPT_REGION <- c(
  decimals = "rows",
  stats = "rows",
  group_display = "rows",
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

# Keys whose semantics are ORDERING, not display: they render in the ORDER
# section (`.opt_order_section()`) below so ordering lives in one place,
# never inside the regular AXES-style schema grouping.
.RANKS_KEYS <- c("hier_sort", "x_order")

# Keys the generic option pass never renders: `population` comes from
# Setup > Analysis sets via the Filters pane's POPULATION section (no per-output
# override; the engine still honours a legacy saved value), and `event_column`
# has its own custom control in the COLUMNS section (`.count_fmt_rows()`), so
# the generic flag renderer must skip it to avoid a double control.
.HIDDEN_OPT_KEYS <- c("population", "event_column")

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
#' as doubles — `identical()` against a parsed `2L` needs the coercion).
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

# Display labels for choice VALUES (the committed value stays the lowercase
# engine token). Initialisms map explicitly; everything else title-cases its
# first letter, so a pill never reads as a bare code word ("rows" -> "Rows").
.OPT_CHOICE_LABELS <- c(
  ci = "CI",
  se = "SE",
  sd = "SD",
  freq = "Frequency",
  alpha = "Alphabetical",
  header_row = "Nested rows",
  column = "Column",
  column_repeat = "Column (repeated)"
)

#' Named choice vector for a segmented pill: values = engine tokens,
#' names = display labels.
#' @noRd
.opt_choice_named <- function(choices) {
  vals <- as.character(choices)
  labs <- vapply(
    vals,
    function(v) {
      if (v %in% names(.OPT_CHOICE_LABELS)) {
        return(.OPT_CHOICE_LABELS[[v]])
      }
      if (grepl("^[a-z]", v)) {
        substr(v, 1, 1) <- toupper(substr(v, 1, 1))
      }
      v
    },
    character(1),
    USE.NAMES = FALSE
  )
  stats::setNames(vals, labs)
}

#' The Treatment control's choices: Setup > Treatment's variables by NAME,
#' each valued by its estimand basis (the token `arm_mode` carries), after
#' "Auto" (defer to the bound analysis set's basis / generator convention).
#' `vars` is the RESOLVED row list (`.trt_vars()` — committed theme rows or
#' the same seeds Setup displays), so the two surfaces never disagree. Two
#' variables sharing a basis keep the first; no rows falls back to the bare
#' estimand words.
#' @noRd
.arm_mode_choices <- function(vars) {
  ch <- c(Auto = "auto")
  if (is.list(vars)) {
    for (v in vars) {
      basis <- as.character(v$basis %||% "")
      nm <- as.character(v$var %||% "")
      if (basis %in% c("actual", "planned") && nzchar(nm) && !basis %in% ch) {
        ch[[nm]] <- basis
      }
    }
  }
  if (length(ch) == 1L) {
    ch <- c(ch, Actual = "actual", Planned = "planned")
  }
  ch
}

# ---- controls ---------------------------------------------------------

#' A blur/Enter-commit text input: a RAW <input> whose onchange posts the
#' namespaced option input, so typing never commits per keystroke (audit
#' note) — the value lands when the field loses focus or the
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

#' An id-less native checkbox: NO Shiny input binding, so a drill-switch
#' rebind can never replay a stale value onto the newly drilled output (the
#' known data-loss class). `onchange` posts `this.checked` to the SAME
#' `opt_<key>` channel the bound control used — the per-key observers are
#' untouched, and a post now happens only on a REAL click (no bind-time
#' seed post to guard against).
#' @noRd
.opt_flag_input <- function(ns, input_id, value) {
  js <- sprintf(
    "Shiny.setInputValue('%s', this.checked, {priority: 'event'})",
    ns(input_id)
  )
  shiny::tags$div(
    class = "form-group shiny-input-container",
    shiny::tags$div(
      class = "checkbox",
      shiny::tags$label(
        shiny::tags$input(
          type = "checkbox",
          checked = if (isTRUE(value)) NA,
          onchange = js
        ),
        shiny::tags$span()
      )
    )
  )
}

#' An id-less pill group: native radios sharing only a `name` for mutual
#' exclusion (never a Shiny id), posting `this.value` to the `opt_<key>`
#' channel on a real click — the hier_sort ORDER-pill idiom, generalized.
#' @noRd
.opt_pill_group <- function(ns, input_id, choices, current) {
  labs <- names(choices)
  if (is.null(labs)) {
    labs <- as.character(choices)
  }
  js <- sprintf(
    "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
    ns(input_id)
  )
  pills <- lapply(seq_along(choices), function(i) {
    shiny::tags$label(
      class = "radio-inline",
      shiny::tags$input(
        type = "radio",
        name = ns(paste0(input_id, "_r")),
        value = choices[[i]],
        checked = if (
          identical(as.character(current), as.character(choices[[i]]))
        ) {
          NA
        },
        onchange = js
      ),
      shiny::tags$span(labs[[i]])
    )
  })
  shiny::tags$div(class = "shiny-options-group", pills)
}

#' An id-less native `<select>`: posts `this.value` to the `opt_<key>`
#' channel on change; no Shiny binding to replay across a drill switch.
#' @noRd
.opt_native_select <- function(
  ns,
  input_id,
  choices,
  current,
  width = "170px"
) {
  labs <- names(choices)
  if (is.null(labs)) {
    labs <- as.character(choices)
  }
  js <- sprintf(
    "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
    ns(input_id)
  )
  opts <- lapply(seq_along(choices), function(i) {
    shiny::tags$option(
      value = choices[[i]],
      selected = if (
        identical(as.character(current), as.character(choices[[i]]))
      ) {
        NA
      },
      labs[[i]]
    )
  })
  shiny::tags$select(
    class = "form-control ar-opt-select",
    style = paste0("width:", width, ";"),
    onchange = js,
    opts
  )
}

#' A text input carrying `inputmode="numeric"` — the plan's int control
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
#' standard option path — typing in the text input still works unchanged.
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
.opt_stats_control <- function(ns, object, row, study_stats = character(0)) {
  # Mirror the engine's `.stats_opt()`: the BASE set is Setup > Summaries'
  # continuous rows (`study_stats`) when the study defines them, else the
  # generator schema's default block. An unset `options$stats` means "all of
  # base" (not the schema default), so the inspector shows exactly what the
  # render emits; a set value selects/reorders within base.
  all_stats <- if (length(study_stats) > 0L) {
    study_stats
  } else {
    as.character(row$choices[[1]])
  }
  sel <- object@options[[row$key]]
  current <- if (is.null(sel)) {
    all_stats
  } else {
    intersect(as.character(sel), all_stats)
  }
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
#' bind. Returns `NULL` (row skipped) while the x slot is unfilled — there
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
.opt_control <- function(
  con,
  ns,
  object,
  row,
  study_stats = character(0),
  trt_vars = list()
) {
  key <- row$key
  current <- .opt_current(object, row)
  input_id <- paste0("opt_", key)
  # Legend renders as a Show-legend toggle; the four position pills appear
  # only while it is on (a value of "none" = off). The position pill keeps
  # the generic `opt_legend_position` commit path; the toggle has its own
  # observer that flips between "none" and "bottom".
  if (identical(key, "legend_position")) {
    show <- !identical(current, "none")
    return(shiny::tagList(
      shiny::tags$div(
        class = "ar-opt-row",
        shiny::tags$span(class = "ar-opt-label", "Show legend"),
        .opt_flag_input(ns, "opt_legend_show", show)
      ),
      if (show) {
        shiny::tags$div(
          class = "ar-opt-row ar-opt-row-block",
          shiny::tags$span(class = "ar-opt-label", "Position"),
          .opt_pill_group(
            ns,
            input_id,
            .opt_choice_named(setdiff(
              as.character(row$choices[[1]]),
              "none"
            )),
            current
          )
        )
      }
    ))
  }
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
    flag = .opt_flag_input(ns, input_id, isTRUE(current)),
    # Treatment picks BY VARIABLE: the choices are Setup > Treatment's
    # variables (value = each one's estimand basis — the token the engine
    # consumes), so the user selects TRT01A/TRT01P, never an abstract word.
    choice = if (identical(key, "arm_mode")) {
      .opt_native_select(ns, input_id, .arm_mode_choices(trt_vars), current)
    } else if (identical(key, "group_display")) {
      # Three long labels never fit the docked pane as a pill group.
      .opt_native_select(
        ns,
        input_id,
        .opt_choice_named(row$choices[[1]]),
        current
      )
    } else {
      .opt_pill_group(
        ns,
        input_id,
        .opt_choice_named(row$choices[[1]]),
        current
      )
    },
    levels = if (identical(key, "stats")) {
      .opt_stats_control(ns, object, row, study_stats)
    } else {
      .opt_levels_control(con, ns, object, row)
    },
    NULL
  )
  if (is.null(control)) {
    return(NULL)
  }
  # Stack the label ABOVE the control for free-text and choice knobs (matches
  # Setup's field pattern): a full-width text box reads far better than the old
  # 88px right-pinned box, and a segmented pill group (see the .radio-inline CSS)
  # has room to lay out its options instead of wrapping. Short toggles (flag)
  # and the int stepper stay inline (label left, control right).
  # The Treatment select sits inline (label left, 170px control right) like
  # the layout selects; other choice knobs stack as full-width pill groups.
  block <- row$kind %in%
    c("levels", "text", "choice") &&
    !key %in% c("arm_mode", "group_display")
  row_tag <- shiny::tags$div(
    class = paste0("ar-opt-row", if (block) " ar-opt-row-block"),
    shiny::tags$span(class = "ar-opt-label", row$label),
    control
  )
  # Statistics carry a per-output override STATUS: unset = inheriting Setup's
  # continuous rows, set = this output overrides them (with a one-click Reset
  # back to the Setup default). Makes the study -> output layering visible at
  # the knob, not just in the header banner.
  if (identical(key, "stats")) {
    overridden <- !is.null(object@options[["stats"]])
    status <- if (overridden) {
      shiny::tags$p(
        class = "ar-opt-status ar-mono ar-opt-override",
        "Per-output override \u2014 ",
        shiny::tags$button(
          type = "button",
          class = "btn btn-link ar-fn-add ar-opt-reset",
          onclick = sprintf(
            "Shiny.setInputValue('%s', {nonce: Date.now()}, {priority: 'event'})",
            ns("opt_stat_reset")
          ),
          "Reset to Setup"
        )
      )
    } else {
      shiny::tags$p(
        class = "ar-opt-status ar-mono",
        "Inheriting Setup default"
      )
    }
    return(shiny::tagList(row_tag, status))
  }
  # The derived-precision contract (mean at d, SD at d+1, % always 1 dp) is
  # documented in the ROWS help topic, not spelled out inline.
  row_tag
}

# ---- sections ---------------------------------------------------------

#' One pane section: a fold/unfold accordion (`.accordion_section()`) around
#' the content rows, with an optional pre-built `help` tag (Task 10's
#' `.help_icon()`) and an optional leading icon in the summary.
#' @noRd
.opt_section <- function(label, rows, help = NULL, icon = NULL) {
  .accordion_section(label, rows, icon = icon, help = help)
}

#' The TITLE section: number + label word (both editable, addendum
#' decision — numbering is SAP-shell driven metadata, never re-derived),
#' the title line, and the Appendix-I continuation title lines
#' (`options$titles` — "Title 2 .. Title X", centered under the main
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
  # Number / label / title are ID-LESS native inputs posting one shared
  # `title_edit {field, value, nonce}` on blur/change — a real Shiny id
  # here replays the PREVIOUS output's values on rebind when the drill
  # switches outputs, silently overwriting the new output's title/number
  # (observed data-loss bug). The onchange idiom posts only
  # on a real user edit, so there is nothing to replay.
  edit_js <- function(field) {
    sprintf(
      "Shiny.setInputValue('%s', {field: '%s', value: this.value, nonce: Date.now()}, {priority: 'event'})",
      ns("title_edit"),
      field
    )
  }
  sel_label <- if (current_label %in% labels) current_label else "Table"
  .opt_section(
    "TITLE",
    help = .help_icon(ns, "title"),
    rows = list(
      shiny::tags$div(
        class = "ar-opt-number ar-mono",
        shiny::tags$select(
          class = "ar-lst-glue-sel ar-opt-labelword",
          `aria-label` = "Number label word",
          onchange = edit_js("number_label"),
          lapply(labels, function(l) {
            shiny::tags$option(
              value = l,
              selected = if (identical(l, sel_label)) "selected",
              l
            )
          })
        ),
        shiny::tags$input(
          type = "text",
          class = "ar-fn-input ar-mono",
          value = object@options[["number"]] %||% "",
          placeholder = "14.1.1",
          `aria-label` = "TLF number",
          onchange = edit_js("number")
        )
      ),
      shiny::tags$input(
        type = "text",
        class = "ar-fn-input ar-opt-title-in",
        value = object@title,
        placeholder = "Output title",
        `aria-label` = "Output title",
        onchange = edit_js("title")
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
#' (blur/Enter — footnotes are sentences, not live-typed previews), and a
#' remove button posting `fn_remove`. Footnote 1 renders ONLY as a
#' footnote (the old promotion into the title block was removed
#' — the canvas shows what the options carry, nothing more).
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
    help = .help_icon(ns, "footnotes_out"),
    rows = list(
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
#' `.OPT_SECTIONS` order. Ordering keys (`.RANKS_KEYS`) are the ORDER
#' section's content (`.opt_order_section()`) and are filtered out here.
#' @noRd
.opt_schema_sections <- function(
  con,
  ns,
  object,
  schema,
  study_stats = character(0),
  trt_vars = list()
) {
  if (is.null(schema) || nrow(schema) == 0L) {
    return(NULL)
  }
  schema <- schema[
    !schema$key %in% c(.RANKS_KEYS, .HIDDEN_OPT_KEYS),
    ,
    drop = FALSE
  ]
  # The any-event LABEL edits only while the any-event row is on — an
  # editable label for a row the render omits is dead UI.
  if (all(c("overall_row", "overall_label") %in% schema$key)) {
    orow <- schema[schema$key == "overall_row", , drop = FALSE]
    if (!isTRUE(.opt_current(object, orow))) {
      schema <- schema[schema$key != "overall_label", , drop = FALSE]
    }
  }
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
      help = .help_icon(ns, paste0("options_", sec)),
      rows = lapply(idx, function(i) {
        .opt_control(
          con,
          ns,
          object,
          schema[i, , drop = FALSE],
          study_stats,
          trt_vars
        )
      })
    )
  })
}

# ---- ORDER section (relocated from the deleted mod_card_ranks.R) ----------

#' Informational directive when the ORDER section has no drag surface to
#' show yet — e.g. "Assign an X variable in Roles first". Not a decorative
#' placeholder: the sentence tells the user what to do to unlock the
#' section. Rendered as a plain paragraph so no `.ar-*-empty` class ships
#' (the redesign strips those), but the text is still there.
#' @noRd
.order_empty <- function(text) {
  shiny::tags$p(class = "ar-insp-directive", text)
}

#' The row-block order editor (summary/crosstab): one grip row per
#' summarize item, dragging posts `rank_items` and commits through
#' `.reorder_slot()` — identical semantics to reordering inside the Roles
#' fieldset, surfaced here because order IS the rank for a summary table.
#' @noRd
.order_items_section <- function(ns, object) {
  role <- .role_for_slot(object, "summarize")
  items <- if (is.null(role)) list() else role@items
  if (length(items) == 0L) {
    return(.order_empty(paste0(
      "Assign variables in Roles first ",
      "\u2014 their row-block order is ranked here."
    )))
  }
  .opt_section(
    "ROW BLOCKS",
    help = .help_icon(ns, "order"),
    rows = list(
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
      )
    )
  )
}

#' The count-format pill choices per generator: the BASE count-cell shape.
#' Occurrence offers n (%) / n/N (%); the event-count (E) column is a SEPARATE
#' toggle (`.count_fmt_rows()`), not a pill variant. Summary/crosstab add a
#' bare-n option. Values are canonical keys `.count_fmt_template()` maps onto
#' the engine template.
#' @noRd
.count_fmt_choices <- function(type) {
  if (identical(type, "occurrence")) {
    c("n (%)" = "n_pct", "n/N (%)" = "nN_pct")
  } else {
    c("n" = "n", "n (%)" = "n_pct", "n/N (%)" = "nN_pct")
  }
}

#' Compose the engine template from a pill key + the % sign toggle.
#' @noRd
.count_fmt_template <- function(pill, sign) {
  p <- if (isTRUE(sign)) "{p}%" else "{p}"
  switch(
    pill,
    n = "{n}",
    n_pct = paste0("{n} (", p, ")"),
    nN_pct = paste0("{n}/{N} (", p, ")"),
    NULL
  )
}

#' Parse a count-format template into its pill key + % sign flag. Shared by
#' the per-output control (`.count_fmt_state()`) and the study-level Setup
#' control (`.setup_summaries()`). Forgiving: any hand-edited template maps to
#' the nearest pill by token content; an unset/blank template is the engine
#' default "{n} ({p})".
#' @noRd
.count_fmt_parse <- function(tpl) {
  tpl <- if (length(tpl) == 1L && !is.na(tpl) && nzchar(tpl)) {
    gsub("{p%}", "{p}%", as.character(tpl), fixed = TRUE)
  } else {
    "{n} ({p})"
  }
  has <- function(tok) grepl(tok, tpl, fixed = TRUE)
  list(
    pill = paste0(
      if (has("{N}")) "nN" else "n",
      if (has("{p}")) "_pct" else ""
    ),
    sign = grepl("%", gsub("\\{[nNp]\\}", "", tpl))
  )
}

#' Derive the pill key + sign flag from the per-output committed template.
#' @noRd
.count_fmt_state <- function(object) {
  .count_fmt_parse(object@options$count_format)
}

#' The EFFECTIVE count-format state — what the render will actually do.
#' Per-output `options$count_format` wins; else the study theme's template
#' (Setup > Summaries), honouring the legacy `level_format` enum exactly as
#' the engine's `.count_format_opt()` does; else the engine default.
#' `event` resolves the same way: a usable per-output `options$event_column`
#' beats the theme flag. Pass `object = NULL` for the pure study-level state
#' (the Setup pane seed). Controls seeded from this can never contradict the
#' rendered table (review finding: options-only seeding painted an unchecked
#' E toggle while the theme-driven E column rendered).
#' @noRd
.count_fmt_effective <- function(object, theme = list()) {
  cr <- theme$summaries$categorical %||% list()
  v <- if (is.null(object)) NULL else object@options$count_format
  tpl <- if (length(v) == 1L && !is.na(v) && nzchar(v)) {
    as.character(v)
  } else {
    t <- cr$count_format
    if (length(t) == 1L && !is.na(t) && nzchar(t)) {
      as.character(t)
    } else {
      switch(
        as.character(cr$level_format %||% "n_pct")[[1L]],
        n = "{n}",
        pct = "{p}",
        "{n} ({p})"
      )
    }
  }
  ev <- if (is.null(object)) NULL else object@options$event_column
  event <- if (length(ev) == 1L && !is.na(ev)) {
    isTRUE(ev)
  } else {
    isTRUE(cr$event_column)
  }
  c(.count_fmt_parse(tpl), list(event = event))
}

#' The COLUMNS-section count-format rows: an id-less pill group (the
#' hier_sort idiom — native radios sharing only a `name`) plus an id-less
#' "% sign" checkbox. Both post `{pill | sign, nonce}` to `count_fmt`; the
#' shared observer composes the template and commits through
#' `.commit_opt()` with `keep_default = TRUE` (theme-backed keys keep the
#' explicit value). Seeded from the EFFECTIVE state (per-output over study
#' theme) so the controls always paint what the render does.
#' @noRd
.count_fmt_rows <- function(ns, object, theme = list()) {
  st <- .count_fmt_effective(object, theme)
  choices <- .count_fmt_choices(object@type)
  pill_js <- sprintf(
    "Shiny.setInputValue('%s', {pill: this.value, nonce: Date.now()}, {priority: 'event'})",
    ns("count_fmt")
  )
  sign_js <- sprintf(
    "Shiny.setInputValue('%s', {sign: this.checked, nonce: Date.now()}, {priority: 'event'})",
    ns("count_fmt")
  )
  event_js <- sprintf(
    "Shiny.setInputValue('%s', {event: this.checked, nonce: Date.now()}, {priority: 'event'})",
    ns("count_fmt")
  )
  pills <- lapply(seq_along(choices), function(i) {
    shiny::tags$label(
      class = "radio-inline",
      shiny::tags$input(
        type = "radio",
        name = ns("count_fmt_pill"),
        value = choices[[i]],
        checked = if (identical(st$pill, choices[[i]])) NA,
        onchange = pill_js
      ),
      shiny::tags$span(names(choices)[[i]])
    )
  })
  shiny::tagList(
    shiny::tags$div(
      class = "ar-opt-row ar-opt-row-block",
      shiny::tags$span(class = "ar-opt-label", "Count format"),
      shiny::tags$div(class = "shiny-options-group", pills)
    ),
    shiny::tags$div(
      class = "ar-opt-row",
      shiny::tags$span(class = "ar-opt-label", "Percent sign in cells"),
      # Structural twin of shiny::checkboxInput() (so it aligns with the
      # Total / Blank-row checkboxes above) but WITHOUT a Shiny id: a raw
      # <input onchange> has no input binding to replay a stale value on a
      # drill-switch rebind (the id-less pane rule); it posts the sign
      # dimension to the shared `count_fmt` observer only on real clicks.
      shiny::tags$div(
        class = "form-group shiny-input-container",
        shiny::tags$div(
          class = "checkbox",
          shiny::tags$label(
            shiny::tags$input(
              type = "checkbox",
              checked = if (isTRUE(st$sign)) NA,
              onchange = sign_js
            ),
            shiny::tags$span()
          )
        )
      )
    ),
    # Occurrence only: the event-count (E) column is a SEPARATE toggle (not a
    # count-format pill variant); its onchange posts the `event` dimension to
    # the shared `count_fmt` observer, which commits options$event_column.
    if (identical(object@type, "occurrence")) {
      shiny::tags$div(
        class = "ar-opt-row",
        shiny::tags$span(class = "ar-opt-label", "Event count (E) column"),
        shiny::tags$div(
          class = "form-group shiny-input-container",
          shiny::tags$div(
            class = "checkbox",
            shiny::tags$label(
              shiny::tags$input(
                type = "checkbox",
                checked = if (isTRUE(st$event)) NA,
                onchange = event_js
              ),
              shiny::tags$span()
            )
          )
        )
      )
    }
  )
}

#' The SOC/PT incidence order control (occurrence): the engine's
#' `hier_sort` choice as an ID-LESS two-way pill — native radio inputs
#' with NO Shiny id (only a shared `name` for mutual exclusion) whose
#' onchange posts `{key, value, nonce}` to the shared `opt_edit` observer,
#' the TITLE section's `title_edit` idiom. A real Shiny id here would
#' replay a stale value across a drill switch on rebind (the
#' data-loss class); the onchange post only fires on a real user click, so
#' there is nothing to replay. The pill reuses the structural classes the
#' choice-pill CSS keys off (`.ar-opt-row .shiny-options-group
#' .radio-inline`) WITHOUT `shiny-input-radiogroup`, so Shiny's input
#' binding never claims it. Commits route through `.commit_opt()`, so the
#' "freq" default still elides to `NULL` and an identical value is a
#' no-op.
#' @noRd
.order_hier_section <- function(ns, object) {
  schema <- tryCatch(
    arpillar::option_schema(object@type),
    error = function(e) NULL
  )
  row <- if (is.null(schema)) {
    NULL
  } else {
    schema[schema$key == "hier_sort", , drop = FALSE]
  }
  if (is.null(row) || nrow(row) != 1L) {
    return(NULL)
  }
  current <- .opt_current(object, row)
  choices <- .opt_choice_named(row$choices[[1]])
  edit_js <- sprintf(
    "Shiny.setInputValue('%s', {key: 'hier_sort', value: this.value, nonce: Date.now()}, {priority: 'event'})",
    ns("opt_edit")
  )
  pills <- lapply(seq_along(choices), function(i) {
    shiny::tags$label(
      class = "radio-inline",
      shiny::tags$input(
        type = "radio",
        name = ns("order_hier"),
        value = choices[[i]],
        checked = if (identical(current, choices[[i]])) NA,
        onchange = edit_js
      ),
      shiny::tags$span(names(choices)[[i]])
    )
  })
  .opt_section(
    "INCIDENCE ORDER",
    help = .help_icon(ns, "order"),
    rows = list(
      shiny::tags$div(
        class = "ar-opt-row ar-opt-row-block",
        shiny::tags$div(class = "shiny-options-group", pills)
      )
    )
  )
}

#' The x level order control (line/box): the SAME `.opt_levels_control()`
#' every other `levels`-kind key uses, seeded from the committed order else
#' the x variable's distinct values; commits through the EXISTING generic
#' reorder observer (`opt_reorder_x_order`).
#' @noRd
.order_xorder_section <- function(con, ns, object) {
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
    return(.order_empty(
      "Assign an X variable in Roles first \u2014 its levels are ranked here."
    ))
  }
  .opt_section(
    "X LEVEL ORDER",
    help = .help_icon(ns, "order"),
    rows = list(control)
  )
}

#' The ORDER section: per-generator ordering controls, relocated from the
#' deleted `mod_card_ranks.R` so ordering lives inside Options alongside
#' every other per-object control (one docked pane, not four). `NULL` for
#' km/listing — nothing to rank there, so no empty-state clutter.
#' @noRd
.opt_order_section <- function(con, ns, object) {
  switch(
    object@type,
    summary = ,
    crosstab = .order_items_section(ns, object),
    occurrence = .order_hier_section(ns, object),
    line = ,
    box = .order_xorder_section(con, ns, object),
    NULL
  )
}

#' The measure-statistic labels the study defines in Setup > Summaries
#' (`theme$summaries$continuous`), in display order — the BASE set both the
#' engine's `.stats_opt()` and the Options stats control select from. Empty
#' when the study has not defined continuous rows (the control then falls back
#' to the generator schema default). Mirrors the engine's row->label read.
#' @noRd
.study_stat_labels <- function(theme) {
  rows <- theme$summaries$continuous
  if (!is.list(rows) || length(rows) == 0L) {
    return(character(0))
  }
  labs <- vapply(
    rows,
    function(r) as.character(r$label %||% ""),
    character(1)
  )
  labs[nzchar(labs)]
}

# ---- layout sections (global-requirements parity) -------------------------

# Display-name maps for the layout choice knobs: the stored value is the
# engine token, the label the term a user expects. Geometry (orientation /
# paper / font) is a study default owned by Setup > Page & Style and no
# longer edited per-output (plan Phase 3 dedup); what remains here is
# per-analysis.
.LAYOUT_CHOICES <- list(
  page_n = c("Off" = "off", "In arm headers" = "headers"),
  width_mode = c(
    "Auto-fit contents" = "content",
    "Window (fill page)" = "window",
    "Fixed" = "fixed"
  )
)

# The layout keys whose commits ride the generic `.commit_opt()` path
# (text / int / choice kinds); margins has a dedicated observer. Geometry
# (orientation / paper / font_family / font_size), header_n, and the
# running bands moved to Setup study defaults (plan Phase 3) — the render
# leg still resolves any per-output value from presets/import.
.LAYOUT_GENERIC_KEYS <- c(
  "total",
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

#' The dataset's column metadata straight off the engine (no store cache
#' here — the options pane redraws rarely), empty frame on failure.
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
#' catalog is unreachable — the section then shows its waiting hint.
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
        # An arm claimed by an EARLIER band cannot go into this band —
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
    help = .help_icon(ns, "options_spans"),
    rows = list(
      if (length(arms) == 0L) {
        .order_empty("Assign a treatment variable in Roles first.")
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
          )
        )
      }
    )
  )
}

#' The layout sections (COLUMNS / PAGE & OUTPUT / SPANNING HEADER /
#' SUBGROUP / PAGE BY), rendered off `arpillar::layout_schema()` for
#' TABLE outputs only — the figure legs ignore every layout key, so a
#' figure never shows dead knobs. Geometry, header_n, and the running
#' bands moved to Setup study defaults (plan Phase 3 dedup).
#' @noRd
.opt_layout_sections <- function(con, ns, object, theme = list()) {
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
      .opt_native_select(
        ns,
        paste0("opt_", key),
        .LAYOUT_CHOICES[[key]],
        cur(key),
        width = width
      )
    )
  }
  # A listing has no stub column and no pooled arm — those two knobs are
  # ARD-table concepts and never render for it. "Blank row between blocks"
  # stays: it governs the listing's subject-block blank row (group_skip on
  # the id column).
  is_listing <- identical(object@type, "listing")
  list(
    .opt_section(
      "COLUMNS",
      help = .help_icon(ns, "options_columns"),
      rows = list(
        if (!is_listing) {
          shiny::tags$div(
            class = "ar-opt-row ar-opt-row-block",
            shiny::tags$span(class = "ar-opt-label", "Stub column header"),
            # A tidy single-line box (rows = 1) that grows to fit a multi-line
            # header (e.g. "Baseline\nCharacteristics") via CSS `field-sizing`.
            # Multi-line stays load-bearing: Enter inserts a real newline that
            # renders as a line break in the tabular stub column; blur commits.
            # Full-width (block row), no fixed width — CSS governs.
            .opt_change_textarea(
              ns,
              "opt_stub_label",
              cur("stub_label") %||% "",
              placeholder = "e.g. Parameter",
              rows = 1
            )
          )
        },
        shiny::tags$div(
          class = "ar-opt-row",
          shiny::tags$span(
            class = "ar-opt-label",
            "Blank row between blocks"
          ),
          .opt_flag_input(
            ns,
            "opt_group_skip",
            !identical(cur("group_skip"), FALSE)
          )
        ),
        if (!is_listing) {
          shiny::tags$div(
            class = "ar-opt-row",
            shiny::tags$span(class = "ar-opt-label", "Total column"),
            .opt_flag_input(ns, "opt_total", isTRUE(cur("total")))
          )
        },
        if (!is_listing) {
          # Count format: PILLS + a % sign toggle composing the engine's
          # options$count_format template — no free-text (a typed template
          # invited `{p%}`-style typos, observed live 2026-07-11). ID-LESS
          # per the pane convention; occurrence adds the E-column toggle.
          # Seeded from the EFFECTIVE state (per-output over study theme).
          .count_fmt_rows(ns, object, theme)
        },
        NULL
      )
    ),
    .opt_section(
      "PAGE & OUTPUT",
      help = .help_icon(ns, "options_page"),
      rows = list(
        # Margins moved to Setup > Page & Style (study-level): page
        # geometry belongs with orientation / paper / font, not per-output. The
        # engine still honours a per-output `options$margins` override if one
        # was set, but arframe no longer exposes a control for it.
        # The "Total column pools across arms; a heavy edit" note moved to the
        # COLUMNS help topic.
        choice_row("width_mode")
      )
    ),
    .opt_spans_section(con, ns, object),
    .opt_section(
      "SUBGROUP / PAGE BY",
      help = .help_icon(ns, "options_pageby"),
      rows = list(
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
            .opt_native_select(
              ns,
              "opt_page_by",
              c("None" = "", cats),
              cur("page_by") %||% ""
            )
          )
        },
        choice_row("page_n"),
        shiny::tags$div(
          class = "ar-opt-row ar-opt-row-block",
          shiny::tags$span(class = "ar-opt-label", "Banner label"),
          .opt_change_input(
            ns,
            "opt_page_banner",
            cur("page_banner") %||% "",
            placeholder = "e.g. Sex: {SEX}"
          )
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
    )
  )
}

# ---- UI ---------------------------------------------------------------

#' The Options pane UI: a server-rendered section stack — the option rows
#' depend on the selected object's generator schema, so nothing here can
#' be static.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_card_options_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("pane"))
}

# ---- server -------------------------------------------------------------

#' Footnote count for the pane's redraw trigger — add/remove changes the
#' row set (needs a redraw); typing inside a line does not.
#' @noRd
.fn_count <- function(object) {
  if (is.null(object)) 0L else length(object@footnotes)
}

#' Commit one schema-row option value: parse by kind, surface an invalid
#' input as the inline message (NOT committed — the last good value
#' stands), elide a value equal to the engine default, and skip a no-op
#' (the value a freshly-rendered control posts on bind must never push an
#' undo entry).
#' @noRd
.commit_opt <- function(store, rv_err, object, row, raw, keep_default = FALSE) {
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
    # A numeric-typed choice (e.g. ci_level 0.9/0.95) must store a NUMBER:
    # HTML posts strings, the engine consumes the value arithmetically, and
    # a character "0.95" also never compares identical to the numeric schema
    # default (review finding).
    if (is.numeric(.opt_default(row))) {
      num <- suppressWarnings(as.numeric(value))
      if (length(num) == 1L && !is.na(num)) {
        value <- num
      }
    }
  }
  # Elide a schema-default value to "no key" ONLY for plain engine options.
  # A THEME-BACKED key (count_format / event_column) must keep the user's
  # explicit value: absent means "inherit the study default", so eliding a
  # default-equal choice would silently hand the decision back to the theme
  # (per-output-over-theme precedence bug, caught in review).
  if (!keep_default && !is.null(value) && identical(value, .opt_default(row))) {
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
#' footnote count — deliberately NOT on every report commit, so typing in
#' a text input never redraws the input mid-edit. (Known ceiling: an
#' undo/redo while the pane is open can leave control VALUES stale until
#' the next redraw trigger — the store stays authoritative, only the
#' control display lags.)
#' @param id *The module namespace, matching `mod_card_options_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`. From
#'   `new_store()`.
#' @noRd
mod_card_options_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # One shared help observer for every `?` icon rendered in this pane (the
    # sections build icons with THIS module's ns, so the observer lives here,
    # not in the parent card scope). `.show_help` is a no-op for a stale nonce.
    shiny::observeEvent(input$help_open, {
      .show_help(input$help_open$topic)
    })
    rv_err <- shiny::reactiveVal(NULL)
    # Structural edits made FROM this pane whose controls must repaint
    # (footnote reorder re-keys the rows; a stats add/remove changes the
    # row set; a stepper click must show the stepped value). Text commits
    # never bump it — typing must not redraw the input mid-edit.
    pane_redraw <- shiny::reactiveVal(0L)

    output$pane <- shiny::renderUI({
      obj <- selected_object(store)
      if (is.null(obj)) {
        return(NULL)
      }
      schema <- tryCatch(
        arpillar::option_schema(obj@type),
        error = function(e) NULL
      )
      study_stats <- .study_stat_labels(store$rv$report@theme)
      shiny::tagList(
        .opt_title_section(ns, obj),
        .opt_footnotes_section(ns, obj),
        .opt_schema_sections(
          store$con,
          ns,
          obj,
          schema,
          study_stats,
          .trt_vars(store$rv$report@theme, .pop_bindings(store))
        ),
        .opt_order_section(store$con, ns, obj),
        .opt_listing_sections(store$con, ns, obj),
        .opt_layout_sections(store$con, ns, obj, store$rv$report@theme),
        shiny::uiOutput(ns("opt_msg"))
      )
    }) |>
      shiny::bindEvent(
        store$rv$selected,
        .roles_digest(selected_object(store)),
        .fn_count(selected_object(store)),
        # Redraw when Setup > Summaries' continuous rows change so the stats
        # list stays in step with the study base (a narrow trigger — avoids
        # the full-report redraw the text-commit path deliberately skips).
        .study_stat_labels(store$rv$report@theme),
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

    # The listing generator's structured-option observers (sort / transpose
    # / stacks) — registered once here, inert for any other generator
    # (mod_card_listing.R).
    .listing_option_observers(input, output, session, store, pane_redraw)

    # ---- title section commits ----
    # One shared id-less post for number / label word / title (see
    # .opt_title_section: a real Shiny id replays stale values across a
    # drill switch). An identical value is still a no-op.
    shiny::observeEvent(input$title_edit, {
      obj <- selected_object(store)
      field <- as.character(input$title_edit$field %||% "")
      if (is.null(obj) || !field %in% c("title", "number", "number_label")) {
        return()
      }
      val <- as.character(input$title_edit$value %||% "")
      if (identical(field, "title")) {
        if (identical(val, obj@title)) {
          return()
        }
        update_object(
          store,
          obj@id,
          function(o) S7::set_props(o, title = val),
          label = "retitle output"
        )
        return()
      }
      if (identical(field, "number")) {
        val <- trimws(val)
        value <- if (nzchar(val)) val else NULL
        if (identical(value, obj@options[["number"]])) {
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
        return()
      }
      if (
        !val %in% c("Table", "Figure", "Listing") ||
          identical(val, obj@options$number_label)
      ) {
        return()
      }
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

    # A `title` region click (open_card) focuses the Title input — the
    # inspector tab flip is store-driven, so by the time the message lands
    # the Options pane is the visible one.
    shiny::observe({
      if (
        identical(store$rv$region, "title") &&
          identical(store$rv$insp_tab, "options")
      ) {
        # The Title input is deliberately id-less (see .opt_title_section);
        # focus it by class instead.
        session$sendCustomMessage("ar-focus", list(sel = ".ar-opt-title-in"))
      }
    }) |>
      shiny::bindEvent(store$rv$region, store$rv$insp_tab)

    # ---- ORDER section commits (relocated from mod_card_ranks.R) ----
    # Row-block order (summary/crosstab): the SAME reconcile-and-commit the
    # Roles fieldset drag uses — one helper, two surfaces, zero drift.
    # (occurrence's hier_sort and line/box's x_order commit through the
    # EXISTING generic schema-row observers below — see `.opt_order_section`
    # for why no bespoke observer is needed for those two.)
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

    # One shared id-less post for schema-key edits whose controls must NOT
    # carry a real Shiny id (the title_edit idiom — an id'd control
    # replays its stale value across a drill switch on rebind). Routes to
    # the SAME `.commit_opt()` path as the generic per-key observers, so
    # default-elision and the identical-value no-op hold. Today only the
    # ORDER section's hier_sort pill posts here.
    shiny::observeEvent(input$opt_edit, {
      obj <- selected_object(store)
      key <- as.character(input$opt_edit$key %||% "")
      if (is.null(obj) || !nzchar(key)) {
        return()
      }
      schema <- tryCatch(
        arpillar::option_schema(obj@type),
        error = function(e) NULL
      )
      if (is.null(schema)) {
        return()
      }
      row <- schema[schema$key == key, , drop = FALSE]
      if (nrow(row) != 1L) {
        return()
      }
      .commit_opt(store, rv_err, obj, row, input$opt_edit$value)
    })

    # Count-format pills + % sign toggle (COLUMNS section): each post
    # carries ONE dimension ({pill} or {sign}); the other is re-derived
    # from the EFFECTIVE state (per-output over study theme) so a sign-only
    # toggle on a theme-inheriting output never rewrites the shape. Both
    # theme-backed keys commit with keep_default = TRUE: an explicit choice
    # persists even when it equals the engine default, otherwise the theme
    # would silently win back (per-output-over-theme precedence).
    shiny::observeEvent(input$count_fmt, {
      obj <- selected_object(store)
      if (is.null(obj) || .is_figure_type(obj@type)) {
        return()
      }
      # The event-count (E) column dimension commits independently of the base
      # count template (options$event_column, from option_schema not layout).
      if (!is.null(input$count_fmt$event)) {
        schema <- tryCatch(
          arpillar::option_schema(obj@type),
          error = function(e) NULL
        )
        row <- if (is.null(schema)) {
          NULL
        } else {
          schema[schema$key == "event_column", , drop = FALSE]
        }
        if (!is.null(row) && nrow(row) == 1L) {
          .commit_opt(
            store,
            rv_err,
            obj,
            row,
            isTRUE(input$count_fmt$event),
            keep_default = TRUE
          )
        }
        return()
      }
      st <- .count_fmt_effective(obj, store$rv$report@theme)
      pill <- as.character(input$count_fmt$pill %||% st$pill)
      sign <- if (is.null(input$count_fmt$sign)) {
        st$sign
      } else {
        isTRUE(input$count_fmt$sign)
      }
      tpl <- .count_fmt_template(pill, sign)
      if (is.null(tpl)) {
        return()
      }
      .commit_opt(
        store,
        rv_err,
        obj,
        .layout_row("count_format"),
        tpl,
        keep_default = TRUE
      )
    })

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
    # current lines (the Roles reorder discipline — a stale/partial
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

    # Margins moved to Setup > Page & Style (study-level) — its
    # per-output observer + control were removed. The engine still resolves a
    # legacy `options$margins` override, but arframe writes only the study
    # default now (`theme$page$margins`, via `.SETUP_SPEC`'s `page_margins`).

    # ---- spanning header bands ----
    .commit_spans <- function(obj, spans, label) {
      if (length(spans) == 0L) {
        spans <- NULL # no bands = the engine's default band — elide
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

    # The effective statistic set the control DISPLAYS — the same resolution
    # `.opt_stats_control()` and the engine's `.stats_opt()` use: the Setup
    # continuous-row base (else schema default), unset `options$stats` meaning
    # "all of base". Add/remove operate on THIS, so a custom Setup stat is
    # never silently dropped by a schema-default read.
    .stats_current <- function(obj, row) {
      base <- .study_stat_labels(store$rv$report@theme)
      if (length(base) == 0L) {
        base <- as.character(row$choices[[1]])
      }
      sel <- obj@options[["stats"]]
      if (is.null(sel)) base else intersect(as.character(sel), base)
    }

    shiny::observeEvent(input$opt_stat_rm, {
      obj <- selected_object(store)
      row <- if (is.null(obj)) NULL else .stats_row(obj)
      if (is.null(row)) {
        return()
      }
      current <- .stats_current(obj, row)
      kept <- setdiff(current, input$opt_stat_rm$value)
      # Never commit an empty set — the engine would fall back to the full
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
      current <- .stats_current(obj, row)
      if (v %in% current) {
        return()
      }
      .commit_opt(store, rv_err, obj, row, c(current, v))
      pane_redraw(pane_redraw() + 1L)
    })

    # Reset to Setup: drop the per-output `options$stats` so the output falls
    # back to the study's continuous rows (Setup > Summaries).
    shiny::observeEvent(input$opt_stat_reset, {
      obj <- selected_object(store)
      if (is.null(obj) || is.null(obj@options[["stats"]])) {
        return()
      }
      update_object(
        store,
        obj@id,
        function(o) {
          opts <- o@options
          opts$stats <- NULL
          S7::set_props(o, options = opts)
        },
        label = "reset statistics to Setup"
      )
      pane_redraw(pane_redraw() + 1L)
    })

    # Show-legend toggle: off commits "none", on restores "bottom" (the
    # engine default); the position pill then commits through the generic
    # `opt_legend_position` observer. A bind-time post matching the current
    # state is a no-op, so a fresh render never pushes an undo entry.
    shiny::observeEvent(input$opt_legend_show, {
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
        schema[schema$key == "legend_position", , drop = FALSE]
      }
      if (is.null(row) || nrow(row) != 1L) {
        return()
      }
      shown <- !identical(.opt_current(obj, row), "none")
      want <- isTRUE(input$opt_legend_show)
      if (identical(shown, want)) {
        return()
      }
      .commit_opt(store, rv_err, obj, row, if (want) "bottom" else "none")
      pane_redraw(pane_redraw() + 1L)
    })

    # ---- schema-row commits ----
    # One observer per known option key across every generator — the same
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
          before <- obj@options[[k]]
          .commit_opt(store, rv_err, obj, row, input[[input_id]])
          # The any-event toggle shows/hides its label row — repaint only on
          # a REAL flip (a bind-time no-op post must not redraw-loop).
          if (identical(k, "overall_row")) {
            after <- selected_object(store)@options[[k]]
            if (!identical(before, after)) {
              pane_redraw(pane_redraw() + 1L)
            }
          }
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
    # `layout_schema()` instead of the generator schema. Tables only —
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
