# Setup mode: 4 collapsible sections on ONE scrollable page. No tab rail.
# Each section = a titled block with inline groups; the whole page is one
# `renderUI` output so a `commit()` on any field re-renders in place. Writes
# straight into `store$rv$report@theme`; auto-save observer flips `dirty`
# and writes `setup.yml`.
#
# Sections (final, 2026-07-06 -- see plan revision):
#   1. Study        - Identity + Extraction + Data paths
#   2. Populations  - ADaM-flag library (no estimand field; estimand is
#                     per-output via options$arm_mode, not study-wide)
#   3. Page         - Geometry + Running header + Running footer
#   4. Summaries    - Continuous rows + Categorical rules + Precision
#
# Deleted: Data / Arm / Numbering / Decimals sub-tabs (folded into Study /
# per-output / Summaries respectively). Arm entirely gone (estimand is
# per-output).

#' @noRd
mod_setup_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "ar-setup",
    # shinyFiles binds its client-side handler at UI-build time to buttons
    # that already exist in the DOM -- a button rendered later inside a
    # renderUI never gets the binding. Instantiate the two pickers here in
    # the static UI; CSS teleports them into the Data section headers
    # (see `.ar-setup [data-ar-adam-pick]` / `[data-ar-sdtm-pick]`).
    shiny::div(
      class = "ar-setup-pickers",
      shinyFiles::shinyDirButton(
        ns("data_adam_pick"),
        label = "Browse",
        title = "Select a folder of data files",
        icon = .icon("folder_plus", 13),
        class = "ar-dx-tb ar-adam-pick"
      ),
      shinyFiles::shinyDirButton(
        ns("data_sdtm_pick"),
        label = "Browse",
        title = "Select a folder of data files",
        icon = .icon("folder_plus", 13),
        class = "ar-dx-tb ar-sdtm-pick"
      )
    ),
    shiny::uiOutput(ns("page"))
  )
}

#' @noRd
mod_setup_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Seed the theme's populations library on FIRST render so outputs and
    # the {analysis-set} token see them. One-shot; subsequent renders read
    # what the user has since edited.
    shiny::observeEvent(store$rv$report, once = TRUE, {
      r <- store$rv$report
      pops <- r@theme$populations %||% list()
      if (length(pops) == 0L) {
        theme <- r@theme
        theme$populations <- .POP_SEEDS
        if (is.null(theme$default_population)) {
          theme$default_population <- "safety"
        }
        commit(store, S7::set_props(r, theme = theme), label = "seed populations")
      }
    })

    output$page <- shiny::renderUI({
      # React to whole-report edits so completion glyphs stay live.
      store$rv$report
      shiny::tagList(
        .setup_reviewed_banner(store),
        .setup_section(
          ns,
          "study",
          "Study",
          .section_status_glyph(store, "study"),
          .setup_study(ns, store)
        ),
        .setup_section(
          ns,
          "data",
          "Data",
          .section_status_glyph(store, "data"),
          .setup_data(ns, store)
        ),
        .setup_section(
          ns,
          "populations",
          "Populations",
          .section_status_glyph(store, "populations"),
          .setup_populations(ns, store)
        ),
        .setup_section(
          ns,
          "page",
          "Page",
          .section_status_glyph(store, "page"),
          .setup_page_body(ns, store)
        ),
        .setup_section(
          ns,
          "summaries",
          "Summaries",
          .section_status_glyph(store, "summaries"),
          .setup_summaries(ns, store)
        )
      )
    })
    shiny::outputOptions(output, "page", suspendWhenHidden = FALSE)

    # Study block bindings
    .bind_theme_field(input, store, "study", "sponsor")
    .bind_theme_field(input, store, "study", "protocol")
    .bind_theme_field(input, store, "study", "study")
    .bind_theme_field(input, store, "study", "indication")
    .bind_theme_field(input, store, "study", "data_date")
    .bind_theme_field(input, store, "study", "status")

    # Data block bindings
    .bind_theme_field(input, store, "data", "adam_dir")
    .bind_theme_field(input, store, "data", "sdtm_dir")
    .bind_theme_field(input, store, "data", "pop_dataset")
    .bind_theme_field(input, store, "data", "subject_id")
    .bind_theme_field(input, store, "data", "pop_treatment_var")

    # ADaM / SDTM folder pickers: shinyDirChoose delegates to a modal; on
    # a picked path, update the text field AND fire mount_folder so the
    # catalog populates immediately (bindings dropdowns light up).
    volumes <- c(home = path.expand("~"), root = "/")
    shinyFiles::shinyDirChoose(input, "data_adam_pick", roots = volumes)
    shinyFiles::shinyDirChoose(input, "data_sdtm_pick", roots = volumes)
    lapply(c("adam", "sdtm"), function(kind) {
      shiny::observeEvent(input[[paste0("data_", kind, "_pick")]], {
        dir <- shinyFiles::parseDirPath(volumes, input[[paste0("data_", kind, "_pick")]])
        if (length(dir) != 1L || !nzchar(dir)) return()
        # Push path into the visible field.
        shiny::updateTextInput(session, paste0("data_", kind, "_dir"), value = dir)
        # Mount the folder into the catalog. `.mount_folder` is defined in
        # mod_data.R; call via ::: since we're a sibling module.
        tryCatch(
          arframe:::.mount_folder(store, dir),
          error = function(e) {
            log_line(store, sprintf("mount %s failed: %s", kind, conditionMessage(e)))
          }
        )
      })
    })

    # Page block bindings
    .bind_theme_field(input, store, "page", "orientation")
    .bind_theme_field(input, store, "page", "paper")
    .bind_theme_field(input, store, "page", "font_family")
    .bind_theme_field(input, store, "page", "font_size")
    .bind_theme_field(input, store, "page", "pagehead")
    .bind_theme_field(input, store, "page", "pagefoot")

    # Populations default
    .bind_theme_top(input, store, "default_population")

    # Arm column-header bindings (show N + format; the "Treatment" label is
    # arpillar's built-in default, not a user setting).
    .bind_theme_field(input, store, "arm", "header_n_format")
    # show_header_n is a seg control with yes/no -- coerce to boolean.
    shiny::observeEvent(input$arm_show_header_n, {
      val <- identical(input$arm_show_header_n, "yes")
      r <- store$rv$report
      theme <- r@theme
      if (is.null(theme$arm)) theme$arm <- list()
      if (identical(theme$arm$show_header_n, val)) return()
      theme$arm$show_header_n <- val
      commit(store, S7::set_props(r, theme = theme), label = "edit setup")
    }, ignoreInit = TRUE)

    # Populations library observer: on any pop_* field edit, rebuild
    # `theme$populations` from the current inputs (ordered by row index).
    shiny::observe({
      pops <- .collect_pops(input)
      if (length(pops) == 0L) return()
      r <- store$rv$report
      theme <- r@theme
      if (identical(theme$populations, pops)) return()
      theme$populations <- pops
      commit(store, S7::set_props(r, theme = theme), label = "edit populations")
    })

    # Add / delete populations events
    shiny::observeEvent(input$pop_add, {
      r <- store$rv$report
      theme <- r@theme
      pops <- theme$populations %||% list()
      new_id <- paste0("pop", length(pops) + 1L)
      pops[[new_id]] <- list(label = "New population", dataset = "ADSL", filter = "")
      theme$populations <- pops
      commit(store, S7::set_props(r, theme = theme), label = "add population")
    })
    shiny::observeEvent(input$pop_delete, {
      del <- input$pop_delete
      r <- store$rv$report
      theme <- r@theme
      pops <- theme$populations %||% list()
      pops[[del]] <- NULL
      theme$populations <- pops
      if (identical(theme$default_population, del)) {
        theme$default_population <- names(pops)[[1L]] %||% ""
      }
      commit(store, S7::set_props(r, theme = theme), label = "delete population")
    })

    # Running header/footer: each row is 3 inputs (left/center/right).
    # The observer packs them into `list(left = <chr>, center = <chr>,
    # right = <chr>)` -- the shape tabular's page-band consumer expects.
    lapply(c("pagehead", "pagefoot"), function(key) {
      shiny::observe({
        band <- .collect_band_rows(input, key)
        r <- store$rv$report
        theme <- r@theme
        if (is.null(theme$page)) theme$page <- list()
        if (identical(theme$page[[key]], band)) return()
        theme$page[[key]] <- band
        commit(store, S7::set_props(r, theme = theme), label = "edit setup")
      })
    })

    # Add / delete a band row.
    lapply(c("pagehead", "pagefoot"), function(key) {
      shiny::observeEvent(input[[paste0("band_", key, "_add")]], {
        r <- store$rv$report
        theme <- r@theme
        if (is.null(theme$page)) theme$page <- list()
        band <- theme$page[[key]] %||% list(left = character(0), center = character(0), right = character(0))
        for (side in c("left", "center", "right")) {
          band[[side]] <- c(band[[side]] %||% character(0), "")
        }
        theme$page[[key]] <- band
        commit(store, S7::set_props(r, theme = theme), label = "add band row")
      })
      shiny::observeEvent(input[[paste0("band_", key, "_delete")]], {
        i <- as.integer(input[[paste0("band_", key, "_delete")]])
        r <- store$rv$report
        theme <- r@theme
        band <- theme$page[[key]] %||% NULL
        if (is.null(band)) return()
        for (side in c("left", "center", "right")) {
          v <- band[[side]] %||% character(0)
          if (i >= 1L && i <= length(v)) v <- v[-i]
          band[[side]] <- v
        }
        theme$page[[key]] <- band
        commit(store, S7::set_props(r, theme = theme), label = "delete band row")
      })
    })
  })
}

# Reassemble a band from `page_<key>_(left|center|right)_<i>` inputs into
# `list(left = <chr>, center = <chr>, right = <chr>)` -- matches
# `arpillar:::.band_opt`'s expected shape. Empty trailing rows are pruned.
.collect_band_rows <- function(input, key) {
  side_ids <- lapply(c("left", "center", "right"), function(side) {
    pfx <- paste0("page_", key, "_", side, "_")
    ids <- grep(paste0("^", pfx), names(input), value = TRUE)
    if (length(ids) == 0L) return(list(idx = integer(0), vals = character(0)))
    idx <- as.integer(sub(pfx, "", ids))
    ord <- order(idx)
    list(
      idx  = idx[ord],
      vals = vapply(ids[ord], function(id) as.character(input[[id]] %||% ""), character(1))
    )
  })
  names(side_ids) <- c("left", "center", "right")
  n <- max(vapply(side_ids, function(s) if (length(s$idx) == 0L) 0L else max(s$idx), integer(1)))
  if (n == 0L) return(list(left = character(0), center = character(0), right = character(0)))
  out <- list(left = character(n), center = character(n), right = character(n))
  for (side in c("left", "center", "right")) {
    s <- side_ids[[side]]
    for (k in seq_along(s$idx)) {
      out[[side]][[s$idx[[k]]]] <- s$vals[[k]]
    }
  }
  # Drop trailing all-empty rows.
  keep <- vapply(seq_len(n), function(i) {
    nzchar(out$left[[i]]) || nzchar(out$center[[i]]) || nzchar(out$right[[i]])
  }, logical(1))
  last <- if (any(keep)) max(which(keep)) else 0L
  if (last == 0L) return(list(left = character(0), center = character(0), right = character(0)))
  for (side in c("left", "center", "right")) {
    out[[side]] <- out[[side]][seq_len(last)]
  }
  out
}

# Rebuild `theme$populations` from the current pop_* inputs. Row indices
# come from the input names (`pop_id_<i>`, `pop_label_<i>`, ...); returned
# as a NAMED list keyed by the id-field value so downstream code can look
# a population up by id.
.collect_pops <- function(input) {
  id_ids <- grep("^pop_id_[0-9]+$", names(input), value = TRUE)
  if (length(id_ids) == 0L) return(list())
  idx <- as.integer(sub("pop_id_", "", id_ids))
  ord <- order(idx)
  out <- list()
  for (i in idx[ord]) {
    id <- as.character(input[[paste0("pop_id_", i)]] %||% "")
    if (!nzchar(id)) next
    out[[id]] <- list(
      label   = as.character(input[[paste0("pop_label_", i)]] %||% ""),
      dataset = as.character(input[[paste0("pop_dataset_", i)]] %||% ""),
      filter  = as.character(input[[paste0("pop_filter_", i)]] %||% "")
    )
  }
  out
}

# ---- section shell --------------------------------------------------------

.setup_section <- function(ns, id, title, glyph, body) {
  shiny::div(
    class = "ar-setup-section",
    `data-ar-section` = id,
    shiny::div(
      class = "ar-setup-sechead",
      shiny::span(class = "ar-setup-sechead-title", title),
      glyph
    ),
    shiny::div(class = "ar-setup-secbody", body)
  )
}

.section_status_glyph <- function(store, section) {
  status <- .section_status(store$rv$report@theme, section)
  cls <- switch(
    status$state,
    ok = "ar-setup-glyph ar-setup-glyph-ok",
    partial = "ar-setup-glyph ar-setup-glyph-partial",
    "ar-setup-glyph ar-setup-glyph-none"
  )
  text <- switch(
    status$state,
    ok = "✓",
    partial = as.character(status$missing),
    "•"
  )
  shiny::span(class = cls, text)
}

.section_status <- function(theme, section) {
  need <- switch(
    section,
    study = c("sponsor", "protocol", "study", "data_date"),
    data  = c("adam_dir", "pop_dataset", "subject_id", "pop_treatment_var"),
    populations = character(0),
    page = c("orientation", "paper"),
    summaries = character(0),
    character(0)
  )
  block <- theme[[section]] %||% list()
  if (section == "populations") {
    pops <- theme$populations %||% list()
    if (length(pops) == 0L) {
      return(list(state = "none", missing = 0L))
    }
    return(list(state = "ok", missing = 0L))
  }
  if (length(need) == 0L) {
    return(list(state = "none", missing = 0L))
  }
  present <- vapply(need, function(k) {
    v <- block[[k]]
    !is.null(v) && length(v) >= 1L && any(nzchar(as.character(v)))
  }, logical(1))
  n_missing <- sum(!present)
  if (n_missing == 0L) {
    list(state = "ok", missing = 0L)
  } else if (n_missing < length(need)) {
    list(state = "partial", missing = n_missing)
  } else {
    list(state = "none", missing = n_missing)
  }
}

# ---- Reviewed banner ------------------------------------------------------

.setup_reviewed_banner <- function(store) {
  meta <- store$rv$report@theme[["_meta"]] %||% list()
  reviewed <- isTRUE(meta$reviewed)
  cls <- if (reviewed) "ar-review-banner ar-review-banner-on" else "ar-review-banner"
  label <- if (reviewed) {
    sprintf(
      "Reviewed by %s · %s",
      meta$reviewed_by %||% "—",
      meta$reviewed_at %||% "—"
    )
  } else {
    "Draft"
  }
  shiny::div(
    class = cls,
    shiny::span(class = "ar-review-dot"),
    shiny::span(class = "ar-review-lbl", label)
  )
}

# ---- Study section --------------------------------------------------------

.setup_study <- function(ns, store) {
  s <- store$rv$report@theme$study %||% list()
  shiny::tagList(
    .setup_group(
      "Identity",
      shiny::div(
        class = "ar-setup-grid",
        .flat_input(ns, "study_sponsor", "Sponsor", s$sponsor %||% ""),
        .flat_input(ns, "study_protocol", "Protocol", s$protocol %||% ""),
        .flat_input(ns, "study_study", "Study id", s$study %||% ""),
        .flat_input(
          ns,
          "study_indication",
          "Indication (optional)",
          s$indication %||% ""
        )
      )
    ),
    .setup_group(
      "Extraction",
      shiny::div(
        class = "ar-setup-grid",
        .flat_input(
          ns,
          "study_data_date",
          "Data extraction date",
          s$data_date %||% "",
          placeholder = "YYYY-MM-DD"
        ),
        .seg_control(
          ns,
          "study_status",
          "Status",
          c("draft", "final"),
          s$status %||% "draft"
        )
      )
    )
  )
}

# ---- Data section --------------------------------------------------------

.setup_data <- function(ns, store) {
  d <- store$rv$report@theme$data %||% list()
  # Live-catalog probe: which datasets are mounted right now? Used to
  # populate the pop-dataset dropdown after import.
  cat <- tryCatch(arpillar::catalog_grid(store$con), error = function(e) NULL)
  ds_names <- if (is.null(cat) || nrow(cat) == 0L) character(0) else cat$name
  pop_dataset <- d$pop_dataset %||%
    (if ("ADSL" %in% ds_names) "ADSL" else ds_names[[1L]] %||% "")
  # Column vocabulary of the chosen population dataset -- feeds the
  # subject_id + treatment-var dropdowns.
  cols <- character(0)
  if (nzchar(pop_dataset) && !is.null(store$con)) {
    cols <- tryCatch(
      arpillar::data_items(store$con, pop_dataset)$name,
      error = function(e) character(0)
    )
  }
  # L-107 (Global TFL Reqs): subject IDs can stack -- USUBJID + SUBJID for
  # rollover / long-term extension studies. Store as a comma-separated list.
  subject_id <- d$subject_id %||%
    (if ("USUBJID" %in% cols) "USUBJID" else "")
  pop_arm <- d$pop_treatment_var %||%
    (if ("TRT01A" %in% cols) "TRT01A" else if ("TRT01P" %in% cols) "TRT01P" else "")
  bindings_ready <- length(ds_names) > 0L
  shiny::tagList(
    .setup_group(
      "Folder paths",
      shiny::div(
        class = "ar-setup-grid",
        shiny::div(
          class = "ar-setup-field",
          shiny::tags$label(class = "ar-label", "ADaM directory"),
          shiny::div(
            class = "ar-path-row",
            .picker_proxy(ns("data_adam_pick")),
            shiny::tags$input(
              id = ns("data_adam_dir"),
              class = "ar-input-flat ar-mono",
              type = "text",
              value = d$adam_dir %||% s_study(store)$adam_dir %||% "",
              placeholder = "/path/to/adam"
            )
          )
        ),
        shiny::div(
          class = "ar-setup-field",
          shiny::tags$label(class = "ar-label", "SDTM directory (optional)"),
          shiny::div(
            class = "ar-path-row",
            .picker_proxy(ns("data_sdtm_pick")),
            shiny::tags$input(
              id = ns("data_sdtm_dir"),
              class = "ar-input-flat ar-mono",
              type = "text",
              value = d$sdtm_dir %||% s_study(store)$sdtm_dir %||% "",
              placeholder = "/path/to/sdtm"
            )
          )
        )
      )
    ),
    .setup_group(
      "Bindings",
      if (!bindings_ready) {
        shiny::div(
          class = "ar-setup-empty",
          shiny::p("Pick an ADaM folder above and press Import; the bindings dropdowns will populate from the mounted catalog.")
        )
      } else {
        shiny::div(
          class = "ar-setup-grid",
          .select_input(
            ns, "data_pop_dataset", "Population dataset",
            ds_names, pop_dataset
          ),
          shiny::div(
            class = "ar-setup-field",
            shiny::tags$label(class = "ar-label", "Subject ID column(s)"),
            shiny::tags$input(
              id = ns("data_subject_id"),
              class = "ar-input-flat ar-mono",
              type = "text",
              value = subject_id,
              placeholder = "USUBJID, SUBJID"
            ),
            shiny::tags$small(
              class = "ar-muted",
              "One or more, comma-separated. L-107: stack USUBJID + SUBJID for rollover / extension studies."
            )
          ),
          .select_input(
            ns, "data_pop_treatment_var", "Population treatment variable",
            if (length(cols) == 0L) c("TRT01A") else cols,
            pop_arm
          )
        )
      }
    )
  )
}

s_study <- function(store) {
  store$rv$report@theme$study %||% list()
}

# Proxy button placed inside a dynamic renderUI that click-dispatches to a
# real shinyDirButton kept permanently in the static UI (`.ar-setup-pickers`).
# shinyFiles' JS binding is anchored to the real button and survives every
# renderUI re-fire because the real button never moves.
.picker_proxy <- function(target_id) {
  shiny::tags$button(
    type = "button",
    class = "ar-dx-tb ar-picker-proxy",
    onclick = sprintf(
      "var t=document.getElementById('%s'); if(t) t.click();",
      target_id
    ),
    .icon("folder_plus", 13),
    " Browse"
  )
}

.select_input <- function(ns, id, label, choices, selected) {
  shiny::div(
    class = "ar-setup-field",
    shiny::tags$label(class = "ar-label", `for` = ns(id), label),
    shiny::tags$select(
      id = ns(id),
      class = "ar-input-flat",
      lapply(choices, function(v) {
        shiny::tags$option(
          value = v,
          selected = if (identical(v, selected)) "selected" else NULL,
          v
        )
      })
    )
  )
}

# ---- Populations section --------------------------------------------------

# CDISC canonical analysis-set seeds. Rendered as editable rows when the
# theme's populations library is empty so users see the shape without
# hand-writing setup.yml. Once a user edits or adds, the theme takes over.
.POP_SEEDS <- list(
  safety   = list(label = "Safety Analysis Set",     dataset = "ADSL", filter = 'SAFFL == "Y"'),
  efficacy = list(label = "Full Analysis Set (FAS)", dataset = "ADSL", filter = 'FASFL == "Y"'),
  pp       = list(label = "Per-Protocol Set",        dataset = "ADSL", filter = ""),
  pk       = list(label = "PK Analysis Set",         dataset = "ADSL", filter = "")
)

.setup_populations <- function(ns, store) {
  pops <- store$rv$report@theme$populations %||% list()
  if (length(pops) == 0L) pops <- .POP_SEEDS
  default <- store$rv$report@theme$default_population %||% "safety"
  ids <- names(pops)
  header <- shiny::div(
    class = "ar-setup-pop-header",
    shiny::span(class = "ar-mono", "ID"),
    shiny::span("LABEL"),
    shiny::span("DATASET"),
    shiny::span("FILTER"),
    shiny::span("DEFAULT"),
    shiny::span("")
  )
  rows <- lapply(seq_along(pops), function(i) {
    id <- ids[[i]]
    p <- pops[[id]]
    is_default <- identical(default, id)
    shiny::div(
      class = paste(
        "ar-setup-pop-row",
        if (is_default) "ar-setup-pop-default" else ""
      ),
      `data-ar-pop-id` = id,
      .flat_input(ns, paste0("pop_id_", i), NULL, id, mono = TRUE),
      .flat_input(ns, paste0("pop_label_", i), NULL, p$label %||% ""),
      .flat_input(ns, paste0("pop_dataset_", i), NULL, p$dataset %||% "ADSL", mono = TRUE),
      .flat_input(ns, paste0("pop_filter_", i), NULL, p$filter %||% "", mono = TRUE),
      shiny::tags$button(
        type = "button",
        class = paste(
          "ar-pop-default-btn",
          if (is_default) "ar-pop-default-on" else ""
        ),
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
          ns("top_default_population"),
          id
        ),
        if (is_default) "★" else "☆"
      ),
      shiny::tags$button(
        type = "button",
        class = "ar-pop-delete",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
          ns("pop_delete"),
          id
        ),
        title = "Delete",
        "×"
      )
    )
  })
  shiny::tagList(
    .setup_group(
      "Library",
      shiny::tagList(
        shiny::div(class = "ar-setup-pops", header, rows),
        shiny::tags$button(
          id = ns("pop_add"),
          type = "button",
          class = "ar-pop-add action-button",
          "+ Add analysis set"
        ),
        shiny::p(
          class = "ar-muted ar-mono",
          shiny::HTML("Refer to a population in a running header/footer as <code>{analysis-set}</code>, or bind it per output on the Roles tab.")
        )
      )
    )
  )
}

# ---- Page section ---------------------------------------------------------

.setup_page_body <- function(ns, store) {
  p <- store$rv$report@theme$page %||% list()
  pagehead <- p$pagehead %||% c("{sponsor} - {protocol}")
  pagefoot <- p$pagefoot %||% c("{data_date}", "Page {page} of {npages}")
  if (is.list(pagehead)) pagehead <- unlist(pagehead)
  if (is.list(pagefoot)) pagefoot <- unlist(pagefoot)
  shiny::tagList(
    .setup_group(
      "Geometry",
      shiny::div(
        class = "ar-setup-grid",
        .seg_control(
          ns,
          "page_orientation",
          "Orientation",
          c("portrait", "landscape"),
          p$orientation %||% "landscape"
        ),
        .seg_control(
          ns,
          "page_paper",
          "Paper",
          c("letter", "a4"),
          p$paper %||% "letter"
        ),
        .seg_control(
          ns,
          "page_font_family",
          "Font",
          c("mono", "sans", "serif"),
          p$font_family %||% "mono"
        ),
        .flat_input(
          ns,
          "page_font_size",
          "Font size",
          as.character(p$font_size %||% 10L)
        )
      )
    ),
    .setup_group(
      "Header rows",
      .band_rows(ns, "pagehead", p$pagehead)
    ),
    .setup_group(
      "Footer rows",
      .band_rows(ns, "pagefoot", p$pagefoot)
    )
  )
}

# Render `key` as a stack of {left, center, right} rows. Reads the current
# theme block if it exists, else falls back to a single seeded row.
.band_rows <- function(ns, key, band) {
  seed <- if (identical(key, "pagehead")) {
    list(left = "", center = "{sponsor} - {protocol}", right = "")
  } else {
    list(
      left   = "{data_date}",
      center = "",
      right  = "Page {page} of {npages}"
    )
  }
  # Normalize band to list(left = <chr>, center = <chr>, right = <chr>).
  rows <- .band_to_rows(band, seed)
  shiny::div(
    class = "ar-band-lines",
    lapply(seq_along(rows), function(i) {
      r <- rows[[i]]
      shiny::div(
        class = "ar-band-row",
        .flat_input(
          ns, paste0("page_", key, "_left_", i), NULL,
          r$left %||% "", mono = TRUE, placeholder = "Left"
        ),
        .flat_input(
          ns, paste0("page_", key, "_center_", i), NULL,
          r$center %||% "", mono = TRUE, placeholder = "Center"
        ),
        .flat_input(
          ns, paste0("page_", key, "_right_", i), NULL,
          r$right %||% "", mono = TRUE, placeholder = "Right"
        ),
        shiny::tags$button(
          type = "button",
          class = "ar-pop-delete",
          onclick = sprintf(
            "Shiny.setInputValue('%s', %d, {priority: 'event'})",
            ns(paste0("band_", key, "_delete")),
            i
          ),
          title = "Delete row",
          "×"
        )
      )
    }),
    shiny::tags$button(
      id = ns(paste0("band_", key, "_add")),
      type = "button",
      class = "ar-pop-add action-button",
      "+ Add row"
    )
  )
}

# Flip `list(left = c(l1,l2), center = c(c1,c2), right = c(r1,r2))` into
# `list(list(left=l1,center=c1,right=r1), list(left=l2,center=c2,right=r2))`.
# Missing sides fill with "". Empty band -> one row of the seed.
.band_to_rows <- function(band, seed) {
  if (is.null(band) || (is.list(band) && length(band) == 0L)) {
    return(list(seed))
  }
  # Accept a legacy plain character vector: treat as center column only.
  if (is.character(band)) {
    return(lapply(band, function(x) list(left = "", center = x, right = "")))
  }
  n <- max(
    length(band$left %||% character(0)),
    length(band$center %||% character(0)),
    length(band$right %||% character(0)),
    1L
  )
  lapply(seq_len(n), function(i) {
    list(
      left   = (band$left   %||% character(0))[i] %||% "",
      center = (band$center %||% character(0))[i] %||% "",
      right  = (band$right  %||% character(0))[i] %||% ""
    )
  })
}

# ---- Summaries section ----------------------------------------------------

.setup_summaries <- function(ns, store) {
  s <- store$rv$report@theme$summaries %||% list()
  d <- store$rv$report@theme$decimals %||% list()
  # Canonical seed for the Explorer continuous-rows editor. Each row: label
  # + list of stat atoms + a format string ("a", "a (b)", "a - b", ...).
  rows <- s$continuous
  if (is.null(rows) || length(rows) == 0L) {
    rows <- .CONT_SEEDS
  }
  # Precision defaults per Global TFL Requirements (T-103):
  #   * Min / Max          = raw data precision (integer default = 0)
  #   * Mean / Median      = base + 1
  #   * SD                 = base + 2
  #   * % (frequency)      = 1 dp (T-101)
  #   * N                  = 0 (integer)
  # With a base of 0 (integer raw data, typical for AGE / event counts):
  #   N=0, %=1, Mean=1, SD=2, Median=1, Min=0, Max=0.
  prec_defaults <- list(
    n = 0L, pct = 1L, mean = 1L, sd = 2L,
    median = 1L, min = 0L, max = 0L
  )
  prec_labels <- list(
    n       = "N",
    pct     = "%",
    mean    = "Mean",
    sd      = "SD",
    median  = "Median",
    min     = "Min",
    max     = "Max"
  )
  shiny::tagList(
    .setup_group(
      "Continuous rows",
      shiny::div(
        class = "ar-cont-rows",
        lapply(seq_along(rows), function(i) {
          row <- rows[[i]]
          .cont_row(ns, i, row)
        }),
        shiny::tags$button(
          id = ns("cont_add"),
          type = "button",
          class = "ar-pop-add action-button",
          "+ Add statistic row"
        )
      )
    ),
    .setup_group(
      "Categorical rules",
      shiny::div(
        class = "ar-setup-grid",
        .seg_control(
          ns, "cat_header_stat", "Header stat",
          c("n", "total_n", "none"),
          s$categorical$header_stat %||% "n"
        ),
        .seg_control(
          ns, "cat_level_format", "Level format",
          c("n", "pct", "n_pct", "pct_n"),
          s$categorical$level_format %||% "n_pct"
        ),
        .seg_control(
          ns, "cat_show_missing", "Show missing",
          c("auto", "always", "never"),
          s$categorical$show_missing %||% "auto"
        ),
        .flat_input(
          ns, "cat_missing_label", "Missing label",
          s$categorical$missing_label %||% "Missing"
        )
      )
    ),
    .setup_group(
      "Arm column headers",
      shiny::div(
        class = "ar-setup-grid",
        .seg_control(
          ns, "arm_show_header_n", "Show N per arm",
          c("yes", "no"),
          if (isFALSE(store$rv$report@theme$arm$show_header_n)) "no" else "yes"
        ),
        .flat_input(
          ns, "arm_header_n_format", "N format",
          store$rv$report@theme$arm$header_n_format %||% "(N={n})",
          mono = TRUE,
          placeholder = "(N={n})"
        )
      )
    ),
    .setup_group(
      "Precision",
      shiny::div(
        class = "ar-setup-grid ar-setup-precision",
        lapply(
          names(prec_defaults),
          function(k) {
            .flat_input(
              ns,
              paste0("decimals_", k),
              prec_labels[[k]],
              as.character(d[[k]] %||% prec_defaults[[k]]),
              placeholder = as.character(prec_defaults[[k]])
            )
          }
        )
      )
    )
  )
}

# Canonical continuous stat-row seeds. Each row IS the rendered line: a
# label, an ordered list of stat atoms, and a format string keyed by
# positional letters (a, b, c, ...).
.CONT_SEEDS <- list(
  list(label = "n",        stats = "n",           format = "a"),
  list(label = "Mean (SD)", stats = c("mean", "sd"), format = "a (b)"),
  list(label = "Median",   stats = "median",      format = "a"),
  list(label = "Min - Max", stats = c("min", "max"), format = "a - b")
)

.cont_row <- function(ns, i, row) {
  stats <- if (is.null(row$stats)) character(0) else row$stats
  shiny::div(
    class = "ar-cont-row",
    shiny::span(class = "ar-cont-grip", title = "Drag to reorder", "⋮⋮"),
    .flat_input(
      ns, paste0("cont_label_", i), NULL,
      row$label %||% "", placeholder = "Row label"
    ),
    shiny::div(
      class = "ar-cont-atoms",
      lapply(stats, function(st) {
        shiny::tags$span(class = "ar-cont-atom ar-mono", st)
      }),
      if (length(stats) < 2L) NULL else shiny::tags$span(
        class = "ar-cont-fmt-badge ar-mono",
        row$format %||% "a"
      )
    ),
    .flat_input(
      ns, paste0("cont_format_", i), NULL,
      row$format %||% "a", mono = TRUE, placeholder = "a (b)"
    ),
    shiny::tags$button(
      type = "button",
      class = "ar-pop-delete",
      onclick = sprintf(
        "Shiny.setInputValue('%s', %d, {priority: 'event'})",
        ns("cont_delete"),
        i
      ),
      title = "Delete row",
      "×"
    )
  )
}

# ---- shared atoms ---------------------------------------------------------

.setup_group <- function(label, body) {
  shiny::div(
    class = "ar-setup-group",
    shiny::div(class = "ar-setup-group-lbl ar-mono", toupper(label)),
    body
  )
}

.flat_input <- function(ns, id, label, value = "", placeholder = NULL, mono = FALSE) {
  cls <- if (mono) "ar-input-flat ar-mono" else "ar-input-flat"
  shiny::div(
    class = "ar-setup-field",
    shiny::tags$label(
      class = "ar-label",
      `for` = ns(id),
      label
    ),
    shiny::tags$input(
      id = ns(id),
      class = cls,
      type = "text",
      value = value,
      placeholder = placeholder %||% ""
    )
  )
}

.seg_control <- function(ns, id, label, choices, selected) {
  input_id <- ns(id)
  # Inline click handler flips the active class on all siblings and pushes
  # the selected value to the Shiny input via `Shiny.setInputValue`. No
  # bridge.js dependency; works even before the Preact bundle lands.
  click_js <- sprintf(
    "(function(btn){var sibs=btn.parentElement.querySelectorAll('.ar-seg-opt');for(var i=0;i<sibs.length;i++)sibs[i].classList.remove('ar-seg-opt-active');btn.classList.add('ar-seg-opt-active');Shiny.setInputValue('%s', btn.dataset.arSegValue, {priority: 'event'});})(this)",
    input_id
  )
  shiny::div(
    class = "ar-setup-field",
    shiny::tags$label(class = "ar-label", label),
    shiny::div(
      class = "ar-seg",
      lapply(choices, function(ch) {
        shiny::tags$button(
          type = "button",
          class = paste(
            "ar-seg-opt",
            if (identical(ch, selected)) "ar-seg-opt-active" else ""
          ),
          `data-ar-seg-value` = ch,
          onclick = click_js,
          ch
        )
      })
    ),
    # Initial value carrier so the server observer sees the current
    # selection on first render (before any click). `outputArgs` isn't
    # available for a bare hidden input, so we seed via a JS one-shot.
    shiny::tags$script(sprintf(
      "Shiny.setInputValue('%s', %s, {priority: 'event'});",
      input_id,
      jsonlite::toJSON(selected, auto_unbox = TRUE)
    ))
  )
}

# ---- binding helpers ------------------------------------------------------

.bind_theme_field <- function(input, store, block, key) {
  input_id <- paste0(block, "_", key)
  shiny::observeEvent(
    input[[input_id]],
    {
      val <- input[[input_id]]
      r <- store$rv$report
      theme <- r@theme
      if (is.null(theme[[block]])) theme[[block]] <- list()
      if (identical(theme[[block]][[key]], val)) return()
      theme[[block]][[key]] <- val
      commit(store, S7::set_props(r, theme = theme), label = "edit setup")
    },
    ignoreInit = TRUE
  )
}

.bind_theme_top <- function(input, store, key) {
  input_id <- paste0("top_", key)
  shiny::observeEvent(
    input[[input_id]],
    {
      val <- input[[input_id]]
      r <- store$rv$report
      theme <- r@theme
      if (identical(theme[[key]], val)) return()
      theme[[key]] <- val
      commit(store, S7::set_props(r, theme = theme), label = "edit setup")
    },
    ignoreInit = TRUE
  )
}
