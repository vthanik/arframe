# Setup mode: SIX sections on ONE scrollable page.
#   1. Study         (Identity + Extraction)
#   2. Paths         (data + programs + output + logs) - the ONE section
#                    where every filesystem pointer lives, folded up from
#                    Data + Preferences+Paths + Sources.
#   3. Treatment     (trtvar / trtvarn + arm decode grid)
#   4. Populations   (ADaM-flag library)
#   5. Page & Style  (Geometry + Header/footer bands + Footnote register)
#   6. Summaries     (Continuous rows + Categorical rules + Precision)
#   7. Team          (roster + activity + presence)
#
# Design (Stage 1, 2026-07-06 rebuild):
#   * `.SETUP_SPEC` is the single declarative registry of every scalar
#     value input the module owns. `.wire_all()` installs one observer
#     per entry -- dead bindings become structurally impossible. Adding
#     a new field means one row in the spec + a renderer placement at
#     the declared id.
#   * Structural mutations (add-row / delete-row, folder pickers) stay
#     hand-wired -- their semantics are diverse enough that a
#     declarative registry buys nothing.
#   * The store's autosave observer already writes `setup.yml` on every
#     commit; every entry here rides that path.

# Precision fields render as free-form text; only accept non-negative
# integers, otherwise drop the edit silently.
.as_prec <- function(v) {
  iv <- suppressWarnings(as.integer(v))
  if (length(iv) != 1L || is.na(iv) || iv < 0L) NULL else iv
}

# Declarative registry of every SCALAR setup input.
#   id     : Shiny input id (namespaced via `session$ns` at render time).
#   path   : character() path into `report@theme` (nested via `[[`).
#   coerce : optional; identity by default. Runs before the write.
# See `.wire_all()` below.
.SETUP_SPEC <- list(
  # ---- Study --------------------------------------------------------------
  list(id = "study_sponsor", path = c("study", "sponsor")),
  list(id = "study_protocol", path = c("study", "protocol")),
  list(id = "study_study", path = c("study", "study")),
  list(id = "study_indication", path = c("study", "indication")),
  list(id = "study_data_date", path = c("study", "data_date")),
  list(id = "study_status", path = c("study", "status")),
  # ---- Data (ADaM/SDTM + population bindings) ----------------------------
  list(id = "data_adam_dir", path = c("data", "adam_dir")),
  list(id = "data_sdtm_dir", path = c("data", "sdtm_dir")),
  list(id = "data_pop_dataset", path = c("data", "pop_dataset")),
  list(id = "data_subject_id", path = c("data", "subject_id")),
  list(id = "data_pop_treatment_var", path = c("data", "pop_treatment_var")),
  # ---- Treatment (Stage 2) ----------------------------------------------
  list(id = "treatment_trtvar", path = c("treatment", "trtvar")),
  list(id = "treatment_trtvarn", path = c("treatment", "trtvarn")),
  # ---- Paths / Report conventions ---------------------------------------
  list(
    id = "preferences_numbering_scheme",
    path = c("preferences", "numbering_scheme")
  ),
  list(
    id = "preferences_sponsor_style",
    path = c("preferences", "sponsor_style")
  ),
  list(id = "paths_programs_dir", path = c("paths", "programs_dir")),
  list(id = "paths_output_rtf_dir", path = c("paths", "output_rtf_dir")),
  list(id = "paths_datasets_dir", path = c("paths", "datasets_dir")),
  list(id = "paths_logs_dir", path = c("paths", "logs_dir")),
  # ---- Page geometry -----------------------------------------------------
  list(id = "page_orientation", path = c("page", "orientation")),
  list(id = "page_paper", path = c("page", "paper")),
  list(id = "page_font_family", path = c("page", "font_family")),
  list(
    id = "page_font_size",
    path = c("page", "font_size"),
    coerce = function(v) {
      iv <- suppressWarnings(as.integer(v))
      if (length(iv) != 1L || is.na(iv)) NULL else iv
    }
  ),
  # ---- Arm column headers -----------------------------------------------
  list(
    id = "arm_show_header_n",
    path = c("arm", "show_header_n"),
    coerce = function(v) identical(v, "yes")
  ),
  list(id = "arm_header_n_format", path = c("arm", "header_n_format")),
  # ---- Summaries: categorical rules (previously dead) --------------------
  list(
    id = "cat_header_stat",
    path = c("summaries", "categorical", "header_stat")
  ),
  list(
    id = "cat_level_format",
    path = c("summaries", "categorical", "level_format")
  ),
  list(
    id = "cat_show_missing",
    path = c("summaries", "categorical", "show_missing")
  ),
  list(
    id = "cat_missing_label",
    path = c("summaries", "categorical", "missing_label")
  ),
  # ---- Summaries: precision (previously dead) ----------------------------
  list(id = "decimals_n", path = c("decimals", "n"), coerce = .as_prec),
  list(id = "decimals_pct", path = c("decimals", "pct"), coerce = .as_prec),
  list(id = "decimals_mean", path = c("decimals", "mean"), coerce = .as_prec),
  list(id = "decimals_sd", path = c("decimals", "sd"), coerce = .as_prec),
  list(
    id = "decimals_median",
    path = c("decimals", "median"),
    coerce = .as_prec
  ),
  list(id = "decimals_min", path = c("decimals", "min"), coerce = .as_prec),
  list(id = "decimals_max", path = c("decimals", "max"), coerce = .as_prec),
  # ---- Top-level selectors -----------------------------------------------
  list(id = "top_default_population", path = "default_population")
)

#' @noRd
mod_setup_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "ar-setup",
    # `<datalist>` for continuous-stats-row atom autocomplete. Lives
    # in the static UI so a renderUI re-fire never orphans it. The id
    # is not namespaced -- HTML `<input list="...">` looks up by the
    # id in the same document, and datalists have no server-side
    # state.
    shiny::tags$datalist(
      id = "ar-stat-atoms",
      lapply(.CONT_ATOMS, function(a) shiny::tags$option(value = a))
    ),
    # shinyFiles binds its client-side handler at UI-build time to buttons
    # that already exist in the DOM -- a button rendered later inside a
    # renderUI never gets the binding. Instantiate the two pickers here in
    # the static UI; CSS teleports them into the Data section headers
    # (see `.ar-setup [data-ar-adam-pick]` / `[data-ar-sdtm-pick]`).
    shiny::div(
      class = "ar-setup-pickers",
      `aria-hidden` = "true",
      shinyFiles::shinyDirButton(
        ns("data_adam_pick"),
        label = "Browse",
        title = "Select a folder of data files",
        icon = .icon("folder_plus", 13),
        class = "btn btn-outline-secondary ex-btn-sm ar-adam-pick"
      ),
      shinyFiles::shinyDirButton(
        ns("data_sdtm_pick"),
        label = "Browse",
        title = "Select a folder of data files",
        icon = .icon("folder_plus", 13),
        class = "btn btn-outline-secondary ex-btn-sm ar-sdtm-pick"
      ),
      # Sources section (Stage 3): a proxy button in the dynamic renderUI
      # click-dispatches to this static one, so shinyFiles' UI-build-time
      # binding still fires.
      shinyFiles::shinyDirButton(
        ns("sources_pick"),
        label = "Add folder",
        title = "Add a data folder to the project",
        icon = .icon("folder_plus", 13),
        class = "btn btn-outline-secondary ex-btn-sm ar-sources-pick"
      )
    ),
    shiny::uiOutput(ns("page"))
  )
}

#' @noRd
mod_setup_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The visible section. Server-authoritative so a commit-driven
    # re-render of `output$page` never bounces the user back to the first
    # tab: the client swaps the class instantly for feel, and posts here so
    # the next render restamps the same tab.
    active_tab <- shiny::reactiveVal("study")
    shiny::observeEvent(input$setup_tab, {
      if (is.character(input$setup_tab) && nzchar(input$setup_tab)) {
        active_tab(input$setup_tab)
      }
    })

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
        commit(
          store,
          S7::set_props(r, theme = theme),
          label = "seed populations"
        )
      }
    })

    output$page <- shiny::renderUI({
      # React to catalog + whole-report edits so completion glyphs stay
      # live. Seven pharma-aligned sections; Sources / Data / Preferences
      # folded into Paths.
      store$rv$report
      store$rv$catalog_nonce
      # `isolate()` -- a tab click posts `input$setup_tab` and must NOT
      # itself trigger this render (the client already swapped the
      # visible section instantly). Only the commit-driven dependencies
      # above re-fire this block; when they do, the wrapper is restamped
      # with whatever tab is current at that moment.
      active <- shiny::isolate(active_tab())
      sections <- list(
        list(id = "study", title = "Study", body = .setup_study(ns, store)),
        list(id = "paths", title = "Paths", body = .setup_paths(ns, store)),
        list(
          id = "treatment",
          title = "Treatment",
          body = .setup_treatment(ns, store)
        ),
        list(
          id = "populations",
          title = "Populations",
          body = .setup_populations(ns, store)
        ),
        list(
          id = "page",
          title = "Page & Style",
          body = .setup_page_body(ns, store)
        ),
        list(
          id = "summaries",
          title = "Summaries",
          body = .setup_summaries(ns, store)
        ),
        list(id = "team", title = "Team", body = .setup_team(ns, store))
      )
      shiny::div(
        class = paste0("ar-setup-dash ar-setup-tab-", active),
        .setup_overview(store, sections),
        .setup_reviewed_banner(store),
        .setup_tabstrip(ns, store, sections, active),
        lapply(sections, function(s) {
          .setup_section(ns, s$id, s$title, s$body)
        })
      )
    })
    shiny::outputOptions(output, "page", suspendWhenHidden = FALSE)

    # Every scalar field is wired via the declarative `.SETUP_SPEC`. See
    # top of file. Structural mutations (add/delete rows, folder pickers)
    # stay hand-wired below.
    .wire_all(input, store)

    # ADaM / SDTM / Sources folder pickers: shinyDirChoose delegates to a
    # modal; on a picked path, update the text field AND fire mount_folder
    # so the catalog populates immediately (bindings dropdowns light up).
    volumes <- c(home = path.expand("~"), root = "/")
    shinyFiles::shinyDirChoose(input, "data_adam_pick", roots = volumes)
    shinyFiles::shinyDirChoose(input, "data_sdtm_pick", roots = volumes)
    shinyFiles::shinyDirChoose(input, "sources_pick", roots = volumes)

    # Setup > Sources add-folder button: no visible field to update -- the
    # newly-mounted folder shows up as a row in the Sources list surface
    # via the catalog_nonce bump.
    shiny::observeEvent(input$sources_pick, {
      dir <- shinyFiles::parseDirPath(volumes, input$sources_pick)
      if (length(dir) != 1L || !nzchar(dir)) {
        return()
      }
      tryCatch(
        .mount_folder(store, dir),
        error = function(e) {
          log_line(
            store,
            sprintf("mount sources failed: %s", conditionMessage(e))
          )
        }
      )
    })

    lapply(c("adam", "sdtm"), function(kind) {
      shiny::observeEvent(input[[paste0("data_", kind, "_pick")]], {
        dir <- shinyFiles::parseDirPath(
          volumes,
          input[[paste0("data_", kind, "_pick")]]
        )
        if (length(dir) != 1L || !nzchar(dir)) {
          return()
        }
        # Push path into the visible field.
        shiny::updateTextInput(
          session,
          paste0("data_", kind, "_dir"),
          value = dir
        )
        # Mount the folder into the catalog. `.mount_folder` is defined in
        # mod_data.R; call via ::: since we're a sibling module.
        tryCatch(
          .mount_folder(store, dir),
          error = function(e) {
            log_line(
              store,
              sprintf("mount %s failed: %s", kind, conditionMessage(e))
            )
          }
        )
      })
    })

    # Populations library observer: on any pop_* field edit, rebuild
    # `theme$populations` from the current inputs (ordered by row index).
    # Count-mismatch guard: skip mid-flush when the input map lags the
    # theme by one flush cycle (e.g. right after pop_add / pop_delete),
    # otherwise we clobber the just-added or -removed row.
    shiny::observe({
      pops <- .collect_pops(input)
      if (length(pops) == 0L) {
        return()
      }
      r <- store$rv$report
      theme <- r@theme
      prior <- theme$populations %||% list()
      if (length(prior) > 0L && length(pops) != length(prior)) {
        return()
      }
      if (identical(theme$populations, pops)) {
        return()
      }
      theme$populations <- pops
      commit(store, S7::set_props(r, theme = theme), label = "edit populations")
    })

    # Add / delete populations events
    shiny::observeEvent(input$pop_add, {
      r <- store$rv$report
      theme <- r@theme
      pops <- theme$populations %||% list()
      new_id <- paste0("pop", length(pops) + 1L)
      pops[[new_id]] <- list(
        label = "New population",
        dataset = "ADSL",
        filter = ""
      )
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
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = "delete population"
      )
    })

    # Treatment arm library: same shape as populations. Rebuild `arms`
    # from arm_row_* inputs; separate add / delete events for structural
    # mutations.
    shiny::observe({
      arms <- .collect_arms(input)
      if (length(arms) == 0L) {
        return()
      }
      r <- store$rv$report
      theme <- r@theme
      if (is.null(theme$treatment)) {
        theme$treatment <- list()
      }
      prior <- theme$treatment$arms %||% list()
      # Skip mid-flush after arm_add / arm_delete.
      if (length(prior) > 0L && length(arms) != length(prior)) {
        return()
      }
      if (identical(theme$treatment$arms, arms)) {
        return()
      }
      theme$treatment$arms <- arms
      commit(store, S7::set_props(r, theme = theme), label = "edit arms")
    })
    shiny::observeEvent(input$arm_add, {
      r <- store$rv$report
      theme <- r@theme
      if (is.null(theme$treatment)) {
        theme$treatment <- list()
      }
      arms <- theme$treatment$arms %||% .ARM_SEEDS
      arms <- c(
        arms,
        list(list(level_n = length(arms) + 1L, label = ""))
      )
      theme$treatment$arms <- arms
      commit(store, S7::set_props(r, theme = theme), label = "add arm")
    })
    shiny::observeEvent(input$arm_delete, {
      i <- as.integer(input$arm_delete)
      r <- store$rv$report
      theme <- r@theme
      arms <- theme$treatment$arms %||% list()
      if (i >= 1L && i <= length(arms)) {
        arms <- arms[-i]
      }
      theme$treatment$arms <- arms
      commit(store, S7::set_props(r, theme = theme), label = "delete arm")
    })

    # Footnote register: same shape as populations / arms. Rebuild
    # `theme$footnotes` from foot_key_* + foot_text_* inputs; add + delete
    # events for structural mutations.
    shiny::observe({
      foots <- .collect_foots(input)
      r <- store$rv$report
      theme <- r@theme
      prior <- theme$footnotes %||% list()
      # Skip mid-flush after foot_add / foot_delete.
      if (length(prior) > 0L && length(foots) != length(prior)) {
        return()
      }
      if (identical(theme$footnotes, foots)) {
        return()
      }
      theme$footnotes <- foots
      commit(store, S7::set_props(r, theme = theme), label = "edit footnotes")
    })
    shiny::observeEvent(input$foot_add, {
      r <- store$rv$report
      theme <- r@theme
      reg <- theme$footnotes %||% list()
      i <- length(reg) + 1L
      new_key <- paste0("FN", i)
      while (new_key %in% names(reg)) {
        i <- i + 1L
        new_key <- paste0("FN", i)
      }
      reg[[new_key]] <- ""
      theme$footnotes <- reg
      commit(store, S7::set_props(r, theme = theme), label = "add footnote")
    })
    shiny::observeEvent(input$foot_delete, {
      i <- as.integer(input$foot_delete)
      r <- store$rv$report
      theme <- r@theme
      reg <- theme$footnotes %||% list()
      if (i >= 1L && i <= length(reg)) {
        reg[[i]] <- NULL
      }
      theme$footnotes <- reg
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = "delete footnote"
      )
    })

    # Continuous statistic rows: same shape as populations. Rebuild from
    # cont_label_* + cont_format_* + the row-level atoms; add + delete
    # events for structural mutations. The atoms per row are display-
    # only (they render as chips derived from stats), so the observer
    # only tracks label + format.
    shiny::observe({
      collected <- .collect_conts(input)
      if (length(collected) == 0L) {
        return()
      }
      r <- store$rv$report
      theme <- r@theme
      if (is.null(theme$summaries)) {
        theme$summaries <- list()
      }
      prior <- theme$summaries$continuous %||% .CONT_SEEDS
      # Skip mid-flush: the input map lags the theme by one flush cycle
      # right after a structural change (add / delete row). Rewriting
      # here would clobber the just-appended or -removed row.
      if (length(collected) != length(prior)) {
        return()
      }
      # All three of label / stats / format now come from user input --
      # `.collect_conts` parses the comma-separated stats field into a
      # character vector.
      rows <- collected
      if (identical(theme$summaries$continuous, rows)) {
        return()
      }
      theme$summaries$continuous <- rows
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = "edit continuous rows"
      )
    })
    shiny::observeEvent(input$cont_add, {
      r <- store$rv$report
      theme <- r@theme
      if (is.null(theme$summaries)) {
        theme$summaries <- list()
      }
      rows <- theme$summaries$continuous %||% .CONT_SEEDS
      # Append a blank row using the same shape .CONT_SEEDS uses.
      rows <- c(
        rows,
        list(list(label = "", stats = character(0), format = "a"))
      )
      theme$summaries$continuous <- rows
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = "add continuous row"
      )
    })
    shiny::observeEvent(input$cont_delete, {
      i <- as.integer(input$cont_delete)
      r <- store$rv$report
      theme <- r@theme
      rows <- theme$summaries$continuous %||% .CONT_SEEDS
      if (i >= 1L && i <= length(rows)) {
        rows <- rows[-i]
      }
      theme$summaries$continuous <- rows
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = "delete continuous row"
      )
    })

    # Running header/footer: each row is 3 inputs (left/center/right).
    # The observer packs them into `list(left = <chr>, center = <chr>,
    # right = <chr>)` -- the shape tabular's page-band consumer expects.
    lapply(c("pagehead", "pagefoot"), function(key) {
      shiny::observe({
        band <- .collect_band_rows(input, key)
        r <- store$rv$report
        theme <- r@theme
        if (is.null(theme$page)) {
          theme$page <- list()
        }
        # Skip mid-flush after band_*_add / band_*_delete: the band shape
        # is `list(left = <chr>, center = <chr>, right = <chr>)`, so the
        # row count is the length of any side vector.
        prior <- theme$page[[key]]
        prior_n <- if (is.list(prior)) {
          length(prior$left %||% character(0))
        } else {
          0L
        }
        band_n <- length(band$left %||% character(0))
        if (prior_n > 0L && band_n != prior_n) {
          return()
        }
        if (identical(theme$page[[key]], band)) {
          return()
        }
        theme$page[[key]] <- band
        commit(store, S7::set_props(r, theme = theme), label = "edit setup")
      })
    })

    # Add / delete a band row.
    lapply(c("pagehead", "pagefoot"), function(key) {
      shiny::observeEvent(input[[paste0("band_", key, "_add")]], {
        r <- store$rv$report
        theme <- r@theme
        if (is.null(theme$page)) {
          theme$page <- list()
        }
        band <- theme$page[[key]] %||%
          list(left = character(0), center = character(0), right = character(0))
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
        if (is.null(band)) {
          return()
        }
        for (side in c("left", "center", "right")) {
          v <- band[[side]] %||% character(0)
          if (i >= 1L && i <= length(v)) {
            v <- v[-i]
          }
          band[[side]] <- v
        }
        theme$page[[key]] <- band
        commit(
          store,
          S7::set_props(r, theme = theme),
          label = "delete band row"
        )
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
    if (length(ids) == 0L) {
      return(list(idx = integer(0), vals = character(0)))
    }
    idx <- as.integer(sub(pfx, "", ids))
    ord <- order(idx)
    list(
      idx = idx[ord],
      vals = vapply(
        ids[ord],
        function(id) as.character(input[[id]] %||% ""),
        character(1)
      )
    )
  })
  names(side_ids) <- c("left", "center", "right")
  n <- max(vapply(
    side_ids,
    function(s) if (length(s$idx) == 0L) 0L else max(s$idx),
    integer(1)
  ))
  if (n == 0L) {
    return(list(
      left = character(0),
      center = character(0),
      right = character(0)
    ))
  }
  out <- list(left = character(n), center = character(n), right = character(n))
  for (side in c("left", "center", "right")) {
    s <- side_ids[[side]]
    for (k in seq_along(s$idx)) {
      out[[side]][[s$idx[[k]]]] <- s$vals[[k]]
    }
  }
  # Drop trailing all-empty rows.
  keep <- vapply(
    seq_len(n),
    function(i) {
      nzchar(out$left[[i]]) || nzchar(out$center[[i]]) || nzchar(out$right[[i]])
    },
    logical(1)
  )
  last <- if (any(keep)) max(which(keep)) else 0L
  if (last == 0L) {
    return(list(
      left = character(0),
      center = character(0),
      right = character(0)
    ))
  }
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
  if (length(id_ids) == 0L) {
    return(list())
  }
  idx <- as.integer(sub("pop_id_", "", id_ids))
  ord <- order(idx)
  out <- list()
  for (i in idx[ord]) {
    id <- as.character(input[[paste0("pop_id_", i)]] %||% "")
    if (!nzchar(id)) {
      next
    }
    out[[id]] <- list(
      label = as.character(input[[paste0("pop_label_", i)]] %||% ""),
      dataset = as.character(input[[paste0("pop_dataset_", i)]] %||% ""),
      filter = as.character(input[[paste0("pop_filter_", i)]] %||% "")
    )
  }
  out
}

# ---- section shell --------------------------------------------------------

.setup_section <- function(ns, id, title, body) {
  shiny::tagAppendAttributes(
    .card(body, title = title, class = "ar-setup-section"),
    `data-ar-section` = id
  )
}

# The Setup dashboard header strip: a row of stat tiles carrying study-level
# machine facts. Every figure is real or omitted -- a tile whose data is not
# yet resolved does not render (never a fabricated 0). See the
# no-fabricated-render-content rule.
.setup_overview <- function(store, sections) {
  theme <- store$rv$report@theme
  # Sections done: count sections whose status is "ok".
  n_done <- sum(vapply(
    sections,
    function(s) identical(.section_status(theme, s$id)$state, "ok"),
    logical(1)
  ))
  tiles <- list(
    .stat_tile(
      value = sprintf("%d/%d", n_done, length(sections)),
      label = "Sections ready",
      icon = "check"
    )
  )

  cat <- tryCatch(arpillar::catalog_grid(store$con), error = function(e) NULL)
  if (!is.null(cat) && nrow(cat) > 0L) {
    tiles <- c(
      tiles,
      list(.stat_tile(
        value = format(nrow(cat), big.mark = ","),
        label = "Datasets",
        icon = "database"
      ))
    )
    # Population dataset (same resolution as .setup_paths: prefer ADSL).
    d <- theme$data %||% list()
    pop <- d$pop_dataset %||%
      (if ("ADSL" %in% cat$name) "ADSL" else cat$name[[1L]])
    pop_row <- cat[cat$name == pop, , drop = FALSE]
    if (nrow(pop_row) == 1L) {
      tiles <- c(
        tiles,
        list(.stat_tile(
          value = format(pop_row$rows[[1L]], big.mark = ","),
          label = sprintf("Records (%s)", tolower(pop)),
          icon = "table"
        ))
      )
    }
    # Subjects: distinct subject-id in the pop dataset. Only when resolved.
    subj_col <- d$subject_id %||% "USUBJID"
    subj_col <- trimws(strsplit(subj_col, ",", fixed = TRUE)[[1L]][[1L]])
    n_subj <- tryCatch(
      length(arpillar::distinct_values(store$con, pop, subj_col)),
      error = function(e) NA_integer_
    )
    if (!is.na(n_subj) && n_subj > 0L) {
      tiles <- c(
        tiles,
        list(.stat_tile(
          value = format(n_subj, big.mark = ","),
          label = "Subjects",
          icon = "database"
        ))
      )
    }
  }
  shiny::div(class = "ar-setup-overview", tiles)
}

# Horizontal section tab strip. Each tab shows the section title + a status
# glyph from `.section_status` (check when complete, missing-count when
# partial). The inline click handler swaps the active class on the strip and
# on the `.ar-setup-dash` wrapper (instant), then posts `setup_tab` so the
# server keeps authoritative state across re-renders.
.setup_tabstrip <- function(ns, store, sections, active) {
  theme <- store$rv$report@theme
  input_id <- ns("setup_tab")
  shiny::div(
    class = "ar-setup-tabs",
    role = "tablist",
    lapply(sections, function(s) {
      st <- .section_status(theme, s$id)
      badge <- switch(
        st$state,
        ok = shiny::span(class = "ar-setup-tab-badge ar-setup-tab-ok", "✓"),
        partial = shiny::span(
          class = "ar-setup-tab-badge ar-setup-tab-partial",
          as.character(st$missing)
        ),
        NULL
      )
      click_js <- sprintf(
        "(function(btn){var dash=btn.closest('.ar-setup-dash');if(dash){dash.className=dash.className.replace(/\\bar-setup-tab-[a-z]+\\b/,'ar-setup-tab-%s');}var sibs=btn.parentElement.querySelectorAll('.ar-setup-tab');for(var i=0;i<sibs.length;i++)sibs[i].classList.remove('ar-setup-tab-active');btn.classList.add('ar-setup-tab-active');Shiny.setInputValue('%s','%s',{priority:'event'});})(this)",
        s$id,
        input_id,
        s$id
      )
      shiny::tags$button(
        type = "button",
        class = paste(
          "ar-setup-tab",
          if (identical(s$id, active)) "ar-setup-tab-active" else ""
        ),
        role = "tab",
        `data-ar-setup-tab` = s$id,
        onclick = click_js,
        shiny::span(class = "ar-setup-tab-lbl", s$title),
        badge
      )
    })
  )
}

.section_status <- function(theme, section) {
  need <- switch(
    section,
    study = c("sponsor", "protocol", "study", "data_date"),
    paths = character(0), # scored across data + paths blocks
    treatment = character(0), # scored on treatment$arms length
    populations = character(0),
    page = c("orientation", "paper"),
    summaries = character(0),
    team = character(0),
    character(0)
  )
  if (section == "paths") {
    # ADaM directory is the minimum path a study needs; the rest are
    # nice-to-haves. Green when it's set, muted when not.
    v <- theme$data$adam_dir %||% theme$study$adam_dir %||% ""
    if (nzchar(v)) {
      return(list(state = "ok", missing = 0L))
    }
    return(list(state = "none", missing = 1L))
  }
  if (section == "treatment") {
    arms <- theme$treatment$arms %||% list()
    if (length(arms) == 0L) {
      return(list(state = "none", missing = 0L))
    }
    return(list(state = "ok", missing = 0L))
  }
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
  present <- vapply(
    need,
    function(k) {
      v <- block[[k]]
      !is.null(v) && length(v) >= 1L && any(nzchar(as.character(v)))
    },
    logical(1)
  )
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
  cls <- if (reviewed) {
    "ar-review-banner ar-review-banner-on"
  } else {
    "ar-review-banner"
  }
  label <- if (reviewed) {
    sprintf(
      "Reviewed by %s \u00b7 %s",
      meta$reviewed_by %||% "\u2014",
      meta$reviewed_at %||% "\u2014"
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
        # Native `<input type="date">` -- the OS provides its own picker;
        # no JS library, no CSS overrides needed. Reads/writes ISO
        # `YYYY-MM-DD` which is what `theme$study$data_date` stores.
        shiny::div(
          class = "ar-setup-field",
          shiny::tags$label(
            class = "ar-label",
            `for` = ns("study_data_date"),
            "Data extraction date"
          ),
          shiny::tags$input(
            id = ns("study_data_date"),
            class = "ar-input-flat",
            type = "date",
            value = s$data_date %||% ""
          )
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

# ---- Paths section (merges Data + Preferences + Sources, Stage 2) --------

#' Setup > Paths: every filesystem pointer in one section. Groups:
#' Data sources (ADaM + SDTM + Add-folder proxy), Population defaults
#' (pop dataset + subject id + pop treatment var, once a catalog is
#' mounted), Output directories, and Report conventions. Persists to
#' `setup.yml` across the `data`, `paths`, and `preferences` theme
#' blocks (unchanged shapes; only the UI is folded).
#' @noRd
.setup_paths <- function(ns, store) {
  d <- store$rv$report@theme$data %||% list()
  prefs <- store$rv$report@theme$preferences %||% list()
  paths <- store$rv$report@theme$paths %||% list()
  # Live-catalog probe: which datasets are mounted right now? Used to
  # populate the pop-dataset dropdown after import.
  cat <- tryCatch(arpillar::catalog_grid(store$con), error = function(e) NULL)
  ds_names <- if (is.null(cat) || nrow(cat) == 0L) character(0) else cat$name
  pop_dataset <- d$pop_dataset %||%
    (if ("ADSL" %in% ds_names) {
      "ADSL"
    } else if (length(ds_names) > 0L) {
      ds_names[[1L]]
    } else {
      ""
    })
  cols <- character(0)
  if (nzchar(pop_dataset) && !is.null(store$con)) {
    cols <- tryCatch(
      arpillar::data_items(store$con, pop_dataset)$name,
      error = function(e) character(0)
    )
  }
  # L-107 (Global TFL Reqs): subject IDs can stack for rollover /
  # extension studies -- USUBJID + SUBJID, comma-separated.
  subject_id <- d$subject_id %||%
    (if ("USUBJID" %in% cols) "USUBJID" else "")
  pop_arm <- d$pop_treatment_var %||%
    (if ("TRT01A" %in% cols) {
      "TRT01A"
    } else if ("TRT01P" %in% cols) {
      "TRT01P"
    } else {
      ""
    })
  bindings_ready <- length(ds_names) > 0L
  shiny::tagList(
    .setup_group(
      "Data sources",
      shiny::tagList(
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
        ),
        # Add-folder proxy: mount an additional data folder without
        # overwriting the ADaM/SDTM paths above. The full mounted
        # catalog is displayed in Data mode.
        shiny::div(
          class = "ar-setup-extra-source",
          .picker_proxy(ns("sources_pick")),
          shiny::span(
            class = "ar-muted ar-mono",
            " Add another folder \u2014 the full catalog lives in Data mode."
          )
        )
      )
    ),
    if (!bindings_ready) {
      NULL
    } else {
      .setup_group(
        "Population defaults",
        shiny::div(
          class = "ar-setup-grid",
          .select_input(
            ns,
            "data_pop_dataset",
            "Population dataset",
            ds_names,
            pop_dataset
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
            ns,
            "data_pop_treatment_var",
            "Population treatment variable",
            if (length(cols) == 0L) c("TRT01A") else cols,
            pop_arm
          )
        )
      )
    },
    .setup_group(
      "Output directories",
      shiny::tagList(
        shiny::div(
          class = "ar-setup-grid",
          .flat_input(
            ns,
            "paths_programs_dir",
            "Programs folder",
            paths$programs_dir %||% "",
            mono = TRUE,
            placeholder = "./programs/"
          ),
          .flat_input(
            ns,
            "paths_output_rtf_dir",
            "RTF output folder",
            paths$output_rtf_dir %||% "",
            mono = TRUE,
            placeholder = "./output/"
          ),
          .flat_input(
            ns,
            "paths_datasets_dir",
            "Datasets folder",
            paths$datasets_dir %||% "",
            mono = TRUE,
            placeholder = "./data/"
          ),
          .flat_input(
            ns,
            "paths_logs_dir",
            "Logs folder",
            paths$logs_dir %||% "",
            mono = TRUE,
            placeholder = "./.arframe/logs/"
          )
        ),
        shiny::p(
          class = "ar-muted ar-mono",
          "Empty = fall back to the default shown in the placeholder. Absolute paths pass through; relative paths resolve against the project root."
        )
      )
    ),
    .setup_group(
      "Report conventions",
      shiny::div(
        class = "ar-setup-grid",
        .seg_control(
          ns,
          "preferences_numbering_scheme",
          "Default numbering",
          c("14.x.x", "T-01", "None"),
          prefs$numbering_scheme %||% "14.x.x"
        ),
        .flat_input(
          ns,
          "preferences_sponsor_style",
          "Sponsor style library",
          prefs$sponsor_style %||% "",
          placeholder = "(none)"
        )
      )
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
    class = "btn btn-outline-secondary ex-btn-sm ar-picker-proxy",
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

# ---- Treatment section (Stage 2) ------------------------------------------

# Two default arms so a fresh study sees the shape. Overwritten as soon
# as the user edits or adds. Matches every GSK/BMS driver's convention:
# planned treatment 1 = active, 0 or 2 = comparator, plus a Total column
# built by the arm column headers when `arm$show_header_n` is on.
.ARM_SEEDS <- list(
  list(level_n = 1L, label = "Placebo"),
  list(level_n = 2L, label = "Active")
)

#' Setup > Treatment: the analysis-treatment column names + arm decode
#' grid. `trtvar` / `trtvarn` name the ADaM columns used at build time;
#' `arms` is an ordered list of `list(level_n = <int>, label = <chr>)`
#' rows -- what SAS studies encode as `proc format value MDRGSRTF ...`.
#' Rows write to `theme$treatment$arms`; the running header / footer
#' resolves `{arm-label}` from the row currently on-screen.
#' @noRd
.setup_treatment <- function(ns, store) {
  t <- store$rv$report@theme$treatment %||% list()
  arms <- t$arms %||% .ARM_SEEDS
  if (length(arms) == 0L) {
    arms <- .ARM_SEEDS
  }
  header <- shiny::div(
    class = "ar-setup-pop-header ar-setup-arm-header",
    shiny::span(class = "ar-mono", "LEVEL"),
    shiny::span("LABEL"),
    shiny::span("")
  )
  rows <- lapply(seq_along(arms), function(i) {
    a <- arms[[i]]
    shiny::div(
      class = "ar-setup-pop-row ar-setup-arm-row",
      `data-ar-arm-index` = i,
      .flat_input(
        ns,
        paste0("arm_row_level_", i),
        NULL,
        as.character(a$level_n %||% i),
        mono = TRUE,
        placeholder = "1"
      ),
      .flat_input(
        ns,
        paste0("arm_row_label_", i),
        NULL,
        a$label %||% "",
        placeholder = "Treatment label"
      ),
      shiny::tags$button(
        type = "button",
        class = "ar-pop-delete",
        onclick = sprintf(
          "Shiny.setInputValue('%s', %d, {priority: 'event'})",
          ns("arm_delete"),
          i
        ),
        title = "Delete arm",
        "\u00d7"
      )
    )
  })
  shiny::tagList(
    .setup_group(
      "Analysis treatment column",
      shiny::div(
        class = "ar-setup-grid",
        .flat_input(
          ns,
          "treatment_trtvar",
          "Treatment variable",
          t$trtvar %||% "TRT01P",
          mono = TRUE,
          placeholder = "TRT01P"
        ),
        .flat_input(
          ns,
          "treatment_trtvarn",
          "Numeric level variable",
          t$trtvarn %||% "TRT01PN",
          mono = TRUE,
          placeholder = "TRT01PN"
        )
      )
    ),
    .setup_group(
      "Arm decode",
      shiny::tagList(
        shiny::div(class = "ar-setup-pops ar-setup-arms", header, rows),
        shiny::tags$button(
          id = ns("arm_add"),
          type = "button",
          class = "ar-pop-add action-button",
          "+ Add arm"
        ),
        shiny::p(
          class = "ar-muted ar-mono",
          shiny::HTML(
            "Substitutes into a running header or footer as <code>{arm-label}</code>; the arm column header consumes this decode when rendering a per-arm summary."
          )
        )
      )
    )
  )
}

# Rebuild `theme$treatment$arms` from the current arm_row_* inputs.
.collect_arms <- function(input) {
  level_ids <- grep("^arm_row_level_[0-9]+$", names(input), value = TRUE)
  if (length(level_ids) == 0L) {
    return(list())
  }
  idx <- as.integer(sub("arm_row_level_", "", level_ids))
  ord <- order(idx)
  out <- lapply(idx[ord], function(i) {
    lvl <- suppressWarnings(as.integer(input[[paste0("arm_row_level_", i)]]))
    lbl <- as.character(input[[paste0("arm_row_label_", i)]] %||% "")
    list(level_n = if (is.na(lvl)) i else lvl, label = lbl)
  })
  out
}

# ---- Populations section --------------------------------------------------

# CDISC canonical analysis-set seeds. Rendered as editable rows when the
# theme's populations library is empty so users see the shape without
# hand-writing setup.yml. Once a user edits or adds, the theme takes over.
.POP_SEEDS <- list(
  safety = list(
    label = "Safety Analysis Set",
    dataset = "ADSL",
    filter = 'SAFFL == "Y"'
  ),
  efficacy = list(
    label = "Full Analysis Set (FAS)",
    dataset = "ADSL",
    filter = 'FASFL == "Y"'
  ),
  pp = list(label = "Per-Protocol Set", dataset = "ADSL", filter = ""),
  pk = list(label = "PK Analysis Set", dataset = "ADSL", filter = "")
)

.setup_populations <- function(ns, store) {
  pops <- store$rv$report@theme$populations %||% list()
  if (length(pops) == 0L) {
    pops <- .POP_SEEDS
  }
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
      .flat_input(
        ns,
        paste0("pop_dataset_", i),
        NULL,
        p$dataset %||% "ADSL",
        mono = TRUE
      ),
      .flat_input(
        ns,
        paste0("pop_filter_", i),
        NULL,
        p$filter %||% "",
        mono = TRUE
      ),
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
        if (is_default) "\u2605" else "\u2606"
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
        "\u00d7"
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
          shiny::HTML(
            "Refer to a population in a running header/footer as <code>{analysis-set}</code>, or bind it per output on the Roles tab."
          )
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
  if (is.list(pagehead)) {
    pagehead <- unlist(pagehead)
  }
  if (is.list(pagefoot)) {
    pagefoot <- unlist(pagefoot)
  }
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
    ),
    .setup_group(
      "Footnote register",
      .footnote_register(ns, store)
    )
  )
}

# Editable keyword -> text list, written to `theme$footnotes`. Rows
# rendered as `foot_key_<i>` + `foot_text_<i>` pairs; the observer in
# `mod_setup_server` rebuilds the register from the current inputs.
# Reference from any output's footnote as `@KEY` (expanded at render /
# export time -- see `.with_footnotes`).
.footnote_register <- function(ns, store) {
  reg <- store$rv$report@theme$footnotes %||% list()
  if (length(reg) == 0L) {
    reg <- list(SAFPOP = "Safety Population.")
  }
  header <- shiny::div(
    class = "ar-setup-pop-header ar-setup-foot-header",
    shiny::span(class = "ar-mono", "KEY"),
    shiny::span("TEXT"),
    shiny::span("")
  )
  keys <- names(reg)
  rows <- lapply(seq_along(reg), function(i) {
    shiny::div(
      class = "ar-setup-pop-row ar-setup-foot-row",
      `data-ar-foot-key` = keys[[i]],
      .flat_input(
        ns,
        paste0("foot_key_", i),
        NULL,
        keys[[i]],
        mono = TRUE,
        placeholder = "KEY"
      ),
      .flat_input(
        ns,
        paste0("foot_text_", i),
        NULL,
        as.character(reg[[i]] %||% ""),
        placeholder = "Registered footnote text"
      ),
      shiny::tags$button(
        type = "button",
        class = "ar-pop-delete",
        onclick = sprintf(
          "Shiny.setInputValue('%s', %d, {priority: 'event'})",
          ns("foot_delete"),
          i
        ),
        title = "Delete entry",
        "\u00d7"
      )
    )
  })
  shiny::tagList(
    shiny::div(class = "ar-setup-pops ar-setup-foots", header, rows),
    shiny::tags$button(
      id = ns("foot_add"),
      type = "button",
      class = "ar-pop-add action-button",
      "+ Add entry"
    ),
    shiny::p(
      class = "ar-muted ar-mono",
      shiny::HTML(
        "Reference from any output's footnote as <code>@KEY</code>; unregistered keys pass through as literal text."
      )
    )
  )
}

# Rebuild `theme$summaries$continuous` from the current cont_label_* +
# cont_format_* inputs. Row indices come from the input names; atoms
# (the stats vector) survive across the round-trip because the
# renderer reads them from theme and the observer preserves whatever
# atoms the row had -- only the label + format are user-edited scalars.
.collect_conts <- function(input) {
  label_ids <- grep("^cont_label_[0-9]+$", names(input), value = TRUE)
  if (length(label_ids) == 0L) {
    return(list())
  }
  idx <- as.integer(sub("cont_label_", "", label_ids))
  ord <- order(idx)
  out <- lapply(idx[ord], function(i) {
    stats_raw <- as.character(input[[paste0("cont_stats_", i)]] %||% "")
    # Comma-separated -> character vector; drop empty tokens so a
    # trailing comma doesn't leave a "".
    stats <- trimws(strsplit(stats_raw, ",", fixed = TRUE)[[1L]])
    stats <- stats[nzchar(stats)]
    list(
      label = as.character(input[[paste0("cont_label_", i)]] %||% ""),
      stats = stats,
      format = as.character(input[[paste0("cont_format_", i)]] %||% "a")
    )
  })
  out
}

# Rebuild `theme$footnotes` from the current foot_key_* + foot_text_*
# inputs. Empty keys are skipped so a half-typed entry does not clobber
# the register.
.collect_foots <- function(input) {
  key_ids <- grep("^foot_key_[0-9]+$", names(input), value = TRUE)
  if (length(key_ids) == 0L) {
    return(list())
  }
  idx <- as.integer(sub("foot_key_", "", key_ids))
  ord <- order(idx)
  out <- list()
  for (i in idx[ord]) {
    key <- as.character(input[[paste0("foot_key_", i)]] %||% "")
    if (!nzchar(key)) {
      next
    }
    out[[key]] <- as.character(input[[paste0("foot_text_", i)]] %||% "")
  }
  out
}

# Render `key` as a stack of {left, center, right} rows. Reads the current
# theme block if it exists, else falls back to a single seeded row.
.band_rows <- function(ns, key, band) {
  seed <- if (identical(key, "pagehead")) {
    list(left = "", center = "{sponsor} - {protocol}", right = "")
  } else {
    list(
      left = "{data_date}",
      center = "",
      right = "Page {page} of {npages}"
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
          ns,
          paste0("page_", key, "_left_", i),
          NULL,
          r$left %||% "",
          mono = TRUE,
          placeholder = "Left"
        ),
        .flat_input(
          ns,
          paste0("page_", key, "_center_", i),
          NULL,
          r$center %||% "",
          mono = TRUE,
          placeholder = "Center"
        ),
        .flat_input(
          ns,
          paste0("page_", key, "_right_", i),
          NULL,
          r$right %||% "",
          mono = TRUE,
          placeholder = "Right"
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
          "\u00d7"
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
      left = (band$left %||% character(0))[i] %||% "",
      center = (band$center %||% character(0))[i] %||% "",
      right = (band$right %||% character(0))[i] %||% ""
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
    n = 0L,
    pct = 1L,
    mean = 1L,
    sd = 2L,
    median = 1L,
    min = 0L,
    max = 0L
  )
  prec_labels <- list(
    n = "N",
    pct = "%",
    mean = "Mean",
    sd = "SD",
    median = "Median",
    min = "Min",
    max = "Max"
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
          ns,
          "cat_header_stat",
          "Header stat",
          c("n", "total_n", "none"),
          s$categorical$header_stat %||% "n"
        ),
        .seg_control(
          ns,
          "cat_level_format",
          "Level format",
          c("n", "pct", "n_pct", "pct_n"),
          s$categorical$level_format %||% "n_pct"
        ),
        .seg_control(
          ns,
          "cat_show_missing",
          "Show missing",
          c("auto", "always", "never"),
          s$categorical$show_missing %||% "auto"
        ),
        .flat_input(
          ns,
          "cat_missing_label",
          "Missing label",
          s$categorical$missing_label %||% "Missing"
        )
      )
    ),
    .setup_group(
      "Arm column headers",
      shiny::div(
        class = "ar-setup-grid",
        .seg_control(
          ns,
          "arm_show_header_n",
          "Show N per arm",
          c("yes", "no"),
          if (isFALSE(store$rv$report@theme$arm$show_header_n)) "no" else "yes"
        ),
        .flat_input(
          ns,
          "arm_header_n_format",
          "N format",
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
  list(label = "n", stats = "n", format = "a"),
  list(label = "Mean (SD)", stats = c("mean", "sd"), format = "a (b)"),
  list(label = "Median", stats = "median", format = "a"),
  list(label = "Min - Max", stats = c("min", "max"), format = "a - b")
)

.cont_row <- function(ns, i, row) {
  stats <- if (is.null(row$stats)) character(0) else row$stats
  shiny::div(
    class = "ar-cont-row",
    shiny::span(
      class = "ar-cont-grip",
      title = "Drag to reorder",
      "\u22ee\u22ee"
    ),
    .flat_input(
      ns,
      paste0("cont_label_", i),
      NULL,
      row$label %||% "",
      placeholder = "Row label"
    ),
    # Editable stats field: comma-separated atoms (`n, mean, sd`).
    # Chips render underneath as a live-echo of what parsed. Position
    # letters in the format field (`a`, `b`, ...) map to atoms by
    # index. Datalist gives autocomplete on the well-known atoms
    # without restricting -- users can type any name the engine
    # accepts.
    .flat_input(
      ns,
      paste0("cont_stats_", i),
      NULL,
      paste(stats, collapse = ", "),
      mono = TRUE,
      placeholder = "n, mean, sd",
      list_id = "ar-stat-atoms"
    ),
    .flat_input(
      ns,
      paste0("cont_format_", i),
      NULL,
      row$format %||% "a",
      mono = TRUE,
      placeholder = "a (b)"
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
      "\u00d7"
    )
  )
}

# Well-known continuous stat atoms; user can type any string but this
# datalist gives autocomplete for the common ones. Rendered once
# per module UI mount as a hidden `<datalist>`.
.CONT_ATOMS <- c(
  "n",
  "mean",
  "sd",
  "se",
  "median",
  "q1",
  "q3",
  "iqr",
  "min",
  "max",
  "geomean"
)

# ---- Team section ---------------------------------------------------------

#' Setup > Team: current user, activity feed, and who's working now.
#' Wired to `.arframe/team.json`, `.arframe/activity/*.jsonl`, and
#' `.arframe/presence/*.json` -- open the same project folder from
#' another machine and Refresh; teammates show up here.
#' @noRd
.setup_team <- function(ns, store) {
  me <- tryCatch(.who_am_i(), error = function(e) "user")
  path <- store$rv$path
  if (.user_is_generic(me)) {
    return(shiny::div(
      class = "ar-team-you",
      shiny::div(class = "ar-team-avatar", "?"),
      shiny::div(
        class = "ar-team-you-meta",
        shiny::div(class = "ar-team-you-name", "Set your name"),
        shiny::div(
          class = "ar-team-you-badge",
          "Run with ",
          shiny::tags$code("ARFRAME_USER=your.name"),
          " so activity + presence write under your identity."
        )
      )
    ))
  }
  # Simple roster only (2026-07-06 user feedback): "just list the user
  # who has used / using the arframe". No You card, no activity feed,
  # no explainer paragraph. Users come from `.arframe/team.json`; the
  # current user is added automatically on save (see `save_touched`).
  members <- if (is.null(path)) list() else .read_team(path)
  # Always show the current user, even before a project is opened,
  # so the section isn't empty on first launch.
  if (length(members) == 0L) {
    members <- list(list(name = me))
  }
  shiny::tagList(
    shiny::div(
      class = "ar-team-roster",
      lapply(members, function(m) {
        nm <- as.character(m$name %||% "")
        shiny::div(
          class = if (identical(nm, me)) {
            "ar-team-row ar-team-row-you"
          } else {
            "ar-team-row"
          },
          shiny::div(class = "ar-team-avatar", .team_initials(nm)),
          shiny::span(class = "ar-team-name", nm)
        )
      })
    )
  )
}

#' The presence rail: one avatar-pill row per teammate whose heartbeat is
#' fresh (within the last 60 seconds). Falls back to a "just you" line
#' when no other session is present.
#' @noRd
.team_presence_rail <- function(project_dir, me) {
  events <- tryCatch(.presence_list(project_dir), error = function(e) list())
  if (length(events) == 0L) {
    return(shiny::div(
      class = "ar-team-feed-row",
      shiny::div(class = "ar-team-avatar", .team_initials(me)),
      shiny::div(
        class = "ar-team-feed-meta",
        shiny::span(class = "ar-team-feed-who", me),
        shiny::span(class = "ar-team-feed-what", "the only session here")
      )
    ))
  }
  shiny::div(
    class = "ar-team-feed",
    lapply(events, function(ev) {
      who <- ev$user %||% "unknown"
      loc <- ev$current_output %||% ""
      mode <- ev$mode %||% ""
      what <- if (nzchar(loc)) {
        paste0("editing ", loc)
      } else if (nzchar(mode)) {
        paste0("in ", mode)
      } else {
        "here"
      }
      shiny::div(
        class = "ar-team-feed-row",
        shiny::div(
          class = paste(
            "ar-team-avatar",
            if (identical(.team_slug(who), .team_slug(me))) {
              "ar-team-avatar-you"
            } else {
              ""
            }
          ),
          .team_initials(who)
        ),
        shiny::div(
          class = "ar-team-feed-meta",
          shiny::span(class = "ar-team-feed-who", who),
          shiny::span(class = "ar-team-feed-what", what)
        )
      )
    })
  )
}

#' The activity feed: last 20 events across every teammate's per-user
#' JSONL file, sorted newest first. Renders a first-run explainer when
#' no lines exist yet.
#' @noRd
.team_activity_feed <- function(project_dir, me) {
  events <- tryCatch(
    .read_activity(project_dir, tail_n = 20L),
    error = function(e) list()
  )
  if (length(events) == 0L) {
    return(shiny::div(
      class = "ex-empty",
      shiny::div(class = "ex-empty-title", "No activity yet"),
      shiny::div(
        class = "ex-empty-sub",
        "Every save writes one line here. Open a project, make an edit, and this feed will populate."
      )
    ))
  }
  shiny::div(
    class = "ar-team-feed",
    lapply(events, function(ev) {
      who <- ev$user %||% "unknown"
      action <- ev$action %||% "edited"
      targets <- ev$targets %||% list()
      what <- if (length(targets) == 0L) {
        action
      } else if (length(targets) == 1L) {
        sprintf("%s %s", action, targets[[1]])
      } else {
        sprintf("%s %d output(s)", action, length(targets))
      }
      shiny::div(
        class = "ar-team-feed-row",
        shiny::div(class = "ar-team-avatar", .team_initials(who)),
        shiny::div(
          class = "ar-team-feed-meta",
          shiny::span(class = "ar-team-feed-who", who),
          shiny::span(class = "ar-team-feed-what", what),
          shiny::span(class = "ar-team-feed-ts ar-mono", .team_pretty_ts(ev$ts))
        )
      )
    })
  )
}

#' `2026-07-06T18:45:12Z` -> `18:45` (UTC minute-grain, matches the feed
#' bar's typographic scale). Empty stays empty.
#' @noRd
.team_pretty_ts <- function(ts) {
  if (!is.character(ts) || length(ts) == 0L || !nzchar(ts)) {
    return("")
  }
  sub(".*T([0-9]{2}:[0-9]{2}).*", "\\1", ts)
}

#' Initials from a name: first letter of the first two space-separated
#' tokens, uppercased. `"alice smith"` -> `"AS"`, `"vignesh"` -> `"VI"`.
#' @noRd
.team_initials <- function(name) {
  if (!nzchar(name)) {
    return("?")
  }
  parts <- strsplit(name, "[[:space:]._-]+")[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return("?")
  }
  if (length(parts) == 1L) {
    return(toupper(substr(parts[[1]], 1L, 2L)))
  }
  toupper(paste0(substr(parts[[1]], 1L, 1L), substr(parts[[2]], 1L, 1L)))
}

# ---- shared atoms ---------------------------------------------------------

.setup_group <- function(label, body) {
  shiny::div(
    class = "ar-setup-group",
    shiny::div(class = "ar-setup-group-lbl ar-mono", toupper(label)),
    body
  )
}

.flat_input <- function(
  ns,
  id,
  label,
  value = "",
  placeholder = NULL,
  mono = FALSE,
  list_id = NULL
) {
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
      placeholder = placeholder %||% "",
      list = list_id
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

# Iterate `.SETUP_SPEC` and install one `observeEvent` per entry via a
# `lapply` closure -- the pattern shiny's `observeEvent(input[[nm]], ...)`
# reliably supports. Every scalar field carries the same semantics: read
# the input, coerce, walk the nested theme path, commit only on genuine
# change. `.theme_set()`'s idempotence guard drops no-op writes so the
# module can safely re-render with pre-existing values without churning
# the dirty bit. A NULL from `coerce` (parse failure -- e.g. non-integer
# in a precision field) drops the edit silently; the user's next
# keystroke re-attempts. `ignoreNULL = TRUE` skips the initial NULL and
# is more robust in testServer than `ignoreInit = TRUE`.
.wire_all <- function(input, store) {
  lapply(.SETUP_SPEC, function(e) {
    shiny::observeEvent(
      input[[e$id]],
      {
        val <- input[[e$id]]
        coerce <- e$coerce %||% identity
        val <- coerce(val)
        if (is.null(val)) {
          return()
        }
        r <- store$rv$report
        theme <- .theme_set(r@theme, e$path, val)
        if (identical(theme, r@theme)) {
          return()
        }
        commit(store, S7::set_props(r, theme = theme), label = "edit setup")
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )
  })
  invisible(NULL)
}

# Set a nested path in `theme` to `value`, returning a new theme (or
# the unchanged input if the value is idempotent). Missing parents are
# created as empty lists.
.theme_set <- function(theme, path, value) {
  if (length(path) == 1L) {
    if (identical(theme[[path]], value)) {
      return(theme)
    }
    theme[[path]] <- value
    return(theme)
  }
  head <- path[[1L]]
  parent <- theme[[head]] %||% list()
  new_parent <- .theme_set(parent, path[-1L], value)
  if (identical(new_parent, parent)) {
    return(theme)
  }
  theme[[head]] <- new_parent
  theme
}

.bind_theme_field <- function(input, store, block, key) {
  input_id <- paste0(block, "_", key)
  shiny::observeEvent(
    input[[input_id]],
    {
      val <- input[[input_id]]
      r <- store$rv$report
      theme <- r@theme
      if (is.null(theme[[block]])) {
        theme[[block]] <- list()
      }
      if (identical(theme[[block]][[key]], val)) {
        return()
      }
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
      if (identical(theme[[key]], val)) {
        return()
      }
      theme[[key]] <- val
      commit(store, S7::set_props(r, theme = theme), label = "edit setup")
    },
    ignoreInit = TRUE
  )
}
