# Setup mode: SIX sections on ONE scrollable page.
#   1. Study         (Identity + Extraction)
#   2. Paths         (data + programs + output + logs) - the ONE section
#                    where every filesystem pointer lives, folded up from
#                    Data + Preferences+Paths + Sources.
#   3. Treatment     (one treatment variable + auto-filled draggable arms)
#   4. Populations   (ADaM-flag library)
#   5. Page & Style  (Geometry + Header/footer bands)
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

# Page margins coerce: a "top, right, bottom, left" (or single "all sides")
# comma string -> a length-1 or length-4 non-negative numeric vector, the
# shape the engine's `theme$page$margins` / `.margins_opt()` consumes. NULL
# (blank or unparseable) drops the edit, keeping the prior value -- the same
# validation the retired per-output margins control used.
.as_margins <- function(v) {
  raw <- trimws(v %||% "")
  if (!nzchar(raw)) {
    return(NULL)
  }
  parts <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  vals <- suppressWarnings(as.numeric(parts))
  if (anyNA(vals) || !length(vals) %in% c(1L, 4L) || any(vals < 0)) {
    return(NULL)
  }
  vals
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
  # ---- Data (ADaM/SDTM + population bindings) ----------------------------
  list(id = "data_adam_dir", path = c("data", "adam_dir")),
  list(id = "data_sdtm_dir", path = c("data", "sdtm_dir")),
  list(id = "data_pop_dataset", path = c("data", "pop_dataset")),
  # Subject-id is NOT a scalar input: it is a chip list fed by an
  # "Add a variable" picker, managed by dedicated add/remove observers
  # (see `mod_setup_server`). Stored as a comma-separated string.
  # ---- Treatment: ONE variable; its levels become the arm decode --------
  # The `trtvar` write rides `.wire_all`; a companion observer auto-fills
  # `treatment$arms` from the column's levels when the user changes it.
  # (Old `trtvarn` / `data$pop_treatment_var` were folded away 2026-07-07.)
  list(id = "treatment_trtvar", path = c("treatment", "trtvar")),
  # ---- Output directories -----------------------------------------------
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
  list(id = "page_margins", path = c("page", "margins"), coerce = .as_margins),
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
      )
    ),
    # Two decoupled outputs inside a client-owned visibility wrapper. The
    # wrapper's `ar-setup-tab-<active>` class (swapped client-side on a tab
    # click) drives which section card is visible via CSS -- so neither
    # server render restamps it, and a commit never bounces the active tab.
    # `tabstrip` reacts to the report (live completion badges); `sections`
    # reacts ONLY to structural changes (row add/delete, catalog mounts), so
    # typing in a field -- which commits on change but adds no rows -- never
    # re-renders, and never steals focus from, the input being edited.
    # A plain block wrapper is the grid item (stretches to fill the centered
    # 1120px column, exactly as the old single uiOutput did). `.ar-setup-dash`
    # keeps its container-query context (`container-type: inline-size`) INSIDE
    # it -- making it the direct grid item instead collapses it to min-content.
    shiny::div(
      class = "ar-setup-shell",
      shiny::div(
        class = "ar-setup-dash ar-setup-tab-study",
        shiny::uiOutput(ns("tabstrip")),
        shiny::uiOutput(ns("sections"))
      )
    )
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

    # Structural render gate. `sections` re-renders only when the STRUCTURE
    # changes -- a row added or removed, a catalog mounted, the default
    # population moved -- never on a scalar field commit. Every structural
    # mutation observer below calls `bump_sections()`; scalar edits ride
    # `.wire_all` -> commit -> autosave WITHOUT touching this, so the field
    # under the cursor is never rebuilt mid-type.
    render_nonce <- shiny::reactiveVal(0L)
    bump_sections <- function() {
      render_nonce(shiny::isolate(render_nonce()) + 1L)
    }

    # Tracks the treatment variable the arm decode was last filled for, so the
    # dropdown's initial connect-post (which looks like a change from NULL)
    # does NOT clobber arms/labels saved in setup.yml -- only a genuine
    # user switch re-derives the levels. See the auto-fill observer below.
    armed_trtvar <- shiny::reactiveVal(NULL)

    # Tab strip: reacts to the whole report so completion badges stay live as
    # fields fill. It holds only buttons (no text inputs), so re-rendering it
    # on every keystroke costs nothing and steals no focus. The active-tab
    # highlight is server-authoritative via `active_tab()`; the visible
    # section is CSS-driven off the static wrapper class (client-owned), so
    # this render never has to restamp it.
    output$tabstrip <- shiny::renderUI({
      store$rv$report
      store$rv$catalog_nonce
      active <- active_tab()
      meta <- list(
        list(id = "study", title = "Study"),
        list(id = "paths", title = "Paths"),
        list(id = "populations", title = "Populations"),
        list(id = "analysis_sets", title = "Analysis sets"),
        list(id = "treatment", title = "Treatment"),
        list(id = "page", title = "Page & Style"),
        list(id = "summaries", title = "Summaries"),
        list(id = "footnotes", title = "Footnotes"),
        list(id = "team", title = "Team")
      )
      .setup_tabstrip(ns, store, meta, active)
    })
    shiny::outputOptions(output, "tabstrip", suspendWhenHidden = FALSE)

    # Section cards: gated on `render_nonce` + catalog. Report reads are
    # isolated so a scalar commit does not re-render. Seven pharma-aligned
    # sections; Sources / Data / Preferences folded into Paths.
    output$sections <- shiny::renderUI({
      render_nonce()
      store$rv$catalog_nonce
      shiny::isolate({
        sections <- list(
          list(id = "study", title = "Study", body = .setup_study(ns, store)),
          list(id = "paths", title = "Paths", body = .setup_paths(ns, store)),
          list(
            id = "populations",
            title = "Populations",
            body = .setup_populations(ns, store)
          ),
          list(
            id = "analysis_sets",
            title = "Analysis sets",
            body = .setup_analysis_sets(ns, store)
          ),
          list(
            id = "treatment",
            title = "Treatment",
            body = .setup_treatment(ns, store)
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
          list(
            id = "footnotes",
            title = "Footnotes",
            body = .setup_footnotes(ns, store)
          ),
          list(id = "team", title = "Team", body = .setup_team(ns, store))
        )
        lapply(sections, function(s) {
          .setup_section(ns, s$id, s$title, s$body)
        })
      })
    })
    shiny::outputOptions(output, "sections", suspendWhenHidden = FALSE)

    # Every scalar field is wired via the declarative `.SETUP_SPEC`. See
    # top of file. Structural mutations (add/delete rows, folder pickers)
    # stay hand-wired below.
    .wire_all(input, store)

    # The default-population star lives in a section body, so moving it needs
    # a structural re-render (the scalar commit itself is handled by
    # `.wire_all`). Everything else in `.SETUP_SPEC` is a plain field whose
    # value the DOM already holds -- no re-render wanted.
    shiny::observeEvent(
      input$top_default_population,
      bump_sections(),
      ignoreInit = TRUE
    )

    # Changing the population dataset changes which columns the Subject ID
    # picker offers -- re-render so `.items_meta` reflects the new dataset.
    # Safe now that the picker holds no selection state (it lives in chips).
    shiny::observeEvent(
      input$data_pop_dataset,
      bump_sections(),
      ignoreInit = TRUE
    )

    # Subject-id chip list: the "Add a variable" picker appends; each chip's x
    # removes. Current value is read via `.pop_bindings` so the USUBJID
    # default (when nothing is committed yet) is preserved across an add.
    shiny::observeEvent(
      input$data_subject_add,
      {
        choice <- input$data_subject_add
        if (is.null(choice) || !nzchar(choice)) {
          return()
        }
        name <- .unpack_item_name(choice)
        cur <- .split_ids(.pop_bindings(store)$subject_id)
        if (name %in% cur) {
          return()
        }
        r <- store$rv$report
        theme <- r@theme
        if (is.null(theme$data)) {
          theme$data <- list()
        }
        theme$data$subject_id <- paste(c(cur, name), collapse = ", ")
        commit(store, S7::set_props(r, theme = theme), label = "add subject id")
        bump_sections()
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$data_subject_remove,
      {
        name <- input$data_subject_remove$name
        if (is.null(name) || !nzchar(name)) {
          return()
        }
        cur <- .split_ids(.pop_bindings(store)$subject_id)
        r <- store$rv$report
        theme <- r@theme
        if (is.null(theme$data)) {
          theme$data <- list()
        }
        theme$data$subject_id <- paste(setdiff(cur, name), collapse = ", ")
        commit(
          store,
          S7::set_props(r, theme = theme),
          label = "remove subject id"
        )
        bump_sections()
      },
      ignoreInit = TRUE
    )

    # ADaM / SDTM / Sources folder pickers: shinyDirChoose delegates to a
    # modal; on a picked path, update the text field AND fire mount_folder
    # so the catalog populates immediately (bindings dropdowns light up).
    volumes <- c(home = path.expand("~"), root = "/")
    shinyFiles::shinyDirChoose(input, "data_adam_pick", roots = volumes)
    shinyFiles::shinyDirChoose(input, "data_sdtm_pick", roots = volumes)

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
      bump_sections()
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
      bump_sections()
    })

    # Treatment arm value/label edits post PER FIELD via `arm_edit` (not a
    # bulk index-order rebuild), so a drag reorder is never fought by an
    # observer re-deriving arms from the inputs in their old positions.
    shiny::observeEvent(
      input$arm_edit,
      {
        e <- input$arm_edit
        i <- suppressWarnings(as.integer(e$i))
        field <- as.character(e$field %||% "")
        if (is.na(i) || !field %in% c("value", "label")) {
          return()
        }
        r <- store$rv$report
        theme <- r@theme
        arms <- theme$treatment$arms %||% .ARM_SEEDS
        if (i < 1L || i > length(arms)) {
          return()
        }
        val <- as.character(e$value %||% "")
        if (identical(arms[[i]][[field]], val)) {
          return()
        }
        arms[[i]][[field]] <- val
        theme$treatment$arms <- arms
        # No bump_sections: a text edit must not re-render the arm being typed.
        commit(store, S7::set_props(r, theme = theme), label = "edit arm")
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(input$arm_add, {
      r <- store$rv$report
      theme <- r@theme
      if (is.null(theme$treatment)) {
        theme$treatment <- list()
      }
      arms <- theme$treatment$arms %||% .ARM_SEEDS
      arms <- c(arms, list(list(value = "", label = "")))
      theme$treatment$arms <- arms
      commit(store, S7::set_props(r, theme = theme), label = "add arm")
      bump_sections()
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
      bump_sections()
    })

    # Auto-fill arms from the chosen treatment variable's data levels. Fires
    # only on a genuine USER switch: the first post (page connect) merely arms
    # the tracker, so arms/labels saved in setup.yml survive a reload; a later
    # change replaces them with the column's distinct values.
    shiny::observeEvent(
      input$treatment_trtvar,
      {
        trtvar <- input$treatment_trtvar
        if (is.null(trtvar) || !nzchar(trtvar)) {
          return()
        }
        prev <- shiny::isolate(armed_trtvar())
        armed_trtvar(trtvar)
        if (is.null(prev) || identical(prev, trtvar)) {
          return()
        }
        r <- store$rv$report
        theme <- r@theme
        ds <- theme$data$pop_dataset %||% .pop_bindings(store)$pop_dataset
        vals <- tryCatch(
          as.character(arpillar::distinct_values(
            store$con,
            ds,
            trtvar,
            limit = 50L
          )),
          error = function(e) character(0)
        )
        if (is.null(theme$treatment)) {
          theme$treatment <- list()
        }
        theme$treatment$arms <- lapply(vals, function(v) {
          list(value = v, label = v)
        })
        commit(store, S7::set_props(r, theme = theme), label = "auto-fill arms")
        bump_sections()
      },
      ignoreInit = TRUE
    )

    # Drag reorder: bridge.js posts `{order}` = the new sequence of row
    # `data-ar-item` indices. Shiny delivers the JSON array as a LIST, so
    # coerce element-wise (the `mod_card_roles` reorder idiom).
    shiny::observeEvent(input$arm_reorder, {
      ord <- suppressWarnings(as.integer(vapply(
        input$arm_reorder$order,
        as.character,
        character(1)
      )))
      r <- store$rv$report
      theme <- r@theme
      arms <- theme$treatment$arms %||% .ARM_SEEDS
      ord <- ord[!is.na(ord) & ord >= 1L & ord <= length(arms)]
      if (length(ord) != length(arms) || anyDuplicated(ord)) {
        return()
      }
      theme$treatment$arms <- arms[ord]
      commit(store, S7::set_props(r, theme = theme), label = "reorder arms")
      bump_sections()
    })

    # Decimals-by rules: `dec_row_change` edits the dataset + dp scalars; the
    # variable/param NAMES are a multi-select mutated by add / remove chip
    # events (`dec_name_add` / `dec_name_remove`). A dataset change re-renders
    # (new column/param options) and resets that row's names; dp does not.
    # Names are encoded `V|<col>` / `P|<param>`; a row shares one dp across
    # all its names.
    dec_ds_names <- function() {
      cat <- tryCatch(arpillar::catalog_grid(store$con), error = function(e) {
        NULL
      })
      if (is.null(cat) || nrow(cat) == 0L) character(0) else cat$name
    }
    shiny::observeEvent(
      input$dec_row_change,
      {
        e <- input$dec_row_change
        i <- suppressWarnings(as.integer(e$i))
        field <- as.character(e$field %||% "")
        val <- as.character(e$value %||% "")
        r <- store$rv$report
        theme <- r@theme
        rules <- theme$decimals_by %||% list()
        if (is.na(i) || i < 1L || i > length(rules)) {
          return()
        }
        do_bump <- FALSE
        if (identical(field, "dataset")) {
          if (identical(rules[[i]]$dataset %||% "", val)) {
            return()
          }
          rules[[i]]$dataset <- val
          # New dataset -> the old names no longer belong; clear them.
          rules[[i]]$names <- character(0)
          do_bump <- TRUE
        } else if (identical(field, "dp")) {
          dp <- suppressWarnings(as.integer(val))
          rules[[i]]$dp <- if (is.na(dp) || dp < 0L) 0L else dp
        } else {
          return()
        }
        theme$decimals_by <- rules
        commit(store, S7::set_props(r, theme = theme), label = "edit decimals")
        if (do_bump) {
          bump_sections()
        }
      },
      ignoreInit = TRUE
    )
    # Add an encoded `V|<col>` / `P|<param>` to rule i's names.
    shiny::observeEvent(
      input$dec_name_add,
      {
        e <- input$dec_name_add
        i <- suppressWarnings(as.integer(e$i))
        val <- as.character(e$value %||% "")
        if (is.na(i) || !nzchar(val)) {
          return()
        }
        r <- store$rv$report
        theme <- r@theme
        rules <- theme$decimals_by %||% list()
        if (i < 1L || i > length(rules)) {
          return()
        }
        cur <- .dec_rule_names(rules[[i]])
        if (val %in% cur) {
          return()
        }
        rules[[i]]$names <- c(cur, val)
        rules[[i]]$name <- NULL # drop the migrated single-name key
        rules[[i]]$by <- NULL
        theme$decimals_by <- rules
        commit(
          store,
          S7::set_props(r, theme = theme),
          label = "add decimals name"
        )
        bump_sections()
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$dec_name_remove,
      {
        e <- input$dec_name_remove
        i <- suppressWarnings(as.integer(e$i))
        val <- as.character(e$value %||% "")
        if (is.na(i) || !nzchar(val)) {
          return()
        }
        r <- store$rv$report
        theme <- r@theme
        rules <- theme$decimals_by %||% list()
        if (i < 1L || i > length(rules)) {
          return()
        }
        rules[[i]]$names <- setdiff(.dec_rule_names(rules[[i]]), val)
        rules[[i]]$name <- NULL
        rules[[i]]$by <- NULL
        theme$decimals_by <- rules
        commit(
          store,
          S7::set_props(r, theme = theme),
          label = "remove decimals name"
        )
        bump_sections()
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(input$dec_add, {
      r <- store$rv$report
      theme <- r@theme
      rules <- theme$decimals_by %||% list()
      ds <- dec_ds_names()
      rules <- c(
        rules,
        list(list(
          dataset = if (length(ds) > 0L) ds[[1L]] else "",
          names = character(0),
          dp = 0L
        ))
      )
      theme$decimals_by <- rules
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = "add decimals rule"
      )
      bump_sections()
    })
    shiny::observeEvent(input$dec_delete, {
      i <- as.integer(input$dec_delete)
      r <- store$rv$report
      theme <- r@theme
      rules <- theme$decimals_by %||% list()
      if (i >= 1L && i <= length(rules)) {
        rules <- rules[-i]
      }
      theme$decimals_by <- rules
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = "delete decimals rule"
      )
      bump_sections()
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
      bump_sections()
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
      bump_sections()
    })

    # Continuous statistic rows. Only the LABEL is a free-text input now
    # (`cont_label_*`); the stats vector is mutated by add / remove chip
    # events, and row order by drag. This observer tracks label edits only
    # and preserves each row's stats. Format is gone -- the engine infers
    # the join from the atoms.
    shiny::observe({
      label_ids <- grep("^cont_label_[0-9]+$", names(input), value = TRUE)
      if (length(label_ids) == 0L) {
        return()
      }
      idx <- as.integer(sub("cont_label_", "", label_ids))
      r <- store$rv$report
      theme <- r@theme
      rows <- .cont_rows_or_seed(theme)
      # Skip mid-flush: the input map lags the theme by one flush cycle
      # right after a structural change (add / delete / reorder row).
      if (length(idx) != length(rows)) {
        return()
      }
      changed <- FALSE
      for (i in idx) {
        if (i < 1L || i > length(rows)) {
          next
        }
        lab <- as.character(input[[paste0("cont_label_", i)]] %||% "")
        if (!identical(rows[[i]]$label %||% "", lab)) {
          rows[[i]]$label <- lab
          changed <- TRUE
        }
      }
      if (!changed) {
        return()
      }
      theme$summaries$continuous <- rows
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = "edit continuous labels"
      )
    })
    # Append an atom to row i's stats (native add-select).
    shiny::observeEvent(
      input$cont_stat_add,
      {
        e <- input$cont_stat_add
        i <- suppressWarnings(as.integer(e$i))
        stat <- as.character(e$value %||% "")
        if (is.na(i) || !nzchar(stat)) {
          return()
        }
        r <- store$rv$report
        theme <- r@theme
        rows <- .cont_rows_or_seed(theme)
        if (i < 1L || i > length(rows)) {
          return()
        }
        # A statistic is used once across the WHOLE table: reject it if any
        # row already carries it (backstop for the picker's global exclusion).
        used <- unique(unlist(
          lapply(rows, function(r) r$stats %||% character(0)),
          use.names = FALSE
        ))
        if (stat %in% used) {
          return()
        }
        cur <- rows[[i]]$stats %||% character(0)
        rows[[i]]$stats <- c(cur, stat)
        theme$summaries$continuous <- rows
        commit(store, S7::set_props(r, theme = theme), label = "add statistic")
        bump_sections()
      },
      ignoreInit = TRUE
    )
    # Remove an atom from row i's stats (chip x).
    shiny::observeEvent(
      input$cont_stat_remove,
      {
        e <- input$cont_stat_remove
        i <- suppressWarnings(as.integer(e$i))
        stat <- as.character(e$stat %||% "")
        if (is.na(i) || !nzchar(stat)) {
          return()
        }
        r <- store$rv$report
        theme <- r@theme
        rows <- .cont_rows_or_seed(theme)
        if (i < 1L || i > length(rows)) {
          return()
        }
        rows[[i]]$stats <- setdiff(rows[[i]]$stats %||% character(0), stat)
        theme$summaries$continuous <- rows
        commit(
          store,
          S7::set_props(r, theme = theme),
          label = "remove statistic"
        )
        bump_sections()
      },
      ignoreInit = TRUE
    )
    # Drag-reorder continuous rows (SortableJS -> `cont_reorder$order`).
    shiny::observeEvent(input$cont_reorder, {
      ord <- suppressWarnings(as.integer(vapply(
        input$cont_reorder$order,
        as.character,
        character(1)
      )))
      r <- store$rv$report
      theme <- r@theme
      rows <- .cont_rows_or_seed(theme)
      ord <- ord[!is.na(ord) & ord >= 1L & ord <= length(rows)]
      if (length(ord) != length(rows) || anyDuplicated(ord)) {
        return()
      }
      theme$summaries$continuous <- rows[ord]
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = "reorder continuous rows"
      )
      bump_sections()
    })
    shiny::observeEvent(input$cont_add, {
      r <- store$rv$report
      theme <- r@theme
      if (is.null(theme$summaries)) {
        theme$summaries <- list()
      }
      rows <- .cont_rows_or_seed(theme)
      # Append a blank row (label + empty stats -- no format).
      rows <- c(
        rows,
        list(list(label = "", stats = character(0)))
      )
      theme$summaries$continuous <- rows
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = "add continuous row"
      )
      bump_sections()
    })
    shiny::observeEvent(input$cont_delete, {
      i <- as.integer(input$cont_delete)
      r <- store$rv$report
      theme <- r@theme
      rows <- .cont_rows_or_seed(theme)
      if (i >= 1L && i <= length(rows)) {
        rows <- rows[-i]
      }
      theme$summaries$continuous <- rows
      commit(
        store,
        S7::set_props(r, theme = theme),
        label = "delete continuous row"
      )
      bump_sections()
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
        bump_sections()
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
        bump_sections()
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
        ok = shiny::span(
          class = "ar-setup-tab-badge ar-setup-tab-ok",
          "\u2713"
        ),
        partial = shiny::span(
          class = "ar-setup-tab-badge ar-setup-tab-partial",
          as.character(st$missing)
        ),
        NULL
      )
      click_js <- sprintf(
        "(function(btn){var dash=btn.closest('.ar-setup-dash');if(dash){dash.className=dash.className.replace(/\\bar-setup-tab-[a-z_]+\\b/,'ar-setup-tab-%s');}var sibs=btn.parentElement.querySelectorAll('.ar-setup-tab');for(var i=0;i<sibs.length;i++)sibs[i].classList.remove('ar-setup-tab-active');btn.classList.add('ar-setup-tab-active');Shiny.setInputValue('%s','%s',{priority:'event'});})(this)",
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
    footnotes = character(0),
    team = character(0),
    character(0)
  )
  if (section == "footnotes") {
    reg <- theme$footnotes %||% list()
    if (length(reg) == 0L) {
      return(list(state = "none", missing = 0L))
    }
    return(list(state = "ok", missing = 0L))
  }
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
  if (section == "analysis_sets") {
    pops <- theme$populations %||% list()
    if (length(pops) == 0L) {
      return(list(state = "none", missing = 0L))
    }
    return(list(state = "ok", missing = 0L))
  }
  block <- theme[[section]] %||% list()
  if (section == "populations") {
    # Green once a population dataset is bound (the subject-key on-ramp).
    v <- theme$data$pop_dataset %||% ""
    if (nzchar(v)) {
      return(list(state = "ok", missing = 0L))
    }
    return(list(state = "none", missing = 0L))
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

# ---- Study section --------------------------------------------------------

.setup_study <- function(ns, store) {
  s <- store$rv$report@theme$study %||% list()
  # One flat grid -- the Identity/Extraction sub-labels were noise on a
  # five-field section, and Status (draft/final) is dropped (unused).
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
    ),
    # Native `<input type="date">` -- the OS provides its own picker; no JS
    # library needed. Reads/writes ISO `YYYY-MM-DD`, what `data_date` stores.
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
        value = s$data_date %||% "",
        # A native `<input type="date">` is NOT matched by Shiny's text input
        # binding (text/search/url/email only), so without this it never
        # posts -- `data_date` stayed empty and the Study badge counted it
        # missing. Inline onchange mirrors the seg-control / tab-strip idiom.
        onchange = sprintf(
          "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
          ns("study_data_date")
        )
      )
    )
  )
}

# ---- Paths section (merges Data + Preferences + Sources, Stage 2) --------

#' Setup > Paths: every filesystem directory in one flat grid -- the two
#' data-source directories (ADaM + SDTM) and the four output directories.
#' Population / treatment BINDINGS (dataset, subject id, treatment var) moved
#' to the Populations and Treatment sections (they are data config, not
#' paths); report conventions were dropped.
#' @noRd
.setup_paths <- function(ns, store) {
  d <- store$rv$report@theme$data %||% list()
  paths <- store$rv$report@theme$paths %||% list()
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
    ),
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
  )
}

# Live-catalog probe for the population / treatment binding fields: which
# datasets are mounted, the resolved population dataset + its columns, and
# the default subject-id / treatment-var picks. Shared by Populations
# (dataset + subject id) and Treatment (population treatment variable), which
# is where these bindings now live.
.pop_bindings <- function(store) {
  d <- store$rv$report@theme$data %||% list()
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
  subject_id <- d$subject_id %||% (if ("USUBJID" %in% cols) "USUBJID" else "")
  pop_arm <- d$pop_treatment_var %||%
    (if ("TRT01A" %in% cols) {
      "TRT01A"
    } else if ("TRT01P" %in% cols) {
      "TRT01P"
    } else {
      ""
    })
  list(
    ds_names = ds_names,
    cols = cols,
    pop_dataset = pop_dataset,
    subject_id = subject_id,
    pop_arm = pop_arm,
    ready = length(ds_names) > 0L
  )
}

s_study <- function(store) {
  store$rv$report@theme$study %||% list()
}

# Split the stored comma-separated subject-id string back into a character
# vector (empty tokens dropped).
.split_ids <- function(s) {
  v <- trimws(strsplit(s %||% "", ",", fixed = TRUE)[[1L]])
  v[nzchar(v)]
}

# A grid-row DATASET cell: a bare native `<select>` of the mounted dataset
# names, wrapped in `.ar-setup-field` to match the row's other columns. The
# committed value is always kept as an option (even if that dataset is not
# currently mounted) so it never silently drops. `.collect_pops` reads it by
# id exactly like the old text input.
.pop_dataset_cell <- function(ns, id, selected, ds_names) {
  choices <- if (selected %in% ds_names || !nzchar(selected)) {
    ds_names
  } else {
    c(selected, ds_names)
  }
  if (length(choices) == 0L) {
    choices <- selected
  }
  shiny::div(
    class = "ar-setup-field",
    shiny::tags$label(class = "ar-label", `for` = ns(id), NULL),
    shiny::tags$select(
      id = ns(id),
      class = "ar-input-flat ar-mono",
      lapply(choices, function(d) {
        shiny::tags$option(
          value = d,
          selected = if (identical(d, selected)) "selected" else NULL,
          d
        )
      })
    )
  )
}

# One selected subject-id column as a removable chip: "A NAME x". The x
# posts `{name, nonce}` to the shared `data_subject_remove` observer via an
# inline onclick (the `mod_card_roles` remove idiom -- one observer, not one
# per chip).
.subject_chip <- function(ns, name) {
  shiny::span(
    class = "ar-subject-chip",
    `data-ar-subject` = name,
    shiny::span(class = "ar-chip ar-chip-cat", "A"),
    shiny::span(class = "ar-subject-chip-name", name),
    shiny::tags$button(
      type = "button",
      class = "ar-subject-chip-x",
      onclick = sprintf(
        "Shiny.setInputValue('%s', {name: '%s', nonce: Date.now()}, {priority: 'event'})",
        ns("data_subject_remove"),
        name
      ),
      title = "Remove",
      "\u00d7"
    )
  )
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

# Two default arms so a fresh study sees the shape before a treatment
# variable is chosen. Each arm is `list(value = <data level>, label =
# <display>)`; order is list position (set by drag). Once the user picks a
# treatment variable, the real levels replace these (see the auto-fill
# observer in `mod_setup_server`).
.ARM_SEEDS <- list(
  list(value = "Placebo", label = "Placebo"),
  list(value = "Active", label = "Active")
)

#' Setup > Treatment: ONE treatment variable whose data levels become the
#' arm decode. `trtvar` names the ADaM grouping column; `arms` is an ordered
#' list of `list(value = <chr>, label = <chr>)` rows -- the value is a level
#' read from the data, the label its display text, and the ORDER (list
#' position, set by dragging) is the arm order. Picking a new variable
#' auto-fills the levels; the running header / footer resolves `{arm-label}`.
#' @noRd
.setup_treatment <- function(ns, store) {
  t <- store$rv$report@theme$treatment %||% list()
  d <- store$rv$report@theme$data %||% list()
  pb <- .pop_bindings(store)
  # One treatment variable, folding in the old planned/actual pair.
  trtvar <- t$trtvar %||%
    d$pop_treatment_var %||%
    (if ("TRT01P" %in% pb$cols) {
      "TRT01P"
    } else if (length(pb$cols) > 0L) {
      pb$cols[[1L]]
    } else {
      ""
    })
  arms <- t$arms %||% .ARM_SEEDS
  if (length(arms) == 0L) {
    arms <- .ARM_SEEDS
  }
  header <- shiny::div(
    class = "ar-setup-pop-header ar-setup-arm-header",
    shiny::span(""),
    shiny::span("VALUE"),
    shiny::span("LABEL"),
    shiny::span("")
  )
  rows <- lapply(seq_along(arms), function(i) {
    a <- arms[[i]]
    shiny::div(
      class = "ar-setup-pop-row ar-setup-arm-row",
      `data-ar-item` = i,
      shiny::span(class = "ar-level-grip", .icon("grip", 11)),
      # Migrate the old `{level_n, label}` shape: value falls back to the
      # label so a saved arm never shows a blank value before it is re-picked.
      .arm_field(
        ns,
        i,
        "value",
        a$value %||% a$label %||% "",
        mono = TRUE,
        placeholder = "Level"
      ),
      .arm_field(
        ns,
        i,
        "label",
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
  # The treatment variable is a variable picker, so it wears the shared
  # chip + name + label selectize like Roles / Populations (not a bare native
  # `<select>`). `bare_value = TRUE`: the value is the plain column name, which
  # `input$treatment_trtvar` (wired via `.wire_all`) and the arm auto-fill
  # observer read directly. A synthetic row keeps a saved trtvar visible even
  # before its dataset's columns load.
  trt_items <- .items_meta(store, pb$pop_dataset)
  if (nzchar(trtvar) && !(trtvar %in% trt_items$name)) {
    trt_items <- rbind(
      trt_items,
      data.frame(
        name = trtvar,
        type = "category",
        sql_type = "",
        label = "",
        stringsAsFactors = FALSE
      )
    )
  }
  shiny::tagList(
    shiny::div(
      class = "ar-setup-grid",
      shiny::div(
        class = "ar-setup-field",
        shiny::tags$label(class = "ar-label", "Treatment variable"),
        .eligible_picker(
          ns,
          "treatment_trtvar",
          trt_items,
          selected = trtvar,
          placeholder = "Treatment variable",
          bare_value = TRUE
        )
      )
    ),
    .setup_group(
      "Arm decode",
      shiny::tagList(
        header,
        shiny::div(
          class = "ar-setup-pops ar-setup-arms",
          `data-ar-sortable` = "true",
          `data-ar-sortable-handle` = ".ar-level-grip",
          `data-ar-sortable-item` = ".ar-setup-arm-row",
          `data-ar-sortable-attr` = "data-ar-item",
          `data-ar-sortable-input` = ns("arm_reorder"),
          rows
        ),
        shiny::tags$button(
          id = ns("arm_add"),
          type = "button",
          class = "ar-pop-add action-button",
          "+ Add arm"
        ),
        shiny::p(
          class = "ar-muted ar-mono",
          shiny::HTML(
            "Levels are read from the treatment variable -- drag to reorder, edit the label, or <code>+ Add arm</code>. Substitutes into a running header or footer as <code>{arm-label}</code>."
          )
        )
      )
    )
  )
}

# One arm-decode cell: a text input (value or label) that posts a targeted
# `{i, field, value}` edit on change. It carries NO Shiny input id, so Shiny
# does not bind it as a scalar input -- the only channel is `arm_edit`, which
# a drag reorder cannot fight (contrast a bulk index-order rebuild).
.arm_field <- function(ns, i, field, value, mono = FALSE, placeholder = NULL) {
  cls <- paste0("ar-input-flat ar-arm-", field, if (mono) " ar-mono" else "")
  shiny::div(
    class = "ar-setup-field",
    shiny::tags$input(
      class = cls,
      type = "text",
      value = value,
      placeholder = placeholder %||% "",
      onchange = sprintf(
        "Shiny.setInputValue('%s', {i: %d, field: '%s', value: this.value, nonce: Date.now()}, {priority: 'event'})",
        ns("arm_edit"),
        i,
        field
      )
    )
  )
}

# ---- Populations section --------------------------------------------------

# CDISC canonical analysis-set seed. Rendered as an editable row when the
# theme's populations library is empty so users see the shape without
# hand-writing setup.yml. Once a user edits or adds, the theme takes over.
# Only SAFFL is seeded -- it is the one subject-level flag present across the
# ADaM pilot ADSL; the efficacy/FAS seed was dropped (no FASFL in the data).
# Users add their own analysis sets via "+ Add analysis set".
.POP_SEEDS <- list(
  safety = list(
    label = "Safety Analysis Set",
    dataset = "ADSL",
    filter = 'SAFFL == "Y"'
  )
)

# Setup > Populations: the population DATASET binding (which ADaM dataset
# defines the population + the subject key). The analysis-set library moved
# to its own section (`.setup_analysis_sets`, 2026-07-07) so the two
# concepts -- "what is a subject" vs "which filtered sets exist" -- each get
# a tab. Empty when no catalog is mounted yet.
.setup_populations <- function(ns, store) {
  pb <- .pop_bindings(store)
  if (!pb$ready) {
    return(shiny::p(
      class = "ar-muted",
      "Mount a dataset in Data (or set the ADaM directory in Paths) to bind the population dataset and subject key."
    ))
  }
  shiny::div(
    class = "ar-setup-grid",
    .select_input(
      ns,
      "data_pop_dataset",
      "Population dataset",
      pb$ds_names,
      pb$pop_dataset
    ),
    # Subject-id: a chip list of chosen columns (image: "A USUBJID x") fed by
    # the shared rich "Add a variable" picker (`.eligible_picker`, the same
    # component the Roles panel uses). The picker is always empty and only
    # ADDS -- the selected state lives in the server-rendered chips, so the
    # selectize-in-renderUI `selected` quirk cannot bite. Columns come from
    # the population dataset only; a chosen name is excluded from the picker.
    local({
      items <- .items_meta(store, pb$pop_dataset)
      current <- .split_ids(pb$subject_id)
      eligible <- items[!items$name %in% current, , drop = FALSE]
      shiny::div(
        class = "ar-setup-field ar-subject-field",
        shiny::tags$label(class = "ar-label", "Subject ID column(s)"),
        shiny::div(
          class = "ar-subject-chips",
          lapply(current, function(nm) .subject_chip(ns, nm))
        ),
        .eligible_picker(ns, "data_subject_add", eligible),
        shiny::tags$small(
          class = "ar-muted",
          "Add one or more subject-key columns from the population dataset. L-107: stack USUBJID + SUBJID for rollover / extension studies."
        )
      )
    })
  )
}

# Setup > Analysis sets: the editable analysis-set library (CDISC canonical
# safety / FAS / PP / PK seeds), one row per set with id / label / dataset /
# filter and a default-set star. Its own tab as of 2026-07-07.
.setup_analysis_sets <- function(ns, store) {
  pops <- store$rv$report@theme$populations %||% list()
  if (length(pops) == 0L) {
    pops <- .POP_SEEDS
  }
  default <- store$rv$report@theme$default_population %||% "safety"
  ids <- names(pops)
  # Mounted dataset names for the per-row DATASET dropdown.
  cat <- tryCatch(arpillar::catalog_grid(store$con), error = function(e) NULL)
  ds_names <- if (is.null(cat) || nrow(cat) == 0L) character(0) else cat$name
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
      .pop_dataset_cell(
        ns,
        paste0("pop_dataset_", i),
        p$dataset %||% "ADSL",
        ds_names
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
        ),
        .flat_input(
          ns,
          "page_margins",
          "Margins (in)",
          paste(p$margins %||% c(1, 1, 1, 1), collapse = ", ")
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

# ---- Footnotes section ----------------------------------------------------

#' Setup > Footnotes: the study-level footnote register -- the single source
#' of truth for footnotes reused across the whole report. Moved out of Page &
#' Style into its own section (2026-07-07); an output references an entry by
#' its `@KEY` (a per-output key picker lands in Report mode).
#' @noRd
.setup_footnotes <- function(ns, store) {
  .footnote_register(ns, store)
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
    shiny::tags$input(
      type = "search",
      class = "ar-foot-filter ar-input-flat ar-search",
      placeholder = "Filter footnotes by key or text\u2026",
      `aria-label` = "Filter footnotes",
      autocomplete = "off"
    ),
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
  # A statistic is displayed once across the whole table, so every row's
  # add-picker offers only atoms not already used in ANY row (global, not
  # per-row, uniqueness).
  cont_used <- unique(unlist(
    lapply(rows, function(r) r$stats %||% character(0)),
    use.names = FALSE
  ))
  cont_eligible <- setdiff(.CONT_ATOMS, cont_used)
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
  # Section order (2026-07-07): the two settings blocks that shape every
  # table -- Arm column headers, then Categorical rules -- sit on top; the
  # Continuous "summarise" table and its Precision follow; the Decimals-by
  # table is last because it grows with the dataset.
  shiny::tagList(
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
        # Display labels distinct from stored values so the choices read
        # clearly ("n (%)" not "n_pct"); `pct_n` dropped per user request.
        .seg_control(
          ns,
          "cat_level_format",
          "Level format",
          c("n" = "n", "%" = "pct", "n (%)" = "n_pct"),
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
      "Continuous rows",
      shiny::tagList(
        shiny::div(
          class = "ar-cont-rows",
          `data-ar-sortable` = "true",
          `data-ar-sortable-handle` = ".ar-cont-grip",
          `data-ar-sortable-item` = ".ar-cont-row",
          `data-ar-sortable-attr` = "data-ar-item",
          `data-ar-sortable-input` = ns("cont_reorder"),
          lapply(seq_along(rows), function(i) {
            .cont_row(ns, i, rows[[i]], cont_eligible)
          })
        ),
        shiny::tags$button(
          id = ns("cont_add"),
          type = "button",
          class = "ar-pop-add action-button",
          "+ Add statistic row"
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
    ),
    .setup_group(
      "Decimals by variable / parameter",
      .decimals_by_table(ns, store)
    )
  )
}

# Encode / decode a decimals-by NAME choice. A native `<option>` value can't
# carry the unit-separator the roles picker uses, so a printable `|` (which
# never appears in a column name or a CDISC PARAMCD value) splits scope from
# name: "V|AGE" (variable) / "P|SYSBP" (BDS param).
.dec_encode <- function(by, name) {
  paste0(if (identical(by, "param")) "P" else "V", "|", name %||% "")
}

# A rich, searchable "add" per-row picker: the shared chip + name + muted-label
# options (rendered once by the JS bridge, srcjs/bridge.js), always empty (it
# only ADDS). Emits a raw `<select data-ar-picker>` in the bridge's `add-row`
# mode -- on pick the bridge posts `{value, nonce, i}` to the SHARED observer
# named `target_input` (the row index `i` travels in `data-ar-picker-extra`),
# then clears. No per-row Shiny observer, so nothing leaks inside the dynamic
# renderUI. `items` is a data.frame(value, name, sub, type) with type
# measure/date/category/param; the visible text packs `name\x1ftype\x1flabel`.
.rich_picker <- function(ns, target_input, items, placeholder, i) {
  n <- nrow(items)
  choices <- if (n == 0L) {
    character(0)
  } else {
    packed <- vapply(
      seq_len(n),
      function(k) {
        paste0(items$name[[k]], "\x1f", items$type[[k]], "\x1f", items$sub[[k]])
      },
      character(1)
    )
    stats::setNames(items$value, packed)
  }
  # onChange posts `{i, value, nonce}` to the SHARED observer `target_input`
  # (the row index is baked in here), then clears -- so there is no per-row
  # Shiny observer to leak inside the dynamic renderUI. The one-line hook
  # carries per-row wiring, not presentation; the render lives in the bundle.
  onchange <- sprintf(
    paste0(
      "function(value) { if (value) { ",
      "Shiny.setInputValue('%s', {i: %d, value: value, nonce: Date.now()}, ",
      "{priority: 'event'}); this.setValue(''); } }"
    ),
    ns(target_input),
    as.integer(i)
  )
  .ar_picker_select(
    ns = ns,
    input_id = paste0(target_input, "_pick_", i),
    choices = choices,
    placeholder = placeholder,
    onchange = onchange,
    class = "ar-add-picker"
  )
}

# Picker items for a dataset's columns (variables) + PARAMCD levels (params),
# each row a data.frame line for `.rich_picker`; `chosen` (encoded ids) are
# excluded. Variable type comes from the catalog; params render with a 'P'.
.dec_pick_items <- function(store, dataset, chosen) {
  items <- .items_meta(store, dataset)
  empty <- data.frame(
    value = character(0),
    name = character(0),
    sub = character(0),
    type = character(0),
    stringsAsFactors = FALSE
  )
  # Decimals apply only to numeric fields, so offer measure columns only --
  # flags / dates / other categoricals are never rounded. PARAMCD detection
  # below still reads the FULL `items` (PARAMCD is itself a category column).
  num_items <- items[items$type == "measure", , drop = FALSE]
  df_v <- if (nrow(num_items) > 0L) {
    sub <- ifelse(
      is.na(num_items$label) | !nzchar(num_items$label),
      num_items$type,
      num_items$label
    )
    data.frame(
      value = paste0("V|", num_items$name),
      name = num_items$name,
      sub = sub,
      type = num_items$type,
      stringsAsFactors = FALSE
    )
  } else {
    empty
  }
  # BDS parameters: the NAME is PARAMCD (drives the `P|` value); the muted
  # description is the decoded PARAM value from the same row, via a
  # `SELECT DISTINCT PARAMCD, PARAM` on the dataset. Falls back to the bare
  # "parameter" word when PARAM is absent or the pair query fails.
  df_p <- empty
  if (all(c("PARAMCD", "PARAM") %in% items$name)) {
    pairs <- .paramcd_pairs(store, dataset)
    if (!is.null(pairs) && nrow(pairs) > 0L) {
      df_p <- data.frame(
        value = paste0("P|", pairs$PARAMCD),
        name = pairs$PARAMCD,
        sub = ifelse(
          is.na(pairs$PARAM) | !nzchar(pairs$PARAM),
          "parameter",
          pairs$PARAM
        ),
        type = "param",
        stringsAsFactors = FALSE
      )
    }
  }
  if (nrow(df_p) == 0L && "PARAMCD" %in% items$name) {
    params <- tryCatch(
      as.character(
        arpillar::distinct_values(store$con, dataset, "PARAMCD", limit = 200L)
      ),
      error = function(e) character(0)
    )
    if (length(params) > 0L) {
      df_p <- data.frame(
        value = paste0("P|", params),
        name = params,
        sub = "parameter",
        type = "param",
        stringsAsFactors = FALSE
      )
    }
  }
  out <- rbind(df_v, df_p)
  out[!out$value %in% chosen, , drop = FALSE]
}

# Distinct (PARAMCD, PARAM) rows for a BDS dataset -- the decimals-by param
# picker shows PARAMCD as the name and PARAM as the muted description. Reads
# the dataset's DuckDB view name from arpillar's catalog registry (a public S7
# slot, keyed `"<library>::<name>"`) and runs a direct query on the shared
# connection (`store$con@con`). Returns a data.frame(PARAMCD, PARAM) or NULL.
.paramcd_pairs <- function(store, dataset) {
  view_name <- tryCatch(
    {
      reg <- store$con@registry
      key <- paste0("WORK::", dataset)
      if (exists(key, envir = reg, inherits = FALSE)) {
        get(key, envir = reg)$view_name
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )
  if (is.null(view_name) || !nzchar(view_name)) {
    return(NULL)
  }
  db <- store$con@con
  qview <- DBI::dbQuoteIdentifier(db, view_name)
  sql <- paste0(
    "SELECT DISTINCT PARAMCD, PARAM FROM ",
    qview,
    " WHERE PARAMCD IS NOT NULL ORDER BY PARAMCD NULLS LAST LIMIT 200"
  )
  tryCatch(DBI::dbGetQuery(db, sql), error = function(e) NULL)
}

# Picker items for the continuous-statistic vocabulary: each eligible atom
# with its description; numeric so all render with the measure chip.
.stat_pick_items <- function(eligible) {
  data.frame(
    value = eligible,
    name = eligible,
    sub = unname(.CONT_ATOM_DESC[eligible]),
    type = "measure",
    stringsAsFactors = FALSE
  )
}

# Migrate a decimals-by rule to the multi-name shape: `names` is a vector
# of encoded `V|<col>` / `P|<param>` ids sharing one `dp`. An old single
# `by` + `name` rule folds into a one-element `names`.
.dec_rule_names <- function(rl) {
  if (!is.null(rl$names)) {
    return(as.character(rl$names))
  }
  if (!is.null(rl$name) && nzchar(rl$name)) {
    return(.dec_encode(rl$by %||% "variable", rl$name))
  }
  character(0)
}

# One chosen variable/param chip in a decimals-by row: the bare name, an x
# posting `{i, value, nonce}` to the shared `dec_name_remove` observer.
.dec_name_chip <- function(ns, i, enc) {
  nm <- sub("^[VP]\\|", "", enc)
  shiny::span(
    class = "ar-cont-chip",
    title = if (startsWith(enc, "P|")) "parameter" else "variable",
    shiny::span(class = "ar-cont-chip-name ar-mono", nm),
    shiny::tags$button(
      type = "button",
      class = "ar-cont-chip-x",
      onclick = sprintf(
        "Shiny.setInputValue('%s', {i: %d, value: '%s', nonce: Date.now()}, {priority: 'event'})",
        ns("dec_name_remove"),
        i,
        enc
      ),
      title = "Remove",
      "\u00d7"
    )
  )
}

# The VARIABLE / PARAM cell: the chosen names as chips + the rich searchable
# add-picker. Multiple variables/params share one row's `dp` -- e.g. WEIGHTBL
# + HEIGHTBL at 1 dp in one row, BMIBL at 2 in another. The picker posts the
# encoded value `{i, value}` to the shared `dec_name_add` observer (add-one-
# at-a-time, no shift-multiselect; leak-free -- no per-row Shiny observer).
.dec_names_cell <- function(ns, i, store, dataset, names_enc) {
  shiny::div(
    class = "ar-setup-field",
    shiny::div(
      class = "ar-cont-stats",
      shiny::div(
        class = "ar-cont-chips",
        lapply(names_enc, function(e) .dec_name_chip(ns, i, e))
      ),
      .rich_picker(
        ns,
        "dec_name_add",
        .dec_pick_items(store, dataset, names_enc),
        "+ Add variable / param",
        i
      )
    )
  )
}

# A decimals-by row cell: a native `<select>` posting `{i, field, value}` to
# the shared `dec_row_change` observer (the roles / arm-edit idiom).
.dec_select <- function(ns, i, field, choices, selected) {
  shiny::div(
    class = "ar-setup-field",
    shiny::tags$select(
      class = "ar-input-flat ar-mono",
      onchange = sprintf(
        "Shiny.setInputValue('%s', {i: %d, field: '%s', value: this.value, nonce: Date.now()}, {priority: 'event'})",
        ns("dec_row_change"),
        i,
        field
      ),
      lapply(seq_along(choices), function(k) {
        # choices is `label -> value` (names are the display labels).
        v <- choices[[k]]
        shiny::tags$option(
          value = v,
          selected = if (identical(v, selected)) "selected" else NULL,
          names(choices)[[k]]
        )
      })
    )
  )
}

# Setup > Summaries > Decimals by variable / parameter: one raw-precision
# rule per row (DecimBy sheet). Each row picks a dataset, then a variable OR a
# BDS PARAMCD level, and the raw decimal places; the per-statistic offsets in
# Precision above add to this at render time.
.decimals_by_table <- function(ns, store) {
  cat <- tryCatch(arpillar::catalog_grid(store$con), error = function(e) NULL)
  ds_names <- if (is.null(cat) || nrow(cat) == 0L) character(0) else cat$name
  rules <- store$rv$report@theme$decimals_by %||% list()
  header <- shiny::div(
    class = "ar-setup-pop-header ar-setup-dec-header",
    shiny::span("DATASET"),
    shiny::span("VARIABLE / PARAM"),
    shiny::span(class = "ar-mono", "RAW DP"),
    shiny::span("")
  )
  rows <- lapply(seq_along(rules), function(i) {
    rl <- rules[[i]]
    ds <- rl$dataset %||% (if (length(ds_names) > 0L) ds_names[[1L]] else "")
    shiny::div(
      class = "ar-setup-pop-row ar-setup-dec-row",
      `data-ar-dec` = i,
      .dec_select(ns, i, "dataset", stats::setNames(ds_names, ds_names), ds),
      .dec_names_cell(ns, i, store, ds, .dec_rule_names(rl)),
      shiny::div(
        class = "ar-setup-field",
        shiny::tags$input(
          class = "ar-input-flat ar-mono",
          type = "number",
          min = "0",
          value = as.character(rl$dp %||% 0L),
          onchange = sprintf(
            "Shiny.setInputValue('%s', {i: %d, field: 'dp', value: this.value, nonce: Date.now()}, {priority: 'event'})",
            ns("dec_row_change"),
            i
          )
        )
      ),
      shiny::tags$button(
        type = "button",
        class = "ar-pop-delete",
        onclick = sprintf(
          "Shiny.setInputValue('%s', %d, {priority: 'event'})",
          ns("dec_delete"),
          i
        ),
        title = "Delete rule",
        "\u00d7"
      )
    )
  })
  shiny::tagList(
    if (length(ds_names) == 0L) {
      shiny::p(
        class = "ar-muted",
        "Mount a dataset to set per-variable or per-parameter precision."
      )
    } else {
      shiny::tagList(
        shiny::div(class = "ar-setup-pops ar-setup-decs", header, rows),
        shiny::tags$button(
          id = ns("dec_add"),
          type = "button",
          class = "ar-pop-add action-button",
          "+ Add rule"
        ),
        shiny::p(
          class = "ar-muted ar-mono",
          "Raw data precision per variable (e.g. AGE) or per BDS parameter (e.g. SYSBP). The per-statistic offsets above add to this."
        )
      )
    }
  )
}

# Canonical continuous stat-row seeds. Each row IS the rendered line: a
# label plus an ordered list of stat atoms. The engine infers the join
# from the atoms (1 -> bare, mean+sd -> "a (b)", other pairs -> "a, b"),
# so there is no format template.
.CONT_SEEDS <- list(
  list(label = "n", stats = "n"),
  list(label = "Mean (SD)", stats = c("mean", "sd")),
  list(label = "Median", stats = "median"),
  list(label = "Min - Max", stats = c("min", "max"))
)

# The continuous rows in play: the theme's list, or the canonical seeds when
# the theme carries none. The arpillar default theme seeds an EMPTY list (not
# NULL), so `%||%` alone would miss it -- and then the renderer (which shows
# seeds for an empty list) would be out of sync with observers (which mutate
# the theme). Both go through this helper so a first edit materializes the
# seeds into the theme.
.cont_rows_or_seed <- function(theme) {
  rows <- theme$summaries$continuous
  if (is.null(rows) || length(rows) == 0L) .CONT_SEEDS else rows
}

.cont_row <- function(ns, i, row, eligible) {
  stats <- if (is.null(row$stats)) character(0) else row$stats
  shiny::div(
    class = "ar-cont-row",
    `data-ar-item` = i,
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
    # Stats cell: the chosen atoms as removable chips (in render order --
    # the order the engine joins them), plus a native descriptive select
    # that appends one atom at a time (no shift-multiselect). The engine
    # infers the join from the atoms (1 -> bare, mean+sd -> "a (b)", other
    # pairs -> "a, b"), so there is no format template to edit.
    shiny::div(
      class = "ar-cont-stats",
      shiny::div(
        class = "ar-cont-chips",
        lapply(stats, function(s) .cont_stat_chip(ns, i, s))
      ),
      .cont_stat_add(ns, i, eligible)
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

# One chosen-statistic chip: the atom code with its description as a
# tooltip, and an x that posts `{i, stat, nonce}` to the shared
# `cont_stat_remove` observer (the subject-chip idiom).
.cont_stat_chip <- function(ns, i, stat) {
  shiny::span(
    class = "ar-cont-chip",
    title = .CONT_ATOM_DESC[[stat]] %||% stat,
    shiny::span(class = "ar-cont-chip-name ar-mono", stat),
    shiny::tags$button(
      type = "button",
      class = "ar-cont-chip-x",
      onclick = sprintf(
        "Shiny.setInputValue('%s', {i: %d, stat: '%s', nonce: Date.now()}, {priority: 'event'})",
        ns("cont_stat_remove"),
        i,
        stat
      ),
      title = "Remove",
      "\u00d7"
    )
  )
}

# The "+ add statistic" control: the rich searchable add-picker over the
# eligible atoms (name + description, measure chip). Picking one posts
# `{i, stat, nonce}` to the shared `cont_stat_add` observer and clears --
# leak-free (no per-row Shiny observer inside the dynamic renderUI).
.cont_stat_add <- function(ns, i, eligible) {
  .rich_picker(
    ns,
    "cont_stat_add",
    .stat_pick_items(eligible),
    "+ Add statistic",
    i
  )
}

# Well-known continuous stat atoms; user can type any string but this
# datalist gives autocomplete for the common ones. Rendered once
# per module UI mount as a hidden `<datalist>`.
# The continuous-statistic vocabulary a stat row can draw on -- the full
# standard set (count / central / spread / range / quantiles / CI of the mean
# / geometric family for PK). Rendered as the `<datalist>` typeahead; the
# per-statistic decimal offset for each is the Global TFL default until
# overridden. `iqr` and `qrange` are aliases for the Q3-Q1 spread.
.CONT_ATOM_DESC <- c(
  # count + central tendency
  n = "Count (non-missing)",
  mean = "Mean",
  median = "Median",
  sum = "Sum",
  # spread
  sd = "Standard Deviation",
  se = "Standard Error",
  cv = "Coefficient of Variation (%)",
  var = "Variance",
  iqr = "Interquartile Range",
  qrange = "Quartile Range",
  # range + quantiles
  min = "Minimum",
  max = "Maximum",
  q1 = "1st Quartile (P25)",
  q3 = "3rd Quartile (P75)",
  p1 = "1st Percentile",
  p5 = "5th Percentile",
  p10 = "10th Percentile",
  p90 = "90th Percentile",
  p95 = "95th Percentile",
  p99 = "99th Percentile",
  # confidence limits of the mean
  lclm = "Lower Confidence Limit",
  uclm = "Upper Confidence Limit",
  # geometric family (PK)
  geomean = "Geometric Mean",
  geosd = "Geometric Standard Deviation",
  geose = "Geometric Standard Error",
  geocv = "Geometric CV (%)",
  geolclm = "Geometric Lower Confidence Limit",
  geouclm = "Geometric Upper Confidence Limit"
)
.CONT_ATOMS <- names(.CONT_ATOM_DESC)

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
      # `choices` may be a named vector: the name is the display label, the
      # value is the stored/posted value (e.g. c("n (%)" = "n_pct")). Unnamed
      # -> label == value, the original behaviour.
      local({
        labs <- names(choices)
        lapply(seq_along(choices), function(k) {
          ch <- as.character(choices[[k]])
          lab <- if (!is.null(labs) && nzchar(labs[[k]])) labs[[k]] else ch
          shiny::tags$button(
            type = "button",
            class = paste(
              "ar-seg-opt",
              if (identical(ch, selected)) "ar-seg-opt-active" else ""
            ),
            `data-ar-seg-value` = ch,
            onclick = click_js,
            lab
          )
        })
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
