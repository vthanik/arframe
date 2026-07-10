# The injected structured store (design spec 5.1): one S7 report + galley
# pointers + undo/redo + a two-stage ARD cache. Created once per session and
# handed to every module server; modules communicate ONLY through it, never
# with each other. ALL draft/edit state lives in `store$rv`, never in the
# DOM -- a folded, lazy-mounted, or unmounted pane can never silently drop
# configuration (the suspend-contract regression test proves this end to
# end, with zero Shiny session mounted).

# ---- construction -----------------------------------------------------

#' Create the injected structured store.
#'
#' `con` is the arpillar catalog handle; `report` seeds the live document
#' (defaults to a fresh one-page "Untitled report"). Returns a plain list
#' bundling the catalog, the reactive pointer set every module reads/writes,
#' a plain-environment undo/redo stack, and a plain-environment memo cache
#' (`cached_ard()`'s two-stage seam).
#' @noRd
new_store <- function(con, report = NULL) {
  if (is.null(report)) {
    report <- arpillar::report(
      id = "report1",
      name = "Untitled report",
      pages = list(arpillar::page(id = "p1"))
    )
  }
  undo <- new.env(parent = emptyenv())
  undo$stack <- list()
  undo$redo <- list()
  list(
    con = con,
    rv = shiny::reactiveValues(
      report = report,
      selected = NULL,
      # Report LoC contents-rail filter: NULL = all outputs, else a kind key
      # ("table"/"figure"/"listing") narrowing the main table to that group
      # (the Report-mode twin of `data_source`).
      loc_group = NULL,
      # Report contents-rail: kind folders whose nested outputs are collapsed
      # (the Report twin of `src_collapsed`; default empty = all expanded).
      loc_collapsed = character(0),
      # LoC multi-select: the set of output ids selected in the main table
      # (shift / Cmd click). `selected` is the anchor (single); `loc_selected`
      # is what Delete acts on. The Report twin of Data's `data_selected`.
      loc_selected = character(0),
      region = NULL,
      card = FALSE,
      pinned = FALSE,
      # Data mode is the opening screen (user decision 2026-07-04): the
      # data on-ramp shows before the report it feeds. Must match
      # mod_frame_ui()'s initial ar-mode-* class.
      mode = "setup",
      dataset = NULL,
      bridge_dataset = NULL,
      adding = FALSE,
      filter_draft = list(),
      # The open filter chip's draft index (NULL = no popover). Cleared on
      # selection change (the pane's seed observer); guarded against a
      # stale index by every reader (chips + popover, 2026-07-04).
      filter_open = NULL,
      path = NULL,
      dirty = FALSE,
      saved_at = NULL,
      broken = character(0),
      stale = character(0),
      log = character(0),
      catalog_nonce = 0L,
      rail_collapsed = FALSE,
      # Report CONTENTS rail hide flag -- INDEPENDENT of the Data `rail_collapsed`
      # so collapsing one mode's rail never leaks to the other (re-clicking the
      # active Report tab toggles this).
      loc_rail_collapsed = FALSE,
      insp_collapsed = FALSE,
      insp_tab = "roles",
      run_nonce = 0L,
      code_view = FALSE,
      data_source = NULL,
      data_focus = NULL,
      # Data explorer multi-select: the set of dataset names selected (shift /
      # Cmd click). `data_focus` is the anchor (drives View data + the shift
      # range); `data_selected` is what Delete acts on.
      data_selected = character(0),
      grid_dataset = NULL,
      # Data-mode SOURCES tree: folder labels whose dataset children are
      # collapsed. Server-held (not the DOM) so a tree re-render preserves
      # the expand/collapse state.
      src_collapsed = character(0),
      # Report mode's drill pointer -- the Report-mode twin of `grid_dataset`
      # (2026-07-08): NULL shows the List-of-Contents table, a non-NULL output
      # id drills into that output's paper + inspector. `drill_open()` sets it
      # (alongside `selected`); the `ar-report-open` workspace class mirrors
      # `!is.null()` for the layout gate.
      report_open = NULL,
      # Open variable-peek rows in the Roles editor, by item name -- store-
      # side (never the DOM) so a digest redraw or Sortable re-init cannot
      # fold an open peek.
      peek = character(0)
    ),
    undo = undo,
    cache = new.env(parent = emptyenv()),
    # Data-mode provenance maps, keyed by dataset name. arframe mounts every
    # dataset into arpillar's single WORK library (the engine resolves an
    # output's `@dataset` there and has no per-object library), so the
    # SOURCE FOLDER a dataset came from and its original file KIND are
    # arframe-side UI concepts the catalog does not carry. Plain envs (not
    # reactive) -- the explorer reads them under `catalog_nonce`, which
    # every mount/delete bumps.
    sources = new.env(parent = emptyenv()),
    kinds = new.env(parent = emptyenv()),
    # Output-file mtimes tracked by open_project() / save_touched() /
    # scan_and_merge() (fct_project.R). Keyed by basename; values are
    # numeric POSIX seconds. Empty until a project folder is opened.
    mtimes = new.env(parent = emptyenv())
  )
}

#' Install the auto-save observer.
#'
#' Watches `store$rv$dirty` and, whenever it flips TRUE while the project has
#' a path set, schedules a 500ms trailing debounce that writes to disk via
#' `save_touched()`. `later::later` is preferred over `shiny::debounce` here
#' because we want a background timer that survives Shiny's flush cycle, not
#' a reactive expression tied to any specific consumer.
#'
#' Called once per Shiny session, after `new_store()`.
#' @noRd
install_autosave <- function(store) {
  shiny::observeEvent(
    store$rv$dirty,
    {
      if (!isTRUE(store$rv$dirty) || is.null(store$rv$path)) {
        return()
      }
      later::later(
        function() {
          # `later` fires OUTSIDE the reactive graph, so bare reads of
          # `store$rv$*` abort with "outside of reactive consumer". Wrap
          # in `isolate()` -- the writes happen inside `save_touched`,
          # which walks the store non-reactively anyway.
          shiny::isolate({
            if (isTRUE(store$rv$dirty) && !is.null(store$rv$path)) {
              tryCatch(
                save_touched(store),
                error = function(e) {
                  log_line(
                    store,
                    sprintf("save failed: %s", conditionMessage(e))
                  )
                }
              )
            }
          })
        },
        delay = 0.5
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )
}

# ---- commit / undo / redo ----------------------------------------------

#' Commit a new report to the store, pushing the prior one onto the undo
#' stack (capped at 50) and clearing the redo stack.
#' @noRd
commit <- function(store, new_report, label = "") {
  store$undo$stack <- c(store$undo$stack, list(store$rv$report))
  n <- length(store$undo$stack)
  if (n > 50L) {
    store$undo$stack <- store$undo$stack[(n - 49L):n]
  }
  store$undo$redo <- list()
  store$rv$report <- new_report
  store$rv$dirty <- TRUE
  invisible(new_report)
}

#' Pop the last undo entry, pushing the current report onto redo.
#' @noRd
undo <- function(store) {
  n <- length(store$undo$stack)
  if (n == 0L) {
    return(invisible(NULL))
  }
  store$undo$redo <- c(store$undo$redo, list(store$rv$report))
  prior <- store$undo$stack[[n]]
  store$undo$stack <- store$undo$stack[-n]
  store$rv$report <- prior
  store$rv$dirty <- TRUE
  invisible(prior)
}

#' Pop the last redo entry, pushing the current report back onto undo.
#' @noRd
redo <- function(store) {
  n <- length(store$undo$redo)
  if (n == 0L) {
    return(invisible(NULL))
  }
  store$undo$stack <- c(store$undo$stack, list(store$rv$report))
  next_report <- store$undo$redo[[n]]
  store$undo$redo <- store$undo$redo[-n]
  store$rv$report <- next_report
  store$rv$dirty <- TRUE
  invisible(next_report)
}

#' Is there an undo entry to pop?
#' @noRd
can_undo <- function(store) {
  length(store$undo$stack) > 0L
}

#' Is there a redo entry to pop?
#' @noRd
can_redo <- function(store) {
  length(store$undo$redo) > 0L
}

# ---- selection / editing ------------------------------------------------

#' The currently selected object, or NULL when nothing is selected.
#' @noRd
selected_object <- function(store) {
  if (is.null(store$rv$selected)) {
    return(NULL)
  }
  .find_object(store$rv$report, store$rv$selected)
}

#' Apply `fn(object) -> object` to the object with `id`, then commit the
#' rebuilt report. Siblings are untouched.
#'
#' **Run semantics (decision #8).** A HEAVY edit -- one that moves the ARD
#' cache key (roles/filters/dataset/type) -- on an output that was READY
#' before the edit marks its proof STALE (`rv$stale`): the paper stops
#' auto re-collecting from DuckDB and shows the stale notice until Run
#' re-typesets. A CHEAP edit (options/title/footnotes -- display-only, the
#' key is unchanged) renders live. A DRAFT output is never marked: filling
#' its last slot is the ghost-fills-into-a-table payoff and must typeset
#' immediately.
#' @noRd
update_object <- function(store, id, fn, label = "") {
  obj <- .find_object(store$rv$report, id)
  if (is.null(obj)) {
    return(invisible(NULL))
  }
  new_obj <- fn(obj)
  # A BROKEN output is exempt: its render failed, so no proof exists to go
  # stale -- the fix-it edit must re-render live (the error-summary ->
  # fixed-table loop), never demand a Run first.
  theme <- store$rv$report@theme
  if (
    !identical(.ard_key(obj, theme), .ard_key(new_obj, theme)) &&
      identical(arpillar::output_status(obj), "ready") &&
      !(id %in% store$rv$broken)
  ) {
    store$rv$stale <- union(store$rv$stale, id)
  }
  # Stamp modified_by/at AND clear any Reviewed signature (every edit
  # invalidates the QC stamp). Undo/redo does NOT stamp -- it just replays
  # a prior snapshot.
  new_obj <- .stamp_meta(new_obj, "modify")
  new_report <- .replace_object(store$rv$report, id, new_obj)
  commit(store, new_report, label = label)
}

#' Append `obj` to the first page, commit, and select it. Shared tail of
#' `add_from_preset()`/`add_from_generator()`.
#' @noRd
.append_and_select <- function(store, obj, label) {
  # Stamp created_by/at + initial modified_by/at on a fresh output.
  obj <- .stamp_meta(obj, "create")
  pages <- store$rv$report@pages
  first <- pages[[1]]
  pages[[1]] <- S7::set_props(first, objects = c(first@objects, list(obj)))
  new_report <- S7::set_props(store$rv$report, pages = pages)
  commit(store, new_report, label = label)
  store$rv$selected <- obj@id
  obj@id
}

#' Add a new output pre-filled from a named preset (`arpillar::preset()`),
#' bind it to `dataset`, select it, and return its freshly minted id.
#'
#' `roles` are rebuilt off `dataset`'s own catalog entry, not copied
#' verbatim from the preset -- a preset var absent from `dataset` still
#' gets a `data_item()` (default `role_type = "category"`), it is never
#' silently dropped, so a mismatched dataset (e.g. an AE-domain preset
#' applied to a dataset without `AEDECOD`) surfaces at validate/render
#' time rather than vanishing here.
#' @noRd
add_from_preset <- function(store, preset_id, dataset) {
  pr <- arpillar::preset(preset_id)
  id <- .next_id(store$rv$report)
  obj <- .object_from_preset(store$con, pr, dataset, id)
  .append_and_select(store, obj, label = "add output from preset")
}

#' Add a bare new output from a generator (`arpillar::generator()`), bind
#' it to `dataset`, select it, and return its freshly minted id.
#'
#' No roles are filled in -- the "blank slate" counterpart to
#' `add_from_preset()`. `options$number` is auto-suggested as the next
#' free `<prefix>.<n>` within the generator's kind group (see
#' `.next_number()`); `options$number_label` is the kind's TLF label
#' ("Table"/"Figure"/"Listing").
#' @noRd
add_from_generator <- function(store, generator_id, dataset) {
  gen <- arpillar::generator(generator_id)
  id <- .next_id(store$rv$report)
  existing_numbers <- vapply(
    .all_objects(store$rv$report),
    function(o) o@options[["number"]] %||% "",
    character(1)
  )
  obj <- .object_from_generator(gen, dataset, id, existing_numbers)
  .append_and_select(store, obj, label = "add output from generator")
}

#' Remove the output with `id`; clear a now-dangling selection.
#' @noRd
remove_output <- function(store, id) {
  new_report <- .remove_object(store$rv$report, id)
  commit(store, new_report, label = "remove output")
  if (identical(store$rv$selected, id)) {
    store$rv$selected <- NULL
  }
  invisible(NULL)
}

#' Move the output with `id` to position `to` within its page.
#' @noRd
move_output <- function(store, id, to) {
  new_report <- .move_object(store$rv$report, id, to)
  commit(store, new_report, label = "move output")
  invisible(NULL)
}

#' Rename the output with `id`.
#' @noRd
rename_output <- function(store, id, title) {
  update_object(
    store,
    id,
    function(o) S7::set_props(o, title = title),
    label = "rename output"
  )
  invisible(NULL)
}

# ---- ARD cache: the two-stage seam ---------------------------------------

#' The ARD memo key for an output: `dataset + type + roles + filters` (+ the
#' resolved population) -- deliberately EXCLUDING the DISPLAY `options`, so a
#' display-only edit (decimals, cutoffs, ...) reuses the already-built ARD
#' instead of recollecting from DuckDB. `options$population` is the one option
#' folded in: it resolves (against `theme`) to a row filter + denominator
#' dataset that genuinely mutate the ARD. The same key doubles as
#' `update_object()`'s cheap/heavy oracle: an edit is HEAVY exactly when it
#' moves this key.
#'
#' Item `label`s are display-only too: `build_ard()` never reads them (the
#' row headers consume them at the render_display stage), so a relabel
#' re-renders live off the memoized ARD instead of stale-marking the proof.
#' `role_type` stays in the key -- it genuinely changes the ARD (measure
#' stats vs category counts).
#' @noRd
.ard_key <- function(object, theme = list()) {
  roles <- lapply(object@roles, function(r) {
    list(
      slot = r@slot,
      items = lapply(r@items, function(it) {
        list(name = it@name, role_type = it@role_type)
      })
    )
  })
  parts <- list(
    object@dataset,
    object@type,
    roles,
    object@filters
  )
  # Conditional appends: `total` (a pooled arm) and `page_by` (a second
  # grouping level) change the ARD, so they are part of the key -- but
  # ONLY when set, so every earlier object keeps its legacy hash and
  # nothing goes stale on upgrade.
  if (isTRUE(object@options$total)) {
    parts$total <- TRUE
  }
  pb <- object@options$page_by
  if (length(pb) == 1L && !is.na(pb) && nzchar(pb)) {
    parts$page_by <- as.character(pb)
  }
  # A listing bakes these into its SQL pull (ORDER BY / LIMIT / PIVOT), so
  # they change the collected frame, not just the display -- key them.
  # Conditional, so an option-free listing keeps a stable hash.
  if (identical(object@type, "listing")) {
    for (k in c("sort", "limit", "transpose")) {
      v <- object@options[[k]]
      if (length(v) > 0L) {
        parts[[k]] <- v
      }
    }
  }
  # The bound analysis set changes the ARD -- it ANDs a row filter and (for
  # occurrence) picks the denominator dataset -- and it resolves against the
  # THEME, so a Setup edit to the set's filter must invalidate the memo. Fold
  # the RESOLVED set in, but only when a population is bound, so an unbound
  # output keeps its legacy hash.
  pop <- arpillar::resolve_population(object, theme)
  if (!is.null(pop$dataset) || length(pop$filter) > 0L) {
    parts$population <- list(dataset = pop$dataset, filter = pop$filter)
  }
  paste0("ard::", rlang::hash(parts))
}

#' A keyed [arpillar::build_ard()] memo -- the two-stage render seam.
#'
#' Keys come from `.ard_key()` and are prefixed `"ard::"`: the one cache
#' environment is shared with Add-output recommendation memos (`"rec::"`)
#' and Data-mode profiles (`"profile::"`), so a caller counting ARD
#' entries must filter on the prefix, never take a bare `ls()` length.
#' @noRd
cached_ard <- function(store, object) {
  theme <- store$rv$report@theme
  key <- .ard_key(object, theme)
  hit <- store$cache[[key]]
  if (!is.null(hit)) {
    return(hit)
  }
  ard <- arpillar::build_ard(store$con, object, theme)
  store$cache[[key]] <- ard
  ard
}

# ---- data sources (v5, decision #8) --------------------------------------

# The file extensions arframe will register from a folder -- the formats
# arpillar::register_dataset() ingests (parquet directly; xpt/json via
# artoo). Anything else in the folder is skipped.
.DATA_EXTS <- c("parquet", "xpt", "json")

#' The registerable dataset files in `dir` (non-recursive), as a named
#' character vector `c(<name> = <path>)` where the name is the file stem
#' uppercased (the catalog-visible name, e.g. `adsl.parquet` -> `"ADSL"`).
#' @noRd
.folder_datasets <- function(dir) {
  files <- list.files(dir, full.names = TRUE)
  ext <- tolower(tools::file_ext(files))
  keep <- files[ext %in% .DATA_EXTS]
  stats::setNames(keep, toupper(tools::file_path_sans_ext(basename(keep))))
}

#' Mount every registerable dataset in `dir` into the WORK library,
#' recording each dataset's source folder and file kind (keyed by name) and
#' bumping `catalog_nonce`. A name already registered is skipped (not an
#' error) -- re-mounting a folder is idempotent, and a same-named dataset in
#' a second folder keeps the first (a WORK-flat namespace has one binding
#' per name). Returns the number of datasets newly registered.
#' @noRd
.mount_folder <- function(store, dir, folder = basename(normalizePath(dir))) {
  datasets <- .folder_datasets(dir)
  n <- 0L
  for (i in seq_along(datasets)) {
    name <- names(datasets)[[i]]
    path <- datasets[[i]]
    ok <- tryCatch(
      {
        arpillar::register_dataset(store$con, name, path)
        TRUE
      },
      arpillar_error_input = function(e) FALSE
    )
    if (isTRUE(ok)) {
      store$sources[[name]] <- folder
      store$kinds[[name]] <- paste0(".", tolower(tools::file_ext(path)))
      n <- n + 1L
    }
  }
  store$rv$catalog_nonce <- store$rv$catalog_nonce + 1L
  log_line(store, sprintf("mounted %d dataset(s) from %s", n, folder))
  n
}

#' Unmount the dataset `name` from WORK: drop it from the catalog, clear its
#' source-folder and kind entries, and clear a now-dangling focus/grid
#' pointer. Bumps `catalog_nonce`.
#' @noRd
.unmount_dataset <- function(store, name) {
  arpillar::unregister_dataset(store$con, name)
  for (env in list(store$sources, store$kinds)) {
    if (exists(name, envir = env, inherits = FALSE)) {
      rm(list = name, envir = env)
    }
  }
  if (identical(store$rv$data_focus, name)) {
    store$rv$data_focus <- NULL
  }
  store$rv$data_selected <- setdiff(store$rv$data_selected, name)
  if (identical(store$rv$grid_dataset, name)) {
    store$rv$grid_dataset <- NULL
  }
  store$rv$catalog_nonce <- store$rv$catalog_nonce + 1L
  log_line(store, sprintf("removed %s", name))
  invisible(NULL)
}

#' Recompute a row-selection set after a click, given the modifier keys. Plain
#' click single-selects the clicked item; Cmd/Ctrl toggles it in/out; Shift
#' range-selects from the anchor through `ordered` (the display order). Shared
#' by Data's explorer and the Report LoC (same `.ar-dx-row` multi-select).
#' @param clicked *The clicked item id/name.* `<character(1)>`.
#' @param anchor *The current anchor (last non-shift click), or NULL.*
#' @param ordered *All ids/names in display order* (for the shift range).
#' @param current *The current selection set.* `<character>`.
#' @param shift,meta *Modifier keys held.* `<logical(1)>`.
#' @noRd
.select_update <- function(
  clicked,
  anchor,
  ordered,
  current,
  shift = FALSE,
  meta = FALSE
) {
  if (isTRUE(shift) && !is.null(anchor)) {
    a <- match(anchor, ordered)
    b <- match(clicked, ordered)
    if (!is.na(a) && !is.na(b)) {
      return(ordered[seq(min(a, b), max(a, b))])
    }
    return(clicked)
  }
  if (isTRUE(meta)) {
    return(
      if (clicked %in% current) {
        setdiff(current, clicked)
      } else {
        union(current, clicked)
      }
    )
  }
  clicked
}

#' The source folder a dataset was mounted from, or `NA` when arframe did
#' not mount it (a test/demo catalog registered directly).
#' @noRd
.source_folder <- function(store, name) {
  store$sources[[name]] %||% NA_character_
}

#' The source kind (file extension) recorded for a dataset, or `NA`.
#' @noRd
.source_kind <- function(store, name) {
  store$kinds[[name]] %||% NA_character_
}

# ---- logging ------------------------------------------------------------

#' Append a timestamped line onto `rv$log`.
#' @noRd
log_line <- function(store, msg) {
  ts <- format(Sys.time(), "%H:%M:%S")
  store$rv$log <- c(store$rv$log, paste0("[", ts, "] ", msg))
  invisible(NULL)
}

# ---- galley card / docked inspector ------------------------------------

#' Route a region click into the docked inspector (v5): remember the
#' region, switch the inspector to the tab that owns it, and expand a
#' collapsed inspector so the routed click never lands on a folded panel.
#' (`rv$card` stays TRUE for compatibility with the pre-dock float logic
#' until every reader is migrated.)
#' @noRd
open_card <- function(store, region) {
  store$rv$region <- region
  store$rv$insp_tab <- .card_region_group(region)
  store$rv$insp_collapsed <- FALSE
  store$rv$card <- TRUE
  invisible(NULL)
}

#' Close the galley card -- a no-op while the card is pinned.
#' @noRd
close_card <- function(store) {
  if (isTRUE(store$rv$pinned)) {
    return(invisible(NULL))
  }
  store$rv$card <- FALSE
  invisible(NULL)
}

#' Toggle whether the galley card is docked open.
#' @noRd
toggle_pin <- function(store) {
  store$rv$pinned <- !isTRUE(store$rv$pinned)
  invisible(NULL)
}

# ---- Report-mode drill (2026-07-08) ---------------------------------------

#' Drill into an output: open Report mode's paper + inspector on `id`.
#'
#' The Report-mode twin of Data's `grid_dataset` assignment: sets
#' `report_open` (the layout gate the `ar-report-open` class mirrors) AND
#' `selected` (which the paper + inspector render from), so a drill is one
#' atomic move. `drill_close()` returns to the List-of-Contents table.
#' @noRd
drill_open <- function(store, id) {
  store$rv$selected <- id
  store$rv$report_open <- id
  invisible(NULL)
}

#' Close the drill -- back to the List-of-Contents table (`report_open` NULL,
#' so `ar-report-open` drops and the LoC surface fills the report body).
#' @noRd
drill_close <- function(store) {
  store$rv$report_open <- NULL
  invisible(NULL)
}

# ---- v5 panel collapse (decision #8) --------------------------------------

#' Toggle the contents rail between full and the status-dot strip.
#' @noRd
toggle_rail <- function(store) {
  store$rv$rail_collapsed <- !isTRUE(store$rv$rail_collapsed)
  invisible(NULL)
}

#' Toggle the docked inspector between full and folded (2026-07-10 -- the
#' icon strip is gone; the toolbar's `panel_toggle` button and the
#' auto-fold-on-no-selection observer are the two callers).
#' @noRd
toggle_insp <- function(store) {
  store$rv$insp_collapsed <- !isTRUE(store$rv$insp_collapsed)
  invisible(NULL)
}
