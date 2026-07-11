# Data mode (design spec #5, decision #8): the datasetviewer "Manage Data"
# surface in the Galley skin. A SOURCES tree (one node per mounted folder /
# library) on the left; the explorer on the right — a file-manager detail
# table (name / folder / kind / cols / rows / size / status / modified) that
# drills into a preview grid with a column picker. Full-width: the inspector
# drops out (a dataset explorer needs the horizontal room). Every mutation
# runs through the store; the catalog itself is arpillar's (no DBI here).

# ---- formatting helpers ---------------------------------------------------

#' Human byte size: `31.2 KB`, `1.2 MB`, `681.8 KB`. Bytes under 1 KB show
#' as `N B`.
#' @noRd
.fmt_bytes <- function(n) {
  if (is.na(n)) {
    return("\u2014")
  }
  units <- c("B", "KB", "MB", "GB")
  i <- if (n <= 0) 1L else min(length(units), floor(log(n, 1024)) + 1L)
  val <- n / 1024^(i - 1L)
  if (i == 1L) {
    sprintf("%d B", as.integer(n))
  } else {
    sprintf("%.1f %s", val, units[i])
  }
}

#' The catalog grid enriched for the explorer: the arpillar
#' `catalog_grid()` columns plus arframe's recorded source `folder` and
#' `kind` (all datasets live in WORK; the folder is UI provenance), filtered
#' to `source` (a folder node, or `NULL`/`"*"` for all).
#' @noRd
.explorer_grid <- function(store, source = NULL) {
  grid <- arpillar::catalog_grid(store$con)
  if (nrow(grid) == 0L) {
    grid$folder <- character(0)
    grid$kind <- character(0)
    return(grid)
  }
  grid$folder <- vapply(
    grid$name,
    function(n) .source_folder(store, n),
    character(1)
  )
  grid$kind <- vapply(
    grid$name,
    function(n) .source_kind(store, n),
    character(1)
  )
  if (!is.null(source) && !identical(source, "*")) {
    grid <- grid[!is.na(grid$folder) & grid$folder == source, , drop = FALSE]
  }
  grid
}

# ---- sources tree ---------------------------------------------------------

#' One clickable dataset row nested under its SOURCES folder. A single click
#' opens the viewer (posts `input$open` via the delegated `[data-ar-open]`
#' handler) so a user can hop between datasets without closing the open one.
#' The currently-open dataset (`open`) carries the selected class.
#' @noRd
.src_dataset_row <- function(name, open) {
  sel <- if (!is.null(open) && identical(open, name)) "ar-data-ds-sel" else NULL
  shiny::tags$div(
    class = paste("ar-data-src ar-data-ds", sel),
    `data-ar-open` = name,
    title = name,
    shiny::tags$span(class = "ar-mono ar-data-ds-name", name)
  )
}

#' The SOURCES tree: an "In-memory data" root (the whole catalog) plus one
#' node per distinct source folder, each carrying its dataset count and its
#' datasets nested beneath it as click-to-open rows. The active node
#' (`store$rv$data_source`, `NULL` = the root) gets the selected class; a node
#' posts `input$source` (its folder, or `""` for the root) through the
#' delegated `[data-ar-source]` handler. `open` (`store$rv$grid_dataset`)
#' lights the dataset currently in the viewer. A folder in `collapsed`
#' (`store$rv$src_collapsed`) hides its dataset children; its chevron posts
#' `input$src_toggle` through the delegated `[data-ar-src-toggle]` handler.
#' @noRd
.sources_tree <- function(
  ns,
  grid,
  active,
  open = NULL,
  collapsed = character(0)
) {
  folders <- if (nrow(grid) == 0L) {
    character(0)
  } else {
    sort(unique(grid$folder[!is.na(grid$folder)]))
  }
  # Datasets with no folder (WORK-only) hang directly under the root.
  orphans <- if (nrow(grid) == 0L) {
    character(0)
  } else {
    sort(grid$name[is.na(grid$folder)])
  }
  root_sel <- if (is.null(active)) "ar-toc-row-sel" else NULL
  nodes <- lapply(folders, function(fol) {
    kids <- sort(grid$name[!is.na(grid$folder) & grid$folder == fol])
    sel <- if (identical(active, fol)) "ar-toc-row-sel" else NULL
    is_collapsed <- fol %in% collapsed
    # Chevron (collapse) and folder body (select) are separate hit zones: the
    # chevron sits OUTSIDE the `[data-ar-source]` node so a toggle click never
    # also filters the main list.
    shiny::tags$div(
      class = paste("ar-src-group", if (is_collapsed) "ar-src-collapsed"),
      shiny::tags$div(
        class = "ar-src-folder-row",
        shiny::tags$button(
          class = "ar-src-toggle",
          type = "button",
          `data-ar-src-toggle` = fol,
          `aria-label` = paste("Toggle", fol),
          .icon("chevron_right", 11)
        ),
        shiny::tags$div(
          class = paste("ar-data-src ar-toc-row", sel),
          `data-ar-source` = fol,
          .icon("open", 14),
          shiny::tags$span(class = "ar-mono ar-data-src-name", fol),
          shiny::tags$span(class = "ar-mono ar-data-src-n", length(kids))
        )
      ),
      if (!is_collapsed) {
        lapply(kids, function(nm) .src_dataset_row(nm, open))
      }
    )
  })
  shiny::tagList(
    # No chevron: re-clicking the active Data item on the
    # activity rail is the one show/hide affordance for the left panel.
    shiny::tags$div(
      class = "ar-src-head",
      shiny::tags$div(class = "ar-label", "Sources")
    ),
    shiny::tags$div(
      class = paste("ar-data-src ar-data-src-root ar-toc-row", root_sel),
      `data-ar-source` = "",
      shiny::tags$span(class = "ar-data-src-dot"),
      shiny::tags$span("In-memory data"),
      shiny::tags$span(class = "ar-mono ar-data-src-n", nrow(grid))
    ),
    lapply(orphans, function(nm) .src_dataset_row(nm, open)),
    nodes
    # "+ Add folder" removed from the rail — the toolbar's
    # "Import folder" is the single add-folder affordance, kept at the top.
  )
}

# ---- explorer table -------------------------------------------------------

#' One explorer row. Clicking selects (posts `input$focus`); double-click
#' opens the grid (`input$open`). The focused row (matched by dataset name)
#' carries the selected class. `data-ar-name` lets the delegated handlers
#' name the row without a per-row input.
#' @noRd
.explorer_row <- function(row, selected) {
  is_sel <- row$name %in% selected
  status <- if (isTRUE(row$loaded)) "LOADED" else "LAZY"
  shiny::tags$tr(
    class = paste("ar-dx-row", if (is_sel) "ar-dx-row-sel"),
    `data-ar-name` = row$name,
    shiny::tags$td(class = "ar-mono ar-dx-name", row$name),
    shiny::tags$td(class = "ar-dx-dim", row$folder %||% "\u2014"),
    shiny::tags$td(class = "ar-mono ar-dx-dim", row$kind %||% "\u2014"),
    shiny::tags$td(
      class = "ar-mono ar-dx-num",
      format(row$cols, big.mark = ",")
    ),
    shiny::tags$td(
      class = "ar-mono ar-dx-num",
      format(row$rows, big.mark = ",")
    ),
    shiny::tags$td(class = "ar-mono ar-dx-num", .fmt_bytes(row$size)),
    shiny::tags$td(shiny::tags$span(class = "ar-lz", status)),
    shiny::tags$td(class = "ar-mono ar-dx-dim", row$modified %||% "\u2014")
  )
}

#' The explorer detail table: a sortable-looking header row plus one
#' `.explorer_row()` per dataset, or an empty-state line when the source is
#' empty.
#' @noRd
.explorer_table <- function(ns, grid, selected) {
  if (nrow(grid) == 0L) {
    return(shiny::tags$div(
      class = "ar-dx-empty ar-mono",
      "No datasets. Import a file or add a folder to begin."
    ))
  }
  head_cells <- c(
    "NAME",
    "FOLDER",
    "KIND",
    "COLS",
    "ROWS",
    "SIZE",
    "STATUS",
    "MODIFIED"
  )
  # COLS/ROWS/SIZE carry right-aligned values; right-align their headers too
  # so each label sits directly above its column instead of floating left.
  num_heads <- c("COLS", "ROWS", "SIZE")
  shiny::tags$table(
    class = "ar-dx-table",
    shiny::tags$thead(shiny::tags$tr(
      lapply(head_cells, function(h) {
        shiny::tags$th(class = if (h %in% num_heads) "ar-dx-num", h)
      })
    )),
    shiny::tags$tbody(
      lapply(seq_len(nrow(grid)), function(i) {
        .explorer_row(grid[i, , drop = FALSE], selected)
      })
    )
  )
}

# ---- drill grid -----------------------------------------------------------

#' The full-dataset viewer for one open dataset: a breadcrumb (`< sources /
#' <lib> / <name>`) above the embedded datasetviewer widget.
#'
#' datasetviewer owns the grid entirely — it loads the dataset into an
#' in-browser DuckDB, renders only the visible rows (virtualized), and does
#' typed sort / filter / column select / a SAS-style property panel
#' client-side. That is what lets Data mode find a specific subject in a large
#' dataset and stay fast where a server-rendered sample table could not: the
#' row work never round-trips to R, and no sample is taken (the whole dataset
#' is queryable). The widget itself is populated by `output$dv` in the server.
#' @noRd
.data_grid <- function(ns, store, name) {
  folder <- .source_folder(store, name)
  shiny::tags$div(
    class = "ar-dx-grid",
    shiny::tags$div(
      class = "ar-dx-bc ar-mono",
      .action_btn(
        ns("grid_back"),
        shiny::tagList(.icon("chevrons_left", 11), "sources"),
        variant = "link",
        class = "ar-dx-bk"
      ),
      shiny::tags$span(class = "ar-dx-sep", "/"),
      shiny::tags$span(
        class = "ar-dx-dim",
        if (is.na(folder)) "WORK" else folder
      ),
      shiny::tags$span(class = "ar-dx-sep", "/"),
      shiny::tags$span(class = "ar-dx-name", name),
      shiny::tags$div(class = "ar-bar-spacer"),
      # The list toolbar (filter/import/delete) is hidden while a dataset is
      # open; this X is the only chrome needed here — close the viewer.
      shiny::tags$button(
        id = ns("grid_close"),
        type = "button",
        class = "ar-icon-btn ar-dx-close action-button",
        `aria-label` = "Close data view",
        .icon("close", 14)
      )
    ),
    shiny::tags$div(
      class = "ar-dx-dv",
      datasetviewer::datasetviewerOutput(ns("dv"), height = "100%")
    )
  )
}

# ---- UI -------------------------------------------------------------------

#' The Data-mode UI: the SOURCES rail (left), the manage toolbar, and the
#' explorer main (list or grid, server-rendered). Full-width — there is no
#' inspector column in Data mode.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "ar-data-rail",
      `data-ar-resizable` = "left",
      shiny::tags$div(
        class = "ar-rail-resize",
        `data-ar-resize` = "left",
        `aria-hidden` = "true"
      ),
      shiny::uiOutput(ns("sources"))
    ),
    shiny::div(
      class = "ar-data-main",
      shiny::div(
        class = "ar-dx-bar",
        shiny::tags$input(
          id = ns("filter"),
          type = "text",
          class = "ar-dx-filter ar-search",
          placeholder = "Filter datasets"
        ),
        shiny::div(class = "ar-bar-spacer"),
        .action_btn(
          ns("view"),
          shiny::tagList(.icon("eye", 13), "View data"),
          variant = "primary",
          class = "ex-btn-sm ar-dx-tb"
        ),
        # shinyFiles buttons escape a tagList label to text — pass the
        # icon through their own `icon` slot instead. `btn-outline-secondary
        # ex-btn-sm` mirrors the Setup > Sources pickers so both on-ramps
        # wear one button dialect.
        shinyFiles::shinyFilesButton(
          ns("import_file"),
          label = "Import file",
          title = "Choose a dataset file",
          multiple = FALSE,
          icon = .icon("import", 13),
          class = "btn btn-outline-secondary ex-btn-sm ar-dx-tb"
        ),
        shinyFiles::shinyDirButton(
          ns("import_folder"),
          label = "Import folder",
          title = "Choose a study folder",
          icon = .icon("folder_plus", 13),
          class = "btn btn-outline-secondary ex-btn-sm ar-dx-tb"
        ),
        .action_btn(
          ns("delete"),
          shiny::tagList(.icon("trash", 13), "Delete"),
          variant = "outline-danger",
          class = "ex-btn-sm ar-dx-tb"
        )
      ),
      shiny::uiOutput(ns("explorer"))
    )
  )
}

# ---- server ---------------------------------------------------------------

#' The Data-mode server: renders the sources tree + explorer (list or
#' grid) off the catalog, and wires source selection, row focus, the
#' view/delete/import actions, and the client-side text filter. Every
#' listing reads under `catalog_nonce`, which every mount/delete bumps.
#' @param id *The module namespace, matching `mod_data_ui()`.*
#'   `<character(1)>: required`.
#' @param store *The injected structured store.* `<list>: required`.
#' @noRd
mod_data_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$sources <- shiny::renderUI({
      grid <- .explorer_grid(store)
      .sources_tree(
        ns,
        grid,
        store$rv$data_source,
        store$rv$grid_dataset,
        store$rv$src_collapsed
      )
    }) |>
      shiny::bindEvent(
        store$rv$catalog_nonce,
        store$rv$data_source,
        store$rv$grid_dataset,
        store$rv$src_collapsed
      )

    # The explorer: the drill grid when a dataset is open, else the detail
    # table for the active source.
    output$explorer <- shiny::renderUI({
      if (!is.null(store$rv$grid_dataset)) {
        return(.data_grid(ns, store, store$rv$grid_dataset))
      }
      grid <- .explorer_grid(store, store$rv$data_source)
      .explorer_table(ns, grid, store$rv$data_selected)
    }) |>
      shiny::bindEvent(
        store$rv$catalog_nonce,
        store$rv$data_source,
        store$rv$data_selected,
        store$rv$grid_dataset
      )

    # The Data body starts CSS-hidden (Report is the default mode), and the
    # mode switch is a pure client-side class flip the server never sees —
    # so Shiny would SUSPEND these outputs forever and Data mode would stay
    # blank. Every mode body is always mounted (the "all mount, CSS picks
    # one" frame contract), so rendering while hidden is correct. Set AFTER
    # both outputs exist (outputOptions errors on an undefined output).
    shiny::outputOptions(output, "sources", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "explorer", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$source, {
      store$rv$data_source <- if (nzchar(input$source)) input$source else NULL
      store$rv$grid_dataset <- NULL
    })

    # A folder chevron toggles its collapsed state in the tree (server-held so
    # a re-render keeps it).
    shiny::observeEvent(input$src_toggle, {
      fol <- input$src_toggle
      cur <- store$rv$src_collapsed
      store$rv$src_collapsed <- if (fol %in% cur) {
        setdiff(cur, fol)
      } else {
        c(cur, fol)
      }
    })

    # A row click carries `{name, shift, meta, nonce}` from the delegated
    # handler. Plain click single-selects; Cmd/Ctrl toggles; Shift range-selects
    # from the anchor (`data_focus`) through the explorer's display order.
    # `data_focus` is the anchor (drives View data + the shift range); it only
    # moves on a non-shift click.
    shiny::observeEvent(input$focus, {
      e <- input$focus
      name <- if (is.list(e)) e$name else e
      if (is.null(name)) {
        return()
      }
      shift <- is.list(e) && isTRUE(e$shift)
      meta <- is.list(e) && isTRUE(e$meta)
      ordered <- .explorer_grid(store, store$rv$data_source)$name
      store$rv$data_selected <- .select_update(
        name,
        store$rv$data_focus,
        ordered,
        store$rv$data_selected,
        shift = shift,
        meta = meta
      )
      if (!shift) {
        store$rv$data_focus <- name
      }
    })

    shiny::observeEvent(input$open, {
      store$rv$grid_dataset <- input$open
    })

    shiny::observeEvent(input$view, {
      if (!is.null(store$rv$data_focus)) {
        store$rv$grid_dataset <- store$rv$data_focus
      }
    })

    shiny::observeEvent(input$grid_back, {
      store$rv$grid_dataset <- NULL
    })

    # The X in the open-dataset breadcrumb closes the viewer (same as the
    # "< sources" back link; the list toolbar is CSS-hidden while it is open).
    shiny::observeEvent(input$grid_close, {
      store$rv$grid_dataset <- NULL
    })

    # The embedded datasetviewer widget for the open dataset: fed the dataset's
    # on-disk file (`dataset_path()` — the parquet arpillar registered/
    # converted, labels intact), the widget does all row work in the browser.
    # Bound to `grid_dataset` so opening a new dataset re-inits it.
    output$dv <- datasetviewer::renderDatasetViewer({
      name <- store$rv$grid_dataset
      shiny::req(name)
      datasetviewer::dataset_viewer(arpillar::dataset_path(store$con, name))
    })

    # Delete: confirm, then unmount every SELECTED dataset (single- or
    # multi-select). Unmount is reversible (files stay; re-import restores),
    # but a bulk drop still warrants the confirm the user asked for.
    shiny::observeEvent(input$delete, {
      sel <- store$rv$data_selected
      if (length(sel) == 0L) {
        return()
      }
      shiny::showModal(.confirm_delete_modal(
        session$ns("confirm_delete"),
        length(sel),
        "dataset",
        "The files stay on disk. Re-import the folder to restore them."
      ))
    })

    shiny::observeEvent(input$confirm_delete, {
      for (nm in store$rv$data_selected) {
        .unmount_dataset(store, nm)
      }
      store$rv$data_selected <- character(0)
      store$rv$data_focus <- NULL
      shiny::removeModal()
    })

    # Import folder (tree CTA + toolbar button both target one chooser).
    volumes <- c(home = path.expand("~"), root = "/")
    shinyFiles::shinyDirChoose(input, "import_folder", roots = volumes)
    shinyFiles::shinyFileChoose(input, "import_file", roots = volumes)

    shiny::observeEvent(input$import_folder, {
      dir <- shinyFiles::parseDirPath(volumes, input$import_folder)
      if (length(dir) == 1L && nzchar(dir)) {
        .mount_folder(store, dir)
        # Show the whole catalog (root) so the just-mounted datasets appear
        # APPENDED to the list, not a filtered view of only the new folder
        # (and not left invisible under the previously active folder).
        store$rv$data_source <- NULL
        store$rv$grid_dataset <- NULL
      }
    })

    shiny::observeEvent(input$import_file, {
      picked <- shinyFiles::parseFilePaths(volumes, input$import_file)
      if (nrow(picked) == 1L) {
        path <- picked$datapath[[1]]
        name <- toupper(tools::file_path_sans_ext(basename(path)))
        ok <- tryCatch(
          {
            arpillar::register_dataset(store$con, name, path)
            TRUE
          },
          arpillar_error_input = function(e) FALSE
        )
        if (isTRUE(ok)) {
          store$sources[[name]] <- "imported"
          store$kinds[[name]] <- paste0(".", tolower(tools::file_ext(path)))
          store$rv$catalog_nonce <- store$rv$catalog_nonce + 1L
        }
      }
    })

    invisible(NULL)
  })
}
