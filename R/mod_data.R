# Data mode (design spec #5, decision #8): the datasetviewer "Manage Data"
# surface in the Galley skin. A SOURCES tree (one node per mounted folder /
# library) on the left; the explorer on the right -- a file-manager detail
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
    return("--")
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
#' `catalog_grid()` columns plus arframe's recorded `kind`, filtered to
#' `source` (a library node, or `NULL`/`"*"` for all).
#' @noRd
.explorer_grid <- function(store, source = NULL) {
  grid <- arpillar::catalog_grid(store$con)
  if (nrow(grid) == 0L) {
    return(grid)
  }
  grid$kind <- vapply(
    seq_len(nrow(grid)),
    function(i) .source_kind(store, grid$name[[i]], grid$library[[i]]),
    character(1)
  )
  if (!is.null(source) && !identical(source, "*")) {
    grid <- grid[grid$library == source, , drop = FALSE]
  }
  grid
}

# ---- sources tree ---------------------------------------------------------

#' The SOURCES tree: an "In-memory data" root (the whole catalog) plus one
#' node per distinct library, each carrying its dataset count. The active
#' node (`store$rv$data_source`, `NULL` = the root) gets the selected
#' class. A node posts `input$source` (its library, or `""` for the root)
#' through the delegated `[data-ar-source]` handler.
#' @noRd
.sources_tree <- function(ns, grid, active) {
  libs <- if (nrow(grid) == 0L) character(0) else sort(unique(grid$library))
  root_sel <- if (is.null(active)) "ar-toc-row-sel" else NULL
  nodes <- lapply(libs, function(lib) {
    n <- sum(grid$library == lib)
    sel <- if (identical(active, lib)) "ar-toc-row-sel" else NULL
    shiny::tags$div(
      class = paste("ar-data-src ar-toc-row", sel),
      `data-ar-source` = lib,
      .icon("open", 14),
      shiny::tags$span(class = "ar-mono ar-data-src-name", lib),
      shiny::tags$span(class = "ar-mono ar-data-src-n", n)
    )
  })
  shiny::tagList(
    shiny::tags$div(class = "ar-label", "Sources"),
    shiny::tags$div(
      class = paste("ar-data-src ar-data-src-root ar-toc-row", root_sel),
      `data-ar-source` = "",
      shiny::tags$span(class = "ar-data-src-dot"),
      shiny::tags$span("In-memory data"),
      shiny::tags$span(class = "ar-mono ar-data-src-n", nrow(grid))
    ),
    nodes,
    .action_btn(
      ns("import_folder_tree"),
      shiny::tagList(.icon("plus", 12), "Add folder"),
      variant = "link",
      class = "ar-add-cta"
    )
  )
}

# ---- explorer table -------------------------------------------------------

#' One explorer row. Clicking selects (posts `input$focus`); double-click
#' opens the grid (`input$open`). The focused row carries the selected
#' class. `data-ar-name`/`data-ar-lib` let the delegated handlers name the
#' row without a per-row input.
#' @noRd
.explorer_row <- function(row, focus) {
  is_focus <- !is.null(focus) &&
    identical(focus$name, row$name) &&
    identical(focus$library, row$library)
  status <- if (isTRUE(row$loaded)) "LOADED" else "LAZY"
  shiny::tags$tr(
    class = paste("ar-dx-row", if (is_focus) "ar-dx-row-sel"),
    `data-ar-name` = row$name,
    `data-ar-lib` = row$library,
    shiny::tags$td(class = "ar-mono ar-dx-name", row$name),
    shiny::tags$td(class = "ar-dx-dim", row$library),
    shiny::tags$td(class = "ar-mono ar-dx-dim", row$kind %||% "--"),
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
    shiny::tags$td(class = "ar-mono ar-dx-dim", row$modified %||% "--")
  )
}

#' The explorer detail table: a sortable-looking header row plus one
#' `.explorer_row()` per dataset, or an empty-state line when the source is
#' empty.
#' @noRd
.explorer_table <- function(ns, grid, focus) {
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
  shiny::tags$table(
    class = "ar-dx-table",
    shiny::tags$thead(shiny::tags$tr(
      lapply(head_cells, function(h) shiny::tags$th(h))
    )),
    shiny::tags$tbody(
      lapply(seq_len(nrow(grid)), function(i) {
        .explorer_row(grid[i, , drop = FALSE], focus)
      })
    )
  )
}

# ---- drill grid -----------------------------------------------------------

#' The preview grid for one dataset: a breadcrumb (`< sources / <lib> /
#' <name>`), a column picker (typed badges, all checked), and a sample of
#' rows. `sample_rows()` caps the pull -- the parquet is never fully read.
#' @noRd
.data_grid <- function(ns, store, dataset) {
  name <- dataset$name
  lib <- dataset$library
  sample <- arpillar::sample_rows(store$con, name, n = 100L, library = lib)
  items <- arpillar::data_items(store$con, name, library = lib)
  grid <- .explorer_grid(store, lib)
  total <- grid$rows[grid$name == name][[1]]
  shiny::tags$div(
    class = "ar-dx-grid",
    shiny::tags$div(
      class = "ar-dx-bc ar-mono",
      .action_btn(
        ns("grid_back"),
        "< sources",
        variant = "link",
        class = "ar-dx-bk"
      ),
      shiny::tags$span(" / "),
      shiny::tags$span(class = "ar-dx-dim", lib),
      shiny::tags$span(" / "),
      shiny::tags$span(class = "ar-dx-name", name),
      shiny::tags$span(
        class = "ar-dx-bc-meta",
        sprintf("rows 1-%d of %s", nrow(sample), format(total, big.mark = ","))
      )
    ),
    shiny::tags$div(
      class = "ar-dx-grid-body",
      .column_picker(items),
      .grid_preview(sample)
    )
  )
}

#' The left column picker inside the grid: one checked row per variable,
#' with its type badge (A / # / date). Purely a preview affordance in v1
#' (toggling columns in the grid is a later refinement).
#' @noRd
.column_picker <- function(items) {
  rows <- lapply(seq_len(nrow(items)), function(i) {
    shiny::tags$div(
      class = "ar-colpick-item ar-mono",
      shiny::tags$input(type = "checkbox", checked = "checked"),
      .type_badge(items$type[[i]]),
      items$name[[i]]
    )
  })
  shiny::tags$div(
    class = "ar-colpick",
    shiny::tags$div(
      class = "ar-label",
      sprintf("Columns %d", nrow(items))
    ),
    rows
  )
}

#' The typed variable badge (# = measure, A = category, D = date), from an
#' arpillar `data_items()` type string (`"measure"`/`"category"`/`"date"`).
#' @noRd
.type_badge <- function(type) {
  cls <- switch(
    type,
    measure = "ar-chip-meas",
    date = "ar-chip-date",
    "ar-chip-cat"
  )
  glyph <- switch(type, measure = "#", date = "D", "A")
  shiny::tags$span(class = paste("ar-chip", cls), glyph)
}

#' The grid preview table: a mono data grid of the sampled rows.
#' @noRd
.grid_preview <- function(sample) {
  shiny::tags$div(
    class = "ar-dx-grid-wrap",
    shiny::tags$table(
      class = "ar-dx-data",
      shiny::tags$thead(shiny::tags$tr(
        lapply(names(sample), function(nm) shiny::tags$th(nm))
      )),
      shiny::tags$tbody(
        lapply(seq_len(nrow(sample)), function(i) {
          shiny::tags$tr(lapply(sample[i, , drop = FALSE], function(v) {
            shiny::tags$td(format(v))
          }))
        })
      )
    )
  )
}

# ---- UI -------------------------------------------------------------------

#' The Data-mode UI: the SOURCES rail (left), the manage toolbar, and the
#' explorer main (list or grid, server-rendered). Full-width -- there is no
#' inspector column in Data mode.
#' @param id *The module namespace.* `<character(1)>: required`.
#' @noRd
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "ar-data-rail",
      shiny::uiOutput(ns("sources"))
    ),
    shiny::div(
      class = "ar-data-main",
      shiny::div(
        class = "ar-dx-bar",
        shiny::tags$input(
          id = ns("filter"),
          type = "text",
          class = "ar-dx-filter",
          placeholder = "Filter datasets"
        ),
        shiny::div(class = "ar-bar-spacer"),
        .action_btn(
          ns("view"),
          shiny::tagList(.icon("eye", 13), "View data"),
          variant = "link",
          class = "ar-dx-tb ar-dx-tb-pri"
        ),
        shinyFiles::shinyFilesButton(
          ns("import_file"),
          label = "Import file",
          title = "Choose a dataset file",
          multiple = FALSE,
          class = "ar-dx-tb"
        ),
        shinyFiles::shinyDirButton(
          ns("import_folder"),
          label = "Import folder",
          title = "Choose a study folder",
          class = "ar-dx-tb"
        ),
        .action_btn(
          ns("delete"),
          shiny::tagList(.icon("close", 13), "Delete"),
          variant = "link",
          class = "ar-dx-tb ar-dx-tb-danger"
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
      .sources_tree(ns, grid, store$rv$data_source)
    }) |>
      shiny::bindEvent(store$rv$catalog_nonce, store$rv$data_source)

    # The explorer: the drill grid when a dataset is open, else the detail
    # table for the active source.
    output$explorer <- shiny::renderUI({
      if (!is.null(store$rv$grid_dataset)) {
        return(.data_grid(ns, store, store$rv$grid_dataset))
      }
      grid <- .explorer_grid(store, store$rv$data_source)
      .explorer_table(ns, grid, store$rv$data_focus)
    }) |>
      shiny::bindEvent(
        store$rv$catalog_nonce,
        store$rv$data_source,
        store$rv$data_focus,
        store$rv$grid_dataset
      )

    shiny::observeEvent(input$source, {
      store$rv$data_source <- if (nzchar(input$source)) input$source else NULL
      store$rv$grid_dataset <- NULL
    })

    # `input$focus`/`input$open` carry `list(name=, lib=)` from the
    # delegated row handlers (single vs double click).
    shiny::observeEvent(input$focus, {
      store$rv$data_focus <- list(
        name = input$focus$name,
        library = input$focus$lib
      )
    })

    shiny::observeEvent(input$open, {
      store$rv$grid_dataset <- list(
        name = input$open$name,
        library = input$open$lib
      )
    })

    shiny::observeEvent(input$view, {
      if (!is.null(store$rv$data_focus)) {
        store$rv$grid_dataset <- store$rv$data_focus
      }
    })

    shiny::observeEvent(input$grid_back, {
      store$rv$grid_dataset <- NULL
    })

    shiny::observeEvent(input$delete, {
      f <- store$rv$data_focus
      if (!is.null(f)) {
        .unmount_dataset(store, f$name, f$library)
      }
    })

    # Import folder (tree CTA + toolbar button both target one chooser).
    volumes <- c(home = path.expand("~"), root = "/")
    shinyFiles::shinyDirChoose(input, "import_folder", roots = volumes)
    shinyFiles::shinyFileChoose(input, "import_file", roots = volumes)

    shiny::observeEvent(input$import_folder, {
      dir <- shinyFiles::parseDirPath(volumes, input$import_folder)
      if (length(dir) == 1L && nzchar(dir)) {
        .mount_folder(store, dir)
      }
    })

    shiny::observeEvent(input$import_folder_tree, {
      session$sendCustomMessage(
        "ar-click",
        list(id = ns("import_folder"))
      )
    })

    shiny::observeEvent(input$import_file, {
      picked <- shinyFiles::parseFilePaths(volumes, input$import_file)
      if (nrow(picked) == 1L) {
        path <- picked$datapath[[1]]
        name <- toupper(tools::file_path_sans_ext(basename(path)))
        ok <- tryCatch(
          {
            arpillar::register_dataset(store$con, name, path, library = "WORK")
            TRUE
          },
          arpillar_error_input = function(e) FALSE
        )
        if (isTRUE(ok)) {
          store$kinds[[paste0("WORK::", name)]] <-
            paste0(".", tolower(tools::file_ext(path)))
          store$rv$catalog_nonce <- store$rv$catalog_nonce + 1L
        }
      }
    })

    invisible(NULL)
  })
}
