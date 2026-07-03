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

#' The SOURCES tree: an "In-memory data" root (the whole catalog) plus one
#' node per distinct source folder, each carrying its dataset count. The
#' active node (`store$rv$data_source`, `NULL` = the root) gets the selected
#' class. A node posts `input$source` (its folder, or `""` for the root)
#' through the delegated `[data-ar-source]` handler.
#' @noRd
.sources_tree <- function(ns, grid, active) {
  folders <- if (nrow(grid) == 0L) {
    character(0)
  } else {
    sort(unique(grid$folder[!is.na(grid$folder)]))
  }
  root_sel <- if (is.null(active)) "ar-toc-row-sel" else NULL
  nodes <- lapply(folders, function(fol) {
    n <- sum(!is.na(grid$folder) & grid$folder == fol)
    sel <- if (identical(active, fol)) "ar-toc-row-sel" else NULL
    shiny::tags$div(
      class = paste("ar-data-src ar-toc-row", sel),
      `data-ar-source` = fol,
      .icon("open", 14),
      shiny::tags$span(class = "ar-mono ar-data-src-name", fol),
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
#' opens the grid (`input$open`). The focused row (matched by dataset name)
#' carries the selected class. `data-ar-name` lets the delegated handlers
#' name the row without a per-row input.
#' @noRd
.explorer_row <- function(row, focus) {
  is_focus <- !is.null(focus) && identical(focus, row$name)
  status <- if (isTRUE(row$loaded)) "LOADED" else "LAZY"
  shiny::tags$tr(
    class = paste("ar-dx-row", if (is_focus) "ar-dx-row-sel"),
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
#' <name>`), a left side pane (column list + property panel), and a sortable
#' sample of rows. `sample_rows()` caps the pull -- the parquet is never fully
#' read; column labels/formats come from `.dataset_meta()` (memoized artoo
#' read), the property panel and sort are pure client-side view state.
#' @noRd
.data_grid <- function(ns, store, name) {
  folder <- .source_folder(store, name)
  n <- store$rv$grid_n
  sample <- arpillar::sample_rows(store$con, name, n = n)
  meta <- .dataset_meta(store, name)
  grid <- arpillar::catalog_grid(store$con)
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
      shiny::tags$span(
        class = "ar-dx-dim",
        if (is.na(folder)) "WORK" else folder
      ),
      shiny::tags$span(" / "),
      shiny::tags$span(class = "ar-dx-name", name),
      shiny::tags$span(
        class = "ar-dx-bc-meta",
        sprintf("rows 1-%d of %s", nrow(sample), format(total, big.mark = ","))
      ),
      shiny::tags$div(class = "ar-bar-spacer"),
      .sample_size_select(ns, n)
    ),
    shiny::tags$div(
      class = "ar-dx-grid-body",
      shiny::tags$div(
        class = "ar-dx-side",
        .column_picker(meta),
        .property_panel(meta)
      ),
      .grid_preview(sample, meta)
    )
  )
}

# The preset preview sizes offered by the sample-size selector.
.SAMPLE_SIZES <- c(50L, 100L, 250L, 500L, 1000L)

#' The sample-size selector: a compact `<select>` in the grid breadcrumb that
#' sets how many rows the preview pulls (`store$rv$grid_n`). Changing it posts
#' `grid_n` and re-renders the grid off a fresh `sample_rows()`. A `<select>`
#' (not a free numeric input) keeps the pull bounded to a few sane presets --
#' the preview is a sample, never the whole dataset.
#' @noRd
.sample_size_select <- function(ns, current) {
  shiny::tags$label(
    class = "ar-dx-nsel-wrap ar-mono",
    "Show",
    shiny::tags$select(
      class = "ar-dx-nsel",
      onchange = sprintf(
        "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
        ns("grid_n")
      ),
      lapply(.SAMPLE_SIZES, function(k) {
        shiny::tags$option(
          value = k,
          selected = if (k == current) "selected" else NULL,
          format(k, big.mark = ",")
        )
      })
    ),
    "rows"
  )
}

#' The left column picker inside the grid: one row per variable -- type badge,
#' name, and (when present) its SAS label. Each row carries the variable's
#' full metadata as `data-ar-*` attributes so clicking it fills the property
#' panel with zero server round-trip (arframe.js). The checkbox is a preview
#' affordance (column toggling in the grid is a later refinement).
#' @noRd
.column_picker <- function(meta) {
  rows <- lapply(seq_len(nrow(meta)), function(i) {
    shiny::tags$div(
      class = paste(
        "ar-colpick-item ar-mono",
        if (i == 1L) "ar-colpick-item-sel"
      ),
      `data-ar-col` = meta$name[[i]],
      `data-ar-label` = meta$label[[i]],
      `data-ar-type` = meta$type[[i]],
      `data-ar-len` = meta$length[[i]],
      `data-ar-fmt` = meta$format[[i]],
      shiny::tags$input(type = "checkbox", checked = "checked"),
      .type_badge(meta$type[[i]]),
      shiny::tags$span(class = "ar-colpick-name", meta$name[[i]]),
      if (nzchar(meta$label[[i]])) {
        shiny::tags$span(class = "ar-colpick-label", meta$label[[i]])
      }
    )
  })
  shiny::tags$div(
    class = "ar-colpick",
    shiny::tags$div(
      class = "ar-label",
      sprintf("Columns %d", nrow(meta))
    ),
    rows
  )
}

#' The SAS-Studio-style property panel: a Property/Value table for the active
#' column (the datasetviewer pattern). Server-rendered for the FIRST column so
#' it is populated before any interaction; arframe.js rewrites the `tbody` on
#' every column-picker click from that row's `data-ar-*` attributes.
#' @noRd
.property_panel <- function(meta) {
  if (nrow(meta) == 0L) {
    return(NULL)
  }
  shiny::tags$div(
    class = "ar-prop",
    shiny::tags$div(class = "ar-label", "Property"),
    shiny::tags$table(
      class = "ar-prop-table ar-mono",
      shiny::tags$tbody(class = "ar-prop-body", .property_rows(meta, 1L))
    )
  )
}

#' The five property rows (Label / Name / Type / Length / Format) for row `i`
#' of a metadata frame. Kept a separate helper so the initial server render
#' and arframe.js agree on the exact field set and order. `type` is mapped to
#' the SAS-facing word (Numeric / Character / Date).
#' @noRd
.property_rows <- function(meta, i) {
  type_word <- switch(
    meta$type[[i]],
    measure = "Numeric",
    date = "Date",
    "Character"
  )
  props <- list(
    c("Label", meta$label[[i]]),
    c("Name", meta$name[[i]]),
    c("Type", type_word),
    c("Length", meta$length[[i]]),
    c("Format", meta$format[[i]])
  )
  lapply(props, function(p) {
    shiny::tags$tr(
      shiny::tags$td(class = "ar-prop-k", p[[1]]),
      shiny::tags$td(
        class = "ar-prop-v",
        if (nzchar(p[[2]])) p[[2]] else "\u2014"
      )
    )
  })
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

#' The grid preview table: a mono data grid of the sampled rows, with
#' click-to-sort headers. Each `<th>` carries `data-ar-sort` (the column
#' name) and `data-ar-sort-type` (measure/category/date) so arframe.js sorts
#' the 100-row sample client-side -- numerically for a measure, lexically
#' otherwise, cycling asc -> desc -> original. Each `<tr>` keeps its original
#' index (`data-ar-orig`) so the "original" state restores without a
#' re-render. Sorting a SAMPLE is honest for a preview -- the full dataset is
#' never pulled.
#' @noRd
.grid_preview <- function(sample, meta) {
  types <- stats::setNames(meta$type, meta$name)
  shiny::tags$div(
    class = "ar-dx-grid-wrap",
    shiny::tags$table(
      class = "ar-dx-data",
      `data-ar-grid` = "true",
      shiny::tags$thead(shiny::tags$tr(
        lapply(names(sample), function(nm) {
          shiny::tags$th(
            class = "ar-dx-th",
            `data-ar-sort` = nm,
            `data-ar-sort-type` = types[[nm]] %||% "category",
            shiny::tags$span(class = "ar-dx-th-name", nm),
            shiny::tags$span(class = "ar-dx-sort-caret")
          )
        })
      )),
      shiny::tags$tbody(
        lapply(seq_len(nrow(sample)), function(i) {
          shiny::tags$tr(
            `data-ar-orig` = i - 1L,
            lapply(sample[i, , drop = FALSE], function(v) {
              shiny::tags$td(format(v))
            })
          )
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
        store$rv$grid_dataset,
        store$rv$grid_n
      )

    # The Data body starts CSS-hidden (Report is the default mode), and the
    # mode switch is a pure client-side class flip the server never sees --
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

    # `input$focus`/`input$open` carry the dataset name from the delegated
    # row handlers (single vs double click).
    shiny::observeEvent(input$focus, {
      store$rv$data_focus <- input$focus
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

    # Sample-size selector: re-pull the preview at the chosen row count. Guard
    # against a value outside the offered presets (only those can be posted by
    # the select, but a stray input should never crash the render).
    shiny::observeEvent(input$grid_n, {
      n <- suppressWarnings(as.integer(input$grid_n))
      if (!is.na(n) && n %in% .SAMPLE_SIZES) {
        store$rv$grid_n <- n
      }
    })

    shiny::observeEvent(input$delete, {
      if (!is.null(store$rv$data_focus)) {
        .unmount_dataset(store, store$rv$data_focus)
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
