# The shared dataset-catalog list surface. Ported from explorer's
# `.list_surface()` (/Users/vignesh/projects/r/explorer/explorer/R/
# manage_catalog.R:261-388) + the matching `.ex-grid` / `.ex-manage-*` CSS
# in inst/www/arframe.css section 15. Mounted by Setup > Sources AND
# Data mode so the two read visually identical: an 8-column CSS-Grid
# table with a sticky header, single-click select, double-click view,
# and Lazy vs In-memory status pills.
#
# Data-shape contract per item (list):
#   id            character(1)  filesystem-safe unique id
#   name          character(1)  dataset name (ADSL, ADAE, ...)
#   folder        character(1)  source folder / library
#   source_format character(1)  parquet | xpt | json
#   n_cols        integer(1)    NA permitted
#   n_rows        integer(1)    NA permitted
#   size_bytes    numeric(1)    NA permitted
#   in_memory     logical(1)    TRUE = materialised, FALSE = lazy on disk
#   modified      character(1)  ISO 8601 timestamp

#' The `.ex-manage-list` shell: filter/toolbar bar + scope header + grid slot.
#' @param ns *The parent module namespace function.* `<function>: required`.
#' @param tools *Right-side toolbar content* (View / Import / Delete etc.).
#'   `<tag/tagList>: optional`.
#' @noRd
.catalog_list_surface <- function(ns, tools = NULL) {
  shiny::div(
    class = "ex-manage-list",
    shiny::div(
      class = "ex-manage-bar",
      shiny::div(
        class = "ex-search",
        .icon("search", 14),
        shiny::textInput(
          ns("catalog_filter"),
          label = NULL,
          placeholder = "Filter"
        )
      ),
      shiny::div(class = "ex-manage-tools", tools)
    ),
    shiny::div(
      class = "ex-manage-header",
      shiny::uiOutput(ns("catalog_count"), inline = TRUE)
    ),
    shiny::uiOutput(ns("catalog_grid"))
  )
}

#' The scope strip: bold scope name + micro-count. Rendered into the
#' `catalog_count` slot of `.catalog_list_surface()`.
#' @noRd
.catalog_count <- function(scope, n) {
  shiny::div(
    class = "ex-manage-count",
    shiny::span(class = "ex-scope", scope),
    shiny::span(
      class = "ex-scope-n",
      sprintf("%d dataset%s", n, if (n == 1L) "" else "s")
    )
  )
}

#' The 8-column grid (Name | Folder | Type | Columns | Rows | Size | Status
#' | Date modified). Rendered into the `catalog_grid` slot.
#' @noRd
.catalog_grid_table <- function(rows) {
  shiny::div(
    class = "ex-grid",
    shiny::div(
      class = "ex-grid-head",
      shiny::div(class = "ex-gcell ex-gcell-name", "Name"),
      shiny::div(class = "ex-gcell", "Folder"),
      shiny::div(class = "ex-gcell", "Type"),
      shiny::div(class = "ex-gcell ex-gcell-num", "Columns"),
      shiny::div(class = "ex-gcell ex-gcell-num", "Rows"),
      shiny::div(class = "ex-gcell ex-gcell-num", "Size"),
      shiny::div(class = "ex-gcell", "Status"),
      shiny::div(class = "ex-gcell", "Date modified")
    ),
    shiny::div(class = "ex-grid-body", rows)
  )
}

#' One grid row: single-click posts `input$catalog_select` (with the item's
#' id), double-click posts `input$catalog_view`. Both are namespaced via
#' `ns`. Callers wire the observers.
#' @noRd
.catalog_grid_row <- function(ns, item, selected = FALSE) {
  id <- item$id %||% item$name
  shiny::div(
    class = paste("ex-grid-row", if (isTRUE(selected)) "selected" else ""),
    `data-ar-cat-id` = id,
    onclick = sprintf(
      "Shiny.setInputValue('%s','%s',{priority:'event'})",
      ns("catalog_select"),
      id
    ),
    ondblclick = sprintf(
      "Shiny.setInputValue('%s','%s',{priority:'event'})",
      ns("catalog_view"),
      id
    ),
    shiny::div(
      class = "ex-gcell ex-gcell-name",
      shiny::span(class = "ex-ftype", .icon("table", 14)),
      shiny::span(class = "ex-item-name", item$name)
    ),
    shiny::div(class = "ex-gcell ex-gcell-folder", .catalog_blank(item$folder)),
    shiny::div(
      class = "ex-gcell",
      .catalog_type_chip(item$source_format %||% item$kind)
    ),
    shiny::div(
      class = "ex-gcell ex-gcell-num",
      .catalog_blank(item$n_cols %||% item$cols)
    ),
    shiny::div(
      class = "ex-gcell ex-gcell-num",
      .catalog_blank(item$n_rows %||% item$rows)
    ),
    shiny::div(
      class = "ex-gcell ex-gcell-num",
      .catalog_bytes(item$size_bytes %||% item$size)
    ),
    shiny::div(
      class = "ex-gcell",
      .catalog_status_pill(isTRUE(item$in_memory %||% item$loaded))
    ),
    shiny::div(class = "ex-gcell", .catalog_date(item$modified))
  )
}

#' A quiet monochrome file-type chip (PARQUET / XPT / JSON).
#' @noRd
.catalog_type_chip <- function(fmt) {
  s <- .catalog_blank(fmt)
  if (!nzchar(s)) {
    return("")
  }
  shiny::span(class = "ex-type-chip", toupper(s))
}

#' The Lazy (on disk) vs In-memory (materialised) status pill. A dot for
#' lazy; the lightning-bolt glyph for in-memory.
#' @noRd
.catalog_status_pill <- function(in_memory) {
  if (isTRUE(in_memory)) {
    return(shiny::span(
      class = "ex-status ex-status-mem",
      title = "Held in memory for fast queries",
      shiny::span(class = "ex-status-ico", "\u26a1"),
      "In memory"
    ))
  }
  shiny::span(
    class = "ex-status ex-status-lazy",
    title = "On disk, read lazily",
    shiny::span(class = "ex-status-dot"),
    "Lazy"
  )
}

# ---- local formatting helpers (self-contained; do not overlap mod_data.R) ---

#' Blank for NA / empty; the value coerced to character otherwise.
#' @noRd
.catalog_blank <- function(x) {
  if (length(x) == 0L || is.na(x)) "" else as.character(x)
}

#' Human byte size (31.2 KB, 1.2 MB); blank for NA.
#' @noRd
.catalog_bytes <- function(n) {
  if (length(n) == 0L || is.na(n)) {
    return("")
  }
  n <- as.numeric(n)
  if (n <= 0) {
    return("0 B")
  }
  units <- c("B", "KB", "MB", "GB", "TB")
  e <- min(floor(log(n, 1024)), length(units) - 1L)
  if (e == 0L) {
    sprintf("%d B", as.integer(n))
  } else {
    sprintf("%.1f %s", n / 1024^e, units[[e + 1L]])
  }
}

#' `2026-06-20T08:54:00` -> `2026-06-20 08:54`; blank for NA.
#' @noRd
.catalog_date <- function(x) {
  if (length(x) == 0L || is.na(x) || !nzchar(as.character(x))) {
    return("")
  }
  s <- sub("T", " ", as.character(x))
  # Strip trailing :seconds when present -- feed reads cleanly at minute-grain.
  sub("(\\d{2}:\\d{2}):\\d{2}.*$", "\\1", s)
}

#' Convert an arpillar catalog data.frame row list to the item shape
#' `.catalog_grid_row()` expects. `folder`/`kind` come from arframe's
#' source-tracking overlay in mod_data.R.
#' @noRd
.catalog_items_from_grid <- function(grid) {
  if (is.null(grid) || nrow(grid) == 0L) {
    return(list())
  }
  lapply(seq_len(nrow(grid)), function(i) {
    r <- grid[i, , drop = FALSE]
    list(
      id = as.character(r$name),
      name = as.character(r$name),
      folder = as.character(r$folder %||% NA_character_),
      source_format = as.character(r$kind %||% NA_character_),
      n_cols = suppressWarnings(as.integer(r$cols)),
      n_rows = suppressWarnings(as.integer(r$rows)),
      size_bytes = suppressWarnings(as.numeric(r$size)),
      in_memory = isTRUE(r$loaded),
      modified = as.character(r$modified %||% NA_character_)
    )
  })
}
