# Dataset column metadata (Data mode): the SAS-style Label / Type / Length /
# Format for each variable. The DuckDB catalog's SQL metadata does NOT surface
# variable labels (`arpillar::data_items()` returns them as NA), but they DO
# survive in the on-disk file -- a labelled parquet, or the parquet arpillar
# converts an xpt into. artoo reads the file's schema (labels/formats and all)
# without loading rows (`n_max = 0`), so this is the source for Data mode's
# column labels and the property panel. artoo is the same metadata reader
# datasetviewer uses; a dataset with no readable path (or an unreadable file)
# falls back to `data_items()` (name + type only, blank label/format).

#' Blank out NAs in a vector coerced to character (a `""` cell reads better in
#' a property panel than `NA`).
#' @noRd
.blank_na <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

#' Uncached read of a dataset's column metadata: artoo's schema off the
#' on-disk file joined onto the catalog's own column set (`data_items()` is
#' authoritative for which columns exist and their arframe type), or the
#' `data_items()` fallback when there is no readable path / the file cannot
#' be read. Returns a `data.frame(name, label, type, length, format)`.
#' @noRd
.read_dataset_meta <- function(store, name) {
  di <- arpillar::data_items(store$con, name)
  path <- tryCatch(
    arpillar::dataset_path(store$con, name),
    error = function(e) NA_character_
  )
  cols <- if (!is.na(path) && nzchar(path)) {
    tryCatch(
      artoo::columns(artoo::read_dataset(path, n_max = 0L)),
      error = function(e) NULL
    )
  } else {
    NULL
  }
  if (is.null(cols) || nrow(cols) == 0L) {
    return(.meta_from_items(di))
  }
  # data_items() drives the row set + order (the catalog is authoritative for
  # which columns exist); artoo supplies label/length/format matched by name.
  idx <- match(di$name, cols$Variable)
  data.frame(
    name = di$name,
    label = .blank_na(cols$Label[idx]),
    type = di$type,
    length = .blank_na(cols$Len[idx]),
    format = .blank_na(cols$Format[idx]),
    stringsAsFactors = FALSE
  )
}

#' The fallback metadata frame from `data_items()` alone -- name + type, with
#' whatever label the catalog happens to carry (usually blank) and no
#' length/format.
#' @noRd
.meta_from_items <- function(di) {
  data.frame(
    name = di$name,
    label = .blank_na(di$label),
    type = di$type,
    length = "",
    format = "",
    stringsAsFactors = FALSE
  )
}

#' Column metadata for a dataset, memoized per dataset in the store.
#'
#' Returns a `data.frame(name, label, type, length, format)` -- `type` in
#' arframe's `measure`/`category`/`date` vocabulary (so the existing type
#' badge keeps working). Read once per dataset from the on-disk schema and
#' cached in `store$meta`; `.unmount_dataset()` clears the entry.
#' @noRd
.dataset_meta <- function(store, name) {
  hit <- store$meta[[name]]
  if (!is.null(hit)) {
    return(hit)
  }
  meta <- .read_dataset_meta(store, name)
  store$meta[[name]] <- meta
  meta
}
