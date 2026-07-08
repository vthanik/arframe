# The on-page ghost shell (design spec #4/#7): when an output is not
# "ready" (output_status() != "ready"), the paper renders a COMPLETE page
# shell with a dashed ghost block standing in for each unfilled region,
# instead of an empty canvas or a spinner. Pure and session-free -- no
# Shiny input, no store, no reactive read -- so it is unit-testable with
# zero session mounted, matching the app's "draft state lives in the store,
# never the DOM" discipline (mod_paper.R supplies the click wiring).

# ---- control_id -> region -----------------------------------------------

#' Map one `validate_output()` `control_id` to the page region it belongs
#' on, per the design spec's region table (#4): the treatment-arm slot
#' governs the header band (`columns`), the content slot governs the body
#' rows (`rows`), and a dataset/population/type problem is a title-block
#' concern (`title`) -- there is no dataset picker or type switcher
#' elsewhere on the page. A figure's roles/(figure-only region) all land on
#' `axes`, since the region table lists ONE figure region for the whole
#' x/y/group (or time/censor/group) role set, unlike a table's separate
#' header/body regions.
#' @noRd
.GHOST_REGION_MAP <- c(
  dataset = "title",
  type = "title",
  population = "title",
  "roles-treatment" = "columns",
  "roles-summarize" = "rows",
  "roles-hierarchy" = "rows",
  "roles-x" = "axes",
  "roles-y" = "axes",
  "roles-group" = "axes",
  "roles-time" = "axes",
  "roles-censor" = "axes"
)

#' The region for one `validate_output()` row's `control_id`, defaulting to
#' `"title"` for a control_id this map has not seen (a future oracle
#' addition should still land SOMEWHERE clickable rather than be dropped).
#'
#' `[`-subsetting a named character vector by an unmatched name returns
#' `NA_character_`, not `NULL` -- `%||%` only replaces `NULL`, so the
#' fallback is an explicit `is.na()` check, not a bare `%||%`.
#' @noRd
.ghost_region <- function(control_id) {
  hit <- unname(.GHOST_REGION_MAP[control_id])
  if (is.na(hit)) "title" else hit
}

# ---- slot label lookup ---------------------------------------------------

#' The generator slot label for one `validate_output()` row, lowercased for
#' the ghost hint ("assign treatment arms", not "Assign a treatment
#' variable.") -- `arpillar::generator(type)$slots` is the authority for the
#' display name; `validate_output()`'s own `message` is the fallback for a
#' control_id with no matching slot (`"dataset"`, `"type"`, `"population"`,
#' none of which name a role slot).
#' @noRd
.ghost_hint <- function(type, control_id, message) {
  slot_ids <- sub("^roles-", "", control_id)
  gen <- tryCatch(arpillar::generator(type), error = function(e) NULL)
  if (!is.null(gen)) {
    for (s in gen$slots) {
      if (identical(s$slot, slot_ids)) {
        return(paste0("assign ", tolower(s$label)))
      }
    }
  }
  tolower(sub("\\.$", "", message))
}

# ---- ghost block builder ---------------------------------------------------

#' One dashed ghost block: a `+` glyph and a mono hint, clickable via the
#' same `data-ar-region` attribute a real furniture region carries (see
#' `mod_paper.R`'s region-click delegation) -- clicking an unfilled slot
#' opens the galley card on the region that would fill it, exactly like
#' clicking the eventual real content.
#' @noRd
.ghost_slot <- function(region, hint) {
  # Read-only canvas: the ghost slot is a static "what's still missing"
  # indicator, not a click target (editing happens in the right rail). It
  # keeps the region class for shape/layout, but no `data-ar-region` /
  # `role="button"` / `tabindex` -- nothing on the canvas is interactive.
  shiny::tags$div(
    class = "ar-ghost-slot",
    shiny::span(class = "ar-ghost-plus", "+"),
    shiny::span(class = "ar-ghost-hint ar-mono", hint)
  )
}

# ---- the full shell --------------------------------------------------------

#' The on-page ghost shell for an output that is not ready to render.
#'
#' Walks [arpillar::validate_output()] for `object`, groups the unmet
#' requirements by page region (`.ghost_region()`), and renders ONE ghost
#' block per region in the exact position its real content will occupy: a
#' title-block ghost when the title/dataset/population is the problem, a
#' header-band ghost for an unfilled treatment slot, a body-rows ghost for
#' an unfilled summarize/hierarchy slot, and an axes-frame ghost for a
#' figure's role set. A `type` with no generator (an unrenderable/future
#' type) still renders a title-region ghost rather than an empty page.
#'
#' Pure and session-free: takes only `object`, returns `htmltools` tags, and
#' never reads a Shiny input or the injected store -- unit-testable with
#' zero session mounted.
#' @param object *The output to shell.* `<object>: required`. Any
#'   `arpillar::object`; typically one whose `arpillar::output_status()` is
#'   `"draft"` or `"needs_data"` (a `"ready"` object has nothing to ghost).
#' @return *`<shiny.tag>`.* The ghost page body -- one `.ar-ghost-slot` div
#'   per unmet region, in region order (title, then columns, then rows/axes).
#' @noRd
ghost_shell <- function(object) {
  v <- arpillar::validate_output(object)
  if (nrow(v) == 0L) {
    return(shiny::tagList())
  }
  is_figure <- .is_figure_type(object@type)
  rows <- lapply(seq_len(nrow(v)), function(i) {
    list(
      region = .ghost_region(v$control_id[[i]]),
      hint = .ghost_hint(object@type, v$control_id[[i]], v$message[[i]])
    )
  })
  # One ghost block per DISTINCT region (several unmet requirements can
  # share a region, e.g. two figure roles both land on "axes") -- collapse
  # to the first hint seen for that region rather than stacking duplicate
  # blocks in the same page slot.
  seen <- character(0)
  blocks <- list()
  for (r in rows) {
    if (r$region %in% seen) {
      next
    }
    seen <- c(seen, r$region)
    blocks[[length(blocks) + 1L]] <- .ghost_region_block(
      r$region,
      r$hint,
      is_figure
    )
  }
  shiny::tagList(blocks)
}

#' One region's ghost block, laid out to match where the real furniture
#' would sit: `title` is a slim title-block-shaped ghost, `columns` is a
#' header-band-shaped ghost, `rows`/`axes` is a taller body/axes-frame-
#' shaped ghost (figures get the `ar-ghost-axes` frame styling per the
#' brief's "figure ghost has an axes frame" requirement).
#' @noRd
.ghost_region_block <- function(region, hint, is_figure) {
  shape_class <- switch(
    region,
    title = "ar-ghost-title",
    columns = "ar-ghost-columns",
    if (is_figure) "ar-ghost-axes" else "ar-ghost-rows"
  )
  shiny::tagAppendAttributes(
    .ghost_slot(region, hint),
    class = shape_class
  )
}

#' Is `type` one of the figure-kind generators (renders via
#' `render_ggplot()`, not `render_spec()`)? Reads the live
#' `arpillar::generators()` registry rather than a hard-coded id set, so a
#' newly registered figure generator is picked up automatically; an
#' unknown/unrenderable `type` is treated as NOT a figure (falls back to
#' the table-shaped ghost, which is also the fallback shown for a `"type"`
#' validate_output row).
#' @noRd
.is_figure_type <- function(type) {
  gens <- arpillar::generators()
  g <- gens[[type]]
  !is.null(g) && identical(g$kind, "figure")
}
