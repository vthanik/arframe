# The Galley design system: the bslib brand theme (Bootstrap-level variables)
# plus the <head> link to the extended token + component layer in
# inst/www/tokens.css + inst/www/arframe.css. The extended tokens (surfaces,
# ink scale, stamps, spacing, motion) live in tokens.css, not here, so they
# can be referenced by hand-rolled chrome that bslib does not own. See
# docs/superpowers/specs/2026-07-02-arframe-galley-design-system.md #1-2.

#' The Galley bslib theme — Linear-family aesthetic (indigo accent,
#' near-monochrome ink, 4px radius, IBM Plex Sans Regular).
#'
#' Sets only the Bootstrap-level brand variables. The desk/paper/chrome
#' surfaces, ink scale, stamps, chips, spacing, and motion tokens live in
#' `inst/www/tokens.css` (linked via `.head_assets()`). IBM Plex Sans
#' Regular (sans) + IBM Plex Mono are self-hosted via `@font-face` in
#' that stylesheet. Palette + radius values here mirror the `--ar-*`
#' tokens so bslib-owned surfaces (form controls, modals, dropdown
#' menus) match arframe's hand-rolled chrome.
#' @noRd
ar_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#0378cd",
    bg = "#ffffff",
    fg = "#08090a",
    danger = "#dc2626",
    success = "#16a34a",
    warning = "#b45309",
    base_font = bslib::font_collection(
      "IBM Plex Sans",
      "system-ui",
      "-apple-system",
      "sans-serif"
    ),
    heading_font = bslib::font_collection(
      "IBM Plex Sans",
      "system-ui",
      "sans-serif"
    ),
    code_font = bslib::font_collection(
      "IBM Plex Mono",
      "ui-monospace",
      "SFMono-Regular",
      "Menlo",
      "monospace"
    ),
    "border-radius" = "4px",
    "btn-border-radius" = "4px",
    "input-border-radius" = "4px"
  )
}

#' Path to a packaged `www/` asset (a mockable seam for the absent-asset guard).
#' @noRd
.asset_path <- function(file) {
  system.file("www", file, package = "arframe")
}

#' The Galley chrome stylesheets and the client bridge script, served from
#' the package `www/` and linked into `<head>`. Linked (not inlined) so the
#' `@font-face` rules' relative `url("fonts/...")` resolve against the
#' stylesheet URL and the self-hosted IBM Plex files load. Registers the
#' `arwww` resource path once per session.
#' @noRd
.head_assets <- function() {
  www <- system.file("www", package = "arframe")
  if (!nzchar(www)) {
    return(NULL)
  }
  shiny::addResourcePath("arwww", www)
  htmltools::tags$head(
    htmltools::tags$link(rel = "stylesheet", href = "arwww/tokens.css"),
    htmltools::tags$link(rel = "stylesheet", href = "arwww/arframe.css"),
    htmltools::tags$script(src = "arwww/Sortable.min.js"),
    htmltools::tags$script(src = "arwww/arframe.bundle.js"),
    # shinyFiles' folder-tree chevrons + toolbar icons are Font Awesome
    # <i class="fa fa-*"> HTML -- needs the FA webfont CSS to render, not
    # our SVG helper. Attach via a `shiny::icon()` dummy so htmltools
    # pulls its font-awesome dependency into <head>.
    htmltools::attachDependencies(
      htmltools::tags$span(style = "display:none;", shiny::icon("folder")),
      htmltools::findDependencies(shiny::icon("folder"))
    )
  )
}
