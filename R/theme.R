# The Galley design system: the bslib brand theme (Bootstrap-level variables)
# plus the <head> link to the extended token + component layer in
# inst/www/tokens.css + inst/www/arframe.css. The extended tokens (surfaces,
# ink scale, stamps, spacing, motion) live in tokens.css, not here, so they
# can be referenced by hand-rolled chrome that bslib does not own. See
# docs/superpowers/specs/2026-07-02-arframe-galley-design-system.md #1-2.

#' The Galley bslib theme (ink-stamp blue, IBM Plex, 2px radius).
#'
#' Sets only the Bootstrap-level brand variables. The desk/paper/chrome
#' surfaces, the ink scale, stamps, chips, spacing, and motion tokens live in
#' `inst/www/tokens.css` (linked via `.head_assets()`). IBM Plex Sans / Mono
#' are self-hosted via `@font-face` in that stylesheet.
#' @noRd
ar_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#2D5FA8",
    bg = "#FFFFFF",
    fg = "#1B1F23",
    danger = "#B3261E",
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
    "border-radius" = "2px"
  )
}

#' Path to a packaged `www/` asset (a mockable seam for the absent-asset guard).
#' @noRd
.asset_path <- function(file) {
  system.file("www", file, package = "arframe")
}

#' The Galley chrome stylesheets, served from the package `www/` and linked
#' into `<head>`. Linked (not inlined) so the `@font-face` rules' relative
#' `url("fonts/...")` resolve against the stylesheet URL and the self-hosted
#' IBM Plex files load. Registers the `arwww` resource path once per session.
#' @noRd
.head_assets <- function() {
  www <- system.file("www", package = "arframe")
  if (!nzchar(www)) {
    return(NULL)
  }
  shiny::addResourcePath("arwww", www)
  htmltools::tags$head(
    htmltools::tags$link(rel = "stylesheet", href = "arwww/tokens.css"),
    htmltools::tags$link(rel = "stylesheet", href = "arwww/arframe.css")
  )
}
