#' Launch the arframe report builder
#'
#' Opens the local-first Shiny application. This is the scaffold entry point; the
#' submission-native dual-rail report builder is built out on top of it.
#'
#' @param ... Reserved for future launch options.
#'
#' @return Called for its side effect of running the Shiny application; does not
#'   return a value.
#' @export
arframe <- function(...) {
  ui <- bslib::page_fillable(
    bslib::card(
      bslib::card_header("arframe"),
      "Scaffold. The submission-native report builder lands here."
    )
  )
  server <- function(input, output, session) {}
  shiny::shinyApp(ui, server)
}
