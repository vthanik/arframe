# Accessibility audit (plan Task 17): assert the a11y contract on rendered
# module HTML -- colour never carries status alone (every stamp repeats its
# status as an aria-label sentence), every icon-only control names itself, and
# the render-error summary announces itself. Pure HTML-string assertions, no
# session mounted.

test_that("every status stamp carries an aria-label sentence", {
  for (s in c("ready", "draft", "needs_data", "broken", "stale")) {
    html <- as.character(.stamp(s))
    expect_match(html, "aria-label=", fixed = TRUE)
  }
})

test_that("the app bar's icon-only undo/redo buttons are labelled", {
  html <- as.character(mod_frame_ui(
    "frame",
    report_body = shiny::div(),
    data_body = shiny::div(),
    qc_body = shiny::div()
  ))
  expect_match(html, 'aria-label="Undo"', fixed = TRUE)
  expect_match(html, 'aria-label="Redo"', fixed = TRUE)
  # The server-clicked hidden download link is removed from the a11y tree.
  expect_match(html, "ar-hidden-dl", fixed = TRUE)
})

test_that("the docked inspector's icon-only controls are labelled", {
  html <- as.character(mod_card_ui("card"))
  expect_match(html, 'aria-label="Collapse inspector"', fixed = TRUE)
  expect_match(html, 'aria-label="Expand inspector"', fixed = TRUE)
  expect_match(html, 'aria-label="View reproduction code"', fixed = TRUE)
})

test_that("the TOC row kebab is labelled", {
  html <- as.character(.toc_kebab(
    shiny::NS("contents"),
    list(id = "o1", title = "X")
  ))
  expect_match(html, 'aria-label="Output actions"', fixed = TRUE)
})

test_that("the render-error summary announces itself with role=alert", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  obj <- arpillar::object(
    id = "o1",
    type = "summary",
    title = "X",
    dataset = "ADSL"
  )
  html <- as.character(
    .error_summary(shiny::NS("paper"), "paper-problem", obj, "Boom.")
  )
  expect_match(html, 'role="alert"', fixed = TRUE)
})
