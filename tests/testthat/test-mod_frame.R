# The Galley frame: app bar + status bar + the three mounted mode bodies
# (report/data/qc), shown/hidden by a workspace mode class. Server-side, the
# frame owns mode switching, undo/redo, and the report-title edit -- all
# through the injected store, never local reactiveVal state.

test_that("mod_frame_ui HTML contains the bar, mode buttons, statusbar, and all three mode-body containers", {
  ui <- mod_frame_ui(
    "frame",
    report_body = shiny::div("report placeholder"),
    data_body = shiny::div("data placeholder"),
    qc_body = shiny::div("qc placeholder")
  )
  html <- as.character(ui)

  expect_match(html, "ar-bar", fixed = TRUE)
  expect_match(html, 'data-ar-mode="data"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="qc"', fixed = TRUE)
  expect_match(html, "ar-statusbar", fixed = TRUE)
  expect_match(html, "ar-body-report", fixed = TRUE)
  expect_match(html, "ar-body-data", fixed = TRUE)
  expect_match(html, "ar-body-qc", fixed = TRUE)
})

test_that("mod_frame_server: input$mode sets rv$mode", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(mod_frame_server, args = list(store = store), {
    session$setInputs(mode = "data")
    expect_identical(store$rv$mode, "data")
    # Clicking the active mode again toggles back to Report (home is always
    # reachable from the two-button bar).
    session$setInputs(mode = "data")
    expect_identical(store$rv$mode, "report")
    # A different mode switches straight to it.
    session$setInputs(mode = "qc")
    expect_identical(store$rv$mode, "qc")
  })
})

test_that("mod_frame_server: input$name commits the report name and undo restores it", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(mod_frame_server, args = list(store = store), {
    expect_false(can_undo(store))
    session$setInputs(name = "Renamed report")
    expect_identical(store$rv$report@name, "Renamed report")
    expect_true(can_undo(store))

    undo(store)
    expect_identical(store$rv$report@name, "Untitled report")
  })
})

# ---- end-to-end: the real arframe() launcher, a real browser ---------------

test_that("arframe() launches: bar, mode buttons, all three bodies, mode switch, screenshot", {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("apps/frame"),
    name = "frame",
    height = 900,
    width = 1440
  )
  withr::defer(app$stop())

  html <- app$get_html("body", outer_html = TRUE)
  expect_match(html, "ar-bar", fixed = TRUE)
  expect_match(html, 'data-ar-mode="data"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="qc"', fixed = TRUE)
  expect_match(html, "ar-statusbar", fixed = TRUE)
  expect_match(html, "ar-body-report", fixed = TRUE)
  expect_match(html, "ar-body-data", fixed = TRUE)
  expect_match(html, "ar-body-qc", fixed = TRUE)
  expect_match(html, "ar-mode-report", fixed = TRUE)

  app$click(selector = '[data-ar-mode="qc"]')
  app$wait_for_idle()
  ws_class <- app$get_js("document.querySelector('.ar-workspace').className;")
  expect_match(ws_class, "ar-mode-qc", fixed = TRUE)

  # Dev-only screenshot artifact: .local/ is .Rbuildignore'd, so it does not
  # exist in R CMD check's isolated sandbox copy -- write it only when
  # running from the real source tree, never as a hard requirement.
  screens_dir <- testthat::test_path("../../.local/screens")
  if (dir.exists(screens_dir)) {
    screenshot_path <- file.path(screens_dir, "06-frame.png")
    if (file.exists(screenshot_path)) {
      file.remove(screenshot_path)
    }
    app$get_screenshot(screenshot_path)
  }
})
