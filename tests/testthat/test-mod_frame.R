# The Galley frame: app bar + activity bar + status bar + the four mounted
# mode bodies (report/data/qc/logs), shown/hidden by a workspace mode class.
# Server-side, the frame owns mode switching, undo/redo, and the report-title
# edit -- all through the injected store, never local reactiveVal state.

test_that("mod_frame_ui HTML contains the bar, activity bar, statusbar, and all four mode-body containers", {
  ui <- mod_frame_ui(
    "frame",
    report_body = shiny::div("report placeholder"),
    data_body = shiny::div("data placeholder"),
    qc_body = shiny::div("qc placeholder"),
    logs_body = shiny::div("logs placeholder")
  )
  html <- as.character(ui)

  expect_match(html, "ar-bar", fixed = TRUE)
  # Piece A: the far-left activity bar carries one icon button per mode
  # destination -- it supersedes the v5 segmented toggle + header QC button.
  expect_match(html, "ar-actbar", fixed = TRUE)
  expect_no_match(html, "ar-seg", fixed = TRUE)
  expect_match(html, 'data-ar-mode="data"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="report"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="qc"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="logs"', fixed = TRUE)
  expect_match(html, "ar-statusbar", fixed = TRUE)
  expect_match(html, "ar-body-report", fixed = TRUE)
  expect_match(html, "ar-body-data", fixed = TRUE)
  expect_match(html, "ar-body-qc", fixed = TRUE)
  expect_match(html, "ar-body-logs", fixed = TRUE)
  # Async export (Task 16): a plain action button (not a download link) plus a
  # hidden download link the server clicks once the zip is ready.
  expect_match(html, 'id="frame-export_btn"', fixed = TRUE)
  expect_match(html, 'id="frame-export_dl"', fixed = TRUE)
  expect_match(html, "ar-hidden-dl", fixed = TRUE)
})

test_that("mod_frame_server: every activity-bar destination is idempotent (piece A)", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(mod_frame_server, args = list(store = store), {
    for (m in c("data", "report", "qc", "logs")) {
      session$setInputs(mode = m)
      expect_identical(store$rv$mode, m)
      # Clicking the ACTIVE button is a no-op -- each destination has its
      # own button, so no hidden "toggle back" behavior remains.
      session$setInputs(mode = m)
      expect_identical(store$rv$mode, m)
    }
  })
})

test_that("mod_frame_server: input$collapse flips rail/inspector state in the store (v5)", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(mod_frame_server, args = list(store = store), {
    session$setInputs(collapse = "rail")
    expect_true(store$rv$rail_collapsed)
    session$setInputs(collapse = "rail")
    expect_false(store$rv$rail_collapsed)

    session$setInputs(collapse = "insp")
    expect_true(store$rv$insp_collapsed)
    # Independent: collapsing the inspector never touches the rail.
    expect_false(store$rv$rail_collapsed)
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
  expect_match(html, "ar-actbar", fixed = TRUE)
  expect_match(html, 'data-ar-mode="data"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="report"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="qc"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="logs"', fixed = TRUE)
  expect_match(html, "ar-statusbar", fixed = TRUE)
  expect_match(html, "ar-body-report", fixed = TRUE)
  expect_match(html, "ar-body-data", fixed = TRUE)
  expect_match(html, "ar-body-qc", fixed = TRUE)
  expect_match(html, "ar-body-logs", fixed = TRUE)
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
