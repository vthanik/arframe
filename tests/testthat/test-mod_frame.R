# The Galley frame: the `.ex-appbar` header (brand + segmented mode
# switch + centered title + actions cluster) atop five mounted mode
# bodies (setup/data/report/qc/logs), shown/hidden by a workspace mode
# class. Server-side, the frame owns mode switching, undo/redo, and the
# report-title edit -- all through the injected store, never local
# reactiveVal state. Redesigned in the Stage 2 rebuild: the old
# `.ar-actbar` rail folded into the header, and the empty
# `.ar-statusbar` shell was deleted (dead chrome).

test_that("mod_frame_ui HTML contains the appbar, segmented mode switch, and all five mode-body containers", {
  ui <- mod_frame_ui(
    "frame",
    report_body = shiny::div("report placeholder"),
    data_body = shiny::div("data placeholder"),
    qc_body = shiny::div("qc placeholder"),
    logs_body = shiny::div("logs placeholder"),
    setup_body = shiny::div("setup placeholder")
  )
  html <- as.character(ui)

  # The explorer-parity appbar skeleton (Stage 2).
  expect_match(html, "ex-appbar", fixed = TRUE)
  expect_match(html, "ex-appbar-brand", fixed = TRUE)
  expect_match(html, "ex-section-switch", fixed = TRUE)
  expect_match(html, "ex-appbar-title", fixed = TRUE)
  expect_match(html, "ex-appbar-actions", fixed = TRUE)
  # One segment per mode, each carrying `data-ar-mode` for bridge.js's
  # delegated click handler.
  expect_match(html, 'data-ar-mode="setup"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="data"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="report"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="qc"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="logs"', fixed = TRUE)
  # Five mounted mode bodies.
  expect_match(html, "ar-body-setup", fixed = TRUE)
  expect_match(html, "ar-body-report", fixed = TRUE)
  expect_match(html, "ar-body-data", fixed = TRUE)
  expect_match(html, "ar-body-qc", fixed = TRUE)
  expect_match(html, "ar-body-logs", fixed = TRUE)
  # Async export: plain action button + hidden download link the server
  # clicks once the zip is ready.
  expect_match(html, 'id="frame-export_btn"', fixed = TRUE)
  expect_match(html, 'id="frame-export_dl"', fixed = TRUE)
  expect_match(html, "ar-hidden-dl", fixed = TRUE)
})

test_that("mod_frame_server: mode click switches; active-mode click toggles the rail", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(mod_frame_server, args = list(store = store), {
    # Startup mode is "data" (2026-07-04), so lead with "report" -- the
    # first click must be a SWITCH, not an active-mode toggle.
    for (m in c("report", "data", "qc", "logs")) {
      session$setInputs(mode = m)
      expect_identical(store$rv$mode, m)
      expect_false(store$rv$rail_collapsed)
      # Clicking the ACTIVE button toggles the adjacent panel instead of
      # re-switching (explorer-style show/hide, 2026-07-04): mode holds,
      # rail collapse flips there and back.
      session$setInputs(mode = m)
      expect_identical(store$rv$mode, m)
      expect_true(store$rv$rail_collapsed)
      session$setInputs(mode = m)
      expect_false(store$rv$rail_collapsed)
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
  expect_match(html, "ex-appbar", fixed = TRUE)
  expect_match(html, "ex-section-switch", fixed = TRUE)
  expect_match(html, 'data-ar-mode="setup"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="data"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="report"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="qc"', fixed = TRUE)
  expect_match(html, 'data-ar-mode="logs"', fixed = TRUE)
  expect_match(html, "ar-body-setup", fixed = TRUE)
  expect_match(html, "ar-body-report", fixed = TRUE)
  expect_match(html, "ar-body-data", fixed = TRUE)
  expect_match(html, "ar-body-qc", fixed = TRUE)
  expect_match(html, "ar-body-logs", fixed = TRUE)
  # Opens in Setup mode (user decision 2026-07-06).
  expect_match(html, "ar-mode-setup", fixed = TRUE)

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
