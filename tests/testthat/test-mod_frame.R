# The Galley frame: a top `.ar-topbar` (brand + horizontal mode nav + global
# actions, incl. the centered click-to-edit report title) over five mounted mode
# bodies -- setup/data/report/qc/logs, shown/hidden by a workspace mode class.
# Server-side, the frame owns mode switching, undo/redo, and the report-title
# edit -- all through the injected store, never local reactiveVal state.
# Redesigned 2026-07-07: the left sidebar nav moved back into the top app
# bar; the Refresh/Undo/Redo circle buttons were dropped (undo/redo stay
# keyboard-fed); the nav items still carry `data-ar-mode`.

test_that("mod_frame_ui HTML is a top app bar (mode tabs), a pagehead title, and five mode bodies", {
  ui <- mod_frame_ui(
    "frame",
    report_body = shiny::div("report placeholder"),
    data_body = shiny::div("data placeholder"),
    qc_body = shiny::div("qc placeholder"),
    logs_body = shiny::div("logs placeholder"),
    setup_body = shiny::div("setup placeholder")
  )
  html <- as.character(ui)

  # Top app bar carries the brand, the mode tablist, and the actions cluster.
  expect_match(html, "ar-topbar", fixed = TRUE)
  expect_match(html, "ar-appbar-brand", fixed = TRUE)
  expect_match(html, "ar-nav-item", fixed = TRUE)
  expect_match(html, "ex-appbar-actions", fixed = TRUE)
  # The left sidebar is gone.
  expect_no_match(html, "ar-sidebar", fixed = TRUE)
  # The report title now lives INSIDE the top bar (centered), no separate row.
  expect_match(html, "ar-title-wrap", fixed = TRUE)
  expect_no_match(html, "ar-pagehead", fixed = TRUE)
  # One nav item per mode, each carrying data-ar-mode for bridge.js.
  for (m in c("setup", "data", "report", "qc", "logs")) {
    expect_match(html, sprintf('data-ar-mode="%s"', m), fixed = TRUE)
    expect_match(html, sprintf("ar-body-%s", m), fixed = TRUE)
  }
  # The three circle icons are gone.
  expect_no_match(html, 'id="frame-refresh_btn"', fixed = TRUE)
  expect_no_match(html, 'id="frame-undo_btn"', fixed = TRUE)
  expect_no_match(html, 'id="frame-redo_btn"', fixed = TRUE)
  # Async export button + hidden download link survive.
  expect_match(html, 'id="frame-export_btn"', fixed = TRUE)
  expect_match(html, 'id="frame-export_dl"', fixed = TRUE)
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

test_that("arframe() launches: top app bar nav, all five bodies, per-mode switch + screenshots", {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("apps/frame"),
    name = "frame",
    height = 900,
    width = 1440
  )
  withr::defer(app$stop())

  html <- app$get_html("body", outer_html = TRUE)
  expect_match(html, "ar-topbar", fixed = TRUE)
  expect_match(html, "ar-title-wrap", fixed = TRUE)
  expect_match(html, "ar-nav-item", fixed = TRUE)
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

  # Walk every mode: assert the workspace class flips, and (dev-only) capture a
  # screenshot of each for eyeballing. `.local/screens/` is .Rbuildignore'd, so
  # it is absent in the R CMD check sandbox -- screenshots are written only from
  # the real source tree, never a hard requirement of the assertions.
  screens_dir <- testthat::test_path("../../.local/screens")
  have_screens <- dir.exists(screens_dir)
  modes <- c(setup = "01", data = "02", report = "03", qc = "04", logs = "05")
  for (m in names(modes)) {
    app$click(selector = sprintf('[data-ar-mode="%s"]', m))
    app$wait_for_idle()
    ws_class <- app$get_js("document.querySelector('.ar-workspace').className;")
    expect_match(ws_class, paste0("ar-mode-", m), fixed = TRUE)
    if (have_screens) {
      shot <- file.path(screens_dir, sprintf("%s-%s.png", modes[[m]], m))
      # get_screenshot() will not overwrite, so clear a stale capture first.
      if (file.exists(shot)) {
        file.remove(shot)
      }
      app$get_screenshot(shot)
    }
  }
})
