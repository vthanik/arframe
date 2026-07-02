# The docked inspector (v5, decision #8): fixed-width right panel with the
# Roles/Options/Filters/Ranks tab stack, the Run/.rtf/code action footer,
# and the telemetry line. Tab state lives in the store (`rv$insp_tab`);
# region clicks route to a tab through `open_card()` (tested in
# test-fct_store.R); collapse is frame-owned (test-mod_frame.R).

# A minimal READY summary output on ADSL, selected -- mirrors
# test-mod_paper.R's fixture (kept local per the no-shared-ambient rule).
.mc_ready_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))
  shiny::isolate(update_object(store, id, function(o) {
    S7::set_props(
      o,
      title = "Demographics and Baseline Characteristics",
      options = list(number = "14.1.1", number_label = "Table"),
      footnotes = "Safety Population.",
      roles = list(
        arpillar::role(
          slot = "treatment",
          items = list(arpillar::data_item(name = "TRT01P"))
        ),
        arpillar::role(
          slot = "summarize",
          items = list(
            arpillar::data_item(
              name = "AGE",
              label = "Age (years)",
              role_type = "measure"
            )
          )
        )
      )
    )
  }))
  shiny::isolate(store$rv$selected <- id)
  list(con = con, store = store)
}

test_that("mod_card_ui: tab strip, all four panes, action footer, slim strip", {
  ui <- mod_card_ui("card")
  html <- as.character(ui)

  for (tab in c("roles", "options", "filters", "ranks")) {
    expect_match(html, sprintf('data-ar-insp-tab="%s"', tab), fixed = TRUE)
    expect_match(html, sprintf("ar-insp-pane-%s", tab), fixed = TRUE)
  }
  # Action footer: Run + the per-output .rtf download + code view.
  expect_match(html, 'id="card-run"', fixed = TRUE)
  expect_match(html, 'id="card-rtf"', fixed = TRUE)
  expect_match(html, 'id="card-code"', fixed = TRUE)
  # Both collapse affordances post through the frame's delegated handler.
  expect_match(html, 'data-ar-collapse="insp"', fixed = TRUE)
  expect_match(html, "ar-insp-slim", fixed = TRUE)
  # No float-era chrome (v5): pin and close are gone.
  expect_no_match(html, "ar-card-pin", fixed = TRUE)
  expect_no_match(html, 'id="card-close"', fixed = TRUE)
})

test_that("mod_card_ui: the Ranks pane is the Task-11 placeholder row", {
  html <- as.character(mod_card_ui("card"))
  # Ranks is a stub BY DESIGN (plan: "Ranks stub -> T11"): a quiet disabled
  # row naming what will fill it, plus the coming tag.
  expect_match(
    html,
    "Top-N and incidence cutoffs arrive with the AE hierarchy table",
    fixed = TRUE
  )
  expect_match(html, "ar-tag-coming", fixed = TRUE)
})

test_that("mod_card_server: tab clicks route rv$insp_tab", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_server, args = list(store = fx$store), {
    session$setInputs(tab_options = 1)
    expect_identical(store$rv$insp_tab, "options")
    session$setInputs(tab_filters = 1)
    expect_identical(store$rv$insp_tab, "filters")
    session$setInputs(tab_roles = 1)
    expect_identical(store$rv$insp_tab, "roles")
  })
})

test_that("mod_card_server: Run drops the ARD memo and bumps run_nonce", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_server, args = list(store = fx$store), {
    # Prime the memo through the same seam the paper uses.
    obj <- shiny::isolate(selected_object(store))
    invisible(cached_ard(store, obj))
    expect_length(grep("^ard::", ls(store$cache)), 1L)

    # A heavy edit marked the proof stale; Run must clear it (decision #8).
    obj_id <- shiny::isolate(store$rv$selected)
    store$rv$stale <- obj_id

    session$setInputs(run = 1)
    expect_length(grep("^ard::", ls(store$cache)), 0L)
    expect_identical(store$rv$run_nonce, 1L)
    expect_identical(store$rv$stale, character(0))
    # The run is logged for the QC sheet.
    expect_match(store$rv$log[[length(store$rv$log)]], "run", fixed = TRUE)
  })
})

test_that("mod_card_server: the code button toggles rv$code_view (v5)", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_server, args = list(store = fx$store), {
    expect_false(store$rv$code_view)
    session$setInputs(code = 1)
    expect_true(store$rv$code_view)
    session$setInputs(code = 2)
    expect_false(store$rv$code_view)
  })
})

test_that("mod_card_server: the .rtf download names and writes a non-empty RTF", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_server, args = list(store = fx$store), {
    # In testServer the download handler RUNS: output$rtf is the written
    # temp file's path, named by the handler's own filename function.
    path <- output$rtf
    expect_identical(
      basename(path),
      "t-14-1-1-demographics-and-baseline-characteristics.rtf"
    )
    expect_gt(file.size(path), 0)
    first <- readLines(path, n = 1L, warn = FALSE)
    expect_match(first, "\\{\\\\rtf", perl = TRUE)
  })
})

test_that(".output_slug: kind letter + number + title, filesystem-safe", {
  obj <- arpillar::object(
    id = "o1",
    type = "km",
    dataset = "ADTTE",
    title = "Kaplan-Meier, OS",
    options = list(number = "14.2.1", number_label = "Figure")
  )
  expect_identical(.output_slug(obj), "f-14-2-1-kaplan-meier-os")
})

test_that("mod_card_server: the .rtf download renders a FIGURE through the figure seam", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_generator(store, "line", "ADVS"))
  shiny::isolate(update_object(store, id, function(o) {
    S7::set_props(
      o,
      title = "Mean Systolic BP by Visit",
      options = list(number = "14.2.1", number_label = "Figure"),
      roles = list(
        arpillar::role(
          slot = "x",
          items = list(arpillar::data_item(name = "AVISIT"))
        ),
        arpillar::role(
          slot = "y",
          items = list(arpillar::data_item(
            name = "AVAL",
            role_type = "measure"
          ))
        ),
        arpillar::role(
          slot = "group",
          items = list(arpillar::data_item(name = "TRT01P"))
        )
      )
    )
  }))
  shiny::isolate(store$rv$selected <- id)

  shiny::testServer(mod_card_server, args = list(store = store), {
    path <- output$rtf
    expect_match(basename(path), "^f-14-2-1")
    expect_gt(file.size(path), 0)
  })
})

test_that("mod_card_server: telemetry reports 'no output selected' when nothing is selected", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(mod_card_server, args = list(store = store), {
    html <- as.character(output$telemetry$html)
    expect_match(html, "no output selected", fixed = TRUE)
  })
})

test_that("mod_card_server: telemetry reports dataset and record counts", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_server, args = list(store = fx$store), {
    html <- as.character(output$telemetry$html)
    expect_match(html, "adsl", fixed = TRUE)
    expect_match(html, "records", fixed = TRUE)
  })
})
