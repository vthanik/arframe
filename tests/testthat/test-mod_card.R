# The docked inspector: fixed-width right panel with the explorer-style
# Roles/Options/Filters/Ranks tab rail and the telemetry line (the action
# footer moved to the canvas toolbar -- see test-mod_toolbar.R). Tab state
# lives in the store (`rv$insp_tab`);
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

test_that("mod_card_ui: labeled tab rail, all four panes, no footer or chevrons", {
  ui <- mod_card_ui("card")
  html <- as.character(ui)

  for (tab in c("roles", "options", "filters", "ranks")) {
    expect_match(html, sprintf('data-ar-insp-tab="%s"', tab), fixed = TRUE)
    expect_match(html, sprintf("ar-insp-pane-%s", tab), fixed = TRUE)
  }
  # Explorer-style rail (2026-07-04): visible labels on the tab buttons;
  # the rail itself is the collapsed strip, so chevrons and the slim div
  # are gone, and the action footer moved to the canvas toolbar.
  expect_match(html, '<span class="ar-insp-tab-lbl">Roles</span>', fixed = TRUE)
  expect_no_match(html, "ar-insp-slim", fixed = TRUE)
  expect_no_match(html, "ar-insp-cv", fixed = TRUE)
  expect_no_match(html, "ar-insp-act", fixed = TRUE)
  expect_no_match(html, 'id="card-run"', fixed = TRUE)
  expect_no_match(html, 'id="card-code"', fixed = TRUE)
  # No float-era chrome (v5): pin and close are gone.
  expect_no_match(html, "ar-card-pin", fixed = TRUE)
  expect_no_match(html, 'id="card-close"', fixed = TRUE)
})

test_that("mod_card_ui: the Ranks pane mounts the real ranks module", {
  html <- as.character(mod_card_ui("card"))
  # The Task-11 stub is gone: the pane hosts mod_card_ranks' own output.
  expect_match(html, 'id="card-ranks-pane"', fixed = TRUE)
  expect_no_match(html, "ar-tag-coming", fixed = TRUE)
  expect_no_match(html, "arrive with the AE hierarchy table", fixed = TRUE)
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

test_that("mod_card_server: a tab click clears the region focus (empty-pane regression)", {
  # A direct tab click is navigation, not region routing: a stale "title"
  # region left behind by a jump link must not survive into the Roles pane
  # (where it used to narrow the slot list to nothing).
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_server, args = list(store = fx$store), {
    store$rv$region <- "title"
    session$setInputs(tab_roles = 1)
    expect_null(store$rv$region)
    expect_identical(store$rv$insp_tab, "roles")
  })
})

test_that("mod_card_server: clicking a tab toggles the pane collapsed/open", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_server, args = list(store = fx$store), {
    expect_false(isTRUE(store$rv$insp_collapsed))
    # Switch to Options (open), then click Options again -> collapse.
    session$setInputs(tab_options = 1)
    expect_identical(store$rv$insp_tab, "options")
    expect_false(isTRUE(store$rv$insp_collapsed))
    session$setInputs(tab_options = 2)
    expect_true(store$rv$insp_collapsed)
    # Clicking any tab while collapsed re-opens it on that tab.
    session$setInputs(tab_filters = 1)
    expect_false(store$rv$insp_collapsed)
    expect_identical(store$rv$insp_tab, "filters")
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
