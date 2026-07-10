# The docked inspector: fixed-width right panel with a top segmented pill
# strip (Roles/Options/Filters, 2026-07-10 -- the icon rail is gone) and
# the telemetry line (the action footer moved to the canvas toolbar -- see
# test-mod_toolbar.R). Tab state lives in the store (`rv$insp_tab`);
# region clicks route to a tab through `open_card()` (tested in
# test-fct_store.R); collapse is frame-owned (test-mod_frame.R /
# test-mod_toolbar.R's `panel_toggle`).

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

test_that("mod_card_ui: top pill strip (3 tabs), all three panes, no rail or footer", {
  ui <- mod_card_ui("card")
  html <- as.character(ui)

  for (tab in c("roles", "options", "filters")) {
    expect_match(html, sprintf('data-ar-insp-tab="%s"', tab), fixed = TRUE)
    expect_match(html, sprintf("ar-insp-pane-%s", tab), fixed = TRUE)
  }
  # Ranks left the frame entirely (2026-07-10) -- no tab, no pane, no mount.
  expect_no_match(html, 'data-ar-insp-tab="ranks"', fixed = TRUE)
  expect_no_match(html, "ar-insp-pane-ranks", fixed = TRUE)
  expect_no_match(html, 'id="card-ranks-pane"', fixed = TRUE)

  # The icon rail is gone -- replaced by a horizontal pill strip.
  expect_no_match(html, "ar-insp-tabs", fixed = TRUE)
  expect_no_match(html, "ar-insp-tab-lbl", fixed = TRUE)
  expect_no_match(html, "ar-insp-slim", fixed = TRUE)
  expect_no_match(html, "ar-insp-cv", fixed = TRUE)
  expect_no_match(html, "ar-insp-act", fixed = TRUE)
  expect_no_match(html, 'id="card-run"', fixed = TRUE)
  expect_no_match(html, 'id="card-code"', fixed = TRUE)
  # No float-era chrome (v5): pin and close are gone.
  expect_no_match(html, "ar-card-pin", fixed = TRUE)
  expect_no_match(html, 'id="card-close"', fixed = TRUE)
})

test_that("mod_card_ui: the pill strip is the FIRST child of .ar-insp-main, exactly 3 buttons", {
  html <- as.character(mod_card_ui("card"))
  main_pos <- regexpr('<div class="ar-insp-main">', html, fixed = TRUE)
  expect_true(main_pos > 0)
  after_main <- substring(html, main_pos + attr(main_pos, "match.length"))
  # The first element opened after `.ar-insp-main` must be the strip -- not
  # the pane body (i.e. no rail, no other wrapper in between).
  expect_match(after_main, '^\\s*<div class="ar-insp-strip">', perl = TRUE)

  strip_end <- regexpr("</div>", after_main, fixed = TRUE)
  strip_html <- substring(after_main, 1, strip_end - 1L)
  expect_length(gregexpr("<button", strip_html, fixed = TRUE)[[1]], 3L)
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

test_that("mod_card_server: re-clicking the active tab just switches, it no longer collapses (2026-07-10)", {
  # The strip moved inside the folding pane -- clicking the ACTIVE tab
  # twice is a no-op on collapse state now; only the toolbar's
  # `panel_toggle` (test-mod_toolbar.R) folds the card.
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_server, args = list(store = fx$store), {
    expect_false(isTRUE(store$rv$insp_collapsed))
    session$setInputs(tab_options = 1)
    expect_identical(store$rv$insp_tab, "options")
    expect_false(isTRUE(store$rv$insp_collapsed))
    session$setInputs(tab_options = 2)
    expect_identical(store$rv$insp_tab, "options")
    expect_false(isTRUE(store$rv$insp_collapsed))
  })
})

test_that("mod_card_server: clicking any tab while collapsed re-opens it on that tab", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_server, args = list(store = fx$store), {
    store$rv$insp_collapsed <- TRUE
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
