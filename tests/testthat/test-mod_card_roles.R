# The roles editor (mod_card_roles): the Roles-tab content of the docked
# inspector. One fieldset per generator role slot, region-filtered when a
# specific paper region routed the open, else the full slot set.

test_that(".region_slots returns ALL slots when no region is focused (v5 regression)", {
  # REGRESSION: the v5 docked inspector shows the Roles tab with no region
  # click, so `store$rv$region` is NULL. `switch(NULL, ...)` aborts with
  # "EXPR must be a length 1 vector" -- the Roles pane must instead show the
  # generator's full slot set.
  slots <- arpillar::generator("summary")$slots
  expect_identical(.region_slots(NULL, slots), slots)
  # A stray non-scalar region is guarded the same way.
  expect_identical(.region_slots(c("a", "b"), slots), slots)
})

test_that(".region_slots narrows to the region's own slots when routed", {
  slots <- arpillar::generator("summary")$slots
  ids <- function(x) vapply(x, `[[`, "", "slot")

  # columns -> the treatment/arm slot only.
  expect_setequal(ids(.region_slots("columns", slots)), "treatment")
  # rows -> the table-content slot (summarize for a summary generator).
  expect_true("summarize" %in% ids(.region_slots("rows", slots)))
})

test_that(".region_slots falls back to ALL slots for a non-roles region (empty-pane regression)", {
  # REGRESSION: a stale non-roles region ("title" after a rename jump,
  # "footnotes", anything future) used to narrow the slot list to ZERO and
  # the whole Roles pane rendered NULL -- the reported "Roles is empty" bug.
  # Any region this filter does not own now shows the FULL editor.
  slots <- arpillar::generator("summary")$slots
  expect_identical(.region_slots("title", slots), slots)
  expect_identical(.region_slots("footnotes", slots), slots)
  expect_identical(.region_slots("something-future", slots), slots)
})

test_that("the slots pane renders the full editor under a stale 'title' region", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(store$rv$selected <- id)
  # The reported repro: a title-region focus is still set when the user
  # clicks the Roles tab directly.
  shiny::isolate(store$rv$region <- "title")

  shiny::testServer(mod_card_roles_server, args = list(store = store), {
    session$flushReact()
    html <- output$slots$html
    expect_match(html, "ar-role-slot", fixed = TRUE)
    expect_match(html, "Treatment arms", fixed = TRUE)
    expect_match(html, "TRT01P", fixed = TRUE)
  })
})

test_that("a stale figure region ('axes') on a table shows the full editor, never nothing", {
  slots <- arpillar::generator("summary")$slots
  # "axes" IS a roles region, but a summary generator has no axes slots --
  # the renderUI-level fallback covers this (tested through the server).
  expect_length(.region_slots("axes", slots), 0L)

  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(store$rv$selected <- id)
  shiny::isolate(store$rv$region <- "axes")

  shiny::testServer(mod_card_roles_server, args = list(store = store), {
    session$flushReact()
    expect_match(output$slots$html, "ar-role-slot", fixed = TRUE)
  })
})

test_that("mod_card_roles_server keeps its pane computing while hidden", {
  # The panes are always mounted and CSS-toggled; a suspended-when-hidden
  # output would freeze (or blank) the editor after a pure class-flip tab
  # switch. Pin the literal contract line, as the sibling panes do.
  src <- readLines(test_path("..", "..", "R", "mod_card_roles.R"))
  expect_true(any(grepl(
    'outputOptions(output, "slots", suspendWhenHidden = FALSE)',
    src,
    fixed = TRUE
  )))
})

test_that("mod_card_roles_server: the slots pane renders without a region focus", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(store$rv$selected <- id)
  # region stays NULL -- exactly the state after Add-output selects the new
  # object and the inspector shows the Roles tab.

  shiny::testServer(mod_card_roles_server, args = list(store = store), {
    session$flushReact()
    html <- output$slots$html
    # Renders (no switch-on-NULL crash) with the full role editor: the
    # treatment-arms slot and the summarize slot both show.
    expect_match(html, "ar-role-slot", fixed = TRUE)
    expect_match(html, "Treatment arms", fixed = TRUE)
    expect_match(html, "TRT01P", fixed = TRUE)
  })
})
