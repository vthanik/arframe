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
  # switch. outputOptions() is not introspectable under testServer's mock
  # session, so pin the call in the server body (works installed too).
  src <- paste(deparse(body(mod_card_roles_server)), collapse = "\n")
  expect_match(
    src,
    'outputOptions(output, "slots", suspendWhenHidden = FALSE)',
    fixed = TRUE
  )
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

# ---- accordion sections (Task 11) ------------------------------------------

test_that("each role slot renders as a default-open accordion, legend visually hidden", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(store$rv$selected <- id)

  shiny::testServer(mod_card_roles_server, args = list(store = store), {
    session$flushReact()
    html <- output$slots$html
    expect_match(html, '<details class="ar-acc" open', fixed = TRUE)
    # The fieldset survives INSIDE the accordion body, its legend kept for
    # assistive tech but visually hidden (the visible label moved to the
    # accordion summary).
    expect_match(html, "<fieldset", fixed = TRUE)
    expect_match(html, '<legend class="visually-hidden">', fixed = TRUE)
  })
})

# ---- stage-9 depth: digest, source row, labels, peek, relabel, retype ------

.mcr_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(store$rv$selected <- id)
  list(con = con, store = store, id = id)
}

test_that("the roles digest changes on a label or role_type edit", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))
  obj <- shiny::isolate(selected_object(fx$store))

  d0 <- .roles_digest(obj)
  expect_false(identical(
    .roles_digest(.relabel_item(obj, "summarize", "AGE", "Age (years)")),
    d0
  ))
  expect_false(identical(
    .roles_digest(.retype_item(obj, "summarize", "AGE", "category")),
    d0
  ))
})

test_that("the SOURCE row names the dataset, its structure, and its dims", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_roles_server, args = list(store = fx$store), {
    session$flushReact()
    html <- output$slots$html
    expect_match(html, "ar-role-src", fixed = TRUE)
    expect_match(html, "ADSL", fixed = TRUE)
    # The demo ADSL is subject-level, 12 x 9.
    expect_match(html, "subject", fixed = TRUE)
    expect_match(html, "12", fixed = TRUE)
  })
})

test_that("assigned rows show the CDISC label under the variable name", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_roles_server, args = list(store = fx$store), {
    session$flushReact()
    html <- output$slots$html
    # The demo sidecar labels flow through the preset seed into the rows.
    expect_match(html, "ar-role-sub", fixed = TRUE)
    expect_match(html, "Planned Treatment for Period 01", fixed = TRUE)
  })
})

test_that("the peek expands a category with value-count bars and caches", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_roles_server, args = list(store = fx$store), {
    session$setInputs(peek = list(name = "SEX", nonce = 1))
    session$flushReact()
    expect_identical(store$rv$peek, "SEX")
    html <- output$slots$html
    expect_match(html, "ar-role-peek", fixed = TRUE)
    expect_match(html, "ar-peek-bar-row", fixed = TRUE)
    # The engine facts are memoized per catalog generation.
    expect_true(exists("peek::ADSL::SEX::0", envir = store$cache))

    # Toggling again folds the panel and clears the open state.
    session$setInputs(peek = list(name = "SEX", nonce = 2))
    session$flushReact()
    expect_identical(store$rv$peek, character(0))
    expect_no_match(output$slots$html, "ar-role-peek\"", fixed = TRUE)
  })
})

test_that("the peek shows min/median/max + precision for a measure", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_roles_server, args = list(store = fx$store), {
    session$setInputs(peek = list(name = "AGE", nonce = 1))
    session$flushReact()
    html <- output$slots$html
    expect_match(html, "min 55", fixed = TRUE)
    expect_match(html, "max 74", fixed = TRUE)
    expect_match(html, "observed precision", fixed = TRUE)
    # Numeric column in a both-types slot: the treat-as toggle renders.
    expect_match(html, "ar-peek-type", fixed = TRUE)
  })
})

test_that("a failed peek is never memoized -- the next render retries", {
  # A transient catalog error (view dropped mid-register) used to be cached
  # as NULL for the whole generation, pinning "Distribution unavailable."
  # until the nonce moved. Failures must stay uncached so the next render
  # retries, and the swallowed condition must reach the log.
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))

  calls <- 0L
  real <- arpillar::value_counts
  testthat::local_mocked_bindings(
    value_counts = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) {
        stop("catalog view momentarily dropped")
      }
      real(...)
    },
    .package = "arpillar"
  )

  facts1 <- shiny::isolate(.peek_facts(fx$store, "ADSL", "SEX", NULL))
  expect_identical(facts1$kind, "error")
  expect_match(facts1$message, "momentarily dropped")
  expect_false(exists("peek::ADSL::SEX::0", envir = fx$store$cache))
  expect_match(
    paste(shiny::isolate(fx$store$rv$log), collapse = "\n"),
    "peek failed: ADSL$SEX",
    fixed = TRUE
  )

  # Second render: the engine is back, the peek recovers and memoizes.
  facts2 <- shiny::isolate(.peek_facts(fx$store, "ADSL", "SEX", NULL))
  expect_identical(facts2$kind, "counts")
  expect_true(exists("peek::ADSL::SEX::0", envir = fx$store$cache))
})

test_that("a peek failure shows its reason in the panel", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))

  testthat::local_mocked_bindings(
    column_range = function(...) stop("no such view: ADSL"),
    .package = "arpillar"
  )

  shiny::testServer(mod_card_roles_server, args = list(store = fx$store), {
    session$setInputs(peek = list(name = "AGE", nonce = 1))
    session$flushReact()
    html <- output$slots$html
    expect_match(html, "Distribution unavailable.", fixed = TRUE)
    expect_match(html, "no such view", fixed = TRUE)
    expect_match(
      paste(store$rv$log, collapse = "\n"),
      "peek failed: ADSL$AGE",
      fixed = TRUE
    )
  })
})

test_that("the treat-as toggle never renders for a text column", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_roles_server, args = list(store = fx$store), {
    session$setInputs(peek = list(name = "SEX", nonce = 1))
    session$flushReact()
    # SEX is VARCHAR: it can only ever be a category.
    expect_no_match(output$slots$html, "ar-peek-type\"", fixed = TRUE)
  })
})

test_that("relabel commits a cheap edit: the label lands, nothing goes stale", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_roles_server, args = list(store = fx$store), {
    session$setInputs(
      relabel = list(
        slot = "summarize",
        name = "AGE",
        value = "Age at Baseline",
        nonce = 1
      )
    )
    obj <- shiny::isolate(selected_object(store))
    it <- obj@roles[[2]]@items[[1]]
    expect_identical(it@label, "Age at Baseline")
    expect_identical(shiny::isolate(store$rv$stale), character(0))
  })
})

test_that("retype flips the role_type and marks the proof stale", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_roles_server, args = list(store = fx$store), {
    session$setInputs(
      retype = list(
        slot = "summarize",
        name = "AGE",
        role_type = "category",
        nonce = 1
      )
    )
    obj <- shiny::isolate(selected_object(store))
    it <- obj@roles[[2]]@items[[1]]
    expect_identical(it@role_type, "category")
    expect_identical(shiny::isolate(store$rv$stale), fx$id)

    # An unknown role_type is refused, never committed.
    session$setInputs(
      retype = list(
        slot = "summarize",
        name = "AGE",
        role_type = "banana",
        nonce = 2
      )
    )
    obj2 <- shiny::isolate(selected_object(store))
    expect_identical(obj2@roles[[2]]@items[[1]]@role_type, "category")
  })
})

test_that(".orphan_problems reports nothing when every problem is slot-owned", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  # A bare summary has only roles-* requirements: the strip stays empty,
  # the inline fieldset messages carry them -- nothing double-reports.
  obj <- arpillar::object(id = "o1", type = "summary", dataset = "ADSL")
  slots <- arpillar::generator("summary")$slots
  expect_identical(.orphan_problems(obj, slots), character(0))
})

test_that("the roles pane's empty state directs action when nothing is selected", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(mod_card_roles_server, args = list(store = store), {
    session$flushReact()
    expect_match(output$slots$html, "No output selected", fixed = TRUE)
  })
})

# ---- levels editor (order / include / display-as / expected) ---------------

test_that("the category peek renders the levels editor with observed values", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_roles_server, args = list(store = fx$store), {
    session$setInputs(peek = list(name = "SEX", nonce = 1))
    session$flushReact()
    html <- output$slots$html
    expect_match(html, "ar-levels-editor", fixed = TRUE)
    expect_match(html, "ar-level-row", fixed = TRUE)
    expect_match(html, "Add expected level", fixed = TRUE)
    # A measure peek never shows it.
    session$setInputs(peek = list(name = "SEX", nonce = 2)) # fold
    session$setInputs(peek = list(name = "AGE", nonce = 3))
    session$flushReact()
    expect_no_match(output$slots$html, "ar-levels-editor", fixed = TRUE)
  })
})

test_that("level edits commit @levels and stay CHEAP (never stale)", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_roles_server, args = list(store = fx$store), {
    session$setInputs(peek = list(name = "SEX", nonce = 1))
    item <- function() {
      obj <- shiny::isolate(selected_object(store))
      r <- .role_for_slot(obj, "summarize")
      r@items[[which(vapply(r@items, function(i) i@name, "") == "SEX")]]
    }

    # DISPLAY AS recode.
    session$setInputs(
      lvl_display = list(
        slot = "summarize",
        name = "SEX",
        value = "M",
        display = "Male",
        nonce = 2
      )
    )
    lv <- item()@levels
    expect_identical(lv[[1]]$value, "M")
    expect_identical(lv[[1]]$display, "Male")

    # Include toggle off.
    session$setInputs(
      lvl_include = list(
        slot = "summarize",
        name = "SEX",
        value = "F",
        on = FALSE,
        nonce = 3
      )
    )
    lv <- item()@levels
    expect_true(isFALSE(lv[[2]]$include))

    # Add an expected (dummy) level.
    session$setInputs(
      lvl_add = list(
        slot = "summarize",
        name = "SEX",
        value = "U",
        nonce = 4
      )
    )
    lv <- item()@levels
    expect_true(isTRUE(lv[[3]]$expected))

    # Reorder: U first, M second; F keeps its entry (reconcile).
    session$setInputs(
      lvl_reorder = list(
        order = list("U", "M", "F"),
        slot = "summarize",
        name = "SEX",
        nonce = 5
      )
    )
    lv <- item()@levels
    expect_identical(
      vapply(lv, function(m) m$value, ""),
      c("U", "M", "F")
    )
    expect_identical(lv[[2]]$display, "Male") # fields survived the reorder

    # Remove the expected level.
    session$setInputs(
      lvl_rm = list(
        slot = "summarize",
        name = "SEX",
        value = "U",
        nonce = 6
      )
    )
    expect_false("U" %in% vapply(item()@levels, function(m) m$value, ""))

    # Every one of those edits was display-only: nothing went stale.
    expect_identical(shiny::isolate(store$rv$stale), character(0))
  })
})

test_that("level edits never move the ARD cache key (display-only contract)", {
  fx <- .mcr_store()
  withr::defer(arpillar::engine_close(fx$con))
  obj <- shiny::isolate(selected_object(fx$store))
  k0 <- .ard_key(obj)
  edited <- .update_item(obj, "summarize", "SEX", function(it) {
    S7::set_props(
      it,
      levels = list(list(value = "M", display = "Male", include = TRUE))
    )
  })
  expect_identical(.ard_key(edited), k0)
  # But the pane digest DOES move -- the editor repaints.
  expect_false(identical(.roles_digest(edited), .roles_digest(obj)))
})
