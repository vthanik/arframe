# The Filters pane (mod_card_filters): the Filters-tab content of the
# docked inspector. Presets FIRST (Safety population / Full set), then
# builder rows (column - op - value - include-missing - remove) held in the
# store-side draft (`rv$filter_draft`); a row commits ONLY when complete
# (the engine is drop-tolerant, so an incomplete row would silently
# vanish -- the pane shows an honest `incomplete` badge instead). The live
# count beside the pane label goes through `arpillar::filter_count()` on
# the COMPLETE predicates, debounced 300ms.

# A store with the demographics preset on ADSL (has SAFFL), selected. Seeds
# theme$populations (the analysis-set library Setup would seed on first render)
# so the POPULATION section renders chips -- the tests don't mount Setup.
.mcf_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  shiny::isolate({
    r <- store$rv$report
    theme <- r@theme
    theme$populations <- list(
      safety = list(
        label = "Safety Analysis Set",
        dataset = "ADSL",
        filter = 'SAFFL == "Y"'
      ),
      itt = list(
        label = "ITT Analysis Set",
        dataset = "ADSL",
        filter = 'ITTFL == "Y"'
      )
    )
    theme$default_population <- "safety"
    store$rv$report <- S7::set_props(r, theme = theme)
  })
  out_id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(store$rv$selected <- out_id)
  list(con = con, store = store, id = out_id)
}

# ---- .filter_complete -------------------------------------------------------

test_that(".filter_complete mirrors the engine's compilability rules", {
  # Complete: column + op + at least one real value.
  expect_true(.filter_complete(list(column = "SEX", op = "%in%", value = "F")))
  # is.na / not.na need no value at all.
  expect_true(.filter_complete(list(column = "SEX", op = "is.na")))
  expect_true(.filter_complete(list(column = "SEX", op = "not.na")))
  # An NA value alone counts via the engine's include-missing fold.
  expect_true(.filter_complete(
    list(column = "SEX", op = "%in%", value = NA_character_)
  ))
  expect_true(.filter_complete(
    list(column = "SEX", op = "%in%", value = NULL, include_missing = TRUE)
  ))

  # Incomplete: missing column, unknown op, or no value.
  expect_false(.filter_complete(list(column = "", op = "%in%", value = "F")))
  expect_false(.filter_complete(list(column = "SEX", op = "like", value = "F")))
  expect_false(.filter_complete(list(column = "SEX", op = "%in%")))
  expect_false(.filter_complete(list(column = "AGE", op = ">")))
})

# ---- presets ---------------------------------------------------------------

test_that("POPULATION chips come from Setup's analysis sets, not dataset flags", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$flushReact()
    html <- output$pane$html
    # theme$populations labels (Setup > Analysis sets), NOT the raw *FL flags.
    expect_match(html, "Safety Analysis Set", fixed = TRUE)
    expect_match(html, "ITT Analysis Set", fixed = TRUE)
    expect_no_match(html, "DISCFL = Y", fixed = TRUE)
    expect_no_match(html, "Full set", fixed = TRUE)
  })
})

test_that("picking a population writes options$population (the TOC's field), not a filter", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(pick_population = list(id = "itt", nonce = 1))
    obj <- shiny::isolate(selected_object(store))
    # The SAME field the LoC POPULATION column reads -> the two sync.
    expect_identical(obj@options$population, "itt")
    # Population is NOT written as an ad-hoc filter predicate.
    expect_identical(obj@filters, list())
  })
})

test_that("the current population chip wears the selected state (default when unset)", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$flushReact()
    # Unset options$population -> the study default (safety) is the lit chip.
    html <- output$pane$html
    at <- regexpr("<button[^>]*ar-flt-preset-on", html)
    on <- substr(html, at, at + 1200L)
    expect_match(on, "Safety Analysis Set", fixed = TRUE)

    # Pick ITT -> it becomes the lit chip.
    session$setInputs(pick_population = list(id = "itt", nonce = 1))
    session$flushReact()
    html2 <- output$pane$html
    at2 <- regexpr("<button[^>]*ar-flt-preset-on", html2)
    on2 <- substr(html2, at2, at2 + 1200L)
    expect_match(on2, "ITT Analysis Set", fixed = TRUE)
  })
})

# ---- builder rows ------------------------------------------------------------

test_that("a category row commits SEX %in% F and the live count matches filter_count", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(f_add = 1)
    # The column picker posts the packed NAME\x1fSQL_TYPE choice (the
    # same rich-picker contract as the Roles pane; the type half is the
    # RAW SQL type -- only the name half drives the commit).
    session$setInputs(f_col = "SEX\x1fVARCHAR")
    session$setInputs(f_val = "F")

    filters <- shiny::isolate(selected_object(store))@filters
    expect_identical(
      filters,
      list(list(column = "SEX", op = "%in%", value = "F"))
    )

    # The live count agrees with the engine's own count.
    engine <- arpillar::filter_count(store$con, "ADSL", filters)
    session$elapse(301)
    html <- output$count$html
    expect_match(
      html,
      sprintf("%d of %d", engine$matched, engine$total),
      fixed = TRUE
    )
  })
})

test_that("include-missing folds into the committed predicate", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(f_add = 1)
    session$setInputs(f_col = "SEX\x1fVARCHAR")
    session$setInputs(f_val = "F")
    session$setInputs(f_miss = TRUE)

    filters <- shiny::isolate(selected_object(store))@filters
    expect_true(isTRUE(filters[[1]]$include_missing))

    # Flipping it back off drops the key (minimal predicate shape).
    session$setInputs(f_miss = FALSE)
    filters <- shiny::isolate(selected_object(store))@filters
    expect_null(filters[[1]]$include_missing)
  })
})

test_that("is.na hides the value control and commits value-less", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(f_add = 1)
    session$setInputs(f_col = "SEX\x1fVARCHAR")
    session$setInputs(f_op = "is.na")
    session$flushReact()

    # No value control renders for a null-test op.
    expect_no_match(output$pane$html, "f_val", fixed = TRUE)

    filters <- shiny::isolate(selected_object(store))@filters
    expect_identical(
      filters,
      list(list(column = "SEX", op = "is.na"))
    )
  })
})

test_that("a measure row parses the typed comparison value as numeric", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(f_add = 1)
    session$setInputs(f_col = "AGE\x1fDOUBLE")
    session$setInputs(f_op = ">=")
    session$setInputs(f_val = "65")

    filters <- shiny::isolate(selected_object(store))@filters
    expect_identical(
      filters,
      list(list(column = "AGE", op = ">=", value = 65))
    )
  })
})

test_that("an incomplete row is NOT committed and shows the honest badge", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(f_add = 1)
    session$setInputs(f_col = "SEX\x1fVARCHAR")
    session$flushReact()

    # Column + default op, but no value yet: the engine would silently
    # drop it, so the pane must not commit it -- and must say so.
    expect_identical(shiny::isolate(selected_object(store))@filters, list())
    expect_match(output$pane$html, "incomplete", fixed = TRUE)
  })
})

test_that("removing a row uncommits it", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(f_add = 1)
    session$setInputs(f_col = "SEX\x1fVARCHAR")
    session$setInputs(f_val = "F")
    expect_length(shiny::isolate(selected_object(store))@filters, 1L)

    session$setInputs(chip_rm = list(i = 1, nonce = 1))
    expect_identical(shiny::isolate(selected_object(store))@filters, list())
    expect_identical(shiny::isolate(store$rv$filter_draft), list())
  })
})

# ---- draft seeding -----------------------------------------------------------

test_that("the draft seeds from object@filters on selection change", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id_a <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(update_object(store, id_a, function(o) {
    S7::set_props(
      o,
      filters = list(list(column = "SAFFL", op = "==", value = "Y"))
    )
  }))
  id_b <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))

  shiny::testServer(mod_card_filters_server, args = list(store = store), {
    store$rv$selected <- id_a
    session$flushReact()
    draft <- shiny::isolate(store$rv$filter_draft)
    expect_length(draft, 1L)
    expect_identical(draft[[1]]$column, "SAFFL")

    # Switching to the filterless object clears the draft.
    store$rv$selected <- id_b
    session$flushReact()
    expect_identical(shiny::isolate(store$rv$filter_draft), list())
  })
})

test_that("REGRESSION: the pane computes while hidden (tab flip is client-only)", {
  # Same mod_data lesson as the Options pane -- see that test's note.
  src <- paste(deparse(body(mod_card_filters_server)), collapse = "\n")
  expect_match(
    src,
    'outputOptions(output, "pane", suspendWhenHidden = FALSE)',
    fixed = TRUE
  )
  expect_match(
    src,
    'outputOptions(output, "count", suspendWhenHidden = FALSE)',
    fixed = TRUE
  )
})

test_that("REGRESSION: a committed row re-seeds its picker with a REAL choice value", {
  # The picker's choices are packed NAME\x1fTYPE\x1fLABEL; seeding the
  # selection with any OTHER packing matches nothing, so selectize falls
  # back to the first column and its bind-post RESETS the freshly
  # committed row (seen on the real CDISC pilot mount).
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    # Commit a SAFFL filter via the builder (the population presets no longer
    # write filters -- 2026-07-08).
    session$setInputs(f_add = 1)
    session$setInputs(f_col = "SAFFL\x1fVARCHAR")
    session$setInputs(f_val = "Y")
    # f_add left the row open; close it, then re-open the COMMITTED chip so the
    # editor re-seeds its picker from the committed predicate (the path tested).
    shiny::isolate(store$rv$filter_open <- NULL)
    session$setInputs(chip_open = list(i = 1, nonce = 1))
    session$flushReact()
    html <- output$pane$html
    opt <- regmatches(
      html,
      regexpr("<option[^>]*value=\"SAFFL[^\"]*\"[^>]*>", html)
    )
    expect_length(opt, 1L)
    # Packed exactly as the choices are (type + label) and selected.
    expect_match(
      opt,
      "SAFFL\x1fcategory\x1fSafety Population Flag",
      fixed = TRUE
    )
    expect_match(opt, "selected", fixed = TRUE)
  })
})

# ---- stage-11 depth: humanized ops, paper tag ------------------------------
# (The `*FL`-flag population-chip tests were removed 2026-07-08: POPULATION now
# lists Setup's analysis sets, covered by the tests near the top of this file.
# `.filters_tag_label` still names a hand-added flag filter -- tested below.)

test_that("op labels are humanized but post the exact engine op set", {
  # The engine contract is untouched: the labeled vector's VALUES are the
  # engine ops, in the engine's own display order.
  expect_identical(unname(.FILTER_OP_LABELS), .FILTER_OPS)
  expect_identical(unname(.FILTER_OP_LABELS[["is any of"]]), "%in%")
})

test_that("the paper tag names any recognized flag population", {
  expect_identical(
    .filters_tag_label(list(list(column = "SAFFL", op = "==", value = "Y"))),
    "Safety population"
  )
  expect_identical(
    .filters_tag_label(list(list(column = "ITTFL", op = "==", value = "Y"))),
    "ITT population"
  )
  expect_identical(
    .filters_tag_label(list(list(column = "DISCFL", op = "==", value = "Y"))),
    "DISCFL = Y"
  )
  # A non-flag single predicate and multi-predicate sets stay honest counts.
  expect_identical(
    .filters_tag_label(list(list(column = "AGE", op = ">", value = 65))),
    "1 filter"
  )
  expect_identical(
    .filters_tag_label(list(
      list(column = "SAFFL", op = "==", value = "Y"),
      list(column = "AGE", op = ">", value = 65)
    )),
    "2 filters"
  )
})

# ---- chips + editor (2026-07-04 redesign) ----------------------------------

test_that("+ Filter opens the new row's editor; chip clicks toggle/move it", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(f_add = 1)
    expect_identical(shiny::isolate(store$rv$filter_open), 1L)
    # Clicking the open chip closes the editor; clicking again reopens.
    session$setInputs(chip_open = list(i = 1, nonce = 1))
    expect_null(shiny::isolate(store$rv$filter_open))
    session$setInputs(chip_open = list(i = 1, nonce = 2))
    expect_identical(shiny::isolate(store$rv$filter_open), 1L)
    # Done closes it.
    session$setInputs(f_done = 1)
    expect_null(shiny::isolate(store$rv$filter_open))
  })
})

test_that("selection change clears the open editor (stale-index guard)", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id_a <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))
  id_b <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))

  shiny::testServer(mod_card_filters_server, args = list(store = store), {
    store$rv$selected <- id_a
    session$flushReact()
    session$setInputs(f_add = 1)
    expect_identical(shiny::isolate(store$rv$filter_open), 1L)
    store$rv$selected <- id_b
    session$flushReact()
    expect_null(shiny::isolate(store$rv$filter_open))
  })
})

test_that("removing a chip below the open one shifts the open index down", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(f_add = 1)
    session$setInputs(f_col = "SEX\x1fVARCHAR")
    session$setInputs(f_val = "F")
    session$setInputs(f_add = 2)
    expect_identical(shiny::isolate(store$rv$filter_open), 2L)
    session$setInputs(chip_rm = list(i = 1, nonce = 1))
    expect_identical(shiny::isolate(store$rv$filter_open), 1L)
    session$setInputs(chip_rm = list(i = 1, nonce = 2))
    expect_null(shiny::isolate(store$rv$filter_open))
  })
})

test_that("the chip label reads as a compact predicate", {
  expect_identical(
    .filter_chip_label(list(column = "SAFFL", op = "==", value = "Y")),
    "SAFFL = Y"
  )
  expect_identical(
    .filter_chip_label(list(column = "AGE", op = ">", value = 65)),
    "AGE > 65"
  )
  expect_identical(
    .filter_chip_label(list(
      column = "RACE",
      op = "%in%",
      value = c("WHITE", "ASIAN", "OTHER")
    )),
    "RACE in 3 values"
  )
  expect_identical(
    .filter_chip_label(list(column = "AEDECOD", op = "is.na")),
    "AEDECOD is missing"
  )
  expect_identical(.filter_chip_label(list(column = "")), "New filter")
})
