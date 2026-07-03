# The Filters pane (mod_card_filters): the Filters-tab content of the
# docked inspector. Presets FIRST (Safety population / Full set), then
# builder rows (column - op - value - include-missing - remove) held in the
# store-side draft (`rv$filter_draft`); a row commits ONLY when complete
# (the engine is drop-tolerant, so an incomplete row would silently
# vanish -- the pane shows an honest `incomplete` badge instead). The live
# count beside the pane label goes through `arpillar::filter_count()` on
# the COMPLETE predicates, debounced 300ms.

# A store with the demographics preset on ADSL (has SAFFL), selected.
.mcf_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
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

test_that("the Safety preset shows for a SAFFL dataset and writes in one click", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$flushReact()
    expect_match(output$pane$html, "Safety population", fixed = TRUE)

    session$setInputs(preset_flag = list(column = "SAFFL", nonce = 1))
    filters <- shiny::isolate(selected_object(store))@filters
    expect_identical(
      filters,
      list(list(column = "SAFFL", op = "==", value = "Y"))
    )
    # The draft mirrors the committed state (store-side, never DOM).
    expect_length(shiny::isolate(store$rv$filter_draft), 1L)
  })
})

test_that("the Safety preset is hidden when the dataset has no SAFFL", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  out_id <- shiny::isolate(add_from_generator(store, "line", "ADVS"))
  shiny::isolate(store$rv$selected <- out_id)

  shiny::testServer(mod_card_filters_server, args = list(store = store), {
    session$flushReact()
    expect_no_match(output$pane$html, "Safety population", fixed = TRUE)
    expect_match(output$pane$html, "Full set", fixed = TRUE)
  })
})

test_that("Full set clears every filter", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(preset_flag = list(column = "SAFFL", nonce = 1))
    expect_length(shiny::isolate(selected_object(store))@filters, 1L)

    session$setInputs(preset_full = 1)
    expect_identical(shiny::isolate(selected_object(store))@filters, list())
    expect_identical(shiny::isolate(store$rv$filter_draft), list())
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
    session$setInputs(f_col_1 = "SEX\x1fVARCHAR")
    session$setInputs(f_val_1 = "F")

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
    session$setInputs(f_col_1 = "SEX\x1fVARCHAR")
    session$setInputs(f_val_1 = "F")
    session$setInputs(f_miss_1 = TRUE)

    filters <- shiny::isolate(selected_object(store))@filters
    expect_true(isTRUE(filters[[1]]$include_missing))

    # Flipping it back off drops the key (minimal predicate shape).
    session$setInputs(f_miss_1 = FALSE)
    filters <- shiny::isolate(selected_object(store))@filters
    expect_null(filters[[1]]$include_missing)
  })
})

test_that("is.na hides the value control and commits value-less", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(f_add = 1)
    session$setInputs(f_col_1 = "SEX\x1fVARCHAR")
    session$setInputs(f_op_1 = "is.na")
    session$flushReact()

    # No value control renders for a null-test op.
    expect_no_match(output$pane$html, "f_val_1", fixed = TRUE)

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
    session$setInputs(f_col_1 = "AGE\x1fDOUBLE")
    session$setInputs(f_op_1 = ">=")
    session$setInputs(f_val_1 = "65")

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
    session$setInputs(f_col_1 = "SEX\x1fVARCHAR")
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
    session$setInputs(f_col_1 = "SEX\x1fVARCHAR")
    session$setInputs(f_val_1 = "F")
    expect_length(shiny::isolate(selected_object(store))@filters, 1L)

    session$setInputs(f_rm_1 = 1)
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
    session$setInputs(preset_flag = list(column = "SAFFL", nonce = 1))
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

# ---- stage-11 depth: flag chips, humanized ops, paper tag ------------------

test_that("every *FL category flag becomes a population preset chip", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$flushReact()
    html <- output$pane$html
    # Demo ADSL carries SAFFL (mapped) and DISCFL (unmapped fallback).
    expect_match(html, "Safety population", fixed = TRUE)
    expect_match(html, "DISCFL = Y", fixed = TRUE)
    expect_match(html, "Full set", fixed = TRUE)
  })
})

test_that("an unmapped flag chip writes its canonical predicate", {
  fx <- .mcf_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_filters_server, args = list(store = fx$store), {
    session$setInputs(preset_flag = list(column = "DISCFL", nonce = 1))
    expect_identical(
      shiny::isolate(selected_object(store))@filters,
      list(list(column = "DISCFL", op = "==", value = "Y"))
    )
  })
})

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
