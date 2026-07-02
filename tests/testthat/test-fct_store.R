# The injected structured store: one S7 report + galley pointers +
# undo/redo + a two-stage ARD cache. Every module server is handed this
# store and communicates ONLY through it (design spec 5.1). All store tests
# run under shiny::isolate() -- no session, no UI, proving draft state lives
# entirely off the DOM.

shiny::reactiveConsole(TRUE)
withr::defer(shiny::reactiveConsole(FALSE), teardown_env())

# ---- shape ------------------------------------------------------------

test_that("new_store returns con/rv/undo/cache with the full rv field set", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  expect_identical(store$con, con)
  expect_true(shiny::is.reactivevalues(store$rv))
  expect_true(is.environment(store$undo))
  expect_true(is.environment(store$cache))

  fields <- c(
    "report",
    "selected",
    "region",
    "card",
    "pinned",
    "mode",
    "dataset",
    "bridge_dataset",
    "adding",
    "filter_draft",
    "path",
    "dirty",
    "saved_at",
    "broken",
    "log",
    "catalog_nonce",
    "rail_collapsed",
    "insp_collapsed"
  )
  rv <- shiny::isolate(shiny::reactiveValuesToList(store$rv))
  expect_true(all(fields %in% names(rv)))
})

test_that("collapse defaults are FALSE; toggle_rail()/toggle_insp() flip them (v5)", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  rv <- shiny::isolate(shiny::reactiveValuesToList(store$rv))
  expect_false(rv$rail_collapsed)
  expect_false(rv$insp_collapsed)

  shiny::isolate(toggle_rail(store))
  expect_true(shiny::isolate(store$rv$rail_collapsed))
  shiny::isolate(toggle_rail(store))
  expect_false(shiny::isolate(store$rv$rail_collapsed))

  shiny::isolate(toggle_insp(store))
  expect_true(shiny::isolate(store$rv$insp_collapsed))
  # The two panels collapse independently -- rail is untouched.
  expect_false(shiny::isolate(store$rv$rail_collapsed))
})

test_that("new_store default report is Untitled report / report1 / page p1", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  rep <- shiny::isolate(store$rv$report)
  expect_identical(rep@id, "report1")
  expect_identical(rep@name, "Untitled report")
  expect_length(rep@pages, 1L)
  expect_identical(rep@pages[[1]]@id, "p1")
})

test_that("new_store default rv values match the pointer contract", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  rv <- shiny::isolate(shiny::reactiveValuesToList(store$rv))
  expect_null(rv$selected)
  expect_null(rv$region)
  expect_false(rv$card)
  expect_false(rv$pinned)
  expect_identical(rv$mode, "report")
  expect_null(rv$dataset)
  expect_null(rv$bridge_dataset)
  expect_false(rv$adding)
  expect_identical(rv$filter_draft, list())
  expect_null(rv$path)
  expect_false(rv$dirty)
  expect_null(rv$saved_at)
  expect_identical(rv$broken, character(0))
  expect_identical(rv$log, character(0))
  expect_identical(rv$catalog_nonce, 0L)
})

test_that("new_store accepts a pre-built report instead of the default", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  custom <- arpillar::report(
    id = "r9",
    name = "Custom",
    pages = list(arpillar::page(id = "px"))
  )
  store <- shiny::isolate(new_store(con, report = custom))
  expect_identical(shiny::isolate(store$rv$report)@id, "r9")
})

# ---- commit / undo / redo ----------------------------------------------

test_that("commit swaps in a new report and marks dirty", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  new_rep <- S7::set_props(
    shiny::isolate(store$rv$report),
    name = "Renamed"
  )
  shiny::isolate(commit(store, new_rep))
  expect_identical(shiny::isolate(store$rv$report)@name, "Renamed")
  expect_true(shiny::isolate(store$rv$dirty))
})

test_that("undo/redo round-trips the exact prior S7 tree", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  original_json <- arpillar::report_to_json(shiny::isolate(store$rv$report))

  new_rep <- S7::set_props(
    shiny::isolate(store$rv$report),
    name = "Edited"
  )
  shiny::isolate(commit(store, new_rep))
  edited_json <- arpillar::report_to_json(shiny::isolate(store$rv$report))
  expect_false(identical(original_json, edited_json))

  expect_true(shiny::isolate(can_undo(store)))
  shiny::isolate(undo(store))
  expect_identical(
    arpillar::report_to_json(shiny::isolate(store$rv$report)),
    original_json
  )

  expect_true(shiny::isolate(can_redo(store)))
  shiny::isolate(redo(store))
  expect_identical(
    arpillar::report_to_json(shiny::isolate(store$rv$report)),
    edited_json
  )
})

test_that("undo/redo are no-ops on an empty stack", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  rep0 <- shiny::isolate(store$rv$report)

  expect_null(shiny::isolate(undo(store)))
  expect_identical(shiny::isolate(store$rv$report), rep0)
  expect_null(shiny::isolate(redo(store)))
  expect_identical(shiny::isolate(store$rv$report), rep0)
})

test_that("can_undo/can_redo are FALSE at the boundaries", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  expect_false(shiny::isolate(can_undo(store)))
  expect_false(shiny::isolate(can_redo(store)))

  new_rep <- S7::set_props(shiny::isolate(store$rv$report), name = "X")
  shiny::isolate(commit(store, new_rep))
  expect_true(shiny::isolate(can_undo(store)))
  expect_false(shiny::isolate(can_redo(store)))

  shiny::isolate(undo(store))
  expect_false(shiny::isolate(can_undo(store)))
  expect_true(shiny::isolate(can_redo(store)))
})

test_that("a fresh commit after undo clears the redo stack", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  rep0 <- shiny::isolate(store$rv$report)

  shiny::isolate(commit(store, S7::set_props(rep0, name = "A")))
  shiny::isolate(undo(store))
  expect_true(shiny::isolate(can_redo(store)))

  shiny::isolate(commit(store, S7::set_props(rep0, name = "B")))
  expect_false(shiny::isolate(can_redo(store)))
})

test_that("the undo stack is capped at 50 entries", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  rep0 <- shiny::isolate(store$rv$report)
  for (i in seq_len(60L)) {
    shiny::isolate(commit(store, S7::set_props(rep0, name = paste0("v", i))))
  }
  expect_length(store$undo$stack, 50L)
  # the oldest surviving entry is v11 (v1..v10 fell off the cap)
  first_kept <- store$undo$stack[[1L]]
  expect_identical(first_kept@name, "v10")
})

# ---- add_from_preset / add_from_generator ---------------------------------

test_that("add_from_preset copies type/title/footnotes/options, binds dataset, selects", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))

  obj <- shiny::isolate(selected_object(store))
  pr <- arpillar::preset("demographics")
  expect_identical(obj@id, id)
  expect_identical(obj@type, "summary")
  expect_identical(obj@dataset, "ADSL")
  expect_identical(obj@title, pr$title)
  expect_identical(obj@footnotes, as.character(pr$footnotes))
  expect_identical(obj@options$number, pr$options$number)
  expect_identical(obj@options$number_label, pr$options$number_label)
  expect_identical(shiny::isolate(store$rv$selected), id)
})

test_that("add_from_preset builds roles with role_type resolved from the demo catalog", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  obj <- shiny::isolate(selected_object(store))

  slots <- vapply(obj@roles, function(r) r@slot, character(1))
  expect_setequal(slots, c("treatment", "summarize"))

  summarize_role <- obj@roles[[which(slots == "summarize")]]
  items <- summarize_role@items
  names_ <- vapply(items, function(it) it@name, character(1))
  types_ <- vapply(items, function(it) it@role_type, character(1))
  # AGE/SEX/RACE are all real ADSL columns: AGE numeric (measure), SEX/RACE
  # character (category).
  expect_identical(names_, c("AGE", "SEX", "RACE"))
  expect_identical(types_, c("measure", "category", "category"))
})

test_that("add_from_preset sets population for an occurrence preset", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  # ae_overall targets an AE-domain dataset; the demo catalog has none, so
  # apply it against ADSL to prove the fail-loud contract: AEDECOD is
  # absent from ADSL and must default to role_type "category", never be
  # silently dropped from the built roles.
  id <- shiny::isolate(add_from_preset(store, "ae_overall", "ADSL"))
  obj <- shiny::isolate(selected_object(store))

  expect_identical(obj@type, "occurrence")
  expect_identical(obj@options$population, "ADSL")
  slots <- vapply(obj@roles, function(r) r@slot, character(1))
  hier_role <- obj@roles[[which(slots == "hierarchy")]]
  expect_identical(hier_role@items[[1]]@name, "AEDECOD")
  expect_identical(hier_role@items[[1]]@role_type, "category")
})

test_that("add_from_generator makes a bare object with an auto-suggested number", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))

  obj <- shiny::isolate(selected_object(store))
  expect_identical(obj@id, id)
  expect_identical(obj@type, "summary")
  expect_identical(obj@dataset, "ADSL")
  expect_length(obj@roles, 0L)
  expect_identical(obj@options$number, "14.1.1")
  expect_identical(obj@options$number_label, "Table")
  expect_identical(shiny::isolate(store$rv$selected), id)
})

test_that("add_from_generator auto-suggests the next free number within the same kind", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  shiny::isolate(add_from_preset(store, "demographics", "ADSL")) # seeds 14.1.1
  id2 <- shiny::isolate(add_from_generator(store, "crosstab", "ADSL"))

  obj2 <- shiny::isolate(selected_object(store))
  expect_identical(obj2@id, id2)
  expect_identical(obj2@options$number, "14.1.2")
})

test_that("add_from_preset/add_from_generator ids are monotonic sprintf('out%03d', n)", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id1 <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  id2 <- shiny::isolate(add_from_generator(store, "crosstab", "ADSL"))
  expect_identical(id1, "out001")
  expect_identical(id2, "out002")
})

# ---- update_object / remove_output / move_output / rename -----------------

test_that("selected_object returns NULL when nothing is selected", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  expect_null(shiny::isolate(selected_object(store)))
})

test_that("update_object edits one object; siblings are untouched", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id1 <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  id2 <- shiny::isolate(add_from_preset(store, "disposition", "ADSL"))

  shiny::isolate(update_object(store, id1, function(o) {
    S7::set_props(o, title = "Edited title")
  }))

  rep <- shiny::isolate(store$rv$report)
  obj1 <- .find_object(rep, id1)
  obj2 <- .find_object(rep, id2)
  expect_identical(obj1@title, "Edited title")
  expect_identical(obj2@title, arpillar::preset("disposition")$title)
})

test_that("update_object on an unknown id is a no-op", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  rep_before <- shiny::isolate(store$rv$report)

  shiny::isolate(update_object(store, "nope", function(o) {
    S7::set_props(o, title = "should never apply")
  }))
  expect_identical(shiny::isolate(store$rv$report), rep_before)
  expect_identical(
    .find_object(shiny::isolate(store$rv$report), id)@title,
    arpillar::preset("demographics")$title
  )
})

test_that("remove_output clears a dangling selected pointer", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  expect_identical(shiny::isolate(store$rv$selected), id)

  shiny::isolate(remove_output(store, id))
  expect_null(shiny::isolate(store$rv$selected))
  expect_null(.find_object(shiny::isolate(store$rv$report), id))
})

test_that("remove_output leaves selected untouched when a different id is removed", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id1 <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  id2 <- shiny::isolate(add_from_preset(store, "disposition", "ADSL"))
  # id2 is currently selected (add_from_preset selects the newly added object)
  shiny::isolate(remove_output(store, id1))
  expect_identical(shiny::isolate(store$rv$selected), id2)
})

test_that("move_output reorders and rename_output relabels", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id1 <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  id2 <- shiny::isolate(add_from_preset(store, "disposition", "ADSL"))

  shiny::isolate(move_output(store, id2, 1L))
  ids <- vapply(
    .all_objects(shiny::isolate(store$rv$report)),
    function(o) o@id,
    character(1)
  )
  expect_identical(ids, c(id2, id1))

  shiny::isolate(rename_output(store, id1, "New title"))
  expect_identical(
    .find_object(shiny::isolate(store$rv$report), id1)@title,
    "New title"
  )
})

# ---- open_card / close_card / toggle_pin ---------------------------------

test_that("open_card sets region and opens the card", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  shiny::isolate(open_card(store, "title"))
  expect_identical(shiny::isolate(store$rv$region), "title")
  expect_true(shiny::isolate(store$rv$card))
})

test_that("close_card closes an unpinned card", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  shiny::isolate(open_card(store, "rows"))
  shiny::isolate(close_card(store))
  expect_false(shiny::isolate(store$rv$card))
})

test_that("close_card is a no-op while pinned", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  shiny::isolate(open_card(store, "rows"))
  shiny::isolate(toggle_pin(store))
  expect_true(shiny::isolate(store$rv$pinned))

  shiny::isolate(close_card(store))
  expect_true(shiny::isolate(store$rv$card))

  shiny::isolate(toggle_pin(store))
  expect_false(shiny::isolate(store$rv$pinned))
  shiny::isolate(close_card(store))
  expect_false(shiny::isolate(store$rv$card))
})

# ---- cached_ard: the two-stage seam ---------------------------------------

test_that("cached_ard HITs on an options-only change, MISSes on a role change", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(update_object(store, id, function(o) {
    S7::set_props(
      o,
      roles = list(
        arpillar::role(
          slot = "treatment",
          items = list(arpillar::data_item(name = "TRT01P"))
        ),
        arpillar::role(
          slot = "summarize",
          items = list(arpillar::data_item(
            name = "AGE",
            role_type = "measure"
          ))
        )
      )
    )
  }))

  obj <- shiny::isolate(selected_object(store))
  ard1 <- shiny::isolate(cached_ard(store, obj))
  n_after_first <- sum(startsWith(ls(store$cache), "ard::"))
  expect_identical(n_after_first, 1L)

  # options-only edit: cache HIT, no new entry
  obj_opt <- S7::set_props(obj, options = list(decimals = 2L))
  ard2 <- shiny::isolate(cached_ard(store, obj_opt))
  expect_identical(sum(startsWith(ls(store$cache), "ard::")), 1L)
  expect_identical(ard1, ard2)

  # role edit (add a summarize item): cache MISS, new entry
  obj_role <- S7::set_props(
    obj,
    roles = list(
      obj@roles[[1]],
      arpillar::role(
        slot = "summarize",
        items = list(
          arpillar::data_item(name = "AGE", role_type = "measure"),
          arpillar::data_item(name = "SEX", role_type = "category")
        )
      )
    )
  )
  shiny::isolate(cached_ard(store, obj_role))
  expect_identical(sum(startsWith(ls(store$cache), "ard::")), 2L)
})

test_that("cached_ard MISSes on a filter change", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(update_object(store, id, function(o) {
    S7::set_props(
      o,
      roles = list(
        arpillar::role(
          slot = "treatment",
          items = list(arpillar::data_item(name = "TRT01P"))
        ),
        arpillar::role(
          slot = "summarize",
          items = list(arpillar::data_item(
            name = "AGE",
            role_type = "measure"
          ))
        )
      )
    )
  }))
  obj <- shiny::isolate(selected_object(store))
  shiny::isolate(cached_ard(store, obj))
  expect_identical(sum(startsWith(ls(store$cache), "ard::")), 1L)

  obj_filt <- S7::set_props(
    obj,
    filters = list(list(column = "SEX", op = "==", value = "F"))
  )
  shiny::isolate(cached_ard(store, obj_filt))
  expect_identical(sum(startsWith(ls(store$cache), "ard::")), 2L)
})

# ---- log_line ---------------------------------------------------------

test_that("log_line appends a timestamped line onto rv$log", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  shiny::isolate(log_line(store, "hello"))
  log <- shiny::isolate(store$rv$log)
  expect_length(log, 1L)
  expect_match(log[[1]], "hello", fixed = TRUE)
})

# ---- THE SUSPEND-CONTRACT REGRESSION --------------------------------------

test_that("REGRESSION: config committed via the store survives with no UI mounted", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(update_object(store, id, function(o) {
    S7::set_props(o, options = list(decimals = 2L))
  }))
  json <- arpillar::report_to_json(shiny::isolate(store$rv$report))
  back <- arpillar::report_from_json(json)
  expect_identical(.find_object(back, id)@options$decimals, 2L)
})
