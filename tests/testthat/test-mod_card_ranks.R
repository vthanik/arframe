# The Ranks pane (mod_card_ranks): ordering semantics in one place --
# summarize row-block order (summary/crosstab, via the SAME .reorder_slot
# helper the Roles pane uses), occurrence hier_sort, and the x_order level
# list relocated from the Options pane. km gets an honest empty state.

.mcr_ranks_store <- function(type = "summary", dataset = "ADSL") {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  id <- if (identical(type, "demographics")) {
    shiny::isolate(add_from_preset(store, "demographics", dataset))
  } else {
    shiny::isolate(add_from_generator(store, type, dataset))
  }
  shiny::isolate(store$rv$selected <- id)
  list(con = con, store = store, id = id)
}

test_that("summary: drag-reordering row blocks commits through the shared helper", {
  fx <- .mcr_ranks_store("demographics")
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_ranks_server, args = list(store = fx$store), {
    session$flushReact()
    html <- output$pane$html
    expect_match(html, "ROW BLOCKS", fixed = TRUE)
    for (v in c("AGE", "SEX", "RACE")) {
      expect_match(html, sprintf('data-ar-item="%s"', v), fixed = TRUE)
    }

    session$setInputs(rank_items = list(order = list("RACE", "AGE", "SEX")))
    obj <- shiny::isolate(selected_object(store))
    got <- vapply(obj@roles[[2]]@items, function(it) it@name, character(1))
    expect_identical(got, c("RACE", "AGE", "SEX"))
  })
})

test_that("occurrence: hier_sort commits alpha and elides the freq default", {
  fx <- .mcr_ranks_store("occurrence", "ADAE")
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_ranks_server, args = list(store = fx$store), {
    session$flushReact()
    expect_match(output$pane$html, "INCIDENCE ORDER", fixed = TRUE)

    session$setInputs(hier_sort = "alpha")
    expect_identical(
      shiny::isolate(selected_object(store))@options$hier_sort,
      "alpha"
    )
    # Back to the engine default -> the key elides.
    session$setInputs(hier_sort = "freq")
    expect_null(shiny::isolate(selected_object(store))@options$hier_sort)
  })
})

test_that("line: x_order renders a sortable list seeded from distinct_values", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_generator(store, "line", "ADVS"))
  shiny::isolate(update_object(store, id, function(o) {
    S7::set_props(
      o,
      roles = list(
        arpillar::role(
          slot = "x",
          items = list(arpillar::data_item(name = "AVISIT"))
        )
      )
    )
  }))
  shiny::isolate(store$rv$selected <- id)

  shiny::testServer(mod_card_ranks_server, args = list(store = store), {
    session$flushReact()
    html <- output$pane$html
    expect_match(html, "data-ar-sortable", fixed = TRUE)
    for (lv in c("Baseline", "Week 4", "Week 8")) {
      expect_match(html, sprintf('data-ar-item="%s"', lv), fixed = TRUE)
    }

    # A drop commits the explicit order.
    session$setInputs(
      opt_reorder_x_order = list(order = list("Week 8", "Baseline", "Week 4"))
    )
    expect_identical(
      shiny::isolate(selected_object(store))@options$x_order,
      c("Week 8", "Baseline", "Week 4")
    )
  })
})

test_that("line: the pane directs to Roles until the x slot is filled", {
  fx <- .mcr_ranks_store("line", "ADVS")
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_ranks_server, args = list(store = fx$store), {
    session$flushReact()
    html <- output$pane$html
    expect_no_match(html, "data-ar-sortable", fixed = TRUE)
    expect_match(html, "Assign an X variable in Roles first", fixed = TRUE)
  })
})

test_that("km: the empty state names why nothing is rankable", {
  fx <- .mcr_ranks_store("km", "ADTTE")
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_ranks_server, args = list(store = fx$store), {
    session$flushReact()
    expect_match(
      output$pane$html,
      "ranks itself along time",
      fixed = TRUE
    )
  })
})

test_that("the ranks pane keeps computing while hidden", {
  # outputOptions() is not introspectable under testServer's mock session:
  # pin the call in the server body (works against the installed package).
  src <- paste(deparse(body(mod_card_ranks_server)), collapse = "\n")
  expect_match(
    src,
    'outputOptions(output, "pane", suspendWhenHidden = FALSE)',
    fixed = TRUE
  )
})
