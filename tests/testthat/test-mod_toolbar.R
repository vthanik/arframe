# The canvas toolbar (2026-07-04): Run / .rtf / Output|Code at the top of
# the desk. The visible controls are the Preact component; these tests
# exercise the server side -- the Run re-typeset, the RTF seam, the view
# input, and the state push contract.

# A minimal READY summary output on ADSL, selected -- mirrors
# test-mod_card.R's fixture (kept local per the no-shared-ambient rule).
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

test_that("mod_toolbar_ui: the Preact mount div and the hidden download link", {
  html <- as.character(mod_toolbar_ui("toolbar"))
  expect_match(html, "ar-toolbar", fixed = TRUE)
  expect_match(html, 'data-ar-toolbar="toolbar"', fixed = TRUE)
  expect_match(html, 'id="toolbar-rtf"', fixed = TRUE)
  expect_match(html, "ar-hidden-dl", fixed = TRUE)
})

test_that("mod_toolbar_ui: the panel_toggle button is present", {
  # 2026-07-10: the inspector's icon rail is gone, so the fold/re-open
  # affordance is this quiet icon button at the toolbar's right end.
  html <- as.character(mod_toolbar_ui("toolbar"))
  expect_match(html, 'id="toolbar-panel_toggle"', fixed = TRUE)
  expect_match(html, "ar-tb-icon-btn", fixed = TRUE)
})

test_that("mod_toolbar_server: panel_toggle flips insp_collapsed and mirrors ar-collapse", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_toolbar_server, args = list(store = fx$store), {
    expect_false(isTRUE(store$rv$insp_collapsed))
    session$setInputs(panel_toggle = 1)
    expect_true(store$rv$insp_collapsed)
    session$setInputs(panel_toggle = 2)
    expect_false(store$rv$insp_collapsed)
  })
})

test_that("mod_toolbar_server: the ar-collapse send preserves the LoC rail state", {
  # Regression (2026-07-10 review): bridge.js toggles ALL three workspace
  # classes off one message, so a payload missing `loc_rail` forces a
  # collapsed CONTENTS rail back open. Capture the message MockShinySession
  # otherwise discards by overriding its sendCustomMessage.
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_toolbar_server, args = list(store = fx$store), {
    sent <- NULL
    # `session` here is shiny's session_proxy, which delegates method
    # lookup to its parent -- walk to the root MockShinySession and
    # override its (discarding) sendCustomMessage to record the payload.
    root <- session
    while (inherits(root, "session_proxy")) {
      root <- get("parent", envir = root)
    }
    base::unlockBinding("sendCustomMessage", root)
    assign(
      "sendCustomMessage",
      function(type, message) {
        if (identical(type, "ar-collapse")) sent <<- message
      },
      envir = root
    )
    store$rv$loc_rail_collapsed <- TRUE
    session$setInputs(panel_toggle = 1)
    expect_true(sent$insp)
    expect_true(sent$loc_rail)
    expect_false(sent$rail)
  })
})

test_that("mod_toolbar_server: Run drops the ARD memo and bumps run_nonce", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_toolbar_server, args = list(store = fx$store), {
    # Prime the memo through the same seam the paper uses.
    obj <- shiny::isolate(selected_object(store))
    invisible(cached_ard(store, obj))
    expect_length(grep("^ard::", ls(store$cache)), 1L)

    # A heavy edit marked the proof stale; Run must clear it.
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

test_that("mod_toolbar_server: the Output|Code view input drives rv$code_view", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_toolbar_server, args = list(store = fx$store), {
    expect_false(store$rv$code_view)
    session$setInputs(view = "code")
    expect_true(store$rv$code_view)
    session$setInputs(view = "output")
    expect_false(store$rv$code_view)
  })
})

test_that("mod_toolbar_server: the .rtf download names and writes a non-empty RTF", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_toolbar_server, args = list(store = fx$store), {
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

    # Paper parity: the emitted RTF carries the TLF number line and the
    # footnote EXACTLY once (as a footnote -- the old title promotion was
    # removed 2026-07-04). The auto source line was removed 2026-07-09.
    txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(txt, "Table 14.1.1", fixed = TRUE)
    expect_identical(
      lengths(regmatches(
        txt,
        gregexpr("Safety Population.", txt, fixed = TRUE)
      )),
      1L
    )
    expect_no_match(txt, "Source:", fixed = TRUE)
    # No source is set on the live store object either.
    expect_null(shiny::isolate(selected_object(store))@options$source)
  })
})

test_that("mod_toolbar_server: the .rtf download renders a FIGURE through the figure seam", {
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

  shiny::testServer(mod_toolbar_server, args = list(store = store), {
    path <- output$rtf
    expect_match(basename(path), "^f-14-2-1")
    expect_gt(file.size(path), 0)
  })
})

test_that("mod_toolbar_server: rtf_click relays an ar-click to the hidden link", {
  fx <- .mc_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_toolbar_server, args = list(store = fx$store), {
    # MockShinySession has no custom-message recorder; the relay is a
    # one-line sendCustomMessage whose delivery the paper e2e app covers.
    # Here: the observer runs without error.
    expect_no_error(session$setInputs(rtf_click = 1))
  })
})
