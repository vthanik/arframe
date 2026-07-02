# The typeset sheet: renders the SELECTED output through the export-
# identical arpillar::render_spec()/render_ggplot() seam, or the ghost
# shell / GOV.UK error summary when it is not ready / fails. All fixtures
# here are hand-built against the REAL demo-catalog columns -- the bundled
# `demographics` preset requests a RACE column the minimal demo ADSL does
# not carry (a pre-existing demo-catalog/preset gap, ledger-documented, not
# this module's concern), so a "ready and renders cleanly" fixture is
# built directly off `.tc_ready_summary()`-style roles instead of added
# via `add_from_preset(store, "demographics", "ADSL")`.

# ---- fixtures --------------------------------------------------------------

#' A store with one READY summary object (treatment=TRT01P, summarize=
#' AGE/SEX -- both real demo ADSL columns), title/number/footnote set so
#' the title-block and population-line assertions have real content to
#' match against.
#' @noRd
.pp_ready_store <- function() {
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
            ),
            arpillar::data_item(
              name = "SEX",
              label = "Sex",
              role_type = "category"
            )
          )
        )
      )
    )
  }))
  list(con = con, store = store, id = id)
}

#' A store with one READY figure object (line: x=AVISIT, y=AVAL,
#' group=TRT01P -- all real demo ADVS columns; the bundled
#' `mean_over_time` preset targets a CHG column absent from the demo ADVS,
#' the figure analogue of the RACE gap above).
#' @noRd
.pp_ready_figure_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_generator(store, "line", "ADVS"))
  shiny::isolate(update_object(store, id, function(o) {
    S7::set_props(
      o,
      title = "Mean Systolic BP by Visit",
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
  list(con = con, store = store, id = id)
}

#' A store with one DRAFT summary object (no roles filled).
#' @noRd
.pp_draft_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))
  list(con = con, store = store, id = id)
}

#' A store with one summary object whose role names a column absent from
#' the bound dataset (`output_status() == "ready"` -- role slots are
#' filled -- but the render leg throws, the STATIC-ORACLE GAP the ledger
#' documents: role completeness != column existence).
#' @noRd
.pp_bogus_col_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))
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
            name = "BOGUSVAR",
            role_type = "measure"
          ))
        )
      )
    )
  }))
  expect_identical(
    arpillar::output_status(shiny::isolate(selected_object(store))),
    "ready"
  )
  list(con = con, store = store, id = id)
}

# ---- ready + table ------------------------------------------------------

test_that("a READY table renders tabular-doc markup with arm names and the title-block number", {
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      html <- output$sheet_html_slot$html
      expect_match(html, "tabular-doc", fixed = TRUE)
      expect_match(html, "Placebo", fixed = TRUE)
      expect_match(html, "Xanomeline", fixed = TRUE)
      expect_match(html, "14.1.1", fixed = TRUE)
      expect_match(
        html,
        "Demographics and Baseline Characteristics",
        fixed = TRUE
      )
      expect_match(html, "Safety Population.", fixed = TRUE)
      expect_match(html, "Source:", fixed = TRUE)
      expect_identical(fx$store$rv$broken, character(0))
    }
  )
})

test_that("a READY table's rendered content is inside the paper's table-wrap region", {
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      html <- output$sheet_html_slot$html
      expect_match(html, "ar-paper-table-wrap", fixed = TRUE)
      expect_match(html, "ar-paper-runninghead", fixed = TRUE)
      expect_match(html, "ar-paper-title-block", fixed = TRUE)
    }
  )
})

# ---- ready + occurrence (ae_overall on ADAE) -------------------------------

test_that("a READY occurrence object (ae_overall on ADAE) renders its incidence rows", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  shiny::isolate(add_from_preset(store, "ae_overall", "ADAE"))

  shiny::testServer(
    mod_paper_server,
    args = list(store = store),
    {
      html <- output$sheet_html_slot$html
      expect_match(html, "tabular-doc", fixed = TRUE)
      # ae_overall's hierarchy role is AEDECOD only (PT-level, no SOC) --
      # every PT in the demo ADAE fixture appears as a plain row.
      expect_match(html, "Nausea", fixed = TRUE)
      expect_match(html, "Vomiting", fixed = TRUE)
      expect_match(html, "Atrial fibrillation", fixed = TRUE)
      expect_identical(store$rv$broken, character(0))
    }
  )
})

# ---- ready + figure -------------------------------------------------------

test_that("a READY figure does not error and the figure container is selected", {
  fx <- .pp_ready_figure_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      html <- output$sheet_html_slot$html
      expect_no_match(html, "ar-problem", fixed = TRUE)
      expect_match(html, "ar-paper-title-block", fixed = TRUE)
      expect_match(html, "Mean Systolic BP by Visit", fixed = TRUE)
      expect_identical(fx$store$rv$broken, character(0))
      expect_false(is.null(output$sheet_figure))
    }
  )
})

# ---- not ready: ghost ------------------------------------------------------

test_that("a DRAFT object renders ghost markup, not table/error content", {
  fx <- .pp_draft_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      html <- output$sheet_html_slot$html
      expect_match(html, "ar-ghost-slot", fixed = TRUE)
      expect_no_match(html, "tabular-doc", fixed = TRUE)
      expect_no_match(html, "ar-problem", fixed = TRUE)
    }
  )
})

test_that("no selection at all renders the empty-report ghost with the CTA", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = store),
    {
      html <- output$sheet_html_slot$html
      expect_match(html, "Add your first output", fixed = TRUE)
      expect_match(html, "ar-ghost-cta", fixed = TRUE)
    }
  )
})

test_that("clicking the empty-report CTA sets rv$adding TRUE", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = store),
    {
      expect_false(store$rv$adding)
      session$setInputs(add_first = 1)
      expect_true(store$rv$adding)
    }
  )
})

# ---- render error: bogus column --------------------------------------------

test_that("a role naming a bogus column lands its id in rv$broken with alert + jump-link markup", {
  fx <- .pp_bogus_col_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      html <- output$sheet_html_slot$html
      expect_identical(fx$store$rv$broken, fx$id)
      expect_match(html, 'role="alert"', fixed = TRUE)
      expect_match(html, "There is a problem", fixed = TRUE)
      expect_match(html, "ar-problem", fixed = TRUE)
      expect_match(html, "BOGUSVAR", fixed = TRUE)
      # a log line was appended recording the failure.
      expect_true(any(grepl(fx$id, fx$store$rv$log, fixed = TRUE)))
    }
  )
})

test_that("a fixed re-render clears the id from rv$broken", {
  fx <- .pp_bogus_col_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      output$sheet_html_slot$html
      expect_identical(fx$store$rv$broken, fx$id)

      update_object(fx$store, fx$id, function(o) {
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
      })
      session$flushReact()
      html2 <- output$sheet_html_slot$html
      expect_no_match(html2, "ar-problem", fixed = TRUE)
      expect_match(html2, "tabular-doc", fixed = TRUE)
      expect_identical(fx$store$rv$broken, character(0))
    }
  )
})

# ---- region click -> open_card ---------------------------------------------

test_that("input$region routes through open_card: sets rv$region and rv$card", {
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      expect_false(fx$store$rv$card)
      session$setInputs(region = "columns")
      expect_identical(fx$store$rv$region, "columns")
      expect_true(fx$store$rv$card)
    }
  )
})

# ---- two-stage cache seam ---------------------------------------------

test_that("an options-only edit re-renders WITHOUT adding a new ard:: cache entry", {
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      output$sheet_html_slot$html
      n1 <- sum(startsWith(ls(fx$store$cache), "ard::"))
      expect_identical(n1, 1L)

      update_object(fx$store, fx$id, function(o) {
        S7::set_props(o, options = list(decimals = 2L))
      })
      session$flushReact()
      html2 <- output$sheet_html_slot$html
      n2 <- sum(startsWith(ls(fx$store$cache), "ard::"))
      expect_identical(n2, 1L)
      expect_match(html2, "tabular-doc", fixed = TRUE)
    }
  )
})

test_that("a role edit re-renders WITH a new ard:: cache entry (cache MISS)", {
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      output$sheet_html_slot$html
      n1 <- sum(startsWith(ls(fx$store$cache), "ard::"))

      update_object(fx$store, fx$id, function(o) {
        S7::set_props(
          o,
          roles = list(
            o@roles[[1]],
            arpillar::role(
              slot = "summarize",
              items = list(arpillar::data_item(
                name = "AGE",
                role_type = "measure"
              ))
            )
          )
        )
      })
      session$flushReact()
      output$sheet_html_slot$html
      n2 <- sum(startsWith(ls(fx$store$cache), "ard::"))
      expect_identical(n2, n1 + 1L)
    }
  )
})

# ---- fit/page toggle -----------------------------------------------------

test_that("the fit/page toolbar buttons are present with the fieldset/legend grouping", {
  ui <- mod_paper_ui("paper")
  html <- as.character(ui)
  expect_match(html, "Preview width", fixed = TRUE)
  expect_match(html, "<fieldset", fixed = TRUE)
  expect_match(html, "<legend", fixed = TRUE)
  expect_match(html, 'id="paper-fit_btn"', fixed = TRUE)
  expect_match(html, 'id="paper-page_btn"', fixed = TRUE)
})

test_that("clicking fit/page posts the ar-paper-width session message (no error)", {
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      expect_no_error(session$setInputs(page_btn = 1))
      expect_no_error(session$setInputs(fit_btn = 1))
    }
  )
})

# ---- UI shape --------------------------------------------------------------

test_that("mod_paper_ui mounts both the html slot and the figure plot slot, class-flipped by kind", {
  ui <- mod_paper_ui("paper")
  html <- as.character(ui)
  expect_match(html, "ar-paper", fixed = TRUE)
  expect_match(html, 'data-ar-paper="paper"', fixed = TRUE)
  expect_match(html, "ar-paper-figure-slot", fixed = TRUE)
  expect_match(html, 'id="paper-sheet_html_slot"', fixed = TRUE)
  expect_match(html, 'id="paper-sheet_figure"', fixed = TRUE)
})

# ---- JS bridge smoke test ----------------------------------------------

test_that("arframe.js contains the region-click delegation and the drag-guard queue", {
  js <- readLines(
    system.file("www", "arframe.js", package = "arframe"),
    warn = FALSE
  )
  txt <- paste(js, collapse = "\n")
  expect_match(txt, "data-ar-region", fixed = TRUE)
  expect_match(txt, "arRegionClick", fixed = TRUE)
  expect_match(txt, "arFlushDeferredRegionClicks", fixed = TRUE)
  expect_match(txt, "arDeferredRegionClicks", fixed = TRUE)
  expect_match(txt, "ar-paper-kind", fixed = TRUE)
  expect_match(txt, "ar-paper-width", fixed = TRUE)
  # a ghost slot's `role="button"` + `tabindex="0"` promise (utils_ghost.R)
  # needs a matching Enter/Space keydown handler -- a plain div does not
  # natively fire `click` on those keys the way a real <button> does.
  expect_match(txt, "keydown", fixed = TRUE)
})

# ---- end-to-end: the real arframe() launcher, a real browser ---------------

test_that("arframe() paper: a READY table renders live, screenshots the payoff", {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("apps/paper"),
    name = "paper",
    height = 900,
    width = 1440
  )
  withr::defer(app$stop())

  # Nothing is selected on launch (the fixture report carries 4 outputs but
  # no `rv$selected` pointer -- matching a freshly reopened project) --
  # click the READY demographics row first, exactly like
  # test-mod_contents.R's own e2e click, before asserting selection-
  # dependent content.
  app$click(selector = '.ar-toc-row[data-ar-id="out001"]')
  app$wait_for_idle()

  html <- app$get_html("body", outer_html = TRUE)
  expect_match(html, "tabular-doc", fixed = TRUE)
  expect_match(html, "ar-paper", fixed = TRUE)

  screens_dir <- testthat::test_path("../../.local/screens")
  if (dir.exists(screens_dir)) {
    screenshot_path <- file.path(screens_dir, "09a-paper-table.png")
    if (file.exists(screenshot_path)) {
      file.remove(screenshot_path)
    }
    app$get_screenshot(screenshot_path)
  }
})

test_that("arframe() paper: a focused ghost slot fires input$region on Enter (keyboard activation)", {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("apps/paper"),
    name = "paper-keyboard",
    height = 900,
    width = 1440
  )
  withr::defer(app$stop())

  # out003 is the DRAFT crosstab (ghost shell).
  app$click(selector = '.ar-toc-row[data-ar-id="out003"]')
  app$wait_for_idle()

  app$run_js("document.querySelector('[data-ar-region][tabindex]').focus();")
  focused_region <- app$get_js(
    "document.activeElement.getAttribute('data-ar-region')"
  )
  expect_identical(focused_region, "columns")

  app$run_js(paste0(
    "var el = document.activeElement;",
    "var ev = new KeyboardEvent('keydown', {key: 'Enter', bubbles: true, cancelable: true});",
    "el.dispatchEvent(ev);"
  ))
  app$wait_for_idle()
  expect_identical(app$get_value(input = "paper-region"), "columns")
})

test_that("arframe() paper: the fit/page toolbar toggles the sheet width class live", {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("apps/paper"),
    name = "paper-toggle",
    height = 900,
    width = 1440
  )
  withr::defer(app$stop())

  app$click(selector = '.ar-toc-row[data-ar-id="out001"]')
  app$wait_for_idle()

  app$click(selector = "#paper-page_btn")
  app$wait_for_idle()
  page_class <- app$get_js("document.getElementById('paper-sheet').className")
  expect_match(page_class, "ar-paper--page", fixed = TRUE)

  app$click(selector = "#paper-fit_btn")
  app$wait_for_idle()
  fit_class <- app$get_js("document.getElementById('paper-sheet').className")
  expect_match(fit_class, "ar-paper--fit", fixed = TRUE)
})
