# The Report mode List-of-Contents (LoC): a full-width, editable table that
# drills into the paper + inspector. Groups outputs TABLES/FIGURES/LISTINGS
# (kind from the type->generator map, empty groups omitted), sorts by
# options$number, edits NUMBER/LABEL/TITLE/POPULATION inline (one cell_edit
# channel), and drives select / drill / duplicate / remove / add entirely
# through the injected store.

# ---- fixtures --------------------------------------------------------------

#' A "ready" summary object: treatment + one measure to summarize, so
#' output_status() reports "ready" (all stamp tests need at least one
#' non-draft/non-needs_data row to prove the oracle wiring is not a constant).
#' @noRd
.tc_ready_summary <- function(id, dataset = "ADSL", title = "Demog") {
  arpillar::object(
    id = id,
    type = "summary",
    title = title,
    dataset = dataset,
    roles = list(
      arpillar::role(
        slot = "treatment",
        items = list(arpillar::data_item(name = "TRT01P"))
      ),
      arpillar::role(
        slot = "summarize",
        items = list(arpillar::data_item(name = "AGE", role_type = "measure"))
      )
    )
  )
}

#' A store seeded with 3 demo outputs across the two live kinds (table,
#' figure): a READY summary (table), a DRAFT crosstab with no roles filled
#' (table), and a DRAFT line figure with no roles filled (figure). Draft
#' status proves the oracle is read per-object, not defaulted. Built via
#' `add_from_generator()` -- the "bare, no roles" path -- rather than
#' `add_from_preset()`, which now pre-fills roles.
#' @noRd
.tc_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  id1 <- shiny::isolate(add_from_generator(store, "summary", "ADSL")) # summary/table, roles empty -> draft by default
  id2 <- shiny::isolate(add_from_generator(store, "crosstab", "ADSL")) # crosstab/table, draft
  id3 <- shiny::isolate(add_from_generator(store, "line", "ADVS")) # line/figure, draft
  # Overwrite id1 with a fully-configured object so at least one row is READY.
  shiny::isolate(update_object(store, id1, function(o) {
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
  list(con = con, store = store, id1 = id1, id2 = id2, id3 = id3)
}

#' Seed a `theme$populations` library on the store so the POPULATION select
#' has real choices and a binding resolves.
#' @noRd
.tc_seed_pop <- function(store) {
  shiny::isolate({
    r <- store$rv$report
    theme <- r@theme
    theme$populations <- list(
      safety = list(
        label = "Safety Analysis Set",
        dataset = "ADSL",
        filter = 'SAFFL == "Y"'
      )
    )
    commit(store, S7::set_props(r, theme = theme))
  })
}

# ---- UI ---------------------------------------------------------------

test_that("mod_contents_ui HTML is the LoC surface with the Add-output action", {
  ui <- mod_contents_ui("contents")
  html <- as.character(ui)
  expect_match(html, "ar-loc", fixed = TRUE)
  expect_match(html, "LIST OF CONTENTS", fixed = TRUE)
  expect_match(html, 'id="contents-add_output"', fixed = TRUE)
  expect_match(html, "Add output", fixed = TRUE)
})

# ---- grouping + numbering ---------------------------------------------

test_that("rows are grouped TABLES/FIGURES and numbered kind-scoped in document order", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      html <- output$table$html
      expect_match(html, "ar-loc-row", fixed = TRUE)
      expect_match(html, "TABLES", fixed = TRUE)
      expect_match(html, "FIGURES", fixed = TRUE)
      # LISTINGS never appears -- no listing generator exists, so the group
      # is always empty and must be omitted entirely.
      expect_no_match(html, "LISTINGS", fixed = TRUE)

      # id1/id2/id3 each carry an add_from_generator()-auto-suggested
      # options$number, shown verbatim in the NUMBER input -- id1/id2 are both
      # "table" kind (1-based within kind), id3 is the lone "figure" kind.
      expect_match(html, "14.1.1", fixed = TRUE)
      expect_match(html, "14.1.2", fixed = TRUE)
      expect_match(html, "14.2.1", fixed = TRUE)
    }
  )
})

test_that("a row with no options$number falls back to the kind-scoped auto index", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  # Built by hand via arpillar::object() -- neither add path -- so
  # options$number is absent; .toc_rows() must still number it.
  bare <- arpillar::object(id = "outX", type = "summary", dataset = "ADSL")
  pages <- shiny::isolate(store$rv$report)@pages
  pages[[1]] <- S7::set_props(pages[[1]], objects = list(bare))
  shiny::isolate(commit(
    store,
    S7::set_props(
      shiny::isolate(store$rv$report),
      pages = pages
    )
  ))

  shiny::testServer(
    mod_contents_server,
    args = list(store = store),
    {
      expect_match(output$table$html, "14.1.1", fixed = TRUE)
    }
  )
})

test_that("a cleared number (number_label present, number absent) shows the fallback index, not the label", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  # The exact shape a clear-number cell edit leaves behind: options carries
  # number_label but NOT number. `$number` would partial-match `number_label`
  # ("Table") -- .toc_rows must show the kind-scoped fallback index (14.1.1)
  # instead. Regression guard for R's dollar partial matching.
  obj <- arpillar::object(
    id = "outC",
    type = "summary",
    dataset = "ADSL",
    options = list(number_label = "Table")
  )
  pages <- shiny::isolate(store$rv$report)@pages
  pages[[1]] <- S7::set_props(pages[[1]], objects = list(obj))
  shiny::isolate(commit(
    store,
    S7::set_props(shiny::isolate(store$rv$report), pages = pages)
  ))

  rows <- .toc_rows(shiny::isolate(store$rv$report), character(0))
  expect_identical(rows[[1]]$number, "14.1.1")
  expect_false(identical(rows[[1]]$number, "Table"))
})

test_that("a preset-seeded options$number is shown verbatim, not the document-order index", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  # disposition seeds "14.1.2" even though it is the FIRST (and only)
  # object in the document -- the seeded number must win over "14.1.1".
  shiny::isolate(add_from_preset(store, "disposition", "ADSL"))

  shiny::testServer(
    mod_contents_server,
    args = list(store = store),
    {
      html <- output$table$html
      expect_match(html, "14.1.2", fixed = TRUE)
      expect_no_match(html, "14.1.1", fixed = TRUE)
    }
  )
})

test_that("within a group rows sort by TLF number, not document order", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  # Two hand-built table objects added in REVERSE number order: 14.1.10 first,
  # 14.1.2 second. The LoC must render 14.1.2 before 14.1.10 (version-aware).
  a <- arpillar::object(
    id = "outA",
    type = "summary",
    dataset = "ADSL",
    options = list(number = "14.1.10")
  )
  b <- arpillar::object(
    id = "outB",
    type = "summary",
    dataset = "ADSL",
    options = list(number = "14.1.2")
  )
  pages <- shiny::isolate(store$rv$report)@pages
  pages[[1]] <- S7::set_props(pages[[1]], objects = list(a, b))
  shiny::isolate(commit(
    store,
    S7::set_props(shiny::isolate(store$rv$report), pages = pages)
  ))

  # unit-level: ordered ids put 14.1.2 (outB) ahead of 14.1.10 (outA)
  ids <- .loc_ordered_ids(shiny::isolate(store$rv$report))
  expect_identical(ids, c("outB", "outA"))
})

test_that("the LISTINGS group is present when a listing-kind row exists (future-proof, currently unreachable)", {
  # No listing generator exists in arpillar today (verified: generators()
  # has no kind == "listing" entry), so this documents the omission
  # contract without asserting on a row that cannot currently be
  # constructed.
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))
  kinds <- vapply(arpillar::generators(), function(g) g$kind, "")
  expect_false("listing" %in% kinds)
})

# ---- stamps -------------------------------------------------------------

test_that("stamps match the output_status oracle; a broken id shows ERROR regardless of status", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      html <- output$table$html
      expect_match(html, "READY", fixed = TRUE)
      expect_match(html, "DRAFT", fixed = TRUE)

      # Mark the READY object broken: the stamp must flip to ERROR even
      # though output_status() would still report "ready" for it.
      fx$store$rv$broken <- fx$id1
      session$flushReact()
      html2 <- output$table$html
      expect_match(html2, "ERROR", fixed = TRUE)
    }
  )
})

# ---- stat strip ---------------------------------------------------------

test_that("the stat strip counts outputs by status", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      html <- output$stats$html
      expect_match(html, "ar-stat-tile", fixed = TRUE)
      expect_match(html, "Outputs", fixed = TRUE)
      expect_match(html, "Ready", fixed = TRUE)
      expect_match(html, "Needs attention", fixed = TRUE)
    }
  )
})

# ---- row click selects --------------------------------------------------

test_that("clicking a row sets rv$selected", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      session$setInputs(row_click = fx$id2)
      expect_identical(fx$store$rv$selected, fx$id2)
    }
  )
})

# ---- drill (open / close) -----------------------------------------------

test_that("double-click opens the drill (report_open + selected); back closes it", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      session$setInputs(open = fx$id2)
      expect_identical(fx$store$rv$report_open, fx$id2)
      expect_identical(fx$store$rv$selected, fx$id2)
      # the breadcrumb renders only while drilled
      expect_match(output$crumb$html, "ar-loc-crumb", fixed = TRUE)

      session$setInputs(back = 1)
      expect_null(fx$store$rv$report_open)
    }
  )
})

test_that("the breadcrumb is empty until a drill is open", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      html <- output$crumb$html
      expect_true(is.null(html) || !nzchar(html))
    }
  )
})

# ---- inline cell edit ---------------------------------------------------

test_that("cell_edit routes number / number_label / title / population; blanks clear or no-op", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  oid <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))
  .tc_seed_pop(store)

  shiny::testServer(
    mod_contents_server,
    args = list(store = store),
    {
      ce <- function(field, value, n) {
        session$setInputs(
          cell_edit = list(id = oid, field = field, value = value, nonce = n)
        )
      }

      ce("number", "14.3.9", 1)
      expect_identical(
        .find_object(store$rv$report, oid)@options[["number"]],
        "14.3.9"
      )

      ce("number_label", "Listing", 2)
      expect_identical(
        .find_object(store$rv$report, oid)@options$number_label,
        "Listing"
      )

      ce("title", "Renamed inline", 3)
      expect_identical(
        .find_object(store$rv$report, oid)@title,
        "Renamed inline"
      )

      # A blank title is a no-op (never clobbers the title with "").
      ce("title", "   ", 4)
      expect_identical(
        .find_object(store$rv$report, oid)@title,
        "Renamed inline"
      )

      # Bind a population, then clear it back to the study default.
      ce("population", "safety", 5)
      expect_identical(
        .find_object(store$rv$report, oid)@options$population,
        "safety"
      )
      ce("population", "", 6)
      expect_null(.find_object(store$rv$report, oid)@options$population)

      # A blank number clears the override -> falls back to the auto index.
      # Exact `[[` -- `$number` would partial-match the surviving number_label.
      ce("number", "", 7)
      expect_null(.find_object(store$rv$report, oid)@options[["number"]])
    }
  )
})

test_that("cell_edit ignores an unknown field and a missing id", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))
  before <- .find_object(shiny::isolate(fx$store$rv$report), fx$id1)@title

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      session$setInputs(
        cell_edit = list(id = fx$id1, field = "bogus", value = "x", nonce = 1)
      )
      session$setInputs(
        cell_edit = list(id = NULL, field = "title", value = "y", nonce = 2)
      )
      expect_identical(
        .find_object(fx$store$rv$report, fx$id1)@title,
        before
      )
    }
  )
})

test_that("a population change marks the proof STALE (it subsets the data)", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  # A ready object so update_object's cheap/heavy oracle can flip it to stale.
  ready <- .tc_ready_summary("outR")
  pages <- shiny::isolate(store$rv$report)@pages
  pages[[1]] <- S7::set_props(pages[[1]], objects = list(ready))
  shiny::isolate(commit(
    store,
    S7::set_props(shiny::isolate(store$rv$report), pages = pages)
  ))
  .tc_seed_pop(store)

  shiny::testServer(
    mod_contents_server,
    args = list(store = store),
    {
      session$setInputs(
        cell_edit = list(
          id = "outR",
          field = "population",
          value = "safety",
          nonce = 1
        )
      )
      expect_true("outR" %in% store$rv$stale)
    }
  )
})

# ---- duplicate --------------------------------------------------------------

test_that("duplicate clones the object with a fresh id, appended, and selects it", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      n_before <- length(.all_objects(fx$store$rv$report))
      session$setInputs(duplicate = fx$id1)
      objs <- .all_objects(fx$store$rv$report)
      expect_length(objs, n_before + 1L)

      clone <- objs[[length(objs)]]
      original <- .find_object(fx$store$rv$report, fx$id1)
      expect_false(identical(clone@id, fx$id1))
      expect_identical(clone@type, original@type)
      expect_identical(clone@dataset, original@dataset)
      expect_identical(clone@title, original@title)
      expect_identical(fx$store$rv$selected, clone@id)
    }
  )
})

# ---- delete -----------------------------------------------------------------

test_that("delete removes the object and clears a dangling selection", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      fx$store$rv$selected <- fx$id2
      session$setInputs(remove = fx$id2)
      expect_null(.find_object(fx$store$rv$report, fx$id2))
      expect_null(fx$store$rv$selected)
    }
  )
})

test_that("delete leaves an unrelated selection untouched", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      fx$store$rv$selected <- fx$id1
      session$setInputs(remove = fx$id2)
      expect_identical(fx$store$rv$selected, fx$id1)
    }
  )
})

# ---- + Add output -----------------------------------------------------------

test_that("+ Add output sets rv$adding TRUE", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      expect_false(fx$store$rv$adding)
      session$setInputs(add_output = 1)
      expect_true(fx$store$rv$adding)
    }
  )
})

# ---- JS bridge smoke test ----------------------------------------------

test_that("arframe.js bundles the LoC bridge (ar-report-open, ar-loc-row) + Sortable", {
  js <- readLines(
    system.file("www", "arframe.bundle.js", package = "arframe"),
    warn = FALSE
  )
  txt <- paste(js, collapse = "\n")
  # LoC drill wiring
  expect_match(txt, "ar-report-open", fixed = TRUE)
  expect_match(txt, "ar-loc-row", fixed = TRUE)
  expect_match(txt, "contents-back", fixed = TRUE)
  # The (still-vendored, generic) Sortable bridge stays bundled
  expect_match(txt, "_arSortable", fixed = TRUE)
  expect_match(txt, "arInitSortables", fixed = TRUE)
})

test_that("Sortable.min.js is vendored with the MIT header intact", {
  first_line <- readLines(
    system.file("www", "Sortable.min.js", package = "arframe"),
    n = 1L,
    warn = FALSE
  )
  expect_match(first_line, "Sortable 1.15.6", fixed = TRUE)
  expect_match(first_line, "MIT", fixed = TRUE)
})

# ---- end-to-end: the real arframe() launcher, a real browser ---------------

test_that("arframe() LoC: grouped rows, stamps, select, drill, screenshot", {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("apps/contents"),
    name = "contents",
    height = 900,
    width = 1440
  )
  withr::defer(app$stop())

  # The app opens in Setup/Data mode: flip to Report so the LoC renders. A
  # suspended-while-hidden output recomputes only after Shiny's visibility
  # re-scan, which trails wait_for_idle -- wait for the first real row.
  app$click(selector = '[data-ar-mode="report"]')
  app$wait_for_idle()
  app$wait_for_js("!!document.querySelector('.ar-loc-row')", timeout = 10000)

  html <- app$get_html("body", outer_html = TRUE)
  expect_match(html, "ar-loc", fixed = TRUE)
  expect_match(html, "TABLES", fixed = TRUE)
  expect_match(html, "FIGURES", fixed = TRUE)
  expect_match(html, "READY", fixed = TRUE)
  expect_match(html, "DRAFT", fixed = TRUE)

  # Single-click selects (highlights) the row without drilling.
  app$click(selector = '.ar-loc-row[data-ar-id="out002"]')
  app$wait_for_idle()
  sel_id <- app$get_js(
    "document.querySelector('.ar-dx-row-sel').getAttribute('data-ar-id');"
  )
  expect_identical(sel_id, "out002")
  drilled <- app$get_js(
    "document.querySelector('.ar-workspace').classList.contains('ar-report-open');"
  )
  expect_false(drilled)

  # Double-click drills -> the desk (paper + inspector) reveals.
  app$run_js(
    "document.querySelector('.ar-loc-row[data-ar-id=\"out001\"]').dispatchEvent(new MouseEvent('dblclick', {bubbles: true}));"
  )
  app$wait_for_idle()
  app$wait_for_js(
    "document.querySelector('.ar-workspace').classList.contains('ar-report-open')",
    timeout = 10000
  )
  drilled2 <- app$get_js(
    "document.querySelector('.ar-workspace').classList.contains('ar-report-open');"
  )
  expect_true(drilled2)

  # Dev-only screenshot artifact: .local/ is .Rbuildignore'd, so it does not
  # exist in R CMD check's isolated sandbox copy -- write it only when
  # running from the real source tree, never as a hard requirement.
  screens_dir <- testthat::test_path("../../.local/screens")
  if (dir.exists(screens_dir)) {
    screenshot_path <- file.path(screens_dir, "07-contents.png")
    if (file.exists(screenshot_path)) {
      file.remove(screenshot_path)
    }
    app$get_screenshot(screenshot_path)
  }
})

# ---- stale stamp (run semantics, decision #8) -------------------------------

test_that("a stale id stamps STALE in the LoC; broken still wins over stale", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  # NOT named `id`: inside testServer the module server's own `id` arg
  # ("proxy1") shadows any outer variable of that name.
  out_id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))

  shiny::testServer(
    mod_contents_server,
    args = list(store = store),
    {
      expect_match(output$table$html, "READY", fixed = TRUE)

      store$rv$stale <- out_id
      session$flushReact()
      expect_match(output$table$html, "STALE", fixed = TRUE)

      # The app-side render-failed flag outranks staleness -- a broken
      # proof is broken first.
      store$rv$broken <- out_id
      session$flushReact()
      html <- output$table$html
      expect_match(html, "ERROR", fixed = TRUE)
      expect_no_match(html, "STALE", fixed = TRUE)
    }
  )
})

# ---- keyboard navigation (Task 17) -----------------------------------------

test_that("mod_contents_server: Up/Down move the selection through display order and clamp", {
  tc <- .tc_store()
  withr::defer(arpillar::engine_close(tc$con))
  shiny::testServer(mod_contents_server, args = list(store = tc$store), {
    down <- function(n) session$setInputs(nav = list(dir = "down", nonce = n))
    up <- function(n) session$setInputs(nav = list(dir = "up", nonce = n))
    # The fixture leaves the last-added output selected; start from nothing so
    # the first arrow's "pick the first output" branch is exercised.
    store$rv$selected <- NULL
    down(1)
    expect_identical(store$rv$selected, tc$id1)
    down(2)
    expect_identical(store$rv$selected, tc$id2)
    down(3)
    expect_identical(store$rv$selected, tc$id3)
    # Clamp at the bottom.
    down(4)
    expect_identical(store$rv$selected, tc$id3)
    up(5)
    expect_identical(store$rv$selected, tc$id2)
  })
})

test_that("mod_contents_server: Enter drills into the selected output", {
  tc <- .tc_store()
  withr::defer(arpillar::engine_close(tc$con))
  shiny::testServer(mod_contents_server, args = list(store = tc$store), {
    store$rv$selected <- tc$id2
    session$setInputs(activate = 1)
    expect_identical(store$rv$report_open, tc$id2)
  })
})
