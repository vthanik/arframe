# The Contents column: the TOC that IS the output switcher. Groups outputs
# TABLES/FIGURES/LISTINGS (kind from the type->generator map, empty groups
# omitted), numbers them by preset-seeded/auto-suggested options$number
# (falling back to the old kind-scoped 14.1.n / 14.2.n / 16.2.n index), and
# drives reorder/rename/duplicate/remove/select entirely through the
# injected store.

# ---- fixtures --------------------------------------------------------------

#' A "ready" summary object: treatment + one measure to summarize, so
#' output_status() reports "ready" (all TOC-stamp tests need at least one
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

# ---- UI ---------------------------------------------------------------

test_that("mod_contents_ui HTML contains the CONTENTS label and the Add-output action", {
  ui <- mod_contents_ui("contents")
  html <- as.character(ui)
  expect_match(html, "CONTENTS", fixed = TRUE)
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
      html <- output$toc$html
      expect_match(html, "TABLES", fixed = TRUE)
      expect_match(html, "FIGURES", fixed = TRUE)
      # LISTINGS never appears -- no listing generator exists, so the group
      # is always empty and must be omitted entirely.
      expect_no_match(html, "LISTINGS", fixed = TRUE)

      # id1/id2/id3 each carry an add_from_generator()-auto-suggested
      # options$number, which .toc_rows() prefers over the fallback index --
      # id1/id2 are both "table" kind (1-based within kind, in document
      # order); id3 is the lone "figure" kind entry.
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
      html <- output$toc$html
      expect_match(html, "14.1.1", fixed = TRUE)
    }
  )
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
      html <- output$toc$html
      expect_match(html, "14.1.2", fixed = TRUE)
      expect_no_match(html, "14.1.1", fixed = TRUE)
    }
  )
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
      html <- output$toc$html
      expect_match(html, "READY", fixed = TRUE)
      expect_match(html, "DRAFT", fixed = TRUE)

      # Mark the READY object broken: the stamp must flip to ERROR even
      # though output_status() would still report "ready" for it.
      fx$store$rv$broken <- fx$id1
      session$flushReact()
      html2 <- output$toc$html
      expect_match(html2, "ERROR", fixed = TRUE)
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

# ---- reorder --------------------------------------------------------------

test_that("input$reorder applies the posted order via move_output", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      # document order starts id1, id2, id3 -- post the reverse.
      session$setInputs(
        reorder = list(
          order = list(fx$id3, fx$id2, fx$id1),
          nonce = 1
        )
      )
      ids <- vapply(
        .all_objects(fx$store$rv$report),
        function(o) o@id,
        character(1)
      )
      expect_identical(ids, c(fx$id3, fx$id2, fx$id1))
    }
  )
})

test_that("input$reorder drops a stale id not present in .all_objects", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      session$setInputs(
        reorder = list(
          order = list(fx$id3, "ghost-id", fx$id2, fx$id1),
          nonce = 1
        )
      )
      ids <- vapply(
        .all_objects(fx$store$rv$report),
        function(o) o@id,
        character(1)
      )
      expect_identical(ids, c(fx$id3, fx$id2, fx$id1))
      expect_false("ghost-id" %in% ids)
    }
  )
})

test_that("input$reorder appends an id missing from the posted order at the end", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      # id1 is omitted from the posted order entirely.
      session$setInputs(
        reorder = list(
          order = list(fx$id3, fx$id2),
          nonce = 1
        )
      )
      ids <- vapply(
        .all_objects(fx$store$rv$report),
        function(o) o@id,
        character(1)
      )
      expect_identical(ids, c(fx$id3, fx$id2, fx$id1))
    }
  )
})

# ---- rename ---------------------------------------------------------------

test_that("rename commits the new title", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      session$setInputs(rename = list(id = fx$id2, title = "Renamed table"))
      expect_identical(
        .find_object(fx$store$rv$report, fx$id2)@title,
        "Renamed table"
      )
    }
  )
})

test_that("rename to a blank title is a no-op", {
  fx <- .tc_store()
  withr::defer(arpillar::engine_close(fx$con))
  before <- .find_object(shiny::isolate(fx$store$rv$report), fx$id2)@title

  shiny::testServer(
    mod_contents_server,
    args = list(store = fx$store),
    {
      session$setInputs(rename = list(id = fx$id2, title = "   "))
      expect_identical(
        .find_object(fx$store$rv$report, fx$id2)@title,
        before
      )
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

test_that("arframe.js contains the Sortable bridge (_arSortable, shiny:value)", {
  js <- readLines(
    system.file("www", "arframe.js", package = "arframe"),
    warn = FALSE
  )
  txt <- paste(js, collapse = "\n")
  expect_match(txt, "_arSortable", fixed = TRUE)
  expect_match(txt, "shiny:value", fixed = TRUE)
  expect_match(txt, "arInitSortables", fixed = TRUE)
})

test_that("TOC rows restyle the stamp as dot + word (mockup piece C)", {
  css <- readLines(
    system.file("www", "arframe.css", package = "arframe"),
    warn = FALSE
  )
  txt <- paste(css, collapse = "\n")
  # The scoped override keeps the word and adds a currentColor dot; the
  # letterpress border is dropped only inside `.ar-toc-row`.
  expect_match(txt, ".ar-toc-row .ar-stamp::before", fixed = TRUE)
  expect_match(
    txt,
    ".ar-toc-row .ar-stamp {\n  display: inline-flex",
    fixed = TRUE
  )
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

test_that("arframe() Contents: grouped rows, stamps, select, screenshot", {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("apps/contents"),
    name = "contents",
    height = 900,
    width = 1440
  )
  withr::defer(app$stop())

  html <- app$get_html("body", outer_html = TRUE)
  expect_match(html, "ar-toc", fixed = TRUE)
  expect_match(html, "TABLES", fixed = TRUE)
  expect_match(html, "FIGURES", fixed = TRUE)
  expect_match(html, "READY", fixed = TRUE)
  expect_match(html, "DRAFT", fixed = TRUE)

  app$click(selector = '.ar-toc-row[data-ar-id="out002"]')
  app$wait_for_idle()
  active_id <- app$get_js(
    "document.querySelector('.ar-toc-row-active').getAttribute('data-ar-id');"
  )
  expect_identical(active_id, "out002")

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

test_that("a stale id stamps STALE in the TOC; broken still wins over stale", {
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
      expect_match(output$toc$html, "READY", fixed = TRUE)

      store$rv$stale <- out_id
      session$flushReact()
      expect_match(output$toc$html, "STALE", fixed = TRUE)

      # The app-side render-failed flag outranks staleness -- a broken
      # proof is broken first.
      store$rv$broken <- out_id
      session$flushReact()
      html <- output$toc$html
      expect_match(html, "ERROR", fixed = TRUE)
      expect_no_match(html, "STALE", fixed = TRUE)
    }
  )
})

# ---- keyboard navigation (Task 17) -----------------------------------------

test_that("mod_contents_server: Up/Down move the TOC selection and clamp at the ends", {
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

test_that("mod_contents_server: Enter opens the inspector on the selected output's first gap", {
  tc <- .tc_store()
  withr::defer(arpillar::engine_close(tc$con))
  shiny::testServer(mod_contents_server, args = list(store = tc$store), {
    store$rv$selected <- tc$id2 # the draft crosstab, roles unfilled
    session$setInputs(activate = 1)
    expect_true(store$rv$card)
    expect_false(store$rv$insp_collapsed)
    expect_true(store$rv$insp_tab %in% c("roles", "options", "filters"))
  })
})
