# The typeset sheet: renders the SELECTED output through the export-
# identical arpillar::render_spec()/render_ggplot() seam, or the ghost
# shell / GOV.UK error summary when it is not ready / fails. All fixtures
# here are hand-built against a deliberately narrow slice of the REAL
# demo-catalog columns (not every preset role var), so a "ready and
# renders cleanly" fixture is built directly off `.tc_ready_summary()`-
# style roles instead of added via
# `add_from_preset(store, "demographics", "ADSL")`.

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
#' group=TRT01P -- all real demo ADVS columns; hand-rolled roles rather
#' than `add_from_preset(store, "mean_over_time", "ADVS")`, the figure
#' analogue of the summary fixture above).
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
      # The auto "Source: ... arframe ..." provenance line was removed
      # 2026-07-09 (user call) -- no default footnote on a deliverable.
      expect_no_match(html, "Source:", fixed = TRUE)
      expect_identical(fx$store$rv$broken, character(0))
    }
  )
})

test_that("a READY table is tabular's page alone -- ONE title block, no arframe chrome", {
  # Canvas flip (2026-07-04, supersedes decision #7): tabular renders the
  # whole page (title block, footnotes, source); painting arframe's own
  # title/source around it would double-print both.
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      html <- output$sheet_html_slot$html
      expect_match(html, "ar-paper-table-wrap", fixed = TRUE)
      expect_no_match(html, "ar-paper-title-block", fixed = TRUE)
      expect_no_match(html, "ar-paper-source", fixed = TRUE)
      # The wrap carries the orientation attribute the page-width CSS keys
      # on (landscape default).
      expect_match(html, 'data-ar-orient="landscape"', fixed = TRUE)
      # The title text appears exactly once -- tabular's.
      expect_identical(
        lengths(regmatches(
          html,
          gregexpr(
            "Demographics and Baseline Characteristics",
            html,
            fixed = TRUE
          )
        )),
        1L
      )
    }
  )
})

test_that("the screen render seam applies report@theme, matching the .rtf export", {
  # Screen/export parity: a Setup Summaries change (add a "Geometric Mean" stat
  # row) must move BOTH the on-screen paper (`render_spec`) AND the exported
  # .rtf (`render_rtf`). Both renderers accept `theme=`; if the screen seam
  # forgets it, a study edit lands in the deliverable but not on the paper the
  # user proofs against -- a silent divergence (decimals, the Summaries
  # vocabulary, header-N all route through the same theme). "Geometric Mean" is
  # never a default stat label nor page markup, so it uniquely signals that
  # report@theme reached the renderer. Two rows so the labels render as leaves
  # (a single-stat group collapses its label into the item header).
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  th <- list(
    summaries = list(
      continuous = list(
        list(label = "Mean", stats = "mean"),
        list(label = "Geometric Mean", stats = "geomean")
      )
    )
  )
  rep0 <- shiny::isolate(fx$store$rv$report)
  shiny::isolate(commit(fx$store, S7::set_props(rep0, theme = th)))
  obj <- shiny::isolate(selected_object(fx$store))

  # Export seam -- report@theme threaded (fct_export.R).
  out_dir <- withr::local_tempdir()
  shiny::isolate(.export_render_one(fx$store, obj, out_dir))
  export_txt <- paste(
    readLines(
      list.files(out_dir, "\\.rtf$", full.names = TRUE)[[1L]],
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(export_txt, "Geometric Mean", fixed = TRUE)

  # Screen seam -- the server render the user proofs against MUST match.
  shiny::testServer(mod_paper_server, args = list(store = fx$store), {
    expect_match(output$sheet_html_slot$html, "Geometric Mean", fixed = TRUE)
  })

  # Control: the default (empty-theme) render carries no "Geometric Mean" row,
  # so the matches above uniquely attribute it to report@theme.
  out0 <- withr::local_tempdir()
  shiny::isolate(commit(fx$store, S7::set_props(rep0, theme = list())))
  obj0 <- shiny::isolate(selected_object(fx$store))
  shiny::isolate(.export_render_one(fx$store, obj0, out0))
  export0 <- paste(
    readLines(
      list.files(out0, "\\.rtf$", full.names = TRUE)[[1L]],
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_no_match(export0, "Geometric Mean", fixed = TRUE)
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

test_that("no selection renders nothing -- the LoC owns the empty state (2026-07-08)", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = store),
    {
      # The paper only shows when the LoC is drilled onto a selection; with no
      # selection it renders nothing. The old fabricated empty-report ghost /
      # desk hint is gone -- no dead markup stands in.
      html <- output$sheet_html_slot$html
      html <- if (is.null(html)) "" else html
      expect_no_match(html, "ar-desk-hint", fixed = TRUE)
      expect_no_match(html, "Right-click", fixed = TRUE)
    }
  )
})

test_that("the context menu's Add output (add_first) sets rv$adding TRUE", {
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

# ---- error message splitting -------------------------------------------

test_that(".split_error_message splits a cli-formatted message into headline + cleaned detail lines", {
  # The real shape arpillar_error_input's conditionMessage() takes: a
  # headline, then \n-joined bullet lines each prefixed with a cli glyph
  # and a space.
  msg <- "Unknown column in projection.\n✖ \"BOGUSVAR\" is not in the dataset."
  parsed <- .split_error_message(msg)
  expect_identical(parsed$headline, "Unknown column in projection.")
  expect_identical(parsed$detail, "\"BOGUSVAR\" is not in the dataset.")
})

test_that(".split_error_message strips every cli glyph variant (x/i/bullet/check)", {
  msg <- paste(
    "Headline.",
    "✖ x detail",
    "ℹ i detail",
    "• bullet detail",
    "✔ check detail",
    sep = "\n"
  )
  parsed <- .split_error_message(msg)
  expect_identical(
    parsed$detail,
    c("x detail", "i detail", "bullet detail", "check detail")
  )
})

test_that(".split_error_message on a single-line message has no detail lines", {
  parsed <- .split_error_message("A plain single-line error.")
  expect_identical(parsed$headline, "A plain single-line error.")
  expect_length(parsed$detail, 0L)
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

test_that("the bogus-column error summary renders a clean headline, not a run-on glyph line", {
  fx <- .pp_bogus_col_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      html <- output$sheet_html_slot$html
      # The headline paragraph is clean, with no trailing cli glyph/newline
      # bleeding into it.
      expect_match(
        html,
        '<p class="ar-mono">Unknown column in projection.</p>',
        fixed = TRUE
      )
      # The detail line is its own muted paragraph, with the leading glyph
      # stripped (the raw glyph never reaches the rendered HTML).
      expect_match(html, "ar-problem-detail", fixed = TRUE)
      expect_no_match(html, "✖", fixed = TRUE)
      expect_match(html, '"BOGUSVAR" is not in the dataset.', fixed = TRUE)
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

test_that("a role edit on a proofed output marks it STALE; Run re-typesets (decision #8)", {
  # SUPERSEDES the pre-stale behavior (an immediate cache-MISS rebuild):
  # a heavy edit on an already-typeset output must NOT auto re-collect
  # from DuckDB -- the paper shows the stale notice and Run re-typesets.
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
      html <- output$sheet_html_slot$html
      # The stale notice stands in for the table; no new ARD was built.
      expect_match(html, "ar-paper-stale", fixed = TRUE)
      expect_no_match(html, "tabular-doc", fixed = TRUE)
      expect_identical(sum(startsWith(ls(fx$store$cache), "ard::")), n1)

      # Run (mod_card clears rv$stale, drops the memos, bumps the nonce):
      # the paper re-typesets the new configuration fresh.
      rm(
        list = grep("^ard::", ls(fx$store$cache), value = TRUE),
        envir = fx$store$cache
      )
      fx$store$rv$stale <- character(0)
      fx$store$rv$run_nonce <- fx$store$rv$run_nonce + 1L
      session$flushReact()
      html2 <- output$sheet_html_slot$html
      expect_match(html2, "tabular-doc", fixed = TRUE)
      expect_identical(sum(startsWith(ls(fx$store$cache), "ard::")), 1L)
    }
  )
})

test_that("the stale notice keeps the full page shell: title block + source line", {
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      output$sheet_html_slot$html
      # Swap the summarize items (still READY -- both slots stay filled):
      # a heavy edit on a proofed output.
      update_object(fx$store, fx$id, function(o) {
        S7::set_props(
          o,
          roles = list(
            o@roles[[1]],
            arpillar::role(
              slot = "summarize",
              items = list(arpillar::data_item(
                name = "SEX",
                role_type = "category"
              ))
            )
          )
        )
      })
      session$flushReact()
      html <- output$sheet_html_slot$html
      expect_match(html, "ar-paper-title-block", fixed = TRUE)
      expect_match(html, "ar-paper-stale", fixed = TRUE)
      # Source line removed everywhere 2026-07-09 (user call), incl. the
      # stale path.
      expect_no_match(html, "ar-paper-source", fixed = TRUE)
    }
  )
})

# ---- galley artifact (v5, decision #7) -------------------------------------

test_that("the paper UI has NO fit/page toolbar -- the artifact is content-hugging (v5)", {
  ui <- mod_paper_ui("paper")
  html <- as.character(ui)
  expect_no_match(html, "Preview width", fixed = TRUE)
  expect_no_match(html, "ar-paper-toolbar", fixed = TRUE)
  expect_no_match(html, "fit_btn", fixed = TRUE)
  expect_no_match(html, "ar-paper--fit", fixed = TRUE)
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
  # v5 (decision #8): the code-view surface mounts alongside the sheet in
  # the desk column; a desk class picks which shows.
  expect_match(html, 'id="paper-code_slot"', fixed = TRUE)
  # The canvas toolbar (2026-07-04) mounts as the desk's first child; the
  # Preact component renders into its data-ar-toolbar div.
  expect_match(html, 'data-ar-toolbar="paper-toolbar"', fixed = TRUE)
})

# ---- code view (v5, decision #8) -------------------------------------------

test_that("mod_paper code view renders the emit_code script when rv$code_view is TRUE", {
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_paper_server, args = list(store = fx$store), {
    shiny::isolate(
      fx$store$rv$selected <- .all_objects(fx$store$rv$report)[[1]]@id
    )
    store$rv$code_view <- TRUE
    session$flushReact()
    html <- output$code_slot$html
    # The reproduction script: library, the assign, the emit, the filename
    # bar, and the three actions.
    # Tokens are wrapped in ar-hl-* highlight spans (2026-07-04), so
    # match the spanned forms; the pre's textContent stays byte-identical.
    expect_match(
      html,
      '<span class="ar-hl-kw">library</span>(arpillar)',
      fixed = TRUE
    )
    expect_match(
      html,
      '<span class="ar-hl-fn">engine_open</span>()',
      fixed = TRUE
    )
    expect_match(html, "14.1.1", fixed = TRUE)
    expect_match(html, "-demographics", fixed = TRUE)
    # Ids are namespaced by testServer's own proxy, so match the suffix.
    expect_match(html, "data-ar-copy", fixed = TRUE)
    expect_match(html, "code_dl", fixed = TRUE)
    expect_match(html, "code_close", fixed = TRUE)
  })
})

test_that("mod_paper: code_close returns to the artifact", {
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_paper_server, args = list(store = fx$store), {
    store$rv$code_view <- TRUE
    session$setInputs(code_close = 1)
    expect_false(store$rv$code_view)
  })
})

test_that("mod_paper: the code download writes a parse()-clean .R", {
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_paper_server, args = list(store = fx$store), {
    shiny::isolate(
      fx$store$rv$selected <- .all_objects(fx$store$rv$report)[[1]]@id
    )
    path <- output$code_dl
    expect_match(basename(path), "\\.R$")
    expect_silent(parse(path))
  })
})

# ---- JS bridge smoke test ----------------------------------------------

test_that("the JS bundle: the read-only canvas carries no region-click machinery", {
  js <- readLines(
    system.file("www", "arframe.bundle.js", package = "arframe"),
    warn = FALSE
  )
  txt <- paste(js, collapse = "\n")
  # The canvas is a read-only tabular preview -- no region-click delegation,
  # no drag-guard queue, no margin-mark annotation of tabular's structure.
  expect_no_match(txt, "arRegionClick", fixed = TRUE)
  expect_no_match(txt, "arDeferredRegionClicks", fixed = TRUE)
  expect_no_match(txt, "data-ar-region", fixed = TRUE)
  expect_no_match(txt, "tabular-table thead", fixed = TRUE)
  expect_no_match(txt, "ar-paper-width", fixed = TRUE)
  # What stays: the table/figure class flip, plus the code-view desk-swap and
  # the clipboard Copy handler.
  expect_match(txt, "ar-paper-kind", fixed = TRUE)
  expect_match(txt, "ar-code-view", fixed = TRUE)
  expect_match(txt, "data-ar-copy", fixed = TRUE)
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
  # dependent content. shinytest2 can declare a SECOND consecutive
  # AppDriver "ready" before its first renderUI paints (the chromote
  # tracer reports "Already connected" and skips the value wait), so
  # wait for the row itself, not just idle.
  app$wait_for_js('document.querySelector(".ar-loc-row") !== null')
  app$click(selector = '.ar-loc-row[data-ar-id="out001"]')
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

# The former galley region tests -- ghost-slot keyboard activation and
# tabular-structure region annotation -- are gone with the read-only canvas
# redesign: nothing on the canvas is a click/keyboard target anymore.

test_that("arframe() paper: the canvas renders the running-header bands with tokens resolved (supersedes #7 suppression)", {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("apps/paper"),
    name = "paper-galley",
    height = 900,
    width = 1440
  )
  withr::defer(app$stop())

  # Second consecutive AppDriver: the first render can land AFTER
  # shinytest2's ready gate (see the sibling test above) -- wait for the
  # row before clicking.
  app$wait_for_js('document.querySelector(".ar-loc-row") !== null')
  app$click(selector = '.ar-loc-row[data-ar-id="out001"]')
  app$wait_for_idle()

  # The canvas is a read-only tabular preview -- no clickable region hooks.
  sheet_html <- app$get_html("#paper-sheet", outer_html = TRUE)
  expect_no_match(sheet_html, "data-ar-region", fixed = TRUE)
  # The running header/footer bands now render on the canvas (2026-07-08,
  # supersedes decision #7's on-screen suppression): study tokens resolve to
  # the Setup > Study values; {page}/{npages} are tabular's own field codes.
  expect_match(sheet_html, "Demo Sponsor - XYZ-2026", fixed = TRUE)
  expect_match(sheet_html, "Data as of 2026-07-08", fixed = TRUE)
})

# ---- population/filters tag (Task 12) ---------------------------------------

test_that("a filtered output shows the Population tag routed to the filters region", {
  fx <- .pp_ready_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_paper_server,
    args = list(store = fx$store),
    {
      # No filters -> no tag.
      expect_no_match(
        output$sheet_html_slot$html,
        "ar-paper-filtertag",
        fixed = TRUE
      )

      update_object(fx$store, fx$id, function(o) {
        S7::set_props(
          o,
          filters = list(list(column = "SAFFL", op = "==", value = "Y"))
        )
      })
      session$flushReact()
      html <- output$sheet_html_slot$html
      # The tag names the population preset (read-only -- no click region).
      expect_match(html, "ar-paper-filtertag", fixed = TRUE)
      expect_match(html, "Population: Safety population", fixed = TRUE)
    }
  )
})

test_that(".filters_tag_label names the safety preset, else counts filters", {
  safety <- list(list(column = "SAFFL", op = "==", value = "Y"))
  expect_identical(.filters_tag_label(safety), "Safety population")
  expect_identical(
    .filters_tag_label(list(
      list(column = "SEX", op = "%in%", value = "F"),
      list(column = "AGE", op = ">=", value = 65)
    )),
    "2 filters"
  )
  expect_identical(
    .filters_tag_label(list(list(column = "SEX", op = "is.na"))),
    "1 filter"
  )
})

test_that("a malformed region payload is dropped, never a session error", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(store$rv$selected <- id)

  shiny::testServer(mod_paper_server, args = list(store = store), {
    # The real contract is a plain string; an object payload (as any stray
    # client-side script could post) must be ignored, not crash open_card.
    expect_no_error(
      session$setInputs(region = list(region = "title", nonce = 1))
    )
    expect_null(shiny::isolate(store$rv$region))

    session$setInputs(region = "title")
    expect_identical(shiny::isolate(store$rv$region), "title")
  })
})

# ---- .with_chrome: render-time token stamping -------------------------------

test_that(".with_chrome stamps {datetime}/{program} to literals, keeps {page}", {
  obj <- arpillar::object(
    id = "t9",
    type = "summary",
    dataset = "ADSL",
    title = "Demo",
    options = list(
      pagehead = list(
        left = "Protocol: XY123",
        right = "Page {page} of {npages}"
      ),
      pagefoot = list(left = "Program: {program}", right = "{datetime}")
    )
  )
  now <- as.POSIXct("2026-07-04 09:05:07", tz = "UTC")
  out <- .with_chrome(obj, now = now)
  # The engine-phase tokens are gone -- stamped as literals.
  expect_identical(out@options$pagefoot$right, "04JUL2026:09:05:07")
  expect_match(out@options$pagefoot$left, "^Program: programs/.+\\.R$")
  # The backend-phase field codes pass through untouched.
  expect_identical(out@options$pagehead$right, "Page {page} of {npages}")
  expect_identical(out@options$pagehead$left, "Protocol: XY123")
})

test_that(".with_chrome is a no-op without bands and never double-stamps", {
  obj <- arpillar::object(id = "t9", type = "summary", dataset = "ADSL")
  expect_identical(.with_chrome(obj), obj)

  # A band with no tokens comes through verbatim (idempotent on literals).
  lit <- arpillar::object(
    id = "t9",
    type = "summary",
    dataset = "ADSL",
    options = list(pagefoot = list(right = "04JUL2026:00:00:00"))
  )
  expect_identical(
    .with_chrome(lit)@options$pagefoot$right,
    "04JUL2026:00:00:00"
  )
})

# ---- .with_band_chrome: study band resolution (screen + .rtf/export) --------

test_that(".with_band_chrome stamps study tokens + {datetime}, keeps {page}", {
  obj <- arpillar::object(id = "t1", type = "summary", dataset = "ADSL")
  now <- as.POSIXct("2026-12-31 23:59:09", tz = "UTC")
  theme <- list(
    study = list(
      protocol = "XY-123",
      sponsor = "Acme",
      data_date = "2026-01-01"
    ),
    page = list(
      pagehead = list(left = "{protocol}", right = "Page {page} of {npages}"),
      pagefoot = list(
        left = "Data as of {data_date}",
        center = "{sponsor}",
        right = "{datetime}"
      )
    )
  )
  out <- .with_band_chrome(theme, obj, now = now)
  # Study tokens -> Setup > Study values; bands KEPT (not stripped).
  expect_identical(out$page$pagehead$left, "XY-123")
  expect_identical(out$page$pagefoot$left, "Data as of 2026-01-01")
  expect_identical(out$page$pagefoot$center, "Acme")
  # {datetime} -- the token arpillar rejects as non-deterministic -- is stamped
  # to a literal so `render_rtf()` does not abort (#7).
  expect_identical(out$page$pagefoot$right, .chrome_stamp(now))
  expect_no_match(out$page$pagefoot$right, "{datetime}", fixed = TRUE)
  # {page}/{npages} are tabular's own field codes -> left untouched (the .rtf
  # is the paginated truth; the canvas is one continuous sheet).
  expect_identical(out$page$pagehead$right, "Page {page} of {npages}")
})

test_that(".with_band_chrome errors on a required band token Setup leaves empty", {
  obj <- arpillar::object(id = "t1", type = "summary", dataset = "ADSL")
  theme <- list(
    study = list(protocol = ""),
    page = list(pagehead = list(left = "Protocol: {protocol}"))
  )
  expect_error(.with_band_chrome(theme, obj), class = "arframe_error_input")
})

test_that(".with_band_chrome leaves an OPTIONAL empty token blank, no error", {
  obj <- arpillar::object(id = "t1", type = "summary", dataset = "ADSL")
  theme <- list(
    study = list(),
    page = list(pagefoot = list(left = "{indication}"))
  )
  out <- .with_band_chrome(theme, obj)
  expect_identical(out$page$pagefoot$left, "")
})

test_that(".chrome_stamp is locale-independent ddMMMyyyy:hh:mm:ss", {
  now <- as.POSIXct("2026-12-31 23:59:09", tz = "UTC")
  expect_identical(.chrome_stamp(now), "31DEC2026:23:59:09")
})

test_that("the export report leg stamps every output's chrome tokens", {
  # .report_for_export() composes .with_footnotes() + .with_chrome() with ONE
  # clock for the package, so no raw token ever reaches arpillar's emit.
  obj <- arpillar::object(
    id = "t9",
    type = "summary",
    dataset = "ADSL",
    options = list(pagefoot = list(right = "{datetime}"))
  )
  rep <- arpillar::report(
    id = "r9",
    name = "R",
    pages = list(arpillar::page(id = "p1", name = "P", objects = list(obj)))
  )
  out <- .report_for_export(rep)
  o <- out@pages[[1]]@objects[[1]]
  expect_no_match(o@options$pagefoot$right, "{datetime}", fixed = TRUE)
  expect_match(o@options$pagefoot$right, "^[0-9]{2}[A-Z]{3}[0-9]{4}:")
  # No auto source line is stamped anymore (removed 2026-07-09).
  expect_null(o@options$source)
})

test_that("the export report leg stamps the STUDY running bands too (#7)", {
  # The .rtf/export legs read the STUDY bands (theme$page), not just per-output
  # overrides. A live {datetime} there aborts arpillar's byte-deterministic
  # emit -> render_rtf throws -> Shiny serves the error as HTML. .report_for_
  # export() must stamp theme$page, not only object@options bands.
  obj <- arpillar::object(id = "t7", type = "summary", dataset = "ADSL")
  rep <- arpillar::report(
    id = "r7",
    name = "R",
    pages = list(arpillar::page(id = "p1", name = "P", objects = list(obj))),
    theme = list(
      study = list(protocol = "XY-7", data_date = "2026-01-01"),
      page = list(
        pagefoot = list(
          left = "Data as of {data_date}",
          right = "{datetime}"
        )
      )
    )
  )
  out <- .report_for_export(rep)
  pf <- out@theme$page$pagefoot
  # {datetime} stamped to a literal; the study token resolved.
  expect_no_match(pf$right, "{datetime}", fixed = TRUE)
  expect_match(pf$right, "^[0-9]{2}[A-Z]{3}[0-9]{4}:")
  expect_identical(pf$left, "Data as of 2026-01-01")
})
