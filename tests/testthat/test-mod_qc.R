# The QC sheet (plan Task 15): a paper-styled proof-check sheet + run log.
# It reads the SAME status oracle the TOC does (`.toc_rows()` -> output_status,
# folding rv$broken/rv$stale), lists each not-ready output's validate_output()
# gaps as jump links, and shows the run log newest-first. A jump click flips
# to Report mode, selects the output, and opens the inspector on the mapped
# region. No second status predicate lives here (self-review oracle rule).

# ---- fixture ---------------------------------------------------------------

#' A store seeded with one READY summary (table) + one DRAFT crosstab
#' (table, no roles). The ready row proves the oracle is not a constant; the
#' draft row carries the validate_output() gaps the QC jump links surface.
#' @noRd
.qc_fixture <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  ready_id <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))
  draft_id <- shiny::isolate(add_from_generator(store, "crosstab", "ADSL"))
  shiny::isolate(update_object(store, ready_id, function(o) {
    S7::set_props(
      o,
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
  }))
  list(con = con, store = store, ready_id = ready_id, draft_id = draft_id)
}

# ---- UI --------------------------------------------------------------------

test_that("mod_qc_ui HTML contains the QC container and the sheet slot", {
  html <- as.character(mod_qc_ui("qc"))
  expect_match(html, "ar-qc", fixed = TRUE)
})

# ---- sheet content ---------------------------------------------------------

test_that(".qc_sheet: running head, summary count, per-status stamps", {
  f <- .qc_fixture()
  withr::defer(arpillar::engine_close(f$con))
  report <- shiny::isolate(f$store$rv$report)
  html <- as.character(
    .qc_sheet(shiny::NS("qc"), report, character(0), character(0))
  )

  # Running head names the report (the QC sheet IS a document -- decision #7
  # allows the page chrome here, unlike the on-screen galley artifact).
  expect_match(html, "QC", fixed = TRUE)
  expect_match(html, report@name, fixed = TRUE)
  # Summary: exactly one of two outputs is ready (matches the oracle).
  expect_match(html, "1 of 2 outputs ready", fixed = TRUE)
  # Both stamps present -- the oracle is read per object, not defaulted.
  expect_match(html, "ar-stamp-ready", fixed = TRUE)
  expect_match(html, "ar-stamp-draft", fixed = TRUE)
})

test_that(".logs_sheet: running head + newest-first run log (piece A split)", {
  f <- .qc_fixture()
  withr::defer(arpillar::engine_close(f$con))
  report <- shiny::isolate(f$store$rv$report)
  log <- c("[00:00:01] older line", "[00:00:02] newer line")
  html <- as.character(.logs_sheet(report, log))

  expect_match(html, "Logs", fixed = TRUE)
  expect_match(html, report@name, fixed = TRUE)
  expect_lt(
    regexpr("newer line", html, fixed = TRUE),
    regexpr("older line", html, fixed = TRUE)
  )
  # Empty state is explicit, never a blank gap.
  expect_match(
    as.character(.logs_sheet(report, character(0))),
    "Nothing logged yet.",
    fixed = TRUE
  )
})

test_that("mod_logs_ui HTML contains the desk container and the log-sheet slot", {
  html <- as.character(mod_logs_ui("qc"))
  expect_match(html, "ar-qc", fixed = TRUE)
  expect_match(html, "qc-log_sheet", fixed = TRUE)
})

test_that(".qc_sheet: a not-ready output lists its validate_output gaps as jump links", {
  f <- .qc_fixture()
  withr::defer(arpillar::engine_close(f$con))
  report <- shiny::isolate(f$store$rv$report)
  html <- as.character(
    .qc_sheet(shiny::NS("qc"), report, character(0), character(0))
  )
  expect_match(html, "ar-qc-problems", fixed = TRUE)
  # The jump link carries the draft output's id and posts the `qc-jump` input.
  expect_match(html, f$draft_id, fixed = TRUE)
  expect_match(html, "qc-jump", fixed = TRUE)
})

test_that(".qc_sheet: a broken output shows ERROR with a render-failed jump", {
  f <- .qc_fixture()
  withr::defer(arpillar::engine_close(f$con))
  report <- shiny::isolate(f$store$rv$report)
  # Fold the ready output into rv$broken -- the QC stamp must follow the same
  # app-side flag precedence the TOC uses.
  html <- as.character(
    .qc_sheet(shiny::NS("qc"), report, f$ready_id, character(0))
  )
  expect_match(html, "ar-stamp-broken", fixed = TRUE)
  expect_match(html, "Render failed", fixed = TRUE)
})

# ---- navigation ------------------------------------------------------------

test_that("mod_qc_server: a jump flips to Report mode, selects the output, opens the region", {
  f <- .qc_fixture()
  withr::defer(arpillar::engine_close(f$con))
  store <- f$store
  shiny::testServer(mod_qc_server, args = list(store = store), {
    store$rv$mode <- "qc"
    session$setInputs(jump = list(id = f$draft_id, region = "rows"))
    expect_identical(store$rv$mode, "report")
    expect_identical(store$rv$selected, f$draft_id)
    expect_identical(store$rv$region, "rows")
    # rows -> the Roles tab (open_card -> .card_region_group).
    expect_identical(store$rv$insp_tab, "roles")
    expect_false(store$rv$insp_collapsed)
  })
})

test_that("mod_qc_server: the QC sheet output does not suspend while its mode body is hidden", {
  # The mode body toggles via a custom `ar-mode` class, not a Shiny tabset,
  # so Shiny never learns the body became visible -- the sheet output must
  # not suspend, or switching to QC shows a blank sheet (the same trap the
  # inspector panes hit; see test-mod_card_options.R).
  server_src <- paste(deparse(body(mod_qc_server)), collapse = "\n")
  expect_match(server_src, "suspendWhenHidden", fixed = TRUE)
})
