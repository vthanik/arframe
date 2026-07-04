# The Add-output overlay (design spec #4): recommendations from
# detect_structure(), a searchable domain-grouped preset library, a bare
# generator path, and the dataset picker's best-match default + live
# missing-vars warning. Every Add path ends at add_from_preset()/
# add_from_generator() (fct_store.R) and clears rv$adding itself.

# .recommendations() reads store$rv$catalog_nonce (a reactiveValues field),
# so any direct (non-testServer) call needs a reactive consumer -- the same
# fix test-fct_store.R uses for its own isolate()-only tests.
shiny::reactiveConsole(TRUE)
withr::defer(shiny::reactiveConsole(FALSE), teardown_env())

# ---- fixtures --------------------------------------------------------------

#' A store over the bundled demo catalog (ADSL/ADVS/ADTTE/ADAE).
#' @noRd
.ao_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  list(con = con, store = store)
}

# ---- recommendations -------------------------------------------------------

test_that(".rec_preset_ids: subject -> demographics, bds -> mean_over_time/box_by_visit", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  expect_setequal(.rec_preset_ids(fx$con, "ADSL"), "demographics")
  expect_setequal(
    .rec_preset_ids(fx$con, "ADVS"),
    c("mean_over_time", "box_by_visit")
  )
})

test_that(".rec_preset_ids: occurrence -> ae_overall/ae_soc_pt", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  expect_setequal(.rec_preset_ids(fx$con, "ADAE"), c("ae_overall", "ae_soc_pt"))
})

test_that(".rec_preset_ids: a CNSR column adds km_os, independent of the structure rule", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  # ADTTE has no PARAMCD/*TERM|*DECOD, so detect_structure() falls back to
  # "subject" via the bare USUBJID rule -- it must recommend BOTH
  # demographics (structure) AND km_os (the CNSR column), not just one.
  expect_identical(arpillar::detect_structure(fx$con, "ADTTE"), "subject")
  ids <- .rec_preset_ids(fx$con, "ADTTE")
  expect_true("demographics" %in% ids)
  expect_true("km_os" %in% ids)
})

test_that(".recommendations: one row per (dataset, preset) pair that will actually render", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  # box_by_visit/ae_overall/ae_soc_pt/km_os/demographics/mean_over_time are
  # all fully covered by their recommended dataset on the enriched demo
  # catalog (RACE on ADSL, CHG on ADVS) -- these are the surviving rows.
  recs <- .recommendations(fx$store)
  pairs <- vapply(recs, function(r) paste(r$preset_id, r$dataset), character(1))
  expect_true("box_by_visit ADVS" %in% pairs)
  expect_true("ae_overall ADAE" %in% pairs)
  expect_true("ae_soc_pt ADAE" %in% pairs)
  expect_true("km_os ADTTE" %in% pairs)
  expect_true("demographics ADSL" %in% pairs)
  expect_true("mean_over_time ADVS" %in% pairs)
})

test_that(".recommendations: label is '<preset label> \u2014 from <DATASET>'", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  recs <- .recommendations(fx$store)
  hit <- Filter(
    function(r) {
      identical(r$preset_id, "ae_overall") && identical(r$dataset, "ADAE")
    },
    recs
  )
  expect_length(hit, 1L)
  expect_identical(
    hit[[1]]$label,
    "Overall Summary of Adverse Events \u2014 from ADAE"
  )
})

test_that(".recommendations drops every candidate missing a role var -- only fully-covered pairs survive", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  # Reproduce the drop set from first principles against every candidate
  # .rec_preset_ids() would emit, independent of .recommendations()'s own
  # filtering logic -- proves the invariant, not just today's fixture
  # values.
  grid <- arpillar::catalog_grid(fx$con)
  candidates <- list()
  for (dataset in grid$name) {
    for (preset_id in .rec_preset_ids(fx$con, dataset)) {
      candidates[[length(candidates) + 1L]] <- list(
        preset_id = preset_id,
        dataset = dataset
      )
    }
  }
  expect_true(length(candidates) > 0L)

  recs <- .recommendations(fx$store)
  rec_pairs <- vapply(
    recs,
    function(r) paste(r$preset_id, r$dataset),
    character(1)
  )

  for (cand in candidates) {
    pr <- arpillar::preset(cand$preset_id)
    missing <- .missing_vars(fx$con, pr, cand$dataset)
    pair <- paste(cand$preset_id, cand$dataset)
    if (length(missing) > 0L) {
      expect_false(
        pair %in% rec_pairs,
        info = sprintf(
          "%s should be dropped (missing %s)",
          pair,
          toString(missing)
        )
      )
    } else {
      expect_true(
        pair %in% rec_pairs,
        info = sprintf("%s is fully covered and should be recommended", pair)
      )
    }
  }

  # On the demo catalog specifically: demographics-from-ADTTE (ADTTE has
  # none of AGE/SEX/RACE) is dropped, but demographics-from-ADSL and
  # mean_over_time-from-ADVS are both fully covered (RACE/CHG were added to
  # the demo catalog) and survive.
  expect_false("demographics ADTTE" %in% rec_pairs)
  expect_true("demographics ADSL" %in% rec_pairs)
  expect_true("mean_over_time ADVS" %in% rec_pairs)
})

test_that(".recommendations is memoized under a 'rec::' key on catalog_nonce", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  recs1 <- .recommendations(fx$store)
  key <- paste0("rec::", shiny::isolate(fx$store$rv$catalog_nonce))
  expect_true(exists(key, envir = fx$store$cache, inherits = FALSE))
  # A second call under the same nonce returns the identical cached list
  # (not merely an equal one) -- proves the cache hit path, not a re-derive.
  recs2 <- .recommendations(fx$store)
  expect_identical(recs1, recs2)
})

# ---- preset library ---------------------------------------------------

test_that(".library_rows lists every preset with domain/kind/generator_label", {
  rows <- .library_rows()
  expect_length(rows, length(arpillar::presets()))
  ids <- vapply(rows, `[[`, "", "id")
  expect_true("demographics" %in% ids)
  expect_true("km_os" %in% ids)

  km_row <- rows[[which(ids == "km_os")]]
  expect_identical(km_row$domain, "Efficacy")
  expect_identical(km_row$kind, "figure")
  expect_identical(km_row$generator_label, "Kaplan-Meier")
})

test_that(".library_groups orders domains Safety/Efficacy/PK/General and omits empty ones", {
  rows <- .library_rows()
  groups <- .library_groups(rows)
  domains <- vapply(groups, `[[`, "", "domain")
  expect_identical(domains, c("Safety", "Efficacy", "PK", "General"))

  # A search that matches nothing in PK/General drops those groups entirely.
  g2 <- .library_groups(rows, "demographics")
  expect_identical(vapply(g2, `[[`, "", "domain"), "Safety")
})

test_that(".library_groups filters by a case-insensitive label substring", {
  rows <- .library_rows()
  g <- .library_groups(rows, "kaplan")
  all_rows <- unlist(lapply(g, `[[`, "rows"), recursive = FALSE)
  expect_length(all_rows, 2L)
  expect_setequal(vapply(all_rows, `[[`, "", "id"), c("km_os", "km_pfs"))

  # Case-insensitive: same result upper/lower/mixed case.
  g_upper <- .library_groups(rows, "KAPLAN")
  expect_identical(
    vapply(g_upper[[1]]$rows, `[[`, "", "id"),
    vapply(g[[1]]$rows, `[[`, "", "id")
  )
})

test_that(".library_groups with a blank/whitespace search returns everything", {
  rows <- .library_rows()
  g_blank <- .library_groups(rows, "")
  g_ws <- .library_groups(rows, "   ")
  g_none <- .library_groups(rows)
  n_blank <- sum(vapply(g_blank, function(x) length(x$rows), integer(1)))
  n_ws <- sum(vapply(g_ws, function(x) length(x$rows), integer(1)))
  n_none <- sum(vapply(g_none, function(x) length(x$rows), integer(1)))
  expect_identical(n_blank, n_none)
  expect_identical(n_ws, n_none)
})

# ---- dataset matching -------------------------------------------------

test_that(".best_dataset picks the catalog dataset covering the most preset role vars", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  expect_identical(
    .best_dataset(fx$con, arpillar::preset("demographics")),
    "ADSL"
  )
  expect_identical(
    .best_dataset(fx$con, arpillar::preset("ae_overall")),
    "ADAE"
  )
  expect_identical(.best_dataset(fx$con, arpillar::preset("km_os")), "ADTTE")
})

test_that(".best_dataset returns NULL when no catalog dataset covers any role var", {
  con <- arpillar::engine_open()
  withr::defer(arpillar::engine_close(con))
  df <- data.frame(ZZZZ = 1:3)
  pq <- tempfile(fileext = ".parquet")
  artoo::write_parquet(df, pq)
  arpillar::register_dataset(con, "NOTHING", pq)

  expect_null(.best_dataset(con, arpillar::preset("demographics")))
})

test_that(".missing_vars reports absent role vars; empty when all present", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  # km_os shares AVAL/TRT01P with ADVS but ADVS carries no CNSR -- a
  # genuine single-var partial miss on an otherwise-overlapping dataset.
  km_pr <- arpillar::preset("km_os")
  expect_identical(.missing_vars(fx$con, km_pr, "ADVS"), "CNSR")

  pr <- arpillar::preset("demographics")
  # The demo ADSL now has AGE/SEX/RACE -- demographics is fully covered.
  expect_identical(.missing_vars(fx$con, pr, "ADSL"), character(0))
  # ADVS has none of AGE/SEX/RACE/TRT01P... wait TRT01P is shared; only the
  # summarize vars are checked here too since .preset_vars() flattens ALL
  # slots -- ADVS is missing AGE/SEX/RACE (all three summarize vars).
  expect_setequal(.missing_vars(fx$con, pr, "ADVS"), c("AGE", "SEX", "RACE"))
})

test_that(".missing_vars is empty for a NULL or blank dataset (no picker selection yet)", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  pr <- arpillar::preset("demographics")
  expect_identical(.missing_vars(fx$con, pr, NULL), character(0))
  expect_identical(.missing_vars(fx$con, pr, ""), character(0))
})

# ---- UI ---------------------------------------------------------------

test_that("mod_add_output_ui HTML is an empty uiOutput slot", {
  ui <- mod_add_output_ui("add_output")
  html <- as.character(ui)
  expect_match(html, "ar-add-overlay-slot", fixed = TRUE)
  expect_match(html, 'id="add_output-overlay"', fixed = TRUE)
})

# ---- server: open/close -------------------------------------------------

test_that("the overlay renders NULL while rv$adding is FALSE", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      expect_false(fx$store$rv$adding)
      expect_null(output$overlay$html)
    }
  )
})

test_that("the overlay renders the dialog once rv$adding is TRUE", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      html <- output$overlay$html
      expect_match(html, "ADD OUTPUT", fixed = TRUE)
      expect_match(html, "Recommended for your data", fixed = TRUE)
      expect_match(html, "Preset library", fixed = TRUE)
      expect_match(html, "Start from a generator", fixed = TRUE)
    }
  )
})

test_that("the close button (X) sets rv$adding FALSE", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(close = 1)
      expect_false(fx$store$rv$adding)
    }
  )
})

test_that("a backdrop click (input$dismiss) sets rv$adding FALSE", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(dismiss = 1)
      expect_false(fx$store$rv$adding)
    }
  )
})

# ---- server: recommendations -------------------------------------------

test_that("recommendations include only fully-covered pairs; Demographics-from-ADTTE is never shown", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      html <- output$overlay$html
      expect_match(
        html,
        "Overall Summary of Adverse Events \u2014 from ADAE",
        fixed = TRUE
      )
      expect_match(
        html,
        "Adverse Events by System Organ Class and Preferred Term \u2014 from ADAE",
        fixed = TRUE
      )
      expect_match(
        html,
        "Kaplan-Meier: Overall Survival \u2014 from ADTTE",
        fixed = TRUE
      )
      expect_match(
        html,
        "Distribution of Response by Visit \u2014 from ADVS",
        fixed = TRUE
      )

      # The var-coverage invariant: nothing recommended is missing a role
      # var on its recommended dataset. ADTTE has none of AGE/SEX/RACE, so
      # demographics-from-ADTTE is dropped; the enriched ADSL/ADVS now fully
      # cover demographics/mean_over_time, so those two are recommended.
      expect_no_match(
        html,
        "Demographics and Baseline Characteristics \u2014 from ADTTE",
        fixed = TRUE
      )
      expect_match(
        html,
        "Demographics and Baseline Characteristics \u2014 from ADSL",
        fixed = TRUE
      )
      expect_match(
        html,
        "Mean Change from Baseline Over Time \u2014 from ADVS",
        fixed = TRUE
      )
    }
  )
})

test_that("clicking a recommendation adds the right preset bound to the right dataset and closes", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(
        rec_add = list(preset_id = "ae_soc_pt", dataset = "ADAE", nonce = 1)
      )
      obj <- selected_object(fx$store)
      expect_identical(obj@type, "occurrence")
      expect_identical(obj@dataset, "ADAE")
      expect_identical(obj@options$number, "14.3.2")
      expect_false(fx$store$rv$adding)
    }
  )
})

test_that("a recommendation-added occurrence object has population set", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(
        rec_add = list(preset_id = "ae_overall", dataset = "ADAE", nonce = 1)
      )
      obj <- selected_object(fx$store)
      expect_identical(obj@options$population, "ADSL")
    }
  )
})

# ---- server: preset library pick + dataset default + warning ------------

test_that("selecting ae_overall in the library defaults the dataset picker to ADAE", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(pick_preset = "ae_overall")
      session$flushReact()
      html <- output$overlay$html
      expect_match(html, 'value="ADAE" selected', fixed = TRUE)
    }
  )
})

test_that("picking a preset + a dataset missing its vars shows the warning but still adds", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(pick_preset = "demographics")
      session$flushReact()
      # Override away from the auto-suggested ADSL to ADVS, which has none
      # of AGE/SEX/RACE.
      session$setInputs(picker_dataset = "ADVS")
      session$flushReact()
      warn_html <- output$picker_warning$html
      expect_match(warn_html, "ar-add-warn", fixed = TRUE)
      expect_match(warn_html, "ADVS is missing", fixed = TRUE)
      expect_match(warn_html, "AGE", fixed = TRUE)
      expect_match(warn_html, "SEX", fixed = TRUE)
      expect_match(warn_html, "RACE", fixed = TRUE)

      # Add anyway -- fail-loud, the user's call, never blocked.
      session$setInputs(add_preset = 1)
      obj <- selected_object(fx$store)
      expect_identical(obj@dataset, "ADVS")
      expect_identical(obj@type, "summary")
      expect_false(fx$store$rv$adding)
    }
  )
})

test_that("the missing-vars warning is absent when the picked dataset covers every role var", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(pick_preset = "ae_overall")
      session$flushReact()
      session$setInputs(picker_dataset = "ADAE")
      session$flushReact()
      expect_null(output$picker_warning$html)
    }
  )
})

test_that("the warning updates live when the user overrides the dataset picker (not stuck on the default)", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      # response_summary's only role var (AVALC) is absent from every demo
      # dataset -- both the auto-suggested default (ADSL) and an override
      # (ADVS) stay genuinely partial, unlike demographics/mean_over_time
      # which the enriched catalog now fully covers.
      session$setInputs(pick_preset = "response_summary")
      session$flushReact()

      # First: the auto-suggested default (ADSL, missing AVALC).
      session$setInputs(picker_dataset = "ADSL")
      session$flushReact()
      warn1 <- output$picker_warning$html
      expect_match(warn1, "ADSL is missing AVALC", fixed = TRUE)

      # Then: override to ADVS -- the warning must re-derive against ADVS,
      # not stay pinned to the stale ADSL message.
      session$setInputs(picker_dataset = "ADVS")
      session$flushReact()
      warn2 <- output$picker_warning$html
      expect_match(warn2, "ADVS is missing", fixed = TRUE)
      expect_no_match(warn2, "ADSL is missing", fixed = TRUE)
    }
  )
})

test_that("switching to a different preset recomputes the default, not carrying over the prior override", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(pick_preset = "demographics")
      session$flushReact()
      session$setInputs(picker_dataset = "ADVS")
      session$flushReact()

      # Switch to a totally different preset -- its default (ADAE) must
      # win, not the leftover ADVS override from demographics.
      session$setInputs(pick_preset = "ae_overall")
      session$flushReact()
      html <- output$overlay$html
      expect_match(html, 'value="ADAE" selected', fixed = TRUE)
      expect_no_match(html, 'value="ADVS" selected', fixed = TRUE)
    }
  )
})

test_that("a search keystroke while a picker is open preserves the user's dataset override", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(pick_preset = "demographics")
      session$flushReact()
      session$setInputs(picker_dataset = "ADVS")
      session$flushReact()

      session$setInputs(search = "demog")
      session$flushReact()
      html <- output$overlay$html
      expect_match(html, 'value="ADVS" selected', fixed = TRUE)
    }
  )
})

# ---- server: bare generator path -----------------------------------------

test_that("a bare generator path adds via add_from_generator with no roles", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(pick_generator = "km")
      session$flushReact()
      session$setInputs(picker_dataset = "ADTTE")
      session$flushReact()
      session$setInputs(add_generator = 1)

      obj <- selected_object(fx$store)
      expect_identical(obj@type, "km")
      expect_identical(obj@dataset, "ADTTE")
      expect_length(obj@roles, 0L)
      expect_identical(obj@options$number_label, "Figure")
      expect_false(fx$store$rv$adding)
    }
  )
})

test_that("Add is a no-op until a dataset is chosen (no preset/generator picked, or blank dataset)", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$adding <- TRUE
      session$flushReact()
      n_before <- length(.all_objects(fx$store$rv$report))
      # No pick_preset/pick_generator has fired yet -- add_preset must be a
      # no-op, and the overlay must stay open.
      session$setInputs(add_preset = 1)
      expect_length(.all_objects(fx$store$rv$report), n_before)
      expect_true(fx$store$rv$adding)
    }
  )
})

# ---- server: rv$bridge_dataset pre-select + clear ------------------------

test_that("rv$bridge_dataset pre-selects the preset picker, then clears after add", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$bridge_dataset <- "ADAE"
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(pick_preset = "ae_soc_pt")
      session$flushReact()
      html <- output$overlay$html
      expect_match(html, 'value="ADAE" selected', fixed = TRUE)

      # testServer does not run the client JS that syncs a freshly-mounted
      # selectize's `selected=` attribute back into `input$picker_dataset`
      # (a real browser does this automatically on mount) -- simulate that
      # sync explicitly, matching every other picker-then-add test in this
      # file.
      session$setInputs(picker_dataset = "ADAE")
      session$flushReact()
      session$setInputs(add_preset = 1)
      obj <- selected_object(fx$store)
      expect_identical(obj@dataset, "ADAE")
      expect_null(fx$store$rv$bridge_dataset)
    }
  )
})

test_that("rv$bridge_dataset pre-selects the generator picker too", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$bridge_dataset <- "ADVS"
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(pick_generator = "line")
      session$flushReact()
      html <- output$overlay$html
      expect_match(html, 'value="ADVS" selected', fixed = TRUE)
    }
  )
})

test_that("closing the overlay clears rv$bridge_dataset even without an add", {
  fx <- .ao_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(
    mod_add_output_server,
    args = list(store = fx$store),
    {
      fx$store$rv$bridge_dataset <- "ADAE"
      fx$store$rv$adding <- TRUE
      session$flushReact()
      session$setInputs(close = 1)
      expect_null(fx$store$rv$bridge_dataset)
    }
  )
})

# ---- end-to-end: the real arframe() launcher, a real browser ---------------

test_that("arframe() Add-output overlay: opens, shows Recommended + domain-grouped library, screenshot", {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("apps/add_output"),
    name = "add_output",
    height = 900,
    width = 1440
  )
  withr::defer(app$stop())

  # The app opens in Data mode (2026-07-04): flip to Report first.
  app$click(selector = '[data-ar-mode="report"]')
  app$wait_for_idle()

  app$click(selector = "#contents-add_output")
  app$wait_for_idle()

  html <- app$get_html("body", outer_html = TRUE)
  expect_match(html, "ar-add-card", fixed = TRUE)
  expect_match(html, "ADD OUTPUT", fixed = TRUE)
  expect_match(html, "Recommended for your data", fixed = TRUE)
  expect_match(html, "Preset library", fixed = TRUE)
  expect_match(html, ">Safety<", fixed = TRUE)
  expect_match(html, ">Efficacy<", fixed = TRUE)

  # Dev-only screenshot artifact: .local/ is .Rbuildignore'd, so it does not
  # exist in R CMD check's isolated sandbox copy -- write it only when
  # running from the real source tree, never as a hard requirement.
  screens_dir <- testthat::test_path("../../.local/screens")
  if (dir.exists(screens_dir)) {
    screenshot_path <- file.path(screens_dir, "08-add-output.png")
    if (file.exists(screenshot_path)) {
      file.remove(screenshot_path)
    }
    app$get_screenshot(screenshot_path)
  }
})

test_that("arframe() Add-output overlay: focus moves into the dialog on open, back to the trigger on close/Esc", {
  skip_on_cran()
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("apps/add_output"),
    name = "add_output_focus",
    height = 900,
    width = 1440
  )
  withr::defer(app$stop())

  # The app opens in Data mode (2026-07-04): flip to Report first.
  app$click(selector = '[data-ar-mode="report"]')
  app$wait_for_idle()

  app$click(selector = "#contents-add_output")
  app$wait_for_idle()
  # Regression: output$overlay's first mount of the search textInput
  # triggers Shiny's normal "a freshly bound input echoes its value back
  # once" behavior, causing one extra renderUI cycle right after open (the
  # render depends on input$search for live filtering) -- a server-sent
  # "ar-focus" message raced that extra cycle replacing the dialog DOM
  # node. Focus is client-driven (a MutationObserver watching
  # .ar-add-overlay-slot in arframe.js) specifically to survive that.
  active_id <- app$get_js("document.activeElement.id")
  active_class <- app$get_js("document.activeElement.className")
  expect_identical(active_id, "add_output-dialog")
  expect_match(active_class, "ar-add-card", fixed = TRUE)

  app$click(selector = "#add_output-close")
  app$wait_for_idle()
  expect_identical(
    app$get_js("document.activeElement.id"),
    "contents-add_output"
  )

  # Reopen, close via Esc this time -- same focus-return contract.
  app$click(selector = "#contents-add_output")
  app$wait_for_idle()
  app$run_js(
    "document.dispatchEvent(new KeyboardEvent('keydown', {key: 'Escape', bubbles: true}))"
  )
  app$wait_for_idle()
  expect_false(app$get_js("!!document.querySelector('.ar-add-card')"))
  expect_identical(
    app$get_js("document.activeElement.id"),
    "contents-add_output"
  )
})
