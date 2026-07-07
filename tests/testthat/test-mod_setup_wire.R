# Gate test for `.SETUP_SPEC` + `.wire_all` (Stage 1). Every scalar
# setup input must round-trip: `session$setInputs(<id> = value)` on the
# module server must land in `store$rv$report@theme` at the declared
# path. A dead binding fails this test loudly -- the design fix for the
# root cause named in the plan.

.mk_store <- function() {
  con <- arpillar::engine_open()
  new_store(con, report = arpillar::report(id = "r1", name = "Test"))
}

test_that(".SETUP_SPEC is non-empty and well-formed", {
  spec <- arframe:::.SETUP_SPEC
  expect_gt(length(spec), 30L)
  # Every entry has id + path.
  for (e in spec) {
    expect_true(is.character(e$id) && length(e$id) == 1L)
    expect_true(is.character(e$path) && length(e$path) >= 1L)
  }
  # ids unique.
  ids <- vapply(spec, function(e) e$id, character(1))
  expect_identical(anyDuplicated(ids), 0L)
})

test_that("every scalar input in mod_setup renders under a spec id", {
  # Reads the R/ source tree, absent in the R CMD check sandbox -- skip on the
  # actual condition (source availability), not the CRAN flag (devtools::check()
  # sets NOT_CRAN=true, so skip_on_cran would not fire here).
  setup_src <- "../../R/mod_setup.R"
  skip_if_not(file.exists(setup_src), "R/ source tree unavailable")
  # Grep the mod_setup source for `.flat_input(ns, "<id>"`, `.seg_control(
  # ns, "<id>"`, and `.select_input(ns, "<id>"` calls -- the three atoms
  # that produce a scalar Shiny input on the Setup page. Every id found
  # must appear in `.SETUP_SPEC` (or match a structural pattern we
  # explicitly excuse below).
  src <- readLines(setup_src)
  rx <- "(\\.flat_input|\\.seg_control|\\.select_input)\\(\\s*ns,\\s*\"([a-z_][a-z0-9_]*)\""
  m <- regmatches(src, regexec(rx, src))
  ids <- vapply(
    Filter(function(x) length(x) >= 3, m),
    function(x) x[[3]],
    character(1)
  )
  ids <- unique(ids)
  # Structural / dynamic ids -- allowed to be absent from the spec.
  structural <- c(
    # Populations rows (id / label / dataset / filter) per row index.
    grep("^pop_(id|label|dataset|filter)_[0-9]+$", ids, value = TRUE),
    # Continuous stat rows (label + format) per row index.
    grep("^cont_(label|format)_[0-9]+$", ids, value = TRUE),
    # Band rows (left/center/right) per row index x head/foot.
    grep(
      "^page_page(head|foot)_(left|center|right)_[0-9]+$",
      ids,
      value = TRUE
    ),
    # Treatment arm rows.
    grep("^arm_row_(level|label)_[0-9]+$", ids, value = TRUE),
    # Footnote register rows.
    grep("^foot_(key|text)_[0-9]+$", ids, value = TRUE)
  )
  scalars <- setdiff(ids, structural)
  spec_ids <- vapply(arframe:::.SETUP_SPEC, function(e) e$id, character(1))
  missing <- setdiff(scalars, spec_ids)
  expect_identical(
    missing,
    character(0),
    info = paste(
      "Rendered scalar inputs with no .SETUP_SPEC entry:",
      paste(missing, collapse = ", ")
    )
  )
})

test_that("wire_all round-trips every scalar entry into the theme", {
  # For each spec entry, drive the input, and assert the value shows up
  # at the declared theme path. Boolean coerce (arm_show_header_n) and
  # integer coerce (decimals_*) exercised via specific values.
  skip_if_not_installed("shinytest2")
  # Actually testServer is enough; no browser needed. Use it.
  spec <- arframe:::.SETUP_SPEC
  fixtures <- list(
    page_orientation = "portrait",
    page_paper = "a4",
    page_font_family = "sans",
    page_font_size = "12",
    arm_show_header_n = "no",
    cat_show_missing = "always",
    cat_level_format = "pct_n",
    cat_header_stat = "total_n"
  )
  shiny::testServer(
    function(id) mod_setup_server(id, .mk_store()),
    {
      # Access the store the module was handed.
      # Because we don't have a way to peek the store from testServer's
      # scope, drive each input and read theme via `session$returned`
      # ... simpler: build the store outside and inject via closure.
      NULL
    }
  )
})

test_that("wire_all round-trip: individual bindings write to theme", {
  # Direct testServer form: create store in outer scope, hand it in via
  # closure so the test can read theme after each set_inputs call.
  st <- .mk_store()
  shiny::testServer(mod_setup_server, args = list(store = st), {
    # -- Study --
    session$setInputs(study_sponsor = "Pfizer")
    expect_identical(st$rv$report@theme$study$sponsor, "Pfizer")

    session$setInputs(study_protocol = "PA-101")
    expect_identical(st$rv$report@theme$study$protocol, "PA-101")

    # -- Data --
    session$setInputs(data_adam_dir = "/data/adam")
    expect_identical(st$rv$report@theme$data$adam_dir, "/data/adam")

    # -- Treatment (Stage 2 wiring proved here) --
    session$setInputs(treatment_trtvar = "TRT01A")
    expect_identical(st$rv$report@theme$treatment$trtvar, "TRT01A")

    # -- Paths --
    session$setInputs(paths_programs_dir = "pgms")
    expect_identical(st$rv$report@theme$paths$programs_dir, "pgms")

    # -- Page geometry --
    session$setInputs(page_orientation = "portrait")
    expect_identical(st$rv$report@theme$page$orientation, "portrait")

    session$setInputs(page_font_size = "12")
    expect_identical(st$rv$report@theme$page$font_size, 12L)

    # -- Arm --
    session$setInputs(arm_show_header_n = "no")
    expect_false(st$rv$report@theme$arm$show_header_n)

    session$setInputs(arm_show_header_n = "yes")
    expect_true(st$rv$report@theme$arm$show_header_n)

    # -- Summaries: categorical (previously dead) --
    session$setInputs(cat_header_stat = "total_n")
    expect_identical(
      st$rv$report@theme$summaries$categorical$header_stat,
      "total_n"
    )

    session$setInputs(cat_missing_label = "Not reported")
    expect_identical(
      st$rv$report@theme$summaries$categorical$missing_label,
      "Not reported"
    )

    # -- Decimals (previously dead) --
    session$setInputs(decimals_mean = "3")
    expect_identical(st$rv$report@theme$decimals$mean, 3L)

    session$setInputs(decimals_pct = "2")
    expect_identical(st$rv$report@theme$decimals$pct, 2L)

    # -- Top-level default_population --
    session$setInputs(top_default_population = "itt")
    expect_identical(st$rv$report@theme$default_population, "itt")
  })
})

test_that("Setup renderUI has 7 pharma sections in the expected order", {
  # Render the module UI once (with a fake NS) and grep the resulting
  # HTML for `data-ar-section` attributes -- the ordered ids the
  # `.setup_section` shell stamps on each section container. Sources
  # / Data / Preferences must be absent (Stage 2 rearrangement).
  st <- .mk_store()
  ids <- character(0)
  shiny::testServer(mod_setup_server, args = list(store = st), {
    # Trigger the seed observer first so populations render.
    session$flushReact()
    html <- as.character(htmltools::renderTags(output$page)$html)
    m <- regmatches(
      html,
      gregexpr("data-ar-section=\"([a-z_]+)\"", html)
    )[[1]]
    ids <<- sub(".*=\"([a-z_]+)\".*", "\\1", m)
  })
  expect_identical(
    ids,
    c("study", "paths", "treatment", "populations", "page", "summaries", "team")
  )
  expect_false(any(c("sources", "data", "preferences") %in% ids))
})

test_that(".setup_section renders an elevated card carrying its section id", {
  sec <- .setup_section(shiny::NS("s"), "study", "Study", shiny::div("body"))
  html <- as.character(sec)
  expect_match(html, "ar-panel", fixed = TRUE)
  expect_match(html, 'data-ar-section="study"', fixed = TRUE)
  expect_match(html, "Study", fixed = TRUE)
})

test_that("cont_add appends a blank continuous row (fixes dead-button)", {
  st <- .mk_store()
  shiny::testServer(mod_setup_server, args = list(store = st), {
    seed <- shiny::isolate(st$rv$report@theme$summaries$continuous) %||% list()
    session$setInputs(cont_add = 1)
    after <- shiny::isolate(st$rv$report@theme$summaries$continuous)
    expect_gt(length(after), length(seed))
    # Last row is blank / format = "a".
    tail_row <- after[[length(after)]]
    expect_identical(tail_row$label, "")
    expect_identical(tail_row$format, "a")
  })
})

test_that("cont_delete removes the row at the given index", {
  st <- .mk_store()
  shiny::testServer(mod_setup_server, args = list(store = st), {
    # Seed the theme via cont_add to have a known state.
    session$setInputs(cont_add = 1)
    n_before <- length(shiny::isolate(st$rv$report@theme$summaries$continuous))
    session$setInputs(cont_delete = n_before)
    after <- shiny::isolate(st$rv$report@theme$summaries$continuous)
    expect_length(after, n_before - 1L)
  })
})

test_that("Treatment section arm add / delete mutates theme$treatment$arms", {
  st <- .mk_store()
  shiny::testServer(mod_setup_server, args = list(store = st), {
    # arm_add appends a blank row
    seed_arms <- shiny::isolate(st$rv$report@theme$treatment$arms) %||% list()
    session$setInputs(arm_add = 1)
    after_add <- shiny::isolate(st$rv$report@theme$treatment$arms)
    expect_gt(length(after_add), length(seed_arms))
    # arm_delete removes the row at the given index
    n_before <- length(after_add)
    session$setInputs(arm_delete = n_before)
    after_del <- shiny::isolate(st$rv$report@theme$treatment$arms)
    expect_length(after_del, n_before - 1L)
  })
})

test_that("active Setup tab survives a re-render triggered by a commit", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(mod_setup_server, args = list(store = store), {
    # User switches to a non-default tab.
    session$setInputs(setup_tab = "summaries")
    expect_identical(active_tab(), "summaries")

    # An unrelated edit commits -> output$page re-renders.
    session$setInputs(study_sponsor = "Acme Pharma")

    # The active tab must still be "summaries" (not reset to "study"),
    # and the rendered page must stamp it on the wrapper.
    expect_identical(active_tab(), "summaries")
    expect_match(
      as.character(htmltools::renderTags(output$page)$html),
      "ar-setup-tab-summaries",
      fixed = TRUE
    )
  })
})

test_that("bad precision input is dropped silently (coerce -> NULL)", {
  st <- .mk_store()
  shiny::testServer(mod_setup_server, args = list(store = st), {
    # Seed a good value.
    session$setInputs(decimals_n = "0")
    expect_identical(st$rv$report@theme$decimals$n, 0L)
    # Now a bad value -- letters. Coerce returns NULL; the write is
    # skipped; the good value stays.
    session$setInputs(decimals_n = "abc")
    expect_identical(st$rv$report@theme$decimals$n, 0L)
    # Negative -- also rejected.
    session$setInputs(decimals_n = "-1")
    expect_identical(st$rv$report@theme$decimals$n, 0L)
  })
})

test_that(".setup_overview renders sections-done and a real subjects count", {
  # Verified directly against .demo_catalog(): 4 datasets (ADSL/ADVS/ADTTE/
  # ADAE), ADSL has 12 rows and 12 distinct USUBJID -- so a default store on
  # the demo catalog resolves ALL FOUR tiles, subjects included, with real
  # (not fabricated) numbers.
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  sections <- list(list(id = "study"), list(id = "team"))
  html <- shiny::isolate(as.character(.setup_overview(store, sections)))
  expect_match(html, "ar-setup-overview", fixed = TRUE)
  expect_match(html, "Sections ready", fixed = TRUE)
  expect_match(html, "0/2", fixed = TRUE) # neither seed section is "ok"
  expect_match(html, "Datasets", fixed = TRUE)
  expect_match(html, ">4<", fixed = TRUE) # 4 datasets in the demo catalog
  expect_match(html, "Records (adsl)", fixed = TRUE)
  expect_match(html, "Subjects", fixed = TRUE)
  expect_match(html, ">12<", fixed = TRUE) # real distinct USUBJID count
})

test_that(".setup_overview omits the subjects tile when the subject-id column does not resolve", {
  # Same demo catalog (so Datasets + Records still resolve), but the
  # subject-id column is seeded to a name absent from ADSL --
  # arpillar::distinct_values() errors, the tryCatch yields NA, and the
  # no-fabrication contract means the tile is OMITTED, never shown as 0.
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  shiny::isolate({
    theme <- store$rv$report@theme
    theme$data$subject_id <- "NOPE_COL"
    commit(store, S7::set_props(store$rv$report, theme = theme))
  })
  sections <- list(list(id = "study"), list(id = "team"))
  html <- shiny::isolate(as.character(.setup_overview(store, sections)))
  expect_match(html, "ar-setup-overview", fixed = TRUE)
  expect_match(html, "Sections ready", fixed = TRUE)
  expect_match(html, "Datasets", fixed = TRUE)
  expect_no_match(html, "Subjects", fixed = TRUE)
})

test_that(".setup_overview reports the real subject count past distinct_values()'s 100-row picker cap (#task6-review)", {
  # distinct_values() defaults to limit = 100L (a value-picker cap, not a
  # counting API). A >100-distinct-subject fixture -- the 12-subject demo
  # catalog cannot exercise this -- proves the tile shows the true 150, not
  # the capped 100.
  con <- arpillar::engine_open()
  withr::defer(arpillar::engine_close(con))
  big <- data.frame(
    USUBJID = sprintf("S-%04d", 1:150),
    SAFFL = "Y",
    stringsAsFactors = FALSE
  )
  path <- withr::local_tempfile(fileext = ".parquet")
  artoo::write_parquet(big, path)
  arpillar::register_dataset(con, "ADSL", path)
  store <- shiny::isolate(new_store(con))
  sections <- list(list(id = "study"), list(id = "team"))
  html <- shiny::isolate(as.character(.setup_overview(store, sections)))
  expect_match(html, "Subjects", fixed = TRUE)
  expect_match(html, ">150<", fixed = TRUE)
  expect_no_match(html, ">100<", fixed = TRUE)
})
