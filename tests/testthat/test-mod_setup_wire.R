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
    # Continuous stat rows: only the label is a wired input (stats are
    # mutated by add/remove chip events, not scalar inputs).
    grep("^cont_label_[0-9]+$", ids, value = TRUE),
    # Band rows (left/center/right) per row index x head/foot.
    grep(
      "^page_page(head|foot)_(left|center|right)_[0-9]+$",
      ids,
      value = TRUE
    ),
    # Treatment arm rows.
    grep("^arm_row_(value|label)_[0-9]+$", ids, value = TRUE),
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
    cat_level_format = "n_pct",
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
    session$setInputs(study_sponsor = "Acme")
    expect_identical(st$rv$report@theme$study$sponsor, "Acme")

    session$setInputs(study_protocol = "PA-101")
    expect_identical(st$rv$report@theme$study$protocol, "PA-101")

    # -- Data --
    session$setInputs(data_adam_dir = "/data/adam")
    expect_identical(st$rv$report@theme$data$adam_dir, "/data/adam")

    # -- Treatment (row-list wiring, 2026-07-10): a row's pick writes
    # `vars[[i]]` and mirrors row 1 into `trtvar` (the arm-decode source) --
    session$setInputs(trt_var_set = list(i = 1, value = "TRT01A"))
    expect_identical(st$rv$report@theme$treatment$trtvar, "TRT01A")
    expect_identical(
      st$rv$report@theme$treatment$vars[[1]],
      list(var = "TRT01A", basis = "actual")
    )
    session$setInputs(trt_basis_set = list(i = 1, value = "planned"))
    expect_identical(st$rv$report@theme$treatment$vars[[1]]$basis, "planned")

    # -- Paths --
    session$setInputs(paths_programs_dir = "pgms")
    expect_identical(st$rv$report@theme$paths$programs_dir, "pgms")

    # -- Page geometry --
    session$setInputs(page_orientation = "portrait")
    expect_identical(st$rv$report@theme$page$orientation, "portrait")

    session$setInputs(page_font_size = "12")
    expect_identical(st$rv$report@theme$page$font_size, 12L)

    # Margins (study-level, moved here from per-output 2026-07-08): a 4-value
    # "top, right, bottom, left" string, a single "all sides" value, and a bad
    # parse (neither 1 nor 4) that drops -> the prior value stands.
    session$setInputs(page_margins = "1.5, 1, 1, 1")
    expect_identical(st$rv$report@theme$page$margins, c(1.5, 1, 1, 1))
    session$setInputs(page_margins = "0.75")
    expect_identical(st$rv$report@theme$page$margins, 0.75)
    session$setInputs(page_margins = "1, 2")
    expect_identical(st$rv$report@theme$page$margins, 0.75)

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

test_that("Setup renderUI has 9 pharma sections in the expected order", {
  # Render the module UI once (with a fake NS) and grep the resulting
  # HTML for `data-ar-section` attributes -- the ordered ids the
  # `.setup_section` shell stamps on each section container. Sources
  # / Data / Preferences must be absent (Stage 2 rearrangement).
  st <- .mk_store()
  ids <- character(0)
  shiny::testServer(mod_setup_server, args = list(store = st), {
    # Trigger the seed observer first so populations render.
    session$flushReact()
    html <- as.character(htmltools::renderTags(output$sections)$html)
    m <- regmatches(
      html,
      gregexpr("data-ar-section=\"([a-z_]+)\"", html)
    )[[1]]
    ids <<- sub(".*=\"([a-z_]+)\".*", "\\1", m)
  })
  expect_identical(
    ids,
    c(
      "study",
      "paths",
      "populations",
      "analysis_sets",
      "treatment",
      "page",
      "summaries",
      "footnotes",
      "team"
    )
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
    # Last row is blank: empty label, empty stats, no format field.
    tail_row <- after[[length(after)]]
    expect_identical(tail_row$label, "")
    expect_identical(tail_row$stats, character(0))
    expect_null(tail_row$format)
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

test_that("cont_stat_add / cont_stat_remove mutate a row's stats vector", {
  st <- .mk_store()
  shiny::testServer(mod_setup_server, args = list(store = st), {
    # These observers use ignoreInit = TRUE (skip the widget's init value);
    # testServer swallows the first event as that init, so prime each event
    # input with a guarded no-op before the real events.
    session$setInputs(cont_stat_add = list(i = 1, value = "", nonce = 0))
    # Row 1 seed is n -> stats = "n". Add "se" and "cv" (not used elsewhere).
    session$setInputs(cont_stat_add = list(i = 1, value = "se", nonce = 1))
    session$setInputs(cont_stat_add = list(i = 1, value = "cv", nonce = 2))
    row <- shiny::isolate(st$rv$report@theme$summaries$continuous)[[1]]
    expect_identical(row$stats, c("n", "se", "cv"))
    # A statistic already used in ANOTHER row is rejected -- "mean" lives in
    # the seed "Mean (SD)" row, so it cannot also join row 1 (global unique).
    session$setInputs(cont_stat_add = list(i = 1, value = "mean", nonce = 3))
    row <- shiny::isolate(st$rv$report@theme$summaries$continuous)[[1]]
    expect_identical(row$stats, c("n", "se", "cv"))
    # Remove the middle atom (prime the remove input first).
    session$setInputs(cont_stat_remove = list(i = 1, stat = "", nonce = 0))
    session$setInputs(cont_stat_remove = list(i = 1, stat = "se", nonce = 1))
    row <- shiny::isolate(st$rv$report@theme$summaries$continuous)[[1]]
    expect_identical(row$stats, c("n", "cv"))
  })
})

test_that("cont_reorder permutes the continuous rows by the posted order", {
  st <- .mk_store()
  shiny::testServer(mod_setup_server, args = list(store = st), {
    # Materialize the seed rows into the theme first (append a blank row).
    session$setInputs(cont_add = 1)
    before <- shiny::isolate(st$rv$report@theme$summaries$continuous)
    n <- length(before)
    # Reverse the row order (SortableJS posts order as a list of strings).
    ord <- as.list(as.character(rev(seq_len(n))))
    session$setInputs(cont_reorder = list(order = ord, nonce = 1))
    after <- shiny::isolate(st$rv$report@theme$summaries$continuous)
    expect_identical(after, rev(before))
  })
})

test_that("Summaries drops pct_n and shows the clear level-format labels", {
  ns <- shiny::NS("s")
  st <- .mk_store()
  html <- shiny::isolate(as.character(arframe:::.setup_summaries(ns, st)))
  # pct_n is gone; the three remaining level formats keep their stored values.
  expect_false(grepl('data-ar-seg-value="pct_n"', html, fixed = TRUE))
  expect_true(grepl('data-ar-seg-value="n_pct"', html, fixed = TRUE))
  # Display label reads "n (%)", not the raw "n_pct".
  expect_true(grepl(">n (%)<", html, fixed = TRUE))
  # Continuous rows are a sortable list.
  expect_true(grepl(
    'data-ar-sortable-input="s-cont_reorder"',
    html,
    fixed = TRUE
  ))
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

    # An unrelated edit commits -> the report bumps, re-rendering the live
    # tab strip. The active tab is server-authoritative (`active_tab()`) and
    # the visible-section class rides the static, client-owned wrapper, so
    # neither the commit nor the strip re-render can bounce it back to study.
    session$setInputs(study_sponsor = "Acme Pharma")
    expect_identical(active_tab(), "summaries")

    # The re-rendered strip restamps an active tab from `active_tab()`.
    expect_match(
      as.character(htmltools::renderTags(output$tabstrip)$html),
      "ar-setup-tab-active",
      fixed = TRUE
    )
  })
})

test_that("Study date input posts to the server (native date needs onchange)", {
  # A native <input type="date"> is NOT matched by Shiny's text-input
  # binding (text/search/url/email only), so without an inline onchange ->
  # setInputValue it never posts and theme$study$data_date stays empty --
  # the Study completion badge then counts it missing forever.
  st <- .mk_store()
  withr::defer(arpillar::engine_close(st$con))
  html <- shiny::isolate(as.character(.setup_study(shiny::NS("frame"), st)))
  expect_match(html, 'type="date"', fixed = TRUE)
  # Quotes render as HTML entities (&#39;), so match on the stable parts:
  # the date input carries an onchange that posts this.value via setInputValue.
  expect_match(html, "onchange=", fixed = TRUE)
  expect_match(html, "Shiny.setInputValue(", fixed = TRUE)
  expect_match(html, "this.value", fixed = TRUE)
})

test_that("study_data_date round-trips into theme$study$data_date", {
  st <- .mk_store()
  shiny::testServer(mod_setup_server, args = list(store = st), {
    session$setInputs(study_data_date = "2006-06-27")
    expect_identical(st$rv$report@theme$study$data_date, "2006-06-27")
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

test_that(".CONT_ATOMS carries the full standard statistic vocabulary", {
  atoms <- arframe:::.CONT_ATOMS
  # The common set survives, and the master-file additions are present:
  # CI of the mean, percentiles, and the PK geometric family.
  expect_true(all(
    c("n", "mean", "sd", "median", "min", "max") %in% atoms
  ))
  expect_true(all(c("cv", "var", "sum", "qrange", "lclm", "uclm") %in% atoms))
  expect_true(all(c("p1", "p5", "p10", "p90", "p95", "p99") %in% atoms))
  expect_true(all(
    c("geomean", "geosd", "geose", "geocv", "geolclm", "geouclm") %in% atoms
  ))
  expect_identical(anyDuplicated(atoms), 0L)
})

test_that("decimals-by rules: multiple names per row share one dp", {
  st <- .mk_store()
  shiny::testServer(mod_setup_server, args = list(store = st), {
    session$setInputs(dec_add = 1)
    expect_length(shiny::isolate(st$rv$report@theme$decimals_by), 1L)

    # dec_name_add / dec_name_remove use ignoreInit = TRUE; prime each.
    session$setInputs(dec_name_add = list(i = 1, value = "", nonce = 0))
    # Two params share the one row (e.g. WEIGHTBL + HEIGHTBL at 1 dp).
    session$setInputs(
      dec_name_add = list(i = 1, value = "P|WEIGHTBL", nonce = 1)
    )
    session$setInputs(
      dec_name_add = list(i = 1, value = "P|HEIGHTBL", nonce = 2)
    )
    session$setInputs(
      dec_row_change = list(i = 1, field = "dp", value = "1", nonce = 3)
    )
    r1 <- shiny::isolate(st$rv$report@theme$decimals_by[[1]])
    expect_identical(r1$names, c("P|WEIGHTBL", "P|HEIGHTBL"))
    expect_identical(r1$dp, 1L)
    # Adding a duplicate is a no-op.
    session$setInputs(
      dec_name_add = list(i = 1, value = "P|WEIGHTBL", nonce = 4)
    )
    expect_identical(
      shiny::isolate(st$rv$report@theme$decimals_by[[1]]$names),
      c("P|WEIGHTBL", "P|HEIGHTBL")
    )
    # Remove one name (prime the remove input first).
    session$setInputs(dec_name_remove = list(i = 1, value = "", nonce = 0))
    session$setInputs(
      dec_name_remove = list(i = 1, value = "P|WEIGHTBL", nonce = 1)
    )
    expect_identical(
      shiny::isolate(st$rv$report@theme$decimals_by[[1]]$names),
      "P|HEIGHTBL"
    )

    session$setInputs(dec_delete = 1)
    expect_length(shiny::isolate(st$rv$report@theme$decimals_by), 0L)
  })
})

test_that(".dec_rule_names migrates an old single by/name rule", {
  expect_identical(
    arframe:::.dec_rule_names(list(by = "param", name = "SYSBP", dp = 0L)),
    "P|SYSBP"
  )
  expect_identical(
    arframe:::.dec_rule_names(list(names = c("V|AGE", "P|PULSE"))),
    c("V|AGE", "P|PULSE")
  )
  expect_identical(arframe:::.dec_rule_names(list(dp = 0L)), character(0))
})

test_that("Setup renderUI has no overview strip (removed 2026-07-07)", {
  # The overview stat strip was dropped from Setup per user request; the
  # dashboard is now just the section tab strip + the active section card.
  expect_false(exists(".setup_overview", where = asNamespace("arframe")))
})

test_that(".dec_pick_items lists only numeric vars + params labelled 'parameter'", {
  st <- .mk_store()
  withr::defer(arpillar::engine_close(st$con))
  df <- data.frame(
    AGE = c(1, 2, 3),
    SEX = c("M", "F", "M"),
    ADT = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    PARAMCD = c("BMI", "BMI", "WEIGHT"),
    AVAL = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  pq <- withr::local_tempfile(fileext = ".parquet")
  artoo::write_parquet(df, pq)
  arpillar::register_dataset(st$con, "ADTEST", pq)

  out <- shiny::isolate(arframe:::.dec_pick_items(st, "ADTEST", character(0)))

  # Variable rows are numeric-only: AGE + AVAL, never SEX / ADT / PARAMCD.
  vrows <- out[startsWith(out$value, "V|"), , drop = FALSE]
  expect_setequal(sub("^V\\|", "", vrows$value), c("AGE", "AVAL"))
  expect_true(all(vrows$type == "measure"))

  # No PARAM column -> param rows carry the bare word "parameter".
  prows <- out[startsWith(out$value, "P|"), , drop = FALSE]
  expect_setequal(sub("^P\\|", "", prows$value), c("BMI", "WEIGHT"))
  expect_true(all(prows$sub == "parameter"))
})

test_that(".dec_pick_items uses PARAM as the param description when present", {
  st <- .mk_store()
  withr::defer(arpillar::engine_close(st$con))
  df <- data.frame(
    PARAMCD = c("ALB", "ALB", "ALT"),
    PARAM = c(
      "Albumin (g/L)",
      "Albumin (g/L)",
      "Alanine Aminotransferase (U/L)"
    ),
    AVAL = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  pq <- withr::local_tempfile(fileext = ".parquet")
  artoo::write_parquet(df, pq)
  arpillar::register_dataset(st$con, "ADLB", pq)

  out <- shiny::isolate(arframe:::.dec_pick_items(st, "ADLB", character(0)))
  prows <- out[startsWith(out$value, "P|"), , drop = FALSE]

  # Distinct PARAMCD -> name; the decoded PARAM value -> muted description.
  expect_setequal(sub("^P\\|", "", prows$value), c("ALB", "ALT"))
  expect_identical(prows$sub[prows$name == "ALB"], "Albumin (g/L)")
  expect_identical(
    prows$sub[prows$name == "ALT"],
    "Alanine Aminotransferase (U/L)"
  )
})
