# The Options pane (mod_card_options): the Options-tab content of the docked
# inspector. Title section (editable TLF number + label word + title),
# footnotes editor (line 1 = the population statement by convention), and
# schema-generated option rows (arpillar::option_schema) with default-elision
# commits -- a value equal to the engine default REMOVES the key, keeping the
# persisted JSON and emitted code minimal.

# A store with the demographics preset on ADSL, selected -- the summary
# generator's schema is the minimal one (a single decimals int row).
.mco_demo_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(store$rv$selected <- id)
  list(con = con, store = store, id = id)
}

# A bare km output on ADTTE, selected -- the richest option schema (int,
# flag, choice, text, numvec kinds all present).
.mco_km_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_generator(store, "km", "ADTTE"))
  shiny::isolate(store$rv$selected <- id)
  list(con = con, store = store, id = id)
}

# ---- .opt_parse ----------------------------------------------------------

test_that(".opt_parse: int accepts whole numbers, rejects junk, empty removes", {
  expect_identical(.opt_parse("int", "2"), list(ok = TRUE, value = 2L))
  expect_identical(.opt_parse("int", " 3 "), list(ok = TRUE, value = 3L))
  expect_false(.opt_parse("int", "x")$ok)
  expect_false(.opt_parse("int", "1.5")$ok)
  # Empty input = "back to the engine default": ok with a NULL (remove) value.
  expect_identical(.opt_parse("int", ""), list(ok = TRUE, value = NULL))
})

test_that(".opt_parse: numvec parses comma-separated numbers, sorted", {
  expect_identical(
    .opt_parse("numvec", "12, 0, 6"),
    list(ok = TRUE, value = c(0, 6, 12))
  )
  # A trailing comma / stray whitespace never poisons the parse.
  expect_identical(
    .opt_parse("numvec", "0, 6, 12,"),
    list(ok = TRUE, value = c(0, 6, 12))
  )
  expect_false(.opt_parse("numvec", "a, b")$ok)
  expect_identical(.opt_parse("numvec", ""), list(ok = TRUE, value = NULL))
})

test_that(".opt_parse: text passes through, empty string removes the key", {
  expect_identical(
    .opt_parse("text", "Weeks since randomization"),
    list(ok = TRUE, value = "Weeks since randomization")
  )
  expect_identical(.opt_parse("text", ""), list(ok = TRUE, value = NULL))
})

# ---- title section --------------------------------------------------------

test_that("title-section edits commit number, label word, and title props", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$setInputs(number = "14.9.9")
    expect_identical(
      shiny::isolate(selected_object(store))@options$number,
      "14.9.9"
    )

    session$setInputs(number_label = "Listing")
    expect_identical(
      shiny::isolate(selected_object(store))@options$number_label,
      "Listing"
    )

    session$setInputs(title = "Baseline Characteristics")
    expect_identical(
      shiny::isolate(selected_object(store))@title,
      "Baseline Characteristics"
    )
  })
})

test_that("an identical value never commits (no undo churn on input seeding)", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    current <- shiny::isolate(selected_object(store))@title
    depth <- length(store$undo$stack)
    # A dynamically-rendered input posts its seeded value on bind; that
    # first post must not push an undo entry or flip the dirty flag.
    session$setInputs(title = current)
    expect_length(store$undo$stack, depth)
  })
})

# ---- option rows: render --------------------------------------------------

test_that("demographics renders exactly the decimals row (summary schema)", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$flushReact()
    html <- output$pane$html
    expect_match(html, "Decimal places", fixed = TRUE)
    # The int control is a numeric-keyboard text input, not a spinner.
    expect_match(html, 'inputmode="numeric"', fixed = TRUE)
    # No figure-only rows leak into a summary table's pane.
    expect_no_match(html, "Palette", fixed = TRUE)
    expect_no_match(html, "Legend position", fixed = TRUE)
  })
})

test_that("km renders the axes/series/legend groups with engine defaults preselected", {
  fx <- .mco_km_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$flushReact()
    html <- output$pane$html
    # Axes rows (option_schema("km")).
    for (lbl in c(
      "Event value of censor",
      "Confidence band",
      "At-risk table",
      "Censor marks"
    )) {
      expect_match(html, lbl, fixed = TRUE)
    }
    # Section micro-labels group rows by their paper region.
    expect_match(html, ">AXES<")
    expect_match(html, ">SERIES<")
    expect_match(html, ">LEGEND<")
    # Engine defaults preselected: palette Set2 + legend bottom radios are
    # checked; the at-risk flag (default TRUE) is checked.
    expect_match(html, 'value="Set2"\\s+checked')
    expect_match(html, 'value="bottom"\\s+checked')
  })
})

# ---- option rows: commits -------------------------------------------------

test_that("int: '2' commits 2L; 'x' shows the inline message and never commits", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$setInputs(opt_decimals = "2")
    expect_identical(
      shiny::isolate(selected_object(store))@options$decimals,
      2L
    )

    session$setInputs(opt_decimals = "x")
    # Not committed: the last good value stands.
    expect_identical(
      shiny::isolate(selected_object(store))@options$decimals,
      2L
    )
    msg <- output$opt_msg$html
    expect_match(msg, "not a whole number", fixed = TRUE)
  })
})

test_that("default-elision: a value equal to the schema default removes the key", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$setInputs(opt_decimals = "2")
    expect_identical(
      shiny::isolate(selected_object(store))@options$decimals,
      2L
    )
    # Back to the engine default (1) -> the key vanishes from options.
    session$setInputs(opt_decimals = "1")
    expect_null(shiny::isolate(selected_object(store))@options$decimals)
  })
})

test_that("flag and choice commit off-default and elide back on-default", {
  fx <- .mco_km_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    # ci defaults FALSE: TRUE commits, FALSE removes.
    session$setInputs(opt_ci = TRUE)
    expect_true(shiny::isolate(selected_object(store))@options$ci)
    session$setInputs(opt_ci = FALSE)
    expect_null(shiny::isolate(selected_object(store))@options$ci)

    # legend_position defaults "bottom": "right" commits, "bottom" removes.
    session$setInputs(opt_legend_position = "right")
    expect_identical(
      shiny::isolate(selected_object(store))@options$legend_position,
      "right"
    )
    session$setInputs(opt_legend_position = "bottom")
    expect_null(
      shiny::isolate(selected_object(store))@options$legend_position
    )
  })
})

test_that("text commits, empty string removes; numvec parses + sorts", {
  fx <- .mco_km_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$setInputs(opt_x_label = "Weeks since randomization")
    expect_identical(
      shiny::isolate(selected_object(store))@options$x_label,
      "Weeks since randomization"
    )
    session$setInputs(opt_x_label = "")
    expect_null(shiny::isolate(selected_object(store))@options$x_label)

    session$setInputs(opt_time_breaks = "12, 0, 6")
    expect_identical(
      shiny::isolate(selected_object(store))@options$time_breaks,
      c(0, 6, 12)
    )
  })
})

test_that("an options-only commit leaves the ARD memo untouched (cheap edit)", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    obj <- shiny::isolate(selected_object(store))
    invisible(cached_ard(store, obj))
    expect_identical(sum(startsWith(ls(store$cache), "ard::")), 1L)

    session$setInputs(opt_decimals = "2")
    # The cache key excludes options: the memo neither drops nor doubles.
    expect_identical(sum(startsWith(ls(store$cache), "ard::")), 1L)
  })
})

# ---- footnotes ------------------------------------------------------------

test_that("footnote add/edit/remove round-trips through the footnotes prop", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    before <- shiny::isolate(selected_object(store))@footnotes
    n <- length(before)

    session$setInputs(fn_add = 1)
    expect_length(shiny::isolate(selected_object(store))@footnotes, n + 1L)

    session$setInputs(
      fn_edit = list(i = n + 1L, value = "See protocol Section 9.")
    )
    expect_identical(
      shiny::isolate(selected_object(store))@footnotes[[n + 1L]],
      "See protocol Section 9."
    )

    session$setInputs(fn_remove = list(i = n + 1L))
    expect_identical(shiny::isolate(selected_object(store))@footnotes, before)
  })
})

test_that("footnote rows carry no population badge (removed by request)", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$flushReact()
    html <- output$pane$html
    expect_no_match(html, "ar-fn-pop", fixed = TRUE)
    expect_no_match(html, ">population<", fixed = TRUE)
  })
})

# ---- empty state ----------------------------------------------------------

test_that("the pane renders empty (no crash) with no selection", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  shiny::testServer(mod_card_options_server, args = list(store = store), {
    session$flushReact()
    expect_no_match(output$pane$html %||% "", "ar-opt-sec")
  })
})

test_that("REGRESSION: the pane computes while hidden (tab flip is client-only)", {
  # The inspector tab flip never reaches the server (a pure class change),
  # so a suspended output leaves the Options tab blank until an unrelated
  # re-layout. outputOptions() is not introspectable under testServer's
  # mock session, so pin the call in the server body instead.
  src <- paste(deparse(body(mod_card_options_server)), collapse = "\n")
  expect_match(
    src,
    'outputOptions(output, "pane", suspendWhenHidden = FALSE)',
    fixed = TRUE
  )
  expect_match(
    src,
    'outputOptions(output, "opt_msg", suspendWhenHidden = FALSE)',
    fixed = TRUE
  )
})

# ---- stage-10 depth: reorder, stepper, stats editor, ranks-key filter ------

test_that("footnote drag-reorder commits the new order and re-keys the rows", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$setInputs(fn_add = 1)
    session$setInputs(
      fn_edit = list(i = 2L, value = "Second line.", nonce = 1)
    )
    before <- shiny::isolate(selected_object(store))@footnotes
    expect_identical(before, c("Safety Population.", "Second line."))

    session$setInputs(fn_reorder = list(order = list("2", "1")))
    expect_identical(
      shiny::isolate(selected_object(store))@footnotes,
      c("Second line.", "Safety Population.")
    )

    # The pane redrew, so row 1 now carries the moved line (fresh keys).
    html <- output$pane$html
    expect_match(html, 'value="Second line."[^>]*aria-label="Footnote 1"')

    # A partial/stale payload reconciles instead of losing lines.
    session$setInputs(fn_reorder = list(order = list("2")))
    expect_identical(
      shiny::isolate(selected_object(store))@footnotes,
      c("Safety Population.", "Second line.")
    )
  })
})

test_that("the decimals stepper steps off the committed value and repaints", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    # Preset decimals = 1 (the engine default, elided). Step up -> 2.
    session$setInputs(opt_step = list(key = "decimals", dir = 1, nonce = 1))
    expect_identical(
      shiny::isolate(selected_object(store))@options$decimals,
      2L
    )
    # The derived-precision hint follows the stepped value.
    expect_match(output$pane$html, "mean 2 dp", fixed = TRUE)

    # Step down back to the default -> the key elides.
    session$setInputs(opt_step = list(key = "decimals", dir = -1, nonce = 2))
    expect_null(shiny::isolate(selected_object(store))@options$decimals)

    # Never steps below zero.
    session$setInputs(opt_step = list(key = "decimals", dir = -1, nonce = 3))
    session$setInputs(opt_step = list(key = "decimals", dir = -1, nonce = 4))
    d <- shiny::isolate(selected_object(store))@options$decimals
    expect_true(is.null(d) || d >= 0L)
  })
})

test_that("stats: remove and add-back commit an ordered subset; the default elides", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$flushReact()
    # The stats editor renders one grip row per default statistic.
    html <- output$pane$html
    expect_match(html, "ar-opt-stats", fixed = TRUE)
    expect_match(html, 'data-ar-item="Mean (SD)"', fixed = TRUE)

    session$setInputs(opt_stat_rm = list(value = "Min, Max", nonce = 1))
    expect_identical(
      shiny::isolate(selected_object(store))@options$stats,
      c("n", "Mean (SD)", "Median", "Q1, Q3")
    )

    # Add it back -> the full default set -> the key elides.
    session$setInputs(opt_stat_add = "Min, Max")
    expect_null(shiny::isolate(selected_object(store))@options$stats)
  })
})

test_that("stats: a reorder commits the explicit order through default-elision", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$setInputs(
      opt_reorder_stats = list(
        order = list("Median", "n", "Mean (SD)", "Q1, Q3", "Min, Max")
      )
    )
    expect_identical(
      shiny::isolate(selected_object(store))@options$stats,
      c("Median", "n", "Mean (SD)", "Q1, Q3", "Min, Max")
    )

    # Dragging back to the engine default order elides the key entirely.
    session$setInputs(
      opt_reorder_stats = list(
        order = list("n", "Mean (SD)", "Median", "Q1, Q3", "Min, Max")
      )
    )
    expect_null(shiny::isolate(selected_object(store))@options$stats)
  })
})

test_that("stats: the last remaining statistic can never be removed", {
  fx <- .mco_demo_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    for (v in c("n", "Mean (SD)", "Median", "Q1, Q3")) {
      session$setInputs(opt_stat_rm = list(value = v, nonce = v))
    }
    expect_identical(
      shiny::isolate(selected_object(store))@options$stats,
      "Min, Max"
    )
    # Removing the last one is refused -- an empty set would silently mean
    # "everything" engine-side, which reads as a broken control.
    session$setInputs(opt_stat_rm = list(value = "Min, Max", nonce = "last"))
    expect_identical(
      shiny::isolate(selected_object(store))@options$stats,
      "Min, Max"
    )
  })
})

test_that("ordering keys (hier_sort, x_order) no longer render in Options", {
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

  shiny::testServer(mod_card_options_server, args = list(store = store), {
    session$flushReact()
    html <- output$pane$html
    # x_order's sortable list moved to the Ranks pane; the axes section
    # still carries the other figure knobs.
    expect_no_match(html, "X level order", fixed = TRUE)
    expect_match(html, "X axis label", fixed = TRUE)
  })
})
