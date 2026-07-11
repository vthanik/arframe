# .emit_programs() writes programs/<slug>.R to match the {program} chrome
# token mod_paper.R's .study_tokens() already stamps (paste0("programs/",
# arpillar::output_slug(object), ".R")). Renaming/re-slugging an output must
# prune its old program file, not accumulate stale ones.

.demo_project_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  id1 <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))
  shiny::isolate(update_object(store, id1, function(o) {
    S7::set_props(
      o,
      title = "Demographics",
      options = list(number = "14.1.1", number_label = "Table"),
      roles = list(
        arpillar::role(
          slot = "treatment",
          items = list(arpillar::data_item(name = "TRT01P"))
        ),
        arpillar::role(
          slot = "summarize",
          items = list(arpillar::data_item(
            name = "AGE",
            label = "Age",
            role_type = "measure"
          ))
        )
      )
    )
  }))
  id2 <- shiny::isolate(add_from_generator(store, "summary", "ADSL"))
  shiny::isolate(update_object(store, id2, function(o) {
    S7::set_props(
      o,
      title = "Baseline",
      options = list(number = "14.1.2", number_label = "Table"),
      roles = list(
        arpillar::role(
          slot = "treatment",
          items = list(arpillar::data_item(name = "TRT01P"))
        ),
        arpillar::role(
          slot = "summarize",
          items = list(arpillar::data_item(
            name = "AGE",
            label = "Age",
            role_type = "measure"
          ))
        )
      )
    )
  }))
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  shiny::isolate(store$rv$path <- dir)
  list(con = con, store = store, ids = c(id1, id2), dir = dir)
}

test_that(".emit_programs writes slug-named programs and prunes stale ones", {
  fx <- .demo_project_store()
  withr::defer(arpillar::engine_close(fx$con))
  store <- fx$store

  shiny::isolate(.emit_programs(store))
  slugs <- shiny::isolate(arpillar::output_slugs(store$rv$report))
  progs <- list.files(file.path(fx$dir, "programs"))
  expect_setequal(progs, c(paste0(unname(slugs), ".R"), "run-all.R"))

  # Renaming an output changes its slug -- the OLD program file must be
  # pruned, not left behind alongside the new one.
  first <- fx$ids[[1]]
  shiny::isolate(update_object(
    store,
    first,
    function(o) S7::set_props(o, title = "Renamed"),
    label = "retitle"
  ))
  shiny::isolate(.emit_programs(store))
  slugs2 <- shiny::isolate(arpillar::output_slugs(store$rv$report))
  progs2 <- list.files(file.path(fx$dir, "programs"))
  expect_setequal(progs2, c(paste0(unname(slugs2), ".R"), "run-all.R"))
})

test_that(".emit_programs never prunes the last-known-good program on emit failure", {
  fx <- .demo_project_store()
  withr::defer(arpillar::engine_close(fx$con))
  store <- fx$store

  # Pass 1 (real emits): both slug programs exist -- the record on disk.
  shiny::isolate(.emit_programs(store))
  slugs1 <- shiny::isolate(arpillar::output_slugs(store$rv$report))
  kept_prog <- paste0(slugs1[[fx$ids[[1]]]], ".R")
  old_sibling_prog <- paste0(slugs1[[fx$ids[[2]]]], ".R")
  expect_true(file.exists(file.path(fx$dir, "programs", kept_prog)))

  # Rename the SIBLING (its slug changes) and force every emit_code to
  # fail. The unchanged output's last-known-good program must SURVIVE the
  # failed pass ("the program IS the record"); the sibling's OLD slug file
  # is still pruned because no current output claims it.
  shiny::isolate(update_object(
    store,
    fx$ids[[2]],
    function(o) S7::set_props(o, title = "Renamed sibling"),
    label = "retitle"
  ))
  testthat::local_mocked_bindings(
    emit_code = function(...) stop("emit failed"),
    .package = "arpillar"
  )
  shiny::isolate(.emit_programs(store))

  progs <- list.files(file.path(fx$dir, "programs"))
  expect_true(kept_prog %in% progs)
  expect_false(old_sibling_prog %in% progs)
  expect_true("run-all.R" %in% progs)
})

test_that(".emit_programs falls back to programs/ when programs_dir is blank, not just NULL", {
  # Setup > Paths writes an EMPTY STRING (not NULL) when the path field is
  # left at its default. `%||%` only defaults on NULL, so a blank string
  # must also fall back to ./programs/ -- otherwise the .R programs spill
  # into the project root and the canonical folder layout breaks.
  fx <- .demo_project_store()
  withr::defer(arpillar::engine_close(fx$con))
  store <- fx$store
  shiny::isolate({
    r <- store$rv$report
    store$rv$report <- S7::set_props(
      r,
      theme = modifyList(r@theme, list(paths = list(programs_dir = "")))
    )
  })

  shiny::isolate(.emit_programs(store))

  slugs <- shiny::isolate(arpillar::output_slugs(store$rv$report))
  expect_true(dir.exists(file.path(fx$dir, "programs")))
  expect_setequal(
    list.files(file.path(fx$dir, "programs")),
    c(paste0(unname(slugs), ".R"), "run-all.R")
  )
  # No .R spilled into the project root.
  expect_length(list.files(fx$dir, pattern = "\\.R$"), 0L)
})

test_that(".emit_programs is a no-op when the store has no project path", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))

  expect_null(shiny::isolate(.emit_programs(store)))
})
