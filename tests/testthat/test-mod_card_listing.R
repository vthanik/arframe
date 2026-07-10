# The listing structured-option editors (mod_card_listing): SORT /
# TRANSPOSE / STACKED COLUMNS sections in the Options pane, rendered only
# for the listing generator. Every commit pins the EXACT shape the
# engine's render leg consumes (fct_render_listing.R), and an emptied
# editor removes its key entirely -- never an empty list. Cell recodes
# live in the Roles LEVELS editor (item @levels), not here.

# A bare listing on ADSL, selected -- the structured schema kinds (sort /
# transpose / stacks) all live on this generator.
.mcl_listing_store <- function() {
  con <- .demo_catalog()
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_generator(store, "listing", "ADSL"))
  shiny::isolate(store$rv$selected <- id)
  list(con = con, store = store, id = id)
}

# The packed picker choice for one ADSL column (what a picker posts).
.mcl_choice <- function(con, name) {
  items <- arpillar::data_items(con, "ADSL")
  i <- match(name, items$name)
  .pack_item_choice(items$name[[i]], items$type[[i]], items$label[[i]])
}

# ---- .opt_listing_sections --------------------------------------------------

test_that(".opt_listing_sections: NULL for a summary, three sections for a listing", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))

  summary_obj <- arpillar::object(id = "s", type = "summary", dataset = "ADSL")
  expect_null(.opt_listing_sections(con, shiny::NS("x"), summary_obj))

  listing_obj <- arpillar::object(id = "l", type = "listing", dataset = "ADSL")
  html <- paste(
    as.character(.opt_listing_sections(con, shiny::NS("x"), listing_obj)),
    collapse = ""
  )
  for (lbl in c("SORT", "TRANSPOSE", "STACKED COLUMNS")) {
    expect_match(html, lbl, fixed = TRUE)
  }
  # Value legends are gone: the recode lives in the Roles LEVELS editor.
  expect_no_match(html, "VALUE LEGENDS", fixed = TRUE)
  expect_match(html, "Add a sort key", fixed = TRUE)
  expect_match(html, "On duplicates", fixed = TRUE)
})

test_that("transpose and stack pickers offer only the selected variables", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))

  obj <- arpillar::object(
    id = "l",
    type = "listing",
    dataset = "ADSL",
    roles = list(
      arpillar::role(
        slot = "id",
        items = list(arpillar::data_item(name = "USUBJID"))
      ),
      arpillar::role(
        slot = "columns",
        items = list(
          arpillar::data_item(name = "SEX"),
          arpillar::data_item(name = "AGE")
        )
      )
    ),
    options = list(
      stacks = list(list(
        name = NULL,
        indent = FALSE,
        entries = list(list(vars = character(0)))
      ))
    )
  )
  items <- arpillar::data_items(con, "ADSL")

  # TRANSPOSE: Parameter offers the selected list variables only; Value
  # offers the numeric ones among them.
  tr <- paste(
    as.character(.listing_transpose_section(shiny::NS("x"), obj, items)),
    collapse = ""
  )
  expect_match(tr, '"SEX"', fixed = TRUE)
  expect_match(tr, '"AGE"', fixed = TRUE)
  expect_no_match(tr, '"RACE"', fixed = TRUE) # unselected dataset column
  # AGE is numeric -> a Value choice; SEX is not.
  value_sel <- sub(".*tr_value", "", tr)
  expect_match(value_sel, '"AGE"', fixed = TRUE)
  expect_no_match(value_sel, '"SEX"', fixed = TRUE)

  # STACKED COLUMNS: the per-line add-picker carries only id + list vars.
  st <- paste(
    as.character(.listing_stacks_section(shiny::NS("x"), obj, items)),
    collapse = ""
  )
  expect_match(st, "USUBJID", fixed = TRUE)
  expect_no_match(st, "RACE", fixed = TRUE)
})

test_that(".opt_listing_sections renders committed state (rows, not drafts)", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))

  obj <- arpillar::object(id = "l", type = "listing", dataset = "ADSL")
  obj <- S7::set_props(
    obj,
    options = list(
      sort = list(list(col = "AGE", dir = "desc"))
    )
  )
  html <- paste(
    as.character(.opt_listing_sections(con, shiny::NS("x"), obj)),
    collapse = ""
  )
  expect_match(html, ">AGE<")
  # The desc toggle is the active one on this row.
  expect_match(html, "ar-peek-type-on", fixed = TRUE)
  expect_match(html, ">desc<", fixed = TRUE)
})

# ---- sort -------------------------------------------------------------------

test_that("sort: add commits list(col, dir='asc'); flip -> desc; remove -> absent", {
  fx <- .mcl_listing_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    obj <- function() shiny::isolate(selected_object(store))

    session$setInputs(srt_add = .mcl_choice(fx$con, "AGE"))
    expect_identical(
      obj()@options$sort,
      list(list(col = "AGE", dir = "asc"))
    )

    session$setInputs(srt_dir = list(i = 1, dir = "desc", nonce = 1))
    expect_identical(
      obj()@options$sort,
      list(list(col = "AGE", dir = "desc"))
    )

    # A second key appends after the first.
    session$setInputs(srt_add = .mcl_choice(fx$con, "USUBJID"))
    expect_identical(
      obj()@options$sort,
      list(
        list(col = "AGE", dir = "desc"),
        list(col = "USUBJID", dir = "asc")
      )
    )

    session$setInputs(srt_rm = list(i = 2, nonce = 2))
    session$setInputs(srt_rm = list(i = 1, nonce = 3))
    # Removing the last key removes the key entirely -- never list().
    expect_null(obj()@options$sort)
  })
})

test_that("sort: a stale index and a bad dir are inert", {
  fx <- .mcl_listing_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    obj <- function() shiny::isolate(selected_object(store))
    session$setInputs(srt_add = .mcl_choice(fx$con, "AGE"))

    session$setInputs(srt_dir = list(i = 5, dir = "desc", nonce = 1))
    session$setInputs(srt_dir = list(i = 1, dir = "sideways", nonce = 2))
    session$setInputs(srt_rm = list(i = 9, nonce = 3))
    expect_identical(
      obj()@options$sort,
      list(list(col = "AGE", dir = "asc"))
    )
  })
})

# ---- transpose ---------------------------------------------------------------

test_that("transpose: commits only when param + value are set and distinct", {
  fx <- .mcl_listing_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    obj <- function() shiny::isolate(selected_object(store))

    # Param alone is incomplete -- the key stays absent.
    session$setInputs(tr_param = "SEX")
    expect_null(obj()@options$transpose)

    session$setInputs(tr_value = "AGE")
    expect_identical(
      obj()@options$transpose,
      list(param = "SEX", value = "AGE", agg = "first")
    )

    session$setInputs(tr_agg = "mean")
    expect_identical(
      obj()@options$transpose,
      list(param = "SEX", value = "AGE", agg = "mean")
    )

    # Clearing the value removes the key (incomplete -> absent).
    session$setInputs(tr_value = "")
    expect_null(obj()@options$transpose)
  })
})

test_that("transpose: param == value never commits", {
  fx <- .mcl_listing_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$setInputs(tr_param = "AGE")
    session$setInputs(tr_value = "AGE")
    expect_null(shiny::isolate(selected_object(store))@options$transpose)
  })
})

# ---- stacks ------------------------------------------------------------------

test_that("stacks: add stack + line + var commit the engine shape; remove -> absent", {
  fx <- .mcl_listing_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    obj <- function() shiny::isolate(selected_object(store))

    session$setInputs(stk_add = list(nonce = 1))
    st <- obj()@options$stacks
    expect_length(st, 1L)
    # A new stack starts with one empty entry line ready for its picker.
    expect_identical(st[[1]]$entries, list(list(vars = character(0))))
    expect_false(isTRUE(st[[1]]$indent))

    session$setInputs(
      stk_var_add = list(
        i = 1,
        j = 1,
        value = .mcl_choice(fx$con, "AGE"),
        nonce = 3
      )
    )
    session$setInputs(
      stk_var_add = list(
        i = 1,
        j = 1,
        value = .mcl_choice(fx$con, "SEX"),
        nonce = 4
      )
    )
    session$setInputs(
      stk_field = list(i = 1, field = "name", value = "Age/Sex", nonce = 5)
    )
    session$setInputs(stk_indent = list(i = 1, on = TRUE, nonce = 6))
    session$setInputs(
      stk_entry_field = list(
        i = 1,
        j = 1,
        field = "delim",
        value = "/",
        nonce = 7
      )
    )
    session$setInputs(
      stk_entry_field = list(
        i = 1,
        j = 1,
        field = "prefix",
        value = "(",
        nonce = 8
      )
    )
    session$setInputs(
      stk_entry_field = list(
        i = 1,
        j = 1,
        field = "suffix",
        value = ")",
        nonce = 9
      )
    )

    st <- obj()@options$stacks
    expect_identical(st[[1]]$name, "Age/Sex")
    expect_true(st[[1]]$indent)
    expect_identical(st[[1]]$entries[[1]]$vars, c("AGE", "SEX"))
    expect_identical(st[[1]]$entries[[1]]$delim, "/")
    expect_identical(st[[1]]$entries[[1]]$prefix, "(")
    expect_identical(st[[1]]$entries[[1]]$suffix, ")")

    # A SECOND line prefills the DPP continuation wrap: "(", ")".
    session$setInputs(stk_line_add = list(i = 1, nonce = 10))
    st <- obj()@options$stacks
    expect_length(st[[1]]$entries, 2L)
    expect_identical(st[[1]]$entries[[2]]$prefix, "(")
    expect_identical(st[[1]]$entries[[2]]$suffix, ")")
    session$setInputs(stk_line_rm = list(i = 1, j = 2, nonce = 11))

    # Var + line removal walk back through the same shared inputs.
    session$setInputs(
      stk_var_rm = list(i = 1, j = 1, name = "SEX", nonce = 12)
    )
    expect_identical(obj()@options$stacks[[1]]$entries[[1]]$vars, "AGE")
    session$setInputs(stk_line_rm = list(i = 1, j = 1, nonce = 13))
    expect_identical(obj()@options$stacks[[1]]$entries, list())

    # Removing the only stack removes the key entirely.
    session$setInputs(stk_rm = list(i = 1, nonce = 14))
    expect_null(obj()@options$stacks)
  })
})

# ---- generator guard ----------------------------------------------------------

test_that("listing inputs are inert on a non-listing output (stale client post)", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  store <- shiny::isolate(new_store(con))
  id <- shiny::isolate(add_from_preset(store, "demographics", "ADSL"))
  shiny::isolate(store$rv$selected <- id)
  fx <- list(con = con, store = store, id = id)

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    obj <- function() shiny::isolate(selected_object(store))
    session$setInputs(srt_add = .mcl_choice(fx$con, "AGE"))
    session$setInputs(tr_param = "SEX")
    session$setInputs(tr_value = "AGE")
    session$setInputs(stk_add = list(nonce = 1))
    for (k in c("sort", "transpose", "stacks")) {
      expect_null(obj()@options[[k]])
    }
  })
})

test_that("the listing pane renders the three structured sections in the inspector", {
  fx <- .mcl_listing_store()
  withr::defer(arpillar::engine_close(fx$con))

  shiny::testServer(mod_card_options_server, args = list(store = fx$store), {
    session$flushReact()
    html <- output$pane$html
    for (lbl in c("SORT", "TRANSPOSE", "STACKED COLUMNS")) {
      expect_match(html, lbl, fixed = TRUE)
    }
    expect_no_match(html, "VALUE LEGENDS", fixed = TRUE)
    # ARD-only layout knobs are hidden for a listing (req 6).
    expect_no_match(html, "Stub column header", fixed = TRUE)
    expect_no_match(html, "Total column", fixed = TRUE)
    expect_match(html, "Blank row between blocks", fixed = TRUE)
  })
})
