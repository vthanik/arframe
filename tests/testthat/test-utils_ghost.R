# The on-page ghost shell: pure, session-free (no Shiny input, no store) --
# takes an arpillar::object, returns tags. Every test here runs with ZERO
# session mounted, proving the shell can be built and unit-tested
# independently of mod_paper.R's server wiring.

# ---- ghost_shell: unfilled slots -----------------------------------------

test_that("a draft summary object gets one ghost block per unfilled slot, naming the slot label", {
  draft <- arpillar::object(id = "t1", type = "summary", dataset = "ADSL")
  html <- as.character(ghost_shell(draft))

  # treatment (-> columns region) and summarize (-> rows region), the two
  # unmet .SLOT_REQS for "summary".
  expect_match(html, "assign treatment arms", fixed = TRUE)
  expect_match(html, "assign summarize", fixed = TRUE)
  expect_match(html, 'data-ar-region="columns"', fixed = TRUE)
  expect_match(html, 'data-ar-region="rows"', fixed = TRUE)
})

test_that("filling one slot removes only that slot's ghost block", {
  half_filled <- arpillar::object(
    id = "t1",
    type = "summary",
    dataset = "ADSL",
    roles = list(
      arpillar::role(
        slot = "treatment",
        items = list(arpillar::data_item(name = "TRT01P"))
      )
    )
  )
  html <- as.character(ghost_shell(half_filled))
  expect_no_match(html, "assign treatment arms", fixed = TRUE)
  expect_match(html, "assign summarize", fixed = TRUE)
})

test_that("a fully configured (READY) object ghosts nothing -- ghost_shell returns empty", {
  ready <- arpillar::object(
    id = "t1",
    type = "summary",
    dataset = "ADSL",
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
  expect_identical(arpillar::output_status(ready), "ready")
  shell <- ghost_shell(ready)
  expect_length(shell, 0L)
})

test_that("an unbound dataset shows a title-region ghost (needs_data precedence)", {
  unbound <- arpillar::object(id = "t1", type = "summary", dataset = "")
  html <- as.character(ghost_shell(unbound))
  expect_match(html, 'data-ar-region="title"', fixed = TRUE)
})

# ---- figure ghost: the axes frame ---------------------------------------

test_that("a draft figure gets an axes-frame-shaped ghost (ar-ghost-axes)", {
  draft_fig <- arpillar::object(id = "f1", type = "line", dataset = "ADVS")
  html <- as.character(ghost_shell(draft_fig))
  expect_match(html, "ar-ghost-axes", fixed = TRUE)
  expect_match(html, 'data-ar-region="axes"', fixed = TRUE)
})

test_that("a draft table gets a rows-shaped ghost (ar-ghost-rows), not the figure axes shape", {
  draft_table <- arpillar::object(
    id = "t1",
    type = "summary",
    dataset = "ADSL",
    roles = list(
      arpillar::role(
        slot = "treatment",
        items = list(arpillar::data_item(name = "TRT01P"))
      )
    )
  )
  html <- as.character(ghost_shell(draft_table))
  expect_match(html, "ar-ghost-rows", fixed = TRUE)
  expect_no_match(html, "ar-ghost-axes", fixed = TRUE)
})

test_that("multiple unmet figure roles collapse to ONE axes ghost block, not three", {
  draft_fig <- arpillar::object(id = "f1", type = "km", dataset = "ADTTE")
  shell <- ghost_shell(draft_fig)
  # km has 3 unmet requirements (time, censor, group) but they all map to
  # the single "axes" region -- the shell must render one block, not three.
  html <- as.character(shell)
  n_axes <- lengths(regmatches(html, gregexpr("ar-ghost-axes", html)))
  expect_identical(n_axes, 1L)
})

# ---- occurrence: hierarchy + population -----------------------------------

test_that("an occurrence object missing population gets a title-region ghost", {
  obj <- arpillar::object(
    id = "o1",
    type = "occurrence",
    dataset = "ADAE",
    roles = list(
      arpillar::role(
        slot = "treatment",
        items = list(arpillar::data_item(name = "TRT01P"))
      ),
      arpillar::role(
        slot = "hierarchy",
        items = list(arpillar::data_item(name = "AEDECOD"))
      )
    )
  )
  expect_identical(arpillar::output_status(obj), "draft")
  html <- as.character(ghost_shell(obj))
  expect_match(html, 'data-ar-region="title"', fixed = TRUE)
})

test_that("an occurrence object missing hierarchy gets a rows-region ghost naming it", {
  obj <- arpillar::object(
    id = "o1",
    type = "occurrence",
    dataset = "ADAE",
    options = list(population = "ADSL"),
    roles = list(
      arpillar::role(
        slot = "treatment",
        items = list(arpillar::data_item(name = "TRT01P"))
      )
    )
  )
  html <- as.character(ghost_shell(obj))
  expect_match(html, 'data-ar-region="rows"', fixed = TRUE)
  expect_match(html, "hierarchy", fixed = TRUE)
})

# ---- empty report ------------------------------------------------------

test_that(".ghost_empty_report renders the CTA wired to ns('add_first')", {
  ns <- shiny::NS("paper")
  html <- as.character(.ghost_empty_report(ns))
  expect_match(html, "Add your first output", fixed = TRUE)
  expect_match(html, "Add output", fixed = TRUE)
  expect_match(html, 'id="paper-add_first"', fixed = TRUE)
  expect_match(html, "action-button", fixed = TRUE)
})

# ---- region mapping (helper unit coverage) ---------------------------------

test_that(".ghost_region maps every documented control_id per the brief's jump-link table", {
  expect_identical(.ghost_region("dataset"), "title")
  expect_identical(.ghost_region("roles-treatment"), "columns")
  expect_identical(.ghost_region("roles-summarize"), "rows")
  expect_identical(.ghost_region("roles-x"), "axes")
  expect_identical(.ghost_region("roles-y"), "axes")
  expect_identical(.ghost_region("roles-group"), "axes")
  expect_identical(.ghost_region("roles-time"), "axes")
  expect_identical(.ghost_region("roles-censor"), "axes")
  expect_identical(.ghost_region("roles-hierarchy"), "rows")
  expect_identical(.ghost_region("population"), "title")
})

test_that(".ghost_region falls back to title for an unrecognized control_id", {
  expect_identical(.ghost_region("something-new"), "title")
})

test_that(".ghost_hint falls back to the validate_output message when no slot matches", {
  hint <- .ghost_hint("summary", "dataset", "Choose a dataset for this output.")
  expect_identical(hint, "choose a dataset for this output")
})

test_that(".ghost_hint reads the generator's own slot label, not validate_output's sentence", {
  hint <- .ghost_hint(
    "summary",
    "roles-treatment",
    "Assign a treatment variable."
  )
  expect_identical(hint, "assign treatment arms")
})
