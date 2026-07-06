# Pure walkers over the S7 document tree: .find_object, .replace_object,
# .remove_object, .move_object, .all_objects, .object_from_template, .next_id.
# No store, no reactives, no catalog -- plain report -> page -> object trees.

.tr_object <- function(id, type = "summary", dataset = "ADSL", title = "") {
  arpillar::object(id = id, type = type, title = title, dataset = dataset)
}

.tr_report <- function(...) {
  objs <- list(...)
  arpillar::report(
    id = "r1",
    name = "Test report",
    pages = list(arpillar::page(id = "p1", objects = objs))
  )
}

test_that(".find_object locates an object by id across pages", {
  o1 <- .tr_object("out001")
  o2 <- .tr_object("out002")
  rep <- arpillar::report(
    id = "r1",
    name = "Test",
    pages = list(
      arpillar::page(id = "p1", objects = list(o1)),
      arpillar::page(id = "p2", objects = list(o2))
    )
  )
  expect_identical(.find_object(rep, "out002")@id, "out002")
  expect_null(.find_object(rep, "nope"))
})

test_that(".replace_object swaps one object, siblings untouched", {
  o1 <- .tr_object("out001", title = "A")
  o2 <- .tr_object("out002", title = "B")
  rep <- .tr_report(o1, o2)
  new_o1 <- S7::set_props(o1, title = "A-edited")
  rep2 <- .replace_object(rep, "out001", new_o1)
  expect_identical(.find_object(rep2, "out001")@title, "A-edited")
  expect_identical(.find_object(rep2, "out002")@title, "B")
  # original report is untouched (S7 immutability -- rebuild, not mutate)
  expect_identical(.find_object(rep, "out001")@title, "A")
})

test_that(".remove_object drops the object and leaves siblings in order", {
  o1 <- .tr_object("out001")
  o2 <- .tr_object("out002")
  o3 <- .tr_object("out003")
  rep <- .tr_report(o1, o2, o3)
  rep2 <- .remove_object(rep, "out002")
  ids <- vapply(rep2@pages[[1]]@objects, function(o) o@id, character(1))
  expect_identical(ids, c("out001", "out003"))
})

test_that(".move_object reorders within a page", {
  o1 <- .tr_object("out001")
  o2 <- .tr_object("out002")
  o3 <- .tr_object("out003")
  rep <- .tr_report(o1, o2, o3)
  rep2 <- .move_object(rep, "out003", 1L)
  ids <- vapply(rep2@pages[[1]]@objects, function(o) o@id, character(1))
  expect_identical(ids, c("out003", "out001", "out002"))
})

test_that(".move_object is a no-op on a page that does not hold the id", {
  o1 <- .tr_object("out001")
  o2 <- .tr_object("out002")
  rep <- arpillar::report(
    id = "r1",
    name = "Test",
    pages = list(
      arpillar::page(id = "p1", objects = list(o1)),
      arpillar::page(id = "p2", objects = list(o2))
    )
  )
  rep2 <- .move_object(rep, "out001", 1L)
  ids2 <- vapply(rep2@pages[[2]]@objects, function(o) o@id, character(1))
  expect_identical(ids2, "out002")
})

test_that(".all_objects flattens objects across every page in order", {
  o1 <- .tr_object("out001")
  o2 <- .tr_object("out002")
  o3 <- .tr_object("out003")
  rep <- arpillar::report(
    id = "r1",
    name = "Test",
    pages = list(
      arpillar::page(id = "p1", objects = list(o1, o2)),
      arpillar::page(id = "p2", objects = list(o3))
    )
  )
  ids <- vapply(.all_objects(rep), function(o) o@id, character(1))
  expect_identical(ids, c("out001", "out002", "out003"))
})

test_that(".roles_from_preset resolves role_type off the catalog, category fallback", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  roles <- .roles_from_preset(
    con,
    "ADSL",
    list(treatment = "TRT01P", summarize = c("AGE", "BOGUSVAR"))
  )
  items <- roles[[2]]@items
  expect_identical(items[[1]]@role_type, "measure")
  # BOGUSVAR is not a column of any demo dataset: never dropped, defaults
  # to category (+ no label), and the validate step reports it.
  expect_identical(items[[2]]@role_type, "category")
  expect_identical(items[[2]]@label, "")
})

test_that(".roles_from_preset builds one role per slot with per-var role_type", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  roles <- .roles_from_preset(
    con,
    "ADSL",
    list(treatment = "TRT01P", summarize = c("AGE", "SEX"))
  )
  expect_length(roles, 2L)
  expect_identical(roles[[1]]@slot, "treatment")
  expect_length(roles[[1]]@items, 1L)
  # `.roles_from_preset()` swaps the preset-listed "TRT01P" for the first
  # CDISC arm-var alternative the target dataset actually carries; the
  # demo ADSL ships both TRT01A and TRT01P, and TRT01A wins per
  # `.ARM_VAR_ALTS` order (safety-actual first).
  expect_identical(roles[[1]]@items[[1]]@name, "TRT01A")
  expect_identical(roles[[1]]@items[[1]]@role_type, "category")

  expect_identical(roles[[2]]@slot, "summarize")
  expect_length(roles[[2]]@items, 2L)
  expect_identical(roles[[2]]@items[[1]]@name, "AGE")
  expect_identical(roles[[2]]@items[[1]]@role_type, "measure")
  expect_identical(roles[[2]]@items[[2]]@name, "SEX")
  expect_identical(roles[[2]]@items[[2]]@role_type, "category")
})

test_that(".roles_from_preset does not drop a var absent from the dataset", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  # BOGUSVAR is absent from the demo ADSL -- it must still appear as a
  # data_item (default role_type "category"), never be silently filtered.
  roles <- .roles_from_preset(
    con,
    "ADSL",
    list(summarize = c("AGE", "SEX", "BOGUSVAR"))
  )
  names <- vapply(roles[[1]]@items, function(it) it@name, character(1))
  expect_identical(names, c("AGE", "SEX", "BOGUSVAR"))
  bogus_item <- roles[[1]]@items[[3]]
  expect_identical(bogus_item@role_type, "category")
})

test_that(".object_from_preset copies type/title/footnotes/filters/options and binds dataset", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  pr <- arpillar::preset("demographics")
  obj <- .object_from_preset(con, pr, "ADSL", "out001")

  expect_identical(obj@id, "out001")
  expect_identical(obj@type, pr$generator)
  expect_identical(obj@type, "summary")
  expect_identical(obj@title, pr$title)
  expect_identical(obj@dataset, "ADSL")
  # Presets carry a canned footnote (e.g. "Safety Population.") that reads
  # as noise most of the time: `.object_from_preset()` intentionally starts
  # `footnotes` empty like `.object_from_generator()`; users can add their
  # own. `title`/`filters`/`options` still carry through.
  expect_identical(obj@footnotes, character(0))
  expect_identical(obj@filters, pr$filters)
  expect_identical(obj@options$number, "14.1.1")
  expect_identical(obj@options$number_label, "Table")
})

test_that(".object_from_preset builds roles for every preset slot", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  pr <- arpillar::preset("demographics")
  obj <- .object_from_preset(con, pr, "ADSL", "out001")

  slots <- vapply(obj@roles, function(r) r@slot, character(1))
  expect_identical(slots, names(pr$roles))
  treatment_role <- obj@roles[[which(slots == "treatment")]]
  # See the `.roles_from_preset()` test above: TRT01A wins over the
  # preset's hard-coded "TRT01P" on the demo ADSL.
  expect_identical(treatment_role@items[[1]]@name, "TRT01A")
})

test_that(".object_from_preset carries population for an occurrence preset", {
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  pr <- arpillar::preset("ae_overall")
  # ae_overall targets an AE-domain dataset; applying it to ADSL (which
  # lacks AEDECOD) must NOT error -- the absent hierarchy var defaults to
  # role_type "category" rather than being dropped (fail-loud belongs to
  # validate/render, not construction).
  obj <- .object_from_preset(con, pr, "ADSL", "out001")

  expect_identical(obj@type, "occurrence")
  expect_identical(obj@options$population, "ADSL")
  slots <- vapply(obj@roles, function(r) r@slot, character(1))
  expect_true("hierarchy" %in% slots)
  hier_role <- obj@roles[[which(slots == "hierarchy")]]
  expect_identical(hier_role@items[[1]]@name, "AEDECOD")
  expect_identical(hier_role@items[[1]]@role_type, "category")
})

test_that(".object_from_preset handles a preset-shaped list with no footnotes", {
  pr <- list(
    generator = "km",
    title = "No-footnote figure",
    roles = list(),
    filters = list(),
    options = list(number = "14.2.9", number_label = "Figure"),
    footnotes = character(0)
  )
  con <- .demo_catalog()
  withr::defer(arpillar::engine_close(con))
  obj <- .object_from_preset(con, pr, "ADTTE", "out001")
  expect_identical(obj@footnotes, character(0))
})

test_that(".kind_number_label maps table/figure/listing to their TLF label", {
  expect_identical(.kind_number_label("table"), "Table")
  expect_identical(.kind_number_label("figure"), "Figure")
  expect_identical(.kind_number_label("listing"), "Listing")
})

test_that(".object_from_generator builds a bare object with an auto-suggested number", {
  gen <- arpillar::generator("summary")
  obj <- .object_from_generator(
    gen,
    "ADSL",
    "out001",
    existing_numbers = character(0)
  )

  expect_identical(obj@id, "out001")
  expect_identical(obj@type, "summary")
  expect_identical(obj@dataset, "ADSL")
  expect_length(obj@roles, 0L)
  expect_identical(obj@options$number, "14.1.1")
  expect_identical(obj@options$number_label, "Table")
})

test_that(".object_from_generator suggests one past the highest existing same-kind number", {
  gen <- arpillar::generator("crosstab") # kind "table", prefix "14.1"
  obj <- .object_from_generator(
    gen,
    "ADSL",
    "out004",
    existing_numbers = c("14.1.1", "14.1.3", "14.2.9")
  )
  # 14.2.9 (figure kind) is ignored -- only same-kind (14.1.*) numbers count.
  expect_identical(obj@options$number, "14.1.4")
})

test_that(".next_number starts at .1 for an empty document and skips other-kind numbers", {
  expect_identical(.next_number("table", character(0)), "14.1.1")
  expect_identical(.next_number("figure", c("14.1.1", "14.1.2")), "14.2.1")
  expect_identical(
    .next_number("figure", c("14.2.1", "14.2.2", "14.1.9")),
    "14.2.3"
  )
})

test_that(".next_id is monotonic over existing ids, sprintf('out%03d', n)", {
  rep_empty <- .tr_report()
  expect_identical(.next_id(rep_empty), "out001")

  o1 <- .tr_object("out001")
  o2 <- .tr_object("out005")
  rep <- .tr_report(o1, o2)
  expect_identical(.next_id(rep), "out006")
})
