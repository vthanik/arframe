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

test_that(".object_from_template copies type/title/footnotes and binds dataset", {
  tpl <- arpillar::template("demographics")
  obj <- .object_from_template(tpl, "ADSL", "out001")
  expect_identical(obj@id, "out001")
  expect_identical(obj@type, tpl$type)
  expect_identical(obj@title, tpl$title)
  expect_identical(obj@dataset, "ADSL")
  expect_identical(obj@footnotes, as.character(tpl$footnotes))
})

test_that(".object_from_template handles a template with no footnotes", {
  tpl <- arpillar::template("km")
  obj <- .object_from_template(tpl, "ADTTE", "out001")
  expect_identical(obj@footnotes, character(0))
})

test_that(".next_id is monotonic over existing ids, sprintf('out%03d', n)", {
  rep_empty <- .tr_report()
  expect_identical(.next_id(rep_empty), "out001")

  o1 <- .tr_object("out001")
  o2 <- .tr_object("out005")
  rep <- .tr_report(o1, o2)
  expect_identical(.next_id(rep), "out006")
})
