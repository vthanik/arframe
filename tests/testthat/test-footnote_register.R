# Footnote register (Stage 4). `@KEY` in an object's footnotes resolves
# to the registered text at render/export time -- the study footnote
# sheet pattern of a SAS TFL QC workbook. Unregistered keys pass
# through literally. Byte-golden invariant: empty register + no `@KEY`
# footnotes render identically to a run with no register at all.

.mk_object_fn <- function(footnotes) {
  arpillar::object(
    id = "t1",
    type = "summary",
    dataset = "ADSL",
    title = "Test",
    footnotes = footnotes,
    options = list(),
    roles = list()
  )
}

test_that("@KEY expands to registered text", {
  obj <- .mk_object_fn("@SAFPOP")
  out <- .with_footnotes(
    obj,
    theme = list(
      footnotes = list(SAFPOP = "Safety Population.")
    )
  )
  expect_identical(out@footnotes, "Safety Population.")
})

test_that("@KEY followed by extra text keeps the extra text", {
  obj <- .mk_object_fn("@SAFPOP N is the enrolled subject count.")
  out <- .with_footnotes(
    obj,
    theme = list(
      footnotes = list(SAFPOP = "Safety Population.")
    )
  )
  expect_identical(
    out@footnotes,
    "Safety Population. N is the enrolled subject count."
  )
})

test_that("unregistered @KEY passes through literally (never silently dropped)", {
  obj <- .mk_object_fn("@UNKNOWN")
  out <- .with_footnotes(obj, theme = list(footnotes = list()))
  expect_identical(out@footnotes, "@UNKNOWN")
})

test_that("no @ prefix -> string passes through unchanged", {
  obj <- .mk_object_fn("Safety Population.")
  out <- .with_footnotes(
    obj,
    theme = list(
      footnotes = list(SAFPOP = "Something else.")
    )
  )
  expect_identical(out@footnotes, "Safety Population.")
})

test_that("empty register + no @KEY footnotes -> identical (byte-golden precondition)", {
  obj <- .mk_object_fn(c("A", "B"))
  a <- .with_footnotes(obj, theme = list())
  b <- .with_footnotes(obj, theme = list(footnotes = list()))
  expect_identical(a@footnotes, obj@footnotes)
  expect_identical(b@footnotes, obj@footnotes)
})

test_that("multi-line footnotes: only @KEY lines resolve", {
  obj <- .mk_object_fn(c("@SAFPOP", "N counts subjects with data.", "@ITT"))
  out <- .with_footnotes(
    obj,
    theme = list(
      footnotes = list(
        SAFPOP = "Safety Population.",
        ITT = "Intention-to-Treat."
      )
    )
  )
  expect_identical(
    out@footnotes,
    c(
      "Safety Population.",
      "N counts subjects with data.",
      "Intention-to-Treat."
    )
  )
})

test_that("object with no footnotes returns unchanged (fast path)", {
  obj <- .mk_object_fn(character(0))
  out <- .with_footnotes(
    obj,
    theme = list(
      footnotes = list(SAFPOP = "Safety Population.")
    )
  )
  expect_identical(out, obj)
})
