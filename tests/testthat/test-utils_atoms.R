test_that(".stamp maps the four oracle states to letterpress stamps", {
  s <- as.character(.stamp("ready"))
  expect_match(s, "READY", fixed = TRUE)
  expect_match(s, "ar-stamp-ready", fixed = TRUE)
  expect_match(s, "aria-label", fixed = TRUE)
  expect_match(as.character(.stamp("needs_data")), "NO DATA", fixed = TRUE)
  expect_match(as.character(.stamp("broken")), "ERROR", fixed = TRUE)
  # The app-side stale flag (run semantics, decision #8) stamps too.
  expect_match(as.character(.stamp("stale")), "STALE", fixed = TRUE)
  expect_match(as.character(.stamp("stale")), "ar-stamp-stale", fixed = TRUE)
  expect_error(.stamp("nope"), class = "arframe_error_input")
})

test_that(".icon pins fontawesome margins; .type_chip maps three kinds", {
  fa <- as.character(.icon("pin"))
  expect_match(fa, "margin-left:0", fixed = TRUE)
  expect_match(
    as.character(.type_chip("measure")),
    "ar-chip-meas",
    fixed = TRUE
  )
  expect_match(as.character(.type_chip("date")), "ar-chip-date", fixed = TRUE)
  expect_match(
    as.character(.type_chip("category")),
    "ar-chip-cat",
    fixed = TRUE
  )
})

test_that(".action_btn is Shiny-bindable without btn-default", {
  b <- as.character(.action_btn("go", "Go"))
  expect_match(b, "action-button", fixed = TRUE)
  expect_no_match(b, "btn-default")
})

test_that(".type_icon renders a distinct glyph per generator type, with a safe fallback", {
  occ <- as.character(.type_icon("occurrence"))
  expect_match(occ, "M3.8 3.2 V11.3", fixed = TRUE)
  expect_false(identical(
    as.character(.type_icon("km")),
    as.character(.type_icon("box"))
  ))
  # Every registered generator type is distinct from every other.
  types <- names(.TYPE_ICONS)
  rendered <- vapply(
    types,
    function(t) as.character(.type_icon(t)),
    character(1)
  )
  expect_length(unique(rendered), length(types))
  # An unknown type falls back to the summary glyph rather than erroring.
  expect_identical(
    as.character(.type_icon("not-a-real-type")),
    as.character(.type_icon("summary"))
  )
})

test_that(".output_slug: kind letter + number + title, filesystem-safe", {
  obj <- arpillar::object(
    id = "o1",
    type = "km",
    dataset = "ADTTE",
    title = "Kaplan-Meier, OS",
    options = list(number = "14.2.1", number_label = "Figure")
  )
  expect_identical(.output_slug(obj), "f-14-2-1-kaplan-meier-os")
})
