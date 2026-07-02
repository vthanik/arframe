test_that(".stamp maps the four oracle states to letterpress stamps", {
  s <- as.character(.stamp("ready"))
  expect_match(s, "READY", fixed = TRUE)
  expect_match(s, "ar-stamp-ready", fixed = TRUE)
  expect_match(s, "aria-label", fixed = TRUE)
  expect_match(as.character(.stamp("needs_data")), "NO DATA", fixed = TRUE)
  expect_match(as.character(.stamp("broken")), "ERROR", fixed = TRUE)
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
