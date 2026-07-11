# The icon system (utils_icons.R): FA chrome icons, per-generator type
# glyphs, and the one-off chrome glyph registry. Tests moved from
# test-utils_atoms.R when the system got its own file (2026-07-11).

test_that(".icon pins fontawesome margins and rejects unknown names", {
  fa <- as.character(.icon("pin"))
  expect_match(fa, "margin-left:0", fixed = TRUE)
  expect_snapshot(error = TRUE, .icon("nope"))
  expect_error(.icon("nope"), class = "arframe_error_input")
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

test_that(".glyph renders house SVGs with dashed state classes; unknown names error", {
  closed <- as.character(.glyph("panel_close"))
  open <- as.character(.glyph("panel_open"))
  # The class carries the dashed name -- that is what the state CSS keys on.
  expect_match(closed, "ar-glyph ar-glyph-panel-close", fixed = TRUE)
  expect_match(open, "ar-glyph ar-glyph-panel-open", fixed = TRUE)
  # House style constants come from the shared .svg_tag wrapper.
  expect_match(closed, 'stroke-width="1.3"', fixed = TRUE)
  expect_match(closed, 'viewBox="0 0 16 16"', fixed = TRUE)
  expect_false(identical(closed, open))
  # A chrome glyph name is author-chosen; a miss is a typo and errors loud.
  expect_snapshot(error = TRUE, .glyph("nope"))
  expect_error(.glyph("nope"), class = "arframe_error_input")
})
