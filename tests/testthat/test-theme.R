test_that("ar_theme is Bootstrap brand variables only, Galley values", {
  th <- ar_theme()
  expect_s3_class(th, "bs_theme")
  # Linear-family chrome + explorer-blue accent (2026-07-06 redesign).
  # Case-insensitive compare — bslib normalises to uppercase on read.
  expect_identical(
    toupper(bslib::bs_get_variables(th, "primary")[["primary"]]),
    "#0378CD"
  )
})

test_that("head assets link both stylesheets and the bridge script from one resource path", {
  # .head_assets() wraps its <link>/<script> tags in tags$head() (verified
  # against a live-served Shiny app: an UNwrapped <link> stays in <body>,
  # which is non-standard placement). htmltools::renderTags() correspondingly
  # splits head-destined content into $head, separate from $html (the body
  # stream) -- so a plain as.character(tagList(...)) can never see it;
  # inspect $head.
  head <- as.character(
    htmltools::renderTags(htmltools::tagList(.head_assets()))$head
  )
  expect_match(head, "arwww/tokens.css", fixed = TRUE)
  expect_match(head, "arwww/arframe.css", fixed = TRUE)
  expect_match(head, "arwww/arframe.bundle.js", fixed = TRUE)
  # Sortable must load BEFORE the bundle -- bridge.js references the global.
  expect_lt(
    regexpr("arwww/Sortable.min.js", head, fixed = TRUE),
    regexpr("arwww/arframe.bundle.js", head, fixed = TRUE)
  )
})

test_that("Galley ink/paper pairs hold WCAG AA", {
  lum <- function(hex) {
    v <- strtoi(
      c(substr(hex, 2, 3), substr(hex, 4, 5), substr(hex, 6, 7)),
      16L
    ) /
      255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(v * c(0.2126, 0.7152, 0.0722))
  }
  cr <- function(a, b) {
    (max(lum(a), lum(b)) + 0.05) / (min(lum(a), lum(b)) + 0.05)
  }
  expect_gte(cr("#1B1F23", "#FFFFFF"), 4.5) # ink on paper
  expect_gte(cr("#5C6670", "#FFFFFF"), 4.5) # ink-3 on paper (footnotes, source)
  expect_gte(cr("#5C6670", "#E9EBEA"), 4.5) # ink-3 on desk = the floor for ALL
  #                                           readable small text (micro-labels,
  #                                           group headers, TOC numbers). ink-4 /
  #                                           ink-5 are decoration only (leader
  #                                           dots, hairlines) -- never info text.
  expect_gte(cr("#B3261E", "#FFFFFF"), 4.5) # error on paper
  expect_gte(cr("#0378CD", "#FFFFFF"), 4.5) # accent (Run button fill under
  #                                           white label; links on paper)
  # Stamp text sits in the TOC on the desk, so each stamp hex must clear 4.5:1
  # THERE (mockup greens/ambers are indicative; tune the three hexes to pass).
  for (hex in c("#257045", "#7A5409", "#B3261E")) {
    # ready / draft / error text
    expect_gte(cr(hex, "#E9EBEA"), 4.5)
  }
})
