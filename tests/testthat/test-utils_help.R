# The help registry (utils_help.R): every Setup card and inspector section
# has an in-depth tutorial entry; the `?` icon posts without toggling its
# accordion/card; unknown topics no-op; no inline `ar-opt-hint` survives.

test_that("registry covers exactly .HELP_REQUIRED", {
  expect_setequal(names(arframe:::.HELP_TOPICS), arframe:::.HELP_REQUIRED)
})

test_that("help entries are substantive tutorials, not tooltips", {
  for (topic in names(arframe:::.HELP_TOPICS)) {
    html <- as.character(arframe:::.HELP_TOPICS[[topic]]())
    visible <- gsub("<[^>]+>", "", html)
    expect_gt(nchar(visible), 400) # real prose, not two words
    expect_match(html, "ar-help-code", info = topic) # >= 1 worked example
  }
})

test_that(".help_icon posts topic without toggling the accordion", {
  tag <- as.character(arframe:::.help_icon(shiny::NS("x"), "filters"))
  expect_match(tag, "event.preventDefault\\(\\); event.stopPropagation\\(\\)")
  expect_match(tag, "x-help_open")
  expect_match(tag, "ar-help-btn")
})

test_that(".show_help on an unknown topic is a silent no-op", {
  expect_silent(res <- arframe:::.show_help("no_such_topic_xyz"))
  expect_null(res)
})

test_that("no ar-opt-hint paragraphs remain in R/ source", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ source not present (installed package)")
  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  hits <- unlist(lapply(files, function(f) {
    grep("ar-opt-hint", readLines(f, warn = FALSE), value = TRUE)
  }))
  expect_equal(length(hits), 0L)
})
