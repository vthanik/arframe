test_that("arframe() builds a shiny app object", {
  app <- arframe()
  expect_s3_class(app, "shiny.appobj")
})
