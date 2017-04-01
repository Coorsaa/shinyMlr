context("downloadShinyMlr")

test_that("downloadShinyMlr", {
  downloadShinyMlr()
  app.dir = "./shinyMlr"
  # test directory got created
  expect_true(dir.exists(app.dir))
  # check there's actually a shiny app in there
  app = shinyAppDir(app.dir)
  expect_true(is.shiny.appobj(app))
})
