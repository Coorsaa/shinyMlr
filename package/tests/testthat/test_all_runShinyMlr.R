# FIXME: Add tetsts for runShinyMlr()
# Possibly use shinytest https://github.com/rstudio/shinytest

context("runShinyMlr")

test_that("can run app", {
  expect_message({
    R.utils::evalWithTimeout(runShinyMlr(port = 445566), timeout = 10, onTimeout = "silent")
  }, "Listening on http://127.0.0.1:445566")
})

# test_that("extra args to 'runApp'", {
# })
