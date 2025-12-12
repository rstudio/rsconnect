test_that("deploy does not error", {
  expect_true(deployApp("example-shiny", account = account))
})
