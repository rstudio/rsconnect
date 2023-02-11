test_that("errors if no accounts", {
  mockr::local_mock(accounts = function(...) {
    data.frame(name = character(), server = character(), stringsAsFactors = FALSE)
  })

  expect_snapshot(deploymentTarget(), error = TRUE)
})

test_that("errors if unknown server", {
  # TODO: give better error for unknown server
  expect_snapshot(deploymentTarget(server = "baz"), error = TRUE)
})

test_that("errors if bad account", {
  mockr::local_mock(accounts = function(...) {
    data.frame(name = "ron", server = "bar", stringsAsFactors = FALSE)
  })

  expect_snapshot(error = TRUE, {
    deploymentTarget(server = NULL, account = "john")
  })
})
