test_that("errors if no accounts", {
  mockr::local_mock(accounts = fake_accounts(character(), character()))

  expect_snapshot(deploymentTarget(), error = TRUE)
})

test_that("errors if unknown server", {
  # TODO: give better error for unknown server
  expect_snapshot(deploymentTarget(server = "baz"), error = TRUE)
})

test_that("errors if bad account", {
  mockr::local_mock(accounts = fake_accounts("ron", "bar"))

  expect_snapshot(error = TRUE, {
    deploymentTarget(server = NULL, account = "john")
  })
})
