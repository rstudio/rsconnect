test_that("error when no deployments and no accounts", {
  local_temp_config()

  app <- local_temp_app()

  expect_snapshot(findDeployment(app, appName = "placeholder"), error = TRUE)
})

test_that("returns stubbed details when single account has no deployments", {
  local_temp_config()
  addTestServer()
  addTestAccount()

  app <- local_temp_app()

  # derived from directory name.
  dep <- findDeployment(app)
  expect_equal(dep, list(name = basename(app), account = "ron", server = "example.com"))

  # name given.
  dep <- findDeployment(app, appName = "placeholder")
  expect_equal(dep, list(name = "placeholder", account = "ron", server = "example.com"))
})

test_that("finds single deployment", {
  local_temp_config()
  addTestServer()
  addTestAccount()

  app <- local_temp_app()
  addTestDeployment(app)

  dep <- findDeployment(app)
  expect_equal(dep$name, "test")
})

test_that("disambiguates multiple deployments", {
  local_temp_config()
  addTestServer()
  addTestAccount()

  app <- local_temp_app()
  addTestDeployment(app, "test1")
  addTestDeployment(app, "test2")
  expect_snapshot(findDeployment(app), error = TRUE)

  simulate_user_input(2)
  expect_snapshot(dep <- findDeployment(app))
  expect_equal(dep$name, "test2")
})
