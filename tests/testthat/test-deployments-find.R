test_that("errors if no deployments", {
  app <- local_temp_app()
  expect_snapshot(findDeployment(app), error = TRUE)
})

test_that("finds single deployment", {
  app <- local_temp_app()
  addTestServer()
  addTestAccount()
  addTestDeployment(app)

  dep <- findDeployment(app)
  expect_equal(dep$name, "test")
})

test_that("disambiguates multiple deployments", {
  app <- local_temp_app()
  addTestServer()
  addTestAccount()
  addTestDeployment(app, "test1")
  addTestDeployment(app, "test2")
  expect_snapshot(findDeployment(app), error = TRUE)

  simulate_user_input(2)
  expect_snapshot(dep <- findDeployment(app))
  expect_equal(dep$name, "test2")
})
