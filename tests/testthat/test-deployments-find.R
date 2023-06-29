test_that("errors if no deployments", {
  dir <- local_temp_app()
  expect_snapshot(findDeployment(dir), error = TRUE)
})

test_that("finds single deployment", {
  dir <- local_temp_app()
  addTestServer()
  addTestAccount()
  addTestDeployment(dir)

  dep <- findDeployment(dir)
  expect_equal(dep$name, "test")
})

test_that("disambiguates multiple deployments", {
  dir <- local_temp_app()
  addTestServer()
  addTestAccount()
  addTestDeployment(dir, "test1")
  addTestDeployment(dir, "test2")
  expect_snapshot(findDeployment(dir), error = TRUE)

  simulate_user_input(2)
  expect_snapshot(dep <- findDeployment(dir))
  expect_equal(dep$name, "test2")
})
