test_that("syncAppMetadata updates deployment records", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  app <- local_temp_app()
  addTestDeployment(app, appId = "123", metadata = list(when = 123))
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      getApplication = function(...) list(title = "newtitle", url = "newurl")
    )
  })

  syncAppMetadata(app)
  deps <- deployments(app)
  expect_equal(deps$title, "newtitle")
  expect_equal(deps$url, "newurl")
  expect_equal(deps$when, NULL)
})

test_that("syncAppMetadata deletes deployment records if needed", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  app <- local_temp_app()
  addTestDeployment(app, appId = "123", metadata = list(when = 123))
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      getApplication = function(...) abort(class = "rsconnect_http_404")
    )
  })

  expect_snapshot(syncAppMetadata(app))
  expect_equal(nrow(deployments(app)), 0)
})
