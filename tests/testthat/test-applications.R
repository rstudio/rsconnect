test_that("syncAppMetadata updates deployment records", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  dir <- local_temp_app()
  addTestDeployment(dir, appId = "123", metadata = list(when = 123))
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      getApplication = function(...) list(title = "newtitle", url = "newurl")
    )
  })

  syncAppMetadata(dir)
  deps <- deployments(dir)
  expect_equal(deps$title, "newtitle")
  expect_equal(deps$url, "newurl")
  expect_equal(deps$when, NULL)
})

test_that("syncAppMetadata deletes deployment records if needed", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  dir <- local_temp_app()
  addTestDeployment(dir, appId = "123", metadata = list(when = 123))
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      getApplication = function(...) abort(class = "rsconnect_http_404")
    )
  })

  expect_snapshot(syncAppMetadata(dir))
  expect_equal(nrow(deployments(dir)), 0)
})
