test_that("configureApp() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()

  expect_error(
    configureApp("myapp", account = "connect-user", server = "connect-server"),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    configureApp(
      "myapp",
      account = "cloud-user",
      server = "connect.posit.cloud"
    ),
    regexp = "`server` must be shinyapps\\.io"
  )
})

test_that("setProperty() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()
  appDir <- local_temp_app()

  expect_error(
    setProperty(
      "application.instances.count",
      1,
      appPath = appDir,
      appName = "myapp",
      account = "connect-user",
      server = "connect-server"
    ),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    setProperty(
      "application.instances.count",
      1,
      appPath = appDir,
      appName = "myapp",
      account = "cloud-user",
      server = "connect.posit.cloud"
    ),
    regexp = "`server` must be shinyapps\\.io"
  )
})

test_that("unsetProperty() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()
  appDir <- local_temp_app()

  expect_error(
    unsetProperty(
      "application.instances.count",
      appPath = appDir,
      appName = "myapp",
      account = "connect-user",
      server = "connect-server"
    ),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    unsetProperty(
      "application.instances.count",
      appPath = appDir,
      appName = "myapp",
      account = "cloud-user",
      server = "connect.posit.cloud"
    ),
    regexp = "`server` must be shinyapps\\.io"
  )
})

test_that("showProperties() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()
  appDir <- local_temp_app()

  expect_error(
    showProperties(
      appPath = appDir,
      appName = "myapp",
      account = "connect-user",
      server = "connect-server"
    ),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    showProperties(
      appPath = appDir,
      appName = "myapp",
      account = "cloud-user",
      server = "connect.posit.cloud"
    ),
    regexp = "`server` must be shinyapps\\.io"
  )
})
