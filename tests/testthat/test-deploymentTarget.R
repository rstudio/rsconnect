test_that("errors if no accounts", {
  local_temp_config()

  expect_snapshot(deploymentTarget(), error = TRUE)
})

test_that("errors if unknown account or server", {
  local_temp_config()
  addTestServer("bar")
  addTestAccount("foo", "bar")

  expect_snapshot(error = TRUE, {
    deploymentTarget(server = "unknown")
    deploymentTarget(account = "john")
  })
})

test_that("errors if no previous deployments and multiple accounts", {
  local_temp_config()
  addTestServer("foo1")
  addTestServer("foo2")
  addTestAccount("ron", "foo1")
  addTestAccount("ron", "foo2")

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  expect_snapshot(error = TRUE, {
    deploymentTarget(app_dir)
    deploymentTarget(app_dir, appName = "test")
  })
})

test_that("handles accounts if only server specified", {
  local_temp_config()
  addTestServer("foo")
  addTestAccount("ron", "foo")
  addTestAccount("john", "foo")
  local_mocked_bindings(applications = function(...) data.frame())

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  expect_snapshot(deploymentTarget(app_dir, server = "foo"), error = TRUE)

  target <- deploymentTarget(
    app_dir,
    server = "foo",
    account = "ron"
  )
  expect_equal(target$username, "ron")
})

test_that("errors/prompts if multiple deployments", {
  local_temp_config()
  addTestServer("server1.com")
  addTestServer("server2.com")
  addTestAccount("ron", "server1.com")
  addTestAccount("ron", "server2.com")

  app_dir <- withr::local_tempdir()
  addTestDeployment(app_dir, server = "server1.com")
  addTestDeployment(app_dir, server = "server2.com")

  expect_snapshot(error = TRUE, {
    deploymentTarget(app_dir, appName = "test")
    deploymentTarget(app_dir)
  })

  simulate_user_input(1)
  expect_snapshot(out <- deploymentTarget(app_dir))
  expect_equal(out$appName, "test")
})

test_that("succeeds if there's a single existing deployment", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "test",
    appId = "1",
    username = "ron",
    version = "999"
  )

  target <- deploymentTarget(app_dir)
  expect_equal(target$appId, "1")
  expect_equal(target$username, "ron")
  expect_equal(target$version, "999")

  target <- deploymentTarget(app_dir, appName = "test")
  expect_equal(target$appId, "1")
  expect_equal(target$username, "ron")
})

test_that("errors if single deployment and appId doesn't match", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  app_dir <- withr::local_tempdir()
  addTestDeployment(app_dir, appName = "test", appId = "1", username = "ron")

  expect_snapshot(
    error = TRUE,
    deploymentTarget(app_dir, appName = "test", appId = "2")
  )
})

test_that("new title overrides existing title", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")
  local_mocked_bindings(applications = function(...) data.frame())
  app_dir <- withr::local_tempdir()
  addTestDeployment(app_dir, appTitle = "old title")

  target <- deploymentTarget(app_dir)
  expect_equal(target$appTitle, "old title")

  target <- deploymentTarget(app_dir, appTitle = "new title")
  expect_equal(target$appTitle, "new title")
})

test_that("new env vars overrides existing", {
  local_temp_config()
  app <- local_temp_app()
  addTestServer()
  addTestAccount()
  addTestDeployment(app, envVars = "TEST1")

  target <- deploymentTarget(app)
  expect_equal(target$envVars, "TEST1")

  target <- deploymentTarget(app, envVars = "TEST2")
  expect_equal(target$envVars, "TEST2")

  # And check that it works with vectors
  addTestDeployment(app, envVars = c("TEST1", "TEST2"))
  target <- deploymentTarget(app)
  expect_equal(target$envVars, c("TEST1", "TEST2"))

  target <- deploymentTarget(app, envVars = "TEST2")
  expect_equal(target$envVars, "TEST2")
})

test_that("empty character vector removes env vars", {
  local_temp_config()
  app <- local_temp_app()
  addTestServer()
  addTestAccount()
  addTestDeployment(app, envVars = "TEST1")

  target <- deploymentTarget(app, envVars = character())
  expect_equal(target$envVars, character())
})

test_that("succeeds if there are no deployments and a single account", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")
  local_mocked_bindings(applications = function(...) data.frame())

  app_dir <- dirCreate(file.path(withr::local_tempdir(), "my_app"))

  target <- deploymentTarget(app_dir, envVars = c("TEST1", "TEST2"))
  expect_equal(target$appName, "my_app")
  expect_equal(target$username, "ron")
  expect_equal(target$envVars, c("TEST1", "TEST2"))

  target <- deploymentTarget(app_dir, appName = "foo")
  expect_equal(target$username, "ron")
})

test_that("default title is the empty string", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")
  local_mocked_bindings(applications = function(...) data.frame())

  app_dir <- withr::local_tempdir()
  target <- deploymentTarget(app_dir)
  expect_equal(target$appTitle, "")
})

confirm_existing_app_used <- function(server) {
  local_temp_config()
  addTestServer()
  addTestAccount("ron", server = server)
  local_mocked_bindings(
    applications = function(...) data.frame(
      name = "my_app",
      id = 123,
      url = "http://example.com/test",
      stringsAsFactors = FALSE
    ),
    shouldUpdateApp = function(...) TRUE
  )

  app_dir <- withr::local_tempdir()
  target <- deploymentTarget(app_dir, appName = "my_app", server = server)
  expect_equal(target$appId, 123)
}

test_that("can find existing application on server & use it", {
  confirm_existing_app_used("example.com")
})

test_that("can find existing application on shinyapps.io & use it", {
  confirm_existing_app_used("shinyapps.io")
})

confirm_existing_app_not_used <- function(server) {
  local_temp_config()
  addTestServer()
  addTestAccount("ron", server = server)
  local_mocked_bindings(
    applications = function(...) data.frame(
      name = "my_app",
      id = 123,
      url = "http://example.com/test",
      stringsAsFactors = FALSE
    ),
    shouldUpdateApp = function(...) FALSE
  )

  app_dir <- withr::local_tempdir()
  target <- deploymentTarget(app_dir, appName = "my_app", server = server)
  expect_equal(target$appName, "my_app-1")
  expect_equal(target$appId, NULL)
}

test_that("can find existing application on server & not use it", {
  confirm_existing_app_not_used("example.com")
})

test_that("can find existing application on shinyapps.io & not use it", {
  confirm_existing_app_not_used("shinyapps.io")
})

# defaultAppName ----------------------------------------------------------

test_that("defaultAppName works with sites, documents, and directories", {
  expect_equal(defaultAppName("foo/bar.Rmd"), "bar")
  expect_equal(defaultAppName("foo/index.html"), "foo")
  expect_equal(defaultAppName("foo/bar"), "bar")
})

test_that("defaultAppName reifies appNames for shinyApps", {
  expect_equal(defaultAppName("a b c", "shinyapps.io"), "a_b_c")
  expect_equal(defaultAppName("a!b!c", "shinyapps.io"), "a_b_c")
  expect_equal(defaultAppName("a  b  c", "shinyapps.io"), "a_b_c")

  long_name <- strrep("abcd", 64 / 4)
  expect_equal(defaultAppName(paste(long_name, "..."), "shinyapps.io"), long_name)
})

test_that("deploymentTargetForApp works with cloud", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")
  local_mocked_bindings(
    getApplication = function(...) list(name = "name", id = "id")
  )

  target <- deploymentTargetForApp("123")
  expect_equal(target$username, "ron")
})

# helpers -----------------------------------------------------------------

test_that("shouldUpdateApp errors when non-interactive", {
  app <- list(name = "my_app", url = "https://example.com")

  expect_snapshot(shouldUpdateApp(app, "my_app-1"), error = TRUE)
})

test_that("forceUpdate shortcircuits shouldUpdateApp", {
  expect_true(shouldUpdateApp(forceUpdate = TRUE))
})

test_that("shouldUpdateApp handles 3 options", {
  app <- list(name = "my_app", url = "https://example.com")

  simulate_user_input(c(1, 2, 3))
  expect_snapshot(error = TRUE, {
    one <- shouldUpdateApp(app, "my_app-1")
    two <- shouldUpdateApp(app, "my_app-1")
    three <- shouldUpdateApp(app, "my_app-1")
  })
  expect_equal(one, TRUE)
  expect_equal(two, FALSE)
})

test_that("findUnique always returns unique name", {
  expect_equal(findUnique("x", c("x", "y")), "x-1")
  expect_equal(findUnique("x", c("x", "x-1")), "x-2")
  expect_equal(findUnique("x", c("x", "x-1", "x-2")), "x-3")
})
