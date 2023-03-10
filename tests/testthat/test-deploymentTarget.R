test_that("errors if no accounts", {
  mockr::local_mock(accounts = fakeAccounts(character(), character()))

  expect_snapshot(deploymentTarget(), error = TRUE)
})

test_that("errors if unknown account or server", {
  mockr::local_mock(accounts = fakeAccounts("foo", "bar"))

  expect_snapshot(error = TRUE, {
    deploymentTarget(server = "unknown")
    deploymentTarget(account = "john")
  })
})

test_that("errors if no previous deployments and multiple accounts", {
  mockr::local_mock(accounts = fakeAccounts("ron", c("foo1", "foo2")))

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  expect_snapshot(error = TRUE, {
    deploymentTarget(app_dir)
    deploymentTarget(app_dir, appName = "test")
  })
})

test_that("handles accounts if only server specified", {
  mockr::local_mock(
    accounts = fakeAccounts(c("ron", "john"), "foo"),
    applications = function(...) data.frame()
  )

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
  mockr::local_mock(accounts = fakeAccounts("ron", c("server1.com", "server2.com")))

  app_dir <- withr::local_tempdir()
  saveDeployment(
    app_dir,
    target = createDeploymentTarget(
      appName = "test",
      appTitle = "old title",
      appId = "x",
      server = "server1.com",
      username = "ron",
      account = "ron"
    ),
    application = list(id = NA, url = "http://server1.com/test"),
    bundleId = NA,
    hostUrl = NA
  )
  saveDeployment(
    app_dir,
    target = createDeploymentTarget(
      appName = "test",
      appTitle = "old title",
      appId = "x",
      server = "server2.com",
      username = "ron",
      account = "ron"
    ),
    application = list(id = NA, url = "http://server2.com/test"),
    bundleId = NA,
    hostUrl = NA
  )

  expect_snapshot(error = TRUE, {
    deploymentTarget(app_dir, appName = "test")
    deploymentTarget(app_dir)
  })

  withr::local_options(
    rlang_interactive = TRUE,
    cli_prompt = "1"
  )
  expect_snapshot(out <- deploymentTarget(app_dir))
  expect_equal(out$appName, "test")
})

test_that("succeeds if there's a single existing deployment", {
  mockr::local_mock(
    accounts = fakeAccounts("ron", "bar"),
    applications = function(...) data.frame()
  )

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))
  saveDeployment(
    app_dir,
    target = createDeploymentTarget(
      appName = "test",
      appTitle = "old title",
      appId = "x",
      server = "bar",
      username = "ron",
      account = "ron"
    ),
    application = list(id = "1", url = NA),
    bundleId = 1,
    hostUrl = NA
  )

  target <- deploymentTarget(app_dir)
  expect_equal(target$appId, "1")
  expect_equal(target$username, "ron")

  target <- deploymentTarget(app_dir, appName = "test")
  expect_equal(target$appId, "1")
  expect_equal(target$username, "ron")
})

test_that("new title overrides existing title", {
  mockr::local_mock(
    accounts = fakeAccounts("ron", "bar"),
    applications = function(...) data.frame()
  )

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))
  saveDeployment(
    app_dir,
    target = createDeploymentTarget(
      appName = "test",
      appTitle = "old title",
      appId = "x",
      server = "bar",
      username = NA,
      account = "ron"
    ),
    application = list(id = "1", url = NA),
    bundleId = 1,
    hostUrl = NA
  )

  target <- deploymentTarget(app_dir)
  expect_equal(target$appTitle, "old title")

  target <- deploymentTarget(app_dir, appTitle = "new title")
  expect_equal(target$appTitle, "new title")
})

test_that("succeeds if there are no deployments and a single account", {
  mockr::local_mock(
    accounts = fakeAccounts("ron", "bar"),
    applications = function(...) data.frame()
  )

  dir <- withr::local_tempdir()
  app_dir <- file.path(dir, "my_app")
  dir.create(app_dir)
  file.create(file.path(app_dir, "app.R"))

  target <- deploymentTarget(app_dir)
  expect_equal(target$appName, "my_app")
  expect_equal(target$username, "ron")

  target <- deploymentTarget(app_dir, appName = "foo")
  expect_equal(target$username, "ron")
})

test_that("default title is the empty string", {
  mockr::local_mock(
    accounts = fakeAccounts("ron", "bar"),
    applications = function(...) data.frame()
  )

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  target <- deploymentTarget(app_dir)
  expect_equal(target$appTitle, "")
})

test_that("deploy can update title", {
  mockr::local_mock(accounts = fakeAccounts("ron", "bar"))

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  saveDeployment(
    app_dir,
    target = createDeploymentTarget(
      appName = "my_title",
      appTitle = "my title",
      appId = "x",
      server = "bar",
      username = "ron",
      account = "ron"
    ),
    application = list(id = "1", url = NA),
    bundleId = 1,
    hostUrl = NA
  )
  target <- deploymentTarget(app_dir, appTitle = "my new title")
  expect_equal(target$appName, "my_title")
})

test_that("can look up existing application on server", {
  mockr::local_mock(
    accounts = fakeAccounts("ron", "bar"),
    applications = function(...) data.frame(
      name = "my_app",
      id = 123,
      url = "http://example.com/test",
      stringsAsFactors = FALSE
    ),
    shouldUpdateApp = function(...) TRUE
  )

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  target <- deploymentTarget(app_dir, appName = "my_app")
  expect_equal(target$appId, 123)

  # TODO: Minimise when we switch to testhat::local_mocked_bindings
  # and can progressively modify the same environment
  mockr::local_mock(
    accounts = fakeAccounts("ron", "bar"),
    applications = function(...) data.frame(
      name = "my_app",
      id = 123,
      url = "http://example.com/test",
      stringsAsFactors = FALSE
    ),
    shouldUpdateApp = function(...) FALSE
  )

  target <- deploymentTarget(app_dir, appName = "my_app")
  expect_equal(target$appName, "my_app-1")
  expect_equal(target$appId, NULL)
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


# helpers -----------------------------------------------------------------

test_that("shouldUpdateApp errors when non-interactive", {
  app <- list(name = "my_app", url = "https://example.com")

  expect_snapshot(shouldUpdateApp(app, "my_app-1"), error = TRUE)
})

test_that("forceUpdate shortcircuits shouldUpdateApp", {
  expect_true(shouldUpdateApp(forceUpdate = TRUE))
})

test_that("shouldUpdateApp handles 3 options", {
  withr::local_options(
    rlang_interactive = TRUE,
    cli_prompt = c("1", "2", "3")
  )
  app <- list(name = "my_app", url = "https://example.com")

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
