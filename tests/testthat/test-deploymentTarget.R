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

test_that("succeeds if app is fully specified", {
  mockr::local_mock(accounts = fakeAccounts("ron", "bar"))

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  target <- deploymentTarget(
    app_dir,
    appName = "test",
    appTitle = "mytitle",
    appId = "123",
    account = "ron"
  )
  expect_equal(target$appId, "123")
  expect_equal(target$username, "ron")
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
  mockr::local_mock(accounts = fakeAccounts(c("ron", "john"), "foo"))

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  expect_snapshot(deploymentTarget(app_dir, server = "foo"), error = TRUE)

  target <- deploymentTarget(app_dir, server = "foo", account = "ron")
  expect_equal(target$username, "ron")
})

test_that("errors if multiple deployments", {
  mockr::local_mock(accounts = fakeAccounts("ron", c("foo1", "foo2")))

  app_dir <- withr::local_tempdir()
  saveDeployment(
    app_dir,
    name = "test",
    title = "",
    username = "ron",
    account = "ron",
    server = "foo1",
    hostUrl = "",
    appId = "123",
    bundleId = "abc",
    url = "http://example.com"
  )
  saveDeployment(
    app_dir,
    name = "test",
    title = "",
    username = "ron",
    account = "ron",
    server = "foo2",
    hostUrl = "",
    appId = "123",
    bundleId = "abc",
    url = "http://example.com"
  )

  expect_snapshot(error = TRUE, {
    deploymentTarget(app_dir, appName = "test")
    deploymentTarget(app_dir)
  })
})

test_that("succeeds if there's a single existing deployment", {
  mockr::local_mock(accounts = fakeAccounts("ron", "bar"))

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))
  saveDeployment(
    app_dir,
    name = "test",
    title = "my title",
    username = "ron",
    account = "ron",
    server = "bar",
    hostUrl = "",
    appId = "123",
    bundleId = "abc",
    url = "http://example.com"
  )

  target <- deploymentTarget(app_dir)
  expect_equal(target$appId, "123")
  expect_equal(target$username, "ron")
  expect_equal(target$appTitle, "my title")

  target <- deploymentTarget(app_dir, appName = "test")
  expect_equal(target$appId, "123")
  expect_equal(target$username, "ron")
  expect_equal(target$appTitle, "my title")
})

test_that("new title overrides existing title", {
  mockr::local_mock(accounts = fakeAccounts("ron", "bar"))

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))
  saveDeployment(
    app_dir,
    name = "test",
    title = "old title",
    username = "ron",
    account = "ron",
    server = "bar",
    hostUrl = "",
    appId = "123",
    bundleId = "abc",
    url = "http://example.com"
  )

  target <- deploymentTarget(app_dir)
  expect_equal(target$appTitle, "old title")

  target <- deploymentTarget(app_dir, appTitle = "new title")
  expect_equal(target$appTitle, "new title")
})

test_that("succeeds if there are no deployments and a single account", {
  mockr::local_mock(accounts = fakeAccounts("ron", "bar"))

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  target <- deploymentTarget(app_dir)
  expect_equal(target$username, "ron")

  target <- deploymentTarget(app_dir, appName = "foo")
  expect_equal(target$username, "ron")
})
