test_that("errors if no accounts", {
  mockr::local_mock(accounts = fakeAccounts(character(), character()))

  expect_snapshot(deploymentTarget(), error = TRUE)
})

test_that("errors if unknown server", {
  # TODO(HW): give better error for unknown server
  expect_snapshot(deploymentTarget(server = "baz"), error = TRUE)
})

test_that("errors if bad account", {
  mockr::local_mock(accounts = fakeAccounts("ron", "bar"))

  expect_snapshot(error = TRUE, {
    deploymentTarget(server = NULL, account = "john")
  })
})

test_that("succeeds if app is fully specified", {
  mockr::local_mock(
    accounts = fakeAccounts("ron", "bar"),
    accountInfo = fakeAccountInfo(ron = list(
      username = "foo",
      server = "test"
    ))
  )

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
  expect_equal(target$username, "foo")
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
    accountInfo = fakeAccountInfo(ron = list(
      username = "foo",
      server = "test"
    ))
  )

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  expect_snapshot(deploymentTarget(app_dir, server = "foo"), error = TRUE)

  target <- deploymentTarget(app_dir, server = "foo", account = "ron", appId = "123")
  expect_equal(target$username, "foo")
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
  mockr::local_mock(
    accounts = fakeAccounts("ron", "bar"),
    accountInfo = fakeAccountInfo(ron = list(
      username = "foo",
      server = "test"
    ))
  )

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

  target <- deploymentTarget(app_dir, appId = "123")
  expect_equal(target$appId, "123")
  expect_equal(target$appTitle, "my title")

  target <- deploymentTarget(app_dir, appName = "test", appId = "123")
  expect_equal(target$appId, "123")
  # TODO(HW): why doesn't this equal my title?
  # expect_equal(target$appTitle, "my title")
})

test_that("succeeds if there are no deployments and a single account", {
  mockr::local_mock(
    accounts = fakeAccounts("ron", "bar"),
    accountInfo = fakeAccountInfo(ron = list(
      username = "ron_smith",
      server = "test"
    ))
  )

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  target <- deploymentTarget(app_dir, appId = "123")
  expect_equal(target$username, "ron_smith")

  target <- deploymentTarget(app_dir, appName = "foo", appId = "123")
  expect_equal(target$username, "ron_smith")
})
