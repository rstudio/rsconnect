test_that("errors if no accounts", {
  mockr::local_mock(accounts = fake_accounts(character(), character()))

  expect_snapshot(deploymentTarget(), error = TRUE)
})

test_that("errors if unknown server", {
  # TODO: give better error for unknown server
  expect_snapshot(deploymentTarget(server = "baz"), error = TRUE)
})

test_that("errors if bad account", {
  mockr::local_mock(accounts = fake_accounts("ron", "bar"))

  expect_snapshot(error = TRUE, {
    deploymentTarget(server = NULL, account = "john")
  })
})

test_that("fully specified app stitches includes data from account", {
  mockr::local_mock(
    accounts = fake_accounts("ron", "bar"),
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
  mockr::local_mock(accounts = fake_accounts("ron", c("foo1", "foo2")))

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  expect_snapshot(deployApp(app_dir, appName = "test"), error = TRUE)
})

test_that("errors if multiple deployments", {
  mockr::local_mock(accounts = fake_accounts("ron", c("foo1", "foo2")))

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

  expect_snapshot(deploymentTarget(app_dir, appName = "test"), error = TRUE)
})
