test_that("deployments() works empty data frame if no deployments", {
  dir <- local_temp_app()
  out <- deployments(dir)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
})

test_that("deployments() can filter", {
  mockr::local_mock(accounts = fakeAccounts(c("foo", "bar"), c("foo", "bar")))
  dir <- local_temp_app()
  saveDeployment(
    dir,
    createDeploymentTarget(
      appName = "my-app",
      appTitle = "",
      appId = 10,
      account = "foo",
      username = "foo",
      server = "foo"
    ),
    application = list(),
    hostUrl = NA
  )
  saveDeployment(
    dir,
    createDeploymentTarget(
      appName = "my-app2",
      appTitle = "",
      appId = 10,
      account = "bar",
      username = "bar",
      server = "bar"
    ),
    application = list(id = "123"),
    hostUrl = NA
  )

  out <- deployments(dir)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)

  out <- deployments(dir, nameFilter = "my-app")
  expect_equal(nrow(out), 1)

  out <- deployments(dir, serverFilter = "foo")
  expect_equal(nrow(out), 1)

  out <- deployments(dir, accountFilter = "foo")
  expect_equal(nrow(out), 1)

})

test_that("deployments() can excludes orphans", {
  mockr::local_mock(accounts = fakeAccounts("foo", "bar"))
  dir <- local_temp_app()
  saveDeployment(
    dir,
    createDeploymentTarget(
      appName = "my-app",
      appTitle = "",
      appId = 10,
      account = "foo1",
      username = "foo1",
      server = "bar1"
    ),
    application = list(),
    hostUrl = NA
  )
  out <- deployments(dir)
  expect_equal(nrow(out), 0)

  out <- deployments(dir, excludeOrphaned = FALSE)
  expect_equal(nrow(out), 1)
})

test_that("can read/write metadata", {
  mockr::local_mock(accounts = fakeAccounts("foo", "bar"))
  dir <- local_temp_app()
  saveDeployment(
    dir,
    createDeploymentTarget(
      appName = "my-app",
      appTitle = "",
      appId = 10,
      account = "foo",
      username = "foo",
      server = "bar"
    ),
    application = list(),
    hostUrl = NA,
    metadata = list(meta1 = "one", meta2 = "two")
  )
  out <- deployments(dir, excludeOrphaned = FALSE)
  expect_equal(out$meta1, "one")
  expect_equal(out$meta2, "two")
})
