test_that("deployments() works empty data frame if no deployments", {
  dir <- local_temp_app()
  out <- deployments(dir)
  expect_s3_class(out, "data.frame")
  expect_named(out, c(deploymentFields, "deploymentFile"))
  expect_equal(nrow(out), 0)
})

test_that("combines fields across deployments", {
  local_temp_config()
  dir <- local_temp_app()

  writeDeploymentRecord(
    list(x = 1),
    deploymentConfigFile(dir, "app1", "account", "server")
  )
  writeDeploymentRecord(
    list(y = 1),
    deploymentConfigFile(dir, "app2", "account", "server")
  )

  out <- deployments(dir, excludeOrphaned = FALSE)
  expect_s3_class(out, "data.frame")
  expect_named(out, c(deploymentFields, "x", "y", "deploymentFile"))
  expect_equal(nrow(out), 2)
})

test_that("deployments() can filter", {
  local_temp_config()
  addTestServer("foo")
  addTestServer("bar")
  addTestAccount("foo", "foo")
  addTestAccount("bar", "bar")
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
    hostUrl = NULL
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
    hostUrl = NULL
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
  local_temp_config()
  addTestServer("bar")
  addTestAccount("foo", "bar")

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
    hostUrl = NULL
  )
  out <- deployments(dir)
  expect_equal(nrow(out), 0)

  out <- deployments(dir, excludeOrphaned = FALSE)
  expect_equal(nrow(out), 1)
})

test_that("can read/write metadata", {
  local_temp_config()
  addTestServer("bar")
  addTestAccount("foo", "bar")
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
    hostUrl = NULL,
    metadata = list(meta1 = "one", meta2 = "two")
  )
  out <- deployments(dir, excludeOrphaned = FALSE)
  expect_equal(out$meta1, "one")
  expect_equal(out$meta2, "two")
})

test_that("saveDeployment appends to global history", {
  local_temp_config()
  addTestServer("foo")
  addTestAccount("bar", "foo")

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
    hostUrl = NULL
  )

  history <- read.dcf(deploymentHistoryPath())
  expect_equal(nrow(history), 1)
  expect_setequal(colnames(history), c(deploymentFields, "appPath"))
})

test_that("addToDeploymentHistory() adds needed new lines", {
  local_temp_config()

  expect_snapshot({
    addToDeploymentHistory("path", list(x = 1))
    writeLines(readLines(deploymentHistoryPath()))
    addToDeploymentHistory("path", list(x = 2))
    writeLines(readLines(deploymentHistoryPath()))
  })
})
