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
  addTestDeployment(dir, appName = "my-app", account = "foo", server = "foo")
  addTestDeployment(dir, appName = "my-app2", account = "bar", server = "bar")

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

test_that("deployments() can exclude orphans", {
  dir <- local_temp_app()
  addTestDeployment(dir, server = "bar1")

  out <- deployments(dir)
  expect_equal(nrow(out), 0)

  out <- deployments(dir, excludeOrphaned = FALSE)
  expect_equal(nrow(out), 1)
})

test_that("can read/write metadata", {
  dir <- local_temp_app()

  addTestDeployment(dir, metadata = list(meta1 = "one", meta2 = "two"))
  out <- deployments(dir, excludeOrphaned = FALSE)
  expect_equal(out$meta1, "one")
  expect_equal(out$meta2, "two")
})

test_that("can read/write version", {
  dir <- local_temp_app()

  addTestDeployment(dir, version = "999")
  out <- deployments(dir, excludeOrphaned = FALSE)
  expect_equal(out$version, "999")
})

test_that("can read/write missing version", {
  # also tests we can read files written by previous versions of package
  dir <- local_temp_app()

  path <- addTestDeployment(dir, version = NA)
  out <- deployments(dir, excludeOrphaned = FALSE)

  expect_false("version" %in% rownames(read.dcf(path)))
  expect_equal(out$version, NA)
})

test_that("can read/write env vars", {
  app <- local_temp_app()
  addTestDeployment(app, "test1", envVars = c("TEST1", "TEST2"))
  addTestDeployment(app, "test2")

  deps <- deployments(app, excludeOrphaned = FALSE)
  expect_equal(deps$envVars, list(c("TEST1", "TEST2"), character()))
})

test_that("can read/write empty env vars", {
  # also tests we can read files written by previous versions of package
  app <- local_temp_app()

  # With empty character vector
  path <- addTestDeployment(app, "test1", envVars = character())
  deps <- deployments(app, excludeOrphaned = FALSE)
  expect_false("envVars" %in% rownames(read.dcf(path)))
  expect_equal(deps$envVars, list(character()))

  # Or with empty string
  path <- addTestDeployment(app, "test1", envVars = "")
  deps <- deployments(app, excludeOrphaned = FALSE)
  expect_false("envVars" %in% rownames(read.dcf(path)))
  expect_equal(deps$envVars, list(character()))
})

test_that("can read/write env vars", {
  app <- local_temp_app()
  addTestDeployment(app, "test1", envVars = c("TEST1", "TEST2"))
  addTestDeployment(app, "test2")

  deps <- deployments(app, excludeOrphaned = FALSE)
  expect_equal(deps$envVars, list(c("TEST1", "TEST2"), character()))
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
      envVars = "abc", # ensure there's an envVars column in output
      account = "foo",
      username = "foo",
      server = "bar",
      version = 1
    ),
    application = list(id = 1),
    hostUrl = NULL
  )

  history <- read.dcf(deploymentHistoryPath())
  expect_equal(nrow(history), 1)
  expect_setequal(colnames(history), c(deploymentFields, "appPath"))
})

test_that("saveDeployment captures hostUrl", {
  local_temp_config()
  addTestServer()
  addTestAccount("foo")

  dir <- local_temp_app()
  saveDeployment(
    dir,
    createDeploymentTarget(
      appName = "my-app",
      appTitle = "",
      appId = 10,
      envVars = NULL,
      account = "foo",
      username = "foo",
      server = "example.com",
      version = 1
    ),
    application = list(id = 10)
  )

  out <- deployments(dir)
  expect_equal(out$hostUrl, "https://example.com")
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
