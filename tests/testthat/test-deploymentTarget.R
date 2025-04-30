test_that("errors if no accounts", {
  local_temp_config()

  expect_snapshot(findDeploymentTarget(), error = TRUE)
})

test_that("errors if unknown account or server", {
  local_temp_config()
  addTestServer("bar")
  addTestAccount("foo", "bar")

  expect_snapshot(error = TRUE, {
    findDeploymentTarget(server = "unknown")
    findDeploymentTarget(account = "john")
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
    findDeploymentTarget(app_dir)
    findDeploymentTarget(app_dir, appName = "test")
  })
})

test_that("uses appId given a local deployment record; created by a local account", {
  # Demonstrates that the deployment record is sufficient without a call to
  # the remote server.
  local_temp_config()
  addTestServer("local")
  addTestAccount("leslie", "local")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "local-record",
    appId = "the-appid",
    account = "leslie",
    server = "local"
  )

  target <- findDeploymentTarget(app_dir, appId = "the-appid")
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "leslie")
  expect_equal(accountDetails$server, "local")
  expect_equal(deployment$appId, "the-appid")
  expect_equal(deployment$name, "local-record")
  expect_equal(deployment$username, "leslie")
  expect_equal(deployment$account, "leslie")
  expect_equal(deployment$server, "local")
})

test_that("uses appId given a local deployment record; created by a collaborator", {
  # Demonstrates that the target account does not need to be the account that
  # created the deployment record. The deployment record is sufficient without
  # a call to the remote server.
  local_temp_config()
  addTestServer("local")
  addTestAccount("leslie", "local")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "local-record",
    appId = "the-appid",
    account = "ron",
    server = "local"
  )

  target <- findDeploymentTarget(app_dir, appId = "the-appid")
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "leslie")
  expect_equal(accountDetails$server, "local")
  expect_equal(deployment$appId, "the-appid")
  expect_equal(deployment$name, "local-record")
  expect_equal(deployment$username, "ron")
  expect_equal(deployment$account, "ron")
  expect_equal(deployment$server, "local")
})

test_that("uses appId without local deployment record; created by local account", {
  local_temp_config()
  addTestServer("local")
  addTestAccount("leslie", "local")

  local_mocked_bindings(
    getApplication = function(...)
      data.frame(
        id = "the-appid",
        name = "remote-record",
        owner_username = "leslie",
        stringsAsFactors = FALSE
      )
  )

  app_dir <- withr::local_tempdir()

  target <- findDeploymentTarget(app_dir, appId = "the-appid")
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "leslie")
  expect_equal(accountDetails$server, "local")
  expect_equal(deployment$appId, "the-appid")
  expect_equal(deployment$name, "remote-record")
  expect_equal(deployment$username, "leslie")
  expect_equal(deployment$account, "leslie")
  expect_equal(deployment$server, "local")
})

test_that("uses appId without local deployment record; created by collaborator", {
  local_temp_config()
  addTestServer("local")
  addTestAccount("leslie", "local")

  app_dir <- withr::local_tempdir()

  local_mocked_bindings(
    getApplication = function(...)
      data.frame(
        id = "the-appid",
        name = "remote-record",
        owner_username = "ron",
        stringsAsFactors = FALSE
      )
  )

  target <- findDeploymentTarget(app_dir, appId = "the-appid")
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "leslie")
  expect_equal(accountDetails$server, "local")
  expect_equal(deployment$appId, "the-appid")
  expect_equal(deployment$name, "remote-record")
  expect_equal(deployment$username, "ron")
  # note: account+server does not correspond to the "ron" account, but this is
  # the best we can do, as we do not have the original deployment record.
  expect_equal(deployment$account, "leslie")
  expect_equal(deployment$server, "local")
})


test_that("handles accounts if only server specified", {
  local_temp_config()
  addTestServer("foo")
  addTestAccount("ron", "foo")
  addTestAccount("john", "foo")
  local_mocked_bindings(getAppByName = function(...) NULL)

  app_dir <- withr::local_tempdir()
  file.create(file.path(app_dir, "app.R"))

  expect_snapshot(findDeploymentTarget(app_dir, server = "foo"), error = TRUE)

  target <- findDeploymentTarget(
    app_dir,
    server = "foo",
    account = "ron"
  )
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "ron")
  expect_equal(accountDetails$server, "foo")
  expect_equal(deployment$username, "ron")
  expect_equal(deployment$account, "ron")
  expect_equal(deployment$server, "foo")
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
    findDeploymentTarget(app_dir, appName = "test")
    findDeploymentTarget(app_dir)
  })

  simulate_user_input(1)
  expect_snapshot(target <- findDeploymentTarget(app_dir))
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "ron")
  expect_equal(accountDetails$server, "server1.com")
  expect_equal(deployment$name, "test")
})

test_that("succeeds if there's a single existing deployment", {
  local_temp_config()
  addTestServer("example.com")
  addTestAccount("ron")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "test",
    appId = "1",
    username = "ron",
    account = "ron",
    server = "example.com",
    version = "999"
  )
  expect_equal(
    nrow(deployments(
      app_dir,
      accountFilter = "ron",
      serverFilter = "example.com"
    )),
    1
  )
  expect_equal(nrow(deployments(app_dir)), 1)

  target <- findDeploymentTarget(app_dir)
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "ron")
  expect_equal(accountDetails$server, "example.com")
  expect_equal(deployment$appId, "1")
  expect_equal(deployment$name, "test")
  expect_equal(deployment$username, "ron")
  expect_equal(deployment$account, "ron")
  expect_equal(deployment$server, "example.com")
  expect_equal(deployment$version, "999")

  target <- findDeploymentTarget(app_dir, appName = "test")
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "ron")
  expect_equal(accountDetails$server, "example.com")
  expect_equal(deployment$appId, "1")
  expect_equal(deployment$name, "test")
  expect_equal(deployment$username, "ron")
  expect_equal(deployment$account, "ron")
  expect_equal(deployment$server, "example.com")
  expect_equal(deployment$version, "999")
})

test_that("appId is used even when name does not match", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  app_dir <- withr::local_tempdir()
  addTestDeployment(app_dir, appName = "test", appId = "1", username = "ron")
  addTestDeployment(app_dir, appName = "second", appId = "2", username = "ron")

  target <- findDeploymentTarget(app_dir, appName = "mismatched", appId = "1")
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "ron")
  expect_equal(accountDetails$server, "example.com")
  expect_equal(deployment$appId, "1")
})

test_that("new title overrides existing title", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  app_dir <- withr::local_tempdir()
  addTestDeployment(app_dir, appTitle = "old title")

  target <- findDeploymentTarget(app_dir)
  deployment <- target$deployment
  expect_equal(deployment$title, "old title")

  target <- findDeploymentTarget(app_dir, appTitle = "new title")
  deployment <- target$deployment
  expect_equal(deployment$title, "new title")
})

test_that("new env vars overrides existing", {
  local_temp_config()
  app <- local_temp_app()
  addTestServer()
  addTestAccount()
  addTestDeployment(app, envVars = "TEST1")

  target <- findDeploymentTarget(app)
  deployment <- target$deployment
  expect_equal(deployment$envVars, "TEST1")

  target <- findDeploymentTarget(app, envVars = "TEST2")
  deployment <- target$deployment
  expect_equal(deployment$envVars, "TEST2")

  # And check that it works with vectors
  addTestDeployment(app, envVars = c("TEST1", "TEST2"))
  target <- findDeploymentTarget(app)
  deployment <- target$deployment
  expect_equal(deployment$envVars, c("TEST1", "TEST2"))

  target <- findDeploymentTarget(app, envVars = "TEST2")
  deployment <- target$deployment
  expect_equal(deployment$envVars, "TEST2")
})

test_that("empty character vector removes env vars", {
  local_temp_config()
  app <- local_temp_app()
  addTestServer()
  addTestAccount()
  addTestDeployment(app, envVars = "TEST1")

  target <- findDeploymentTarget(app, envVars = character())
  deployment <- target$deployment
  expect_equal(deployment$envVars, character())
})

test_that("succeeds if there are no deployments and a single account", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")
  local_mocked_bindings(
    getAppByName = function(...)
      data.frame(
        name = "remotename",
        url = "app-url",
        stringsAsFactors = FALSE
      )
  )

  app_dir <- dirCreate(file.path(withr::local_tempdir(), "my_app"))

  expect_snapshot(error = TRUE, {
    findDeploymentTarget(app_dir)
  })

  simulate_user_input(1)
  target <- findDeploymentTarget(app_dir)
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "ron")
  expect_equal(accountDetails$server, "example.com")
  expect_equal(deployment$name, "remotename")
  expect_equal(deployment$username, "ron")
  expect_equal(deployment$account, "ron")
  expect_equal(deployment$server, "example.com")

  target <- findDeploymentTarget(app_dir, forceUpdate = TRUE)
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "ron")
  expect_equal(accountDetails$server, "example.com")
  expect_equal(deployment$name, "remotename")
  expect_equal(deployment$username, "ron")
  expect_equal(deployment$account, "ron")
  expect_equal(deployment$server, "example.com")

  target <- findDeploymentTarget(
    app_dir,
    envVars = c("TEST1", "TEST2"),
    forceUpdate = TRUE
  )
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "ron")
  expect_equal(accountDetails$server, "example.com")
  expect_equal(deployment$name, "remotename")
  expect_equal(deployment$username, "ron")
  expect_equal(deployment$account, "ron")
  expect_equal(deployment$server, "example.com")
  expect_equal(deployment$envVars, c("TEST1", "TEST2"))

  target <- findDeploymentTarget(app_dir, appName = "foo", forceUpdate = TRUE)
  accountDetails <- target$accountDetails
  deployment <- target$deployment
  expect_equal(accountDetails$name, "ron")
  expect_equal(accountDetails$server, "example.com")
  expect_equal(deployment$name, "remotename")
  expect_equal(deployment$username, "ron")
  expect_equal(deployment$account, "ron")
  expect_equal(deployment$server, "example.com")
})

test_that("default title is the empty string", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")
  local_mocked_bindings(
    getAppByName = function(...)
      data.frame(
        name = "remotename",
        url = "app-url",
        stringsAsFactors = FALSE
      )
  )

  app_dir <- withr::local_tempdir()
  target <- findDeploymentTarget(app_dir, forceUpdate = TRUE)
  deployment <- target$deployment
  expect_equal(deployment$title, "")
})

confirm_existing_app_used <- function(server) {
  local_temp_config()
  addTestServer()
  addTestAccount("ron", server = server)
  local_mocked_bindings(
    getAppByName = function(...)
      data.frame(
        name = "my_app",
        id = 123,
        url = "http://example.com/test",
        stringsAsFactors = FALSE
      ),
    shouldUpdateApp = function(...) TRUE
  )

  app_dir <- withr::local_tempdir()
  target <- findDeploymentTarget(app_dir, appName = "my_app", server = server)
  deployment <- target$deployment
  expect_equal(deployment$appId, 123)
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
    getAppByName = function(...)
      data.frame(
        name = "my_app",
        id = 123,
        url = "http://example.com/test",
        stringsAsFactors = FALSE
      ),
    shouldUpdateApp = function(...) FALSE
  )

  app_dir <- withr::local_tempdir()
  target <- findDeploymentTarget(app_dir, appName = "my_app", server = server)
  deployment <- target$deployment
  expect_equal(deployment$name, "my_app-1")
  expect_equal(deployment$appId, NULL)
}

test_that("can find existing application on server & not use it", {
  confirm_existing_app_not_used("example.com")
})

test_that("can find existing application on shinyapps.io & not use it", {
  confirm_existing_app_not_used("shinyapps.io")
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

test_that("createDeploymentFromApplication promotes fields", {
  expect_equal(
    createDeploymentFromApplication(
      application = list(
        id = "1",
        name = "app-name",
        title = "app-title",
        owner_username = "alice.username"
      ),
      accountDetails = list(
        name = "alice",
        server = "example.com"
      )
    ),
    list(
      name = "app-name",
      title = "app-title",
      envVars = NULL,
      appId = "1",
      username = "alice.username",
      account = "alice",
      server = "example.com",
      version = deploymentRecordVersion
    )
  )
})

test_that("updateDeployment updates fields", {
  expect_equal(
    updateDeployment(
      list(
        name = "app-name",
        title = "app-title",
        envVars = NULL,
        appId = "1",
        username = "alice.username",
        account = "alice",
        server = "example.com",
        version = deploymentRecordVersion
      ),
      appTitle = "updated-title",
      envVars = c("VAR-NAME")
    ),
    list(
      name = "app-name",
      title = "updated-title",
      envVars = c("VAR-NAME"),
      appId = "1",
      username = "alice.username",
      account = "alice",
      server = "example.com",
      version = deploymentRecordVersion
    )
  )
})

test_that("updateDeployment ignores NULL updates", {
  expect_equal(
    updateDeployment(
      list(
        name = "app-name",
        title = "app-title",
        envVars = c("VAR-NAME"),
        appId = "1",
        username = "alice.username",
        account = "alice",
        server = "example.com",
        version = deploymentRecordVersion
      ),
      appTitle = NULL,
      envVars = NULL
    ),
    list(
      name = "app-name",
      title = "app-title",
      envVars = c("VAR-NAME"),
      appId = "1",
      username = "alice.username",
      account = "alice",
      server = "example.com",
      version = deploymentRecordVersion
    )
  )
})
