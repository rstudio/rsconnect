test_that("syncAppMetadata updates deployment records", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  app <- local_temp_app()
  addTestDeployment(app, appId = "123", metadata = list(when = 123))
  local_mocked_bindings(
    getApplication.connectClient = function(...) {
      list(title = "newtitle", url = "newurl")
    }
  )
  local_mocked_bindings(clientForAccount = function(...) {
    fake_client("connectClient")
  })

  syncAppMetadata(app)
  deps <- deployments(app)
  expect_equal(deps$title, "newtitle")
  expect_equal(deps$url, "newurl")
  expect_equal(deps$when, NULL)
})

test_that("syncAppMetadata deletes deployment records if needed", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  app <- local_temp_app()
  addTestDeployment(app, appId = "123", metadata = list(when = 123))
  local_mocked_bindings(
    getApplication.connectClient = function(...) {
      abort(class = "rsconnect_http_404")
    }
  )
  local_mocked_bindings(clientForAccount = function(...) {
    fake_client("connectClient")
  })

  expect_snapshot(syncAppMetadata(app))
  expect_equal(nrow(deployments(app)), 0)
})

test_that("syncAppMetadata skips Connect Cloud deployment records", {
  local_temp_config()
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app <- local_temp_app()
  addTestDeployment(
    app,
    appId = "123",
    account = "myaccount",
    server = "connect.posit.cloud",
    metadata = list(when = 123)
  )
  local_mocked_bindings(
    getApplication.connectCloudClient = function(...) {
      stop("getApplication should not be called")
    }
  )
  local_mocked_bindings(clientForAccount = function(...) {
    fake_client("connectCloudClient")
  })

  syncAppMetadata(app)
  expect_equal(deployments(app)$when, "123")
})

test_that("applications() builds config_url for standard Connect accounts", {
  local_temp_config()
  addTestServer(url = "https://connect.example.com")
  addTestAccount("ron", server = "connect.example.com")

  local_mocked_bindings(
    listApplications.connectClient = function(client, accountId, ...) {
      list(list(
        id = "123",
        name = "myapp",
        title = "My App",
        url = "https://connect.example.com/content/123/",
        build_status = "ready",
        created_time = "2024-01-01T00:00:00Z",
        last_deployed_time = "2024-01-02T00:00:00Z",
        guid = "3bfbd98a-6d6d-41bd-a15f-cab52025742f"
      ))
    }
  )
  local_mocked_bindings(clientForAccount = function(...) {
    fake_client("connectClient")
  })

  result <- applications(account = "ron", server = "connect.example.com")
  expect_equal(
    result$config_url,
    "https://connect.example.com/connect/#/apps/123"
  )
})

test_that("applications() returns all columns for Connect accounts", {
  local_temp_config()
  addTestServer(url = "https://connect.example.com")
  addTestAccount("ron", server = "connect.example.com")

  local_mocked_bindings(
    listApplications.connectClient = function(client, accountId, ...) {
      list(list(
        id = "123",
        name = "myapp",
        title = "My App",
        url = "https://connect.example.com/content/123/",
        build_status = "ready",
        created_time = "2024-01-01T00:00:00Z",
        last_deployed_time = "2024-01-02T00:00:00Z",
        guid = "3bfbd98a-6d6d-41bd-a15f-cab52025742f",
        owner_guid = "not-kept"
      ))
    }
  )
  local_mocked_bindings(clientForAccount = function(...) {
    fake_client("connectClient")
  })

  result <- applications(account = "ron", server = "connect.example.com")
  expect_equal(
    result,
    data.frame(
      id = "123",
      name = "myapp",
      title = "My App",
      url = "https://connect.example.com/content/123/",
      status = "ready",
      created_time = "2024-01-01T00:00:00Z",
      updated_time = "2024-01-02T00:00:00Z",
      guid = "3bfbd98a-6d6d-41bd-a15f-cab52025742f",
      size = NA,
      instances = NA,
      config_url = "https://connect.example.com/connect/#/apps/123",
      stringsAsFactors = FALSE
    )
  )
})

test_that("applications() returns all columns for shinyapps.io accounts", {
  local_temp_config()
  addTestServer(url = "https://shinyapps.io", name = "shinyapps.io")
  addTestAccount("myaccount", server = "shinyapps.io")

  local_mocked_bindings(
    listApplications.shinyAppsClient = function(client, accountId, ...) {
      list(
        list(
          id = 456L,
          name = "myapp",
          url = "https://myaccount.shinyapps.io/myapp/",
          status = "running",
          created_time = "2024-01-01T00:00:00Z",
          updated_time = "2024-01-02T00:00:00Z",
          deployment = list(
            properties = list(
              application.instances.template = "large",
              application.instances.count = 2L
            )
          ),
          owner_id = "not-kept"
        ),
        list(
          id = 789L,
          name = "otherapp",
          url = "https://myaccount.shinyapps.io/otherapp/",
          status = "terminated",
          created_time = "2024-02-01T00:00:00Z",
          updated_time = "2024-02-02T00:00:00Z",
          deployment = list(properties = list())
        )
      )
    }
  )
  local_mocked_bindings(clientForAccount = function(...) {
    fake_client("shinyAppsClient")
  })

  result <- applications(account = "myaccount", server = "shinyapps.io")
  expect_equal(
    result,
    data.frame(
      id = c(456L, 789L),
      name = c("myapp", "otherapp"),
      url = c(
        "https://myaccount.shinyapps.io/myapp/",
        "https://myaccount.shinyapps.io/otherapp/"
      ),
      status = c("running", "terminated"),
      created_time = c("2024-01-01T00:00:00Z", "2024-02-01T00:00:00Z"),
      updated_time = c("2024-01-02T00:00:00Z", "2024-02-02T00:00:00Z"),
      size = c("large", NA),
      instances = c(2L, NA),
      guid = NA,
      title = NA_character_,
      config_url = c(
        "https://www.shinyapps.io/admin/#/application/456",
        "https://www.shinyapps.io/admin/#/application/789"
      ),
      stringsAsFactors = FALSE
    )
  )
})

test_that("applications() returns a data frame for PCC accounts", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  # Local alias "myaccount" intentionally differs from the server-side slug
  # "real-slug" to confirm that url/config_url use the resolved slug, not the alias.
  # userId is passed as accountId in registerAccount() via addTestAccount().
  addTestAccount("myaccount", server = "connect.posit.cloud", userId = "acct-1")

  local_mocked_bindings(
    listApplications.connectCloudClient = function(client, accountId, ...) {
      # GET /contents embeds the current revision, whose `url` is the served
      # (vanity/custom) URL of the published content.
      list(list(
        id = "abc-123",
        title = "My App",
        account_id = "acct-1",
        created_time = "2024-01-01T00:00:00Z",
        updated_time = "2024-01-02T00:00:00Z",
        current_revision = list(
          url = "https://my-app.share.connect.posit.cloud/"
        )
      ))
    }
  )
  local_mocked_bindings(clientForAccount = function(...) {
    fake_client("connectCloudClient", getAccounts = function() {
      list(data = list(list(id = "acct-1", name = "real-slug")))
    })
  })

  result <- applications(account = "myaccount", server = "connect.posit.cloud")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$title, "My App")
  # url = the revision's served URL; config_url = the settings page (built from
  # the server-side slug, not the local alias).
  expect_equal(
    result$url,
    "https://my-app.share.connect.posit.cloud/"
  )
  expect_equal(
    result$config_url,
    "https://connect.posit.cloud/real-slug/content/abc-123/settings/info"
  )
})

test_that("applications() falls back to the constructed url when content is unpublished", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud", userId = "acct-1")

  local_mocked_bindings(
    # No current_revision for content that has never published successfully;
    # url falls back to the constructed content-id URL.
    listApplications.connectCloudClient = function(client, accountId, ...) {
      list(list(
        id = "abc-123",
        title = "My App",
        account_id = "acct-1"
      ))
    }
  )
  local_mocked_bindings(clientForAccount = function(...) {
    fake_client(
      "connectCloudClient",
      getAccounts = function() {
        list(data = list(list(id = "acct-1", name = "real-slug")))
      }
    )
  })

  result <- applications(account = "myaccount", server = "connect.posit.cloud")
  expect_equal(
    result$url,
    "https://abc-123.share.connect.posit.cloud/"
  )
})

test_that("applications() returns empty data frame for PCC account with no content", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  local_mocked_bindings(
    listApplications.connectCloudClient = function(...) list()
  )
  local_mocked_bindings(clientForAccount = function(...) {
    fake_client("connectCloudClient")
  })

  result <- applications(account = "myaccount", server = "connect.posit.cloud")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(
    result,
    c(
      "id",
      "name",
      "title",
      "url",
      "status",
      "size",
      "instances",
      "config_url",
      "created_time",
      "updated_time",
      "guid"
    )
  )
})

test_that("showLogs() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()
  appDir <- local_temp_app()

  expect_error(
    showLogs(
      appPath = appDir,
      appName = "myapp",
      account = "connect-user",
      server = "connect-server"
    ),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    showLogs(
      appPath = appDir,
      appName = "myapp",
      account = "cloud-user",
      server = "connect.posit.cloud"
    ),
    regexp = "`server` must be shinyapps\\.io"
  )
})

test_that("getLogs() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()
  appDir <- local_temp_app()

  expect_error(
    getLogs(
      appPath = appDir,
      appName = "myapp",
      account = "connect-user",
      server = "connect-server"
    ),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    getLogs(
      appPath = appDir,
      appName = "myapp",
      account = "cloud-user",
      server = "connect.posit.cloud"
    ),
    regexp = "`server` must be shinyapps\\.io"
  )
})
