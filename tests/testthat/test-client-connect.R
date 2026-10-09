test_that("leading timestamps are stripped", {
  expect_snapshot(
    stripConnectTimestamps(
      c(
        "2024/04/24 13:08:04.901698921 [rsc-session] Content GUID: 3bfbd98a-6d6d-41bd-a15f-cab52025742f",
        "2024/04/24 13:08:04.901734307 [rsc-session] Content ID: 43888",
        "2024/04/24 13:08:04.901742487 [rsc-session] Bundle ID: 94502",
        "2024/04/24 13:08:04.901747536 [rsc-session] Variant ID: 6465"
      )
    )
  )
})

test_that("non-leading timestamps remain", {
  expect_snapshot(
    stripConnectTimestamps(
      c(
        "this message has a timestamp 2024/04/24 13:08:04.901698921 within a line"
      )
    )
  )
})

test_that("messages without recognized timestamps are unmodified", {
  expect_snapshot(
    stripConnectTimestamps(
      c(
        "this message has no timestamp",
        "2024/04/24 13:08 this message timestamp has a different format"
      )
    )
  )
})

test_that("waitForTask", {
  skip_if_not_installed("webfakes")

  task_app <- webfakes::new_app()
  task_app$use(webfakes::mw_json())
  task_app$get("/v1/tasks/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = I(req$params$id),
        user_id = I(42),
        output = c(
          "2024/04/24 13:08:04.901698921 [rsc-session] Content GUID: 3bfbd98a-6d6d-41bd-a15f-cab52025742f",
          "2024/04/24 13:08:04.901734307 [rsc-session] Content ID: 43888",
          "2024/04/24 13:08:04.901742487 [rsc-session] Bundle ID: 94502",
          "2024/04/24 13:08:04.901747536 [rsc-session] Variant ID: 6465"
        ),
        result = NULL,
        finished = TRUE,
        code = 0,
        error = "",
        last = 4
      ),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::new_app_process(task_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    secret = NULL,
    private_key = NULL,
    apiKey = "the-api-key",
    protocol = "https",
    certificate = NULL
  )
  client <- connectClient(service, authInfo)

  # task messages are logged when not quiet.
  expect_snapshot(invisible(connectWaitForTask(client, 101, quiet = FALSE)))
  # task messages are not logged when quiet.
  expect_snapshot(invisible(connectWaitForTask(client, 42, quiet = TRUE)))
})

test_that("getApplication() fills in dashboard_url from guid when missing", {
  skip_if_not_installed("webfakes")

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$get("/applications/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = I(req$params$id),
        guid = "3bfbd98a-6d6d-41bd-a15f-cab52025742f"
      ),
      auto_unbox = TRUE
    )
  })
  process <- webfakes::new_app_process(app)
  service <- parseHttpUrl(process$url())

  authInfo <- list(apiKey = "the-api-key")
  client <- connectClient(service, authInfo)

  result <- getApplication(client, 101)
  expect_equal(
    result$dashboard_url,
    connectDashboardUrl(
      buildHttpUrl(service),
      "3bfbd98a-6d6d-41bd-a15f-cab52025742f"
    )
  )
})

# Tests for Snowflake authentication with auto-detection

test_that("getDefaultSnowflakeConnectionName auto-detects matching default connection", {
  local_mocked_bindings(
    snowflake_connection = function(name = NULL) {
      expect_null(name)
      list(
        account = "org-account",
        name = "default"
      )
    },
    .package = "snowflakeauth"
  )

  result <- getDefaultSnowflakeConnectionName(
    "https://prefix-org-account.snowflakecomputing.app/__api__"
  )

  expect_equal(result, "default")
})

test_that("getDefaultSnowflakeConnectionName normalizes underscores to hyphens", {
  local_mocked_bindings(
    snowflake_connection = function(name = NULL) {
      expect_null(name)
      list(
        account = "org_account",
        name = "default"
      )
    },
    .package = "snowflakeauth"
  )

  result <- getDefaultSnowflakeConnectionName(
    "https://prefix-org-account.snowflakecomputing.app/__api__"
  )

  expect_equal(result, "default")
})

test_that("getDefaultSnowflakeConnectionName errors when default connection doesn't match server", {
  local_mocked_bindings(
    snowflake_connection = function(name = NULL) {
      list(
        account = "different-xyz789",
        name = "default"
      )
    },
    .package = "snowflakeauth"
  )

  expect_snapshot(
    getDefaultSnowflakeConnectionName(
      "https://prefix-org-account.snowflakecomputing.app/__api__"
    ),
    error = TRUE
  )
})

test_that("getDefaultSnowflakeConnectionName errors when no default connection exists", {
  local_mocked_bindings(
    snowflake_connection = function(name = NULL) {
      stop("No default connection configured")
    },
    .package = "snowflakeauth"
  )

  expect_snapshot(
    getDefaultSnowflakeConnectionName(
      "https://prefix-org-account.snowflakecomputing.app/__api__"
    ),
    error = TRUE
  )
})

test_that("extractSnowflakeAccount handles various hostname formats", {
  # Non-privatelink SPCS format.
  expect_equal(
    extractSnowflakeAccount("prefix-org-account.snowflakecomputing.app"),
    "org-account"
  )
  # Privatelink format. The .privatelink suffix is part of the account name.
  expect_equal(
    extractSnowflakeAccount("prefix-org-account.privatelink.snowflake.app"),
    "org-account.privatelink"
  )
})


test_that("connectDashboardUrl builds the dashboard URL from a server URL and id", {
  expect_equal(
    connectDashboardUrl("https://connect.example.com/__api__", "42"),
    "https://connect.example.com/connect/#/apps/42"
  )
  expect_equal(
    connectDashboardUrl(
      "https://connect.example.com/__api__",
      "3bfbd98a-6d6d-41bd-a15f-cab52025742f"
    ),
    "https://connect.example.com/connect/#/apps/3bfbd98a-6d6d-41bd-a15f-cab52025742f"
  )
})

test_that("uploadBundle POSTs the bundle to the content guid and returns it", {
  skip_if_not_installed("webfakes")

  bundlePath <- withr::local_tempfile(fileext = ".tar.gz")
  writeLines("bundle contents", bundlePath)

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$post("/v1/content/:guid/bundles", function(req, res) {
    res$set_status(200L)$send_json(
      list(id = paste0("bundle-for-", req$params$guid)),
      auto_unbox = TRUE
    )
  })
  proc <- webfakes::local_app_process(app)
  service <- parseHttpUrl(proc$url())
  client <- connectClient(service, list(server = "example.com"))

  bundle <- uploadBundle(client, list(guid = "guid-1"), bundlePath)
  expect_equal(bundle$id, "bundle-for-guid-1")
})

test_that("createContent() POSTs the name and title to v1/content", {
  sent <- NULL
  local_mocked_bindings(
    POST_JSON = function(service, authInfo, path, json) {
      sent <<- list(path = path, json = json)
      list(
        id = "99",
        guid = "guid-99",
        content_url = "https://example.com/content/guid-99/",
        dashboard_url = "https://example.com/connect/#/apps/guid-99"
      )
    }
  )
  client <- connectClient(list(), list())

  application <- createContent(
    client,
    deployment = list(name = "my-app", title = "My App"),
    accountDetails = list(accountId = "1"),
    appMetadata = list(appMode = "shiny")
  )

  expect_equal(sent$path, "/v1/content")
  expect_equal(sent$json, list(name = "my-app", title = "My App"))
  expect_equal(
    application,
    list(
      id = "99",
      guid = "guid-99",
      url = "https://example.com/content/guid-99/",
      dashboard_url = "https://example.com/connect/#/apps/guid-99"
    )
  )
})

test_that("createContent() does not send an empty title", {
  sent <- NULL
  local_mocked_bindings(
    POST_JSON = function(service, authInfo, path, json) {
      sent <<- json
      list()
    }
  )
  client <- connectClient(list(), list())

  createContent(
    client,
    deployment = list(name = "my-app", title = ""),
    accountDetails = list(accountId = "1"),
    appMetadata = list(appMode = "shiny")
  )

  expect_equal(sent, list(name = "my-app"))
})

test_that("findContent() gets the application for the deployment record", {
  requested <- NULL
  local_mocked_bindings(
    getApplication.connectClient = function(
      client,
      applicationId
    ) {
      requested <<- applicationId
      list(id = applicationId, url = "https://example.com/content/42/")
    }
  )
  client <- fake_client("connectClient")

  application <- findContent(
    client,
    deployment = list(appId = "42", version = "1"),
    quiet = TRUE
  )

  expect_equal(requested, "42")
  expect_equal(application$id, "42")
})

test_that("prepareContent() sets the env vars of the deployment", {
  sent <- NULL
  local_mocked_bindings(
    connectSetEnvVars = function(client, guid, vars) {
      sent <<- list(guid = guid, vars = vars)
    }
  )
  client <- fake_client("connectClient")
  application <- list(id = "42", guid = "guid-42")

  result <- prepareContent(
    client,
    application,
    deployment = list(envVars = c("A", "B")),
    appMetadata = list(),
    appVisibility = NULL,
    isNewContent = FALSE,
    upload = TRUE,
    quiet = TRUE
  )

  expect_equal(sent, list(guid = "guid-42", vars = c("A", "B")))
  expect_equal(result, application)
})

test_that("prepareContent() does not set env vars when the deployment has none", {
  local_mocked_bindings(
    connectSetEnvVars = function(...) stop("setEnvVars() should not be called")
  )
  client <- fake_client("connectClient")

  expect_no_error(prepareContent(
    client,
    list(id = "42", guid = "guid-42"),
    deployment = list(envVars = NULL),
    appMetadata = list(),
    appVisibility = NULL,
    isNewContent = FALSE,
    upload = TRUE,
    quiet = TRUE
  ))
})

test_that("activateContent() deploys the bundle and waits for the task", {
  deployed <- NULL
  waited <- NULL
  local_mocked_bindings(
    connectDeployApplication = function(client, application, bundleId = NULL) {
      deployed <<- list(guid = application$guid, bundleId = bundleId)
      list(task_id = "task-1")
    },
    connectWaitForTask = function(client, taskId, quiet = FALSE) {
      waited <<- taskId
      list(finished = TRUE, code = 0)
    }
  )
  client <- fake_client("connectClient")
  application <- list(guid = "guid-42", url = "https://example.com/42/")

  result <- activateContent(
    client,
    application,
    bundle = list(id = "bundle-1"),
    quiet = TRUE
  )

  expect_equal(deployed, list(guid = "guid-42", bundleId = "bundle-1"))
  expect_equal(waited, "task-1")
  expect_equal(
    result,
    list(succeeded = TRUE, url = "https://example.com/42/", error = NULL)
  )
})

test_that("activateContent() reports a failed task", {
  local_mocked_bindings(
    connectDeployApplication = function(...) list(task_id = "task-1"),
    connectWaitForTask = function(...) list(code = 1, error = "Build failed")
  )
  client <- fake_client("connectClient")

  result <- activateContent(
    client,
    list(guid = "guid-42", url = "https://example.com/42/"),
    bundle = list(id = "bundle-1"),
    quiet = TRUE
  )

  expect_false(result$succeeded)
  expect_equal(result$error, "Build failed")
})

test_that("listApplications() filters by account and by name", {
  sent <- list()
  local_mocked_bindings(
    listApplicationsRequest = function(service, authInfo, path, query, ...) {
      sent[[length(sent) + 1]] <<- list(path = path, query = query)
      list()
    }
  )
  client <- connectClient(list(), list())

  listApplications(client, "1")
  listApplications(client, "1", filters = list(name = "my-app"))

  expect_equal(sent[[1]]$path, "/applications")
  expect_equal(sent[[1]]$query, "filter=account_id:1")
  expect_equal(sent[[2]]$query, "filter=account_id:1&filter=name:my-app")
})

test_that("currentUser() GETs the current user", {
  requested <- NULL
  local_mocked_bindings(
    GET = function(service, authInfo, path, ...) {
      requested <<- list(service = service, path = path)
      list(id = 1, username = "me")
    }
  )
  client <- connectClient(list(host = "connect.example.com"), list())

  user <- currentUser(client)

  expect_equal(requested$service, list(host = "connect.example.com"))
  expect_equal(requested$path, "/users/current")
  expect_equal(user, list(id = 1, username = "me"))
})
