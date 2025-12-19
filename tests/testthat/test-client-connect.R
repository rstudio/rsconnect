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
  expect_snapshot(invisible(client$waitForTask(101, quiet = FALSE)))
  # task messages are not logged when quiet.
  expect_snapshot(invisible(client$waitForTask(42, quiet = TRUE)))
})

# NOTE: These tests expect that you're already running connect; the tests
# will speak to that running connect process (if it can find it)
findConnect <- function() {
  connect <- Sys.which("connect")
  if (connect == "") {
    possibleLocs <- c(
      "~/git/connect/bin"
    )
    for (loc in possibleLocs) {
      if (file.exists(file.path(loc, "connect"))) {
        return(normalizePath(file.path(loc, "connect")))
      }
    }
    stop("Couldn't find an appropriate 'connect' binary")
  }
}

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
