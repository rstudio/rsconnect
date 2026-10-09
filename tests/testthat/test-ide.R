test_that("validateServerUrl() when not Connect", {
  skip_on_cran()
  skip_if_not_installed("webfakes")

  service <- service_settings_404()

  url <- buildHttpUrl(service)
  expect_false(validateServerUrl(url)$valid)
})

test_that("validateServerUrl() when Connect", {
  skip_on_cran()
  skip_if_not_installed("webfakes")

  service <- service_settings_200()
  url <- buildHttpUrl(service)
  expected_url <- paste0(url, "__api__")

  redirect <- service_redirect(paste0(url, "__api__/server_settings"))
  redirect_url <- buildHttpUrl(redirect)

  # Full server URL.
  result <- validateServerUrl(url)
  expect_true(result$valid, info = url)
  expect_equal(result$url, expected_url, info = url)

  # Overspecified (includes /__api__)
  result <- validateServerUrl(expected_url)
  expect_true(result$valid, info = expected_url)
  expect_equal(result$url, expected_url, info = expected_url)

  # Incomplete (lacks path).
  # Lack of protocol is not easily tested because validateConnectUrl()
  # prefers https://.
  partial_url <- paste0(
    service$protocol,
    "://",
    service$host,
    ":",
    service$port
  )
  result <- validateServerUrl(partial_url)
  expect_true(result$valid, info = partial_url)
  expect_equal(result$url, expected_url, info = partial_url)

  # Redirects
  result <- validateServerUrl(redirect_url)
  expect_true(result$valid)
  expect_equal(result$url, expected_url)
})

test_that("validateServerUrl() hosted", {
  skip_on_cran()

  expect_false(validateServerUrl("https://shinyapps.io")$valid)
})

test_that("getAppById() fails where expected", {
  local_temp_config()
  addTestServer()
  addTestAccount("susan")

  expect_snapshot(error = TRUE, {
    getAppById("123", "susan", "unknown", "unknown.com")
    getAppById("123", "robert", "unknown", "https://example.com")
  })
})

current_user_service <- function() {
  app <- env_cache(
    cache,
    "current_user_app",
    {
      json_app <- webfakes::new_app()
      json_app$use(webfakes::mw_json())
      json_app$get("/users/current", function(req, res) {
        res$set_status(200L)$send_json(list(
          username = jsonlite::unbox("susan")
        ))
      })
      app <- webfakes::new_app_process(json_app)
    }
  )
  parseHttpUrl(app$url())
}

test_that("getUserFromRawToken having a single matching server", {
  skip_if_not_installed("webfakes")

  local_temp_config()

  service <- current_user_service()
  url <- buildHttpUrl(service)

  addTestServer("test", url = url)

  token <- generateToken()
  claimUrl <- url

  user <- getUserFromRawToken(claimUrl, token$token, token$private_key)
  expect_equal(user$username, "susan")
})

test_that("getUserFromRawToken having multiple matching servers", {
  skip_if_not_installed("webfakes")

  local_temp_config()

  service <- current_user_service()
  url <- buildHttpUrl(service)

  addTestServer("test", url = url)
  addTestServer("test2", url = url)

  token <- generateToken()
  claimUrl <- url

  user <- getUserFromRawToken(claimUrl, token$token, token$private_key)
  expect_equal(user$username, "susan")
})

test_that("registerUserToken registers a user token", {
  # Checks the arguments as supplied by RStudio as of
  # https://github.com/rstudio/rstudio/blob/320fff61d4205104d7c42219e0449d1ae35a78b0/src/cpp/session/modules/SessionRSConnect.R#L302-L303
  local_temp_config()

  registerUserToken(
    serverName = "the-server",
    accountName = "the-account",
    userId = 42,
    token = "the-token",
    privateKey = "the-private-key"
  )

  expect_snapshot({
    accountInfo("the-account")
  })
})

test_that("the RStudio IDE can get env vars from a Connect client", {
  # Matches the calls in rsconnect.getApplicationEnvVarsImpl in
  # https://github.com/rstudio/rstudio/blob/main/src/cpp/session/modules/SessionRSConnect.R
  local_temp_config()
  addTestServer()
  addTestAccount("susan")
  requested <- NULL
  local_mocked_bindings(
    connectGetEnvVars = function(client, guid) {
      requested <<- list(class = class(client), guid = guid)
      c("A", "B")
    }
  )

  accountDetails <- accountInfo("susan", "example.com")
  client <- clientForAccount(accountDetails)

  expect_equal(client$getEnvVars("guid-1"), c("A", "B"))
  expect_equal(
    requested,
    list(class = c("connectClient", "rsconnectClient"), guid = "guid-1")
  )
})

test_that("the RStudio IDE can get content from a Connect Cloud client", {
  # Matches the calls in rsconnect.getConnectCloudContent in
  # https://github.com/rstudio/rstudio/blob/main/src/cpp/session/modules/SessionRSConnect.R
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("susan", server = "connect.posit.cloud")
  requested <- NULL
  local_mocked_bindings(
    connectCloudGetContent = function(client, contentId) {
      requested <<- list(class = class(client), contentId = contentId)
      list(id = contentId)
    }
  )

  accountDetails <- accountInfo("susan", "connect.posit.cloud")
  client <- clientForAccount(accountDetails)

  expect_equal(client$getContent("content-1"), list(id = "content-1"))
  expect_equal(
    requested,
    list(
      class = c("connectCloudClient", "rsconnectClient"),
      contentId = "content-1"
    )
  )
})
