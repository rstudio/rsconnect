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

test_that("the RStudio IDE can get an app by id", {
  local_temp_config()
  addTestServer()
  addTestAccount("susan")
  requested <- NULL
  local_mocked_bindings(
    getApplication = function(client, applicationId) {
      requested <<- list(class = class(client), applicationId = applicationId)
      list(id = applicationId)
    }
  )

  app <- getAppById("123", "susan", "example.com", "https://example.com")

  expect_equal(app, list(id = "123"))
  expect_equal(
    requested,
    list(class = c("connectClient", "rsconnectClient"), applicationId = "123")
  )
})

test_that("the RStudio IDE can get an app by id with an unknown server name", {
  local_temp_config()
  addTestServer()
  addTestAccount("susan")
  local_mocked_bindings(
    getApplication = function(client, applicationId) list(id = applicationId)
  )

  app <- getAppById("123", "susan", "unknown", "https://example.com")

  expect_equal(app, list(id = "123"))
})

test_that("getUserFromRawToken having a single matching server", {
  skip_if_not_installed("webfakes")

  local_temp_config()

  service <- current_user_service()
  url <- buildHttpUrl(service)

  addTestServer("test", url = url)

  token <- generateToken()
  claimUrl <- url

  user <- getUserFromRawToken(
    serverUrl = claimUrl,
    token = token$token,
    privateKey = token$private_key
  )
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

  user <- getUserFromRawToken(
    serverUrl = claimUrl,
    token = token$token,
    privateKey = token$private_key
  )
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

test_that("findAndRegisterLocalServer() returns the name of a known server", {
  local_temp_config()
  addTestServer("test", url = "https://example.com/__api__")

  expect_equal(
    findAndRegisterLocalServer("https://example.com/__api__"),
    "test"
  )
  expect_equal(nrow(servers(local = TRUE)), 1)
})

test_that("findAndRegisterLocalServer() registers an unknown server", {
  local_temp_config()

  expect_equal(findAndRegisterLocalServer("https://example.com"), "example.com")
  expect_equal(
    servers(local = TRUE)[c("name", "url")],
    data.frame(name = "example.com", url = "https://example.com/__api__")
  )
})

test_that("the RStudio IDE can show lint results as source markers", {
  # Matches the calls in the get_rsconnect_lint_results handler in
  # https://github.com/rstudio/rstudio/blob/main/src/cpp/session/modules/SessionRSConnect.R
  basePath <- test_path("test-rmd-bad-case")
  results <- lint(project = basePath)
  called <- NULL
  local_mocked_bindings(
    callFun = function(fname, ...) {
      called <<- list(fname = fname, ...)
    },
    .package = "rstudioapi"
  )

  showRstudioSourceMarkers(basePath = basePath, results)

  expect_equal(
    called,
    list(
      fname = "sourceMarkers",
      name = "Publish Content Issues",
      markers = list(list(
        type = "warning",
        file = file.path(basePath, "index.Rmd"),
        line = 29,
        column = 1,
        message = "Filepaths are case-sensitive on deployment server."
      )),
      basePath = basePath,
      autoSelect = "first"
    )
  )
})

test_that("the RStudio IDE can classify servers", {
  expect_true(isPositConnectCloudServer("connect.posit.cloud"))
  expect_false(isPositConnectCloudServer("shinyapps.io"))
  expect_false(isPositConnectCloudServer("example.com"))

  expect_true(isConnectServer("example.com"))
  expect_false(isConnectServer("shinyapps.io"))
  expect_false(isConnectServer("connect.posit.cloud"))
})

test_that("the RStudio IDE can get the Connect Cloud UI URL", {
  withr::local_options(rsconnect.connect_cloud_environment = NULL)

  expect_equal(connectCloudUrls()$ui, "https://connect.posit.cloud")
})

test_that("the RStudio IDE can check lint results for problems", {
  badApp <- test_path("test-rmd-bad-case")
  goodApp <- local_temp_app(list("app.R" = "shinyApp(ui, server)"))

  expect_true(hasLint(lint(project = badApp)))
  expect_false(hasLint(lint(project = goodApp)))
})

test_that("the RStudio IDE can get an auth token for a new token", {
  local_temp_config()
  addTestServer()
  requested <- NULL
  local_mocked_bindings(
    connectAddToken = function(client, token) {
      requested <<- list(class = class(client), user_id = token$user_id)
      list(token_claim_url = "https://example.com/claim")
    }
  )

  token <- getAuthToken(server = "example.com")

  expect_equal(names(token), c("token", "private_key", "claim_url"))
  expect_type(token$token, "character")
  expect_s3_class(token$private_key, "rsconnect_secret")
  expect_equal(token$claim_url, "https://example.com/claim")
  expect_equal(
    requested,
    list(class = c("connectClient", "rsconnectClient"), user_id = 0L)
  )
})

test_that("the RStudio IDE can get an auth token for a cached API key", {
  local_temp_config()
  addTestServer(url = "https://example.com/__api__")
  local_mocked_bindings(
    getCachedApiKey = function(serverUrl) "cached-key",
    currentUser = function(client) list(username = "susan")
  )

  token <- getAuthToken(server = "example.com")

  expect_equal(
    unclass(token),
    list(
      token = "cached-key",
      private_key = secret(""),
      claim_url = "https://example.com/connect/#/auth-success",
      authenticated = TRUE
    )
  )
})

test_that("the RStudio IDE can replace cli_menu() to select a Connect Cloud account", {
  # RStudio replaces cli_menu() in the rsconnect namespace with a function that
  # has this signature.
  menu <- NULL
  local_mocked_bindings(
    connectCloudGetAccounts = function(client) {
      list(
        data = list(
          list(name = "first", permissions = list("content:create")),
          list(name = "second", permissions = list("content:create"))
        )
      )
    },
    cli_menu = function(header, prompt, choices, quit = NULL, ...) {
      menu <<- list(header = header, prompt = prompt, choices = choices)
      2L
    }
  )

  expect_message(
    account <- selectCloudAccount(fake_client("connectCloudClient")),
    "multiple accounts"
  )

  expect_equal(account$name, "second")
  expect_equal(
    menu,
    list(
      header = "Multiple accounts found.",
      prompt = "Which account do you want to use?",
      choices = c("first", "second")
    )
  )
})

test_that("the RStudio IDE can read accounts and servers", {
  local_temp_config()
  addTestServer()
  addTestAccount("susan")

  accounts <- accounts()
  expect_equal(
    accounts[c("name", "server")],
    data.frame(name = "susan", server = "example.com")
  )

  servers <- servers()
  expect_true(all(c("name", "url") %in% names(servers)))
  expect_equal(
    servers[servers$name == "example.com", "url"],
    "https://example.com"
  )
})
