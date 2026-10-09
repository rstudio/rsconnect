test_that("shinyapps accounts create shinyapps clients", {
  account <- list(server = "shinyapps.io")
  client <- clientForAccount(account)
  expect_s3_class(client, "shinyAppsClient")
})

test_that("connect cloud accounts create connect cloud clients", {
  account <- list(server = "connect.posit.cloud")
  client <- clientForAccount(account)
  expect_s3_class(client, "connectCloudClient")
})

test_that("connect accounts create connect clients", {
  local_temp_config()

  addTestServer("example.com")
  account <- list(server = "example.com")
  client <- clientForAccount(account)
  expect_s3_class(client, "connectClient")
})

# Every S3 generic implemented for at least one client class must be implemented
# (directly, or inherited from "rsconnectClient") for every other client class
test_that("every client generic has a method for every client class", {
  for (generic in client_generics()) {
    for (class in client_classes()) {
      expect_client_method(generic, class)
    }
  }
})

test_that("findContentByName() returns the content with that name", {
  local_mocked_bindings(
    listApplications.shinyAppsClient = function(client, accountId, filters) {
      list(list(id = 42, name = filters$name))
    }
  )
  client <- fake_client("shinyAppsClient")
  content <- findContentByName(client, list(accountId = "1"), "my-app")
  expect_equal(content, list(id = 42, name = "my-app"))
})

test_that("findContentByName() returns NULL when no content has that name", {
  local_mocked_bindings(
    listApplications.connectClient = function(client, accountId, filters) list()
  )
  client <- fake_client("connectClient")
  expect_null(findContentByName(client, list(accountId = "1"), "my-app"))
})

test_that("findContentByName() does not look up content on Connect Cloud", {
  local_mocked_bindings(
    listApplications.connectCloudClient = function(...) {
      stop("listApplications was called")
    }
  )
  client <- fake_client("connectCloudClient")
  expect_null(findContentByName(client, list(accountId = "1"), "my-app"))
})

test_that("client checks accept only their own client class", {
  shinyapps <- fake_client("shinyAppsClient")
  cloud <- fake_client("connectCloudClient")
  connect <- fake_client("connectClient")

  expect_no_error(checkShinyappsClient(shinyapps))
  expect_no_error(checkConnectCloudClient(cloud))
  expect_no_error(checkConnectClient(connect))

  expect_snapshot(error = TRUE, {
    checkShinyappsClient(connect)
    checkConnectCloudClient(shinyapps)
    checkConnectClient(cloud)
  })
})
