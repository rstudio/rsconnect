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
