# Helper: write a minimal fixture DCF for a shinyapps.io deployment.
write_shinyapps_dcf <- function(appDir, appName = "myapp", account = "myaccount") {
  dcfDir <- file.path(appDir, "rsconnect", "shinyapps.io", account)
  dir.create(dcfDir, recursive = TRUE)
  dcfPath <- file.path(dcfDir, paste0(appName, ".dcf"))
  write.dcf(
    list(
      name     = appName,
      title    = "My App",
      username = account,
      account  = account,
      server   = "shinyapps.io",
      hostUrl  = "https://api.shinyapps.io/v1",
      appId    = "42",
      bundleId = "7",
      url      = paste0("https://", account, ".shinyapps.io/", appName),
      version  = "1"
    ),
    dcfPath,
    width = 4096
  )
  dcfPath
}

test_that("migrateDeployment() rewrites DCF and removes source record", {
  appDir   <- withr::local_tempdir()
  srcPath  <- write_shinyapps_dcf(appDir)

  local_mocked_bindings(
    accounts = function(...) {
      data.frame(
        name   = c("myaccount", "cc-account"),
        server = c("shinyapps.io", "connect.posit.cloud"),
        stringsAsFactors = FALSE
      )
    },
    findAccountInfo = function(name, server, ...) {
      list(name = "cc-account", server = "connect.posit.cloud", accessToken = "tok")
    },
    clientForAccount = function(info) {
      list(
        getContent = function(contentId) {
          list(
            id    = contentId,
            title = "My App",
            url   = paste0("https://connect.posit.cloud/cc-account/content/", contentId),
            state = "active"
          )
        }
      )
    }
  )

  newPath <- migrateDeployment(appDir, contentId = "abc123", cloudAccount = "cc-account")

  # New record exists under connect.posit.cloud/.
  expect_true(file.exists(newPath))
  newRec <- as.list(as.data.frame(read.dcf(newPath)))
  expect_equal(newRec$server,   "connect.posit.cloud")
  expect_equal(newRec$appId,    "abc123")
  expect_equal(newRec$account,  "cc-account")
  expect_equal(newRec$bundleId, "")   # Connect Cloud never uses bundleId

  # Old shinyapps.io record is gone.
  expect_false(file.exists(srcPath))
})

test_that("migrateDeployment() aborts when source already targets Connect Cloud", {
  appDir <- withr::local_tempdir()
  ccDir  <- file.path(appDir, "rsconnect", "connect.posit.cloud", "cc-account")
  dir.create(ccDir, recursive = TRUE)
  write.dcf(
    list(name = "myapp", account = "cc-account", server = "connect.posit.cloud",
         appId = "abc123", version = "1"),
    file.path(ccDir, "myapp.dcf"),
    width = 4096
  )

  local_mocked_bindings(
    accounts         = function(...) data.frame(name = "cc-account", server = "connect.posit.cloud",
                                                stringsAsFactors = FALSE),
    findAccountInfo  = function(...) list(name = "cc-account", server = "connect.posit.cloud", accessToken = "tok"),
    clientForAccount = function(...) list(getContent = function(id) list(id = id, title = "", url = "", state = "active"))
  )

  expect_error(
    migrateDeployment(appDir, contentId = "abc123"),
    "already targets Connect Cloud"
  )
})

test_that("ensureConnectCloudAccount() aborts in non-interactive session with no CC accounts", {
  local_mocked_bindings(
    accounts = function(...) data.frame(name = character(), server = character(),
                                        stringsAsFactors = FALSE)
  )
  expect_error(
    ensureConnectCloudAccount(),
    "No Posit Connect Cloud account registered"
  )
})

test_that("migrateDeployment() aborts with no deployment records", {
  appDir <- withr::local_tempdir()

  local_mocked_bindings(
    accounts        = function(...) data.frame(name = "cc-account", server = "connect.posit.cloud",
                                               stringsAsFactors = FALSE),
    findAccountInfo = function(...) list(name = "cc-account", server = "connect.posit.cloud", accessToken = "tok"),
    clientForAccount = function(...) list(getContent = function(id) list(id = id, title = "", url = "", state = "active"))
  )

  expect_error(
    migrateDeployment(appDir, contentId = "abc123"),
    "No deployment records found"
  )
})
