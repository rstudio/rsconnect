# Helper: write a minimal fixture DCF for a shinyapps.io deployment.
write_shinyapps_dcf <- function(
  appDir,
  appName = "myapp",
  account = "myaccount"
) {
  dcfDir <- file.path(appDir, "rsconnect", "shinyapps.io", account)
  dir.create(dcfDir, recursive = TRUE)
  dcfPath <- file.path(dcfDir, paste0(appName, ".dcf"))
  write.dcf(
    list(
      name = appName,
      title = "My App",
      username = account,
      account = account,
      server = "shinyapps.io",
      hostUrl = "https://api.shinyapps.io/v1",
      appId = "42",
      bundleId = "7",
      url = paste0("https://", account, ".shinyapps.io/", appName),
      version = "1"
    ),
    dcfPath,
    width = 4096
  )
  dcfPath
}

test_that("migrateToConnectCloud() rewrites DCF and removes source record", {
  appDir <- withr::local_tempdir()
  srcPath <- write_shinyapps_dcf(appDir)

  local_mocked_bindings(
    accounts = function(...) {
      data.frame(
        name = c("myaccount", "cc-account"),
        server = c("shinyapps.io", "connect.posit.cloud"),
        stringsAsFactors = FALSE
      )
    },
    findAccountInfo = function(name, server, ...) {
      list(
        name = "cc-account",
        server = "connect.posit.cloud",
        accessToken = "tok"
      )
    },
    clientForAccount = function(info) {
      list(
        getContent = function(contentId) {
          list(
            id = contentId,
            title = "My App",
            account_id = "acct-1",
            state = "active"
          )
        },
        getAccounts = function() {
          list(data = list(list(id = "acct-1", name = "cc-account")))
        }
      )
    }
  )

  newPath <- migrateToConnectCloud(
    appDir,
    contentId = "abc123",
    cloudAccount = "cc-account"
  )

  # New record exists under connect.posit.cloud/.
  expect_true(file.exists(newPath))
  newRec <- as.list(as.data.frame(read.dcf(newPath)))
  expect_equal(newRec$server, "connect.posit.cloud")
  expect_equal(newRec$appId, "abc123")
  expect_equal(newRec$account, "cc-account")
  expect_equal(newRec$bundleId, "") # Connect Cloud never uses bundleId
  # URL is built from the content's owning account (acct-1 -> "cc-account"),
  # not assumed from the locally authenticated account.
  expect_equal(
    newRec$url,
    "https://connect.posit.cloud/cc-account/content/abc123"
  )

  # Old shinyapps.io record is gone.
  expect_false(file.exists(srcPath))
})

test_that("migrateToConnectCloud() builds the URL from the content's owning account, not the local account", {
  # Regression test: the caller is authenticated as "cc-account" but the
  # content being migrated belongs to a different account ("team-account"),
  # e.g. a collaborator migrating content they don't own. The URL must use
  # the owning account's slug, not the locally authenticated one.
  appDir <- withr::local_tempdir()
  write_shinyapps_dcf(appDir)

  local_mocked_bindings(
    accounts = function(...) {
      data.frame(
        name = c("myaccount", "cc-account"),
        server = c("shinyapps.io", "connect.posit.cloud"),
        stringsAsFactors = FALSE
      )
    },
    findAccountInfo = function(...) {
      list(
        name = "cc-account",
        server = "connect.posit.cloud",
        accessToken = "tok"
      )
    },
    clientForAccount = function(...) {
      list(
        getContent = function(contentId) {
          list(
            id = contentId,
            title = "My App",
            account_id = "acct-2",
            state = "active"
          )
        },
        getAccounts = function() {
          list(
            data = list(
              list(id = "acct-1", name = "cc-account"),
              list(id = "acct-2", name = "team-account")
            )
          )
        }
      )
    }
  )

  newPath <- migrateToConnectCloud(
    appDir,
    contentId = "abc123",
    cloudAccount = "cc-account"
  )

  newRec <- as.list(as.data.frame(read.dcf(newPath)))
  expect_equal(
    newRec$url,
    "https://connect.posit.cloud/team-account/content/abc123"
  )
})

test_that("migrateToConnectCloud() aborts when the content's account can't be resolved", {
  # If the content's account_id isn't among the accounts the caller has a
  # role on, the caller likely can't deploy to it either -- fail early
  # instead of writing a record with an empty url.
  appDir <- withr::local_tempdir()
  srcPath <- write_shinyapps_dcf(appDir)

  local_mocked_bindings(
    accounts = function(...) {
      data.frame(
        name = c("myaccount", "cc-account"),
        server = c("shinyapps.io", "connect.posit.cloud"),
        stringsAsFactors = FALSE
      )
    },
    findAccountInfo = function(...) {
      list(
        name = "cc-account",
        server = "connect.posit.cloud",
        accessToken = "tok"
      )
    },
    clientForAccount = function(...) {
      list(
        getContent = function(id) {
          list(
            id = id,
            title = "My App",
            account_id = "acct-unknown",
            state = "active"
          )
        },
        getAccounts = function() {
          list(data = list(list(id = "acct-1", name = "cc-account")))
        }
      )
    }
  )

  expect_error(
    migrateToConnectCloud(
      appDir,
      contentId = "abc123",
      cloudAccount = "cc-account"
    ),
    "Unable to determine the Connect Cloud account"
  )

  # Nothing was written or deleted.
  expect_true(file.exists(srcPath))
})

test_that("migrateToConnectCloud() aborts when source already targets Connect Cloud", {
  appDir <- withr::local_tempdir()
  ccDir <- file.path(appDir, "rsconnect", "connect.posit.cloud", "cc-account")
  dir.create(ccDir, recursive = TRUE)
  write.dcf(
    list(
      name = "myapp",
      account = "cc-account",
      server = "connect.posit.cloud",
      appId = "abc123",
      version = "1"
    ),
    file.path(ccDir, "myapp.dcf"),
    width = 4096
  )

  local_mocked_bindings(
    accounts = function(...) {
      data.frame(
        name = "cc-account",
        server = "connect.posit.cloud",
        stringsAsFactors = FALSE
      )
    },
    findAccountInfo = function(...) {
      list(
        name = "cc-account",
        server = "connect.posit.cloud",
        accessToken = "tok"
      )
    },
    clientForAccount = function(...) {
      list(getContent = function(id) {
        list(id = id, title = "", url = "", state = "active")
      })
    }
  )

  expect_error(
    migrateToConnectCloud(appDir, contentId = "abc123"),
    "already targets Connect Cloud"
  )
})

test_that("migrateToConnectCloud() aborts when a record already exists at the target path (non-interactive)", {
  appDir <- withr::local_tempdir()
  srcPath <- write_shinyapps_dcf(appDir)

  # Pre-create a colliding Connect Cloud record at the path migrateToConnectCloud()
  # would write to.
  ccDir <- file.path(appDir, "rsconnect", "connect.posit.cloud", "cc-account")
  dir.create(ccDir, recursive = TRUE)
  write.dcf(
    list(
      name = "myapp",
      account = "cc-account",
      server = "connect.posit.cloud",
      appId = "existing123",
      version = "1"
    ),
    file.path(ccDir, "myapp.dcf"),
    width = 4096
  )

  local_mocked_bindings(
    accounts = function(...) {
      data.frame(
        name = c("myaccount", "cc-account"),
        server = c("shinyapps.io", "connect.posit.cloud"),
        stringsAsFactors = FALSE
      )
    },
    findAccountInfo = function(...) {
      list(
        name = "cc-account",
        server = "connect.posit.cloud",
        accessToken = "tok"
      )
    },
    # The path collision check happens before any Connect Cloud API calls,
    # so clientForAccount() should never even be invoked here.
    clientForAccount = function(...) {
      stop("clientForAccount() should not be called before the collision check")
    }
  )

  expect_error(
    migrateToConnectCloud(
      appDir,
      contentId = "abc123",
      cloudAccount = "cc-account",
      server = "shinyapps.io"
    ),
    "already exists"
  )

  # Neither the source nor the pre-existing target record were touched.
  expect_true(file.exists(srcPath))
  newRec <- as.list(as.data.frame(read.dcf(file.path(ccDir, "myapp.dcf"))))
  expect_equal(newRec$appId, "existing123")
})

test_that("ensureConnectCloudAccount() aborts in non-interactive session with no CC accounts", {
  local_mocked_bindings(
    accounts = function(...) {
      data.frame(
        name = character(),
        server = character(),
        stringsAsFactors = FALSE
      )
    }
  )
  expect_error(
    ensureConnectCloudAccount(),
    "No Posit Connect Cloud account registered"
  )
})

test_that("migrateToConnectCloud() aborts when source record cannot be deleted", {
  skip_on_cran()
  skip_on_os("windows")
  appDir <- withr::local_tempdir()
  srcPath <- write_shinyapps_dcf(appDir)
  # Lock the parent dir so unlink() returns non-zero.
  srcDir <- dirname(srcPath)
  Sys.chmod(srcDir, mode = "555")
  withr::defer(Sys.chmod(srcDir, mode = "755"))

  local_mocked_bindings(
    accounts = function(...) {
      data.frame(
        name = c("myaccount", "cc-account"),
        server = c("shinyapps.io", "connect.posit.cloud"),
        stringsAsFactors = FALSE
      )
    },
    findAccountInfo = function(...) {
      list(
        name = "cc-account",
        server = "connect.posit.cloud",
        accessToken = "tok"
      )
    },
    clientForAccount = function(...) {
      list(
        getContent = function(id) {
          list(
            id = id,
            title = "My App",
            account_id = "acct-1",
            state = "active"
          )
        },
        getAccounts = function() {
          list(data = list(list(id = "acct-1", name = "cc-account")))
        }
      )
    }
  )

  expect_error(
    migrateToConnectCloud(
      appDir,
      contentId = "abc123",
      cloudAccount = "cc-account"
    ),
    "Failed to remove source deployment record"
  )
})

test_that("migrateToConnectCloud() aborts with no deployment records", {
  appDir <- withr::local_tempdir()

  local_mocked_bindings(
    accounts = function(...) {
      data.frame(
        name = "cc-account",
        server = "connect.posit.cloud",
        stringsAsFactors = FALSE
      )
    },
    findAccountInfo = function(...) {
      list(
        name = "cc-account",
        server = "connect.posit.cloud",
        accessToken = "tok"
      )
    },
    clientForAccount = function(...) {
      list(getContent = function(id) {
        list(id = id, title = "", url = "", state = "active")
      })
    }
  )

  expect_error(
    migrateToConnectCloud(appDir, contentId = "abc123"),
    "No deployment records found"
  )
})
