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

# An accounts() frame with both a shinyapps.io and a Connect Cloud account.
shinyapps_and_cc_accounts <- data.frame(
  name = c("myaccount", "cc-account"),
  server = c("shinyapps.io", "connect.posit.cloud"),
  stringsAsFactors = FALSE
)

# Helper: install the Connect Cloud mocks migrateToConnectCloud() relies on.
# Every knob has a sensible default; tests override only what they exercise.
#   - accounts:       the accounts() frame (defaults to a single CC account)
#   - title:          content title returned by getContent()
#   - account_id:     owning account id returned by getContent()
#   - owned_accounts: the accounts getAccounts() reports a role on
#   - get_content:    override the whole getContent() function (e.g. to error)
#   - client:         override the whole clientForAccount() function
local_migrate_mocks <- function(
  accounts = data.frame(
    name = "cc-account",
    server = "connect.posit.cloud",
    stringsAsFactors = FALSE
  ),
  title = "My App",
  account_id = "acct-1",
  owned_accounts = list(list(id = "acct-1", name = "cc-account")),
  get_content = NULL,
  client = NULL,
  .env = parent.frame()
) {
  get_content <- get_content %||%
    function(id) {
      list(id = id, title = title, account_id = account_id, state = "active")
    }
  client <- client %||%
    function(...) {
      fake_client(
        "connectCloudClient",
        getContent = get_content,
        getAccounts = function() list(data = owned_accounts)
      )
    }
  local_mocked_bindings(
    accounts = function(...) accounts,
    findAccountInfo = function(...) {
      list(
        name = "cc-account",
        server = "connect.posit.cloud",
        accessToken = "tok"
      )
    },
    clientForAccount = client,
    .package = "rsconnect",
    .env = .env
  )
}

test_that("migrateToConnectCloud() rewrites DCF and removes source record", {
  appDir <- withr::local_tempdir()
  srcPath <- write_shinyapps_dcf(appDir)

  local_migrate_mocks(accounts = shinyapps_and_cc_accounts)

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

  local_migrate_mocks(
    accounts = shinyapps_and_cc_accounts,
    account_id = "acct-2",
    owned_accounts = list(
      list(id = "acct-1", name = "cc-account"),
      list(id = "acct-2", name = "team-account")
    )
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

  local_migrate_mocks(
    accounts = shinyapps_and_cc_accounts,
    account_id = "acct-unknown"
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

  local_migrate_mocks()

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

  # The path collision check happens before any Connect Cloud API calls, so
  # clientForAccount() should never even be invoked here.
  local_migrate_mocks(
    accounts = shinyapps_and_cc_accounts,
    client = function(...) {
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

test_that("migrateToConnectCloud() aborts with an unmatched appName instead of orphaning the source record", {
  # An appName that matches no existing record must not fall through to
  # reconstruction -- that would write a new Connect Cloud record while
  # leaving the original (differently named) record behind.
  appDir <- withr::local_tempdir()
  srcPath <- write_shinyapps_dcf(appDir, appName = "oldname")

  local_migrate_mocks(
    accounts = shinyapps_and_cc_accounts,
    client = function(...) stop("network should not be reached")
  )

  expect_error(
    migrateToConnectCloud(
      appDir,
      contentId = "abc123",
      cloudAccount = "cc-account",
      appName = "newname"
    ),
    "No deployment record named"
  )

  # Source untouched, and no Connect Cloud record was written.
  expect_true(file.exists(srcPath))
  ccPath <- file.path(
    appDir,
    "rsconnect",
    "connect.posit.cloud",
    "cc-account",
    "newname.dcf"
  )
  expect_false(file.exists(ccPath))
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

  local_migrate_mocks(accounts = shinyapps_and_cc_accounts)

  expect_error(
    migrateToConnectCloud(
      appDir,
      contentId = "abc123",
      cloudAccount = "cc-account"
    ),
    "Failed to remove source deployment record"
  )
})

test_that("migrateToConnectCloud() reconstructs a record from contentId when no source exists (appName supplied)", {
  # Lost rsconnect directory: no source record on disk, but the content is
  # already on Connect Cloud. The record is rebuilt from contentId + appName.
  appDir <- withr::local_tempdir()

  local_migrate_mocks()

  newPath <- migrateToConnectCloud(
    appDir,
    contentId = "abc123",
    cloudAccount = "cc-account",
    appName = "myapp"
  )

  expect_true(file.exists(newPath))
  rec <- as.list(as.data.frame(read.dcf(newPath)))
  expect_equal(rec$server, "connect.posit.cloud")
  expect_equal(rec$appId, "abc123")
  expect_equal(rec$name, "myapp")
  expect_equal(rec$title, "My App")
  expect_equal(rec$url, "https://connect.posit.cloud/cc-account/content/abc123")
})

test_that("migrateToConnectCloud() derives the record name from the content title when no source and no appName", {
  appDir <- withr::local_tempdir()

  local_migrate_mocks(title = "My Great App")

  newPath <- migrateToConnectCloud(appDir, contentId = "abc123")

  rec <- as.list(as.data.frame(read.dcf(newPath)))
  # generateAppName() munges "My Great App" -> "my_great_app".
  expect_equal(rec$name, "my_great_app")
  expect_equal(rec$appId, "abc123")
})

test_that("migrateToConnectCloud() errors with guidance when the name can't be derived", {
  # Empty content title and a directory whose basename ("a") is too short to
  # form a valid app name -- the caller must supply appName.
  root <- withr::local_tempdir()
  appDir <- file.path(root, "a")
  dir.create(appDir)

  local_migrate_mocks(title = "")

  expect_error(
    migrateToConnectCloud(
      appDir,
      contentId = "abc123",
      cloudAccount = "cc-account"
    ),
    "Could not derive an application name"
  )
})

test_that("migrateToConnectCloud() derives the record name from the directory when the content has no title", {
  root <- withr::local_tempdir()
  appDir <- file.path(root, "coolapp")
  dir.create(appDir)

  local_migrate_mocks(title = "")

  newPath <- migrateToConnectCloud(appDir, contentId = "abc123")

  rec <- as.list(as.data.frame(read.dcf(newPath)))
  expect_equal(rec$name, "coolapp")
})

test_that("migrateToConnectCloud() reconstructs a record with empty envVars when there is no source", {
  appDir <- withr::local_tempdir()

  local_migrate_mocks()

  newPath <- migrateToConnectCloud(
    appDir,
    contentId = "abc123",
    appName = "myapp"
  )

  # No source record means no envVars to carry over; deploymentRecord() writes
  # NA, which read.dcf() surfaces as absent or "NA".
  rec <- as.list(as.data.frame(read.dcf(newPath)))
  expect_true(is.null(rec$envVars) || rec$envVars %in% c("", "NA"))
})

test_that("migrateToConnectCloud() aborts when the target content is missing (no source record)", {
  appDir <- withr::local_tempdir()

  local_migrate_mocks(
    get_content = function(id) {
      cli::cli_abort(
        "Content is pending deletion.",
        class = c("rsconnect_http_404", "rsconnect_http")
      )
    }
  )

  expect_error(
    migrateToConnectCloud(
      appDir,
      contentId = "abc123",
      appName = "myapp"
    ),
    "pending deletion"
  )
})

test_that("migrateToConnectCloud() requires a string contentId", {
  appDir <- withr::local_tempdir()
  expect_error(migrateToConnectCloud(appDir))
  expect_error(migrateToConnectCloud(appDir, contentId = 123))
})

test_that("migrateToConnectCloud() rejects a blank appName", {
  appDir <- withr::local_tempdir()
  expect_error(
    migrateToConnectCloud(appDir, contentId = "abc123", appName = "")
  )
})

test_that("migrateToConnectCloud() aborts when the source already targets Connect Cloud", {
  appDir <- local_temp_app()
  local_pcc_deploy_env(appDir, appId = "content-abc")

  expect_error(
    migrateToConnectCloud(appDir, contentId = "abc123"),
    regexp = "already targets Connect Cloud"
  )
})
