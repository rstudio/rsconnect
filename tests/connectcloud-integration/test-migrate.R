# See README.md in this directory for the manual developer workflow
# (register both accounts, deploy to both platforms, migrate, verify).

# Required env var for the automated test below:
#   CONNECT_CLOUD_CONTENT_ID — ID of a pre-existing Connect Cloud content item
#   owned by CONNECT_CLOUD_ACCOUNT (provision manually via the admin UI or
#   a one-time deployApp() call).

test_that("migrateDeployment() rewrites a fixture DCF against the live Connect Cloud API", {
  content_id <- Sys.getenv("CONNECT_CLOUD_CONTENT_ID")
  if (content_id == "") {
    skip(
      "CONNECT_CLOUD_CONTENT_ID not set — skipping live API integration test."
    )
  }

  # Fixture: a minimal shinyapps.io deployment record (no actual shinyapps.io
  # deploy needed — we just need a source DCF on disk).
  appDir <- withr::local_tempdir()
  dcfDir <- file.path(appDir, "rsconnect", "shinyapps.io", "fixture-account")
  dir.create(dcfDir, recursive = TRUE)
  write.dcf(
    list(
      name = "migrate-test",
      title = "Migrate Test",
      username = "fixture-account",
      account = "fixture-account",
      server = "shinyapps.io",
      hostUrl = "https://api.shinyapps.io/v1",
      appId = "99",
      bundleId = "1",
      url = "https://fixture-account.shinyapps.io/migrate-test",
      version = "1"
    ),
    file.path(dcfDir, "migrate-test.dcf"),
    width = 4096
  )
  old_dcf <- file.path(dcfDir, "migrate-test.dcf")

  # Run the migration against the live Connect Cloud API.
  new_dcf <- migrateDeployment(
    appPath = appDir,
    contentId = content_id,
    cloudAccount = cc_local_name # set by setup.R
  )

  # New record exists and points at Connect Cloud.
  expect_true(file.exists(new_dcf))
  rec <- as.list(as.data.frame(read.dcf(new_dcf)))
  expect_equal(rec$server, "connect.posit.cloud")
  expect_equal(rec$appId, content_id)
  expect_equal(rec$account, cc_local_name)
  expect_equal(rec$bundleId, "")

  # Old shinyapps.io record is gone.
  expect_false(file.exists(old_dcf))
})
