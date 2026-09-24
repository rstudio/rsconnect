test_that("deletes content from the deployment record and removes the record", {
  app_dir <- withr::local_tempdir()
  local_pcc_deploy_env(app_dir, appId = "content-uuid-123")
  deleted_id <- NULL
  local_mock_pcc_delete_client(function(id) deleted_id <<- id)

  expect_message(
    expect_message(
      deleteContent(
        appDir = app_dir,
        server = "connect.posit.cloud",
        force = TRUE
      ),
      "Deleted content"
    ),
    "Removed deployment record"
  )
  expect_equal(deleted_id, "content-uuid-123")
  expect_equal(nrow(deployments(app_dir)), 0)
})

test_that("contentId deletes directly and leaves local records alone", {
  app_dir <- withr::local_tempdir()
  local_pcc_deploy_env(app_dir, appId = "content-uuid-123")
  deleted_id <- NULL
  local_mock_pcc_delete_client(function(id) deleted_id <<- id)

  expect_message(
    deleteContent(
      appDir = app_dir,
      contentId = "other-content",
      server = "connect.posit.cloud",
      force = TRUE
    )
  )
  expect_equal(deleted_id, "other-content")
  expect_equal(nrow(deployments(app_dir)), 1)
})

test_that("requires force in non-interactive sessions", {
  app_dir <- withr::local_tempdir()
  local_pcc_deploy_env(app_dir, appId = "content-uuid-123")
  deleted_id <- NULL
  local_mock_pcc_delete_client(function(id) deleted_id <<- id)
  withr::local_options(rlang_interactive = FALSE)

  expect_error(
    deleteContent(appDir = app_dir, server = "connect.posit.cloud"),
    "force = TRUE"
  )
  expect_null(deleted_id)
})

test_that("confirmation prompt can cancel or proceed", {
  app_dir <- withr::local_tempdir()
  local_pcc_deploy_env(app_dir, appId = "content-uuid-123")
  deleted_id <- NULL
  local_mock_pcc_delete_client(function(id) deleted_id <<- id)

  simulate_user_input("1")
  expect_error(
    suppressMessages(
      deleteContent(appDir = app_dir, server = "connect.posit.cloud")
    ),
    "Quitting"
  )
  expect_null(deleted_id)

  simulate_user_input("2")
  suppressMessages(
    deleteContent(appDir = app_dir, server = "connect.posit.cloud")
  )
  expect_equal(deleted_id, "content-uuid-123")
})

test_that("reports content that is already deleted", {
  app_dir <- withr::local_tempdir()
  local_pcc_deploy_env(app_dir, appId = "content-uuid-123")
  local_mocked_bindings(clientForAccount = function(...) {
    fake_client(
      "connectCloudClient",
      getContent = function(contentId) {
        cli::cli_abort(
          "Content is pending deletion.",
          class = "rsconnect_http_404"
        )
      },
      deleteContent = function(contentId) stop("should not be called")
    )
  })

  expect_message(
    expect_error(
      deleteContent(
        appDir = app_dir,
        server = "connect.posit.cloud",
        force = TRUE
      ),
      "may already be deleted"
    ),
    "Removed deployment record"
  )
  expect_equal(nrow(deployments(app_dir)), 0)
})

test_that("warns when the deployment record can't be removed", {
  skip_on_os("windows")
  app_dir <- withr::local_tempdir()
  local_pcc_deploy_env(app_dir, appId = "content-uuid-123")
  deleted_id <- NULL
  local_mock_pcc_delete_client(function(id) deleted_id <<- id)

  record_dir <- dirname(deployments(app_dir)$deploymentFile)
  Sys.chmod(record_dir, "0555")
  withr::defer(Sys.chmod(record_dir, "0755"))

  expect_warning(
    suppressMessages(
      deleteContent(
        appDir = app_dir,
        server = "connect.posit.cloud",
        force = TRUE
      )
    ),
    "Failed to remove deployment record"
  )
  expect_equal(deleted_id, "content-uuid-123")
})

test_that("rejects servers other than Posit Connect Cloud", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  expect_error(
    deleteContent(account = "ron", server = "example.com", force = TRUE),
    "must be Posit Connect Cloud"
  )
})
