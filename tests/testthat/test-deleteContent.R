local_pcc_deployment <- function(env = caller_env()) {
  local_temp_config(env)
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir(.local_envir = env)
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  app_dir
}

local_mock_pcc_client <- function(deleted, env = caller_env()) {
  local_mocked_bindings(
    clientForAccount = function(...) {
      list(
        getContent = function(contentId) {
          list(id = contentId, title = "My App")
        },
        deleteContent = function(contentId) {
          deleted(contentId)
          invisible(TRUE)
        }
      )
    },
    .env = env
  )
}

test_that("deletes content from the deployment record and removes the record", {
  app_dir <- local_pcc_deployment()
  deleted_id <- NULL
  local_mock_pcc_client(function(id) deleted_id <<- id)

  expect_message(
    deleteContent(
      appDir = app_dir,
      server = "connect.posit.cloud",
      force = TRUE
    ),
    "Deleted content"
  )
  expect_equal(deleted_id, "content-uuid-123")
  expect_equal(nrow(deployments(app_dir)), 0)
})

test_that("contentId deletes directly and leaves local records alone", {
  app_dir <- local_pcc_deployment()
  deleted_id <- NULL
  local_mock_pcc_client(function(id) deleted_id <<- id)

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
  app_dir <- local_pcc_deployment()
  deleted_id <- NULL
  local_mock_pcc_client(function(id) deleted_id <<- id)
  withr::local_options(rlang_interactive = FALSE)

  expect_error(
    deleteContent(appDir = app_dir, server = "connect.posit.cloud"),
    "force = TRUE"
  )
  expect_null(deleted_id)
})

test_that("confirmation prompt can cancel or proceed", {
  app_dir <- local_pcc_deployment()
  deleted_id <- NULL
  local_mock_pcc_client(function(id) deleted_id <<- id)

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
  app_dir <- local_pcc_deployment()
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      getContent = function(contentId) {
        cli::cli_abort(
          "Content is pending deletion.",
          class = "rsconnect_http_404"
        )
      },
      deleteContent = function(contentId) stop("should not be called")
    )
  })

  expect_error(
    deleteContent(
      appDir = app_dir,
      server = "connect.posit.cloud",
      force = TRUE
    ),
    "may already be deleted"
  )
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
