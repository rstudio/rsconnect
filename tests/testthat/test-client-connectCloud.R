
test_that("awaitCompletion", {
  skip_if_not_installed("webfakes")

  revision_app <- webfakes::new_app()
  revision_app$use(webfakes::mw_json())
  revision_app$get("/revisions/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = I(req$params$id),
        publish_result = "success",
        url = "https://example.posit.cloud/content/123",
        publish_error_details = NULL
      ),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::new_app_process(revision_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    secret = NULL,
    private_key = NULL,
    apiKey = "the-api-key",
    protocol = "https",
    certificate = NULL
  )
  client <- connectCloudClient(service, authInfo)

  # test successful completion
  result <- client$awaitCompletion("rev123")
  expect_true(result$success)
  expect_equal(result$url, "https://example.posit.cloud/content/123")
  expect_null(result$error)
})

test_that("awaitCompletion handles failure", {
  skip_if_not_installed("webfakes")

  revision_app <- webfakes::new_app()
  revision_app$use(webfakes::mw_json())
  revision_app$get("/revisions/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = I(req$params$id),
        publish_result = "failure",
        url = NULL,
        publish_error_details = "Deployment failed due to missing dependencies"
      ),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::new_app_process(revision_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    secret = NULL,
    private_key = NULL,
    apiKey = "the-api-key",
    protocol = "https",
    certificate = NULL
  )
  client <- connectCloudClient(service, authInfo)

  # test failure case
  result <- client$awaitCompletion("rev456")
  expect_false(result$success)
  expect_null(result$url)
  expect_equal(result$error, "Deployment failed due to missing dependencies")
})
