test_that("uploadBundle creates, uploads, marks ready, and returns the bundle", {
  skip_if_not_installed("webfakes")

  bundlePath <- withr::local_tempfile(fileext = ".tar.gz")
  writeLines("bundle contents", bundlePath)

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  # Step 1: register a pending bundle and return the presigned upload URL.
  app$post("/bundles", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = "bundle-1",
        presigned_url = paste0("http://", req$get_header("host"), "/upload"),
        presigned_checksum = "checksum-abc"
      ),
      auto_unbox = TRUE
    )
  })
  # Step 2: the presigned PUT. Status 200 means the upload worked.
  app$put("/upload", function(req, res) {
    res$set_status(200L)$send("")
  })
  # Step 3: mark the bundle ready. webfakes runs in a separate process, so the
  # server validates the status and returns 400 on a bad value; a clean 200
  # then proves the client sent status = "ready".
  app$post("/bundles/:id/status", function(req, res) {
    if (identical(req$json$status, "ready")) {
      res$set_status(200L)$send_json(list(), auto_unbox = TRUE)
    } else {
      res$set_status(400L)$send_json(
        list(error = "status must be ready"),
        auto_unbox = TRUE
      )
    }
  })
  # Step 4: return the updated bundle.
  app$get("/bundles/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(id = req$params$id, status = "ready"),
      auto_unbox = TRUE
    )
  })
  proc <- webfakes::local_app_process(app)
  service <- parseHttpUrl(proc$url())

  authInfo <- list(server = "shinyapps.io", name = "some-user")
  client <- shinyAppsClient(service, authInfo)

  bundle <- uploadBundle(
    client,
    list(application_id = "42"),
    bundlePath
  )

  expect_equal(bundle$id, "bundle-1")
  expect_equal(bundle$status, "ready")
})

test_that("uploadBundle stops when the presigned upload fails", {
  skip_if_not_installed("webfakes")

  bundlePath <- withr::local_tempfile(fileext = ".tar.gz")
  writeLines("bundle contents", bundlePath)

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$post("/bundles", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = "bundle-1",
        presigned_url = paste0("http://", req$get_header("host"), "/upload"),
        presigned_checksum = "checksum-abc"
      ),
      auto_unbox = TRUE
    )
  })
  # The presigned URL rejects the upload.
  app$put("/upload", function(req, res) {
    res$set_status(403L)$send("")
  })
  proc <- webfakes::local_app_process(app)
  service <- parseHttpUrl(proc$url())

  authInfo <- list(server = "shinyapps.io", name = "some-user")
  client <- shinyAppsClient(service, authInfo)

  expect_error(
    uploadBundle(client, list(application_id = "42"), bundlePath),
    "Could not upload file"
  )
})

test_that("createContent() POSTs the name, template, and account", {
  sent <- NULL
  local_mocked_bindings(
    POST_JSON = function(service, authInfo, path, json) {
      sent <<- list(path = path, json = json)
      list(id = 7, url = "https://some-user.shinyapps.io/my-app/")
    }
  )
  client <- shinyAppsClient(list(), list())

  application <- createContent(
    client,
    deployment = list(name = "my-app", title = "My App"),
    accountDetails = list(accountId = "12"),
    appMetadata = list(appMode = "shiny")
  )

  expect_equal(sent$path, "/applications/")
  expect_equal(
    sent$json,
    list(name = "my-app", template = "shiny", account = 12)
  )
  expect_equal(
    application,
    list(
      id = 7,
      application_id = 7,
      url = "https://some-user.shinyapps.io/my-app/"
    )
  )
})

test_that("findContent() gets the application for the deployment record", {
  requested <- NULL
  client <- fake_client(
    "shinyAppsClient",
    getApplication = function(applicationId, deploymentRecordVersion) {
      requested <<- list(applicationId, deploymentRecordVersion)
      list(id = applicationId, url = "https://some-user.shinyapps.io/app/")
    }
  )

  application <- findContent(
    client,
    deployment = list(appId = "42", version = "1"),
    quiet = TRUE
  )

  expect_equal(requested, list("42", "1"))
  expect_equal(application$id, "42")
})

test_that("prepareContent() sets the visibility when it changes", {
  sent <- NULL
  client <- fake_client(
    "shinyAppsClient",
    setApplicationProperty = function(applicationId, propertyName, value) {
      sent <<- list(applicationId, propertyName, value)
    }
  )
  application <- list(
    id = "42",
    deployment = list(
      properties = list(application.visibility = "public")
    )
  )

  result <- prepareContent(
    client,
    application,
    deployment = list(),
    appMetadata = list(),
    appVisibility = "private",
    isNewContent = FALSE,
    upload = TRUE,
    quiet = TRUE
  )

  expect_equal(sent, list("42", "application.visibility", "private"))
  expect_equal(result, application)
})

test_that("prepareContent() does not set the visibility when it is the same", {
  client <- fake_client(
    "shinyAppsClient",
    setApplicationProperty = function(...) {
      stop("setApplicationProperty() should not be called")
    }
  )
  application <- list(
    id = "42",
    deployment = list(
      properties = list(application.visibility = "private")
    )
  )

  expect_no_error(prepareContent(
    client,
    application,
    deployment = list(),
    appMetadata = list(),
    appVisibility = "private",
    isNewContent = FALSE,
    upload = TRUE,
    quiet = TRUE
  ))
})

test_that("prepareContent() ignores env vars on the deployment", {
  client <- fake_client("shinyAppsClient")

  expect_no_error(prepareContent(
    client,
    list(id = "42"),
    deployment = list(envVars = "A"),
    appMetadata = list(),
    appVisibility = NULL,
    isNewContent = FALSE,
    upload = TRUE,
    quiet = TRUE
  ))
})

test_that("activateContent() deploys the uploaded bundle and waits for the task", {
  deployed <- NULL
  waited <- NULL
  client <- fake_client(
    "shinyAppsClient",
    deployApplication = function(application, bundleId = NULL) {
      deployed <<- bundleId
      list(task_id = "task-1")
    },
    waitForTask = function(taskId, quiet = FALSE) {
      waited <<- taskId
      list()
    }
  )
  application <- list(
    id = "42",
    url = "https://some-user.shinyapps.io/app/",
    deployment = list(bundle = list(id = "bundle-old"))
  )

  result <- activateContent(
    client,
    application,
    bundle = list(id = "bundle-new"),
    quiet = TRUE
  )

  expect_equal(deployed, "bundle-new")
  expect_equal(waited, "task-1")
  expect_equal(
    result,
    list(
      succeeded = TRUE,
      url = "https://some-user.shinyapps.io/app/",
      error = NULL
    )
  )
})

test_that("activateContent() deploys the current bundle when nothing was uploaded", {
  deployed <- NULL
  client <- fake_client(
    "shinyAppsClient",
    deployApplication = function(application, bundleId = NULL) {
      deployed <<- bundleId
      list(task_id = "task-1")
    },
    waitForTask = function(...) list()
  )
  application <- list(
    id = "42",
    deployment = list(bundle = list(id = "bundle-old"))
  )

  activateContent(client, application, bundle = NULL, quiet = TRUE)

  expect_equal(deployed, "bundle-old")
})

test_that("activateContent() reports a failed task", {
  client <- fake_client(
    "shinyAppsClient",
    deployApplication = function(...) list(task_id = "task-1"),
    waitForTask = function(...) list(code = 1, error = "Build failed")
  )

  result <- activateContent(
    client,
    list(id = "42"),
    bundle = list(id = "bundle-1"),
    quiet = TRUE
  )

  expect_false(result$succeeded)
  expect_equal(result$error, "Build failed")
})
