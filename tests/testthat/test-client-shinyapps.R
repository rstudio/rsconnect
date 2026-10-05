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
  local_mocked_bindings(
    getApplication.shinyAppsClient = function(
      client,
      applicationId
    ) {
      requested <<- applicationId
      list(id = applicationId, url = "https://some-user.shinyapps.io/app/")
    }
  )
  client <- fake_client("shinyAppsClient")

  application <- findContent(
    client,
    deployment = list(appId = "42", version = "1"),
    quiet = TRUE
  )

  expect_equal(requested, "42")
  expect_equal(application$id, "42")
})

test_that("prepareContent() sets the visibility when it changes", {
  sent <- NULL
  local_mocked_bindings(
    shinyappsSetApplicationProperty = function(
      client,
      applicationId,
      propertyName,
      value
    ) {
      sent <<- list(applicationId, propertyName, value)
    }
  )
  client <- fake_client("shinyAppsClient")
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
  local_mocked_bindings(
    shinyappsSetApplicationProperty = function(...) {
      stop("setApplicationProperty() should not be called")
    }
  )
  client <- fake_client("shinyAppsClient")
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

test_that("needsVisibilityChange() compares with the current visibility", {
  public <- list(
    deployment = list(properties = list(application.visibility = "public"))
  )
  noVisibility <- list(deployment = list(properties = list()))

  expect_false(needsVisibilityChange(public, NULL))
  expect_false(needsVisibilityChange(public, "public"))
  expect_true(needsVisibilityChange(public, "private"))
  expect_false(needsVisibilityChange(noVisibility, "public"))
  expect_true(needsVisibilityChange(noVisibility, "private"))
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
  local_mocked_bindings(
    shinyappsDeployApplication = function(
      client,
      application,
      bundleId = NULL
    ) {
      deployed <<- bundleId
      list(task_id = "task-1")
    },
    shinyappsWaitForTask = function(client, taskId, quiet = FALSE) {
      waited <<- taskId
      list()
    }
  )
  client <- fake_client("shinyAppsClient")
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
  local_mocked_bindings(
    shinyappsDeployApplication = function(
      client,
      application,
      bundleId = NULL
    ) {
      deployed <<- bundleId
      list(task_id = "task-1")
    },
    shinyappsWaitForTask = function(...) list()
  )
  client <- fake_client("shinyAppsClient")
  application <- list(
    id = "42",
    deployment = list(bundle = list(id = "bundle-old"))
  )

  activateContent(client, application, bundle = NULL, quiet = TRUE)

  expect_equal(deployed, "bundle-old")
})

test_that("activateContent() reports a failed task", {
  local_mocked_bindings(
    shinyappsDeployApplication = function(...) list(task_id = "task-1"),
    shinyappsWaitForTask = function(...) list(code = 1, error = "Build failed")
  )
  client <- fake_client("shinyAppsClient")

  result <- activateContent(
    client,
    list(id = "42"),
    bundle = list(id = "bundle-1"),
    quiet = TRUE
  )

  expect_false(result$succeeded)
  expect_equal(result$error, "Build failed")
})

test_that("inviteApplicationUser() POSTs the email and the invitation options", {
  sent <- NULL
  local_mocked_bindings(
    POST_JSON = function(service, authInfo, path, json) {
      sent <<- list(path = path, json = json)
      list()
    }
  )
  client <- shinyAppsClient(list(), list())

  inviteApplicationUser(
    client,
    applicationId = 42,
    email = "alice@example.com",
    sendEmail = FALSE,
    emailMessage = "Welcome"
  )

  expect_equal(sent$path, "/applications/42/authorization/users")
  expect_equal(
    sent$json,
    list(
      email = "alice@example.com",
      invite_email = FALSE,
      invite_email_message = "Welcome"
    )
  )
})

test_that("inviteApplicationUser() omits the invitation options when they are NULL", {
  sent <- NULL
  local_mocked_bindings(
    POST_JSON = function(service, authInfo, path, json) {
      sent <<- json
      list()
    }
  )
  client <- shinyAppsClient(list(), list())

  inviteApplicationUser(client, 42, "alice@example.com")

  expect_equal(sent, list(email = "alice@example.com"))
})

test_that("removeApplicationUser() DELETEs the user from the application", {
  deleted <- NULL
  local_mocked_bindings(
    DELETE = function(service, authInfo, path, query) {
      deleted <<- path
      NULL
    }
  )
  client <- shinyAppsClient(list(), list())

  removeApplicationUser(client, applicationId = 42, userId = 101)

  expect_equal(deleted, "/applications/42/authorization/users/101")
})

test_that("resendApplicationInvitation() POSTs regenerate to the invitation", {
  sent <- NULL
  local_mocked_bindings(
    POST_JSON = function(service, authInfo, path, json) {
      sent <<- list(path = path, json = json)
      list()
    }
  )
  client <- shinyAppsClient(list(), list())

  resendApplicationInvitation(client, invitationId = 9, regenerate = TRUE)

  expect_equal(sent$path, "/invitations/9/send")
  expect_equal(sent$json, list(regenerate = TRUE))
})

test_that("listInvitations() maps the shinyapps.io invitation fields", {
  local_mocked_bindings(
    shinyappsListApplicationInvitations = function(client, applicationId) {
      list(
        list(
          id = 9,
          email = "alice@example.com",
          link = "https://shinyapps.io/invite/abc",
          expired = FALSE
        ),
        list(id = 10)
      )
    }
  )
  client <- fake_client("shinyAppsClient")

  expect_equal(
    listInvitations(client, 42),
    data.frame(
      id = c("9", "10"),
      email = c("alice@example.com", NA),
      link = c("https://shinyapps.io/invite/abc", NA),
      expired = c(FALSE, NA),
      stringsAsFactors = FALSE
    )
  )
})

test_that("listInvitations() returns an empty data frame when there are no invitations", {
  local_mocked_bindings(
    shinyappsListApplicationInvitations = function(client, applicationId) list()
  )
  client <- fake_client("shinyAppsClient")

  expect_equal(listInvitations(client, 42), emptyInvitations())
})

test_that("resolveContentTarget() finds the application with the client it gets", {
  local_mocked_bindings(
    clientForAccount = function(...) stop("built a second client"),
    listApplications.shinyAppsClient = function(client, accountId, ...) {
      list(list(id = 42, name = "other-app"), list(id = 43, name = "my-app"))
    }
  )
  client <- fake_client("shinyAppsClient")

  target <- resolveContentTarget(
    client,
    accountDetails = list(accountId = "1"),
    appDir = "my-app",
    appName = NULL
  )

  expect_equal(target, list(id = 43, deploymentFile = NULL))
})

test_that("listApplications() filters by account, Shiny type, and name", {
  sent <- list()
  local_mocked_bindings(
    listRequest = function(service, authInfo, path, query, ...) {
      sent[[length(sent) + 1]] <<- list(path = path, query = query)
      list()
    }
  )
  client <- shinyAppsClient(list(), list())

  listApplications(client, "1")
  listApplications(client, "1", filters = list(name = "my-app"))

  expect_equal(sent[[1]]$path, "/applications/")
  expect_equal(sent[[1]]$query, "filter=account_id:1&filter=type:shiny")
  expect_equal(
    sent[[2]]$query,
    "filter=account_id:1&filter=type:shiny&filter=name:my-app"
  )
})

test_that("getApplication() GETs the application and copies its id", {
  requested <- NULL
  local_mocked_bindings(
    GET = function(service, authInfo, path, ...) {
      requested <<- path
      list(id = 42, name = "my-app")
    }
  )
  client <- shinyAppsClient(list(), list())

  application <- getApplication(client, 42)

  expect_equal(requested, "/applications/42")
  expect_equal(application, list(id = 42, name = "my-app", application_id = 42))
})

test_that("currentUser() GETs the current user", {
  requested <- NULL
  local_mocked_bindings(
    GET = function(service, authInfo, path, ...) {
      requested <<- path
      list(id = 1, username = "me")
    }
  )
  client <- shinyAppsClient(list(), list())

  expect_equal(currentUser(client), list(id = 1, username = "me"))
  expect_equal(requested, "/users/current/")
})
