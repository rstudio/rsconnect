mockServerFactory <- function(initialResponses) {
  # Stock responses for certain endpoints. Can still be overridden by tests.
  if (is.null(initialResponses$"^GET /v1/users/current")) {
    initialResponses$"GET /v1/users/current" <- list(
      content = list(
        id = 100
      )
    )
  }

  if (is.null(initialResponses$"^GET /v1/accounts/?")) {
    initialResponses$"^GET /v1/accounts/?" <- list(
      content = list(
        count = 1,
        total = 1,
        offset = 0,
        accounts = list(
          list(
            id = 50,
            name = "testthat-account",
            account = "testthat-account"
          ),
          list(
            id = 51,
            name = "testthat-superfluous-account",
            account = "testthat-superfluous-account"
          )
        )
      )
    )
  }

  mockServer <- list(
    responses = initialResponses
  )

  mockServer$addResponse <- function(methodAndPath, response) {
    mockServer$responses <- append(mockServer$responses, response)
  }

  mockServer$impl <- function(protocol,
                          host,
                          port,
                          method,
                          path,
                          headers,
                          contentType = NULL,
                          contentFile = NULL,
                          certificate = NULL,
                          timeout = NULL) {

    methodAndPath <- paste(method, path)

    request <- list(
      protocol = protocol,
      host = host,
      port = port,
      method = method,
      path = path
    )

    response <- list(
      req = request,
      status = 200,
      location = "",
      contentType = "application/json"
    )

    found <- FALSE

    for (pathRegex in names(mockServer$responses)) {
      match <- regexec(pathRegex, methodAndPath)[[1]]
      if (match[1] != -1) {
        found <- TRUE
        responseSupplement <- mockServer$responses[[pathRegex]]

        for (respProperty in names(responseSupplement)) {
          if (is.function(responseSupplement[[respProperty]])) {
            responseSupplement[[respProperty]] <- responseSupplement[[respProperty]](
              methodAndPath,
              match,
              headers = headers,
              contentType = contentType,
              contentFile = contentFile)
          }

          response[[respProperty]] <- responseSupplement[[respProperty]]
        }

        if (is.list(response$content)) {
          response$content <- jsonlite::toJSON(response$content, auto_unbox = TRUE)
        }

        break
      }
    }

    if (!found) {
      stop(paste("No mocked response defined for", methodAndPath))
    }

    response
  }

  mockServer
}

configureTestAccount <- function(server = "posit.cloud", name = NULL) {
  if (is.null(name)) {
    name <- "testthat-account"
  }

  existingAccount <- NULL
  tryCatch(
    existingAccount <- accountInfo(name, server),
    error = function(e) { existingAccount <- NULL }
  )

  if (is.null(existingAccount)) {
    setAccountInfo(
      name = name,
      token = "foo",
      secret = "bar",
      server = server
    )
  }

  name
}

test_that("Get application", {
  mockServer <- mockServerFactory(list(
    "^GET /outputs/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        output_id <- strtoi(substr(methodAndPath, match[2], end))

        if (output_id != 5) {
          cli::cli_abort(
            "404 error",
            class = "rsconnect_http_404"
          )
        }

        list(
          "id" = 5,
          "source_id" = 10,
          "url" = "http://fake-url.test.me/",
          "state" = "active"
        )
      }
    ),
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))

        if (application_id != 10) {
          cli::cli_abort(
            "404 error",
            class = "rsconnect_http_404"
          )
        }


        list(
          "id" = application_id,
          "content_id" = 5
        )
      }
    )
  ))

  withr::local_options(rsconnect.http = mockServer$impl)

  fakeService <- list(
    protocol = "test",
    host = "unit-test",
    port = 42
  )
  client <- cloudClient(fakeService, NULL)

  app <- client$getApplication("10", NA)
  expect_equal(app$id, 5)
  expect_equal(app$application_id, 10)
  expect_equal(app$url, "http://fake-url.test.me/")

  app <- client$getApplication("5", 1)
  expect_equal(app$id, 5)
  expect_equal(app$application_id, 10)
  expect_equal(app$url, "http://fake-url.test.me/")

  app <- client$getApplication("5", "unknown")
  expect_equal(app$id, 5)
  expect_equal(app$application_id, 10)
  expect_equal(app$url, "http://fake-url.test.me/")

  app <- client$getApplication("10", "unknown")
  expect_equal(app$id, 5)
  expect_equal(app$application_id, 10)
  expect_equal(app$url, "http://fake-url.test.me/")
})

test_that("Get application output trashed", {
  mockServer <- mockServerFactory(list(
    "^GET /outputs/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        output_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id" = output_id,
          "source_id" = 1,
          "url" = "http://fake-url.test.me/",
          "state" = "trashed"
        )
      }
    ),
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id" = application_id,
          "content_id" = 5
        )
      }
    ),
    "^PATCH /outputs/5" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        output_id <- strtoi(substr(methodAndPath, match[2], end))
        list()
      }
    )
  ))

  withr::local_options(rsconnect.http = mockServer$impl)

  fakeService <- list(
    protocol = "test",
    host = "unit-test",
    port = 42
  )
  client <- cloudClient(fakeService, NULL)

  app <- client$getApplication(10, NA)

  expect_equal(app$id, 5)
  expect_equal(app$application_id, 10)
  expect_equal(app$url, "http://fake-url.test.me/")

  app <- client$getApplication(5, deploymentRecordVersion)

  expect_equal(app$id, 5)
  expect_equal(app$application_id, 1)
  expect_equal(app$url, "http://fake-url.test.me/")
})

test_that("Create application", {
  mockServer <- mockServerFactory(list(
    "^POST /outputs" = list(
      content = function(methodAndPath, match, contentFile, ...) {
        content <- jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))
        expect_equal(content$application_type, "connect")
        list(
          "id" = 1,
          "source_id" = 2,
          "url" = "http://fake-url.test.me/",
          "state" = "active"
        )
      }
    ),
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id" = application_id,
          "content_id" = 1
        )
      })
  ))

  withr::local_options(rsconnect.http = mockServer$impl)

  fakeService <- list(
    protocol = "test",
    host = "unit-test",
    port = 42
  )
  client <- cloudClient(fakeService, NULL)

  app <- client$createApplication("test app", "unused?", "unused?", "unused?", "shiny")

  expect_equal(app$id, 1)
  expect_equal(app$application_id, 2)
  expect_equal(app$url, "http://fake-url.test.me/")
})

test_that("Create application with space id", {
  mockServer <- mockServerFactory(list(
    "^POST /outputs" = list(
      content = function(methodAndPath, match, contentFile, ...) {
        content <- jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))
        expect_equal(content$application_type, "connect")
        expect_equal(content$space, 333)
        list(
          "id" = 1,
          "source_id" = 2,
          "url" = "http://fake-url.test.me/",
          "state" = "active"
        )
      }
    ),
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id" = application_id,
          "content_id" = 1
        )
      })
  ))

  withr::local_options(rsconnect.http = mockServer$impl)

  fakeService <- list(
    protocol = "test",
    host = "unit-test",
    port = 42
  )
  client <- cloudClient(fakeService, NULL)

  app <- client$createApplication("test app", "unused?", "unused?", "unused?", "shiny", spaceId = 333)

  expect_equal(app$id, 1)
  expect_equal(app$application_id, 2)
  expect_equal(app$url, "http://fake-url.test.me/")
})

test_that("Create static application", {
  mockServer <- mockServerFactory(list(
    "^POST /outputs" = list(
      content = function(methodAndPath, match, contentFile, ...) {
        content <- jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))
        expect_equal(content$application_type, "static")
        expect_equal(content$content_category, "document")
        list(
          "id" = 1,
          "source_id" = 2,
          "url" = "http://fake-url.test.me/",
          "state" = "active"
        )
      }
    ),
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id" = application_id,
          "content_id" = 1
        )
      })
  ))

  withr::local_options(rsconnect.http = mockServer$impl)

  fakeService <- list(
    protocol = "test",
    host = "unit-test",
    port = 42
  )
  client <- cloudClient(fakeService, NULL)

  app <- client$createApplication("test app", "unused?", "unused?", "unused?", "static", "document")

  expect_equal(app$id, 1)
  expect_equal(app$application_id, 2)
  expect_equal(app$url, "http://fake-url.test.me/")
})

test_that("Create static server-side-rendered application", {
  mockServer <- mockServerFactory(list(
    "^POST /outputs" = list(
      content = function(methodAndPath, match, contentFile, ...) {
        content <- jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))
        expect_equal(content$application_type, "static")
        expect_equal(content$render_by, "server")
        expect_equal(content$content_category, "document")
        list(
          "id" = 1,
          "source_id" = 2,
          "url" = "http://fake-url.test.me/",
          "state" = "active"
        )
      }
    ),
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id" = application_id,
          "content_id" = 1
        )
      })
  ))

  withr::local_options(rsconnect.http = mockServer$impl)

  fakeService <- list(
    protocol = "test",
    host = "unit-test",
    port = 42
  )
  client <- cloudClient(fakeService, NULL)

  app <- client$createApplication("test app", "unused?", "unused?", "unused?", "quarto-static", "document")

  expect_equal(app$id, 1)
  expect_equal(app$application_id, 2)
  expect_equal(app$url, "http://fake-url.test.me/")
})

test_that("deployApplication updates the parent project", {
  mockServer <- mockServerFactory(list(
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- substr(methodAndPath, match[2], end)
        expect_equal(application_id, "42")

        list(
          "id" = 42,
          "content_id" = 41
        )
      }
    ),
    "^PATCH /outputs" = list(
      content = function(methodAndPath, match, contentFile, ...) {
        content <- jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))
        if (!is.null(content$project)) {
          expect_equal(content$project, 41)
        } else {
          expect_equal(content$space, 333)
        }
        list(
          "id" = 41
        )
      }
    ),
    "^POST /applications/([0-9]+)/deploy" = list(
      content = function(methodAndPath, match, contentFile, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- substr(methodAndPath, match[2], end)
        expect_equal(application_id, "101/")

        content <- jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))
        expect_equal(content$rebuild, FALSE)
        list(
          "id" = 101
        )
      }
    )
  ))

  withr::local_options(rsconnect.http = mockServer$impl)
  withr::local_envvar(LUCID_APPLICATION_ID = "42")

  fakeService <- list(
    protocol = "test",
    host = "unit-test",
    port = 42
  )
  client <- cloudClient(fakeService, NULL)

  application <- list(
    "id" = 100,
    "application_id" = 101
  )
  client$deployApplication(application, spaceId = 333)
})

test_that("Create static RMD application", {
  mockServer <- mockServerFactory(list(
    "^POST /outputs" = list(
      content = function(methodAndPath, match, contentFile, ...) {
        content <- jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))
        expect_equal(content$application_type, "static")
        expect_equal(content$render_by, "server")
        expect_equal(content$content_category, "document")
        list(
          "id" = 1,
          "source_id" = 2,
          "url" = "http://fake-url.test.me/",
          "state" = "active"
        )
      }
    ),
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id" = application_id,
          "content_id" = 1
        )
      })
  ))

  withr::local_options(rsconnect.http = mockServer$impl)

  fakeService <- list(
    protocol = "test",
    host = "unit-test",
    port = 42
  )
  client <- cloudClient(fakeService, NULL)

  app <- client$createApplication("test app", "unused?", "unused?", "1", "rmd-static", "document")

  expect_equal(app$id, 1)
  expect_equal(app$application_id, 2)
  expect_equal(app$url, "http://fake-url.test.me/")
})

test_that("Create application with linked source project", {
  mockServer <- mockServerFactory(list(
    "^POST /outputs" = list(
      content = function(methodAndPAth, match, contentFile, ...) {
        content <- jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))

        expect_equal(content$project, 41)
        expect_equal(content$space, 99)

        list(
        "id" = 1,
        "source_id" = 2,
        "url" = "http://fake-url.test.me/",
        "state" = "active"
        )
      }
    ),
    "^GET /applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))
        list(
          "id" = application_id,
          "content_id" = application_id - 1
        )
      }
    ),
    "^GET /content/41" = list(
      content = list(
        "id" = 41,
        "space_id" = 99
      )
    )
  ))

  withr::local_options(rsconnect.http = mockServer$impl)

  Sys.setenv(LUCID_APPLICATION_ID = "42")
  withr::defer(Sys.unsetenv("LUCID_APPLICATION_ID"))

  fakeService <- list(
    protocol = "test",
    host = "unit-test",
    port = 42
  )
  client <- cloudClient(fakeService, NULL)

  app <- client$createApplication("test app", "unused?", "unused?", "unused?", "static")

  expect_equal(app$id, 1)
  expect_equal(app$application_id, 2)
  expect_equal(app$url, "http://fake-url.test.me/")
})

test_that("findDeploymentTarget() results in correct Cloud API calls when given appId", {
  local_temp_config()

  mockServer <- mockServerFactory(list(
    "^GET /v1/applications/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))
        list(
          "id" = application_id,
          "content_id" = application_id - 1,
          "name" = paste("testthat app", application_id)
        )
      }
    ),
    "^GET /v1/outputs/([0-9]+)" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        output_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          "id" = output_id,
          "source_id" = output_id + 1,
          "url" = "http://fake-url.test.me/",
          "state" = "active",
          "name" = "my output"
        )
      }
    )
  ))

  withr::local_options(rsconnect.http = mockServer$impl)

  testAccount <- configureTestAccount()
  withr::defer(removeAccount(testAccount))

  target <- findDeploymentTarget(
    appId = 3,
    account = testAccount,
    server = "posit.cloud",
  )

  accountDetails <- target$accountDetails
  deployment <- target$deployment

  expect_equal(deployment$name, "my output")
  expect_equal(deployment$account, testAccount)
  expect_equal(deployment$server, "posit.cloud")
  expect_equal(deployment$appId, 3)
})

deployAppMockServerFactory <- function(expectedAppType, outputState) {
  outputResponse <- list(
    "id" = 1,
    "source_id" = 2,
    "url" = "http://fake-url.test.me/",
    "state" = outputState
  )

  actualCalls <- list(
    outputCreated = FALSE,
    outputStateUpdated = FALSE,
    revisionCreated = FALSE,
    bundleCreated = FALSE,
    bundleUploaded = FALSE,
    bundleReady = FALSE,
    deployStarted = FALSE
  )

  server <- mockServerFactory(list(
    # An attempt to search for preexisting applications with the same name.
    # This call should change, see https://github.com/rstudio/rsconnect/issues/808
    "^GET /v1/applications/[?]filter=account_id:50&filter=type:connect" = list(
      content = list(
        count = 0,
        total = 0,
        applications = list()
      )
    ),
    "^POST /v1/outputs$" = list(
      content = function(methodAndPath, match, contentFile, ...) {
        e <- environment()
        p <- parent.env(e)
        p$actualCalls$outputCreated <- TRUE

        content <- jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))

        expect_equal(content$application_type, expectedAppType)
        expect_equal(content$name, "Desired name here")

        outputResponse
      }
    ),
    "^GET /v1/applications/([0-9]+)$" = list(
      content = function(methodAndPath, match, ...) {
        end <- attr(match, "match.length")[2] + match[2]
        application_id <- strtoi(substr(methodAndPath, match[2], end))

        list(
          id = application_id,
          content_id = application_id - 1,
          name = paste("testthat app", application_id),
          type = expectedAppType
        )
      }
    ),
    "^POST /v1/bundles/123/status$" = list(
      content = function(methodAndPath, match, contentFile, ...) {
        e <- environment()
        p <- parent.env(e)
        p$actualCalls$bundleReady <- TRUE

        content <- jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))
        expect_equal(content$status, "ready")

        list()
      }
    ),
    "^POST /v1/bundles$" = list(
      content = function(methodAndPath, match, contentFile, ...) {
        e <- environment()
        p <- parent.env(e)
        p$actualCalls$bundleCreated <- TRUE

        content <- jsonlite::fromJSON(readChar(contentFile, file.info(contentFile)$size))

        list(
          id = 123,
          presigned_url = "https://write-only-memory.com/fake-presigned-url",
          presigned_checksum = content$checksum
        )
      }
    ),
    "^PUT /fake-presigned-url" = list(
      content = function(...) {
        e <- environment()
        p <- parent.env(e)
        p$actualCalls$bundleUploaded <- TRUE
      }
    ),
    "^GET /v1/bundles/123$" = list(
      content = list(
        id = 123,
        status = "ready"
      )
    ),
    "^POST /v1/applications/[23]/deploy$" = list(
      content = function(...) {
        e <- environment()
        p <- parent.env(e)
        p$actualCalls$deployStarted <- TRUE
        list(
          task_id = "testthat-task-id"
        )
      }
    ),
    "^GET /v1/tasks/testthat-task-id$" = list(
      content = list(
        status = "success",
        finished = TRUE
      )
    ),
    # Fetch existing output when re-deploying from .dcf data
    "^GET /v1/outputs/1$" = list(
      content = outputResponse
    ),
    "^POST /v1/outputs/1/revisions" = list(
      content = function(...) {
        e <- environment()
        p <- parent.env(e)
        p$actualCalls$revisionCreated <- TRUE

        list(
          application_id = 3
        )
      }
    ),
    "^PATCH /v1/outputs" = list(
      content = function(...) {
        e <- environment()
        p <- parent.env(e)
        p$actualCalls$outputStateUpdated <- TRUE
      }
    )
  ))

  expect_calls <- function(expectedCalls) {
    expect_equal(actualCalls, expectedCalls)
  }

  reset_calls <- function() {
    e <- environment()
    p <- parent.env(e)
    p$actualCalls <- list(
      outputCreated = FALSE,
      outputStateUpdated = FALSE,
      revisionCreated = FALSE,
      bundleCreated = FALSE,
      bundleUploaded = FALSE,
      bundleReady = FALSE,
      deployStarted = FALSE
    )
  }

  list(
    server = server,
    expect_calls = expect_calls,
    reset_calls = reset_calls
  )
}

test_that("deployApp() for shiny results in correct Cloud API calls", {
  skip_on_cran()

  local_temp_config()

  withr::local_options(renv.verbose = TRUE)

  mock <- deployAppMockServerFactory(expectedAppType = "connect", outputState = "active")
  mockServer <- mock$server

  withr::local_options(rsconnect.http = mockServer$impl)

  testAccount <- configureTestAccount()
  withr::defer(removeAccount(testAccount))

  sourcePath <- test_path("shinyapp-simple")
  # Remove local deployment info at end for reproducibility and tidiness.
  withr::defer(forgetDeployment(appPath = sourcePath, force = TRUE))

  deployApp(
    appName = "Desired name here",
    appDir = sourcePath,
    server = "posit.cloud",
    account = testAccount,
    logLevel = "quiet"
  )

  mock$expect_calls(list(
    outputCreated = TRUE,
    outputStateUpdated = FALSE,
    revisionCreated = FALSE,
    bundleCreated = TRUE,
    bundleUploaded = TRUE,
    bundleReady = TRUE,
    deployStarted = TRUE
  ))

  mock$reset_calls()

  # deploy again to test existing deployment path
  deployApp(
    appDir = sourcePath,
    logLevel = "quiet"
  )

  mock$expect_calls(list(
    outputCreated = FALSE,
    outputStateUpdated = FALSE,
    revisionCreated = FALSE,
    bundleCreated = TRUE,
    bundleUploaded = TRUE,
    bundleReady = TRUE,
    deployStarted = TRUE
  ))

  # Start over, add another posit.cloud account and test again with that environment
  mock$reset_calls()
  forgetDeployment(appPath = sourcePath, force = TRUE)

  extraLocalAccount <- configureTestAccount(name = "testthat-superfluous-account")
  withr::defer(removeAccount(extraLocalAccount))

  deployApp(
    appName = "Desired name here",
    appDir = sourcePath,
    server = "posit.cloud",
    account = testAccount,
    logLevel = "quiet"
  )

  mock$expect_calls(list(
    outputCreated = TRUE,
    outputStateUpdated = FALSE,
    revisionCreated = FALSE,
    bundleCreated = TRUE,
    bundleUploaded = TRUE,
    bundleReady = TRUE,
    deployStarted = TRUE
  ))

  mock$reset_calls()

  # deploy again to test existing deployment path
  deployApp(
    appDir = sourcePath,
    logLevel = "quiet"
  )

  mock$expect_calls(list(
    outputCreated = FALSE,
    outputStateUpdated = FALSE,
    revisionCreated = FALSE,
    bundleCreated = TRUE,
    bundleUploaded = TRUE,
    bundleReady = TRUE,
    deployStarted = TRUE
  ))
})

test_that("deployDoc() results in correct Cloud API calls", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  local_temp_config()

  mock <- deployAppMockServerFactory(expectedAppType = "static", outputState = "active")
  mockServer <- mock$server

  withr::local_options(rsconnect.http = mockServer$impl)

  testAccount <- configureTestAccount()
  withr::defer(removeAccount(testAccount))

  sourcePath <- test_path("static-with-quarto-yaml")
  # Remove local deployment info at end for reproducibility and tidiness.
  withr::defer(forgetDeployment(appPath = sourcePath, force = TRUE))

  deployDoc(
    paste(sourcePath, "slideshow.html", sep = "/"),
    appName = "Desired name here",
    server = "posit.cloud",
    account = testAccount,
    logLevel = "quiet"
  )

  mock$expect_calls(list(
    outputCreated = TRUE,
    outputStateUpdated = FALSE,
    revisionCreated = FALSE,
    bundleCreated = TRUE,
    bundleUploaded = TRUE,
    bundleReady = TRUE,
    deployStarted = TRUE
  ))

  mock$reset_calls()

  # deploy again to test existing deployment path
  deployApp(
    appDir = sourcePath,
    logLevel = "quiet"
  )

  mock$expect_calls(list(
    outputCreated = FALSE,
    outputStateUpdated = FALSE,
    revisionCreated = TRUE,
    bundleCreated = TRUE,
    bundleUploaded = TRUE,
    bundleReady = TRUE,
    deployStarted = TRUE
  ))
})
