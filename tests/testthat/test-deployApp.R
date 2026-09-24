test_that("appDir must be an existing directory", {
  expect_snapshot(error = TRUE, {
    deployApp(1)
    deployApp("doesntexist")
  })
})

test_that("appPrimaryDoc must exist, if supplied", {
  skip_on_cran()
  dir <- local_temp_app()

  expect_snapshot(error = TRUE, {
    deployApp(dir, appPrimaryDoc = c("foo.Rmd", "bar.Rmd"))
    deployApp(dir, appPrimaryDoc = "foo.Rmd")
  })
})

test_that("startup scripts are logged by default", {
  dir <- local_temp_app()
  withr::local_dir(dir)
  writeLines("1 + 1", file.path(dir, ".rsconnect_profile"))

  expect_snapshot(runStartupScripts("."))
})

# record directory --------------------------------------------------------

test_that("findRecordPath() uses recordDir, then appPrimaryDoc, then appDir", {
  expect_equal(findRecordPath("a"), "a")
  expect_equal(findRecordPath("a", recordDir = "b"), "b")
  expect_equal(findRecordPath("a", appPrimaryDoc = "c"), "a/c")
})

# app visibility ----------------------------------------------------------

test_that("needsVisibilityChange() returns FALSE when no change needed", {
  dummyApp <- function(visibility) {
    list(
      deployment = list(
        properties = list(
          application.visibility = visibility
        )
      )
    )
  }

  expect_false(needsVisibilityChange("connect.com"))
  expect_false(needsVisibilityChange("shinyapps.io", dummyApp("public"), NULL))
  expect_false(needsVisibilityChange(
    "shinyapps.io",
    dummyApp("public"),
    "public"
  ))
  expect_true(needsVisibilityChange("shinyapps.io", dummyApp(NULL), "private"))
  expect_true(needsVisibilityChange(
    "shinyapps.io",
    dummyApp("public"),
    "private"
  ))
})

test_that("checkConnectSupportsNodejs errors for old server versions", {
  client <- list(
    serverSettings = function() list(version = "2025.12.0")
  )
  expect_error(
    checkConnectSupportsNodejs(client),
    "2026.04.0"
  )
})

test_that("checkConnectSupportsNodejs passes for supported server versions", {
  client <- list(
    serverSettings = function() list(version = "2026.04.0")
  )
  expect_no_error(checkConnectSupportsNodejs(client))

  client <- list(
    serverSettings = function() list(version = "2026.05.0")
  )
  expect_no_error(checkConnectSupportsNodejs(client))

  client <- list(
    serverSettings = function() list(version = "2026.05.0-dev+54-sdlkfjsd")
  )
  expect_no_error(checkConnectSupportsNodejs(client))
})

test_that("checkConnectSupportsNodejs messages when version is unavailable", {
  client <- list(
    serverSettings = function() list(version = "")
  )
  expect_message(
    checkConnectSupportsNodejs(client),
    "Could not determine"
  )
})

test_that("checkConnectSupportsNodejs messages when serverSettings errors", {
  client <- list(
    serverSettings = function() stop("connection failed")
  )
  expect_message(
    checkConnectSupportsNodejs(client),
    "Could not determine"
  )
})

test_that("checkConnectSupportsNodejs messages when version is unparseable", {
  client <- list(
    serverSettings = function() list(version = "not-a-version")
  )
  expect_message(
    checkConnectSupportsNodejs(client),
    "Could not determine"
  )
})

test_that("connectVersionLt compares versions correctly", {
  expect_true(connectVersionLt("2025.12.0", "2026.04.0"))
  expect_false(connectVersionLt("2026.04.0", "2026.04.0"))
  expect_false(connectVersionLt("2026.05.0", "2026.04.0"))
  expect_false(connectVersionLt("2027.01.0", "2026.04.0"))
})

test_that("connectVersionLt handles dev versions gracefully", {
  expect_false(connectVersionLt("2026.04.0-dev+67", "2026.04.0"))
})

test_that("connectVersionLt returns NA for unparseable or missing input", {
  expect_true(is.na(connectVersionLt("garbage", "2026.04.0")))
  expect_true(is.na(connectVersionLt(NULL, "2026.04.0")))
  expect_true(is.na(connectVersionLt("", "2026.04.0")))
})

test_that("deployHook executes function if set", {
  withr::local_options(rsconnect.pre.deploy = NULL)
  expect_equal(
    runDeploymentHook("PATH", "rsconnect.pre.deploy"),
    NULL
  )

  withr::local_options(rsconnect.pre.deploy = function(path) path)
  expect_equal(
    runDeploymentHook("PATH", "rsconnect.pre.deploy"),
    "PATH"
  )
  expect_snapshot(
    . <- runDeploymentHook("PATH", "rsconnect.pre.deploy", verbose = TRUE)
  )
})

# deleted apps ------------------------------------------------------------

test_that("applicationDeleted() errors or prompts as needed", {
  local_temp_config()
  addTestServer("s")
  addTestAccount("a", "s")
  app <- local_temp_app()
  addTestDeployment(app, appName = "name", account = "a", server = "s")
  target <- createDeployment("name", "title", "id", NULL, "a", "a", "s", 1)
  client <- list(createApplication = function(...) NULL)

  expect_snapshot(applicationDeleted(client, target, app), error = TRUE)
  expect_length(dir(app, recursive = TRUE), 1)

  simulate_user_input(2)
  expect_snapshot(. <- applicationDeleted(client, target, app))
  expect_length(dir(app, recursive = TRUE), 0)
})

# envvars -----------------------------------------------------------------

test_that("deployApp() errors if envVars is given a named vector", {
  expect_snapshot(error = TRUE, {
    deployApp(local_temp_app(), envVars = c("FLAG" = "true"))
  })
})

# with manifestPath arg ---------------------------------------------------

test_that("manifestPath must exist", {
  skip_on_cran()
  appDir <- local_temp_app(list(app.R = "# shiny app"))

  expect_error(
    deployApp(appDir, manifestPath = "manifest.json"),
    "Manifest file not found"
  )
})

test_that("manifest file must be valid JSON", {
  skip_on_cran()
  appDir <- local_temp_app(list(app.R = "# shiny app"))
  writeLines("not valid json {", file.path(appDir, "manifest.json"))

  expect_error(
    deployApp(appDir, manifestPath = "manifest.json"),
    "invalid string in json text"
  )
})

test_that("manifest must contain required fields", {
  skip_on_cran()
  appDir <- local_temp_app(list(app.R = "# shiny app"))

  # Empty manifest
  writeLines("{}", file.path(appDir, "manifest.json"))
  expect_snapshot(error = TRUE, {
    deployApp(appDir, manifestPath = "manifest.json")
  })

  # Manifest without appmode
  writeLines(
    '{"metadata": {}, "files": {}}',
    file.path(appDir, "manifest.json")
  )
  expect_snapshot(error = TRUE, {
    deployApp(appDir, manifestPath = "manifest.json")
  })
})

test_that("manifest must contain files", {
  skip_on_cran()
  appDir <- local_temp_app(list(app.R = "# shiny app"))
  writeLines(
    '{"metadata": {"appmode": "shiny"}, "files": {}}',
    file.path(appDir, "manifest.json")
  )

  expect_snapshot(error = TRUE, {
    deployApp(appDir, manifestPath = "manifest.json")
  })
})

test_that("all files in manifest must exist in appDir", {
  skip_on_cran()
  appDir <- local_temp_app(list(app.R = "# shiny app"))
  writeManifest(appDir, quiet = TRUE)

  # Add a non-existent file to the manifest
  manifestPath <- file.path(appDir, "manifest.json")
  manifest <- jsonlite::fromJSON(manifestPath)
  manifest$files$missing.R <- list(checksum = "abc123")
  writeLines(
    jsonlite::toJSON(manifest, auto_unbox = TRUE),
    manifestPath
  )

  expect_snapshot(error = TRUE, {
    deployApp(appDir, manifestPath = "manifest.json")
  })
})

test_that("manifestPath ignored when NULL", {
  skip_on_cran()
  appDir <- local_temp_app(list(app.R = "# shiny app"))

  # Should work without manifest
  expect_no_error({
    # Will error later in deployment, but not due to missing manifest
    tryCatch(
      deployApp(appDir, manifestPath = NULL, server = "fake-server"),
      error = function(e) {
        # Expected to fail on server lookup, not manifest
        print(e)
        expect_false(grepl("manifest", e$message, ignore.case = TRUE))
      }
    )
  })
})

# confirmDependencySourceLibrary -------------------------------------------

test_that("confirmDependencySourceLibrary proceeds when user answers Y", {
  simulate_user_input("Y")
  expect_message(
    confirmDependencySourceLibrary(),
    "renv.lock will be ignored"
  )
})

test_that("confirmDependencySourceLibrary proceeds on empty input (default Y)", {
  simulate_user_input("")
  expect_message(
    confirmDependencySourceLibrary(),
    "renv.lock will be ignored"
  )
})

test_that("confirmDependencySourceLibrary aborts when user answers n", {
  simulate_user_input("n")
  expect_error(
    suppressMessages(confirmDependencySourceLibrary()),
    "Deployment cancelled"
  )
})

test_that("confirmDependencySourceLibrary informs non-interactively", {
  expect_message(
    confirmDependencySourceLibrary(),
    "renv.lock.*will be ignored"
  )
})

test_that("openURL() does not launch the browser on success with no valid url", {
  # e.g. Connect Cloud's awaitCompletion() falling back to url = "" when it
  # can't resolve the content's owning account.
  launched <- FALSE
  openURL(
    client = NULL,
    application = list(url = "", dashboard_url = NULL),
    server = "connect.posit.cloud",
    launch.browser = function(url) launched <<- TRUE,
    on.failure = function(url) {
      stop("on.failure should not be called on success")
    },
    deploymentSucceeded = TRUE
  )
  expect_false(launched)
})

test_that("openURL() launches the browser on success with a valid url", {
  launched <- FALSE
  openURL(
    client = NULL,
    application = list(
      url = "https://connect.posit.cloud/acct/content/abc123",
      dashboard_url = NULL
    ),
    server = "connect.posit.cloud",
    launch.browser = function(url) launched <<- TRUE,
    on.failure = function(url) {
      stop("on.failure should not be called on success")
    },
    deploymentSucceeded = TRUE
  )
  expect_true(launched)
})

# PCC deploy: updateContent / isNewContent guard -------------------------
# Shared fixtures (pcc_existing_content_*, local_pcc_deploy_env) live in
# helper.R.

test_that("existing PCC content with NULL current_revision calls updateContent", {
  skip_on_cran()
  appDir <- local_temp_app(list("app.R" = "library(shiny)"))
  local_pcc_deploy_env(appDir)

  # updateContent() mints a fresh upload URL; the bundle must go there, not to
  # the stale URL from the getContent() response.
  old_upload_url <-
    pcc_existing_content_null_revision$next_revision$source_bundle_upload_url
  new_upload_url <- "https://upload.example.com/bundle-fresh"
  updated_content <- pcc_existing_content_null_revision
  updated_content$next_revision$source_bundle_upload_url <- new_upload_url

  update_content_called <- FALSE
  uploaded_url <- NULL
  local_mocked_bindings(
    clientForAccount = function(...) {
      fake_client(
        "connectCloudClient",
        getContent = function(id) pcc_existing_content_null_revision,
        updateContent = function(id, envVars, newBundle, primaryFile, appMode) {
          update_content_called <<- TRUE
          updated_content
        },
        uploadBundle = function(bundlePath, url) {
          uploaded_url <<- url
          TRUE
        },
        publish = function(id) invisible(NULL),
        awaitCompletion = function(revisionId) {
          list(
            success = TRUE,
            url = "https://connect.posit.cloud/myaccount/content/content-abc"
          )
        }
      )
    },
    bundleApp = function(...) {
      tmp <- tempfile(fileext = ".tar.gz")
      file.create(tmp)
      tmp
    }
  )

  suppressMessages(deployApp(
    appDir,
    appName = "myapp",
    account = "myaccount",
    server = "connect.posit.cloud",
    logLevel = "quiet",
    lint = FALSE,
    launch.browser = FALSE
  ))

  expect_true(update_content_called)
  expect_equal(uploaded_url, new_upload_url)
  expect_false(identical(uploaded_url, old_upload_url))
})

test_that("newly-created PCC content (no prior record) does NOT call updateContent", {
  skip_on_cran()
  appDir <- local_temp_app(list("app.R" = "library(shiny)"))
  local_pcc_deploy_env(appDir, appId = NULL)

  update_content_called <- FALSE
  local_mocked_bindings(
    clientForAccount = function(...) {
      fake_client(
        "connectCloudClient",
        createContent = function(...) pcc_existing_content_null_revision,
        updateContent = function(...) {
          update_content_called <<- TRUE
          pcc_existing_content_null_revision
        },
        uploadBundle = function(bundlePath, url) TRUE,
        publish = function(id) invisible(NULL),
        awaitCompletion = function(revisionId) {
          list(
            success = TRUE,
            url = "https://connect.posit.cloud/myaccount/content/content-abc"
          )
        }
      )
    },
    bundleApp = function(...) {
      tmp <- tempfile(fileext = ".tar.gz")
      file.create(tmp)
      tmp
    }
  )

  suppressMessages(deployApp(
    appDir,
    appName = "myapp",
    account = "myaccount",
    server = "connect.posit.cloud",
    logLevel = "quiet",
    lint = FALSE,
    launch.browser = FALSE
  ))

  expect_false(update_content_called)
})

test_that("existing PCC content with non-null current_revision still calls updateContent", {
  skip_on_cran()
  appDir <- local_temp_app(list("app.R" = "library(shiny)"))
  local_pcc_deploy_env(appDir)

  update_content_called <- FALSE
  local_mocked_bindings(
    clientForAccount = function(...) {
      fake_client(
        "connectCloudClient",
        getContent = function(id) pcc_existing_content_with_revision,
        updateContent = function(...) {
          update_content_called <<- TRUE
          pcc_existing_content_with_revision
        },
        uploadBundle = function(bundlePath, url) TRUE,
        publish = function(id) invisible(NULL),
        awaitCompletion = function(revisionId) {
          list(
            success = TRUE,
            url = "https://connect.posit.cloud/myaccount/content/content-abc"
          )
        }
      )
    },
    bundleApp = function(...) {
      tmp <- tempfile(fileext = ".tar.gz")
      file.create(tmp)
      tmp
    }
  )

  suppressMessages(deployApp(
    appDir,
    appName = "myapp",
    account = "myaccount",
    server = "connect.posit.cloud",
    logLevel = "quiet",
    lint = FALSE,
    launch.browser = FALSE
  ))

  expect_true(update_content_called)
})

test_that("deployApp(upload=FALSE) on PCC does not error with 'bundle not found'", {
  skip_on_cran()
  appDir <- local_temp_app(list("app.R" = "library(shiny)"))
  local_pcc_deploy_env(appDir)

  local_mocked_bindings(
    clientForAccount = function(...) {
      fake_client(
        "connectCloudClient",
        getContent = function(id) pcc_existing_content_with_revision,
        updateContent = function(...) pcc_existing_content_with_revision,
        publish = function(id) invisible(NULL),
        awaitCompletion = function(revisionId) {
          list(
            success = TRUE,
            url = "https://connect.posit.cloud/myaccount/content/content-abc"
          )
        }
      )
    }
  )

  expect_no_error(suppressMessages(deployApp(
    appDir,
    appName = "myapp",
    account = "myaccount",
    server = "connect.posit.cloud",
    upload = FALSE,
    logLevel = "quiet",
    lint = FALSE,
    launch.browser = FALSE
  )))
})

test_that("fresh Connect deploy uploads to the newly created app, not an existing one", {
  skip_on_cran()
  appDir <- local_temp_app(list("app.R" = "library(shiny)"))
  local_temp_config()
  addTestServer()
  addTestAccount("myaccount")

  uploaded_guid <- NULL
  local_mocked_bindings(
    clientForAccount = function(...) {
      fake_client(
        "connectClient",
        # No local deployment record, so deployApp() checks the server for an
        # app with a matching name before deciding this is a fresh deploy.
        listApplications = function(...) list(),
        createApplication = function(...) {
          list(
            id = "99",
            guid = "guid-new",
            url = "https://example.com/content/99",
            dashboard_url = "https://example.com/connect/#/apps/guid-new"
          )
        },
        getApplication = function(...) {
          stop("getApplication() should not be called for a fresh deploy")
        },
        uploadBundle = function(contentGuid, bundlePath) {
          uploaded_guid <<- contentGuid
          list(id = "bundle-1")
        },
        deployApplication = function(...) list(id = "task-1"),
        waitForTask = function(...) list()
      )
    },
    bundleApp = function(...) {
      tmp <- tempfile(fileext = ".tar.gz")
      file.create(tmp)
      tmp
    }
  )

  suppressMessages(deployApp(
    appDir,
    appName = "myapp",
    account = "myaccount",
    server = "example.com",
    logLevel = "quiet",
    lint = FALSE,
    launch.browser = FALSE
  ))

  expect_equal(uploaded_guid, "guid-new")
})

test_that("redeploy to Connect uploads to the existing app, not a new one", {
  skip_on_cran()
  appDir <- local_temp_app(list("app.R" = "library(shiny)"))
  local_temp_config()
  addTestServer()
  addTestAccount("myaccount")
  addTestDeployment(
    appDir,
    appName = "myapp",
    appId = "42",
    account = "myaccount"
  )

  # A deployment record exists, so this is a redeploy: it must upload to the
  # recorded content rather than create new content.
  uploaded_guid <- NULL
  local_mocked_bindings(
    clientForAccount = function(...) {
      fake_client(
        "connectClient",
        createApplication = function(...) {
          stop("createApplication() should not be called for a redeploy")
        },
        getApplication = function(...) {
          list(
            id = "42",
            guid = "guid-42",
            url = "https://example.com/content/42",
            dashboard_url = "https://example.com/connect/#/apps/guid-42"
          )
        },
        uploadBundle = function(contentGuid, bundlePath) {
          uploaded_guid <<- contentGuid
          list(id = "bundle-1")
        },
        deployApplication = function(...) list(id = "task-1"),
        waitForTask = function(...) list()
      )
    },
    bundleApp = function(...) {
      tmp <- tempfile(fileext = ".tar.gz")
      file.create(tmp)
      tmp
    }
  )

  suppressMessages(deployApp(
    appDir,
    appName = "myapp",
    account = "myaccount",
    server = "example.com",
    logLevel = "quiet",
    lint = FALSE,
    launch.browser = FALSE
  ))

  expect_equal(uploaded_guid, "guid-42")
})

test_that("fresh shinyapps.io deploy uploads to a newly created app, not an existing one", {
  skip_on_cran()
  appDir <- local_temp_app(list("app.R" = "library(shiny)"))
  local_temp_config()
  addTestServer(name = "shinyapps.io", url = "https://shinyapps.io")
  addTestAccount("myaccount", server = "shinyapps.io")

  uploaded_id <- NULL
  local_mocked_bindings(
    clientForAccount = function(...) {
      fake_client(
        "shinyAppsClient",
        listApplications = function(...) list(),
        createApplication = function(...) {
          list(
            id = "new-1",
            application_id = "new-1",
            url = "https://myaccount.shinyapps.io/myapp/"
          )
        },
        getApplication = function(...) {
          stop("getApplication() should not be called for a fresh deploy")
        },
        deployApplication = function(...) list(id = "task-1"),
        waitForTask = function(...) list()
      )
    },
    bundleApp = function(...) {
      tmp <- tempfile(fileext = ".tar.gz")
      file.create(tmp)
      tmp
    },
    uploadShinyappsBundle = function(client, application_id, bundlePath) {
      uploaded_id <<- application_id
      list(id = "bundle-1")
    }
  )

  suppressMessages(deployApp(
    appDir,
    appName = "myapp",
    account = "myaccount",
    server = "shinyapps.io",
    logLevel = "quiet",
    lint = FALSE,
    launch.browser = FALSE
  ))

  expect_equal(uploaded_id, "new-1")
})

test_that("redeploy to shinyapps.io uploads to the existing app, not a new one", {
  skip_on_cran()
  appDir <- local_temp_app(list("app.R" = "library(shiny)"))
  local_temp_config()
  addTestServer(name = "shinyapps.io", url = "https://shinyapps.io")
  addTestAccount("myaccount", server = "shinyapps.io")
  addTestDeployment(
    appDir,
    appName = "myapp",
    appId = "99",
    account = "myaccount",
    server = "shinyapps.io"
  )

  # A deployment record exists, so this is a redeploy: it must upload to the
  # recorded app rather than create a new one.
  uploaded_id <- NULL
  local_mocked_bindings(
    clientForAccount = function(...) {
      fake_client(
        "shinyAppsClient",
        createApplication = function(...) {
          stop("createApplication() should not be called for a redeploy")
        },
        getApplication = function(...) {
          list(
            id = "99",
            application_id = "99",
            url = "https://myaccount.shinyapps.io/myapp/",
            deployment = list(bundle = list(id = "bundle-old"))
          )
        },
        deployApplication = function(...) list(id = "task-1"),
        waitForTask = function(...) list()
      )
    },
    bundleApp = function(...) {
      tmp <- tempfile(fileext = ".tar.gz")
      file.create(tmp)
      tmp
    },
    uploadShinyappsBundle = function(client, application_id, bundlePath) {
      uploaded_id <<- application_id
      list(id = "bundle-1")
    }
  )

  suppressMessages(deployApp(
    appDir,
    appName = "myapp",
    account = "myaccount",
    server = "shinyapps.io",
    logLevel = "quiet",
    lint = FALSE,
    launch.browser = FALSE
  ))

  expect_equal(uploaded_id, "99")
})

test_that("deployApp(upload=FALSE) on shinyapps.io does not error", {
  skip_on_cran()
  appDir <- local_temp_app(list("app.R" = "library(shiny)"))
  local_temp_config()
  addTestServer(name = "shinyapps.io", url = "https://shinyapps.io")
  addTestAccount("myaccount", server = "shinyapps.io")
  addTestDeployment(
    appDir,
    appName = "myapp",
    appId = "99",
    account = "myaccount",
    server = "shinyapps.io"
  )

  # Application as returned by getApplication(), with an existing
  # deployment/bundle, as returned for an app that has already been deployed
  # once.
  shinyapps_app_with_bundle <- list(
    id = "99",
    application_id = "99",
    url = "https://myaccount.shinyapps.io/myapp/",
    deployment = list(bundle = list(id = "bundle-old"))
  )

  local_mocked_bindings(
    clientForAccount = function(...) {
      fake_client(
        "shinyAppsClient",
        getApplication = function(...) shinyapps_app_with_bundle,
        deployApplication = function(...) list(id = "task-1"),
        waitForTask = function(...) list()
      )
    }
  )

  expect_no_error(suppressMessages(deployApp(
    appDir,
    appName = "myapp",
    account = "myaccount",
    server = "shinyapps.io",
    upload = FALSE,
    logLevel = "quiet",
    lint = FALSE,
    launch.browser = FALSE
  )))
})

test_that("deployApp() aborts for Node.js content on shinyapps.io", {
  skip_on_cran()
  local_temp_config()
  appDir <- local_nodejs_app()
  local_mocked_bindings(
    findDeploymentTarget = function(...) {
      list(
        accountDetails = list(name = "shinyapps-user", server = "shinyapps.io"),
        deployment = list(name = "nodejs-app", appId = NULL)
      )
    }
  )

  expect_error(
    deployApp(
      appDir,
      appName = "nodejs-app",
      account = "shinyapps-user",
      server = "shinyapps.io",
      logLevel = "quiet",
      lint = FALSE,
      launch.browser = FALSE
    ),
    regexp = "Node\\.js content is not supported on shinyapps\\.io"
  )
})

test_that("deployApp() aborts for Node.js content on Connect Cloud", {
  skip_on_cran()
  local_temp_config()
  appDir <- local_nodejs_app()
  local_mocked_bindings(
    findDeploymentTarget = function(...) {
      list(
        accountDetails = list(
          name = "cloud-user",
          server = "connect.posit.cloud"
        ),
        deployment = list(name = "nodejs-app", appId = NULL)
      )
    }
  )

  expect_error(
    deployApp(
      appDir,
      appName = "nodejs-app",
      account = "cloud-user",
      server = "connect.posit.cloud",
      logLevel = "quiet",
      lint = FALSE,
      launch.browser = FALSE
    ),
    regexp = "Node\\.js content is not supported on Posit Connect Cloud"
  )
})

test_that("deployApp(envVars = ) aborts on shinyapps.io", {
  skip_on_cran()
  local_temp_config()
  appDir <- local_temp_app(list("app.R" = "library(shiny)"))
  local_mocked_bindings(
    findDeploymentTarget = function(...) {
      list(
        accountDetails = list(name = "shinyapps-user", server = "shinyapps.io"),
        deployment = list(name = "myapp", appId = "42")
      )
    }
  )

  expect_error(
    deployApp(
      appDir,
      appName = "myapp",
      account = "shinyapps-user",
      server = "shinyapps.io",
      envVars = "FOO",
      logLevel = "quiet",
      lint = FALSE,
      launch.browser = FALSE
    ),
    regexp = "shinyapps\\.io does not support setting `envVars`"
  )
})

test_that("deployApp(upload = FALSE) aborts on Posit Connect", {
  skip_on_cran()
  local_temp_config()
  appDir <- local_temp_app(list("app.R" = "library(shiny)"))
  local_mocked_bindings(
    findDeploymentTarget = function(...) {
      list(
        accountDetails = list(name = "connect-user", server = "connect-server"),
        deployment = list(name = "myapp", appId = "42")
      )
    }
  )

  expect_error(
    deployApp(
      appDir,
      appName = "myapp",
      account = "connect-user",
      server = "connect-server",
      upload = FALSE,
      logLevel = "quiet",
      lint = FALSE,
      launch.browser = FALSE
    ),
    regexp = "Posit Connect does not support deploying without uploading"
  )
})
