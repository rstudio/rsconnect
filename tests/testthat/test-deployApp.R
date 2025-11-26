test_that("appDir must be an existing directory", {
  expect_snapshot(error = TRUE, {
    deployApp(1)
    deployApp("doesntexist")
  })
})

test_that("single document appDir is deprecated", {
  skip_on_cran()
  expect_snapshot(error = TRUE, {
    deployApp("foo.Rmd")
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

  expect_error(deployApp(appDir, manifestPath = "manifest.json"), "Manifest file not found")
})

test_that("manifest file must be valid JSON", {
  skip_on_cran()
  appDir <- local_temp_app(list(app.R = "# shiny app"))
  writeLines("not valid json {", file.path(appDir, "manifest.json"))


  expect_error(deployApp(appDir, manifestPath = "manifest.json"), "invalid string in json text")
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
  writeLines('{"metadata": {}, "files": {}}', file.path(appDir, "manifest.json"))
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