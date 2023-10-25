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
  expect_false(needsVisibilityChange("shinyapps.io", dummyApp("public"), "public"))
  expect_true(needsVisibilityChange("shinyapps.io", dummyApp(NULL), "private"))
  expect_true(needsVisibilityChange("shinyapps.io", dummyApp("public"), "private"))
})

test_that("needsVisibilityChange() errors for cloud", {
  expect_snapshot(error = TRUE,
    needsVisibilityChange("posit.cloud", appVisibility = "public")
  )
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
