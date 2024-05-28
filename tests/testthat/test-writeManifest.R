makeManifest <- function(appDir, appPrimaryDoc = NULL, ...) {
  writeManifest(appDir, appPrimaryDoc = appPrimaryDoc, ..., quiet = TRUE)
  manifestFile <- file.path(appDir, "manifest.json")
  data <- readLines(manifestFile, warn = FALSE, encoding = "UTF-8")
  manifestJson <- jsonlite::fromJSON(data)
  unlink(manifestFile)
  manifestJson
}

test_that("renv.lock is included for renv projects", {
  skip_on_cran()
  skip_if_not_installed("foreign")
  skip_if_not_installed("MASS")

  withr::local_options(renv.verbose = FALSE)

  app_dir <- local_temp_app(list(app.R = "library(foreign); library(MASS)"))
  renv::snapshot(app_dir, prompt = FALSE)

  manifest <- makeManifest(app_dir)
  # note: we don't see an .Rprofile here because we only renv::snapshot and
  # do not fully create an renv project.
  expect_named(manifest$files, c("app.R", "renv.lock"))
})

test_that("renv.lock is not included for non-renv projects", {
  skip_on_cran()
  skip_if_not_installed("foreign")
  skip_if_not_installed("MASS")

  withr::local_options(renv.verbose = FALSE)

  app_dir <- local_temp_app(list(app.R = "library(foreign); library(MASS)"))

  manifest <- makeManifest(app_dir)
  expect_named(manifest$files, c("app.R"))
})

test_that("Rmd with reticulate as a dependency includes python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  python <- pythonPathOrSkip()

  appDir <- test_path("test-reticulate-rmds")
  manifest <- makeManifest(appDir, python = python)
  requirements_file <- file.path(appDir, manifest$python$package_manager$package_file)
  expect_equal(requirements_file, "test-reticulate-rmds/requirements.txt")
  defer(unlink(requirements_file))

  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "index.Rmd")
  expect_true(file.exists(requirements_file))
})

test_that("Rmd with reticulate as an inferred dependency includes reticulate and python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  python <- pythonPathOrSkip()

  appDir <- test_path("test-reticulate-rmds")
  manifest <- makeManifest(appDir, "implicit.Rmd", python = python)
  requirements_file <- file.path(appDir, manifest$python$package_manager$package_file)
  expect_equal(requirements_file, "test-reticulate-rmds/requirements.txt")
  defer(unlink(requirements_file))

  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "implicit.Rmd")
  expect_true(file.exists(requirements_file))
})

test_that("Rmd without a python block doesn't include reticulate or python in the manifest", {
  skip_on_cran()

  manifest <- makeManifest(test_path("test-rmds"), "simple.Rmd", python = NULL)
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_null(manifest$python)
})

test_that("Rmd without a python block doesn't include reticulate or python in the manifest even if python specified", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  python <- pythonPathOrSkip()

  manifest <- makeManifest("test-rmds", "simple.Rmd", python = python)
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$python, NULL)
  # Confirm that we have removed packrat entries from our file listing but
  # retain entries for other files.
  filenames <- names(manifest$files)
  expect_false(any(grepl("^packrat/", filenames, perl = TRUE)), filenames)
  expect_true(any(grepl("simple.Rmd", filenames, fixed = TRUE)), filenames)
})

# Quarto Tests

test_that("Quarto website includes quarto in the manifest", {
  skip_on_cran()
  skip_if_no_quarto()

  appDir <- test_path("quarto-website-r")
  manifest <- makeManifest(appDir, quarto = TRUE)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "index.qmd")
})

test_that("Quarto document includes quarto in the manifest", {
  skip_on_cran()
  skip_if_no_quarto()

  appDir <- test_path("quarto-doc-none")
  appPrimaryDoc <- "quarto-doc-none.qmd"
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = TRUE)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "markdown")
  expect_equal(manifest$metadata$primary_rmd, "quarto-doc-none.qmd")
})

test_that("Specifying quarto arg includes quarto in the manifest, even with no appPrimaryDoc specified (.qmd)", {
  skip_on_cran()
  skip_if_no_quarto()

  appDir <- test_path("quarto-doc-none")
  appPrimaryDoc <- NULL
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = TRUE)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "markdown")
  expect_equal(manifest$metadata$primary_rmd, "quarto-doc-none.qmd")
})

test_that("Specifying quarto arg includes quarto in the manifest, even with no appPrimaryDoc specified (.Rmd)", {
  skip_on_cran()
  skip_if_no_quarto()

  appDir <- test_path("shiny-rmds")
  appPrimaryDoc <- NULL
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = TRUE)

  expect_equal(manifest$metadata$appmode, "quarto-shiny")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "non-shiny-rmd.Rmd")
})

test_that("specifying quarto arg with non-quarto app does not include quarto in the manifest", {
  skip_on_cran()
  skip_if_no_quarto()

  appDir <- test_path("shinyapp-singleR")
  appPrimaryDoc <- "single.R"
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = TRUE)

  expect_null(manifest$quarto)
})

test_that("Quarto shiny project includes quarto in the manifest", {
  skip_on_cran()
  skip_if_no_quarto()

  appDir <- test_path("quarto-proj-r-shiny")
  manifest <- makeManifest(appDir, quarto = TRUE)

  expect_equal(manifest$metadata$appmode, "quarto-shiny")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "quarto-proj-r-shiny.qmd")
})

test_that("Quarto R + Python website includes quarto and python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  skip_if_no_quarto()
  python <- pythonPathOrSkip()

  appDir <- test_path("quarto-website-r-py")
  manifest <- makeManifest(appDir, python = python, quarto = TRUE)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "index.qmd")

  expect_true(all(c("quarto", "python") %in% names(manifest)))
  expect_true("reticulate" %in% names(manifest$packages))
})

test_that("Quarto Python-only website gets correct manifest data", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  skip_if_no_quarto()

  python <- pythonPathOrSkip()

  appDir <- test_path("quarto-website-py")
  manifest <- makeManifest(appDir, python = python, quarto = TRUE)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "jupyter")
  expect_equal(manifest$metadata$primary_rmd, "index.qmd")

  # We expect Quarto and Python metadata, but no R packages.
  expect_true(all(c("quarto", "python") %in% names(manifest)))
  expect_null(manifest$packages)
})

test_that("Deploying a Quarto project without Quarto is an error", {
  skip_on_cran()
  local_mocked_bindings(quarto_path = function() NULL)

  appDir <- test_path("quarto-website-r")
  expect_snapshot(makeManifest(appDir), error = TRUE)
})

test_that("Deploying R Markdown content with Quarto gives a Quarto app mode", {
  skip_on_cran()
  skip_if_no_quarto()

  manifest <- makeManifest(test_path("test-rmds"), "simple.Rmd", quarto = TRUE)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
})

test_that("Deploying static content with _quarto.yaml succeeds without quartoInfo", {
  skip_on_cran()

  manifest <- makeManifest(test_path("static-with-quarto-yaml"))

  expect_equal(manifest$metadata$appmode, "static")
})

test_that("environment.image is not set when image is not provided", {
  skip_on_cran()

  withr::local_options(renv.verbose = TRUE)

  appDir <- test_path("shinyapp-simple")

  manifest <- makeManifest(appDir)
  expect_null(manifest$environment)
})

test_that("TensorFlow models are identified", {
  skip_on_cran()

  app_dir <- local_temp_app(list(
    "1/saved_model.pb" = "fake-saved-model"
  ))
  manifest <- makeManifest(app_dir)
  expect_equal(manifest$metadata$appmode, "tensorflow-saved-model")
  expect_null(manifest$packages)
  expect_named(manifest$files, c("1/saved_model.pb"))
})

test_that("environment.image is set when image is provided", {
  skip_on_cran()

  withr::local_options(renv.verbose = TRUE)

  appDir <- test_path("shinyapp-simple")

  manifest <- makeManifest(appDir, image = "rstudio/content-base:latest")
  expect_equal(manifest$environment$image, "rstudio/content-base:latest")
})

test_that("environment.image is set and uses a provided image even when RSCONNECT_IMAGE is set", {
  skip_on_cran()

  withr::local_options(renv.verbose = TRUE)
  withr::local_envvar(RSCONNECT_IMAGE = "rstudio/content-base:older")

  appDir <- test_path("shinyapp-simple")

  manifest <- makeManifest(appDir, image = "rstudio/content-base:latest")
  expect_equal(manifest$environment$image, "rstudio/content-base:latest")
})

test_that("environment.image is not set when RSCONNECT_IMAGE is empty", {
  skip_on_cran()

  withr::local_options(renv.verbose = TRUE)
  withr::local_envvar(RSCONNECT_IMAGE = "")

  appDir <- test_path("shinyapp-simple")

  manifest <- makeManifest(appDir)
  expect_null(manifest$environment)
})

test_that("environment.image is set when RSCONNECT_IMAGE is nonempty", {
  skip_on_cran()

  withr::local_options(renv.verbose = TRUE)
  withr::local_envvar(RSCONNECT_IMAGE = "rstudio/content-base:latest")

  appDir <- test_path("shinyapp-simple")

  manifest <- makeManifest(appDir)
  expect_equal(manifest$environment$image, "rstudio/content-base:latest")
})

test_that("Sets environment.environment_management in the manifest if envManagement is defined", {
  skip_on_cran()

  withr::local_options(renv.verbose = TRUE)

  appDir <- test_path("shinyapp-simple")

  # test shorthand arg
  manifest <- makeManifest(appDir, envManagement = FALSE, envManagementR = TRUE, envManagementPy = TRUE)
  expect_equal(manifest$environment$environment_management$r, FALSE)
  expect_equal(manifest$environment$environment_management$python, FALSE)

  # test R and Python args
  manifest <- makeManifest(appDir, envManagementR = TRUE)
  expect_equal(manifest$environment$environment_management$r, TRUE)
  expect_null(manifest$environment$environment_management$python)
  manifest <- makeManifest(appDir, envManagementPy = TRUE)
  expect_equal(manifest$environment$environment_management$python, TRUE)
  expect_null(manifest$environment$environment_management$r)

  # environment_management is not defined when envManagementR and envManagementPy are NULL
  manifest <- makeManifest(appDir, image = "rstudio/content-base:latest")
  expect_null(manifest$environment$environment_management)
})

# appMode Inference tests

test_that("content type (appMode) is inferred and can be overridden", {
  skip_on_cran()

  appDir <- local_temp_app(list(
    "app.R" = "",
    "index.html" = "",
    "plumber.R" = "",
    "report.Rmd" = ""
  ))
  files <- c("app.R", "index.html", "plumber.R", "report.Rmd")

  manifest <- makeManifest(appDir)
  expect_equal(manifest$metadata$appmode, "api")
  expect_named(manifest$files, files)

  manifest <- makeManifest(appDir, appMode = "shiny")
  expect_equal(manifest$metadata$appmode, "shiny")
  expect_named(manifest$files, files)

  manifest <- makeManifest(appDir, appMode = "rmd-static")
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_named(manifest$files, files)

  manifest <- makeManifest(appDir, appMode = "static")
  expect_equal(manifest$metadata$appmode, "static")
  expect_named(manifest$files, files)
})
