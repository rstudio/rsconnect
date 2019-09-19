context("bundle")

makeShinyBundleTempDir <- function(appName, appDir, appPrimaryDoc, python = NULL) {
  tarfile <- bundleApp(appName, appDir, bundleFiles(appDir), appPrimaryDoc,
                       "application", NULL, python = python)
  bundleTempDir <- tempfile()
  utils::untar(tarfile, exdir = bundleTempDir)
  unlink(tarfile)
  bundleTempDir
}

makeManifest <- function(appDir, appPrimaryDoc, python = NULL) {
  writeManifest(appDir, NULL, appPrimaryDoc, NULL, python)
  manifestFile <-file.path(appDir, "manifest.json")
  data <- readLines(manifestFile, warn = FALSE, encoding = "UTF-8")
  manifestJson <- jsonlite::fromJSON(data)
  unlink(manifestFile)
  manifestJson
}

# avoid 'trying to use CRAN without setting a mirror' errors
repos <- getOption("repos")
options(repos = c(CRAN = "https://cran.rstudio.com"))
on.exit(options(repos = repos), add = TRUE)

test_that("simple Shiny app bundle includes correct files", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("simple_shiny", "shinyapp-simple",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  # listBundleFiles only includes "user" files and ignores
  # generated files like the packrat and manifest data.
  files <- listBundleFiles(bundleTempDir)
  expect_identical(files$contents, c("server.R", "ui.R"))
  expect_identical(list.files(bundleTempDir), c("manifest.json", "packrat", "server.R", "ui.R"))
})

test_that("simple Shiny app bundle is runnable", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("simple_shiny", "shinyapp-simple",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("app.R Shiny app bundle is runnable", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("app_r_shiny", "shinyapp-appR",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("single-file Shiny app bundle is runnable", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("app_r_shiny", "shinyapp-singleR",
                                          "single.R")
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("recommended packages are snapshotted", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("MASS", "project-MASS", "MASS.R")
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_true("MASS" %in% names(deps))
})

test_that("simple Rmd as primary not identified as parameterized when parameterized Rmd in bundle", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("rmd primary", "test-rmds",
                                          "simple.Rmd")
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$metadata$has_parameters, FALSE)
})

test_that("parameterized Rmd identified as parameterized when other Rmd in bundle", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("rmd primary", "test-rmds",
                                          "parameterized.Rmd")
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "parameterized.Rmd")
  expect_equal(manifest$metadata$has_parameters, TRUE)
})

test_that("primary doc can be inferred (and non-parameterized dispite an included parameterized", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("rmd primary", "test-rmds",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "index.Rmd")
  expect_equal(manifest$metadata$has_parameters, FALSE)
})

test_that("multiple shiny Rmd without index file have a generated one", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("rmd primary", "shiny-rmds",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-shiny")
  expect_true(file.exists(file.path(bundleTempDir, "index.htm")))
})

test_that("Rmd with reticulate as a dependency includes python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")
  pipMissing <- system2(python, "-m pip help", stdout = NULL, stderr = NULL)
  skip_if(pipMissing != 0, "pip module is not installed")

  bundleTempDir <- makeShinyBundleTempDir("reticulated rmd", "test-reticulate-rmds",
                                          NULL, python = python)
  on.exit(unlink(bundleTempDir, recursive = TRUE))

  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_true("reticulate" %in% names(deps))

  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "index.Rmd")
  expect_true(file.exists(file.path(bundleTempDir, manifest$python$package_manager$package_file)))
})

test_that("Rmd with reticulate as an inferred dependency includes reticulate and python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")
  pipMissing <- system2(python, "-m pip help", stdout = NULL, stderr = NULL)
  skip_if(pipMissing != 0, "pip module is not installed")

  bundleTempDir <- makeShinyBundleTempDir("reticulated rmd", "test-reticulate-rmds",
                                          "implicit.Rmd", python = python)
  on.exit(unlink(bundleTempDir, recursive = TRUE))

  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_true("reticulate" %in% names(deps))

  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "implicit.Rmd")
  expect_true(file.exists(file.path(bundleTempDir, manifest$python$package_manager$package_file)))
})

test_that("Rmd without a python block doesn't include reticulate or python in the manifest", {
  skip_on_cran()

  bundleTempDir <- makeShinyBundleTempDir("plain rmd", "test-rmds",
                                          "simple.Rmd", python = NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))

  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_false("reticulate" %in% names(deps))

  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$python, NULL)
})

test_that("Rmd without a python block doesn't include reticulate or python in the manifest even if python specified", {
  skip_on_cran()
  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")

  bundleTempDir <- makeShinyBundleTempDir("plain rmd", "test-rmds",
                                          "simple.Rmd", python = python)
  on.exit(unlink(bundleTempDir, recursive = TRUE))

  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_false("reticulate" %in% names(deps))

  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$python, NULL)
})

test_that("writeManifest: Rmd with reticulate as a dependency includes python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")
  pipMissing <- system2(python, "-m pip help", stdout = NULL, stderr = NULL)
  skip_if(pipMissing != 0, "pip module is not installed")

  appDir <- "test-reticulate-rmds"
  manifest <- makeManifest(appDir, NULL, python = python)
  requirements_file <- file.path(appDir, manifest$python$package_manager$package_file)
  expect_equal(requirements_file, "test-reticulate-rmds/requirements.txt")
  on.exit(unlink(requirements_file))

  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "index.Rmd")
  expect_true(file.exists(requirements_file))
})

test_that("writeManifest: Rmd with reticulate as an inferred dependency includes reticulate and python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")
  pipMissing <- system2(python, "-m pip help", stdout = NULL, stderr = NULL)
  skip_if(pipMissing != 0, "pip module is not installed")

  appDir <- "test-reticulate-rmds"
  manifest <- makeManifest(appDir, "implicit.Rmd", python = python)
  requirements_file <- file.path(appDir, manifest$python$package_manager$package_file)
  expect_equal(requirements_file, "test-reticulate-rmds/requirements.txt")
  on.exit(unlink(requirements_file))

  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "implicit.Rmd")
  expect_true(file.exists(requirements_file))
})

test_that("writeManifest: Rmd without a python block doesn't include reticulate or python in the manifest", {
  skip_on_cran()

  manifest <- makeManifest("test-rmds", "simple.Rmd", python = NULL)
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_null(manifest$python)
})

test_that("writeManifest: Rmd without a python block doesn't include reticulate or python in the manifest even if python specified", {
  skip_on_cran()
  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")

  manifest <- makeManifest("test-rmds", "simple.Rmd", python = python)
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$python, NULL)
})

test_that("getPython handles null python by checking RETICULATE_PYTHON", {
  skip_on_cran()

  Sys.setenv(RETICULATE_PYTHON="/usr/local/bin/python")
  expect_equal(getPython(NULL), "/usr/local/bin/python")
  Sys.unsetenv("RETICULATE_PYTHON")
})

test_that("getPython handles null python and empty RETICULATE_PYTHON", {
  skip_on_cran()

  Sys.unsetenv("RETICULATE_PYTHON")
  expect_equal(getPython(NULL), NULL)
})

test_that("getPython expands paths", {
  skip_on_cran()

  result <- getPython("~/bin/python")
  expect_true(result != "~/bin/python")
  expect_match(result, "*/bin/python")
})

test_that("getPythonForTarget honors rsconnect.python.enabled = FALSE", {
  skip_on_cran()

  options(rsconnect.python.enabled = FALSE)
  result <- getPythonForTarget("/usr/bin/python", list(server="shinyapps.io"))
  expect_equal(result, NULL)
  options(rsconnect.python.enabled = NULL)
})

test_that("getPythonForTarget honors rsconnect.python.enabled = TRUE", {
  skip_on_cran()

  options(rsconnect.python.enabled = TRUE)
  result <- getPythonForTarget("/usr/bin/python", list(server="shinyapps.io"))
  expect_equal(result, "/usr/bin/python")
  options(rsconnect.python.enabled = NULL)
})

test_that("getPythonForTarget defaults to enabled for Connect", {
  skip_on_cran()

  result <- getPythonForTarget("/usr/bin/python", list(server="connect.example.com"))
  expect_equal(result, "/usr/bin/python")
})

test_that("getPythonForTarget defaults to disabled for shinyapps.io", {
  skip_on_cran()

  result <- getPythonForTarget("/usr/bin/python", list(server="shinyapps.io"))
  expect_equal(result, NULL)
})
