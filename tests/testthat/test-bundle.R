makeShinyBundleTempDir <- function(appName, appDir, appPrimaryDoc, python = NULL) {
  appFiles <- bundleFiles(appDir)
  appMetadata <- appMetadata(appDir, appFiles, appPrimaryDoc = appPrimaryDoc)

  tarfile <- bundleApp(
    appName,
    appDir,
    appFiles = appFiles,
    appMetadata = appMetadata,
    pythonConfig = pythonConfigurator(python)
  )
  bundleTempDir <- tempfile()
  utils::untar(tarfile, exdir = bundleTempDir)
  unlink(tarfile)
  bundleTempDir
}

makeManifest <- function(appDir, appPrimaryDoc = NULL, ...) {
  writeManifest(appDir, appPrimaryDoc = appPrimaryDoc, ...)
  manifestFile <- file.path(appDir, "manifest.json")
  data <- readLines(manifestFile, warn = FALSE, encoding = "UTF-8")
  manifestJson <- jsonlite::fromJSON(data)
  unlink(manifestFile)
  manifestJson
}

# avoid 'trying to use CRAN without setting a mirror' errors
repos <- getOption("repos")
options(repos = c(CRAN = "https://cran.rstudio.com"))
on.exit(options(repos = repos), add = TRUE)

test_that("simple Shiny app bundle is runnable", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  bundleTempDir <- makeShinyBundleTempDir(
    "simple_shiny",
    test_path("shinyapp-simple"),
    NULL
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("app.R Shiny app bundle is runnable", {
  # shiny:::shinyAppDir() attach shiny so do it here so we can do it quietly
  library(shiny, warn.conflicts = FALSE, quietly = TRUE)

  skip_on_cran()
  skip_if_not_installed("shiny")
  bundleTempDir <- makeShinyBundleTempDir(
    "app_r_shiny",
    test_path("shinyapp-appR"),
    NULL
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("single-file Shiny app bundle is runnable", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  bundleTempDir <- makeShinyBundleTempDir(
    "app_r_shiny",
    test_path("shinyapp-singleR"),
    "single.R"
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("simple Rmd as primary not identified as parameterized when parameterized Rmd in bundle", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir(
    "rmd primary",
    test_path("test-rmds"),
    "simple.Rmd"
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$metadata$has_parameters, FALSE)
})

test_that("parameterized Rmd identified as parameterized when other Rmd in bundle", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir(
    "rmd primary",
    test_path("test-rmds"),
    "parameterized.Rmd"
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "parameterized.Rmd")
  expect_equal(manifest$metadata$has_parameters, TRUE)
})

test_that("primary doc can be inferred (and non-parameterized dispite an included parameterized", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir(
    "rmd primary",
    test_path("test-rmds"),
    NULL
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "index.Rmd")
  expect_equal(manifest$metadata$has_parameters, FALSE)
})

test_that("Rmd with reticulate as a dependency includes python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  python <- pythonPathOrSkip()

  bundleTempDir <- makeShinyBundleTempDir(
    "reticulated rmd",
    test_path("test-reticulate-rmds"),
    NULL,
    python = python
  )
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
  python <- pythonPathOrSkip()

  bundleTempDir <- makeShinyBundleTempDir(
    "reticulated rmd",
    test_path("test-reticulate-rmds"),
    "implicit.Rmd",
    python = python
  )
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

  bundleTempDir <- makeShinyBundleTempDir(
    "plain rmd",
    test_path("test-rmds"),
    "simple.Rmd",
    python = NULL
  )
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
  skip_if_not_installed("reticulate")
  python <- pythonPathOrSkip()

  bundleTempDir <- makeShinyBundleTempDir(
    "plain rmd",
    test_path("test-rmds"),
    "simple.Rmd",
    python = python
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))

  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_false("reticulate" %in% names(deps))

  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$python, NULL)
})

test_that("tarImplementation: checks environment variable and option before using default", {
  tar_implementation <- function(option, envvar) {
    withr::local_options(rsconnect.tar = option)
    withr::local_envvar(RSCONNECT_TAR = envvar)
    getTarImplementation()
  }

  # Environment variable only set should use environment varaible
  expect_equal(tar_implementation(NULL, "envvar"), "envvar")

  # Option only set should use option
  expect_equal(tar_implementation("option", NA), "option")

  # Both environment variable and option set should use option
  expect_equal(tar_implementation("option", "envvar"), "option")

  # Neither set should use "internal"
  expect_equal(tar_implementation(NULL, NA), "internal")
})

# tweakRProfile -----------------------------------------------------------

test_that(".Rprofile tweaked automatically", {
  dir <- withr::local_tempdir()
  writeLines('source("renv/activate.R")', file.path(dir, ".Rprofile"))

  bundled <- bundleAppDir(dir, list.files(dir, all.files = TRUE))
  expect_match(
    readLines(file.path(bundled, ".Rprofile")),
    "Modified by rsconnect",
    all = FALSE
  )
})

test_that(".Rprofile without renv/packrt left as is", {
  lines <- c("1 + 1", "# Line 2", "library(foo)")
  path <- withr::local_tempfile(lines = lines)

  tweakRProfile(path)
  expect_equal(readLines(path), lines)
})

test_that("removes renv/packrat activation", {
  path <- withr::local_tempfile(lines = c(
    "# Line 1",
    'source("renv/activate.R")',
    "# Line 3",
    'source("packrat/init.R")',
    "# Line 5"
  ))

  expect_snapshot(
    {
      tweakRProfile(path)
      writeLines(readLines(path))
    },
    transform = function(x) gsub("on \\d{4}.+", "on <NOW>", x)
  )
})
