context("bundle")

makeShinyBundleTempDir <- function(appName, appDir, appPrimaryDoc) {
  tarfile <- bundleApp(appName, appDir, bundleFiles(appDir), appPrimaryDoc,
                       "application", NULL)
  bundleTempDir <- tempfile()
  utils::untar(tarfile, exdir = bundleTempDir)
  unlink(tarfile)
  bundleTempDir
}

# avoid 'trying to use CRAN without setting a mirror' errors
repos <- getOption("repos")
options(repos = c(CRAN = "https://cran.rstudio.com"))
on.exit(options(repos = repos), add = TRUE)

test_that("simple Shiny app bundle is runnable", {
  bundleTempDir <- makeShinyBundleTempDir("simple_shiny", "shinyapp-simple",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("app.R Shiny app bundle is runnable", {
  bundleTempDir <- makeShinyBundleTempDir("app_r_shiny", "shinyapp-appR",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("single-file Shiny app bundle is runnable", {
  bundleTempDir <- makeShinyBundleTempDir("app_r_shiny", "shinyapp-singleR",
                                          "single.R")
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("recommended packages are snapshotted", {
  bundleTempDir <- makeShinyBundleTempDir("MASS", "project-MASS", "MASS.R")
  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_true("MASS" %in% names(deps))
})
