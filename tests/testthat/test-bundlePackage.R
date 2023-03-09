test_that("non-R apps don't have packages", {
  app_dir <- withr::local_tempdir()
  out <- bundlePackages(app_dir, appMode = "static")
  expect_equal(out, list())
})

test_that("returns list of package details", {
  app_dir <- local_temp_app(list("foo.Rmd" = ""))
  out <- bundlePackages(app_dir)
  expect_type(out, "list")

  common <- c("Source", "Repository", "description")
  expect_equal(setdiff(common, names(out[[1]])), character())
})

test_that("recommended packages are snapshotted", {
  app_dir <- withr::local_tempdir()
  writeLines(con = file.path(app_dir, "index.Rmd"), c(
    "```{r}",
    "library(MASS)",
    "```"
  ))
  out <- bundlePackages(app_dir)
  expect_true("MASS" %in% names(out))
})

test_that("errors if dependencies aren't installed", {
  mockr::local_mock(snapshotRDependencies = function(...) {
    data.frame(
      Package = c("doesntexist1", "doesntexist2"),
      Source = "CRAN",
      Repository = "https://cran.rstudio.com",
      stringsAsFactors = FALSE
    )
  })

  app_dir <- withr::local_tempdir()
  writeLines(con = file.path(app_dir, "index.Rmd"), c(
    "```{r}",
    "library(doesntexist1)",
    "library(doesntexist2)",
    "```"
  ))

  expect_snapshot(
    bundlePackages(app_dir, appMode = "rmd-static"),
    error = TRUE
  )
})

test_that("warns if can't find source", {
  mockr::local_mock(snapshotRDependencies = function(...) {
    data.frame(
      Package = "shiny",
      Source = NA,
      Repository = NA,
      stringsAsFactors = FALSE
    )
  })

  app_dir <- withr::local_tempdir()
  writeLines(con = file.path(app_dir, "index.Rmd"), c(
    "```{r}",
    "library(shiny)",
    "```"
  ))

  expect_snapshot(
    . <- bundlePackages(app_dir, appMode = "rmd-static")
  )
})

test_that("clear error if can't run performPackratSnapshot()", {
  dir <- withr::local_tempdir()

  expect_snapshot(
    addPackratSnapshot(dir, "doesntexist"),
    error = TRUE,
    transform = function(x) gsub('"', "'", x, fixed = TRUE)
  )
})

test_that("cleans up implicit dependency files", {
  dir <- withr::local_tempdir()
  addPackratSnapshot(dir, "rlang")
  expect_equal(list.files(dir), "packrat")
})
