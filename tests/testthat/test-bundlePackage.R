test_that("non-R apps don't have packages", {
  app_dir <- withr::local_tempdir()
  out <- bundlePackages(app_dir, appMode = "static")
  expect_equal(out, list())

  out <- bundlePackages(app_dir, appMode = "", quartoInfo = list(engines = "blather"))
  expect_equal(out, list())
})

test_that("returns list of package details", {
  app_dir <- withr::local_tempdir()
  out <- bundlePackages(app_dir, appMode = "api", hasParameters = TRUE)

  expect_type(out, "list")

  common <- c("Source", "Repository", "description")
  expect_equal(setdiff(common, names(out[[1]])), character())
})

test_that("includes inferred dependencies", {
  app_dir <- withr::local_tempdir()
  out <- bundlePackages(app_dir, appMode = "rmd-static", hasParameters = TRUE)

  expect_true("rmarkdown" %in% names(out))
  expect_true("shiny" %in% names(out))
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
  expect_snapshot(addPackratSnapshot("doesntexit"), error = TRUE)
})

test_that("cleans up implicit dependency files", {
  dir <- withr::local_tempdir()
  addPackratSnapshot(dir, "rlang")
  expect_equal(list.files(dir), "packrat")
})

test_that("infers correct packages for each source", {
  # Simple regression test in preparation for refactoring
  expect_snapshot({
    inferRPackageDependencies("rmd-static")
    inferRPackageDependencies("rmd-static", TRUE)
    inferRPackageDependencies("quarto-static")
    inferRPackageDependencies("quarto-shiny")
    inferRPackageDependencies("rmd-shiny")
    inferRPackageDependencies("shiny")
    inferRPackageDependencies("api")
    inferRPackageDependencies("api", documentsHavePython = TRUE)
  })
})
