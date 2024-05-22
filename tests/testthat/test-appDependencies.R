test_that("appDependencies includes implicit deps", {
  skip_on_cran()

  withr::local_options(renv.verbose = TRUE)

  path <- local_temp_app(list("test.Rmd" = ""))
  deps <- appDependencies(path)

  expect_true("rmarkdown" %in% deps$Package)
})

test_that("appDependencies includes implicit deps when appMode forced", {
  skip_on_cran()
  skip_if_not_installed("plumber")
  skip_if_not_installed("shiny")
  skip_if_not_installed("rmarkdown")

  withr::local_options(renv.verbose = TRUE)

  dir <- local_temp_app(list(
    "app.R" = "",
    "plumber.R" = "",
    "report.Rmd" = "",
    "index.html" = ""
  ))
  files <- c("app.R", "plumber.R", "report.Rmd", "index.html")

  # unless forced to "static", the presence of *.Rmd forces rmarkdown as an R
  # dependency, regardless of appMode.

  # inference indicates a Plumber API.
  deps <- appDependencies(dir)
  expect_true("plumber" %in% deps$Package)
  expect_false("shiny" %in% deps$Package)
  expect_true("rmarkdown" %in% deps$Package)

  deps <- appDependencies(dir, appMode = "api")
  expect_true("plumber" %in% deps$Package)
  expect_false("shiny" %in% deps$Package)
  expect_true("rmarkdown" %in% deps$Package)

  deps <- appDependencies(dir, appMode = "shiny")
  expect_false("plumber" %in% deps$Package)
  expect_true("shiny" %in% deps$Package)
  expect_true("rmarkdown" %in% deps$Package)

  deps <- appDependencies(dir, appMode = "rmd-static")
  expect_false("plumber" %in% deps$Package)
  expect_false("shiny" %in% deps$Package)
  expect_true("rmarkdown" %in% deps$Package)

  deps <- appDependencies(dir, appMode = "static")
  expect_equal(deps, data.frame(
    Package = character(),
    Version = character(),
    Source = character(),
    Repository = character(),
    stringsAsFactors = FALSE
  ))
})

test_that("static project doesn't have deps", {
  skip_on_cran()

  path <- local_temp_app(list("index.html" = ""))
  deps <- appDependencies(path)

  expect_equal(deps, data.frame(
    Package = character(),
    Version = character(),
    Source = character(),
    Repository = character(),
    stringsAsFactors = FALSE
  ))
})

test_that("infers correct packages for each source", {
  skip_on_cran()

  simulateMetadata <- function(appMode,
                               hasParameters = FALSE,
                               documentsHavePython = FALSE) {
    list(
      appMode = appMode,
      hasParameters = hasParameters,
      documentsHavePython = documentsHavePython
    )
  }

  # Simple regression test in preparation for refactoring
  expect_snapshot({
    inferRPackageDependencies(simulateMetadata("rmd-static"))
    inferRPackageDependencies(simulateMetadata("rmd-static", hasParameters = TRUE))
    inferRPackageDependencies(simulateMetadata("quarto-static"))
    inferRPackageDependencies(simulateMetadata("quarto-shiny"))
    inferRPackageDependencies(simulateMetadata("rmd-shiny"))
    inferRPackageDependencies(simulateMetadata("shiny"))
    inferRPackageDependencies(simulateMetadata("api"))
    inferRPackageDependencies(simulateMetadata("api", documentsHavePython = TRUE))
  })
})
