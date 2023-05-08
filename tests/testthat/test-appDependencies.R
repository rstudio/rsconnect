test_that("appDepenencies includes implicit deps", {
  dir <- local_temp_app(test.Rmd = "")
  deps <- appDependencies(dir)

  expect_true("rmarkdown" %in% deps$Package)
})

test_that("static project doesn't have deps", {
  dir <- local_temp_app(index.html = "")
  deps <- appDependencies(dir)

  expect_equal(deps, data.frame(
    Package = character(),
    Version = character(),
    Source = character(),
    Repository = character(),
    stringsAsFactors = FALSE
  ))
})

test_that("infers correct packages for each source", {
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
