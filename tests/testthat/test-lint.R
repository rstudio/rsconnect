context("lint")

test_that("linter warns about absolute paths and relative paths", {

  ## Create a local file that 'server.R' tries to use
  testDir <- "~/.rsconnect-tests"
  exists <- file.exists(testDir)

  dir.create("~/.rsconnect-tests", showWarnings = FALSE)
  file.create("~/.rsconnect-tests/local-file.txt")

  serverPath <- list.files("shinyapp-with-absolute-paths",
                           pattern = "server\\.R$")

  result <- lint("shinyapp-with-absolute-paths")

  absPathLintedIndices <- result[[serverPath]]$absolute.paths$indices
  expect_identical(as.numeric(absPathLintedIndices), 15)

  relPathLintedIndices <- result[[serverPath]]$invalid.relative.paths$indices
  expect_identical(as.numeric(relPathLintedIndices), 16)

  if (!exists)
    unlink(testDir, recursive = TRUE)
})

test_that("badRelativePaths identifies bad paths correctly", {

  path <- "R/test.R"
  ok <- "file.path('../inst/include')"
  expect_false(badRelativePaths(ok, path = path))

  bad <- "file.path('../../elsewhere')"
  expect_true(badRelativePaths(bad, path = path))

  ok <- "'../foo', '../bar', '../baz'"
  expect_false(badRelativePaths(ok, path = path))

})

test_that("The linter identifies invalid application structures", {
  expect_error(lint("shiny-app-in-subdir"))
  lint("shiny-app-in-subdir/my-app")
})

test_that("The linter identifies files not matching in case sensitivity", {
  result <- lint("shinyapp-with-absolute-paths")
  server.R <- result[["server.R"]]
  filepath.capitalization <- server.R[["filepath.capitalization"]]
  expect_equal(as.integer(filepath.capitalization$indices), 31)
})

test_that("The linter identifies files with Markdown links not matching in case sensitivity", {
  result <- lint("test-rmd-bad-case")
  index.Rmd <- result[["index.Rmd"]]
  filepath.capitalization <- index.Rmd[["filepath.capitalization"]]
  expect_equal(as.integer(filepath.capitalization$indices), 29)
})

test_that("The linter believes that the Shiny example apps are okay", {

  examples <- list.files(system.file("examples", package = "shiny"), full.names = TRUE)
  if (length(examples)) {

    results <- lapply(examples, lint)

    lints <- suppressMessages(lapply(results, printLinterResults))
    lapply(lints, function(project) {
      lapply(project, function(file) {
        lapply(file, function(linter) {
          expect_true(length(linter$indices) == 0)
        })
      })
    })
  }
})

test_that("The linter identifies browser() statements correctly", {
  result <- lint("shinyapp-with-browser")
  server.R <- result[["server.R"]]
  browseLines <- server.R[["browser"]]
  expect_true(browseLines$indices == 9)
})

test_that("The linter identifies browseURL() statements correctly", {
  result <- lint("shinyapp-with-browser")
  server.R <- result[["server.R"]]
  browseLines <- server.R[["browseURL"]]
  expect_true(browseLines$indices == 5)
})

test_that("The linter accepts a plumber API", {
  result <- lint("test-plumber")
  expect_false(is.null(result$plumber.R))
})

test_that("The linter accepts a TensorFlow Saved Model", {
  lint("tf-saved-model")
  lint("tf-human-readable-saved-model")
  lint("tf-saved-model-rootdir")
  expect_true(TRUE) # didn't stop()
})

test_that("Linters can run on files with multibyte characters", {
  lint("multibyte-characters")
  expect_true(TRUE) # didn't stop
})
