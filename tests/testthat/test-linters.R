test_that("linter warns about absolute paths and relative paths", {
  dirCreate("~/.rsconnect-tests")
  file.create("~/.rsconnect-tests/local-file.txt")
  withr::defer(unlink("~/.rsconnect-tests", recursive = TRUE))

  result <- lint(test_path("shinyapp-with-absolute-paths"))
  expect_snapshot(result)

  absPathLintedIndices <- result[["server.R"]]$absolute.paths$indices
  expect_identical(as.numeric(absPathLintedIndices), 12)
})

test_that("The linter identifies files not matching in case sensitivity", {
  result <- lint(test_path("shinyapp-with-absolute-paths"))
  server.R <- result[["server.R"]]
  filepath.capitalization <- server.R[["filepath.capitalization"]]
  expect_equal(as.integer(filepath.capitalization$indices), 28)
})

test_that("The linter identifies files with Markdown links not matching in case sensitivity", {
  result <- lint(test_path("test-rmd-bad-case"))
  index.Rmd <- result[["index.Rmd"]]
  filepath.capitalization <- index.Rmd[["filepath.capitalization"]]
  expect_equal(as.integer(filepath.capitalization$indices), 29)
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

test_that("Linters can run on files with multibyte characters", {
  lint("multibyte-characters")
  expect_true(TRUE) # didn't stop
})
