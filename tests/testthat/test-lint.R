context("lint")

test_that("linter warns about absolute paths and relative paths", {
  serverPath <- list.files("shinyapp-with-absolute-paths", pattern = "server\\.R$")
  result <- lint("shinyapp-with-absolute-paths")
  printLinterResults(result)
  absPathLintedIndices <- result[[serverPath]]$absolute.paths$indices
  expect_identical(as.numeric(absPathLintedIndices), c(15, 16))
  relPathLintedIndices <- result[[serverPath]]$invalid.relative.paths$indices
  expect_identical(as.numeric(relPathLintedIndices), 17)
})
