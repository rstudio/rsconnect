context("lint")

test_that("linter warns about absolute paths", {
  serverPath <- list.files("shinyapp-with-absolute-paths", pattern = "server\\.R$")
  result <- lint("shinyapp-with-absolute-paths")
  print(result)
  lintedIndices <- result[[serverPath]]$absolute.paths$indices
  expect_identical(as.numeric(lintedIndices), c(15, 16))
})