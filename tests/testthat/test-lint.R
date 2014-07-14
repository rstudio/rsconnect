context("lint")

test_that("linter warns about absolute paths", {
  serverPath <- list.files("shinyapp-with-absolute-paths", full.names = TRUE, pattern = "server\\.R$")
  result <- suppressMessages(lint("shinyapp-with-absolute-paths"))
  expect_true(all(result[[serverPath]]$absolute.paths$indices == c(15, 16)))
})