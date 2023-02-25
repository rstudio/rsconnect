test_that("generateAppName has been deprecated", {
  expect_snapshot({
    . <- generateAppName("my title")
  })
})

test_that("generated application names do not exceed maximum length", {
  withr::local_options(lifecycle_verbosity = "quiet")
  title <- "Good heavens, this title is simply enormous! I can't imagine why anyone would use such a cumbersome moniker."
  name <- generateAppName(title)
  expect_lt(nchar(name), 65)
})

test_that("empty titles are rejected", {
  withr::local_options(lifecycle_verbosity = "quiet")
  title <- ""
  expect_error(generateAppName(title))
})

test_that("path is used to generate valid title if title isn't specified", {
  withr::local_options(lifecycle_verbosity = "quiet")
  title <- NULL
  path <- "shiny-app-in-subdir"
  name <- generateAppName(title, path)
  expect_gt(nchar(name), 5)
})

test_that("invalid characters are removed from titles", {
  withr::local_options(lifecycle_verbosity = "quiet")
  title <- "Free!* (* = With $5 Donation)"
  name <- generateAppName(title)
  expect_false(grepl("[!*=$]", name))
})

test_that("valid characters are not removed from titles", {
  withr::local_options(lifecycle_verbosity = "quiet")
  title <- "inexpensive_kittens-5"
  name <- generateAppName(title)
  expect_identical(title, name)
})
