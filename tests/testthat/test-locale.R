test_that("locale is en_US on mac GHA", {
  skip_on_os(c("linux", "windows"))
  skip_if_not(identical(Sys.getenv("GITHUB_ACTIONS"), "true"))

  expect_equal(detectLocale(), "en_US")
})

test_that("locale is en_US on mac GHA", {
  skip_on_os(c("linux", "mac"))
  skip_if_not(identical(Sys.getenv("GITHUB_ACTIONS"), "true"))

  expect_equal(windowsLocale(), "en_US")
})

test_that("normalizeLocale handles common forms", {
  expect_equal(normalizeLocale("en-US"), "en_US")
  expect_equal(normalizeLocale("az-Cyrl"), "az")
  expect_equal(normalizeLocale("az-Cyrl-AZ"), "az_AZ")
})
