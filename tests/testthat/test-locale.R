test_that("locale is en_US on GHA", {
  skip_if_not(identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
  expect_equal(detectLocale(), "en_US")
})
