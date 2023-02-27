test_that("locale is en_US on mac/windows GHA", {
  skip_on_os("linux")
  skip_if_not(identical(Sys.getenv("GITHUB_ACTIONS"), "true"))

  expect_equal(detectLocale(), "en_US")
})
