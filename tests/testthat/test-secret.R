test_that("NULLs aren't secret", {
  expect_equal(secret(NULL), NULL)
})

test_that("print and str obfuscate output", {
  expect_snapshot({
    x <- secret("THIS IS MY PASSWORD: foo")
    x
    str(x)
  })
})

test_that("can put a secret in a data frame", {
  x <- secret("x")
  df <- data.frame(x)
  expect_equal(df$x, x)
})
