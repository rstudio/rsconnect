test_that("NULLs aren't secret", {
  expect_equal(secret(NULL), NULL)
})

test_that("print and str obfuscate output", {
  x <- secret("THIS IS MY PASSWORD: foo")
  expect_snapshot(x)
  expect_equal(trimws(capture.output(str(x))), "THIS I... (redacted)")
})

test_that("can put a secret in a data frame", {
  x <- secret("x")
  df <- data.frame(x)
  expect_equal(df$x, x)
})
