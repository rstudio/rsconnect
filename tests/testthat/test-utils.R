test_that("file_path_sans_ext removes extensions", {
  # extensions are removed.
  expect_equal(
      file_path_sans_ext(c("noext",
                           "extension.ext",
                           "doubledot..ext")),
      c("noext",
        "extension",
        "doubledot."))

  # compression extensions are removed first (when explicitly requested).
  expect_equal(
      file_path_sans_ext(c("noext",
                           "extension.ext",
                           "doubledot..ext",
                           "compressed.gz",
                           "compressext.ext.bz2",
                           "compressdoubledot..ext.xz"),
                         compression = TRUE),
      c("noext",
        "extension",
        "doubledot.",
        "compressed",
        "compressext",
        "compressdoubledot."))
})

# rbind_fill --------------------------------------------------------------

test_that("adds missing columns", {
  dfs <- list(data.frame(x = 1), data.frame(y = 2))
  out <- rbind_fill(dfs)
  expect_equal(out, data.frame(x = c(1, NA), y = c(NA, 2)))
})

test_that("order of col_names has preference", {
  dfs <- list(data.frame(y = 1, x = 2))
  out <- rbind_fill(dfs, c("x", "y"))
  expect_equal(out, data.frame(x = 2, y = 1))
})

test_that("uses col_names if no inputs", {
  expect_equal(rbind_fill(list()), data.frame())
  expect_equal(
    rbind_fill(list(), c("x", "y")),
    data.frame(x = logical(), y = logical())
  )
})
