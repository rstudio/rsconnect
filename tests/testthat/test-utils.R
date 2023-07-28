test_that("file_path_sans_ext removes extensions", {
  # extensions are removed.
  expect_equal(
    file_path_sans_ext(c("noext", "extension.ext", "doubledot..ext")),
    c("noext", "extension", "doubledot.")
  )

  # compression extensions are removed first (when explicitly requested).
  expect_equal(
    file_path_sans_ext(
      c(
        "noext",
        "extension.ext",
        "doubledot..ext",
        "compressed.gz",
        "compressext.ext.bz2",
        "compressdoubledot..ext.xz"
      ),
      compression = TRUE
    ),
    c(
      "noext",
      "extension",
      "doubledot.",
      "compressed",
      "compressext",
      "compressdoubledot."
    )
  )
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

test_that("can work with empty data frames", {
  out <- rbind_fill(list(data.frame(x = 1), data.frame()))
  expect_equal(out, data.frame(x = 1))
})

# hashing -----------------------------------------------------------------

test_that("we can hash an empty file", {
  emptyFile <- withr::local_tempfile()
  file.create(emptyFile)

  # computed by openssl::md5("")
  expect_equal(fileMD5(emptyFile), "d41d8cd98f00b204e9800998ecf8427e")
  expect_equal(fileMD5(NULL), "d41d8cd98f00b204e9800998ecf8427e")
})

test_that("we can hash a file with well known contents", {
  path <- withr::local_tempfile()
  # Open in binary mode so the contents are identical on all platforms
  # (otherwise the file contains a \n on Unix and \r\n on Windows, which
  # hash differently)
  con <- file(path, open = "wb")
  writeLines("go bananas!", con)
  close(con)

  expect_equal(fileMD5(path), "52d2daa95d288f3c01e4d4d87f85727e")
})

test_that("truthy is truthy", {
  # fallback-to-default checks
  expect_false(truthy(c()))
  expect_true(truthy(c(), default = TRUE))
  expect_false(truthy(NA))
  expect_true(truthy(NA, default = TRUE))

  # true value checks
  expect_true(truthy(TRUE, default = FALSE))
  expect_true(truthy("TRUE", default = FALSE))
  expect_true(truthy("True", default = FALSE))
  expect_true(truthy("true", default = FALSE))
  expect_true(truthy("T", default = FALSE))
  expect_true(truthy("1", default = FALSE))
  expect_true(truthy(1, default = FALSE))
  expect_true(truthy(42, default = FALSE))

  # false value checks
  expect_false(truthy(FALSE, default = TRUE))
  expect_false(truthy("FALSE", default = TRUE))
  expect_false(truthy("False", default = TRUE))
  expect_false(truthy("false", default = TRUE))
  expect_false(truthy("F", default = TRUE))
  expect_false(truthy("0", default = TRUE))
  expect_false(truthy(0, default = TRUE))
  expect_false(truthy("nonsense", default = TRUE))
})
