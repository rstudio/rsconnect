context("utils")

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
