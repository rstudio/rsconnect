context("hashes")

emptyFile <- tempfile(fileext = ".empty.txt")
contentsFile <- tempfile(fileext = ".contents.txt")

# computed by openssl::md5("")
expectedEmptyMD5 <- "d41d8cd98f00b204e9800998ecf8427e"
# computed by openssl::md5("go bananas!\n")
expectedContentsMD5 <- "52d2daa95d288f3c01e4d4d87f85727e"
# computed by openssl::md5("")
expectedMissingMD5 <- "d41d8cd98f00b204e9800998ecf8427e"

setup({
  # Create an empty file.
  if (!dir.exists(dirname(emptyFile))) {
    dir.create(dirname(emptyFile), recursive = TRUE)
  }
  file.create(emptyFile)

  # Create a file with well-known contents.
  if (!dir.exists(dirname(contentsFile))) {
    dir.create(dirname(contentsFile), recursive = TRUE)
  }

  # Open in binary mode so the contents are identical on all platforms
  # (otherwise the file contains a \n on Unix and \r\n on Windows, which
  # hash differently)
  con <- file(contentsFile, open = "wb")
  writeLines("go bananas!", con)
  close(con)
})

teardown({
  unlink(emptyFile)
  unlink(contentsFile)
})

expect_hash_type <- function(object) {
  expect_type(object, "raw")
}

test_that("we can hash an empty file", {
  rawEmptyMD5 <- fileMD5(emptyFile)
  expect_hash_type(rawEmptyMD5)
  emptyMD5 <- md5.as.string(rawEmptyMD5)
  expect_equal(emptyMD5, expectedEmptyMD5)
  expect_type(emptyMD5, "character")

  emptyMD5 <- fileMD5.as.string(emptyFile)
  expect_equal(emptyMD5, expectedEmptyMD5)
  expect_type(emptyMD5, "character")
})

test_that("we can hash a file with well known contents", {
  rawContentsMD5 <- fileMD5(contentsFile)
  expect_hash_type(rawContentsMD5)
  contentsMD5 <- md5.as.string(rawContentsMD5)
  expect_equal(contentsMD5, expectedContentsMD5)
  expect_type(contentsMD5, "character")

  contentsMD5 <- fileMD5.as.string(contentsFile)
  expect_equal(contentsMD5, expectedContentsMD5)
  expect_type(contentsMD5, "character")
})

test_that("we can hash when not supplied a filename", {
  rawMissingMD5 <- fileMD5(NULL)
  expect_hash_type(rawMissingMD5)
  missingMD5 <- md5.as.string(rawMissingMD5)
  expect_equal(missingMD5, expectedMissingMD5)
  expect_type(missingMD5, "character")

  missingMD5 <- fileMD5.as.string(NULL)
  expect_equal(missingMD5, expectedMissingMD5)
  expect_type(missingMD5, "character")
})
