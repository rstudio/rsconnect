test_that("bundle directories are recursively enumerated", {
  targetDir <- tempfile()
  dir.create(targetDir)
  on.exit(unlink(targetDir, recursive = TRUE))

  # tree that resembles the case from https://github.com/rstudio/rsconnect/issues/464
  files <- c(
      "app.R",
      "index.htm",
      "models/abcd/a_b_pt1/a/b/c1/1.RDS",
      "models/abcd/a_b_pt1/a/b/c1/2.RDS",
      "models/abcd/a_b_pt1/a/b/c1/3.RDS",
      "models/abcd/a_b_pt1/a/b/c1/4.RDS",
      "models/abcd/a_b_pt1/a/b/c1/5.RDS"
  )

  # Create and write each file.
  sapply(files, function(file) {
    content <- c("this is the file named", file)
    targetFile <- file.path(targetDir, file)
    dir.create(dirname(targetFile), recursive = TRUE, showWarnings = FALSE)
    writeLines(content, con = targetFile, sep = "\n")
  })

  infos <- file.info(file.path(targetDir, files))
  totalSize <- sum(infos$size)
  totalFiles <- length(files)

  result <- listBundleFiles(targetDir)

  # Files are included in the list, count, and sizes, not directories.
  # Paths are enumerated relative to the target directory, not absolute paths.
  expect_identical(result$contents, files)
  expect_equal(result$totalSize, totalSize)
  expect_equal(result$totalFiles, totalFiles)
})


# explodeFiles ------------------------------------------------------------

test_that("returns relative paths", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "x"))
  file.create(file.path(dir, "x", c("a", "b", "c")))

  expect_equal(explodeFiles(dir, "x"), c("x/a", "x/b", "x/c"))
})

test_that("silently drops non-existent files", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("a", "b", "c")))

  expect_equal(explodeFiles(dir, c("a", "d")), "a")
})

test_that("expands files and directory", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "x"))
  file.create(file.path(dir, "x", c("a", "b")))
  file.create(file.path(dir, "c"))

  expect_equal(explodeFiles(dir, c("x", "c")), c("x/a", "x/b", "c"))
})


# standardAppFiles --------------------------------------------------------

test_that("can read all files from directory", {
  dir <- local_temp_app(list("a.R" = "", "b.R" = ""))
  expect_equal(standardizeAppFiles(dir), c("a.R", "b.R"))

  dir <- local_temp_app()
  expect_snapshot(standardizeAppFiles(dir), error = TRUE)
})

test_that("can read selected files from directory", {
  dir <- local_temp_app(list("a.R" = "", "b.R" = ""))
  expect_equal(standardizeAppFiles(dir, "b.R"), "b.R")
  # silently ignores files that aren't present
  expect_equal(standardizeAppFiles(dir, c("b.R", "c.R")), "b.R")

  expect_snapshot(standardizeAppFiles(dir, "c.R"), error = TRUE)
})

test_that("can read selected files from manifest", {
  dir <- local_temp_app(list(
    "a.R" = "",
    "b.R" = "",
    "manifest" = "b.R"
  ))
  expect_equal(
    standardizeAppFiles(dir, appFileManifest = file.path(dir, "manifest")),
    "b.R"
  )

  # silently ignores files that aren't present
  dir <- local_temp_app(list(
    "a.R" = "",
    "b.R" = "",
    "manifest" = c("b.R", "c.R")
  ))
  expect_equal(
    standardizeAppFiles(dir, appFileManifest = file.path(dir, "manifest")),
    "b.R"
  )

  # errors if no matching files
  dir <- local_temp_app(list(
    "a.R" = "",
    "b.R" = "",
    "manifest" = "c.R"
  ))
  expect_snapshot(
    standardizeAppFiles(dir, appFileManifest = file.path(dir, "manifest")),
    error = TRUE
  )
})

test_that("checks its inputs", {
  dir <- local_temp_app()
  expect_snapshot(error = TRUE, {
    standardizeAppFiles(dir, appFiles = "a.R", appFileManifest = "b.R")
    standardizeAppFiles(dir, appFiles = 1)
    standardizeAppFiles(dir, appFileManifest = "doestexist")
  })
})
