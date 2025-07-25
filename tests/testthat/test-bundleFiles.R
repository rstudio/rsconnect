# listDeploymentFiles ------------------------------------------------------

test_that("can read all files from directory", {
  dir <- local_temp_app(list("a.R" = "", "b.R" = ""))
  expect_equal(listDeploymentFiles(dir), c("a.R", "b.R"))
  expect_equal(listDeploymentFiles(dir, NULL, NULL), c("a.R", "b.R"))

  dir <- local_temp_app()
  expect_snapshot(listDeploymentFiles(dir), error = TRUE)
})

test_that("can read selected files from directory", {
  dir <- local_temp_app(list("a.R" = "", "b.R" = ""))
  expect_equal(listDeploymentFiles(dir, "b.R"), "b.R")
  expect_snapshot(out <- listDeploymentFiles(dir, c("b.R", "c.R")))
  expect_equal(out, "b.R")
  expect_snapshot(listDeploymentFiles(dir, character()), error = TRUE)
})

test_that("can read selected files from manifest", {
  dir <- local_temp_app(list(
    "a.R" = "",
    "b.R" = "",
    "manifest" = "b.R"
  ))
  expect_equal(
    listDeploymentFiles(dir, appFileManifest = file.path(dir, "manifest")),
    "b.R"
  )

  dir <- local_temp_app(list(
    "a.R" = "",
    "b.R" = "",
    "manifest" = c("b.R", "c.R")
  ))
  expect_snapshot(
    out <- listDeploymentFiles(
      dir,
      appFileManifest = file.path(dir, "manifest")
    ),
  )
  expect_equal(out, "b.R")

  # errors if no matching files
  dir <- local_temp_app(list(
    "manifest" = ""
  ))
  expect_snapshot(
    listDeploymentFiles(dir, appFileManifest = file.path(dir, "manifest")),
    error = TRUE
  )
})

test_that("checks its inputs", {
  dir <- local_temp_app()
  expect_snapshot(error = TRUE, {
    listDeploymentFiles(dir)
    listDeploymentFiles(dir, appFiles = "a.R", appFileManifest = "b.R")
    listDeploymentFiles(dir, appFiles = 1)
    listDeploymentFiles(dir, appFileManifest = "doestexist")
  })
})


# listBundleFiles ---------------------------------------------------------

test_that("bundle directories are recursively enumerated", {
  dir <- withr::local_tempdir()

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
  dirCreate(file.path(dir, "models/abcd/a_b_pt1/a/b/c1/"))
  file.create(file.path(dir, files))

  size <- sum(file.info(file.path(dir, files))$size)
  result <- listBundleFiles(dir)

  # Files are included in the list, count, and sizes, not directories.
  # Paths are enumerated relative to the target directory, not absolute paths.
  expect_identical(result$contents, files)
  expect_equal(result$totalSize, size)
  expect_equal(result$totalFiles, length(files))
})

test_that("ignores RStudio files", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("test.Rproj", ".Rproj.user", "rsconnect")))

  expect_equal(bundleFiles(dir), character())
})

test_that("ignores knitr cache directories", {
  dir <- withr::local_tempdir()
  dirCreate(file.path(dir, c("foo_cache", "bar_cache")))
  file.create(file.path(dir, c("foo_cache", "bar_cache"), "contents"))
  file.create(file.path(dir, c("foo.Rmd")))

  expect_setequal(bundleFiles(dir), c("bar_cache/contents", "foo.Rmd"))
})

test_that("ignores files anywhere in path", {
  dir <- withr::local_tempdir()
  dirCreate(file.path(dir, "a/b/c"))
  file.create(file.path(dir, c("x", "a/b/.gitignore", "a/b/c/.DS_Store")))

  expect_equal(bundleFiles(dir), "x")
})

test_that("ignores files listed in .rscignore", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, "a"))
  file.create(file.path(dir, c("x", "a/y")))
  expect_setequal(bundleFiles(dir), c("x", "a/y"))

  writeLines("x", file.path(dir, ".rscignore"))
  expect_setequal(bundleFiles(dir), "a/y")

  writeLines("y", file.path(dir, "a/.rscignore"))
  expect_setequal(bundleFiles(dir), character())
})

test_that("supports gitignore-style wildcard patterns in .rscignore", {
  dir <- local_temp_app()
  file.create(file.path(dir, c("app.log", "debug.log", "config.txt", "data.csv")))
  expect_setequal(bundleFiles(dir), c("app.log", "debug.log", "config.txt", "data.csv"))

  # Test wildcard patterns
  writeLines("*.log", file.path(dir, ".rscignore"))
  expect_setequal(bundleFiles(dir), c("config.txt", "data.csv"))
  
  # Test multiple patterns
  writeLines(c("*.log", "*.csv"), file.path(dir, ".rscignore"))
  expect_setequal(bundleFiles(dir), "config.txt")
})

test_that("supports gitignore-style directory patterns in .rscignore", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, c("logs", "temp", "src")))
  file.create(file.path(dir, c("app.R", "logs/error.log", "temp/cache.txt", "src/main.R")))
  
  # Test directory-only patterns
  writeLines("logs/", file.path(dir, ".rscignore"))
  result <- bundleFiles(dir)
  expect_true("temp/cache.txt" %in% result)
  expect_true("src/main.R" %in% result) 
  expect_true("app.R" %in% result)
  expect_false("logs/error.log" %in% result)
})

test_that("supports gitignore-style negation patterns in .rscignore", {
  dir <- local_temp_app()
  file.create(file.path(dir, c("app.log", "debug.log", "error.log", "config.txt")))
  
  # Test negation patterns
  writeLines(c("*.log", "!error.log"), file.path(dir, ".rscignore"))
  result <- bundleFiles(dir)
  expect_true("error.log" %in% result)
  expect_true("config.txt" %in% result)
  expect_false("app.log" %in% result)
  expect_false("debug.log" %in% result)
})

test_that("supports gitignore-style relative path patterns in .rscignore", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, "src"))
  file.create(file.path(dir, c("config.json", "src/config.json", "src/app.R")))
  
  # Test relative path patterns
  writeLines("/config.json", file.path(dir, ".rscignore"))
  result <- bundleFiles(dir)
  expect_false("config.json" %in% result)
  expect_true("src/config.json" %in% result)
  expect_true("src/app.R" %in% result)
})

test_that("supports simple gitignore-style double-asterisk patterns in .rscignore", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, c("logs", "temp")))
  file.create(file.path(dir, c("cache.tmp", "app.R", "logs/other.tmp", "temp/data.txt")))
  
  # Test 1: **/pattern (matches anywhere in current directory scope)
  writeLines("**/cache.tmp", file.path(dir, ".rscignore"))
  result <- bundleFiles(dir)
  expect_false("cache.tmp" %in% result)  # Ignored at root level
  expect_true("logs/other.tmp" %in% result)  # Different file in subdirectory
  expect_true("app.R" %in% result)
  expect_true("temp/data.txt" %in% result)
})

test_that("supports hierarchical .rscignore behavior with ** patterns", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, c("logs", "temp")))
  file.create(file.path(dir, c("cache.tmp", "app.R", "logs/other.tmp", "temp/data.txt")))
  
  # Test subdirectory .rscignore with ** pattern to ignore everything in temp/
  writeLines("**", file.path(dir, "temp/.rscignore"))  # Ignore everything in temp/
  result <- bundleFiles(dir)
  expect_false("temp/data.txt" %in% result) # Contents of temp/ are ignored
  expect_true("cache.tmp" %in% result)
  expect_true("app.R" %in% result)
  expect_true("logs/other.tmp" %in% result)
  
  # Test hierarchical behavior - subdirectory .rscignore with specific ** pattern
  writeLines("**/other.tmp", file.path(dir, "logs/.rscignore"))
  result <- bundleFiles(dir)
  expect_false("logs/other.tmp" %in% result) # Now ignored by subdirectory .rscignore
  expect_false("temp/data.txt" %in% result) # Still ignored by temp/.rscignore
  expect_true("cache.tmp" %in% result)
  expect_true("app.R" %in% result)
})

test_that("handles comments and empty lines in .rscignore", {
  dir <- local_temp_app()
  file.create(file.path(dir, c("app.log", "debug.log", "config.txt")))
  
  # Test with comments and empty lines
  writeLines(c(
    "# This is a comment",
    "*.log",
    "",
    "# Another comment",
    "   # Indented comment"
  ), file.path(dir, ".rscignore"))
  
  result <- bundleFiles(dir)
  expect_setequal(result, "config.txt")
})

test_that("handles edge cases in gitignore patterns", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, c("docs", "src")))
  file.create(file.path(dir, c("app.R", "config.txt", "docs/readme.md", "src/main.R")))
  
  # Test ** alone (matches everything)
  writeLines("**", file.path(dir, ".rscignore"))
  expect_length(bundleFiles(dir), 0)
  
  # Test **/ (matches all directories)
  writeLines("**/", file.path(dir, ".rscignore"))
  result <- bundleFiles(dir)
  expect_true("app.R" %in% result)
  expect_true("config.txt" %in% result)
  expect_false("docs/readme.md" %in% result)
  expect_false("src/main.R" %in% result)
})

test_that("maintains backward compatibility with simple .rscignore patterns", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, "a"))
  file.create(file.path(dir, c("simple_file.txt", "a/another_file.R")))
  
  # Test that simple string matching still works
  writeLines("simple_file.txt", file.path(dir, ".rscignore"))
  result <- bundleFiles(dir)
  expect_false("simple_file.txt" %in% result)
  expect_true("a/another_file.R" %in% result)
  
  # Test hierarchical behavior is preserved
  writeLines("another_file.R", file.path(dir, "a/.rscignore"))
  expect_length(bundleFiles(dir), 0)
})

test_that("ignores temporary files", {
  ignored <- ignoreBundleFiles(
    dir = ".",
    contents = c("foo.xlsx", "~$foo.xlsx", "foo.csv", "foo.csv~")
  )
  expect_equal(ignored, c("foo.xlsx", "foo.csv"))
})

test_that("ignores Python virtual envs (non-Windows)", {
  dir <- withr::local_tempdir()

  names <- c(
    # well-known names ...
    ".env",
    ".venv",
    "venv",

    # other names ...
    "test"
  )

  dirCreate(file.path(dir, names, "bin"))
  file.create(file.path(dir, names, "bin", "python"))

  expect_equal(bundleFiles(dir), character())
})

test_that("ignores Python virtual envs (Windows)", {
  dir <- withr::local_tempdir()

  names <- c(
    # well-known names ...
    ".env",
    ".venv",
    "venv",

    # other names ...
    "test"
  )

  dirCreate(file.path(dir, names, "Scripts"))
  file.create(file.path(dir, names, "Scripts", "python.exe"))

  expect_equal(bundleFiles(dir), character())
})

test_that("ignores Python virtual envs (Windows-GUI)", {
  dir <- withr::local_tempdir()

  names <- c(
    # well-known names ...
    ".env",
    ".venv",
    "venv",

    # other names ...
    "test"
  )

  dirCreate(file.path(dir, names, "Scripts"))
  file.create(file.path(dir, names, "Scripts", "pythonw.exe"))

  expect_equal(bundleFiles(dir), character())
})

test_that("ignores Python virtual envs (Windows-debug)", {
  dir <- withr::local_tempdir()

  names <- c(
    # well-known names ...
    ".env",
    ".venv",
    "venv",

    # other names ...
    "test"
  )

  dirCreate(file.path(dir, names, "Scripts"))
  file.create(file.path(dir, names, "Scripts", "pythond.exe"))

  expect_equal(bundleFiles(dir), character())
})

test_that("preserves well-known names when not Python virtual environment", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c(".env", ".venv", "venv")))

  expect_setequal(bundleFiles(dir), c(".env", ".venv", "venv"))
})

# explodeFiles ------------------------------------------------------------

test_that("returns relative paths", {
  dir <- withr::local_tempdir()
  dirCreate(file.path(dir, "x"))
  file.create(file.path(dir, "x", c("a", "b", "c")))

  expect_equal(explodeFiles(dir, "x"), c("x/a", "x/b", "x/c"))
})

test_that("drops drops non-existent files with warning", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("a", "b", "c")))

  expect_snapshot(out <- explodeFiles(dir, c("a", "d")))
  expect_equal(out, "a")
})

test_that("expands files and directory", {
  dir <- withr::local_tempdir()
  dirCreate(file.path(dir, "x"))
  file.create(file.path(dir, "x", c("a", "b")))
  file.create(file.path(dir, "c"))

  expect_equal(explodeFiles(dir, c("x", "c")), c("x/a", "x/b", "c"))
})

test_that("can include nested files/directories", {
  dir <- withr::local_tempdir()
  dirCreate(file.path(dir, "x", "y"))
  file.create(file.path(dir, "x", c("a", "b", "c")))
  file.create(file.path(dir, "x", "y", c("d", "e")))

  expect_equal(explodeFiles(dir, "x/a"), "x/a")
  expect_equal(explodeFiles(dir, "x/y"), c("x/y/d", "x/y/e"))
  expect_equal(explodeFiles(dir, c("x/a", "x/y/d")), c("x/a", "x/y/d"))
})

test_that("doesn't ignore user supplied files", {
  dir <- withr::local_tempdir()
  dirCreate(file.path(dir, "x", "y"))
  file.create(file.path(dir, "x", "packrat"))
  file.create(file.path(dir, "x", "y", "packrat"))

  expect_equal(explodeFiles(dir, "x"), c("x/packrat", "x/y/packrat"))
})

# enforceBundleLimits -----------------------------------------------------

test_that("explodeFiles() and bundleFiles() both eagerly enforce limits", {
  dir <- withr::local_tempdir()
  dirCreate(file.path(dir, c("a", "b")))
  file.create(file.path(dir, "a", letters))
  file.create(file.path(dir, "b", letters))

  withr::local_options(rsconnect.max.bundle.files = 1)

  # there are 52 files total, so eagerly implies we stop after one directory
  expect_error(explodeFiles(dir, c("a", "b")), "at least 2")
  expect_error(bundleFiles(dir), "at least 2")
})

test_that("generate nicely formatted messages", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("a", "b")))
  writeLines(letters, file.path(dir, "c"))

  withr::local_options(
    rsconnect.max.bundle.files = 1,
    rsconnect.max.bundle.size = 5
  )

  expect_snapshot(
    {
      explodeFiles(dir, c("a", "b"))
      explodeFiles(dir, "c")
    },
    error = TRUE,
    transform = function(x) {
      x <- gsub(dir, "<TEMPDIR>", x, fixed = TRUE)
      # file size is different on windows because of \r\n
      x <- gsub("\\d{2,} bytes", "?? bytes", x)
      x
    }
  )
})

# detectLongNames ---------------------------------------------------------

test_that("detectLongNames produces informative warning if needed", {
  skip_on_os("windows")

  dir <- local_temp_app(c("a.r" = "", "b.r" = "", "c.r" = ""))
  expect_snapshot(detectLongNames(dir, 0))
  expect_silent(detectLongNames(dir, Inf))
})
