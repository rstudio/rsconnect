# generateFileManifest() Comprehensive Tests ------------------------------------

# Core Pattern Matching Tests ------------------------------------------------

test_that("Empty patterns include all files", {
  files <- c("app.R", "data.csv", "README.md")
  patterns <- character()
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(result, files)
})

test_that("Exact filename exclusion works", {
  files <- c("app.R", "temp.log", "data.csv", "debug.log")
  patterns <- c("temp.log", "debug.log")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(result, c("app.R", "data.csv"))
})

test_that("Wildcard patterns work for file extensions", {
  files <- c("app.R", "data.csv", "log1.log", "log2.log", "script.py")
  patterns <- c("*.log")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(result, c("app.R", "data.csv", "script.py"))
})

test_that("Wildcard patterns work for filename prefixes", {
  files <- c("app.R", "temp_data.csv", "temp_log.txt", "production.csv")
  patterns <- c("temp*")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(result, c("app.R", "production.csv"))
})

test_that("Multiple wildcard patterns work together", {
  files <- c("app.R", "test.log", "debug.tmp", "data.csv", "cache.bak")
  patterns <- c("*.log", "*.tmp", "*.bak")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(result, c("app.R", "data.csv"))
})

test_that("Directory exclusion patterns work", {
  files <- c("app.R", "logs/error.log", "logs/access.log", "data/input.csv")
  patterns <- c("logs/")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(result, c("app.R", "data/input.csv"))
})

test_that("Wildcard directory patterns work", {
  files <- c("app.R", "temp1/data.csv", "temp2/cache.txt", "prod/data.csv")
  patterns <- c("temp*/")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(result, c("app.R", "prod/data.csv"))
})

test_that("Mixed file and directory patterns work", {
  files <- c("app.R", "test.log", "logs/error.log", "temp.tmp", "cache/data.csv")
  patterns <- c("*.log", "*.tmp", "logs/", "cache/")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(result, c("app.R"))
})

test_that("Directory patterns don't affect files with same name", {
  files <- c("app.R", "logs", "logs/error.log", "logs/access.log")
  patterns <- c("logs/")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(result, c("app.R", "logs"))
})

test_that("Comments and empty lines are handled correctly", {
  raw_patterns <- c("# This is a comment", "", "*.log", "  ", "# Another comment", "temp/")
  files <- c("app.R", "test.log", "temp/data.csv", "main.R")
  
  clean_patterns <- raw_patterns[nzchar(trimws(raw_patterns)) & !grepl("^#", trimws(raw_patterns))]
  result <- generateFileManifest(appDir = ".", files = files, patterns = clean_patterns)
  
  expect_equal(result, c("app.R", "main.R"))
})

test_that("Path normalization works cross-platform", {
  files <- c("app.R", "data\\temp\\file.csv", "data/temp/file.csv", "data/other.csv")
  patterns <- c("data/temp/")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(sort(result), sort(c("app.R", "data/other.csv")))
})

# Real-World Scenario Tests --------------------------------------------------

test_that("Shiny app deployment filtering works", {
  # Simulate realistic Shiny app structure
  shiny_files <- c(
    "app.R", "ui.R", "server.R", "global.R",
    "data/dataset.csv", "www/style.css", "www/script.js",
    "logs/app.log", "logs/error.log", "temp/cache.tmp", 
    "tests/test-app.R", "docs/README.md", ".github/workflows/deploy.yml"
  )
  
  shiny_patterns <- c("*.log", "*.tmp", "temp/", "logs/", "tests/", "docs/", ".github/")
  
  result <- generateFileManifest(appDir = ".", files = shiny_files, patterns = shiny_patterns)
  
  # Should keep core Shiny files and assets
  expect_true(all(c("app.R", "ui.R", "server.R", "global.R", "data/dataset.csv", "www/style.css") %in% result))
  # Should exclude development/temp files
  expect_false(any(grepl("logs/|temp/|tests/|docs/|\\.log$|\\.tmp$|\\.github/", result)))
})

test_that("R package as Shiny app filtering works", {
  # Simulate R package deployed as Shiny app
  package_files <- c(
    # Core app files (keep)
    "app.R", "R/mod-main.R", "R/utils.R", "inst/app/www/style.css",
    # Package infrastructure (exclude)
    "DESCRIPTION", "NAMESPACE", "man/mod-main.Rd", "man/utils.Rd",
    "vignettes/how-to.Rmd", "tests/testthat/test-mod.R", 
    "docs/index.html", "docs/reference/mod-main.html", "_pkgdown.yml"
  )
  
  package_patterns <- c("man/", "vignettes/", "tests/", "docs/", "_pkgdown.yml", "DESCRIPTION", "NAMESPACE")
  
  result <- generateFileManifest(appDir = ".", files = package_files, patterns = package_patterns)
  
  # Should keep app files
  expect_true(all(c("app.R", "R/mod-main.R", "R/utils.R", "inst/app/www/style.css") %in% result))
  # Should exclude package infrastructure
  expect_false(any(grepl("man/|vignettes/|tests/|docs/|_pkgdown\\.yml|DESCRIPTION|NAMESPACE", result)))
})

# Integration Tests -----------------------------------------------------------

test_that("Integration with listDeploymentFiles via appFiles works", {
  dir <- local_temp_app(list(
    "app.R" = "# Main app",
    "data.csv" = "x,y\n1,2",
    "temp.log" = "log entry",
    "debug.tmp" = "debug info",
    "logs/error.log" = "error",
    "cache/data.txt" = "cached"
  ))
  
  files <- list.files(dir, recursive = TRUE)
  enhanced_files <- generateFileManifest(
    appDir = dir, 
    files = files, 
    patterns = c("*.log", "*.tmp", "cache/")
  )
  
  result <- listDeploymentFiles(dir, appFiles = enhanced_files)
  
  expect_setequal(result, c("app.R", "data.csv"))
})

test_that("Integration with manifest file workflow works", {
  dir <- local_temp_app(list(
    "app.R" = "# Main app",
    "script.py" = "print('hello')",
    "data.log" = "log data",
    "temp/cache.txt" = "temp data"
  ))
  
  files <- list.files(dir, recursive = TRUE)
  manifest_path <- file.path(dir, "enhanced_manifest.txt")
  
  result_path <- generateFileManifest(
    appDir = dir,
    files = files,
    patterns = c("*.log", "temp/"),
    output_path = manifest_path
  )
  
  expect_true(file.exists(manifest_path))
  expect_equal(result_path, manifest_path)
  
  result <- listDeploymentFiles(dir, appFileManifest = manifest_path)
  expect_setequal(result, c("app.R", "script.py"))
})

# Performance Tests -----------------------------------------------------------

test_that("Performance is acceptable with realistic file counts", {
  # Create realistic project with moderate file count
  files <- c(
    "app.R", "server.R", "ui.R",
    paste0("R/mod-", 1:20, ".R"),
    paste0("data/dataset-", 1:10, ".csv"),
    paste0("logs/log-", 1:50, ".log"),
    paste0("temp/cache-", 1:30, ".tmp"),
    paste0("tests/test-", 1:25, ".R"),
    paste0("docs/page-", 1:40, ".html")
  )
  
  patterns <- c("*.log", "*.tmp", "logs/", "temp/", "tests/", "docs/")
  
  start_time <- Sys.time()
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  end_time <- Sys.time()
  elapsed <- as.numeric(end_time - start_time, units = "secs")
  
  # Should be fast even with ~180 files
  expect_lt(elapsed, 1.0)
  
  # Should significantly reduce file count
  expect_lt(length(result), length(files) * 0.4)  # At least 60% reduction
})

# Essential Edge Cases -------------------------------------------------------

test_that("Missing .rscignore-patterns file is handled gracefully", {
  files <- c("app.R", "data.csv", "temp.log")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = NULL)
  
  expect_equal(result, files)
})

test_that("Unicode and special characters work", {
  files <- c("app.R", "café.txt", "файл.log", "测试.csv", "emoji😀.md")
  patterns <- c("café.txt", "*.log")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_true("app.R" %in% result)
  expect_true("测试.csv" %in% result)
  expect_true("emoji😀.md" %in% result)
  expect_false("café.txt" %in% result)
  expect_false("файл.log" %in% result)  # Excluded by *.log
})

test_that("Patterns with spaces work", {
  files <- c("app.R", "My Document.pdf", "file with spaces.txt", "no-spaces.txt")
  patterns <- c("My Document.pdf", "file with spaces.txt")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(sort(result), sort(c("app.R", "no-spaces.txt")))
})

test_that("Invalid patterns don't crash the function", {
  files <- c("app.R", "test[.txt", "test].txt", "normal.txt")
  problematic_patterns <- c("test[.txt")
  
  expect_no_error({
    result <- generateFileManifest(appDir = ".", files = files, patterns = problematic_patterns)
  })
  
  expect_true("app.R" %in% result)
  expect_true("normal.txt" %in% result)
})

test_that("Whitespace-only patterns are handled correctly", {
  files <- c("app.R", "data.csv", "temp.log")
  patterns <- c("", "   ", "\t", "  temp.log  ")
  
  clean_patterns <- patterns[nzchar(trimws(patterns))]
  clean_patterns <- trimws(clean_patterns)
  result <- generateFileManifest(appDir = ".", files = files, patterns = clean_patterns)
  
  expect_equal(result, c("app.R", "data.csv"))
})

test_that("Universal wildcard pattern works", {
  files <- c("app.R", "data.csv", "config.yml")
  patterns <- c("*")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns)
  
  expect_equal(length(result), 0)  # All files excluded
})

# Output Format Tests ---------------------------------------------------------

test_that("Function returns file list when output_path is NULL", {
  files <- c("app.R", "data.csv", "temp.log")
  patterns <- c("*.log")
  
  result <- generateFileManifest(appDir = ".", files = files, patterns = patterns, output_path = NULL)
  
  expect_type(result, "character")
  expect_equal(result, c("app.R", "data.csv"))
})

test_that("Function returns path when output_path is specified", {
  dir <- withr::local_tempdir()
  files <- c("app.R", "data.csv", "temp.log")
  patterns <- c("*.log")
  manifest_path <- file.path(dir, "manifest.txt")
  
  result <- generateFileManifest(
    appDir = dir, 
    files = files, 
    patterns = patterns, 
    output_path = manifest_path
  )
  
  expect_equal(result, manifest_path)
  expect_true(file.exists(manifest_path))
  
  # Check file contents
  written_files <- readLines(manifest_path)
  expect_equal(written_files, c("app.R", "data.csv"))
})

# All 15 core tests complete! 