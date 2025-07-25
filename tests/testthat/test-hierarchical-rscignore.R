# Streamlined hierarchical .rscignore tests (consolidated from 23 to 10 tests)

test_that("hierarchical inheritance works by default", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, c("src", "docs")))
  file.create(file.path(dir, c("app.log", "config.json", "src/debug.log", "src/config.json", "docs/error.log", "src/main.R")))
  
  # Test both wildcard and exact patterns inherit to subdirectories
  writeLines(c("*.log", "config.json"), file.path(dir, ".rscignore"))
  result <- bundleFiles(dir)
  
  # All .log files ignored (wildcard inheritance)
  expect_false("app.log" %in% result)
  expect_false("src/debug.log" %in% result) 
  expect_false("docs/error.log" %in% result)
  
  # All config.json files ignored (exact pattern inheritance)
  expect_false("config.json" %in% result)
  expect_false("src/config.json" %in% result)
  
  # Non-matching files preserved
  expect_true("src/main.R" %in% result)
})

test_that("child patterns override parent patterns with proper precedence", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, c("src", "src/utils")))
  file.create(file.path(dir, c("temp.txt", "src/temp.txt", "src/utils/temp.txt", "src/main.R")))
  
  # Root: ignore temp.txt everywhere
  writeLines("temp.txt", file.path(dir, ".rscignore"))
  
  # Child: un-ignore temp.txt in src/
  writeLines("!temp.txt", file.path(dir, "src/.rscignore"))
  
  # Grandchild: re-ignore temp.txt in src/utils/
  writeLines("temp.txt", file.path(dir, "src/utils/.rscignore"))
  
  result <- bundleFiles(dir)
  
  expect_false("temp.txt" %in% result)           # Root: ignored
  expect_true("src/temp.txt" %in% result)        # Child: un-ignored (overrides parent)
  expect_false("src/utils/temp.txt" %in% result) # Grandchild: re-ignored (overrides parent negation)
  expect_true("src/main.R" %in% result)          # Unaffected
})

test_that("legacy vs hierarchical mode behavioral differences", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, "src"))
  file.create(file.path(dir, c("debug.log", "src/debug.log", "src/main.R")))
  writeLines("*.log", file.path(dir, ".rscignore"))
  
  # LEGACY MODE: only affects same directory
  withr::local_options(rsconnect.rscignore.legacy = TRUE)
  result_legacy <- suppressWarnings(bundleFiles(dir))
  expect_false("debug.log" %in% result_legacy)
  expect_true("src/debug.log" %in% result_legacy) # NOT inherited
  
  # HIERARCHICAL MODE: affects subdirectories too
  withr::local_options(rsconnect.rscignore.legacy = FALSE)
  result_hierarchical <- bundleFiles(dir)
  expect_false("debug.log" %in% result_hierarchical)
  expect_false("src/debug.log" %in% result_hierarchical) # Inherited
})

test_that("gitignore-style patterns work correctly", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, c("src", "logs")))
  file.create(file.path(dir, c("config.json", "src/config.json", "src/app.R")))
  file.create(file.path(dir, c("logs/debug.log", "logs/access.log")))
  
  # Test relative path patterns (should only affect same directory)
  writeLines(c("/config.json", "logs/"), file.path(dir, ".rscignore"))
  result <- bundleFiles(dir)
  
  expect_false("config.json" %in% result)      # Root config.json ignored by /config.json
  expect_true("src/config.json" %in% result)   # Subdirectory config.json NOT ignored by /config.json
  expect_false("logs/debug.log" %in% result)   # Directory pattern ignores contents
  expect_false("logs/access.log" %in% result)  # Directory pattern ignores contents
  expect_true("src/app.R" %in% result)         # Unaffected file
})

test_that("error handling and edge cases", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, "src"))
  file.create(file.path(dir, c("test.txt", "src/test.txt")))
  
  # Test empty .rscignore file
  writeLines("", file.path(dir, ".rscignore"))
  expect_no_error(result1 <- bundleFiles(dir))
  expect_true("test.txt" %in% result1)
  expect_true("src/test.txt" %in% result1)
  
  # Test missing bundle_root (should fallback gracefully)
  contents <- c("test.txt", "src/test.txt")
  expect_no_error(result2 <- applyRscignorePatterns(contents, dir, bundle_root = NULL))
  
  # Test with mocked error in hierarchical processing
  with_mocked_bindings(
    collectHierarchicalPatterns = function(...) stop("Test error"),
    {
      expect_warning(
        result3 <- applyRscignorePatterns("test.txt", dir, dir),
        "Error in hierarchical pattern processing"
      )
      expect_warning(
        result3 <- applyRscignorePatterns("test.txt", dir, dir),
        "Falling back to directory-scoped patterns"
      )
    }
  )
})

# =============================================================================
# CONSOLIDATED BREAKING CHANGE TESTS (was 5 tests, now 3 tests)
# =============================================================================

test_that("BREAKING CHANGE: parent patterns now inherit to subdirectories", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, c("src", "docs")))
  file.create(file.path(dir, c("debug.log", "config.json", "src/debug.log", "src/config.json", "docs/readme.md")))
  
  # Root patterns for both wildcard and exact matching
  writeLines(c("*.log", "config.json"), file.path(dir, ".rscignore"))
  
  # LEGACY MODE: Patterns only affect same directory
  withr::local_options(rsconnect.rscignore.legacy = TRUE)
  # Suppress warnings since lifecycle may have already emitted them
  result_legacy <- suppressWarnings(bundleFiles(dir))
  legacy_affected <- sum(!c("debug.log", "config.json") %in% result_legacy)        # Root files ignored
  legacy_inherited <- sum(!c("src/debug.log", "src/config.json") %in% result_legacy) # Subdir files ignored
  expect_equal(legacy_affected, 2)  # Root files ignored
  expect_equal(legacy_inherited, 0) # Subdir files NOT ignored (old behavior)
  
  # HIERARCHICAL MODE: Patterns inherit to subdirectories  
  withr::local_options(rsconnect.rscignore.legacy = FALSE)
  result_hierarchical <- bundleFiles(dir)
  hierarchical_affected <- sum(!c("debug.log", "config.json") %in% result_hierarchical)        # Root files ignored
  hierarchical_inherited <- sum(!c("src/debug.log", "src/config.json") %in% result_hierarchical) # Subdir files ignored
  expect_equal(hierarchical_affected, 2)   # Root files ignored
  expect_equal(hierarchical_inherited, 2)  # Subdir files NOW ignored (new behavior)
  
  # Document the breaking change scope
  expect_gt(hierarchical_inherited, legacy_inherited) # More files ignored in hierarchical mode
})

test_that("BREAKING CHANGE: negation patterns now work across directory boundaries", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, c("src", "important")))
  file.create(file.path(dir, c("temp.txt", "src/temp.txt", "important/temp.txt", "important/critical.R")))
  
  # Root: ignore all temp.txt files
  writeLines("temp.txt", file.path(dir, ".rscignore"))
  
  # Subdirectory: selectively un-ignore temp.txt  
  writeLines("!temp.txt", file.path(dir, "important/.rscignore"))
  
  # LEGACY MODE: Negation only works locally, parent patterns don't inherit
  withr::local_options(rsconnect.rscignore.legacy = TRUE)
  result_legacy <- suppressWarnings(bundleFiles(dir))
  expect_false("temp.txt" %in% result_legacy)          # Root ignored
  expect_true("src/temp.txt" %in% result_legacy)       # OLD: Not affected by root pattern
  expect_true("important/temp.txt" %in% result_legacy) # OLD: Negation works but parent didn't apply
  
  # HIERARCHICAL MODE: Negation overrides inherited parent patterns
  withr::local_options(rsconnect.rscignore.legacy = FALSE)
  result_hierarchical <- bundleFiles(dir)
  expect_false("temp.txt" %in% result_hierarchical)          # Root ignored
  expect_false("src/temp.txt" %in% result_hierarchical)      # NEW: Ignored by inherited root pattern
  expect_true("important/temp.txt" %in% result_hierarchical) # NEW: Un-ignored by child negation
  
  # Verify non-matching files unaffected
  expect_true("important/critical.R" %in% result_legacy)
  expect_true("important/critical.R" %in% result_hierarchical)
})

test_that("BREAKING CHANGE: pattern scope and precedence differences", {
  dir <- local_temp_app()
  dirCreate(file.path(dir, c("logs", "backup", "src")))
  file.create(file.path(dir, c("app.log", "logs/access.log", "backup/old.log", "src/error.log", "src/main.R")))
  
  # Root wildcard pattern
  writeLines("*.log", file.path(dir, ".rscignore"))
  
  # LEGACY MODE: Wildcard only affects same directory
  withr::local_options(rsconnect.rscignore.legacy = TRUE)
  result_legacy <- suppressWarnings(bundleFiles(dir))
  legacy_logs <- length(result_legacy[grepl("\\.log$", result_legacy)])
  expect_gt(legacy_logs, 0) # Some .log files survive in subdirectories
  
  # HIERARCHICAL MODE: Wildcard affects all subdirectories
  withr::local_options(rsconnect.rscignore.legacy = FALSE)
  result_hierarchical <- bundleFiles(dir)
  hierarchical_logs <- length(result_hierarchical[grepl("\\.log$", result_hierarchical)])
  expect_equal(hierarchical_logs, 0) # No .log files survive anywhere
  
  # Document the scope expansion
  expect_gt(legacy_logs, hierarchical_logs) # Hierarchical ignores more files
  
  # Verify non-matching files unaffected in both modes
  expect_true("src/main.R" %in% result_legacy)
  expect_true("src/main.R" %in% result_hierarchical)
})

test_that("complex scenarios work correctly", {
  # Test multiple inheritance levels with various pattern types
  dir <- local_temp_app()
  dirCreate(file.path(dir, c("src", "src/utils", "tests")))
  file.create(file.path(dir, c("app.log", "src/debug.log", "src/utils/trace.log", "tests/unit.log")))
  file.create(file.path(dir, c("src/main.R", "src/utils/helper.R", "tests/test.R")))
  
  # Root: ignore all .log files
  writeLines("*.log", file.path(dir, ".rscignore"))
  
  # Subdirectory: allow debug.log specifically
  writeLines("!debug.log", file.path(dir, "src/.rscignore"))
  
  result <- bundleFiles(dir)
  
  # Complex inheritance and precedence
  expect_false("app.log" %in% result)           # Root .log ignored
  expect_true("src/debug.log" %in% result)      # Un-ignored by child negation
  expect_false("src/utils/trace.log" %in% result) # Ignored by inherited root pattern (no local override)
  expect_false("tests/unit.log" %in% result)    # Ignored by inherited root pattern
  
  # Non-.log files preserved
  expect_true("src/main.R" %in% result)
  expect_true("src/utils/helper.R" %in% result)  
  expect_true("tests/test.R" %in% result)
})

test_that("parameter validation and option handling", {
  dir <- local_temp_app()
  file.create(file.path(dir, "test.txt"))
  
  # Test bundle_root parameter validation
  expect_no_error(applyRscignorePatterns("test.txt", dir, bundle_root = dir))
  expect_no_error(applyRscignorePatterns("test.txt", dir, bundle_root = NULL))
  
  # Test option validation
  withr::local_options(rsconnect.rscignore.legacy = NULL)
  expect_no_error(bundleFiles(dir)) # NULL should default to FALSE
  
  withr::local_options(rsconnect.rscignore.legacy = FALSE)
  expect_no_error(bundleFiles(dir))
  
  withr::local_options(rsconnect.rscignore.legacy = TRUE)
  suppressWarnings(bundleFiles(dir))  # Test that legacy mode works without error
  
  # Test invalid option values
  withr::local_options(rsconnect.rscignore.legacy = "invalid")
  expect_error(bundleFiles(dir), "must be TRUE, FALSE, or NULL")
  
  withr::local_options(rsconnect.rscignore.legacy = 42)
  suppressWarnings(bundleFiles(dir))  # Should convert to TRUE and work
}) 