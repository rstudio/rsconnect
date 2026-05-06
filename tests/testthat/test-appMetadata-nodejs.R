test_that("package.json is detected as nodejs", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "app.js"}',
    "package-lock.json" = '{"lockfileVersion": 3}',
    "app.js" = ""
  ))
  files <- list.files(dir)
  result <- inferAppMode(dir, files)
  expect_equal(result$appMode, "nodejs")
})

test_that("R files take priority over package.json", {
  dir <- local_temp_app(list(
    "app.R" = "",
    "package.json" = '{"name": "test"}',
    "package-lock.json" = "{}"
  ))
  files <- list.files(dir)
  result <- inferAppMode(dir, files)
  expect_equal(result$appMode, "shiny")
})

test_that("Rmd files take priority over package.json", {
  dir <- local_temp_app(list(
    "index.Rmd" = "",
    "package.json" = '{"name": "test"}',
    "package-lock.json" = "{}"
  ))
  files <- list.files(dir)
  result <- inferAppMode(dir, files)
  expect_equal(result$appMode, "rmd-static")
})

test_that("plumber API takes priority over package.json", {
  dir <- local_temp_app(list(
    "plumber.R" = "",
    "package.json" = '{"name": "test"}',
    "package-lock.json" = "{}"
  ))
  files <- list.files(dir)
  result <- inferAppMode(dir, files)
  expect_equal(result$appMode, "api")
})

test_that("Quarto .qmd files take priority over package.json", {
  dir <- local_temp_app(list(
    "index.qmd" = "",
    "package.json" = '{"name": "test"}',
    "package-lock.json" = "{}"
  ))
  files <- list.files(dir)
  result <- inferAppMode(dir, files)
  expect_equal(result$appMode, "quarto-static")
})

test_that("server.R takes priority over package.json", {
  dir <- local_temp_app(list(
    "server.R" = "",
    "package.json" = '{"name": "test"}',
    "package-lock.json" = "{}"
  ))
  files <- list.files(dir)
  result <- inferAppMode(dir, files)
  expect_equal(result$appMode, "shiny")
})

test_that("package.json takes priority over static HTML", {
  dir <- local_temp_app(list(
    "index.html" = "<html></html>",
    "package.json" = '{"name": "test", "main": "app.js"}',
    "package-lock.json" = "{}",
    "app.js" = ""
  ))
  files <- list.files(dir)
  result <- inferAppMode(dir, files)
  expect_equal(result$appMode, "nodejs")
})

test_that("package.json in subdirectory does not trigger nodejs", {
  dir <- local_temp_app(list(
    "index.html" = "<html></html>",
    "subdir/package.json" = '{"name": "test"}'
  ))
  files <- c("index.html", "subdir/package.json")
  result <- inferAppMode(dir, files)
  expect_equal(result$appMode, "static")
})

test_that("appMode = 'nodejs' can be set explicitly on a non-nodejs directory", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "app.js"}',
    "package-lock.json" = "{}",
    "app.js" = "",
    "app.R" = ""
  ))
  files <- list.files(dir)
  # app.R takes priority by default
  metadata <- appMetadata(dir, files)
  expect_equal(metadata$appMode, "shiny")

  # can override to force nodejs
  metadata <- appMetadata(dir, files, appMode = "nodejs")
  expect_equal(metadata$appMode, "nodejs")
  expect_equal(metadata$nodejsInfo$entrypoint, "app.js")
})

# inferNodejsInfo tests live in test-bundleNodejs.R
# (paralleling test-bundlePython.R for Python helper functions)

# appMetadata - Node.js -----------------------------------------------------

test_that("appMetadata errors when package-lock.json is missing", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "app.js"}',
    "app.js" = ""
  ))
  files <- list.files(dir)
  expect_error(
    appMetadata(dir, files),
    "package-lock.json"
  )
})

test_that("appMetadata returns nodejsInfo for nodejs content", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "app.js", "engines": {"node": ">=22.18.0"}}',
    "package-lock.json" = '{"lockfileVersion": 3}',
    "app.js" = ""
  ))
  files <- list.files(dir)
  metadata <- appMetadata(dir, files)
  expect_equal(metadata$appMode, "nodejs")
  expect_equal(metadata$nodejsInfo$entrypoint, "app.js")
  expect_equal(metadata$nodejsInfo$enginesNode, ">=22.18.0")
})

test_that("appMetadata warns when entrypoint file is missing", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "dist/server.js"}',
    "package-lock.json" = "{}"
  ))
  files <- list.files(dir)
  expect_warning(
    appMetadata(dir, files),
    "dist/server.js"
  )
})

test_that("appMetadata allows appMode override from nodejs", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test"}',
    "package-lock.json" = "{}",
    "index.html" = ""
  ))
  files <- list.files(dir)
  metadata <- appMetadata(dir, files, appMode = "static")
  expect_equal(metadata$appMode, "static")
  expect_null(metadata$nodejsInfo)
})

test_that("appMetadata returns NULL appPrimaryDoc for nodejs", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "app.js"}',
    "package-lock.json" = "{}",
    "app.js" = ""
  ))
  files <- list.files(dir)
  metadata <- appMetadata(dir, files)
  expect_null(metadata$appPrimaryDoc)
})

test_that("appMetadata returns FALSE hasParameters for nodejs", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "app.js"}',
    "package-lock.json" = "{}",
    "app.js" = ""
  ))
  files <- list.files(dir)
  metadata <- appMetadata(dir, files)
  expect_false(metadata$hasParameters)
})

test_that("appMetadata returns FALSE documentsHavePython for nodejs", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "app.js"}',
    "package-lock.json" = "{}",
    "app.js" = ""
  ))
  files <- list.files(dir)
  metadata <- appMetadata(dir, files)
  expect_false(metadata$documentsHavePython)
})

test_that("appMetadata returns NULL quartoInfo for nodejs", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "app.js"}',
    "package-lock.json" = "{}",
    "app.js" = ""
  ))
  files <- list.files(dir)
  metadata <- appMetadata(dir, files)
  expect_null(metadata$quartoInfo)
})

test_that("appMetadata returns NULL plumberInfo for nodejs", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "app.js"}',
    "package-lock.json" = "{}",
    "app.js" = ""
  ))
  files <- list.files(dir)
  metadata <- appMetadata(dir, files)
  expect_null(metadata$plumberInfo)
})

test_that("needsR returns FALSE for nodejs", {
  metadata <- list(appMode = "nodejs")
  expect_false(needsR(metadata))
})
