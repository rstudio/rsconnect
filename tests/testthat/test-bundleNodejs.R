# inferNodejsInfo -----------------------------------------------------------
# (mirrors test-bundlePython.R which tests getPython, inferPythonEnv, etc.)

test_that("inferNodejsInfo reads main field for entrypoint", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "server.js"}'
  ))
  info <- inferNodejsInfo(dir)
  expect_equal(info$entrypoint, "server.js")
})

test_that("inferNodejsInfo defaults to index.js when main is missing", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test"}'
  ))
  info <- inferNodejsInfo(dir)
  expect_equal(info$entrypoint, "index.js")
})

test_that("inferNodejsInfo defaults to index.js when main is empty string", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": ""}'
  ))
  info <- inferNodejsInfo(dir)
  expect_equal(info$entrypoint, "index.js")
})

test_that("inferNodejsInfo handles TypeScript entrypoint", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "app.ts"}'
  ))
  info <- inferNodejsInfo(dir)
  expect_equal(info$entrypoint, "app.ts")
})

test_that("inferNodejsInfo reads engines.node", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "engines": {"node": ">=22.18.0"}}'
  ))
  info <- inferNodejsInfo(dir)
  expect_equal(info$enginesNode, ">=22.18.0")
})

test_that("inferNodejsInfo returns NULL enginesNode when engines not set", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "main": "app.js"}'
  ))
  info <- inferNodejsInfo(dir)
  expect_null(info$enginesNode)
})

test_that("inferNodejsInfo returns NULL enginesNode when engines has no node field", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test", "engines": {"npm": ">=8.0.0"}}'
  ))
  info <- inferNodejsInfo(dir)
  expect_null(info$enginesNode)
})

test_that("inferNodejsInfo detects missing package-lock.json", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test"}'
  ))
  info <- inferNodejsInfo(dir)
  expect_false(info$hasLockfile)
})

test_that("inferNodejsInfo detects present package-lock.json", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test"}',
    "package-lock.json" = "{}"
  ))
  info <- inferNodejsInfo(dir)
  expect_true(info$hasLockfile)
})

# Node.js bundle file exclusions --------------------------------------------

test_that("node_modules is excluded from bundle files", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test"}',
    "app.js" = ""
  ))
  dir.create(file.path(dir, "node_modules", "express"), recursive = TRUE)
  file.create(file.path(dir, "node_modules", "express", "index.js"))

  files <- bundleFiles(dir)
  expect_false(any(grepl("node_modules", files)))
})

test_that(".npm is excluded from bundle files", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test"}',
    "app.js" = ""
  ))
  dir.create(file.path(dir, ".npm"), recursive = TRUE)
  file.create(file.path(dir, ".npm", "cache-file"))

  files <- bundleFiles(dir)
  expect_false(any(grepl("\\.npm", files)))
})

test_that("node_modules excluded via listDeploymentFiles", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test"}',
    "app.js" = ""
  ))
  dir.create(file.path(dir, "node_modules", "express"), recursive = TRUE)
  file.create(file.path(dir, "node_modules", "express", "index.js"))

  files <- listDeploymentFiles(dir)
  expect_false(any(grepl("node_modules", files)))
  expect_true("package.json" %in% files)
  expect_true("app.js" %in% files)
})

test_that("deeply nested node_modules is excluded", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test"}',
    "app.js" = ""
  ))
  dir.create(
    file.path(dir, "node_modules", "express", "node_modules", "qs"),
    recursive = TRUE
  )
  file.create(file.path(
    dir,
    "node_modules",
    "express",
    "node_modules",
    "qs",
    "index.js"
  ))

  files <- bundleFiles(dir)
  expect_false(any(grepl("node_modules", files)))
})

test_that("non-nodejs files alongside package.json are included in bundle", {
  dir <- local_temp_app(list(
    "package.json" = '{"name": "test"}',
    "app.js" = "",
    "public/index.html" = "<html></html>",
    "lib/utils.js" = "module.exports = {};"
  ))

  files <- bundleFiles(dir)
  expect_true("app.js" %in% files)
  expect_true("public/index.html" %in% files)
  expect_true("lib/utils.js" %in% files)
})
