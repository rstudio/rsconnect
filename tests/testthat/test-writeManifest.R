makeManifest <- function(appDir, appPrimaryDoc, python = NULL, quarto = NULL, image = NULL) {
  writeManifest(
    appDir,
    appPrimaryDoc = appPrimaryDoc,
    python = python,
    quarto = quarto,
    image = image
  )
  manifestFile <- file.path(appDir, "manifest.json")
  data <- readLines(manifestFile, warn = FALSE, encoding = "UTF-8")
  manifestJson <- jsonlite::fromJSON(data)
  unlink(manifestFile)
  manifestJson
}

test_that("Rmd with reticulate as a dependency includes python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  python <- pythonPathOrSkip()

  appDir <- test_path("test-reticulate-rmds")
  manifest <- makeManifest(appDir, NULL, python = python)
  requirements_file <- file.path(appDir, manifest$python$package_manager$package_file)
  expect_equal(requirements_file, "test-reticulate-rmds/requirements.txt")
  on.exit(unlink(requirements_file))

  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "index.Rmd")
  expect_true(file.exists(requirements_file))
})

test_that("Rmd with reticulate as an inferred dependency includes reticulate and python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  python <- pythonPathOrSkip()

  appDir <- test_path("test-reticulate-rmds")
  manifest <- makeManifest(appDir, "implicit.Rmd", python = python)
  requirements_file <- file.path(appDir, manifest$python$package_manager$package_file)
  expect_equal(requirements_file, "test-reticulate-rmds/requirements.txt")
  on.exit(unlink(requirements_file))

  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "implicit.Rmd")
  expect_true(file.exists(requirements_file))
})

test_that("Rmd without a python block doesn't include reticulate or python in the manifest", {
  skip_on_cran()

  manifest <- makeManifest(test_path("test-rmds"), "simple.Rmd", python = NULL)
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_null(manifest$python)
})

test_that("Rmd without a python block doesn't include reticulate or python in the manifest even if python specified", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  python <- pythonPathOrSkip()

  manifest <- makeManifest("test-rmds", "simple.Rmd", python = python)
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$python, NULL)
  # Confirm that we have removed packrat entries from our file listing but
  # retain entries for other files.
  filenames <- names(manifest$files)
  expect_false(any(grepl("^packrat/", filenames, perl = TRUE)), filenames)
  expect_true(any(grepl("simple.Rmd", filenames, fixed = TRUE)), filenames)
})

# Quarto Tests

test_that("Quarto website includes quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("quarto-website-r")
  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "index.qmd")
})

test_that("Quarto document includes quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("quarto-doc-none")
  appPrimaryDoc <- "quarto-doc-none.qmd"
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "markdown")
  expect_equal(manifest$metadata$primary_rmd, "quarto-doc-none.qmd")
})

test_that("Specifying quarto arg includes quarto in the manifest, even with no appPrimaryDoc specified (.qmd)", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("quarto-doc-none")
  appPrimaryDoc <- NULL
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "markdown")
  expect_equal(manifest$metadata$primary_rmd, "quarto-doc-none.qmd")
})

test_that("Specifying quarto arg includes quarto in the manifest, even with no appPrimaryDoc specified (.Rmd)", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("shiny-rmds")
  appPrimaryDoc <- NULL
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-shiny")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "non-shiny-rmd.Rmd")
})

test_that("specifying quarto arg with non-quarto app does not include quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("shinyapp-singleR")
  appPrimaryDoc <- "single.R"
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_null(manifest$quarto)
})

test_that("Quarto shiny project includes quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("quarto-proj-r-shiny")
  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-shiny")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "quarto-proj-r-shiny.qmd")
})

test_that("Quarto R + Python website includes quarto and python in the manifest", {
  skip_if_not_installed("reticulate")
  quarto <- quartoPathOrSkip()
  python <- pythonPathOrSkip()

  appDir <- test_path("quarto-website-r-py")
  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, python = python, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "index.qmd")

  expect_true(all(c("quarto", "python") %in% names(manifest)))
  expect_true("reticulate" %in% names(manifest$packages))
})

test_that("Quarto Python-only website gets correct manifest data", {
  skip_if_not_installed("reticulate")
  quarto <- quartoPathOrSkip()
  python <- pythonPathOrSkip()

  appDir <- test_path("quarto-website-py")
  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, python = python, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "jupyter")
  expect_equal(manifest$metadata$primary_rmd, "index.qmd")

  # We expect Quarto and Python metadata, but no R packages.
  expect_true(all(c("quarto", "python") %in% names(manifest)))
  expect_null(manifest$packages)
})

test_that("Deploying a Quarto project without Quarto info in an error", {
  appDir <- test_path("quarto-website-r")
  expect_snapshot(
    makeManifest(appDir, appPrimaryDoc = NULL, quarto = NULL),
    error = TRUE
  )
})

test_that("Deploying a Quarto doc without Quarto info in an error", {
  appDir <- test_path("quarto-doc-none")
  appPrimaryDoc <- "quarto-doc-none.qmd"
  expect_snapshot(
    makeManifest(appDir, appPrimaryDoc = appPrimaryDoc, quarto = NULL),
    error = TRUE
  )
})

test_that("Deploying R Markdown content with Quarto gives a Quarto app mode", {
  quarto <- quartoPathOrSkip()

  manifest <- makeManifest(test_path("test-rmds"), "simple.Rmd", quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
})

test_that("Deploying static content with _quarto.yaml succeeds without quartoInfo", {

  manifest <- makeManifest(test_path("static-with-quarto-yaml"), NULL, quarto = NULL)

  expect_equal(manifest$metadata$appmode, "static")
})

test_that("Sets environment.image in the manifest if one is provided", {
  appDir <- test_path("shinyapp-simple")

  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, image = "rstudio/content-base:latest")
  expect_equal(manifest$environment$image, "rstudio/content-base:latest")

  manifest <- makeManifest(appDir, appPrimaryDoc = NULL)
  expect_null(manifest$environment)
})
