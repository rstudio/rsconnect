context("bundle")

makeShinyBundleTempDir <- function(appName, appDir, appPrimaryDoc, python = NULL) {
  tarfile <- bundleApp(appName, appDir, bundleFiles(appDir), appPrimaryDoc,
                       "application", NULL, python = python)
  bundleTempDir <- tempfile()
  utils::untar(tarfile, exdir = bundleTempDir)
  unlink(tarfile)
  bundleTempDir
}

makeManifest <- function(appDir, appPrimaryDoc, python = NULL, quarto = NULL, image = NULL) {
  writeManifest(appDir, NULL, appPrimaryDoc, NULL, python = python, quarto = quarto, image = image)
  manifestFile <-file.path(appDir, "manifest.json")
  data <- readLines(manifestFile, warn = FALSE, encoding = "UTF-8")
  manifestJson <- jsonlite::fromJSON(data)
  unlink(manifestFile)
  manifestJson
}

fakeQuartoMetadata <- function(version, engines) {
  # See quarto-r/R/publish.R lines 396 and 113.
  metadata <- list()
  metadata$quarto_version <- version
  metadata$quarto_engines <- I(engines)
  return(metadata)
}

quarto_path <- function() {
  path_env <- Sys.getenv("QUARTO_PATH", unset = NA)
  if (!is.na(path_env)) {
    return(path_env)
  } else {
    locations <- c(
      "quarto", # Use PATH
      "/usr/local/bin/quarto", # Location used by some installers
      "/opt/quarto/bin/quarto", # Location used by some installers
      "/Applications/RStudio.app/Contents/MacOS/quarto/bin/quarto" # macOS IDE
    )
    for (location in locations) {
      path <- unname(Sys.which(location))
      if (nzchar(path)) return(path)
    }
    return(NULL)
  }
}

quartoPathOrSkip <- function() {
  skip_on_cran()
  quarto <- quarto_path()
  skip_if(is.null(quarto), "quarto cli is not installed")
  return(quarto)
}

# avoid 'trying to use CRAN without setting a mirror' errors
repos <- getOption("repos")
options(repos = c(CRAN = "https://cran.rstudio.com"))
on.exit(options(repos = repos), add = TRUE)

test_that("simple Shiny app bundle includes correct files", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("simple_shiny", "shinyapp-simple",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  # listBundleFiles only includes "user" files and ignores
  # generated files like the packrat and manifest data.
  files <- listBundleFiles(bundleTempDir)
  expect_identical(files$contents, c("server.R", "ui.R"))
  expect_identical(list.files(bundleTempDir), c("manifest.json", "packrat", "server.R", "ui.R"))
})

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

test_that("simple Shiny app bundle is runnable", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  bundleTempDir <- makeShinyBundleTempDir("simple_shiny", "shinyapp-simple",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("app.R Shiny app bundle is runnable", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  bundleTempDir <- makeShinyBundleTempDir("app_r_shiny", "shinyapp-appR",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("single-file Shiny app bundle is runnable", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  bundleTempDir <- makeShinyBundleTempDir("app_r_shiny", "shinyapp-singleR",
                                          "single.R")
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("recommended packages are snapshotted", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("MASS", "project-MASS", "MASS.R")
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_true("MASS" %in% names(deps))
})

test_that("simple Rmd as primary not identified as parameterized when parameterized Rmd in bundle", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("rmd primary", "test-rmds",
                                          "simple.Rmd")
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$metadata$has_parameters, FALSE)
})

test_that("parameterized Rmd identified as parameterized when other Rmd in bundle", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("rmd primary", "test-rmds",
                                          "parameterized.Rmd")
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "parameterized.Rmd")
  expect_equal(manifest$metadata$has_parameters, TRUE)
})

test_that("primary doc can be inferred (and non-parameterized dispite an included parameterized", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("rmd primary", "test-rmds",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "index.Rmd")
  expect_equal(manifest$metadata$has_parameters, FALSE)
})

test_that("multiple shiny Rmd without index file have a generated one", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir("rmd primary", "shiny-rmds",
                                          NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-shiny")
  expect_true(file.exists(file.path(bundleTempDir, "index.htm")))
})

test_that("Rmd with reticulate as a dependency includes python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")
  pipMissing <- system2(python, "-m pip help", stdout = NULL, stderr = NULL)
  skip_if(pipMissing != 0, "pip module is not installed")

  bundleTempDir <- makeShinyBundleTempDir("reticulated rmd", "test-reticulate-rmds",
                                          NULL, python = python)
  on.exit(unlink(bundleTempDir, recursive = TRUE))

  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_true("reticulate" %in% names(deps))

  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "index.Rmd")
  expect_true(file.exists(file.path(bundleTempDir, manifest$python$package_manager$package_file)))
})

test_that("Rmd with reticulate as an inferred dependency includes reticulate and python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")
  pipMissing <- system2(python, "-m pip help", stdout = NULL, stderr = NULL)
  skip_if(pipMissing != 0, "pip module is not installed")

  bundleTempDir <- makeShinyBundleTempDir("reticulated rmd", "test-reticulate-rmds",
                                          "implicit.Rmd", python = python)
  on.exit(unlink(bundleTempDir, recursive = TRUE))

  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_true("reticulate" %in% names(deps))

  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "implicit.Rmd")
  expect_true(file.exists(file.path(bundleTempDir, manifest$python$package_manager$package_file)))
})

test_that("Rmd without a python block doesn't include reticulate or python in the manifest", {
  skip_on_cran()

  bundleTempDir <- makeShinyBundleTempDir("plain rmd", "test-rmds",
                                          "simple.Rmd", python = NULL)
  on.exit(unlink(bundleTempDir, recursive = TRUE))

  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_false("reticulate" %in% names(deps))

  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$python, NULL)
})

test_that("Rmd without a python block doesn't include reticulate or python in the manifest even if python specified", {
  skip_on_cran()
  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")

  bundleTempDir <- makeShinyBundleTempDir("plain rmd", "test-rmds",
                                          "simple.Rmd", python = python)
  on.exit(unlink(bundleTempDir, recursive = TRUE))

  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_false("reticulate" %in% names(deps))

  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$python, NULL)
})

test_that("writeManifest: Rmd with reticulate as a dependency includes python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")
  pipMissing <- system2(python, "-m pip help", stdout = NULL, stderr = NULL)
  skip_if(pipMissing != 0, "pip module is not installed")

  appDir <- "test-reticulate-rmds"
  manifest <- makeManifest(appDir, NULL, python = python)
  requirements_file <- file.path(appDir, manifest$python$package_manager$package_file)
  expect_equal(requirements_file, "test-reticulate-rmds/requirements.txt")
  on.exit(unlink(requirements_file))

  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "index.Rmd")
  expect_true(file.exists(requirements_file))
})

test_that("writeManifest: Rmd with reticulate as an inferred dependency includes reticulate and python in the manifest", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")
  pipMissing <- system2(python, "-m pip help", stdout = NULL, stderr = NULL)
  skip_if(pipMissing != 0, "pip module is not installed")

  appDir <- "test-reticulate-rmds"
  manifest <- makeManifest(appDir, "implicit.Rmd", python = python)
  requirements_file <- file.path(appDir, manifest$python$package_manager$package_file)
  expect_equal(requirements_file, "test-reticulate-rmds/requirements.txt")
  on.exit(unlink(requirements_file))

  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "implicit.Rmd")
  expect_true(file.exists(requirements_file))
})

test_that("writeManifest: Rmd without a python block doesn't include reticulate or python in the manifest", {
  skip_on_cran()

  manifest <- makeManifest("test-rmds", "simple.Rmd", python = NULL)
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_null(manifest$python)
})

test_that("writeManifest: Rmd without a python block doesn't include reticulate or python in the manifest even if python specified", {
  skip_on_cran()
  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")

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

test_that("getPython handles null python by checking RETICULATE_PYTHON", {
  skip_on_cran()

  Sys.setenv(RETICULATE_PYTHON="/usr/local/bin/python")
  expect_equal(getPython(NULL), "/usr/local/bin/python")
  Sys.unsetenv("RETICULATE_PYTHON")
})

test_that("getPython handles null python and empty RETICULATE_PYTHON by checking RETICULATE_PYTHON_FALLBACK", {
  skip_on_cran()

  Sys.unsetenv("RETICULATE_PYTHON")
  Sys.setenv(RETICULATE_PYTHON_FALLBACK="/usr/local/bin/python")
  expect_equal(getPython(NULL), "/usr/local/bin/python")
})

test_that("getPython handles null python, empty RETICULATE_PYTHON, and empty RETICULATE_PYTHON_FALLBACK", {
  skip_on_cran()

  Sys.unsetenv("RETICULATE_PYTHON")
  Sys.unsetenv("RETICULATE_PYTHON_FALLBACK")
  expect_equal(getPython(NULL), NULL)
})

test_that("getPython expands paths", {
  skip_on_cran()

  result <- getPython("~/bin/python")
  expect_true(result != "~/bin/python")
  expect_match(result, "*/bin/python")
})

test_that("getPythonForTarget honors rsconnect.python.enabled = FALSE", {
  skip_on_cran()

  options(rsconnect.python.enabled = FALSE)
  result <- getPythonForTarget("/usr/bin/python", list(server="shinyapps.io"))
  expect_equal(result, NULL)
  options(rsconnect.python.enabled = NULL)
})

test_that("getPythonForTarget honors rsconnect.python.enabled = TRUE", {
  skip_on_cran()

  options(rsconnect.python.enabled = TRUE)
  result <- getPythonForTarget("/usr/bin/python", list(server="shinyapps.io"))
  expect_equal(result, "/usr/bin/python")
  options(rsconnect.python.enabled = NULL)
})

test_that("getPythonForTarget defaults to enabled for Connect", {
  skip_on_cran()

  result <- getPythonForTarget("/usr/bin/python", list(server="connect.example.com"))
  expect_equal(result, "/usr/bin/python")
})

test_that("getPythonForTarget defaults to disabled for shinyapps.io", {
  skip_on_cran()

  result <- getPythonForTarget("/usr/bin/python", list(server="shinyapps.io"))
  expect_equal(result, NULL)
})

test_that("getPythonForTarget defaults to enabled for rstudio.cloud", {
  skip_on_cran()

  result <- getPythonForTarget("/usr/bin/python", list(server="rstudio.cloud"))
  expect_equal(result, "/usr/bin/python")
})

# Quarto Tests

test_that("quartoInspect identifies on Quarto projects", {
  quarto <- quartoPathOrSkip()

  inspect <- quartoInspect(appDir = "quarto-website-r", quarto = quarto)
  expect_true(all(c("quarto", "engines") %in% names(inspect)))

  inspect <- NULL
  inspect <- quartoInspect(appDir = "quarto-proj-r-shiny", quarto = quarto)
  expect_true(all(c("quarto", "engines") %in% names(inspect)))
})

test_that("quartoInspect identifies Quarto documents", {
  quarto <- quartoPathOrSkip()

  inspect <- quartoInspect(
    appDir = "quarto-doc-none",
    appPrimaryDoc = "quarto-doc-none.qmd",
    quarto = quarto
  )
  expect_true(all(c("quarto", "engines") %in% names(inspect)))
})

test_that("quartoInspect returns NULL on non-quarto Quarto content", {
  quarto <- quartoPathOrSkip()

  inspect <- quartoInspect(appDir = "shinyapp-simple", quarto = quarto)
  expect_null(inspect)
})

test_that("quartoInspect returns null when no quarto is provided", {
  quarto <- quartoPathOrSkip()

  expect_null(quartoInspect(appDir = "quarto-website-r", quarto = NULL))
})

test_that("inferQuartoInfo correctly detects info when quarto is provided alone", {
  quarto <- quartoPathOrSkip()

  quartoInfo <- inferQuartoInfo(
    appDir = "quarto-doc-none",
    appPrimaryDoc = "quarto-doc-none.qmd",
    appFiles = bundleFiles("quarto-doc-none"),
    quarto = quarto,
    metadata = list()
  )
  expect_named(quartoInfo, c("version", "engines"))
  expect_equal(quartoInfo$engines, I(c("markdown")))

  quartoInfo <- inferQuartoInfo(
    appDir = "quarto-website-r",
    appPrimaryDoc = NULL,
    appFiles = bundleFiles("quarto-website-r"),
    quarto = quarto,
    metadata = list()
  )
  expect_named(quartoInfo, c("version", "engines"))
  expect_equal(quartoInfo$engines, I(c("knitr")))
})

test_that("inferQuartoInfo extracts info from metadata when it is provided alone", {
  quarto <- quartoPathOrSkip()

  metadata <- fakeQuartoMetadata(version = "99.9.9", engines = c("internal-combustion"))

  quartoInfo <- inferQuartoInfo(
    appDir = "quarto-website-r",
    appPrimaryDoc = NULL,
    appFiles = bundleFiles("quarto-website-r"),
    quarto = NULL,
    metadata = metadata
  )
  expect_named(quartoInfo, c("version", "engines"))
  expect_equal(quartoInfo$engines, I(c("internal-combustion")))
})

test_that("inferQuartoInfo prefers using metadata over running quarto inspect itself when both are provided", {
  quarto <- quartoPathOrSkip()

  metadata <- fakeQuartoMetadata(version = "99.9.9", engines = c("internal-combustion"))

  quartoInfo <- inferQuartoInfo(
    appDir = "quarto-website-r",
    appPrimaryDoc = NULL,
    appFiles = bundleFiles("quarto-website-r"),
    quarto = quarto,
    metadata = metadata
  )
  expect_equal(quartoInfo$engines, I(c("internal-combustion")))
})

test_that("inferQuartoInfo returns NULL when content cannot be rendered by Quarto", {
  quarto <- quartoPathOrSkip()

  quartoInfo <- inferQuartoInfo(
    appDir = "shinyapp-simple",
    appPrimaryDoc = NULL,
    appFiles = bundleFiles("shinyapp-simple"),
    quarto = quarto,
    metadata = list()
  )
  expect_null(quartoInfo)
})

test_that("writeManifest: Quarto website includes quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- "quarto-website-r"
  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "index.qmd")
})

test_that("writeManifest: Quarto document includes quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- "quarto-doc-none"
  appPrimaryDoc = "quarto-doc-none.qmd"
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "markdown")
  expect_equal(manifest$metadata$primary_rmd, "quarto-doc-none.qmd")
})

test_that("writeManifest: Specifying quarto arg includes quarto in the manifest, even with no appPrimaryDoc specified (.qmd)", {
  quarto <- quartoPathOrSkip()

  appDir <- "quarto-doc-none"
  appPrimaryDoc = NULL
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "markdown")
  expect_equal(manifest$metadata$primary_rmd, "quarto-doc-none.qmd")
})

test_that("writeManifest: Specifying quarto arg includes quarto in the manifest, even with no appPrimaryDoc specified (.Rmd)", {
  quarto <- quartoPathOrSkip()

  appDir <- "shiny-rmds"
  appPrimaryDoc = NULL
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-shiny")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "non-shiny-rmd.Rmd")
})

test_that("writeManifest: specifying quarto arg with non-quarto app does not include quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- "shinyapp-singleR"
  appPrimaryDoc = "single.R"
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_null(manifest$quarto)
})

test_that("writeManifest: Quarto shiny project includes quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- "quarto-proj-r-shiny"
  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-shiny")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "quarto-proj-r-shiny.qmd")
})

test_that("writeManifest: Quarto R + Python website includes quarto and python in the manifest", {
  quarto <- quartoPathOrSkip()
  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")

  appDir <- "quarto-website-r-py"
  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, python = python, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "index.qmd")

  expect_true(all(c("quarto", "python") %in% names(manifest)))
  expect_true("reticulate" %in% names(manifest$packages))
})

test_that("writeManifest: Quarto Python-only website gets correct manifest data", {
  quarto <- quartoPathOrSkip()
  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")

  appDir <- "quarto-website-py"
  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, python = python, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "jupyter")
  expect_equal(manifest$metadata$primary_rmd, "index.qmd")

  # We expect Quarto and Python metadata, but no R packages.
  expect_true(all(c("quarto", "python") %in% names(manifest)))
  expect_null(manifest$packages)
})

test_that("writeManifest: Deploying a Quarto project without Quarto info in an error", {
  missingQuartoInfoErrorText <- paste(
    "Attempting to deploy Quarto content without Quarto metadata.",
    "Please provide the path to a quarto binary to the 'quarto' argument."
  )

  appDir <- "quarto-website-r"
  expect_error(
    makeManifest(appDir, appPrimaryDoc = NULL, quarto = NULL),
    missingQuartoInfoErrorText
  )
})

test_that("writeManifest: Deploying a Quarto doc without Quarto info in an error", {
  missingQuartoInfoErrorText <- paste(
    "Attempting to deploy Quarto content without Quarto metadata.",
    "Please provide the path to a quarto binary to the 'quarto' argument."
  )

  appDir <- "quarto-doc-none"
  appPrimaryDoc <- "quarto-doc-none.qmd"
  expect_error(
    makeManifest(appDir, appPrimaryDoc = appPrimaryDoc, quarto = NULL),
    missingQuartoInfoErrorText
  )
})

test_that("writeManifest: Deploying R Markdown content with Quarto gives a Quarto app mode", {
  quarto <- quartoPathOrSkip()

  manifest <- makeManifest("test-rmds", "simple.Rmd", quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
})

test_that("writeManifest: Deploying static content with _quarto.yaml succeeds without quartoInfo", {

  manifest <- makeManifest("static-with-quarto-yaml", NULL, quarto = NULL)

  expect_equal(manifest$metadata$appmode, "static")
})

test_that("writeManifest: Sets environment.image in the manifest if one is provided", {
  appDir <- "shinyapp-simple"

  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, image = "rstudio/content-base:latest")
  expect_equal(manifest$environment$image, "rstudio/content-base:latest")

  manifest <- makeManifest(appDir, appPrimaryDoc = NULL)
  expect_null(manifest$environment)
})

test_that("tarImplementation: checks environment variable and option before using default", {
  option_value <- getOption("rsconnect.tar")
  envvar_value <- Sys.getenv("RSCONNECT_TAR", unset = NA)
  on.exit({
    options("rsconnect.tar" = option_value)
    Sys.setenv("RSCONNECT_TAR" = envvar_value)
  }, add = TRUE, after = FALSE)

  # Environment variable only set should use environment varaible
  Sys.setenv("RSCONNECT_TAR" = "envvar")
  options("rsconnect.tar" = NULL)
  tarImplementation <- getTarImplementation()
  expect_equal(tarImplementation, "envvar")

  # Option only set should use option
  Sys.unsetenv("RSCONNECT_TAR")
  options("rsconnect.tar" = "option")
  tarImplementation <- getTarImplementation()
  expect_equal(tarImplementation, "option")

  # Both environment variable and option set should use option
  Sys.setenv("RSCONNECT_TAR" = "envvar")
  options("rsconnect.tar" = "option")
  tarImplementation <- getTarImplementation()
  expect_equal(tarImplementation, "option")

  # Neither set should use "internal"
  Sys.unsetenv("RSCONNECT_TAR")
  options("rsconnect.tar" = NULL)
  tarImplementation <- getTarImplementation()
  expect_equal(tarImplementation, "internal")
})
