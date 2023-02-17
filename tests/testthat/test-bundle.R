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
  manifestFile <- file.path(appDir, "manifest.json")
  data <- readLines(manifestFile, warn = FALSE, encoding = "UTF-8")
  manifestJson <- jsonlite::fromJSON(data)
  unlink(manifestFile)
  manifestJson
}

# avoid 'trying to use CRAN without setting a mirror' errors
repos <- getOption("repos")
options(repos = c(CRAN = "https://cran.rstudio.com"))
on.exit(options(repos = repos), add = TRUE)

test_that("simple Shiny app bundle includes correct files", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir(
    "simple_shiny",
    test_path("shinyapp-simple"),
    NULL
  )
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
  bundleTempDir <- makeShinyBundleTempDir(
    "simple_shiny",
    test_path("shinyapp-simple"),
    NULL
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("app.R Shiny app bundle is runnable", {
  # shiny:::shinyAppDir() attach shiny so do it here so we can do it quietly
  library(shiny, warn.conflicts = FALSE, quietly = TRUE)

  skip_on_cran()
  skip_if_not_installed("shiny")
  bundleTempDir <- makeShinyBundleTempDir(
    "app_r_shiny",
    test_path("shinyapp-appR"),
    NULL
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("single-file Shiny app bundle is runnable", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  bundleTempDir <- makeShinyBundleTempDir(
    "app_r_shiny",
    test_path("shinyapp-singleR"),
    "single.R"
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  expect_true(inherits(shiny::shinyAppDir(bundleTempDir), "shiny.appobj"))
})

test_that("recommended packages are snapshotted", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir(
    "MASS",
    test_path("project-MASS"),
    "MASS.R"
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  lockfile <- file.path(bundleTempDir, "packrat/packrat.lock")
  deps <- packrat:::readLockFilePackages(lockfile)
  expect_true("MASS" %in% names(deps))
})

test_that("simple Rmd as primary not identified as parameterized when parameterized Rmd in bundle", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir(
    "rmd primary",
    test_path("test-rmds"),
    "simple.Rmd"
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
  expect_equal(manifest$metadata$has_parameters, FALSE)
})

test_that("parameterized Rmd identified as parameterized when other Rmd in bundle", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir(
    "rmd primary",
    test_path("test-rmds"),
    "parameterized.Rmd"
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "parameterized.Rmd")
  expect_equal(manifest$metadata$has_parameters, TRUE)
})

test_that("primary doc can be inferred (and non-parameterized dispite an included parameterized", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir(
    "rmd primary",
    test_path("test-rmds"),
    NULL
  )
  on.exit(unlink(bundleTempDir, recursive = TRUE))
  manifest <- jsonlite::fromJSON(file.path(bundleTempDir, "manifest.json"))
  expect_equal(manifest$metadata$appmode, "rmd-static")
  expect_equal(manifest$metadata$primary_rmd, "index.Rmd")
  expect_equal(manifest$metadata$has_parameters, FALSE)
})

test_that("multiple shiny Rmd without index file have a generated one", {
  skip_on_cran()
  bundleTempDir <- makeShinyBundleTempDir(
    "rmd primary",
    test_path("shiny-rmds"),
    NULL
  )
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

  bundleTempDir <- makeShinyBundleTempDir(
    "reticulated rmd",
    test_path("test-reticulate-rmds"),
    NULL,
    python = python
  )
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

  bundleTempDir <- makeShinyBundleTempDir(
    "reticulated rmd",
    test_path("test-reticulate-rmds"),
    "implicit.Rmd",
    python = python
  )
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

  bundleTempDir <- makeShinyBundleTempDir(
    "plain rmd",
    test_path("test-rmds"),
    "simple.Rmd",
    python = NULL
  )
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

  bundleTempDir <- makeShinyBundleTempDir(
    "plain rmd",
    test_path("test-rmds"),
    "simple.Rmd",
    python = python
  )
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

  appDir <- test_path("test-reticulate-rmds")
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

  appDir <- test_path("test-reticulate-rmds")
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

  manifest <- makeManifest(test_path("test-rmds"), "simple.Rmd", python = NULL)
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

  Sys.setenv(RETICULATE_PYTHON = "/usr/local/bin/python")
  expect_equal(getPython(NULL), "/usr/local/bin/python")
  Sys.unsetenv("RETICULATE_PYTHON")
})

test_that("getPython handles null python and empty RETICULATE_PYTHON by checking RETICULATE_PYTHON_FALLBACK", {
  skip_on_cran()

  Sys.unsetenv("RETICULATE_PYTHON")
  Sys.setenv(RETICULATE_PYTHON_FALLBACK = "/usr/local/bin/python")
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
  result <- getPythonForTarget("/usr/bin/python", list(server = "shinyapps.io"))
  expect_equal(result, NULL)
  options(rsconnect.python.enabled = NULL)
})

test_that("getPythonForTarget honors rsconnect.python.enabled = TRUE", {
  skip_on_cran()

  options(rsconnect.python.enabled = TRUE)
  result <- getPythonForTarget("/usr/bin/python", list(server = "shinyapps.io"))
  expect_equal(result, "/usr/bin/python")
  options(rsconnect.python.enabled = NULL)
})

test_that("getPythonForTarget defaults to enabled for Connect", {
  skip_on_cran()

  result <- getPythonForTarget("/usr/bin/python", list(server = "connect.example.com"))
  expect_equal(result, "/usr/bin/python")
})

test_that("getPythonForTarget defaults to disabled for shinyapps.io", {
  skip_on_cran()

  result <- getPythonForTarget("/usr/bin/python", list(server = "shinyapps.io"))
  expect_equal(result, NULL)
})

test_that("getPythonForTarget defaults to enabled for rstudio.cloud", {
  skip_on_cran()

  result <- getPythonForTarget("/usr/bin/python", list(server = "rstudio.cloud"))
  expect_equal(result, "/usr/bin/python")
})

test_that("writeManifest: Quarto website includes quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("quarto-website-r")
  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "index.qmd")
})

test_that("writeManifest: Quarto document includes quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("quarto-doc-none")
  appPrimaryDoc <- "quarto-doc-none.qmd"
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "markdown")
  expect_equal(manifest$metadata$primary_rmd, "quarto-doc-none.qmd")
})

test_that("writeManifest: Specifying quarto arg includes quarto in the manifest, even with no appPrimaryDoc specified (.qmd)", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("quarto-doc-none")
  appPrimaryDoc <- NULL
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "markdown")
  expect_equal(manifest$metadata$primary_rmd, "quarto-doc-none.qmd")
})

test_that("writeManifest: Specifying quarto arg includes quarto in the manifest, even with no appPrimaryDoc specified (.Rmd)", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("shiny-rmds")
  appPrimaryDoc <- NULL
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-shiny")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "non-shiny-rmd.Rmd")
})

test_that("writeManifest: specifying quarto arg with non-quarto app does not include quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("shinyapp-singleR")
  appPrimaryDoc <- "single.R"
  manifest <- makeManifest(appDir, appPrimaryDoc, quarto = quarto)

  expect_null(manifest$quarto)
})

test_that("writeManifest: Quarto shiny project includes quarto in the manifest", {
  quarto <- quartoPathOrSkip()

  appDir <- test_path("quarto-proj-r-shiny")
  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-shiny")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "quarto-proj-r-shiny.qmd")
})

test_that("writeManifest: Quarto R + Python website includes quarto and python in the manifest", {
  quarto <- quartoPathOrSkip()
  python <- Sys.which("python")
  skip_if(python == "", "python is not installed")

  appDir <- test_path("quarto-website-r-py")
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

  appDir <- test_path("quarto-website-py")
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

  appDir <- test_path("quarto-website-r")
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

  appDir <- test_path("quarto-doc-none")
  appPrimaryDoc <- "quarto-doc-none.qmd"
  expect_error(
    makeManifest(appDir, appPrimaryDoc = appPrimaryDoc, quarto = NULL),
    missingQuartoInfoErrorText
  )
})

test_that("writeManifest: Deploying R Markdown content with Quarto gives a Quarto app mode", {
  quarto <- quartoPathOrSkip()

  manifest <- makeManifest(test_path("test-rmds"), "simple.Rmd", quarto = quarto)

  expect_equal(manifest$metadata$appmode, "quarto-static")
  expect_equal(manifest$quarto$engines, "knitr")
  expect_equal(manifest$metadata$primary_rmd, "simple.Rmd")
})

test_that("writeManifest: Deploying static content with _quarto.yaml succeeds without quartoInfo", {

  manifest <- makeManifest(test_path("static-with-quarto-yaml"), NULL, quarto = NULL)

  expect_equal(manifest$metadata$appmode, "static")
})

test_that("writeManifest: Sets environment.image in the manifest if one is provided", {
  appDir <- test_path("shinyapp-simple")

  manifest <- makeManifest(appDir, appPrimaryDoc = NULL, image = "rstudio/content-base:latest")
  expect_equal(manifest$environment$image, "rstudio/content-base:latest")

  manifest <- makeManifest(appDir, appPrimaryDoc = NULL)
  expect_null(manifest$environment)
})

test_that("tarImplementation: checks environment variable and option before using default", {
  tar_implementation <- function(option, envvar) {
    withr::local_options(rsconnect.tar = option)
    withr::local_envvar(RSCONNECT_TAR = envvar)
    getTarImplementation()
  }

  # Environment variable only set should use environment varaible
  expect_equal(tar_implementation(NULL, "envvar"), "envvar")

  # Option only set should use option
  expect_equal(tar_implementation("option", NA), "option")

  # Both environment variable and option set should use option
  expect_equal(tar_implementation("option", "envvar"), "option")

  # Neither set should use "internal"
  expect_equal(tar_implementation(NULL, NA), "internal")
})
