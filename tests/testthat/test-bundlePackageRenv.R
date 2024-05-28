# snapshotRenvDependencies() ----------------------------------------------

test_that("non-R apps don't have packages", {
  skip_on_cran()
  app_dir <- local_temp_app(list(index.html = ""))
  out <- snapshotRenvDependencies(app_dir)
  expect_equal(out, data.frame())
})

test_that("manifest has correct data types", {
  skip_on_cran()
  withr::local_options(renv.verbose = TRUE)
  app <- local_temp_app(list("index.Rmd" = ""))
  deps <- snapshotRenvDependencies(app)
  expect_type(deps$description, "list")
  expect_type(deps$description[[1]], "list")
})

test_that("recommended packages are snapshotted", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  withr::local_options(renv.verbose = TRUE)
  app <- local_temp_app(list("index.Rmd" = c(
    "```{r}",
    "library(MASS)",
    "```"
  )))
  deps <- snapshotRenvDependencies(app)
  expect_true("MASS" %in% deps$Package)
})

test_that("extra packages are snapshotted", {
  skip_on_cran()
  skip_if_not_installed("foreign")

  withr::local_options(renv.verbose = TRUE)
  app <- local_temp_app(list("index.Rmd" = ""))
  deps <- snapshotRenvDependencies(app, extraPackages = c("foreign"))
  expect_true("foreign" %in% deps$Package)
})

test_that("works with BioC packages", {
  skip_on_cran()
  skip_on_ci()

  app <- local_temp_app(list("index.R" = c(
    "library(Biobase)"
  )))
  biocRepos <- BiocManager::repositories()
  withr::local_options(repos = biocRepos)
  expect_no_condition(
    { deps <- snapshotRenvDependencies(app) },
    class = "rsconnect_biocRepos"
  )
  Biobase <- deps[deps$Package == "Biobase", ]
  expect_equal(Biobase$Source, "Bioconductor")
  expect_equal(Biobase$Repository, biocRepos[["BioCsoft"]])

  withr::local_options(repos = c(
    CRAN = "https://cran.rstudio.com"
  ))
  expect_condition(
    deps <- snapshotRenvDependencies(app),
    class = "rsconnect_biocRepos"
  )

  Biobase <- deps[deps$Package == "Biobase", ]
  expect_equal(Biobase$Source, "Bioconductor")
  expect_equal(Biobase$Repository, biocRepos(".")[[1]])
})

# https://github.com/rstudio/rsconnect/issues/968
test_that("large directories are analyzed", {
  skip_on_cran()
  skip_if_not_installed("foreign")

  app_dir <- local_temp_app(list("foo.R" = "library(foreign)"))
  data_dir <- file.path(app_dir, "data")
  dir.create(data_dir)
  for (each in seq_len(1001)) {
    writeLines(character(0), file.path(data_dir, paste0(each, ".txt")))
  }
  expect_snapshot(
    deps <- snapshotRenvDependencies(app_dir)
  )
  expect_contains(deps$Package, "foreign")
})

# parseRenvDependencies ---------------------------------------------------

test_that("gets DESCRIPTION from renv & system libraries", {
  skip_if_not_installed("foreign")
  skip_if_not_installed("MASS")

  withr::local_options(renv.verbose = FALSE)

  app_dir <- local_temp_app(list("foo.R" = "library(foreign); library(MASS)"))
  renv::snapshot(app_dir, prompt = FALSE)

  deps <- parseRenvDependencies(app_dir)
  expect_setequal(deps$Package, c("foreign", "MASS", "renv"))

  expect_type(deps$description, "list")
  expect_type(deps$description[[which(deps$Package == "foreign")]], "list")
  expect_type(deps$description[[which(deps$Package == "MASS")]], "list")
})


test_that("errors if library and project are inconsistent", {
  skip_if_not_installed("foreign")
  skip_if_not_installed("MASS")

  withr::local_options(renv.verbose = FALSE)

  app_dir <- local_temp_app(list("foo.R" = "library(foreign); library(MASS)"))
  renv::snapshot(app_dir, prompt = FALSE)
  renv::record("MASS@0.1.1", project = app_dir)

  expect_snapshot(parseRenvDependencies(app_dir), error = TRUE)
})

# standardizeRenvPackage -----------------------------------------

test_that("SCM get names translated", {
  bitbucket <- list(Package = "pkg", Source = "Bitbucket")
  gitlab <- list(Package = "pkg", Source = "GitLab")
  github <- list(Package = "pkg", Source = "GitHub")

  expect_equal(
    standardizeRenvPackage(bitbucket),
    list(Package = "pkg", Source = "bitbucket")
  )
  expect_equal(
    standardizeRenvPackage(gitlab),
    list(Package = "pkg", Source = "gitlab")
  )
  expect_equal(
    standardizeRenvPackage(github),
    list(Package = "pkg", Source = "github")
  )
})

test_that("BioC gets normalized repo", {
  Bioconductor <- list(Package = "pkg", Source = "Bioconductor")

  packages <- data.frame(
    Package = "pkg",
    Repository = "https://b.com/src/contrib",
    stringsAsFactors = FALSE
  )

  expect_equal(
    standardizeRenvPackage(Bioconductor, packages),
    list(Package = "pkg", Source = "Bioconductor", Repository = "https://b.com")
  )
})

test_that("has special handling for CRAN packages", {
  packages <- as.matrix(data.frame(
    Package = "pkg",
    Version = "1.0.0",
    Repository = "https://cran.com/src/contrib",
    stringsAsFactors = FALSE
  ))
  repos <- c(CRAN = "https://cran.com")

  spec <- function(version, source = "Repository", repo = "CRAN") {
    list(Package = "pkg", Version = version, Source = source, Repository = repo)
  }

  expect_equal(
    standardizeRenvPackage(spec("1.0.0"), packages, repos),
    spec("1.0.0", "CRAN", "https://cran.com")
  )

  expect_equal(
    standardizeRenvPackage(spec("1.0.0.9000"), packages, repos),
    spec("1.0.0.9000", NA_character_, NA_character_)
  )
})

test_that("packages installed from other repos get correctly named", {
  pkg <- list(Package = "pkg", Source = "Repository", Repository = "https://test2.com")
  packages <- as.matrix(data.frame(
    Package = "pkg",
    Version = "1.0.0",
    Repository = "https://test2.com/src/contrib",
    stringsAsFactors = FALSE
  ))
  repos <- c(TEST1 = "https://test1.com", TEST2 = "https://test2.com")

  expect_equal(
    standardizeRenvPackage(pkg, packages, repos = repos),
    list(Package = "pkg", Source = "TEST2", Repository = "https://test2.com")
  )
})

test_that("source packages get NA source + repository", {
  source <- list(Package = "pkg", Source = "unknown", Repository = "useless")
  expect_equal(
    standardizeRenvPackage(source),
    list(Package = "pkg", Source = NA_character_, Repository = NA_character_)
  )
})

test_that("Local packages get NA source + repository", {
  source <- list(Package = "pkg", Source = "Local", Repository = "useless")
  expect_equal(
    standardizeRenvPackage(source),
    list(Package = "pkg", Source = NA_character_, Repository = NA_character_)
  )
})
