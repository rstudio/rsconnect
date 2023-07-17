# snapshotRenvDependencies() ----------------------------------------------

test_that("non-R apps don't have packages", {
  app_dir <- local_temp_app(list(index.html = ""))
  out <- snapshotRenvDependencies(app_dir)
  expect_equal(out, data.frame())
})

test_that("manifest has correct data types", {
  withr::local_options(renv.verbose = TRUE)
  app <- local_temp_app(list("index.Rmd" = ""))
  deps <- snapshotRenvDependencies(app)
  expect_type(deps$description, "list")
  expect_type(deps$description[[1]], "list")
})

test_that("recommended packages are snapshotted", {
  withr::local_options(renv.verbose = TRUE)
  app <- local_temp_app(list("index.Rmd" = c(
    "```{r}",
    "library(MASS)",
    "```"
  )))
  deps <- snapshotRenvDependencies(app)
  expect_true("MASS" %in% deps$Package)
})

test_that("works with BioC packages", {
  skip_on_cran()
  skip_on_ci()
  app <- local_temp_app(list("index.R" = c(
    "library(Biobase)"
  )))
  withr::local_options(repos = c(
    CRAN = "https://cran.rstudio.com",
    BioC = "https://bioconductor.org/packages/3.16/bioc"
  ))
  expect_no_condition(
    deps <- snapshotRenvDependencies(app),
    class = "rsconnect_biocRepos"
  )
  Biobase <- deps[deps$Package == "Biobase", ]
  expect_equal(Biobase$Source, "Bioconductor")
  expect_equal(Biobase$Repository, "https://bioconductor.org/packages/3.16/bioc")

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

# parseRenvDependencies ---------------------------------------------------

test_that("gets DESCRIPTION from renv library", {
  withr::local_options(renv.verbose = FALSE)

  app_dir <- local_temp_app(list("foo.R" = "library(withr); library(foreign)"))
  renv::snapshot(app_dir, prompt = FALSE)

  deps <- parseRenvDependencies(app_dir)
  expect_setequal(deps$Package, c("foreign", "withr"))
  expect_type(deps$description, "list")
  expect_type(deps$description[[1]], "list")
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

test_that("source packages get NA source", {
  source <- list(Package = "pkg", Source = "unknown")
  expect_equal(
    standardizeRenvPackage(source),
    list(Package = "pkg", Source = NA_character_)
  )
})
