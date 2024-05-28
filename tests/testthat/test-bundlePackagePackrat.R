test_that("non-R apps don't have packages", {
  app_dir <- local_temp_app(list(index.html = ""))
  out <- snapshotPackratDependencies(app_dir)
  expect_equal(out, data.frame())
})

test_that("manifest has correct data types", {
  app <- local_temp_app(list("index.Rmd" = ""))

  deps <- snapshotPackratDependencies(app)
  expect_type(deps$description, "list")
  expect_type(deps$description[[1]], "list")
})

test_that("uninstalled packages error", {
  app <- local_temp_app(list("index.Rmd" = c(
    "```{r}",
    "library(doesntexist1)",
    "library(doesntexist2)",
    "```"
  )))
  expect_snapshot(
    snapshotPackratDependencies(app),
    error = TRUE,
    transform = function(x) gsub('"', "'", x, fixed = TRUE)
  )
})

test_that("recommended packages are snapshotted", {
  skip_if_not_installed("MASS")
  app <- local_temp_app(list("index.Rmd" = c(
    "```{r}",
    "library(MASS)",
    "```"
  )))
  deps <- snapshotPackratDependencies(app)
  expect_true("MASS" %in% deps$Package)
})

test_that("works with BioC packages", {
  skip_on_cran()
  skip_on_ci()
  app <- local_temp_app(list("index.Rmd" = c(
    "```{r}",
    "library(Biobase)",
    "```"
  )))
  withr::local_options(repos = c(
    CRAN = "https://cran.rstudio.com",
    BioC = "https://bioconductor.org/packages/release/bioc"
  ))

  deps <- snapshotPackratDependencies(app)

  Biobase <- deps[deps$Package == "Biobase", ]
  expect_equal(Biobase$Source, "Bioconductor")
  expect_equal(Biobase$Repository, "https://bioconductor.org/packages/release/bioc")

  BiocGenerics <- deps[deps$Package == "BiocGenerics", ]
  expect_equal(BiocGenerics$Source, "Bioconductor")
  expect_equal(BiocGenerics$Repository, "https://bioconductor.org/packages/release/bioc")
})

# addPackratSnapshot() ----------------------------------------------------

test_that("cleans up implicit dependency files", {
  dir <- withr::local_tempdir()
  addPackratSnapshot(dir, "rlang")
  expect_equal(list.files(dir), "packrat")
})

# standardizePackratPackage -----------------------------------------

test_that("standardizeRepos adds names and normalizes paths", {
  repos <- c(ONE = "https://cran1.com", "https://cran2.com/")
  expect_equal(
    standardizeRepos(repos),
    c(ONE = "https://cran1.com", repo_2 = "https://cran2.com")
  )
})

test_that("SCM records are left alone", {
  bitbucket <- list(Package = "pkg", Source = "bitbucket")
  gitlab <- list(Package = "pkg", Source = "gitlab")
  github <- list(Package = "pkg", Source = "github")

  expect_equal(
    standardizePackratPackage(bitbucket),
    list(Source = "bitbucket", Repository = NA_character_)
  )
  expect_equal(
    standardizePackratPackage(gitlab),
    list(Source = "gitlab", Repository = NA_character_)
  )
  expect_equal(
    standardizePackratPackage(github),
    list(Source = "github", Repository = NA_character_)
  )
})

test_that("CRAN & BioC get normalized repo", {
  CRAN <- list(Package = "pkg1", Source = "CRAN")
  Bioconductor <- list(Package = "pkg2", Source = "Bioconductor")

  packages <- data.frame(
    Package = c("pkg1", "pkg2", "pkg3"),
    Repository = paste0(
      c("https://a.com", "https://b.com", "https://cran.com"),
      "/src/contrib"
    ),
    stringsAsFactors = FALSE
  )

  expect_equal(
    standardizePackratPackage(CRAN, packages),
    list(Source = "CRAN", Repository = "https://a.com")
  )
  expect_equal(
    standardizePackratPackage(Bioconductor, packages),
    list(Source = "Bioconductor", Repository = "https://b.com")
  )
})

test_that("packages installed from other repos get correctly named", {
  pkg <- list(Package = "pkg", Source = "https://test2.com")
  packages <- as.matrix(data.frame(
    Package = "pkg",
    Version = "1.0.0",
    Repository = "https://test2.com/src/contrib",
    stringsAsFactors = FALSE
  ))
  repos <- c(TEST1 = "https://test1.com", TEST2 = "https://test2.com")

  expect_equal(
    standardizePackratPackage(pkg, packages, repos = repos),
    list(Source = "TEST2", Repository = "https://test2.com")
  )
})

test_that("source packages can't be installed", {
  source <- list(Package = "pkg1", Source = "source")
  expect_equal(
    standardizePackratPackage(source),
    list(Source = NA_character_, Repository = NA_character_)
  )
})

test_that("locally installed CRAN packages are handled correctly", {
  packages <- as.matrix(data.frame(
    Package = "pkg",
    Version = "1.0.0",
    Repository = "https://cran.com/src/contrib",
    stringsAsFactors = FALSE
  ))
  repos <- c(CRAN = "https://cran.com")

  local <- list(
    Package = "pkg",
    Source = "CustomCRANLikeRepository",
    Version = "1.0.0"
  )
  expect_equal(
    standardizePackratPackage(local, packages, repos),
    list(Source = "CRAN", Repository = "https://cran.com")
  )

  local_dev <- list(
    Package = "pkg",
    Source = "CustomCRANLikeRepository",
    Version = "1.0.0.9000"
  )
  expect_equal(
    standardizePackratPackage(local_dev, packages, repos),
    list(Source = NA_character_, Repository = NA_character_)
  )

  archived <- list(
    Package = "pkg2",
    Source = "CustomCRANLikeRepository",
    Version = "1.0.0"
  )
  expect_equal(
    standardizePackratPackage(archived, packages, repos),
    list(Source = NA_character_, Repository = NA_character_)
  )
})
