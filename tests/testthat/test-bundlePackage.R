test_that("non-R apps don't have packages", {
  app_dir <- withr::local_tempdir()
  out <- bundlePackages(app_dir, appMode = "static", quiet = TRUE)
  expect_equal(out, list())
})

test_that("returns list of package details and copies descriptions", {
  app_dir <- local_temp_app(list("foo.Rmd" = ""))
  out <- bundlePackages(app_dir, quiet = TRUE)
  expect_type(out, "list")

  common <- c("Source", "Repository", "description")
  expect_equal(setdiff(common, names(out[[1]])), character())

  expect_setequal(
    names(out),
    list.files(file.path(app_dir, "packrat", "desc"))
  )
})

test_that("recommended packages are snapshotted", {
  app_dir <- withr::local_tempdir()
  writeLines(con = file.path(app_dir, "index.Rmd"), c(
    "```{r}",
    "library(MASS)",
    "```"
  ))
  out <- bundlePackages(app_dir, quiet = TRUE)
  expect_true("MASS" %in% names(out))
})

test_that("works with BioC packages", {
  app_dir <- local_temp_app(list("index.Rmd" = c(
    "```{r}",
    "library(Biobase)",
    "```"
  )))
  withr::local_options(repos = c(
    CRAN = "https://cran.rstudio.com",
    BioC = "https://bioconductor.org/packages/3.16/bioc"
  ))

  out <- bundlePackages(app_dir, quiet = TRUE)
  expect_equal(out$Biobase$Source, "Bioconductor")
  expect_equal(out$Biobase$Repository, "https://bioconductor.org/packages/3.16/bioc")
  expect_equal(out$BiocGenerics$Source, "Bioconductor")
  expect_equal(out$BiocGenerics$Repository, "https://bioconductor.org/packages/3.16/bioc")
})

test_that("errors if dependencies aren't installed", {
  mockr::local_mock(snapshotRDependencies = function(...) {
    data.frame(
      Package = c("doesntexist1", "doesntexist2"),
      Source = "CRAN",
      Repository = "https://cran.rstudio.com",
      stringsAsFactors = FALSE
    )
  })

  app_dir <- withr::local_tempdir()
  writeLines(con = file.path(app_dir, "index.Rmd"), c(
    "```{r}",
    "library(doesntexist1)",
    "library(doesntexist2)",
    "```"
  ))

  expect_snapshot(
    bundlePackages(app_dir, appMode = "rmd-static"),
    error = TRUE
  )
})

test_that("warns if can't find source", {
  mockr::local_mock(snapshotRDependencies = function(...) {
    data.frame(
      Package = "shiny",
      Source = NA,
      Repository = NA,
      stringsAsFactors = FALSE
    )
  })

  app_dir <- withr::local_tempdir()
  writeLines(con = file.path(app_dir, "index.Rmd"), c(
    "```{r}",
    "library(shiny)",
    "```"
  ))

  expect_snapshot(
    . <- bundlePackages(app_dir, appMode = "rmd-static"),
    error = TRUE
  )
})

# addPackratSnapshot() ----------------------------------------------------

test_that("clear error if can't run performPackratSnapshot()", {
  dir <- withr::local_tempdir()

  expect_snapshot(
    addPackratSnapshot(dir, "doesntexist"),
    error = TRUE,
    transform = function(x) gsub('"', "'", x, fixed = TRUE)
  )
})

test_that("cleans up implicit dependency files", {
  dir <- withr::local_tempdir()
  addPackratSnapshot(dir, "rlang")
  expect_equal(list.files(dir), "packrat")
})


# standardizePackageSource -----------------------------------------

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
    standardizePackageSource(bitbucket),
    list(Source = "bitbucket", Repository = NA_character_)
  )
  expect_equal(
    standardizePackageSource(gitlab),
    list(Source = "gitlab", Repository = NA_character_)
  )
  expect_equal(
    standardizePackageSource(github),
    list(Source = "github", Repository = NA_character_)
  )
})

test_that("CRAN & BioC get normalized repo", {
  CRAN <- list(Package = "pkg1", Source = "CRAN")
  Bioconductor <- list(Package = "pkg2", Source = "Bioconductor")

  packages <- data.frame(
    row.names = c("pkg1", "pkg2", "pkg3"),
    Repository = paste0(
      c("https://a.com", "https://b.com", "https://cran.com"),
      "/src/contrib"
    ),
    stringsAsFactors = FALSE
  )

  expect_equal(
    standardizePackageSource(CRAN, packages),
    list(Source = "CRAN", Repository = "https://a.com")
  )
  expect_equal(
    standardizePackageSource(Bioconductor, packages),
    list(Source = "Bioconductor", Repository = "https://b.com")
  )
})

test_that("packages installed from other repos get correctly named", {
  pkg <- list(Package = "pkg", Source = "https://test2.com")
  packages <- as.matrix(data.frame(
    row.names = "pkg",
    Version = "1.0.0",
    Repository = "https://test2.com/src/contrib",
    stringsAsFactors = FALSE
  ))
  repos <- c(TEST1 = "https://test1.com", TEST2 = "https://test2.com")

  expect_equal(
    standardizePackageSource(pkg, packages, repos = repos),
    list(Source = "TEST2", Repository = "https://test2.com")
  )
})

test_that("source packages can't be installed", {
  source <- list(Package = "pkg1", Source = "source")
  expect_equal(
    standardizePackageSource(source),
    list(Source = NA_character_, Repository = NA_character_)
  )
})

test_that("locally installed CRAN packages are handled correctly", {
  packages <- as.matrix(data.frame(
    row.names = "pkg",
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
    standardizePackageSource(local, packages, repos),
    list(Source = "CRAN", Repository = "https://cran.com")
  )

  local_dev <- list(
    Package = "pkg",
    Source = "CustomCRANLikeRepository",
    Version = "1.0.0.9000"
  )
  expect_equal(
    standardizePackageSource(local_dev, packages, repos),
    list(Source = NA_character_, Repository = NA_character_)
  )

  archived <- list(
    Package = "pkg2",
    Source = "CustomCRANLikeRepository",
    Version = "1.0.0"
  )
  expect_equal(
    standardizePackageSource(archived, packages, repos),
    list(Source = NA_character_, Repository = NA_character_)
  )
})
