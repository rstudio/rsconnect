#' @examples
#' makeRenvSnapshot(test_path("renv-cran"), "cli")
#' makeRenvSnapshot(test_path("renv-bioc"), "bioBase", "bioc::Biobase")
#' makeRenvSnapshot(test_path("renv-github"), "withr", "r-lib/withr")
makeRenvSnapshot <- function(path, name, package = name) {
  dir.create(path, showWarnings = FALSE)

  withr::local_dir(path)
  writeLines(paste0("library(", name, ")"), "dependences.R")

  getNamespace("callr")$r(args = list(package = package), function(package) {
    renv::activate()
    renv::install(package)
    renv::snapshot(prompt = FALSE)
  })

  unlink(c(".Rprofile", ".gitignore", "renv"), recursive = TRUE)

  invisible()
}

# -------------------------------------------------------------------------

test_that("recommended packages are snapshotted", {
  app <- local_temp_app(list("index.Rmd" = c(
    "```{r}",
    "library(MASS)",
    "```"
  )))
  deps <- snapshotRenvDependencies(app)
  expect_true("MASS" %in% deps$Package)
})

test_that("works with BioC packages", {
  app <- local_temp_app(list("index.Rmd" = c(
    "```{r}",
    "library(Biobase)",
    "```"
  )))
  withr::local_options(repos = c(
    CRAN = "https://cran.rstudio.com",
    BioC = "https://bioconductor.org/packages/3.16/bioc"
  ))

  deps <- snapshotRenvDependencies(app)

  Biobase <- deps[deps$Package == "Biobase", ]
  expect_equal(Biobase$Source, "Bioconductor")
  expect_equal(Biobase$Repository, "https://bioconductor.org/packages/3.16/bioc")

  BiocGenerics <- deps[deps$Package == "BiocGenerics", ]
  expect_equal(BiocGenerics$Source, "Bioconductor")
  expect_equal(BiocGenerics$Repository, "https://bioconductor.org/packages/3.16/bioc")
})


test_that("generates expected packrat file", {
  expect_snapshot({
    showDcf(parseRenvDependencies(test_path("renv-cran")))
    showDcf(parseRenvDependencies(test_path("renv-github")))
    showDcf(parseRenvDependencies(test_path("renv-bioc")))
  })
})
