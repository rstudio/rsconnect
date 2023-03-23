#' @examples
#' makeRenvSnapshot(test_path("renv-cran"), "cli")
#' makeRenvSnapshot(test_path("renv-bioc"), "bioBase", "bioc::Biobase")
#' makeRenvSnapshot(test_path("renv-github"), "withr", "r-lib/withr")
makeRenvSnapshot <- function(path, name, package = name) {
  dir.create(path, showWarnings = FALSE)

  withr::local_dir(path)
  writeLines(paste0("library(", name, ")"), "dependences.R")

  callr::r(args = list(package = package), function(package) {
    renv::activate()
    renv::install(package)
    renv::snapshot(prompt = FALSE)
  })

  unlink(c(".Rprofile", ".gitignore", "renv"), recursive = TRUE)

  invisible()
}

# -------------------------------------------------------------------------

test_that("generates expected packrat file", {
  expect_snapshot({
    showDcf(parseRenvDependencies(test_path("renv-cran")))
    showDcf(parseRenvDependencies(test_path("renv-github")))
    showDcf(parseRenvDependencies(test_path("renv-bioc")))
  })
})
