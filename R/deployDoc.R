#' Deploy a Document
#'
#' Deploys an application consisting of a single R Markdown document.
#'
#' @param doc Path to the document to deploy.
#' @param ... Additional arguments to \code{\link{deployApp}}
#'
#' @details In addition to the R Markdown file itself, any files which are
#'   required to render and display the file must be deployed.
#'
#'   This method discovers these additional files using
#'   \code{\link[rmarkdown:find_external_resources]{find_external_resources}}
#'   from \pkg{rmarkdown}.
#'
#'   If you find that the document is missing dependencies, either specify the
#'   dependencies explicitly in the document (the documentation for
#'   \code{\link[rmarkdown:find_external_resources]{find_external_resources}}
#'   explains how to do this), or call \code{\link{deployApp}} directly and
#'   specify your own file list in the \code{appFiles} parameter.
#'
#' @export
deployDoc <- function(doc, ...) {
  # validate inputs
  if (!file.exists(doc)) {
    stop("The document '", doc, "' does not exist.")
  }
  if (tolower(tools::file_ext(doc)) != "rmd") {
    stop("Document deployment is only supported for R Markdown documents.")
  }
  if (!require("rmarkdown") ||
      packageVersion("rmarkdown") < "0.5.2") {
    stop("Version 0.5.2 or later of the rmarkdown package is required to ",
         "deploy individual R Markdown documents.")
  }

  # discover the resources in the document using the facilities in rmarkdown
  message("Discovering document dependencies... ", appendLF = FALSE)
  qualified_doc <- normalizePath(doc, winslash = "/")
  res <- rmarkdown::find_external_resources(qualified_doc)
  message("OK")

  # deploy the document with the discovered dependencies
  deployApp(appDir = dirname(qualified_doc),
            appFiles = c(res$path, basename(qualified_doc)),
            appPrimaryRmd = basename(qualified_doc),
            ...)
}

