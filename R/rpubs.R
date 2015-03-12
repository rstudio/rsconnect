#' Upload an HTML file to RPubs
#'
#' This function uploads an HTML file to rpubs.com. If the upload succeeds a
#' list that includes an \code{id} and \code{continueUrl} is returned. A browser
#' should be opened to the \code{continueUrl} to complete publishing of the
#' document. If an error occurs then a diagnostic message is returned in the
#' \code{error} element of the list.
#'
#' @param title The title of the document.
#' @param htmlFile The path to the HTML file to upload.
#' @param originalRmd The R Markdown document that was rendered to produce the
#'   \code{htmlFile}.
#' @param id If this upload is an update of an existing document then the id
#'   parameter should specify the document id to update. Note that the id is
#'   provided as an element of the list returned by successful calls to
#'   \code{rpubsUpload}.
#' @param properties A named list containing additional document properties
#'   (RPubs doesn't currently expect any additional properties, this parameter
#'   is reserved for future use).
#'
#' @return A named list. If the upload was successful then the list contains a
#'   \code{id} element that can be used to subsequently update the document as
#'   well as a \code{continueUrl} element that provides a URL that a browser
#'   should be opened to in order to complete publishing of the document. If the
#'   upload fails then the list contains an \code{error} element which contains
#'   an explanation of the error that occurred.
#'
#' @examples
#' \dontrun{
#' # upload a document
#' result <- rpubsUpload("My document title", "Document.html")
#' if (!is.null(result$continueUrl))
#'    browseURL(result$continueUrl)
#' else
#'    stop(result$error)
#'
#' # update the same document with a new title
#' updateResult <- rpubsUpload("My updated title", "Document.html", result$id)
#' }
#' @export
rpubsUpload <- function(title,
                        htmlFile,
                        originalRmd,
                        id = NULL,
                        properties = list()) {

  # validate inputs
  if (!is.character(title))
    stop("title must be specified")
  if (nzchar(title) == FALSE)
    stop("title pmust be a non-empty string")
  if (!is.character(htmlFile))
    stop("htmlFile parameter must be specified")
  if (!file.exists(htmlFile))
    stop("specified htmlFile does not exist")
  if (!is.character(originalRmd))
    stop("originalRmd parameter must be specified")
  if (!file.exists(originalRmd))
    stop("specified originalRmd does not exist")
  if (!is.list(properties))
    stop("properties paramater must be a named list")

  pathFromId <- function(id) {
    split <- strsplit(id, "^https?://[^/]+")[[1]]
    if (length(split) == 2)
      return (split[2])
    else
      return (NULL)
  }

  buildPackage <- function(title,
                           htmlFile,
                           properties = list()) {

    # build package.json
    properties$title = title
    packageJson <- RJSONIO::toJSON(properties)

    # create a tempdir to build the package in and copy the files to it
    fileSep <- .Platform$file.sep
    packageDir <- tempfile()
    dir.create(packageDir)
    packageFile <- function(fileName) {
      paste(packageDir,fileName,sep=fileSep)
    }
    writeLines(packageJson, packageFile("package.json"))
    file.copy(htmlFile, packageFile("index.html"))

    # switch to the package dir for building
    oldWd <- getwd()
    setwd(packageDir)
    on.exit(setwd(oldWd))

    # create the tarball
    tarfile <- tempfile("package", fileext = ".tar.gz")
    utils::tar(tarfile, files = ".", compression = "gzip")

    # return the full path to the tarball
    return (tarfile)
  }

  # build the package
  packageFile <- buildPackage(title, htmlFile, properties)

  # determine whether this is a new doc or an update
  isUpdate <- FALSE
  method <- "POST"
  path <- "/api/v1/document"
  headers <- list()
  headers$Connection <- "close"
  if (!is.null(id)) {
    isUpdate <- TRUE
    path <- pathFromId(id)
    method <- "PUT"
  }

  # use https if using RCurl, and vanilla HTTP otherwise
  http <- httpFunction()
  if (identical(http, httpRCurl)) {
    protocol <- "https"
    port <- 443
  } else {
    protocol <- "http"
    port <- 80
  }

  # send the request
  result <- http(protocol, "api.rpubs.com", port, method, path, headers,
                 "application/x-compressed", file = packageFile)

  # check for success
  succeeded <- FALSE
  if (isUpdate && (result$status == 200))
    succeeded <- TRUE
  else if (result$status == 201)
    succeeded <- TRUE

  # mark content as UTF-8
  content <- result$content
  Encoding(content) <- "UTF-8"

  # return either id & continueUrl or error
  if (succeeded) {
    parsedContent <- RJSONIO::fromJSON(content)
    id <- ifelse(isUpdate, id, result$location)
    url <- as.character(parsedContent["continueUrl"])

    # write the deployment record
    rpubsRec <- deploymentRecord(basename(originalRmd), "rpubs", "rpubs.com",
                                 id, url)
    rpubsRecFile <- deploymentFile(originalRmd, basename(originalRmd), "rpubs",
                                   "rpubs.com")
    write.dcf(rpubsRec, rpubsRecFile)

    # return the publish information
    return (list(id = id,
                 continueUrl = url))
  }
  else {
    return (list(error = content))
  }
}

