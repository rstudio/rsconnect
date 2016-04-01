#' Deploy a Website
#'
#' Deploy an R Markdown website to a server.
#'
#' @inheritParams deployApp
#'
#' @param siteDir Directory containing website. Defaults to current
#'   working directory.
#' @param siteName Name for the site (names must be unique within
#'   an account). Defaults to the base name of the specified siteDir,
#'   (or to a name provided by a custom site generation function).
#' @param render Rendering behavior for site: "none" to upload a
#'   static version of the current contents of the site directory;
#'   "local" to render the site locally then upload it; "server" to
#'   render the site on the server. Note that for "none" and "local"
#'   R scripts (.R) and markdown documents (.Rmd and .md) will not be
#'   uploaded to the server.
#'
#' @export
deploySite <- function(siteDir = getwd(),
                       siteName = NULL,
                       account = NULL,
                       server = NULL,
                       render = c("none", "local", "server"),
                       launch.browser = getOption("rsconnect.launch.browser", interactive()),
                       quiet = FALSE,
                       lint = FALSE,
                       metadata = list()) {

  # switch to siteDir for duration of this function
  oldwd <- setwd(siteDir)
  on.exit(setwd(oldwd), add = TRUE)

  # validate we have the version of rmarkdown required to discover
  # whether this directory has a website in it, what it's name
  # and content directory are, etc.
  rmarkdownVersion <- "0.9.5.3"
  if (!requireNamespace("rmarkdown") ||
      packageVersion("rmarkdown") < rmarkdownVersion) {
    stop("Version ", rmarkdownVersion, " or later of the rmarkdown package ",
         "is required to deploy websites.")
  }

  # validate and normalize siteDir
  if (!isStringParam(siteDir))
    stop(stringParamErrorMessage("siteDir"))
  siteDir <- normalizePath(siteDir, mustWork = FALSE)
  if (!file.exists(siteDir)) {
    stop(siteDir, " does not exist")
  }

  # discover the site generator
  siteGenerator <- rmarkdown::site_generator(siteDir)
  if (is.null(siteGenerator))
    stop("index file with site entry not found in ", siteDir)

  # render locally if requested
  render <- match.arg(render)
  if (render == "local") {
    siteGenerator$render(input_file = NULL,
                         output_format = NULL,
                         envir = new.env(),
                         quiet = quiet,
                         encoding = getOption("encoding"))
  }

  # if there is no explicit siteName get it from the generator
  appName <- siteName
  if (is.null(appName))
    appName <- siteGenerator$name

  # determine appDir based on whether we are rendering on the server
  if (render == "server") {
    appDir <- '.'
    appFiles <- NULL
  } else {
    appDir <- siteGenerator$output_dir
    appFiles <- bundleFiles(siteGenerator$output_dir)
    appFiles <- appFiles[!grepl("^.*\\.([Rr]|[Rr]md|md)$", appFiles)]
  }

  # determine appSourceDoc
  if (file.exists("index.Rmd"))
    appSourceDoc <- "index.Rmd"
  else
    appSourceDoc <- "index.md"

  # deploy the site
  deployApp(appName = appName,
            appDir = appDir,
            appFiles = appFiles,
            appSourceDoc = appSourceDoc,
            contentCategory = "site",
            account = account,
            server = server,
            launch.browser = launch.browser,
            quiet = quiet,
            lint = lint,
            metadata = metadata)
}
