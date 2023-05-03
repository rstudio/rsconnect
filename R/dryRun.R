#' Perform a deployment "dry run"
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `dryRun()` runs your app locally, attempting to simulate what will happen
#' when you deploy it on another machine. This isn't a 100% reliable way of
#' discovering problems, but it offers a much faster iteration cycle, so where
#' it does reveal a problem, it will typically make identifying and fixing it
#' much faster.
#'
#' This function is still experimental, so please let us know your experiences
#' and where we could do better:
#' <https://github.com/rstudio/rsconnect/issues/new>
#'
#' ## Where it helps
#'
#' `dryRun()` was motivated by the two most common problems when deploying
#' your app:
#'
#' * The server doesn't install all the packages your app needs to work.
#'   `dryRun()` solves this by using `renv::restore()` to create a project
#'   specific library that uses only the packages that are explicitly used
#'   by your project.
#'
#' * The server doesn't have environment variables you need. `dryRun()`
#'   solves this by removing any environment variables that you've
#'   set in `~/.Renviron`, except for those that you declare in `envVars`.
#'   Additionally, to help debugging it also reports whenever any env var is
#'   used.
#'
#' `dryRun` will also log when you use functions that are usually best avoided
#' in deployed code. This includes:
#'
#' * `rsconnect::deployApp()` because you shouldn't deploy an app from another
#'   app. This typically indicates that you've included a file with scratch
#'   code.
#'
#' * `install.packages()` and `.libPaths()` because you should rely on the
#'   server to install and manage your packages.
#'
#' * `browser()`, `browseURL()`, and `rstudioapi::askForPassword()` because
#'    they need an interactive session that your deployment server will lack.
#'
#' ## Current limitations
#'
#' * `dryRun()` currently offers no way to diagnose problems with
#'   mismatched R/Python/Quarto/pandoc versions.
#'
#' * `dryRun()` doesn't help much with paths. There are two common problems
#'   it can't help with: using an absolute path and using the wrong case.
#'   Both of these will work locally but fail on the server. [lint()]
#'   uses an alternative technique (static analysis) to detect many of these
#'   cases.
#'
#' @inheritParams deployApp
#' @param contentCategory Set this to `"site"` if you'd deploy with
#'   [deploySite()]; otherwise leave as is.
#' @param verbose If TRUE, prints progress messages to the console
#' @export
dryRun <- function(appDir = getwd(),
                   envVars = NULL,
                   appFiles = NULL,
                   appFileManifest = NULL,
                   appPrimaryDoc = NULL,
                   contentCategory = NULL,
                   quarto = NA) {
  appFiles <- listDeploymentFiles(
    appDir,
    appFiles = appFiles,
    appFileManifest = appFileManifest
  )

  appMetadata <- appMetadata(
    appDir = appDir,
    appFiles = appFiles,
    appPrimaryDoc = appPrimaryDoc,
    quarto = quarto,
    contentCategory = contentCategory,
  )

  # copy files to bundle dir to stage
  bundleDir <- bundleAppDir(
    appDir = appDir,
    appFiles = appFiles,
    appPrimaryDoc = appMetadata$appPrimaryDoc
  )
  on.exit(unlink(bundleDir, recursive = TRUE), add = TRUE)


  renv::restore(bundleDir, prompt = FALSE)

  # Add tracing code -------------------------------------------------
  file.copy(
    system.file("lint-trace.R", package = "rsconnect"),
    "__rsconnect-dryRunTrace.R"
  )
  appendLines(
    file.path(bundleDir, ".Rprofile"),
    c("", 'source("__rsconnect-dryRunTrace.R")')
  )

  # Run ---------------------------------------------------------------
  if (appMode %in% c("rmd-shiny", "quarto-shiny", "shiny", "api")) {
    cli::cli_alert_info("Terminate the app to complete the dryRun")
  }

  envVarNames <- setdiff(userEnvVars(), envVars)
  envVarReset <- c(rep_named(envVarNames, ""), callr::rcmd_safe_env())

  callr::r(
    appRunner(appMetadata$appMode),
    args = list(primaryDoc = appMetadata$primaryDoc),
    env = envVarReset,
    wd = bundleDir
  )
}

appRunner <- function(appMode) {
  switch(
    "rmd-static" = ,
    "rmd-shiny" = function(primaryDoc) rmarkdown::render(primaryDoc),
    "quarto-static" = ,
    "quarto-shiny" = function(primaryDoc) quarto::quarto_render(primaryDoc),
    "shiny" = function(primaryDoc) shiny::runApp(),
    "api" = function(primaryDoc) plumber::pr_run(plumber::pr("plumber.R")),
    cli::cli_abort("Content type {appMode} not currently supported")
  )
}

appendLines <- function(path, lines) {
  lines <- c(readLines(path), lines)
  writeLines(lines, path)
}

userEnvVars <- function() {
  if (!file.exists("~/.Renviron")) {
    return(charater())
  }

  lines <- readLines("~/.Renviron")
  lines <- lines[lines != ""]
  lines <- lines[!grepl("^#", lines)]

  pieces <- strsplit(lines, "=", fixed = TRUE)
  names <- vapply(
    pieces,
    function(x) if (length(x) >= 2) x[[1]] else "",
    character(1)
  )

  sort(unique(names[names != ""]))
}
