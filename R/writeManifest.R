#' Create a `manifest.json`
#'
#' @description
#' Use `writeManifest()` to generate a `manifest.json`. Among other things,
#' you can commit this file to git to activate
#' [Git-Backed content](https://docs.posit.co/connect/user/git-backed/)
#' for Posit Connect.
#'
#' `manifest.json` contains a list of all files in the app along with their
#' dependencies, so you will need to re-run `writeManifest()` when either of
#' these change.
#'
#' @inheritParams deployApp
#' @param contentCategory Set this to `"site"` if you'd deploy with
#'   [deploySite()]; otherwise leave as is.
#' @param verbose If `TRUE`, prints detailed progress messages.
#' @param quiet If `FALSE`, prints progress messages.
#' @export
writeManifest <- function(appDir = getwd(),
                          appFiles = NULL,
                          appFileManifest = NULL,
                          appPrimaryDoc = NULL,
                          appMode = NULL,
                          contentCategory = NULL,
                          python = NULL,
                          forceGeneratePythonEnvironment = FALSE,
                          quarto = NA,
                          image = NULL,
                          envManagement = NULL,
                          envManagementR = NULL,
                          envManagementPy = NULL,
                          verbose = FALSE,
                          quiet = FALSE) {
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
    appMode = appMode,
    contentCategory = contentCategory,
  )

  # copy files to bundle dir to stage
  bundleDir <- bundleAppDir(
    appDir = appDir,
    appFiles = appFiles,
    appPrimaryDoc = appMetadata$appPrimaryDoc
  )
  defer(unlink(bundleDir, recursive = TRUE))

  python <- getPython(python)
  pythonConfig <- pythonConfigurator(python, forceGeneratePythonEnvironment)

  # generate the manifest and write it into the bundle dir
  manifest <- createAppManifest(
    appDir = bundleDir,
    appMetadata = appMetadata,
    pythonConfig = pythonConfig,
    retainPackratDirectory = FALSE,
    image = image,
    envManagement = envManagement,
    envManagementR = envManagementR,
    envManagementPy = envManagementPy,
    verbose = verbose,
    quiet = quiet
  )
  manifestJson <- toJSON(manifest)
  manifestPath <- file.path(appDir, "manifest.json")
  writeLines(manifestJson, manifestPath, useBytes = TRUE)

  requirementsFilename <- manifest$python$package_manager$package_file
  if (is.null(requirementsFilename)) {
    requirementsFilename <- "requirements.txt"
  }
  srcRequirementsFile <- file.path(bundleDir, requirementsFilename)
  dstRequirementsFile <- file.path(appDir, requirementsFilename)
  if (file.exists(srcRequirementsFile) && !file.exists(dstRequirementsFile)) {
    file.copy(srcRequirementsFile, dstRequirementsFile)
  }

  invisible()
}
