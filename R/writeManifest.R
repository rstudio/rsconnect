#' Create a manifest.json describing deployment requirements.
#'
#' Given a directory content targeted for deployment, write a manifest.json into
#' that directory describing the deployment requirements for that content.
#'
#' @inheritParams deployApp
#' @param verbose If TRUE, prints progress messages to the console
#' @export
writeManifest <- function(appDir = getwd(),
                          appFiles = NULL,
                          appFileManifest = NULL,
                          appPrimaryDoc = NULL,
                          contentCategory = NULL,
                          python = NULL,
                          forceGeneratePythonEnvironment = FALSE,
                          quarto = NULL,
                          image = NULL,
                          verbose = FALSE) {

  appFiles <- standardizeAppFiles(
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
      appPrimaryDoc = appMetadata$appPrimaryDoc)
  on.exit(unlink(bundleDir, recursive = TRUE), add = TRUE)

  python <- getPython(python)
  pythonConfig <- pythonConfigurator(python, forceGeneratePythonEnvironment)

  # generate the manifest and write it into the bundle dir
  manifest <- createAppManifest(
      appDir = bundleDir,
      appMode = appMetadata$appMode,
      contentCategory = contentCategory,
      hasParameters = appMetadata$hasParameters,
      appPrimaryDoc = appMetadata$appPrimaryDoc,
      users = NULL,
      pythonConfig = pythonConfig,
      documentsHavePython = appMetadata$documentsHavePython,
      retainPackratDirectory = FALSE,
      quartoInfo = appMetadata$quartoInfo,
      isCloud = FALSE,
      image = image,
      verbose = verbose)
  manifestJson <- enc2utf8(toJSON(manifest, pretty = TRUE))
  manifestPath <- file.path(appDir, "manifest.json")
  writeLines(manifestJson, manifestPath, useBytes = TRUE)

  requirementsFilename <- manifest$python$package_manager$package_file
  if (is.null(requirementsFilename)) { requirementsFilename <- "requirements.txt" }
  srcRequirementsFile <- file.path(bundleDir, requirementsFilename)
  dstRequirementsFile <- file.path(appDir, requirementsFilename)
  if (file.exists(srcRequirementsFile) && !file.exists(dstRequirementsFile)) {
    file.copy(srcRequirementsFile, dstRequirementsFile)
  }
  invisible()
}
