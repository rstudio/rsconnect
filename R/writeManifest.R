#' Create a manifest.json describing deployment requirements.
#'
#' Given a directory content targeted for deployment, write a manifest.json into
#' that directory describing the deployment requirements for that content.
#'
#' @param appDir Directory containing the content (Shiny application, R Markdown
#'   document, etc).
#'
#' @param appFiles Optional. The full set of files and directories to be
#'   included in future deployments of this content. Used when computing
#'   dependency requirements. When `NULL`, all files in `appDir` are considered.
#'
#' @param appPrimaryDoc Optional. Specifies the primary document in a content
#'   directory containing more than one. If `NULL`, the primary document is
#'   inferred from the file list.
#'
#' @param contentCategory Optional. Specifies the kind of content being deployed
#'   (e.g. `"plot"` or `"site"`).
#'
#' @param python Optional. Full path to a Python binary for use by `reticulate`.
#'   The specified Python binary will be invoked to determine its version and to
#'   list the Python packages installed in the environment. If `python = NULL`,
#'   and `RETICULATE_PYTHON` is set in the environment, its value will be used.
#'
#' @param forceGeneratePythonEnvironment Optional. If an existing
#'   `requirements.txt` file is found, it will be overwritten when this argument
#'   is `TRUE`.
#'
#' @param quarto Optional. Full path to a Quarto binary for use deploying Quarto
#'   content. The provided Quarto binary will be used to run `quarto inspect`
#'   to gather information about the content.
#'
#' @param image Optional. The name of the image to use when building and
#'   executing this content. If none is provided, RStudio Connect will
#'   attempt to choose an image based on the content requirements.
#'
#' @param verbose If TRUE, prints progress messages to the console
#'
#'
#' @export
writeManifest <- function(appDir = getwd(),
                          appFiles = NULL,
                          appPrimaryDoc = NULL,
                          contentCategory = NULL,
                          python = NULL,
                          forceGeneratePythonEnvironment = FALSE,
                          quarto = NULL,
                          image = NULL,
                          verbose = FALSE) {

  condaMode <- FALSE

  if (is.null(appFiles)) {
    appFiles <- bundleFiles(appDir)
  } else {
    appFiles <- explodeFiles(appDir, appFiles)
  }

  quartoInfo <- inferQuartoInfo(
    appDir = appDir,
    appPrimaryDoc = appPrimaryDoc,
    appFiles = appFiles,
    quarto = quarto,
    metadata = list()
  )

  appMode <- inferAppMode(
      appDir = appDir,
      appPrimaryDoc = appPrimaryDoc,
      files = appFiles,
      quartoInfo = quartoInfo)
  appPrimaryDoc <- inferAppPrimaryDoc(
      appPrimaryDoc = appPrimaryDoc,
      appFiles = appFiles,
      appMode = appMode)
  hasParameters <- appHasParameters(
      appDir = appDir,
      appPrimaryDoc = appPrimaryDoc,
      appMode = appMode,
      contentCategory = contentCategory)
  documentsHavePython <- detectPythonInDocuments(
      appDir = appDir,
      files = appFiles)

  # copy files to bundle dir to stage
  bundleDir <- bundleAppDir(
      appDir = appDir,
      appFiles = appFiles,
      appPrimaryDoc = appPrimaryDoc)
  on.exit(unlink(bundleDir, recursive = TRUE), add = TRUE)

  python <- getPython(python)

  # generate the manifest and write it into the bundle dir
  manifest <- createAppManifest(
      appDir = bundleDir,
      appMode = appMode,
      contentCategory = contentCategory,
      hasParameters = hasParameters,
      appPrimaryDoc = appPrimaryDoc,
      assetTypeName = "content",
      users = NULL,
      condaMode = condaMode,
      forceGenerate = forceGeneratePythonEnvironment,
      python = python,
      documentsHavePython = documentsHavePython,
      retainPackratDirectory = FALSE,
      quartoInfo = quartoInfo,
      isShinyApps = FALSE,
      image = image,
      verbose = verbose)
  manifestJson <- enc2utf8(toJSON(manifest, pretty = TRUE))
  manifestPath <- file.path(appDir, "manifest.json")
  writeLines(manifestJson, manifestPath, useBytes = TRUE)

  requirementsFilename <- manifest$python$package_manager$package_file
  if (is.null(requirementsFilename)) { requirementsFilename <- "requirements.txt" }
  srcRequirementsFile <- file.path(bundleDir, requirementsFilename)
  dstRequirementsFile <- file.path(appDir, requirementsFilename)
  if(file.exists(srcRequirementsFile) && !file.exists(dstRequirementsFile)) {
    file.copy(srcRequirementsFile, dstRequirementsFile)
  }
  invisible()
}
