# Does almost exactly the same work as writeManifest(), but called within
# deployApp() instead of being exposed to the user. Returns the path to the
# bundle directory, whereas writeManifest() returns nothing and deletes the
# bundle directory after writing the manifest.
bundleApp <- function(appName, appDir, appFiles, appPrimaryDoc, assetTypeName,
                      contentCategory, verbose = FALSE, python = NULL,
                      condaMode = FALSE, forceGenerate = FALSE, quarto = NULL,
                      isShinyApps = FALSE, metadata = list(), image = NULL) {
  logger <- verboseLogger(verbose)

  quartoInfo <- inferQuartoInfo(
    appDir = appDir,
    appPrimaryDoc = appPrimaryDoc,
    quarto = quarto,
    metadata = metadata
  )

  logger("Inferring App mode and parameters")
  appMode <- inferAppMode(
    appDir = appDir,
    appPrimaryDoc = appPrimaryDoc,
    files = appFiles,
    quartoInfo = quartoInfo
  )
  appPrimaryDoc <- inferAppPrimaryDoc(
    appPrimaryDoc = appPrimaryDoc,
    appFiles = appFiles,
    appMode = appMode
  )
  hasParameters <- appHasParameters(
    appDir = appDir,
    appPrimaryDoc = appPrimaryDoc,
    appMode = appMode,
    contentCategory = contentCategory
  )
  documentsHavePython <- detectPythonInDocuments(
    appDir = appDir,
    files = appFiles
  )

  # get application users (for non-document deployments)
  users <- NULL
  if (is.null(appPrimaryDoc)) {
    users <- suppressWarnings(authorizedUsers(appDir))
  }

  # copy files to bundle dir to stage
  logger("Bundling app dir")
  bundleDir <- bundleAppDir(
    appDir = appDir,
    appFiles = appFiles,
    appPrimaryDoc = appPrimaryDoc
  )
  on.exit(unlink(bundleDir, recursive = TRUE), add = TRUE)

  # generate the manifest and write it into the bundle dir
  logger("Generate manifest.json")
  manifest <- createAppManifest(
    appDir = bundleDir,
    appMode = appMode,
    contentCategory = contentCategory,
    hasParameters = hasParameters,
    appPrimaryDoc = appPrimaryDoc,
    assetTypeName = assetTypeName,
    users = users,
    condaMode = condaMode,
    forceGenerate = forceGenerate,
    python = python,
    documentsHavePython = documentsHavePython,
    retainPackratDirectory = TRUE,
    quartoInfo = quartoInfo,
    isShinyApps = isShinyApps,
    image = image,
    verbose = verbose
  )
  manifestJson <- enc2utf8(toJSON(manifest, pretty = TRUE))
  manifestPath <- file.path(bundleDir, "manifest.json")
  writeLines(manifestJson, manifestPath, useBytes = TRUE)

  # if necessary write an index.htm for shinydoc deployments
  logger("Writing Rmd index if necessary")
  indexFiles <- writeRmdIndex(appName, bundleDir)

  # create the bundle and return its path
  logger("Compressing the bundle")
  bundlePath <- tempfile("rsconnect-bundle", fileext = ".tar.gz")
  writeBundle(bundleDir, bundlePath)
  bundlePath
}
