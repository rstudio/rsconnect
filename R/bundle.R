

bundleApp <- function(appDir, appName) {

  # create a directory to stage the application bundle in
  bundleDir <- tempfile()
  bundleAppDir <- file.path(bundleDir, "app")
  dir.create(bundleAppDir, recursive=TRUE)
  on.exit(unlink(bundleDir))

  # copy the appDir into the bundleAppDir
  appFiles <- file.path(appDir, list.files(appDir))
  file.copy(appFiles, bundleAppDir, recursive=TRUE)
  
  # generate the manifest and write it into the bundle dir
  manifestJson <- createAppManifest(appDir, appName)
  writeLines(manifestJson, file.path(bundleDir, "manifest.json"))
  
  # create the bundle and return it's path
  prevDir <- setwd(bundleDir)
  on.exit(setwd(prevDir))
  bundleName <- paste("shinyapps-", appName, "-", sep="")
  bundlePath <- tempfile(bundleName, fileext = ".tar.gz")
  utils::tar(bundlePath, files = ".", compression = "gzip")
  bundlePath
}

createAppManifest <- function(appDir, appName) {
  
  # provide package description info for all dependencies
  packages <- list()
  for (pkg in appDependencies(appDir))
    packages[[pkg]] <- utils::packageDescription(pkg)

  # create the manifest
  manifest <- list()
  manifest$name <- appName
  manifest$packages <- I(packages)
  
  # return it as json
  RJSONIO::toJSON(manifest, pretty = TRUE)
}

