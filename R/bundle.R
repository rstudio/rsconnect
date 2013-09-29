

bundleApp <- function(name, appDir) {

  # create a directory to stage the application bundle in
  bundleDir <- tempfile()
  bundleAppDir <- file.path(bundleDir, "app")
  dir.create(bundleAppDir, recursive=TRUE)
  on.exit(unlink(bundleDir))

  # copy the appDir into the bundleAppDir
  appFiles <- list.files(appDir)
  file.copy(appFiles, bundleAppDir, recursive=TRUE)
  
  # generate the manifest and write it into the bundle dir
  manifestJson <- createAppManifest(name, appDir)
  writeLines(manifestJson, file.path(bundleDir, "manifest.json"))
  
  # create the bundle and return it's path
  prevDir <- setwd(bundleDir)
  on.exit(setwd(prevDir))
  bundleName <- paste("shinyapps-", name, "-", sep="")
  bundlePath <- tempfile(bundleName, fileext = ".tar.gz")
  utils::tar(bundlePath, files = ".", compression = "gzip")
  bundlePath
}

createAppManifest <- function(name, appDir) {
  
  # get the package dependencies
  dependencies <- appDependencies(appDir)
  
  # read the package descriptions
  descriptions <- list()
  for (pkg in dependencies) {
    description <- utils::packageDescription(pkg)
    descriptions[[pkg]] <- description
  }

  # create the manifest
  manifest <- list()
  manifest$name <- name
  manifest$dependencies <- I(dependencies)
  manifest$package_descriptions <- I(descriptions)
  
  # return it as json
  RJSONIO::toJSON(manifest, pretty = TRUE)
}

