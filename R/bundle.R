

bundleApp <- function(appDir) {

  # create a directory to stage the application bundle in
  bundleDir <- tempfile()
  dir.create(bundleDir, recursive=TRUE)
  on.exit(unlink(bundleDir))
  
  # determine the files that will be in the bundle (exclude the 
  # shinyapps directory and the packages/lib directory)
  files <- list.files(appDir, recursive=TRUE)
  files <- files[!grepl(glob2rx("shinyapps/*"), files)]
  files <- files[!grepl(glob2rx("packages/lib/*"), files)]
  
  # copy the files into the bundle dir
  for (file in files) {
    from <- file.path(appDir, file)
    to <- file.path(bundleDir, file)
    if (!file.exists(dirname(to)))
      dir.create(dirname(to), recursive=TRUE)
    file.copy(from, to)
  }
  
  # generate the manifest and write it into the bundle dir
  manifestJson <- createAppManifest(appDir, files)
  writeLines(manifestJson, file.path(bundleDir, "shinyapps.manifest.json"))
  
  # create the bundle and return it's path
  prevDir <- setwd(bundleDir)
  on.exit(setwd(prevDir))
  bundlePath <- tempfile("shinyapps-bundle", fileext = ".tar.gz")
  utils::tar(bundlePath, files = ".", compression = "gzip")
  bundlePath
}

createAppManifest <- function(appDir, files) {
  
  # provide package description info for all dependencies
  packages <- list()
  for (pkg in appDependencies(appDir))
    packages[[pkg]] <- utils::packageDescription(pkg)

  # provide checksums for all files
  fileChecksums <- list()
  for (file in files) {
    checksum <- list(checksum = digest::digest(file.path(appDir, file), 
                                               algo="md5", file=TRUE))
    fileChecksums[[file]] <- I(checksum)
  }
  
  # create the manifest
  manifest <- list()
  manifest$version <- 1
  manifest$packages <- I(packages)
  manifest$files <- I(fileChecksums)
  
  # return it as json
  RJSONIO::toJSON(manifest, pretty = TRUE)
}

