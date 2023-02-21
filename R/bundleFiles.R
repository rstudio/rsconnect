# build the list of files to deploy -- implicitly (directory contents),
# explicitly via list, or explicitly via manifest. Always returns paths
# related to `appDir`
standardizeAppFiles <- function(appDir,
                                appFiles = NULL,
                                appFileManifest = NULL,
                                error_call = caller_env()) {


  no_content <- function(message) {
    cli::cli_abort(
      c("No content to deploy.", x = message),
      call = error_call
    )
  }

  if (!is.null(appFiles) && !is.null(appFileManifest)) {
    cli::cli_abort(
      "Must specify at most one of {.arg appFiles} and {.arg appFileManifest}",
      call = error_call
    )
  } else if (is.null(appFiles) && is.null(appFileManifest)) {
    # no files supplied at all, just bundle the whole directory
    appFiles <- bundleFiles(appDir)
    if (length(appFiles) == 0) {
      no_content("{.arg appDir} is empty.")
    }
  } else if (!is.null(appFiles)) {
    check_character(appFiles, allow_null = TRUE, call = error_call)
    appFiles <- explodeFiles(appDir, appFiles)
    if (length(appFiles) == 0) {
      no_content("{.arg appFiles} didn't match any files in {.arg appDir}.")
    }
  } else if (!is.null(appFileManifest)) {
    check_file(appFileManifest, error_call = error_call)
    appFiles <- readFileManifest(appFileManifest)
    appFiles <- explodeFiles(appDir, appFiles)
    if (length(appFiles) == 0) {
      no_content("{.arg appFileManifest} contains no usable files.")
    }
  }

  appFiles
}

readFileManifest <- function(appFileManifest, error_call = caller_env()) {
  lines <- readLines(appFileManifest, warn = FALSE)

  # remove empty/comment lines
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^#", lines)]
  lines
}

# given a list of mixed files and directories, explodes the directories
# recursively into their constituent files, and returns just a list of files
explodeFiles <- function(dir, files) {
  exploded <- character()

  totalFiles <- 0
  totalSize <- 0

  for (f in files) {
    target <- file.path(dir, f)
    if (!file.exists(target)) {
      # doesn't exist
      next
    } else if (dirExists(target)) {
      # a directory; explode it
      contentPaths <- file.path(f, list.files(
        target,
        recursive = TRUE,
        include.dirs = FALSE
      ))

      exploded <- c(exploded, contentPaths)
      totalFiles <- totalFiles + length(contentPaths)
      totalSize <- totalSize + sum(file_size(file.path(dir, contentPaths)))
    } else {
      # must be an ordinary file
      exploded <- c(exploded, f)
      totalFiles <- totalFiles + 1
      totalSize <- totalSize + file_size(target)
    }

    enforceBundleLimits(dir, totalFiles, totalSize)
  }

  exploded
}

#' List Files to be Bundled
#'
#' @description
#' Given a directory containing an application, returns the names of the files
#' that by default will be bundled in the application. It works similarly to
#' a recursive directory listing from [list.files()] but:
#'
#' * If the total size of the files exceeds the maximum bundle size,
#'   `listBundleFiles()` will error. The maximum bundle size is controlled by
#'    the `rsconnect.max.bundle.size` option.
#' *  If the total size number of files exceeds the maximum number to be
#'    bundled, `listBundleFiles()` will error. The maximum number of files in
#'    the bundle is controlled by the `rsconnect.max.bundle.files` option.
#' *  Certain files and folders that don't need to be bundled, such as
#'    those containing internal version control and RStudio state, are
#'    automatically excluded.
#' *  You can exclude additional files by listing them in in a `.rscignore`
#'    file. This file must have one file or directory per line (with path
#'    relative to the current directory). It doesn't support wildcards, or
#'    ignoring files in subdirectories.
#'
#' @param appDir Directory containing the application.
#' @return Returns a list containing the following elements:
#' * `totalFiles`: Total number of files.
#' * `totalSize`: Total size of the files (in bytes).
#' * `contents`: Paths to bundle, relative to `appDir`.
#' @export
listBundleFiles <- function(appDir) {
  recursiveBundleFiles(appDir)
}

bundleFiles <- function(appDir) {
  listBundleFiles(appDir)$contents
}

recursiveBundleFiles <- function(dir,
                                 root_dir = dir,
                                 depth = 0,
                                 totalFiles = 0,
                                 totalSize = 0) {
  # generate a list of files at this level
  contents <- list.files(dir, all.files = TRUE, no.. = TRUE, include.dirs = TRUE)
  contents <- ignoreBundleFiles(dir, contents, depth = depth)

  # Info for each file lets us know to recurse (directories) or aggregate (files).
  is_dir <- dir.exists(file.path(dir, contents))
  names(is_dir) <- contents

  children <- character()
  for (name in contents) {
    if (isTRUE(is_dir[[name]])) {
      out <- recursiveBundleFiles(
        dir = file.path(dir, name),
        root_dir = root_dir,
        totalFiles = totalFiles,
        totalSize = totalSize,
        depth = depth + 1
      )

      children <- append(children, file.path(name, out$contents))
      totalFiles <- out$totalFiles
      totalSize <- out$totalSize
    } else {
      children <- append(children, name)
      totalFiles <- totalFiles + 1
      totalSize <- totalSize + file_size(file.path(dir, name))
    }
  }

  enforceBundleLimits(root_dir, totalFiles, totalSize)
  list(
    contents = children,
    totalFiles = totalFiles,
    totalSize = totalSize
  )
}

ignoreBundleFiles <- function(dir, contents, depth = 0) {
  # exclude some well-known files/directories at root level
  if (depth == 0) {
    contents <- contents[!grepl(glob2rx("*.Rproj"), contents)]
    contents <- setdiff(
      contents,
      c("manifest.json", "rsconnect", "packrat", "app_cache", ".Rproj.user")
    )
  }

  # exclude renv files, knitr cache dirs, and another well-known files
  contents <- setdiff(contents, c("renv", "renv.lock"))
  contents <- contents[!isKnitrCacheDir(contents)]
  contents <- setdiff(
    contents,
    c(".DS_Store", ".git", ".gitignore", ".quarto", ".Rhistory", ".svn")
  )

  # remove any files lines listed .rscignore
  if (".rscignore" %in% contents) {
    ignoreContents <- readLines(file.path(dir, ".rscignore"))
    contents <- setdiff(contents, c(ignoreContents, ".rscignore"))
  }

  contents
}

isKnitrCacheDir <- function(files) {
  is_cache <- grepl("^.+_cache$", files)

  cache_rmd <- gsub("_cache$", ".Rmd", files)
  has_rmd <- tolower(cache_rmd) %in% tolower(files)

  ifelse(is_cache, has_rmd, FALSE)
}

enforceBundleLimits <- function(appDir, totalFiles, totalSize) {
  maxSize <- getOption("rsconnect.max.bundle.size")
  maxFiles <- getOption("rsconnect.max.bundle.files")

  if (totalSize > maxSize) {
    cli::cli_abort(c(
      "{.arg appDir} ({.path {appDir}}) is too large to be deployed.",
      x = "The maximum size is {maxSize} bytes.",
      x = "This directory is at least {totalSize} bytes.",
      i = "Remove some files or adjust the rsconnect.max.bundle.size option."
    ))
  }

  if (totalFiles > maxFiles) {
    cli::cli_abort(c(
      "{.arg appDir} ({.path {appDir}}) is too large to be deployed.",
      x = "The maximum number of files is {maxFiles}.",
      x = "This directory contains at least {totalFiles} files.",
      i = "Remove some files or adjust the rsconnect.max.bundle.files option."
    ))
  }
}

# Scan the bundle directory looking for long user/group names.
#
# Warn that the internal tar implementation may produce invalid archives.
# https://github.com/rstudio/rsconnect/issues/446
# https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17871
detectLongNames <- function(bundleDir, lengthLimit = 32) {
  files <- list.files(
    bundleDir,
    recursive = TRUE,
    all.files = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )

  info <- file.info(file.path(bundleDir, files))
  ok <- (is.na(info$uname) | nchar(info$uname) <= lengthLimit) &
    (is.na(info$grname) | nchar(info$grname) <= lengthLimit)

  if (all(ok)) {
    return(invisible(FALSE))
  }

  bad_files <- files[!ok]

  cli::cli_warn(
    c(
      "The bundle contains files with user/group names longer than {lengthLimit}.",
      x = "Files: {.path {bad_files}}",
      x = "Long user and group names cause the internal R tar implementation to produce invalid archives",
      i = "Set the {.code rsconnect.tar} option or the {.code RSCONNECT_TAR} environment variable to the path to a tar executable."
    )
  )
  return(invisible(FALSE))
}
