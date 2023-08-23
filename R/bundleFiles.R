#' Gather files to be bundled with an app
#'
#' @description
#' Given an app directory, and optional `appFiles` and `appFileManifest`
#' arguments, returns vector of paths to bundle in the app. (Note that
#' documents follow a different strategy; see [deployDoc()] for details.)
#'
#' When neither `appFiles` nor `appFileManifest` is supplied,
#' `listDeploymentFiles()` will include all files under `appDir`, apart
#' from the following:
#'
#' *  Certain files and folders that don't need to be bundled, such as
#'    version control directories, internal config files, and RStudio state,
#'    are automatically excluded.
#'
#' *  You can exclude additional files by listing them in in a `.rscignore`
#'    file. This file must have one file or directory per line (with path
#'    relative to the current directory). It doesn't support wildcards, or
#'    ignoring files in subdirectories.
#'
#' `listDeploymentFiles()` will throw an error if the total file size exceeds
#' the maximum bundle size (as controlled by option `rsconnect.max.bundle.size`),
#' or the number of files exceeds the maximum file limit (as controlled by
#' option `rsconnect.max.bundle.files`). This prevents you from accidentally
#' bundling a very large direcfory (i.e. you home directory).
#'
#' @inheritParams deployApp
#' @param error_call The call or environment for error reporting; expert
#'   use only.
#' @return Character of paths to bundle, relative to `appDir`.
#' @export
listDeploymentFiles <- function(appDir,
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
    appFiles <- explodeFiles(appDir, appFiles, "appFiles")
    if (length(appFiles) == 0) {
      no_content("{.arg appFiles} didn't match any files in {.arg appDir}.")
    }
  } else if (!is.null(appFileManifest)) {
    check_file(appFileManifest, error_call = error_call)
    appFiles <- readFileManifest(appFileManifest)
    appFiles <- explodeFiles(appDir, appFiles, "appFileManifest")
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

#' List Files to be Bundled
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' `listBundleFiles()` has been superseded in favour of [listDeploymentFiles()].
#'
#' Given a directory containing an application, returns the names of the files
#' that by default will be bundled in the application. It works similarly to
#' a recursive directory listing from [list.files()] but enforces bundle sizes
#' as described in [listDeploymentFiles()]
#'
#' @param appDir Directory containing the application.
#' @return Returns a list containing the following elements:
#' * `totalFiles`: Total number of files.
#' * `totalSize`: Total size of the files (in bytes).
#' * `contents`: Paths to bundle, relative to `appDir`.
#' @export
#' @keywords internal
listBundleFiles <- function(appDir) {
  recursiveBundleFiles(appDir)
}

bundleFiles <- function(appDir) {
  listBundleFiles(appDir)$contents
}

explodeFiles <- function(dir, files, error_arg = "appFiles") {
  missing <- !file.exists(file.path(dir, files))
  if (any(missing)) {
    cli::cli_warn(c(
      "All files listed in {.arg {error_arg}} must exist.",
      "Problems: {.file {files[missing]}}"
    ))

    files <- files[!missing]
  }

  recursiveBundleFiles(dir, contents = files, ignoreFiles = FALSE)$contents
}

recursiveBundleFiles <- function(dir,
                                 contents = NULL,
                                 rootDir = dir,
                                 totalFiles = 0,
                                 totalSize = 0,
                                 ignoreFiles = TRUE) {
  # generate a list of files at this level
  if (is.null(contents)) {
    contents <- list.files(dir, all.files = TRUE, no.. = TRUE)
  }
  if (ignoreFiles) {
    contents <- ignoreBundleFiles(dir, contents)
  }

  # Info for each file lets us know to recurse (directories) or aggregate (files).
  is_dir <- dir.exists(file.path(dir, contents))
  names(is_dir) <- contents

  children <- character()
  for (name in contents) {
    if (isTRUE(is_dir[[name]])) {
      out <- recursiveBundleFiles(
        dir = file.path(dir, name),
        rootDir = rootDir,
        totalFiles = totalFiles,
        totalSize = totalSize,
        ignoreFiles = ignoreFiles
      )

      children <- append(children, file.path(name, out$contents))
      totalFiles <- out$totalFiles
      totalSize <- out$totalSize
    } else {
      children <- append(children, name)
      totalFiles <- totalFiles + 1
      totalSize <- totalSize + file_size(file.path(dir, name))
    }

    enforceBundleLimits(rootDir, totalFiles, totalSize)
  }

  list(
    contents = children,
    totalFiles = totalFiles,
    totalSize = totalSize
  )
}

ignoreBundleFiles <- function(dir, contents) {
  # entries ignored regardless of type
  ignored <- c(
    # rsconnect packages
    "rsconnect", "rsconnect-python", "manifest.json",
    # packrat + renv,
    "renv", "packrat",
    # version control
    ".git", ".gitignore", ".svn",
    # R/RStudio
    ".Rhistory", ".Rproj.user",
    # other
    ".DS_Store", ".quarto", "app_cache", "__pycache__/"
  )

  contents <- setdiff(contents, ignored)
  contents <- contents[!isKnitrCacheDir(contents)]
  contents <- contents[!isPythonEnv(dir, contents)]
  contents <- contents[!grepl("^~|~$", contents)]
  contents <- contents[!grepl(glob2rx("*.Rproj"), contents)]

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

# https://github.com/rstudio/rsconnect-python/blob/94dbd28797ee503d6/rsconnect/bundle.py#L541-L543
isPythonEnv <- function(dir, files) {
  (file.exists(file.path(dir, files, "bin", "python")) |
     file.exists(file.path(dir, files, "Scripts", "python.exe")) |
     file.exists(file.path(dir, files, "Scripts", "pythond.exe")) |
     file.exists(file.path(dir, files, "Scripts", "pythonw.exe")))
}

enforceBundleLimits <- function(appDir, totalFiles, totalSize) {
  maxSize <- getOption("rsconnect.max.bundle.size", 3000 * 1024^2)
  maxFiles <- getOption("rsconnect.max.bundle.files", 10000)

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
