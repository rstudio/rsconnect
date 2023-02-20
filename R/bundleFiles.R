# build the list of files to deploy -- implicitly (directory contents),
# explicitly via list, or explicitly via manifest. Always returns paths
# related to `appDir`
standardizeAppFiles <- function(appDir,
                                appFiles = NULL,
                                appFileManifest = NULL,
                                error_call = caller_env()) {

  check_exclusive(appFiles, appFileManifest, .require = FALSE, .call = error_call)

  no_content <- function(message) {
    cli::cli_abort(
      c(
        "No content to deploy.",
        x = message
      ),
      call = error_call,
      .frame = caller_env()
    )
  }

  if (is.null(appFiles) && is.null(appFileManifest)) {
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
  exploded <- c()
  totalSize <- 0
  totalFiles <- 0
  for (f in files) {
    target <- file.path(dir, f)
    info <- file.info(target)
    if (is.na(info$isdir)) {
      # don't return this file; it doesn't appear to exist
      next
    } else if (isTRUE(info$isdir)) {
      # a directory; explode it
      contents <- list.files(target, full.names = FALSE, recursive = TRUE,
                             include.dirs = FALSE)
      contentPaths <- file.path(f, contents)
      contentInfos <- file.info(contentPaths)

      totalSize <- totalSize + sum(contentInfos$size, na.rm = TRUE)
      totalFiles <- totalFiles + length(contentPaths)

      exploded <- c(exploded, contentPaths)
    } else {
      # not a directory; an ordinary file

      ourSize <- if (is.na(info$size)) { 0 } else { info$size }
      totalSize <- totalSize + ourSize
      totalFiles <- totalFiles + 1

      exploded <- c(exploded, f)
    }
    # Limits are being enforced after processing each entry on the
    # input. This means that an input directory needs to be fully
    # enumerated before issuing an error. This is different from the
    # approach by bundleFiles, which enforces limits while walking
    # directories.
    enforceBundleLimits(dir, totalSize, totalFiles)
  }
  exploded
}

bundleFiles <- function(appDir) {
  files <- listBundleFiles(appDir)
  enforceBundleLimits(appDir, files$totalSize, length(files$contents))
  files$contents
}



# dir is the path for this step on our recursive walk.
# depth is tracks the number of directories we have descended. depth==0 at the root.
# totalSize is a running total of our encountered file sizes.
# totalFiles is a running count of our encountered files.
maxDirectoryList <- function(dir, depth, totalFiles, totalSize) {
  # generate a list of files at this level
  contents <- list.files(dir, recursive = FALSE, all.files = TRUE,
                         include.dirs = TRUE, no.. = TRUE, full.names = FALSE)

  # At the root, some well-known files and directories are not included in the bundle.
  if (depth == 0) {
    contents <- contents[!grepl(glob2rx("*.Rproj"), contents)]
    contents <- setdiff(contents, c(
                                      ".DS_Store",
                                      ".gitignore",
                                      ".Rhistory",
                                      "manifest.json",
                                      "rsconnect",
                                      "packrat",
                                      "app_cache",
                                      ".svn",
                                      ".git",
                                      ".quarto",
                                      ".Rproj.user"
                                  ))
  }

  # exclude renv files
  contents <- setdiff(contents, c("renv", "renv.lock"))


  # checks for .rscignore file and excludes the files and directories listed
  if (".rscignore" %in% contents) {
    ignoreContents <- readLines(".rscignore")
    contents <- setdiff(
      x = contents,
      y = ignoreContents
    )
  }

  # subdirContents contains all files encountered beneath this directory.
  # Returned paths are relative to this directory.
  subdirContents <- NULL

  # Info for each file lets us know to recurse (directories) or aggregate (files).
  infos <- file.info(file.path(dir, contents), extra_cols = FALSE)
  row.names(infos) <- contents

  for (name in contents) {
    info <- infos[name, ]

    if (isTRUE(info$isdir)) {
      # Directories do not include their self-size in our counts.

      # ignore knitr _cache directories
      if (isKnitrCacheDir(name, contents)) {
        next
      }

      # Recursively enumerate this directory.
      dirList <- maxDirectoryList(file.path(dir, name), depth + 1, totalFiles, totalSize)

      # Inherit the running totals from our child.
      totalSize <- dirList$totalSize
      totalFiles <- dirList$totalFiles

      # Directories are not included, only their files.
      subdirContents <- append(subdirContents, file.path(name, dirList$contents))

    } else {
      # This is a file. It counts and is included in our listing.
      if (is.na(info$isdir)) {
        cat(sprintf("File information for %s is not available; listing as a normal file.\n",
                    file.path(dir, name)))
      }

      ourSize <- if (is.na(info$size)) { 0 } else { info$size }
      totalSize <- totalSize + ourSize
      totalFiles <- totalFiles + 1
      subdirContents <- append(subdirContents, name)
    }

    # abort if we've reached the maximum size
    if (totalSize > getOption("rsconnect.max.bundle.size"))
      break

    # abort if we've reached the maximum number of files
    if (totalFiles > getOption("rsconnect.max.bundle.files"))
      break
  }

  # totalSize - incoming size summed with all file sizes beneath this directory.
  # totalFiles - incoming count summed with file count beneath this directory.
  # contents - all files beneath this directory; paths relative to this directory.
  list(
      totalSize = totalSize,
      totalFiles = totalFiles,
      contents = subdirContents
  )
}


isKnitrCacheDir <- function(subdir, contents) {
  if (grepl("^.+_cache$", subdir)) {
    stem <- substr(subdir, 1, nchar(subdir) - nchar("_cache"))
    rmd <- paste0(stem, ".Rmd")
    tolower(rmd) %in% tolower(contents)
  } else {
    FALSE
  }
}


#' List Files to be Bundled
#'
#' Given a directory containing an application, returns the names of the files
#' to be bundled in the application.
#'
#' @param appDir Directory containing the application.
#'
#' @details This function computes results similar to a recursive directory
#' listing from [list.files()], with the following constraints:
#'
#' \enumerate{
#' \item{If the total size of the files exceeds the maximum bundle size, no
#'    more files are listed. The maximum bundle size is controlled by the
#'    `rsconnect.max.bundle.size` option.}
#' \item{If the total size number of files exceeds the maximum number to be
#'    bundled, no more files are listed. The maximum number of files in the
#'    bundle is controlled by the `rsconnect.max.bundle.files` option.}
#' \item{Certain files and folders that don't need to be bundled, such as
#'    those containing internal version control and RStudio state, are
#'    excluded.}
#' \item{In order to stop specific files in the working directory from being
#'    listed in the bundle, the files must be listed in the .rscignore file.
#'    This file must have one file or directory per line with no support for
#'    wildcards.}
#' }
#'
#' @return Returns a list containing the following elements:
#'
#' \tabular{ll}{
#' `contents` \tab A list of the files to be bundled \cr
#' `totalSize` \tab The total size of the files \cr
#' }
#'
#' @export
listBundleFiles <- function(appDir) {
  maxDirectoryList(appDir, 0, 0, 0)
}


enforceBundleLimits <- function(appDir, totalSize, totalFiles) {
  if (totalSize > getOption("rsconnect.max.bundle.size")) {
    stop("The directory ", appDir, " cannot be deployed because it is too ",
         "large (the maximum size is ", getOption("rsconnect.max.bundle.size"),
         " bytes). Remove some files or adjust the rsconnect.max.bundle.size ",
         "option.", call. = FALSE)
  } else if (totalFiles > getOption("rsconnect.max.bundle.files")) {
    stop("The directory ", appDir, " cannot be deployed because it contains ",
         "too many files (the maximum number of files is ",
         getOption("rsconnect.max.bundle.files"), "). Remove some files or ",
         "adjust the rsconnect.max.bundle.files option.", call. = TRUE)
  }
}

# Scan the bundle directory looking for long user/group names.
#
# Warn that the internal tar implementation may produce invalid archives.
# https://github.com/rstudio/rsconnect/issues/446
# https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17871
detectLongNames <- function(bundleDir, lengthLimit = 32) {
  files <- list.files(bundleDir, recursive = TRUE, all.files = TRUE,
                      include.dirs = TRUE, no.. = TRUE, full.names = FALSE)
  for (f in files) {
    info <- file.info(file.path(bundleDir, f))


    if (longerThan(info$uname, lengthLimit) || longerThan(info$grname, lengthLimit)) {
      warning("The bundle contains files with user/group names having more than ", lengthLimit,
              " characters: ", f, " is owned by ", info$uname, ":", info$grname, ". ",
              "Long user and group names cause the internal R tar implementation to produce invalid archives. ",
              "Set the rsconnect.tar option or the RSCONNECT_TAR environment variable to the path to ",
              "a tar executable to use that implementation.")
      return(invisible(TRUE))
    }
  }
  return(invisible(FALSE))
}

# uname/grname is not always available.
# https://github.com/wch/r-source/blob/8cf68878a1361d00ff2125db2e1ac7dc8f6c8009/src/library/utils/R/tar.R#L539-L549
longerThan <- function(s, lim) {
  if (!is.null(s) && !is.na(s)) {
    return(nchar(s) > lim)
  }
  return(FALSE)
}
