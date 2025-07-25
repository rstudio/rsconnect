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
listDeploymentFiles <- function(
  appDir,
  appFiles = NULL,
  appFileManifest = NULL,
  error_call = caller_env()
) {
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

recursiveBundleFiles <- function(
  dir,
  contents = NULL,
  rootDir = dir,
  totalFiles = 0,
  totalSize = 0,
  ignoreFiles = TRUE
) {
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
    "rsconnect",
    "rsconnect-python",
    "manifest.json",
    # packrat + renv,
    "renv",
    "packrat",
    # version control
    ".git",
    ".gitignore",
    ".svn",
    # R/RStudio
    ".Rhistory",
    ".Rproj.user",
    # other
    ".DS_Store",
    ".quarto",
    "app_cache",
    "__pycache__/"
  )

  contents <- setdiff(contents, ignored)
  contents <- contents[!isKnitrCacheDir(contents)]
  contents <- contents[!isPythonEnv(dir, contents)]
  contents <- contents[!grepl("^~|~$", contents)]
  contents <- contents[!grepl(glob2rx("*.Rproj"), contents)]

  # remove any files listed in .rscignore using gitignore-style patterns
  contents <- applyRscignorePatterns(contents, dir)

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

# GitIgnore-style pattern parsing for .rscignore files
# ============================================================================

#' Apply .rscignore patterns to file list with fallback
#'
#' @param contents File list to filter
#' @param dir Directory containing .rscignore file
#' @return Filtered file list
applyRscignorePatterns <- function(contents, dir) {
  # Check if .rscignore file exists in the directory
  rscignore_path <- file.path(dir, ".rscignore")
  if (!file.exists(rscignore_path)) {
    return(contents)
  }
  
  # Try new pattern-based processing
  tryCatch({
    patterns <- parseIgnoreFile(dir)
    if (length(patterns) > 0) {
      # Apply new gitignore-style patterns
      contents <- applyIgnorePatterns(contents, patterns, dir)
    }
    
    # Always exclude the .rscignore file itself
    contents <- setdiff(contents, ".rscignore")
    return(contents)
    
  }, error = function(e) {
    # Fallback to old behavior
    warning("Error processing .rscignore with new pattern system: ", e$message, call. = FALSE)
    warning("Falling back to simple pattern matching", call. = FALSE)
    
    if (file.exists(rscignore_path)) {
      tryCatch({
        ignoreContents <- readLines(rscignore_path, warn = FALSE)
        contents <- setdiff(contents, c(ignoreContents, ".rscignore"))
      }, error = function(e2) {
        warning("Error reading .rscignore file: ", e2$message, call. = FALSE)
        # Just remove .rscignore file from contents
        contents <- setdiff(contents, ".rscignore")
      })
    }
    return(contents)
  })
}

#' Parse .rscignore file into pattern objects
#'
#' @param directory_path Path to directory containing .rscignore file
#' @return List of pattern objects, or empty list if no .rscignore file
parseIgnoreFile <- function(directory_path) {
  rscignore_path <- file.path(directory_path, ".rscignore")
  if (!file.exists(rscignore_path)) {
    return(list())
  }
  
  tryCatch({
    lines <- readLines(rscignore_path, warn = FALSE)
    patterns <- list()
    
    for (line in lines) {
      pattern_obj <- parseSinglePattern(line)
      if (!is.null(pattern_obj)) {
        patterns <- append(patterns, list(pattern_obj))
      }
    }
    
    return(patterns)
  }, error = function(e) {
    warning("Error reading .rscignore file: ", e$message)
    return(list())
  })
}

#' Parse a single pattern line
#'
#' @param line Raw line from .rscignore file
#' @return Pattern object or NULL if line should be skipped
parseSinglePattern <- function(line) {
  original <- line
  line <- trimws(line)
  
  # Skip empty lines and comments
  if (nchar(line) == 0 || startsWith(line, "#")) {
    return(NULL)
  }
  
  # Handle negation
  negation <- FALSE
  if (startsWith(line, "!")) {
    negation <- TRUE
    line <- substring(line, 2)
  }
  
  # Handle directory-only patterns
  dir_only <- FALSE
  if (endsWith(line, "/")) {
    dir_only <- TRUE
    line <- substring(line, 1, nchar(line) - 1)
  }
  
  # Handle relative vs anywhere patterns
  relative <- FALSE
  if (startsWith(line, "/")) {
    relative <- TRUE
    line <- substring(line, 2)  # Remove leading /
  } else if (grepl("/", line)) {
    relative <- TRUE
  }
  
  # Validate pattern after processing
  if (nchar(line) == 0) {
    return(NULL)
  }
  
  # Handle special double-asterisk edge cases
  warning_msg <- NULL
  if (line == "**") {
    # ** alone matches everything
    line <- "*"
  } else if (grepl("\\*{3,}", line)) {
    # *** or more - matches everything but warn
    warning_msg <- paste("Pattern with multiple consecutive asterisks:", original)
    line <- "*"
  } else if (line == "**/") {
    # **/ matches all directories
    dir_only <- TRUE
    line <- "*"
  }
  
  # Issue warning if needed
  if (!is.null(warning_msg)) {
    warning(warning_msg)
  }
  
  pattern_type <- if (negation) "negation" else if (relative) "relative" else "anywhere"
  
  list(
    raw = original,
    pattern = line,
    type = pattern_type,
    dir_only = dir_only,
    negation = negation,
    relative = relative
  )
}

#' Match a file path against a pattern
#'
#' @param file_path File path relative to current directory
#' @param pattern Pattern object from parseSinglePattern
#' @param current_dir Current directory path (for file info)
#' @return TRUE if pattern matches, FALSE otherwise
matchPattern <- function(file_path, pattern, current_dir) {
  full_path <- file.path(current_dir, file_path)
  is_directory <- dir.exists(full_path)
  
  # Handle directory-only restriction
  if (pattern$dir_only && !is_directory) {
    return(FALSE)
  }
  
  # Handle simple double-asterisk patterns
  if (grepl("\\*\\*/", pattern$pattern) || grepl("/\\*\\*$", pattern$pattern)) {
    return(matchDoubleAsteriskPattern(file_path, pattern))
  }
  
  # Regular glob matching
  return(matchGlobPattern(file_path, pattern))
}

#' Match glob patterns
#'
#' @param file_path File path to match against
#' @param pattern Pattern object
#' @return TRUE if pattern matches, FALSE otherwise
matchGlobPattern <- function(file_path, pattern) {
  tryCatch({
    # Convert glob to regex
    regex_pattern <- glob2rx(pattern$pattern)
    
    # Get target string for matching
    if (pattern$relative) {
      target <- file_path  # Full relative path
    } else {
      target <- basename(file_path)  # Just the filename
    }
    
    # Perform match
    grepl(regex_pattern, target)
  }, error = function(e) {
    warning("Pattern matching error for: ", pattern$raw, " - ", e$message)
    FALSE
  })
}

#' Match simple double-asterisk patterns
#'
#' @param file_path File path to match against  
#' @param pattern Pattern object containing ** 
#' @return TRUE if pattern matches, FALSE otherwise
matchDoubleAsteriskPattern <- function(file_path, pattern) {
  pattern_str <- pattern$pattern
  
  if (startsWith(pattern_str, "**/")) {
    # Case 1: **/foo -> matches foo anywhere (equivalent to just "foo")
    sub_pattern <- substring(pattern_str, 4)  # Remove "**/""
    anywhere_pattern <- pattern
    anywhere_pattern$pattern <- sub_pattern
    anywhere_pattern$relative <- FALSE
    return(matchGlobPattern(file_path, anywhere_pattern))
    
  } else if (endsWith(pattern_str, "/**")) {
    # Case 2: abc/** -> everything under abc/ directory
    prefix <- substring(pattern_str, 1, nchar(pattern_str) - 3)  # Remove "/**"
    return(startsWith(file_path, paste0(prefix, "/")))
  }
  
  # For more complex ** patterns, fall back to basic matching for now
  return(FALSE)
}

#' Apply ignore patterns to a file list
#'
#' @param file_list List of file paths relative to current directory
#' @param patterns List of pattern objects
#' @param current_dir Current directory path
#' @return Filtered file list with ignored files removed
applyIgnorePatterns <- function(file_list, patterns, current_dir) {
  if (length(patterns) == 0) {
    return(file_list)
  }
  
  ignored_files <- character(0)
  
  # Process ignore patterns first
  ignore_patterns <- Filter(function(p) !p$negation, patterns)
  for (pattern in ignore_patterns) {
    for (file in file_list) {
      if (matchPattern(file, pattern, current_dir)) {
        ignored_files <- union(ignored_files, file)
      }
    }
  }
  
  # Process negation patterns (un-ignore)
  negation_patterns <- Filter(function(p) p$negation, patterns)
  for (pattern in negation_patterns) {
    for (file in ignored_files) {
      if (matchPattern(file, pattern, current_dir)) {
        ignored_files <- setdiff(ignored_files, file)
      }
    }
  }
  
  # Return files not in ignored set
  setdiff(file_list, ignored_files)
}
