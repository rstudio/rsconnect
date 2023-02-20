# enforces bundle limits

    Code
      explodeFiles(dir, c("a", "b"))
    Condition
      Error in `enforceBundleLimits()`:
      ! The directory <TEMPDIR> cannot be deployed because it contains too many files (the maximum number of files is 1). Remove some files or adjust the rsconnect.max.bundle.files option.
    Code
      explodeFiles(dir, "c")
    Condition
      Error:
      ! The directory <TEMPDIR> cannot be deployed because it is too large (the maximum size is 5 bytes). Remove some files or adjust the rsconnect.max.bundle.size option.

# can read all files from directory

    Code
      standardizeAppFiles(dir)
    Condition
      Error:
      ! No content to deploy.
      x `appDir` is empty.

# can read selected files from directory

    Code
      standardizeAppFiles(dir, "c.R")
    Condition
      Error:
      ! No content to deploy.
      x `appFiles` didn't match any files in `appDir`.

# can read selected files from manifest

    Code
      standardizeAppFiles(dir, appFileManifest = file.path(dir, "manifest"))
    Condition
      Error:
      ! No content to deploy.
      x `appFileManifest` contains no usable files.

# checks its inputs

    Code
      standardizeAppFiles(dir, appFiles = "a.R", appFileManifest = "b.R")
    Condition
      Error:
      ! Exactly one of `appFiles` or `appFileManifest` must be supplied.
    Code
      standardizeAppFiles(dir, appFiles = 1)
    Condition
      Error:
      ! `appFiles` must be a character vector or `NULL`, not the number 1.
    Code
      standardizeAppFiles(dir, appFileManifest = "doestexist")
    Condition
      Error:
      ! `appFileManifest`, "doestexist", does not exist.

