# enforces bundle limits

    Code
      explodeFiles(dir, c("a", "b"))
    Condition
      Error in `enforceBundleLimits()`:
      ! `appDir` ('<TEMPDIR>') is too large to be deployed.
      x The maximum number of files is 1.
      x This directory containes 2 files.
      i  Remove some files or adjust the rsconnect.max.bundle.files option.
    Code
      explodeFiles(dir, "c")
    Condition
      Error in `enforceBundleLimits()`:
      ! `appDir` ('<TEMPDIR>') is too large to be deployed.
      x The maximum size is 5 bytes.
      x This directory is 52 bytes.
      i  Remove some files or adjust the rsconnect.max.bundle.size option.

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

# detectLongNames produces informative warning

    Code
      detectLongNames(dir, 0)
    Condition
      Warning:
      The bundle contains files with user/group names longer than 0.
      x Files: 'a.r', 'b.r', and 'c.r'
      x Long user and group names cause the internal R tar implementation to produce invalid archives
      i Set the `rsconnect.tar` option or the `RSCONNECT_TAR` environment variable to the path to a tar executable.

