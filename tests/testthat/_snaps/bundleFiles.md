# can read all files from directory

    Code
      listDeploymentFiles(dir)
    Condition
      Error:
      ! No content to deploy.
      x `appDir` is empty.

# can read selected files from directory

    Code
      out <- listDeploymentFiles(dir, c("b.R", "c.R"))
    Condition
      Warning:
      All files listed in `appFiles` must exist.
      Problems: 'c.R'

---

    Code
      listDeploymentFiles(dir, character())
    Condition
      Error:
      ! No content to deploy.
      x `appFiles` didn't match any files in `appDir`.

# can read selected files from manifest

    Code
      out <- listDeploymentFiles(dir, appFileManifest = file.path(dir, "manifest"))
    Condition
      Warning:
      All files listed in `appFileManifest` must exist.
      Problems: 'c.R'

---

    Code
      listDeploymentFiles(dir, appFileManifest = file.path(dir, "manifest"))
    Condition
      Error:
      ! No content to deploy.
      x `appFileManifest` contains no usable files.

# checks its inputs

    Code
      listDeploymentFiles(dir)
    Condition
      Error:
      ! No content to deploy.
      x `appDir` is empty.
    Code
      listDeploymentFiles(dir, appFiles = "a.R", appFileManifest = "b.R")
    Condition
      Error:
      ! Must specify at most one of `appFiles` and `appFileManifest`
    Code
      listDeploymentFiles(dir, appFiles = 1)
    Condition
      Error:
      ! `appFiles` must be a character vector or `NULL`, not the number 1.
    Code
      listDeploymentFiles(dir, appFileManifest = "doestexist")
    Condition
      Error:
      ! `appFileManifest`, "doestexist", does not exist.

# drops drops non-existent files with warning

    Code
      out <- explodeFiles(dir, c("a", "d"))
    Condition
      Warning:
      All files listed in `appFiles` must exist.
      Problems: 'd'

# generate nicely formatted messages

    Code
      explodeFiles(dir, c("a", "b"))
    Condition
      Error in `enforceBundleLimits()`:
      ! `appDir` ('<TEMPDIR>') is too large to be deployed.
      x The maximum number of files is 1.
      x This directory contains at least 2 files.
      i Remove some files or adjust the rsconnect.max.bundle.files option.
    Code
      explodeFiles(dir, "c")
    Condition
      Error in `enforceBundleLimits()`:
      ! `appDir` ('<TEMPDIR>') is too large to be deployed.
      x The maximum size is 5 bytes.
      x This directory is at least ?? bytes.
      i Remove some files or adjust the rsconnect.max.bundle.size option.

# detectLongNames produces informative warning if needed

    Code
      detectLongNames(dir, 0)
    Condition
      Warning:
      The bundle contains files with user/group names longer than 0.
      x Files: 'a.r', 'b.r', and 'c.r'
      x Long user and group names cause the internal R tar implementation to produce invalid archives
      i Set the `rsconnect.tar` option or the `RSCONNECT_TAR` environment variable to the path to a tar executable.

