# writeManifest: Deploying a Quarto project without Quarto info in an error

    Code
      makeManifest(appDir, appPrimaryDoc = NULL, quarto = NULL)
    Condition
      Error in `inferAppMode()`:
      ! Can't deploy Quarto content when `quarto` is `NULL`.
      i Please supply a path to a quarto binary in `quarto`.

# writeManifest: Deploying a Quarto doc without Quarto info in an error

    Code
      makeManifest(appDir, appPrimaryDoc = appPrimaryDoc, quarto = NULL)
    Condition
      Error in `inferAppMode()`:
      ! Can't deploy Quarto content when `quarto` is `NULL`.
      i Please supply a path to a quarto binary in `quarto`.

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

