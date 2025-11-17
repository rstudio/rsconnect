# Gather files to be bundled with an app

Given an app directory, and optional `appFiles` and `appFileManifest`
arguments, returns vector of paths to bundle in the app. (Note that
documents follow a different strategy; see
[`deployDoc()`](https://rstudio.github.io/rsconnect/dev/reference/deployDoc.md)
for details.)

When neither `appFiles` nor `appFileManifest` is supplied,
`listDeploymentFiles()` will include all files under `appDir`, apart
from the following:

- Certain files and folders that don't need to be bundled, such as
  version control directories, internal config files, and RStudio state,
  are automatically excluded.

- You can exclude additional files by listing them in in a `.rscignore`
  file. This file must have one file or directory per line (with path
  relative to the current directory). It doesn't support wildcards, or
  ignoring files in subdirectories.

`listDeploymentFiles()` will throw an error if the total file size
exceeds the maximum bundle size (as controlled by option
`rsconnect.max.bundle.size`), or the number of files exceeds the maximum
file limit (as controlled by option `rsconnect.max.bundle.files`). This
prevents you from accidentally bundling a very large direcfory (i.e. you
home directory).

Supported servers: All servers

## Usage

``` r
listDeploymentFiles(
  appDir,
  appFiles = NULL,
  appFileManifest = NULL,
  error_call = caller_env()
)
```

## Arguments

- appDir:

  A directory containing an application (e.g. a Shiny app or plumber
  API). Defaults to the current directory.

- appFiles, appFileManifest:

  Use `appFiles` to specify a character vector of files to bundle in the
  app or `appFileManifest` to provide a path to a file containing a list
  of such files. If neither are supplied, will bundle all files in
  `appDir`, apart from standard exclusions and files listed in a
  `.rscignore` file. See `listDeploymentFiles()` for more details.

- error_call:

  The call or environment for error reporting; expert use only.

## Value

Character of paths to bundle, relative to `appDir`.
