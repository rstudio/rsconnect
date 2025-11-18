# List Files to be Bundled

**\[superseded\]**

`listBundleFiles()` has been superseded in favour of
[`listDeploymentFiles()`](https://rstudio.github.io/rsconnect/reference/listDeploymentFiles.md).

Given a directory containing an application, returns the names of the
files that by default will be bundled in the application. It works
similarly to a recursive directory listing from
[`list.files()`](https://rdrr.io/r/base/list.files.html) but enforces
bundle sizes as described in
[`listDeploymentFiles()`](https://rstudio.github.io/rsconnect/reference/listDeploymentFiles.md)

## Usage

``` r
listBundleFiles(appDir)
```

## Arguments

- appDir:

  Directory containing the application.

## Value

Returns a list containing the following elements:

- `totalFiles`: Total number of files.

- `totalSize`: Total size of the files (in bytes).

- `contents`: Paths to bundle, relative to `appDir`.
