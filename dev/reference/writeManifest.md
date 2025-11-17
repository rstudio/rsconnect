# Create a `manifest.json`

Use `writeManifest()` to generate a `manifest.json`. Among other things,
you can commit this file to git to activate [Git-Backed
content](https://docs.posit.co/connect/user/git-backed/) for Posit
Connect.

`manifest.json` contains a list of all files in the app along with their
dependencies, so you will need to re-run `writeManifest()` when either
of these change.

Supported servers: All servers

## Usage

``` r
writeManifest(
  appDir = getwd(),
  appFiles = NULL,
  appFileManifest = NULL,
  appPrimaryDoc = NULL,
  appMode = NULL,
  contentCategory = NULL,
  python = NULL,
  forceGeneratePythonEnvironment = FALSE,
  quarto = NA,
  image = NULL,
  envManagement = NULL,
  envManagementR = NULL,
  envManagementPy = NULL,
  verbose = FALSE,
  quiet = FALSE
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
  `.rscignore` file. See
  [`listDeploymentFiles()`](https://rstudio.github.io/rsconnect/dev/reference/listDeploymentFiles.md)
  for more details.

- appPrimaryDoc:

  If the application contains more than one document, this parameter
  indicates the primary one, as a path relative to `appDir`. Can be
  `NULL`, in which case the primary document is inferred from the
  contents being deployed.

- appMode:

  Optional; the type of content being deployed. Provide this option when
  the inferred type of content is incorrect. This can happen, for
  example, when static HTML content includes a downloadable Shiny
  application `app.R`. Accepted values include `"shiny"`, `"api"`,
  `"rmd-static"`, `"rmd-shiny"`, `"quarto-static"`, `"quarto-shiny"`,
  and `"static"`. The Posit Connect API Reference contains a full set of
  available values. Not all servers support all types of content.

- contentCategory:

  Set this to `"site"` if you'd deploy with
  [`deploySite()`](https://rstudio.github.io/rsconnect/dev/reference/deploySite.md);
  otherwise leave as is.

- python:

  Full path to a python binary for use by `reticulate`. Required if
  `reticulate` is a dependency of the app being deployed. If python =
  NULL, and RETICULATE_PYTHON or RETICULATE_PYTHON_FALLBACK is set in
  the environment, its value will be used. The specified python binary
  will be invoked to determine its version and to list the python
  packages installed in the environment.

- forceGeneratePythonEnvironment:

  Optional. If an existing `requirements.txt` file is found, it will be
  overwritten when this argument is `TRUE`.

- quarto:

  Should the deployed content be built by quarto? (`TRUE`, `FALSE`, or
  `NA`). The default, `NA`, will use quarto if there are `.qmd` files in
  the bundle, or if there is a `_quarto.yml` and `.Rmd` files.

  (This option is ignored and quarto will always be used if the
  `metadata` contains `quarto_version` and `quarto_engines` fields.)

- image:

  Optional. The name of the image to use when building and executing
  this content. If none is provided, Posit Connect will attempt to
  choose an image based on the content requirements. You can override
  the default by setting the environment variable `RSCONNECT_IMAGE`.

- envManagement:

  Optional. Should Posit Connect install R and Python packages for this
  content? (`TRUE`, `FALSE`, or `NULL`). The default, `NULL`, will not
  write any values to the bundle manifest, and Connect will fall back to
  the application default environment management strategy, or the server
  default if no application default is defined.

  (This option is a shorthand flag which overwrites the values of both
  `envManagementR` and `envManagementPy`.)

- envManagementR:

  Optional. Should Posit Connect install R packages for this content?
  (`TRUE`, `FALSE`, or `NULL`). The default, `NULL`, will not write any
  values to the bundle manifest, and Connect will fall back to the
  application default R environment management strategy, or the server
  default if no application default is defined.

  (This option is ignored when `envManagement` is non-`NULL`.)

- envManagementPy:

  Optional. Should Posit Connect install Python packages for this
  content? (`TRUE`, `FALSE`, or `NULL`). The default, `NULL`, will not
  write any values to the bundle manifest, and Connect will fall back to
  the application default Python environment management strategy, or the
  server default if no application default is defined.

  (This option is ignored when `envManagement` is non-`NULL`.)

- verbose:

  If `TRUE`, prints detailed progress messages.

- quiet:

  If `FALSE`, prints progress messages.
