# Deploy an Application

Deploy a [shiny](https://rdrr.io/pkg/shiny/man/shiny-package.html)
application, an
[RMarkdown](https://pkgs.rstudio.com/rmarkdown/reference/rmarkdown-package.html)
document, a plumber API, or HTML content to a server.

Supported servers: All servers

## Usage

``` r
deployApp(
  appDir = getwd(),
  appFiles = NULL,
  appFileManifest = NULL,
  appPrimaryDoc = NULL,
  appSourceDoc = NULL,
  appName = NULL,
  appTitle = NULL,
  envVars = NULL,
  appId = NULL,
  appMode = NULL,
  contentCategory = NULL,
  account = NULL,
  server = NULL,
  upload = TRUE,
  recordDir = NULL,
  launch.browser = getOption("rsconnect.launch.browser", is_interactive()),
  on.failure = NULL,
  logLevel = c("normal", "quiet", "verbose"),
  lint = TRUE,
  metadata = list(),
  forceUpdate = NULL,
  python = NULL,
  forceGeneratePythonEnvironment = FALSE,
  quarto = NA,
  appVisibility = NULL,
  image = NULL,
  envManagement = NULL,
  envManagementR = NULL,
  envManagementPy = NULL
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
  [`listDeploymentFiles()`](https://rstudio.github.io/rsconnect/reference/listDeploymentFiles.md)
  for more details.

- appPrimaryDoc:

  If the application contains more than one document, this parameter
  indicates the primary one, as a path relative to `appDir`. Can be
  `NULL`, in which case the primary document is inferred from the
  contents being deployed.

- appSourceDoc:

  **\[deprecated\]** Please use `recordDir` instead.

- appName:

  Application name, a string consisting of letters, numbers, `_` and
  `-`. The application name is used to identify applications on a
  server, so must be unique.

  If not specified, the first deployment will be automatically it from
  the `appDir` for directory and website, and from the `appPrimaryDoc`
  for document. On subsequent deploys, it will use the previously stored
  value.

- appTitle:

  Free-form descriptive title of application. Optional; if supplied,
  will often be displayed in favor of the name. If ommitted, on second
  and subsequent deploys, the title will be unchanged.

- envVars:

  A character vector giving the names of environment variables whose
  values should be synchronised with the server (currently supported by
  Connect only). The values of the environment variables are sent over
  an encrypted connection and are not stored in the bundle, making this
  a safe way to send private data to Connect.

  The values of sensitive environment variables should be set in the
  current session via an `.Renviron` file or with the help of a
  credential store like [keyring](https://keyring.r-lib.org/). Avoid
  using [`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html) for
  sensitive values, as that results in the value appearing in your
  `.Rhistory`.

  The names (not values) are stored in the deployment record so that
  future deployments will automatically update their values. Other
  environment variables on the server will not be affected. This means
  that removing an environment variable from `envVars` will leave it
  unchanged on the server. To remove it, either delete it using the
  Connect UI, or temporarily unset it (with
  [`Sys.unsetenv()`](https://rdrr.io/r/base/Sys.setenv.html) or similar)
  then re-deploy.

  Environment variables are set prior to deployment so that your code
  can use them and the first deployment can still succeed. Note that
  means that if the deployment fails, the values will still be updated.

- appId:

  Use this to deploy to an exact known application, ignoring all
  existing deployment records and `appName`.

  You can use this to update an existing application that is missing a
  deployment record. If you're re-deploying an application that you
  created it's generally easier to use `appName`; `appId` is best
  reserved for re-deploying apps created by someone else.

  You can find the `appId` in the following places:

  - For Posit Connect, it's `guid` from the info tab on the content
    page.

  - For Posit Connect Cloud, it can be found in the content admin page's
    URL `https://connect.posit.cloud/{accountName}/content/{appId}`).

  - On shinyapps.io, it's the `id` listed on the applications page.

- appMode:

  Optional; the type of content being deployed. Provide this option when
  the inferred type of content is incorrect. This can happen, for
  example, when static HTML content includes a downloadable Shiny
  application `app.R`. Accepted values include `"shiny"`, `"api"`,
  `"rmd-static"`, `"rmd-shiny"`, `"quarto-static"`, `"quarto-shiny"`,
  and `"static"`. The Posit Connect API Reference contains a full set of
  available values. Not all servers support all types of content.

- contentCategory:

  Optional; classifies the kind of content being deployed (e.g. `"plot"`
  or `"site"`).

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

  Use
  [`accounts()`](https://rstudio.github.io/rsconnect/reference/accounts.md)
  to see the full list of available options.

- upload:

  If `TRUE` (the default) then the application is uploaded from the
  local system prior to deployment. If `FALSE` then it is re-deployed
  using the last version that was uploaded. `FALSE` is only supported on
  Posit Connect Cloud and shinyapps.io; `TRUE` is required on Posit
  Connect.

- recordDir:

  Directory where deployment record is written. The default, `NULL`,
  uses `appDir`, since this is usually where you want the deployment
  data to be stored. This argument is typically only needed when
  deploying a directory of static files since you want to store the
  record with the code that generated those files, not the files
  themselves.

- launch.browser:

  If true, the system's default web browser will be launched
  automatically after the app is started. Defaults to `TRUE` in
  interactive sessions only. If a function is passed, it will be called
  after the app is started, with the app URL as a paramter.

- on.failure:

  Function to be called if the deployment fails. If a deployment log URL
  is available, it's passed as a parameter.

- logLevel:

  One of `"quiet"`, `"normal"` or `"verbose"`; indicates how much
  logging to the console is to be performed. At `"quiet"` reports no
  information; at `"verbose"`, a full diagnostic log is captured.

- lint:

  Lint the project before initiating deployment, to identify potentially
  problematic code?

- metadata:

  Additional metadata fields to save with the deployment record. These
  fields will be returned on subsequent calls to
  [`deployments()`](https://rstudio.github.io/rsconnect/reference/deployments.md).

  Multi-value fields are recorded as comma-separated values and returned
  in that form. Custom value serialization is the responsibility of the
  caller.

- forceUpdate:

  What should happen if there's no deployment record for the app, but
  there's an app with the same name on the server? If `TRUE`, will
  always update the previously-deployed app. If `FALSE`, will ask the
  user what to do, or fail if not in an interactive context.

  Defaults to `TRUE` when called automatically by the IDE, and `FALSE`
  otherwise. You can override the default by setting option
  `rsconnect.force.update.apps`.

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

- appVisibility:

  One of `NULL`, `"private"`, or `"public"`; the visibility of the
  deployment. When `NULL`, no change to visibility is made. Currently
  has an effect only on deployments to shinyapps.io.

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

## Details

### Deployment records

When deploying an app, `deployApp()` will save a deployment record that
makes it easy to update the app on server from your local source code.
This generally means that you need to only need to supply important
arguments (e.g. `appName`, `appTitle`, `server`/`account`) on the first
deploy, and rsconnect will reuse the same settings on subsequent
deploys.

The metadata needs to make this work is stored in `{appDir}/rsconnect/`.
You should generally check these files into version control to ensure
that future you and other collaborators will publish to the same
location.

If you have lost this directory, all is not lost, as `deployApp()` will
attempt to rediscover existing deployments. This is easiest if you are
updating an app that you created, as you can just supply the `appName`
(and `server`/`account` if you have multiple accounts) and `deployApp()`
will find the existing application account. If you need to update an app
that was created by someone else (that you have write permission) for,
you'll instead need to supply the `appId`.

## See also

[`applications()`](https://rstudio.github.io/rsconnect/reference/applications.md),
[`terminateApp()`](https://rstudio.github.io/rsconnect/reference/terminateApp.md),
and
[`restartApp()`](https://rstudio.github.io/rsconnect/reference/restartApp.md)

Other Deployment functions:
[`applications()`](https://rstudio.github.io/rsconnect/reference/applications.md),
[`deployAPI()`](https://rstudio.github.io/rsconnect/reference/deployAPI.md),
[`deployDoc()`](https://rstudio.github.io/rsconnect/reference/deployDoc.md),
[`deploySite()`](https://rstudio.github.io/rsconnect/reference/deploySite.md),
[`deployTFModel()`](https://rstudio.github.io/rsconnect/reference/deployTFModel.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# deploy the application in the current working dir
deployApp()

# deploy an application in another directory
deployApp("~/projects/shiny/app1")

# deploy using an alternative application name and title
deployApp("~/projects/shiny/app1", appName = "myapp",
          appTitle = "My Application")

# deploy specifying an explicit account name, then
# redeploy with no arguments (will automatically use
# the previously specified account)
deployApp(account = "jsmith")
deployApp()

# deploy but don't launch a browser when completed
deployApp(launch.browser = FALSE)

# deploy a Quarto website, using the quarto package to
# find the Quarto binary
deployApp("~/projects/quarto/site1")

# deploy application with environment variables
# (e.g., `SECRET_PASSWORD=XYZ` is set via an ~/.Renviron file)
rsconnect::deployApp(envVars = c("SECRET_PASSWORD"))
} # }
```
