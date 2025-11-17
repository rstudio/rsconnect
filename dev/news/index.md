# Changelog

## rsconnect (development version)

- Fix an opaque error when creating a manifest using Python \<= 3.10
  with a version requirement in a `pyproject.toml` file. A warning is
  shown rather than an error when the tomllib package is not present.
  ([\#1226](https://github.com/rstudio/rsconnect/issues/1226))

- Address CRAN test failures with some versions seen with some openssl
  configurations.
  ([\#1255](https://github.com/rstudio/rsconnect/issues/1255))

## rsconnect 1.6.1

CRAN release: 2025-11-04

- Fix account registration from RStudio.
  ([\#1250](https://github.com/rstudio/rsconnect/issues/1250))

- SPCS/Snowflake authentication supports Connect API keys for user
  identification. The
  [`connectSPCSUser()`](https://rstudio.github.io/rsconnect/dev/reference/connectSPCSUser.md)
  function now requires an `apiKey` parameter, and the API key is
  included in the `X-RSC-Authorization` header alongside Snowflake token
  authentication. This aligns with updated Connect server requirements
  where Snowflake tokens provide proxied authentication while API keys
  identify users to the Connect server itself.

## rsconnect 1.6.0

CRAN release: 2025-10-28

- Support deploying to Posit Connect Cloud. Use
  [`connectCloudUser()`](https://rstudio.github.io/rsconnect/dev/reference/connectCloudUser.md)
  to add Connect Cloud credentials.

- `rsconnect` now sets the `rsconnect.max.bundle.size` and
  `rsconnect.max.bundle.files` options to their default values on
  startup if they have not yet been set.
  ([\#1204](https://github.com/rstudio/rsconnect/issues/1204))

- The default `rsconnect.max.bundle.size` limit has increased to 5 GiB.
  ([\#1200](https://github.com/rstudio/rsconnect/issues/1200))

- [`getLogs()`](https://rstudio.github.io/rsconnect/dev/reference/showLogs.md)
  returns log lines for a shinyapps.io hosted application.
  ([\#1209](https://github.com/rstudio/rsconnect/issues/1209))

- Python environment inspection errors include the path to the target
  Python binary.
  ([\#1207](https://github.com/rstudio/rsconnect/issues/1207))

- Improve cookie expiration date handling.
  ([\#1212](https://github.com/rstudio/rsconnect/issues/1212))

- Improve documentation and advice for `deployApp(envVars...)`.

- Removed support for publishing to Posit Cloud.
  ([\#1215](https://github.com/rstudio/rsconnect/issues/1215))

  Existing Posit Cloud account records may be removed by using
  `removeAccount("yourname", "posit.cloud")`.

  Existing Posit Cloud deployment records may be removed by using
  `forgetDeployment(name="deployment", account="yourname", server="posit.cloud")`.

- Removed the Posit Cloud-exclusive `space` argument from
  [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md).
  ([\#1215](https://github.com/rstudio/rsconnect/issues/1215))

## rsconnect 1.5.1

CRAN release: 2025-08-28

- Address user registration for Posit Connect deployments hosted in
  Snowpark Container Services when there is more than one configured
  Snowflake connection.
  ([\#1189](https://github.com/rstudio/rsconnect/issues/1189))

- Process cookie expiration dates in addition to the cookie max-age.
  Some servers return already-expired cookies. (1187)

- Removed unused internal methods from Connect client.
  ([\#1182](https://github.com/rstudio/rsconnect/issues/1182))

## rsconnect 1.5.0

CRAN release: 2025-06-26

- Functions for interacting with Posit Connect deployments in Snowpark
  Container Services are now provided by the snowflakeauth package.

## rsconnect 1.4.2

CRAN release: 2025-06-18

- Address duplicate certificate errors on macOS with newer curl.
  ([\#1175](https://github.com/rstudio/rsconnect/issues/1175))

## rsconnect 1.4.1

CRAN release: 2025-05-22

- Fixed processing error during server validation, which prevented
  registration of new Connect accounts.
  ([\#1166](https://github.com/rstudio/rsconnect/issues/1166))

- When waiting for initial Connect account authorization, allow HTTP 401
  responses.
  ([\#1167](https://github.com/rstudio/rsconnect/issues/1167))

## rsconnect 1.4.0

CRAN release: 2025-05-15

- Content directories with a period in their name are no longer treated
  as a document path when computing the location for deployment records.
  ([\#1138](https://github.com/rstudio/rsconnect/issues/1138))

- Quarto documents which specify a server must include executable code
  or an engine declaration.
  ([\#1145](https://github.com/rstudio/rsconnect/issues/1145))

- Fixed errors when analyzing Quarto documents containing long chunks.
  ([\#1114](https://github.com/rstudio/rsconnect/issues/1114))

- A `_server.yml` file indicates that the content is an API.
  ([\#1144](https://github.com/rstudio/rsconnect/issues/1144))

- Expand tilde when resolving the `rsconnect.ca.bundle` option.
  ([\#1152](https://github.com/rstudio/rsconnect/issues/1152))

- Added support for interaction with Posit Connect deployments hosted in
  Snowpark Container Services.

- Introduced detection of required R interpreter version based on
  `DESCRIPTION` file and `renv.lock` file. This setting is inserted into
  the manifest as `environment.r.requires`.

- Introduced detection of required Python interpreter version based on
  project files `.python-version`, `pyproject.toml` and `setup.cfg`.
  This setting is inserted into the manifest as
  `environment.python.requires`.

## rsconnect 1.3.4

CRAN release: 2025-01-22

- Use base64 encoded test data. Addresses CRAN test failures when run
  with newer libssl.
  ([\#1130](https://github.com/rstudio/rsconnect/issues/1130))

## rsconnect 1.3.3

CRAN release: 2024-11-19

- Avoid “legacy” time zone names in tests, as they are not available by
  default in all environments. Addresses CRAN test failures.
  ([\#1115](https://github.com/rstudio/rsconnect/issues/1115))

## rsconnect 1.3.2

CRAN release: 2024-10-28

- Primary Quarto document detection only considers `.R`, `.Rmd`, and
  `.qmd` as end-of-file extensions. Previously, a file with `.R`
  elsewhere in its name, such as `.Rprofile`, was incorrectly
  considered.
  ([\#1106](https://github.com/rstudio/rsconnect/issues/1106))

- Use the repository name identified by renv when
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)
  does not enumerate the package, which occurs for archived packages.
  ([\#1110](https://github.com/rstudio/rsconnect/issues/1110))

- Remove remaining directory layout validation check.
  ([\#1102](https://github.com/rstudio/rsconnect/issues/1102))

- Use the public Connect server API endpoint `/v1/tasks/{id}` to poll
  task progress.
  ([\#1088](https://github.com/rstudio/rsconnect/issues/1088))

## rsconnect 1.3.1

CRAN release: 2024-06-04

- Skip tests when packages “foreign” and “MASS” are not available.
  ([\#1081](https://github.com/rstudio/rsconnect/issues/1081))

## rsconnect 1.3.0

CRAN release: 2024-05-24

- `deployApp(logLevel = "quiet")` suppresses Posit Connect deployment
  task output.
  ([\#1051](https://github.com/rstudio/rsconnect/issues/1051))

- `deployApp(logLevel = "quiet")` and `writeManifest(quiet=TRUE)`
  suppress output when using renv to analyze dependencies.
  ([\#1051](https://github.com/rstudio/rsconnect/issues/1051))

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  and
  [`writeManifest()`](https://rstudio.github.io/rsconnect/dev/reference/writeManifest.md)
  receive the default value for the `image` argument from the
  `RSCONNECT_IMAGE` environment variable.
  ([\#1063](https://github.com/rstudio/rsconnect/issues/1063))

- `deployTF()` can deploy a TensorFlow model to Posit Connect. Requires
  Posit Connect 2024.05.0 or higher.

- Skip tests when suggested packages are not available. Skip Quarto
  tests when run by CRAN.
  ([\#1074](https://github.com/rstudio/rsconnect/issues/1074))

## rsconnect 1.2.2

CRAN release: 2024-04-04

- Use internally computed SHA1 sums and PKI signing when SHA1 is
  disabled in FIPS mode.
  ([\#768](https://github.com/rstudio/rsconnect/issues/768),
  [\#1054](https://github.com/rstudio/rsconnect/issues/1054))

- Allow Quarto content with a rendered script as its primary target.
  ([\#1055](https://github.com/rstudio/rsconnect/issues/1055))

## rsconnect 1.2.1

CRAN release: 2024-01-31

- Restore the `LC_TIME` locale after computing an RFC-2616 date.
  ([\#1035](https://github.com/rstudio/rsconnect/issues/1035))
- Address a problem inspecting Quarto content when the file names and
  paths needed to be quoted. The resulting manifest lacked information
  about the Quarto runtime, which caused difficult-to-understand
  deployment errors.
  ([\#1037](https://github.com/rstudio/rsconnect/issues/1037))
- Produce an error when Quarto content cannot be inspected.
  ([\#1032](https://github.com/rstudio/rsconnect/issues/1032))

## rsconnect 1.2.0

CRAN release: 2023-12-15

- Addressed a number of republishing and collaboration issues where the
  content was incorrectly published to a new location rather than
  reusing an existing deployment.
  ([\#981](https://github.com/rstudio/rsconnect/issues/981),
  [\#1007](https://github.com/rstudio/rsconnect/issues/1007),
  [\#1013](https://github.com/rstudio/rsconnect/issues/1013),
  [\#1019](https://github.com/rstudio/rsconnect/issues/1019))

- [`showLogs()`](https://rstudio.github.io/rsconnect/dev/reference/showLogs.md),
  [`configureApp()`](https://rstudio.github.io/rsconnect/dev/reference/configureApp.md),
  [`setProperty()`](https://rstudio.github.io/rsconnect/dev/reference/setProperty.md),
  and
  [`unsetProperty()`](https://rstudio.github.io/rsconnect/dev/reference/unsetProperty.md)
  search for the application by name when there are no matching
  deployment records.
  ([\#985](https://github.com/rstudio/rsconnect/issues/985),
  [\#989](https://github.com/rstudio/rsconnect/issues/989))

- [`rpubsUpload()`](https://rstudio.github.io/rsconnect/dev/reference/rpubsUpload.md)
  correctly records the initial RPubs destination, allowing
  republishing.
  ([\#976](https://github.com/rstudio/rsconnect/issues/976))

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  and friends record multi-value `metadata` entries as comma-separated
  values. ([\#1017](https://github.com/rstudio/rsconnect/issues/1017))

- [`accountInfo()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md)
  includes `name` and `username` fields. Older versions of rsconnect
  store account records with a `username` field. Recent rsconnect
  versions record `name`. Both `name` and `username` should contain the
  same value.
  ([\#1024](https://github.com/rstudio/rsconnect/issues/1024))

## rsconnect 1.1.1

CRAN release: 2023-10-04

- Added `space` parameter to deploy directly to a space in Posit Cloud.

- Improve reporting of errors returned by shinyapps.io.
  ([\#997](https://github.com/rstudio/rsconnect/issues/997))

- Remove most directory layout validation checks.
  ([\#998](https://github.com/rstudio/rsconnect/issues/998))

- Do not use `getOption("available_packages_filters")` option when
  calling
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html).
  ([\#1002](https://github.com/rstudio/rsconnect/issues/1002))

- Packages installed from source within an renv project are not
  associated with repositories.
  ([\#1004](https://github.com/rstudio/rsconnect/issues/1004))

## rsconnect 1.1.0

CRAN release: 2023-09-05

- Fixed analysis of directories that were smaller than the
  `rsconnect.max.bundle.files=10000` limit but larger than the
  `renv.config.dependencies.limit=1000` limit.
  ([\#968](https://github.com/rstudio/rsconnect/issues/968))

- Ignore `.env`, `.venv`, and `venv` files only when they reference
  Python virtual environments.
  ([\#972](https://github.com/rstudio/rsconnect/issues/972))

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  and
  [`writeManifest()`](https://rstudio.github.io/rsconnect/dev/reference/writeManifest.md)
  accept optional `envManagement`, `envManagementR`, and
  `envManagementPy` arguments. These args specify whether Posit Connect
  should install packages in the package cache. If `envManagement` is
  `FALSE` then Connect will not perform any package installation and it
  is the administrator’s responsibility to ensure the required R/Python
  packages are available in the runtime environment. This is especially
  useful if off-host execution is enabled, when the execution
  environment (specified by the `image` argument) already contains the
  required packages. These values are ignored when
  `Applications.ManifestEnvironmentManagementSelection = false`.
  Requires Posit Connect `>=2023.07.0`.
  ([\#977](https://github.com/rstudio/rsconnect/issues/977))

- Fix account discovery by
  [`showProperties()`](https://rstudio.github.io/rsconnect/dev/reference/showProperties.md).
  ([\#980](https://github.com/rstudio/rsconnect/issues/980))

## rsconnect 1.0.2

CRAN release: 2023-08-17

- Fixed redeployments to shinyapps.io where `appName` is provided, but
  no local record of the deployment exists.
  ([\#932](https://github.com/rstudio/rsconnect/issues/932))

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  and
  [`writeManifest()`](https://rstudio.github.io/rsconnect/dev/reference/writeManifest.md)
  now error if your library and `renv.lock` are out-of-sync. Previously
  it always used what was defined in the `renv.lock` but that was (a)
  slow and (b) could lead to different results than what you see when
  running locally
  ([\#930](https://github.com/rstudio/rsconnect/issues/930)).

- Deploying from an renv project includes the `renv.lock` in the bundle.
  A manifest created for an renv project references the `renv.lock` in
  the `manifest.json`.
  ([\#926](https://github.com/rstudio/rsconnect/issues/926))

- Use the environment variable `RSCONNECT_PACKRAT` to analyze
  dependencies using packrat, as was done prior to rsconnect-1.0.0. Use
  of the `rsconnect.packrat` option is discouraged, as it is not
  effective when using push-button deployment in the RStudio IDE.
  ([\#935](https://github.com/rstudio/rsconnect/issues/935))

- The `renv.lock` is ignored when the `RSCONNECT_PACKRAT` environment
  variable or the `rsconnect.packrat` option is set.
  ([\#936](https://github.com/rstudio/rsconnect/issues/936))

- The content type is inferred by analyzing the set of top-level files.
  ([\#942](https://github.com/rstudio/rsconnect/issues/942))

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  and
  [`writeManifest()`](https://rstudio.github.io/rsconnect/dev/reference/writeManifest.md)
  accept an optional `appMode` argument. Provide this argument if your
  project includes auxiliary files which mislead the existing `appMode`
  inference. For example, if an HTML project includes a downloadable
  Shiny `app.R`, that content will be assumed to be a Shiny application
  even if that application is not meant to be run.
  ([\#948](https://github.com/rstudio/rsconnect/issues/948))

- [`appDependencies()`](https://rstudio.github.io/rsconnect/dev/reference/appDependencies.md)
  accepts an `appFileManifest` argument as an alternate way of providing
  the target set of files.

## rsconnect 1.0.1

CRAN release: 2023-07-20

- [`deployDoc()`](https://rstudio.github.io/rsconnect/dev/reference/deployDoc.md)
  includes `.Rprofile`, `requirements.txt` and `renv.lock` when
  deploying `.Rmd` or `.qmd`. These additional files are not included
  with rendered HTML documents.
  ([\#919](https://github.com/rstudio/rsconnect/issues/919))

- Explicit renv dependencies are preserved.
  ([\#916](https://github.com/rstudio/rsconnect/issues/916))

## rsconnect 1.0.0

CRAN release: 2023-07-17

### New features

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  and
  [`deployDoc()`](https://rstudio.github.io/rsconnect/dev/reference/deployDoc.md)
  now support deploying static content to Posit Cloud. Static RMarkdown
  and Quarto content can be rendered server-side.

- rsconnect requires renv 1.0.0.

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  and
  [`writeManifest()`](https://rstudio.github.io/rsconnect/dev/reference/writeManifest.md)
  now respect renv lock files, if present. If you don’t want to use
  these lockfiles, and instead return the previous behaviour of
  snapshotting on every deploy, add your `renv.lock` to `.rscignore`
  ([\#671](https://github.com/rstudio/rsconnect/issues/671)). Learn more
  `?appDependencies()`.

  Additionally,
  [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  and
  [`writeManifest()`](https://rstudio.github.io/rsconnect/dev/reference/writeManifest.md)
  now use renv to capture app dependencies, rather than packrat. If this
  causes a previously working deploy to fail, please file an issue then
  set `options(rsconnect.packrat = TRUE)` to revert to the previous
  behaviour.

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)’s
  `quarto` argument now takes values `TRUE`, `FALSE` or `NA`. The
  previous value (a path to a quarto binary) is now ignored, and instead
  we automatically figure out the package from `QUARTO_PATH` and `PATH`
  env vars ([\#658](https://github.com/rstudio/rsconnect/issues/658)).
  [`deploySite()`](https://rstudio.github.io/rsconnect/dev/reference/deploySite.md)
  now supports quarto websites
  ([\#813](https://github.com/rstudio/rsconnect/issues/813)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  gains a new `envVars` argument which takes a vector of the names of
  environment variables that should be securely copied to the server.
  The names (not values) of these environment variables are also saved
  in the deployment record and will be updated each time you re-deploy
  the app ([\#667](https://github.com/rstudio/rsconnect/issues/667)).
  This currently only works with Connect, but we hope to add support to
  Posit cloud and shinyapps.io in the future.

- rsconnect gains two new functions for understanding and updating the
  environment variables that your apps currently use.
  `listServerEnvVars()` will return a data frame of applications, with a
  `envVars` list-column giving the names of the environment variables
  used by each application. `updateServerEnvVars()` will update all
  applications that use a specific environment variable with the current
  value of that environment variable
  ([\#667](https://github.com/rstudio/rsconnect/issues/667)).

### Lifecycle changes

- Non-libcurl `rsconnect.http` options have been deprecated. This allows
  us to focus our efforts on a single backend, rather than spreading
  development efforts across five. The old backends will remain
  available for at least 2 years, but if you are using them because
  libcurl doesn’t work for you, please report the problem ASAP so we can
  fix it.

- [`addConnectServer()`](https://rstudio.github.io/rsconnect/dev/reference/addConnectServer.md)
  has been deprecated because it does the same thing as
  [`addServer()`](https://rstudio.github.io/rsconnect/dev/reference/addServer.md)
  now that
  [`addServer()`](https://rstudio.github.io/rsconnect/dev/reference/addServer.md)
  also validates URLs.

- [`deployTFModel()`](https://rstudio.github.io/rsconnect/dev/reference/deployTFModel.md)
  is defunct. Posit Connect no longer supports hosting of TensorFlow
  Model APIs. A TensorFlow model can be deployed as a [Plumber
  API](https://tensorflow.rstudio.com/guides/deploy/plumber.html),
  [Shiny
  application](https://tensorflow.rstudio.com/guides/deploy/shiny), or
  other supported content type.

- `discoverServer()` has been deprecated; it never worked.

- `deployApp("foo.Rmd")` has been deprecated. It was never documented,
  and it does the same job as
  [`deployDoc()`](https://rstudio.github.io/rsconnect/dev/reference/deployDoc.md)
  ([\#698](https://github.com/rstudio/rsconnect/issues/698)).

### Minor improvements and bug fixes

- New `rsconnect.http.headers` and `rsconnect.http.cookies` allow you to
  set extra arbitrary additional headers/cookies on each request
  ([\#405](https://github.com/rstudio/rsconnect/issues/405)). Their use
  is documented in the new
  [`vignette("custom-http")`](https://rstudio.github.io/rsconnect/dev/articles/custom-http.md).

- Uploading large files to RPubs works once more
  ([\#450](https://github.com/rstudio/rsconnect/issues/450)).

- When recording details about deployments to Posit Cloud, appId now
  represents the content id (as seen in URLs of the format
  `https://posit.cloud/content/{id}`) instead of the application id.

- Deployment records no longer contain the time the app was deployed
  (`when`) or when it’s metadata was last synced (`lastSyncTime`) as
  these variables are not very useful, and they lead to uninteresting
  diffs if you have committed the deployment records to git
  ([\#770](https://github.com/rstudio/rsconnect/issues/770)). A
  `version` field has been added to deployment DCF files to facilitate
  future file format changes, if needed. Its value for this release is
  `1`.,

- [`accounts()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md)
  returns a zero-row data frame if no accounts are registered.

- [`accountInfo()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md)
  and
  [`removeAccount()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md)
  no longer require `account` be supplied
  ([\#666](https://github.com/rstudio/rsconnect/issues/666)).

- [`accountInfo()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md)
  and
  [`servers()`](https://rstudio.github.io/rsconnect/dev/reference/servers.md)
  redact sensitive information (secrets, private keys, and certificates)
  to make it hard to accidentally reveal such information in logs
  ([\#675](https://github.com/rstudio/rsconnect/issues/675)).

- [`addServer()`](https://rstudio.github.io/rsconnect/dev/reference/addServer.md)
  includes the port in the default server name, if present.

- [`appDependencies()`](https://rstudio.github.io/rsconnect/dev/reference/appDependencies.md)
  includes implicit dependencies, and returns an additional column
  giving the Repository
  ([\#670](https://github.com/rstudio/rsconnect/issues/670)). Its
  documentation contains more information about how dependency discovery
  works, and how you can control it, if needed.

- [`applications()`](https://rstudio.github.io/rsconnect/dev/reference/applications.md)
  now returns the application title, if available
  ([\#484](https://github.com/rstudio/rsconnect/issues/484)), and
  processes multiple pages of results from a Connect server
  ([\#860](https://github.com/rstudio/rsconnect/issues/860)).

- [`connectApiUser()`](https://rstudio.github.io/rsconnect/dev/reference/connectApiUser.md)
  now clearly requires an `apiKey`
  ([\#741](https://github.com/rstudio/rsconnect/issues/741)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  output has been thoroughly reviewed and tweaked. As well as general
  polish it now gives you more information about what it has discovered
  about the deployment, like the app name, account & server, and which
  files are included in the bundle
  ([\#669](https://github.com/rstudio/rsconnect/issues/669)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  is more aggressive about saving deployment data, which should make it
  less likely that you need to repeat yourself after a failed
  deployment. In particular, it now saves both before and after
  uploading the contents
  ([\#677](https://github.com/rstudio/rsconnect/issues/677)) and it
  saves when you’re updating content originally created by someone else
  ([\#270](https://github.com/rstudio/rsconnect/issues/270)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  now gives an actionable error if you attempt to set visibility of an
  app deployed to posit.cloud
  ([\#838](https://github.com/rstudio/rsconnect/issues/838)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  now uses a stricter policy for determining whether or not a locally
  installed package can be successfully installed on the deployment
  server. This means that you’re more likely to get a clean failure
  prior to deployment
  ([\#659](https://github.com/rstudio/rsconnect/issues/659)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  will now detect if you’re attempting to publish to an app that has
  been deleted and will prompt you to create a new app
  ([\#226](https://github.com/rstudio/rsconnect/issues/226)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  includes some new conveniences for large uploads including reporting
  the size of the bundle you’re uploading and showing a progress bar in
  interactive sessions
  ([\#754](https://github.com/rstudio/rsconnect/issues/754)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  now follows redirects, which should make it more robust to your server
  moving to a new url
  ([\#674](https://github.com/rstudio/rsconnect/issues/674)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  uses simpler logic for determining whether it should create a new app
  or update an existing app. Now `appName`, `account`, and `server` are
  used to find existing deployments. If none are found, it will create a
  new deployment; if one is found, it’ll be updated; if more than one
  are found, it will prompt you to disambiguate
  ([\#666](https://github.com/rstudio/rsconnect/issues/666)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  improves account resolution from `account` and `server` arguments by
  giving specific recommendations on the values that you might use in
  the case of ambiguity or lack of matches
  ([\#666](https://github.com/rstudio/rsconnect/issues/666)).
  Additionally, you’ll now receive a clear error if you accidentally
  provide something other than a string or `NULL` to these arguments.

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  now generates an interactive prompt to select `account`/`server` (if
  no previous deployments) or `appName`/`account`/`server` (if multiple
  previous deployments)
  ([\#691](https://github.com/rstudio/rsconnect/issues/691)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  now advertises which startup scripts are run at the normal `logLevel`,
  and it evaluates each script in its own environment
  ([\#542](https://github.com/rstudio/rsconnect/issues/542)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  now derives `appName` from `appDir` and `appPrimaryDoc`, never using
  the title ([\#538](https://github.com/rstudio/rsconnect/issues/538)).
  It now only simplifies the path if you are publishing to shinyapps.io,
  since its restrictions on application names are much tighter than
  those of Posit Connect.

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  will now warn if `appFiles` or `appManifestFiles` contain files that
  don’t exist, rather than silently ignoring them
  ([\#706](https://github.com/rstudio/rsconnect/issues/706)).

- [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  excludes temporary backup files (names starting or ending with `~`)
  when automatically determining files to bundle
  ([\#111](https://github.com/rstudio/rsconnect/issues/111)) as well as
  directories that are likely to be Python virtual environments
  ([\#632](https://github.com/rstudio/rsconnect/issues/632)).
  Additionally, ignore rules are always now applied to all directories;
  previously some (like `.Rproj.user` and `"manifest.json"`) were only
  applied to the root directory. It correctly handles `.rscignore` files
  (i.e. as documented)
  ([\#568](https://github.com/rstudio/rsconnect/issues/568)).

- `deployApp(appSourceDoc)` has been deprecated; it did the same job as
  `recordDir`.

- [`deployDoc()`](https://rstudio.github.io/rsconnect/dev/reference/deployDoc.md)
  includes a `.Rprofile` in the bundle, if one is found in the same
  directory as the document.

- [`lint()`](https://rstudio.github.io/rsconnect/dev/reference/lint.md)
  should have fewer false positives for path problems: the relative path
  linter has been removed
  ([\#244](https://github.com/rstudio/rsconnect/issues/244)) and the
  case-sensitive linter now only checks strings containing a `/`
  ([\#611](https://github.com/rstudio/rsconnect/issues/611)).

- New
  [`listDeploymentFiles()`](https://rstudio.github.io/rsconnect/dev/reference/listDeploymentFiles.md),
  which supsersedes
  [`listBundleFiles()`](https://rstudio.github.io/rsconnect/dev/reference/listBundleFiles.md).
  It now errors when if the bundle is either too large or contains too
  many files, rather than silently truncating as before
  ([\#684](https://github.com/rstudio/rsconnect/issues/684)).

- [`serverInfo()`](https://rstudio.github.io/rsconnect/dev/reference/servers.md)
  and
  [`removeServer()`](https://rstudio.github.io/rsconnect/dev/reference/addServer.md)
  no longer require a `server` when called interactively.

- [`showMetrics()`](https://rstudio.github.io/rsconnect/dev/reference/showMetrics.md)
  once again returns a correctly named data frame
  ([\#528](https://github.com/rstudio/rsconnect/issues/528)).

- Removed Rmd generation code (`writeRmdIndex()`) which had not worked,
  or been necessary, for quite some time
  ([\#106](https://github.com/rstudio/rsconnect/issues/106),
  [\#109](https://github.com/rstudio/rsconnect/issues/109)).

- Locale detection has been improved on windows
  ([\#233](https://github.com/rstudio/rsconnect/issues/233)).

- The `rsconnect.pre.deploy` and `rsconnect.post.deploy` hooks are now
  always called with the content directory, not sometimes the path to a
  specific file
  ([\#696](https://github.com/rstudio/rsconnect/issues/696)).

- Functions that should only interact with shinyapps.io enforce the
  server type. Updated
  [`addAuthorizedUser()`](https://rstudio.github.io/rsconnect/dev/reference/addAuthorizedUser.md),
  [`removeAuthorizedUser()`](https://rstudio.github.io/rsconnect/dev/reference/removeAuthorizedUser.md),
  [`showUsers()`](https://rstudio.github.io/rsconnect/dev/reference/showUsers.md),
  [`showInvited()`](https://rstudio.github.io/rsconnect/dev/reference/showInvited.md),
  [`resendInvitation()`](https://rstudio.github.io/rsconnect/dev/reference/resendInvitation.md),
  [`configureApp()`](https://rstudio.github.io/rsconnect/dev/reference/configureApp.md),
  [`setProperty()`](https://rstudio.github.io/rsconnect/dev/reference/setProperty.md),
  [`unsetProperty()`](https://rstudio.github.io/rsconnect/dev/reference/unsetProperty.md),
  [`purgeApp()`](https://rstudio.github.io/rsconnect/dev/reference/purgeApp.md),
  [`restartApp()`](https://rstudio.github.io/rsconnect/dev/reference/restartApp.md),
  [`terminateApp()`](https://rstudio.github.io/rsconnect/dev/reference/terminateApp.md),
  [`showUsage()`](https://rstudio.github.io/rsconnect/dev/reference/showUsage.md),
  and
  [`showMetrics()`](https://rstudio.github.io/rsconnect/dev/reference/showMetrics.md)
  ([\#863](https://github.com/rstudio/rsconnect/issues/863),
  [\#864](https://github.com/rstudio/rsconnect/issues/864)).

- When needed packages are not installed, and you’re in an interactive
  environment, rsconnect will now prompt you to install them
  ([\#665](https://github.com/rstudio/rsconnect/issues/665)).

- The confirmation prompt presented upon lint failures indicates “no” as
  its default.
  ([\#652](https://github.com/rstudio/rsconnect/issues/652))

## rsconnect 0.8.29

CRAN release: 2023-01-09

- Introduced support for publishing to Posit Cloud. This feature is
  currently in closed beta and requires access to an enabled account on
  Posit Cloud. See [Posit Cloud’s
  Announcement](https://posit.cloud/learn/whats-new#publishing) for more
  information and to request access.

- Update company and product names for rebranding to Posit.

## rsconnect 0.8.28

CRAN release: 2022-10-24

- Shiny applications and Shiny documents no longer include an implicit
  dependency on [`ragg`](https://ragg.r-lib.org) when that package is
  present in the local environment. This reverts a change introduced in
  0.8.27.

  Shiny applications should add an explicit dependency on `ragg`
  (usually with a [`library("ragg")`](https://ragg.r-lib.org) statement)
  to see it used by
  [`shiny::renderPlot`](https://rdrr.io/pkg/shiny/man/renderPlot.html)
  (via [`shiny::plotPNG`](https://rdrr.io/pkg/shiny/man/plotPNG.html)).

  The documentation for
  [`shiny::plotPNG`](https://rdrr.io/pkg/shiny/man/plotPNG.html)
  explains the use of `ragg`.
  ([\#598](https://github.com/rstudio/rsconnect/issues/598))

- Fix bug that prevented publishing or writing manifests for non-Quarto
  content when a Quarto path was provided to the `quarto` argument of
  [`writeManifest()`](https://rstudio.github.io/rsconnect/dev/reference/writeManifest.md),
  [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md),
  and related functions.

- Escape account names when performing a directory search to determine
  an appropriate server.
  ([\#620](https://github.com/rstudio/rsconnect/issues/620))

## rsconnect 0.8.27

CRAN release: 2022-07-12

- Quarto content will no longer silently deploy as R Markdown content
  when Quarto metadata is missing or cannot be gathered. Functions will
  error, requesting the path to a Quarto binary in the `quarto`
  argument. ([\#594](https://github.com/rstudio/rsconnect/issues/594))
- Fix typo for `.rscignore`.
  ([\#599](https://github.com/rstudio/rsconnect/issues/599))
- Quarto deployments specifying only an `appDir` and `quarto` binary but
  not an `appPrimaryDoc` work more consistently. A directory containing
  a `.qmd` file will deploy as Quarto content instead of failing, and a
  directory containing an `.Rmd` file will successfully deploy as Quarto
  content instead of falling back to R Markdown.
  ([\#601](https://github.com/rstudio/rsconnect/issues/601))
- If the `ragg` package is installed locally, it is now added as an
  implicit dependency to `shiny` apps since
  [`shiny::renderPlot()`](https://rdrr.io/pkg/shiny/man/renderPlot.html)
  now uses it by default (when available). This way, `shiny` apps won’t
  have to add [`library(ragg)`](https://ragg.r-lib.org) to get
  consistent (higher-quality) PNG images when deployed.
  ([\#598](https://github.com/rstudio/rsconnect/issues/598))

## rsconnect 0.8.26

CRAN release: 2022-05-31

- Add ability to resend shinyapps.io application invitations
  ([\#543](https://github.com/rstudio/rsconnect/issues/543))
- Show expiration status in shinyapps.io for invitations
  ([\#543](https://github.com/rstudio/rsconnect/issues/543))
- Allow shinyapps.io deployments to be private at creation time
  ([\#403](https://github.com/rstudio/rsconnect/issues/403))
- Update the minimum `openssl` version to 2.0.0 to enable publishing for
  users on FIPS-compliant systems without the need for API keys.
  ([\#452](https://github.com/rstudio/rsconnect/issues/452))
- Added Quarto support to `writeManifest`, which requires passing the
  absolute path to a Quarto executable to its new `quarto` parameter
- Added `quarto` parameter to `deployApp` to enable deploying Quarto
  documents and websites by supplying the path to a Quarto executable
- Added support for deploying Quarto content that uses only the
  `jupyter` runtime
- Added support for selecting a target `image` in the bundle manifest
- The `showLogs` function takes a `server` parameter.
  ([\#57](https://github.com/rstudio/rsconnect/issues/57))
- Added the `rsconnect.tar` option, which can be used to specify the
  path to a `tar` implementation instead of R’s internal implementation.
  The previous method, using the `RSCONNECT_TAR` environment variable,
  still works, but the new option will take precedence if both are set.

## rsconnect 0.8.25

CRAN release: 2021-11-19

- Use the `curl` option `-T` when uploading files to avoid out of memory
  errors with large files.
  ([\#544](https://github.com/rstudio/rsconnect/issues/544))
- The `rsconnect.max.bundle.size` and `rsconnect.max.bundle.files`
  options are enforced when processing an enumerated set of files.
  Previously, these limits were enforced only when bundling an entire
  content directory.
  ([\#542](https://github.com/rstudio/rsconnect/issues/542))
- Preserve file time stamps when copying files into the bundle staging
  directory, which then propagates into the created tar file.
  ([\#540](https://github.com/rstudio/rsconnect/issues/540))
- Configuration directories align with CRAN policy and use the location
  named by [`tools::R_user_dir`](https://rdrr.io/r/tools/userdir.html).
  Configuration created by earlier versions of this package is
  automatically migrated to the new location.
  ([\#550](https://github.com/rstudio/rsconnect/issues/550))

## rsconnect 0.8.24

CRAN release: 2021-08-05

- Added support for publishing Quarto documents and websites
- Added support for `.rscignore` file to exclude files or directories
  from publishing
  ([\#368](https://github.com/rstudio/rsconnect/issues/368))
- Fixed issue causing missing value errors when publishing content
  containing filenames with extended characters
  ([\#514](https://github.com/rstudio/rsconnect/issues/514))
- Fixed issue preventing error tracebacks from displaying
  ([\#518](https://github.com/rstudio/rsconnect/issues/518))

## rsconnect 0.8.18

CRAN release: 2021-05-24

- Fixed issue causing configuration directory to be left behind after
  `R CMD CHECK`
- Fixed incorrect subdirectory nesting when storing configuration in
  `R_USER_CONFIG_DIR`
- Added linter for different-case Markdown links
  ([\#388](https://github.com/rstudio/rsconnect/issues/388))
- Use new Packrat release on CRAN, 0.6.0
  ([\#501](https://github.com/rstudio/rsconnect/issues/501))
- Fix incorrect linter messages referring to `shiny.R` instead of
  `server.R` ([\#509](https://github.com/rstudio/rsconnect/issues/509))
- Warn, rather than err, when the repository URL for a package
  dependency cannot be validated. This allows deployment when using
  archived CRAN packages, or when using packages installed from source
  that are available on the server.
  ([\#508](https://github.com/rstudio/rsconnect/issues/508))
- Err when the app-mode cannot be inferred; seen with empty
  directories/file-sets
  ([\#512](https://github.com/rstudio/rsconnect/issues/512))
- Add `verbose` option to `writeManifest` utility
  ([\#468](https://github.com/rstudio/rsconnect/issues/468))

## rsconnect 0.8.17

CRAN release: 2021-04-09

- Fixed issue where setting `options(rsconnect.http.trace.json = TRUE)`
  could cause deployment errors with some HTTP transports
  ([\#490](https://github.com/rstudio/rsconnect/issues/490))
- Improve how large bundles (file size and count) are detected
  ([\#464](https://github.com/rstudio/rsconnect/issues/464))
- The `RSCONNECT_TAR` environment variable can be used to select the tar
  implementation used to create bundles
  ([\#446](https://github.com/rstudio/rsconnect/issues/446))
- Warn when files are owned by users or groups with long names, as this
  can cause the internal R tar implementation to produce invalid
  archives ([\#446](https://github.com/rstudio/rsconnect/issues/446))
- Add support for syncing the deployment metadata with the server
  ([\#396](https://github.com/rstudio/rsconnect/issues/396))
- Insist on ShinyApps accounts in
  [`showUsers()`](https://rstudio.github.io/rsconnect/dev/reference/showUsers.md)
  ([\#398](https://github.com/rstudio/rsconnect/issues/398))
- Improve the regex used for the browser and browseURL lints to include
  a word boundary
  ([\#400](https://github.com/rstudio/rsconnect/issues/400))
- Fixed bug where
  [`connectApiUser()`](https://rstudio.github.io/rsconnect/dev/reference/connectApiUser.md)
  did not set a user id
  ([\#407](https://github.com/rstudio/rsconnect/issues/407))
- New arguments to `deployApp` to force the generation of a Python
  environment file or a `requirements.txt` file
  ([\#409](https://github.com/rstudio/rsconnect/issues/409))
- Fail when no repository URL is available for a dependent package
  ([\#410](https://github.com/rstudio/rsconnect/issues/410))
- Fix error when an old version of a package is installed and a current
  version isn’t available
  ([\#431](https://github.com/rstudio/rsconnect/issues/431),
  [\#436](https://github.com/rstudio/rsconnect/issues/436))
- Fix error where packages couldn’t be found with nonstandard contrib
  URLs. ([\#451](https://github.com/rstudio/rsconnect/issues/451),
  [\#457](https://github.com/rstudio/rsconnect/issues/457))
- Improve detection of Shiny R Markdown files when `server.R` is present
  ([\#461](https://github.com/rstudio/rsconnect/issues/461))
- Fix failure to write manifest when package requires a newer R version
  than the active version
  ([\#467](https://github.com/rstudio/rsconnect/issues/467))
- Increase default HTTP timeout on non-Windows platforms
  ([\#476](https://github.com/rstudio/rsconnect/issues/476))
- Require `packrat` 0.5 or later
  ([\#434](https://github.com/rstudio/rsconnect/issues/434))
- Fix error when handling empty application / content lists
  ([\#417](https://github.com/rstudio/rsconnect/issues/417),
  [\#395](https://github.com/rstudio/rsconnect/issues/395))
- Calls to
  [`writeManifest()`](https://rstudio.github.io/rsconnect/dev/reference/writeManifest.md)
  no longer reference `packrat` files in the generated `manifest.json`.
  The `packrat` entries were transient and only existed while computing
  dependencies.
  ([\#472](https://github.com/rstudio/rsconnect/issues/472))
- Fix `applications` when ShinyApps does not return `size` details
  ([\#496](https://github.com/rstudio/rsconnect/issues/496))
- GitLab is seen as a valid SCM source
  ([\#491](https://github.com/rstudio/rsconnect/issues/491))

## rsconnect 0.8.16

CRAN release: 2019-12-13

- Prevent attempts to deploy Connect applications without uploading
  ([\#145](https://github.com/rstudio/rsconnect/issues/145))
- Flag usage of [`browser()`](https://rdrr.io/r/base/browser.html)
  debugging calls when deploying
  ([\#196](https://github.com/rstudio/rsconnect/issues/196))
- Prevent accidental deployment of Plumber APIs to shinyapps.io
  ([\#204](https://github.com/rstudio/rsconnect/issues/204))
- Allow `appId` and other global deployment parameters to `deploySite`
  ([\#231](https://github.com/rstudio/rsconnect/issues/231))
- Fix error when running
  [`deployments()`](https://rstudio.github.io/rsconnect/dev/reference/deployments.md)
  without any registered accounts
  ([\#261](https://github.com/rstudio/rsconnect/issues/261))
- Omit `renv` files from deployment bundle
  ([\#367](https://github.com/rstudio/rsconnect/issues/367))
- Fix failure to deploy in Packrat projects
  ([\#370](https://github.com/rstudio/rsconnect/issues/370))
- Fix issue deploying when a package exists in multiple repos
  ([\#372](https://github.com/rstudio/rsconnect/issues/372))
- Honor `RETICULATE_PYTHON` when writing manifests
  ([\#374](https://github.com/rstudio/rsconnect/issues/374))
- Add `on.failure` user hook to run a function when
  [`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
  fails ([\#375](https://github.com/rstudio/rsconnect/issues/375))
- Fix error when showing non-streaming logs
  ([\#377](https://github.com/rstudio/rsconnect/issues/377))
- Use internally computed MD5 sums when MD5 is disabled in FIPS mode
  ([\#378](https://github.com/rstudio/rsconnect/issues/378),
  [\#382](https://github.com/rstudio/rsconnect/issues/382))
- Make it clearer which log entries are emitted by RStudio Connect
  ([\#385](https://github.com/rstudio/rsconnect/issues/385))
- Add support for `requirements.txt` for Python, if it exists
  ([\#386](https://github.com/rstudio/rsconnect/issues/386))
- Restore compatibility with R \< 3.5
  ([\#394](https://github.com/rstudio/rsconnect/issues/394))
- Add support for authenticating with Connect via an API key rather than
  a token ([\#393](https://github.com/rstudio/rsconnect/issues/393))

## rsconnect 0.8.15

CRAN release: 2019-07-22

- Switch from **RCurl** to **curl** as the default HTTP backend
  ([\#325](https://github.com/rstudio/rsconnect/issues/325))
- Add
  [`purgeApp()`](https://rstudio.github.io/rsconnect/dev/reference/purgeApp.md)
  function to purge previously deployed shinyapps.io applications
  ([\#352](https://github.com/rstudio/rsconnect/issues/352))
