# rsconnect 1.2.1

* Restore the `LC_TIME` locale after computing an RFC-2616 date. (#1035)
* Address a problem inspecting Quarto content when the file names and paths
  needed to be quoted. The resulting manifest lacked information about the
  Quarto runtime, which caused difficult-to-understand deployment errors.
  (#1037)
* Produce an error when Quarto content cannot be inspected. (#1032)

# rsconnect 1.2.0

* Addressed a number of republishing and collaboration issues where the
  content was incorrectly published to a new location rather than reusing an
  existing deployment. (#981, #1007, #1013, #1019)

* `showLogs()`, `configureApp()`, `setProperty()`, and `unsetProperty()`
  search for the application by name when there are no matching deployment
  records. (#985, #989)

* `rpubsUpload()` correctly records the initial RPubs destination, allowing
  republishing. (#976)

* `deployApp()` and friends record multi-value `metadata` entries as
  comma-separated values. (#1017)

* `accountInfo()` includes `name` and `username` fields. Older versions of
  rsconnect store account records with a `username` field. Recent rsconnect
  versions record `name`. Both `name` and `username` should contain the same
  value. (#1024)

# rsconnect 1.1.1

* Added `space` parameter to deploy directly to a space in Posit Cloud.

* Improve reporting of errors returned by shinyapps.io. (#997)

* Remove most directory layout validation checks. (#998)

* Do not use `getOption("available_packages_filters")` option when calling
  `available.packages()`. (#1002)

* Packages installed from source within an renv project are not associated
  with repositories. (#1004)

# rsconnect 1.1.0

* Fixed analysis of directories that were smaller than the
  `rsconnect.max.bundle.files=10000` limit but larger than the
  `renv.config.dependencies.limit=1000` limit. (#968)

* Ignore `.env`, `.venv`, and `venv` files only when they reference Python
  virtual environments. (#972)
  
* `deployApp()` and `writeManifest()` accept optional `envManagement`,
  `envManagementR`, and `envManagementPy` arguments. These args specify whether
  Posit Connect should install packages in the package cache.
  If `envManagement` is `FALSE` then Connect will not perform any package
  installation and it is the administrator's responsibility to ensure the
  required R/Python packages are available in the runtime environment. This is
  especially useful if off-host execution is enabled, when the execution
  environment (specified by the `image` argument) already contains the required
  packages. These values are ignored when
  `Applications.ManifestEnvironmentManagementSelection = false`.
  Requires Posit Connect `>=2023.07.0`. (#977)

* Fix account discovery by `showProperties()`. (#980)

# rsconnect 1.0.2

* Fixed redeployments to shinyapps.io where `appName` is provided, but no local
  record of the deployment exists. (#932)

* `deployApp()` and `writeManifest()` now error if your library and `renv.lock`
  are out-of-sync. Previously it always used what was defined in the `renv.lock`
  but that was (a) slow and (b) could lead to different results than what you
  see when running locally (#930).

* Deploying from an renv project includes the `renv.lock` in the bundle. A
  manifest created for an renv project references the `renv.lock` in the
  `manifest.json`. (#926)

* Use the environment variable `RSCONNECT_PACKRAT` to analyze dependencies
  using packrat, as was done prior to rsconnect-1.0.0. Use of the
  `rsconnect.packrat` option is discouraged, as it is not effective when using
  push-button deployment in the RStudio IDE. (#935)

* The `renv.lock` is ignored when the `RSCONNECT_PACKRAT` environment variable
  or the `rsconnect.packrat` option is set. (#936)

* The content type is inferred by analyzing the set of top-level files. (#942)

* `deployApp()` and `writeManifest()` accept an optional `appMode` argument.
  Provide this argument if your project includes auxiliary files which mislead
  the existing `appMode` inference. For example, if an HTML project includes
  a downloadable Shiny `app.R`, that content will be assumed to be a Shiny
  application even if that application is not meant to be run. (#948)

* `appDependencies()` accepts an `appFileManifest` argument as an alternate
  way of providing the target set of files.

# rsconnect 1.0.1

* `deployDoc()` includes `.Rprofile`, `requirements.txt` and `renv.lock` when
  deploying `.Rmd` or `.qmd`. These additional files are not included with
  rendered HTML documents. (#919)

* Explicit renv dependencies are preserved. (#916)

# rsconnect 1.0.0

## New features

* `deployApp()` and `deployDoc()` now support deploying static content to Posit
  Cloud. Static RMarkdown and Quarto content can be rendered server-side.

* rsconnect requires renv 1.0.0.

* `deployApp()` and `writeManifest()` now respect renv lock files, if present. 
  If you don't want to use these lockfiles, and instead return the previous 
  behaviour of snapshotting on every deploy, add your `renv.lock` to 
  `.rscignore` (#671). Learn more `?appDependencies()`.
  
  Additionally, `deployApp()` and `writeManifest()` now use renv to capture app 
  dependencies,  rather than packrat. If this causes a previously working deploy 
  to fail, please file an issue then set `options(rsconnect.packrat = TRUE)` to 
  revert to the previous behaviour.

* `deployApp()`'s `quarto` argument now takes values `TRUE`, `FALSE` or 
  `NA`. The previous value (a path to a quarto binary) is now ignored,
  and instead we automatically figure out the package from `QUARTO_PATH` and
  `PATH` env vars (#658). `deploySite()` now supports quarto websites (#813).

* `deployApp()` gains a new `envVars` argument which takes a vector of the 
  names of environment variables that should be securely copied to the server. 
  The names (not values) of these environment variables are also saved in the
  deployment record and will be updated each time you re-deploy the app (#667).
  This currently only works with Connect, but we hope to add support to 
  Posit cloud and shinyapps.io in the future.
  
* rsconnect gains two new functions for understanding and updating the 
  environment variables that your apps currently use. `listServerEnvVars()`
  will return a data frame of applications, with a `envVars` list-column
  giving the names of the environment variables used by each application.
  `updateServerEnvVars()` will update all applications that use a specific
  environment variable with the current value of that environment variable
  (#667).

## Lifecycle changes

* Non-libcurl `rsconnect.http` options have been deprecated. This allows us to 
  focus our efforts on a single backend, rather than spreading development
  efforts across five. The old backends will remain available for at least 2
  years, but if you are using them because libcurl doesn't work for you, please
  report the problem ASAP so we can fix it.

* `addConnectServer()` has been deprecated because it does the same
  thing as `addServer()` now that `addServer()` also validates URLs.

* `deployTFModel()` is defunct. Posit Connect no longer supports hosting of
  TensorFlow Model APIs. A TensorFlow model can be deployed as a [Plumber
  API](https://tensorflow.rstudio.com/guides/deploy/plumber.html), [Shiny
  application](https://tensorflow.rstudio.com/guides/deploy/shiny), or other
  supported content type.

* `discoverServer()` has been deprecated; it never worked.

* `deployApp("foo.Rmd")` has been deprecated. It was never documented, and
  it does the same job as `deployDoc()` (#698).

## Minor improvements and bug fixes

* New `rsconnect.http.headers` and `rsconnect.http.cookies` allow you to
  set extra arbitrary additional headers/cookies on each request (#405).
  Their use is documented in the new `vignette("custom-http")`.

* Uploading large files to RPubs works once more (#450).

* When recording details about deployments to Posit Cloud, appId now represents
  the content id (as seen in URLs of the format 
  `https://posit.cloud/content/{id}`) instead of the application id.

* Deployment records no longer contain the time the app was deployed (`when`)
  or when it's metadata was last synced (`lastSyncTime`) as these variables
  are not very useful, and they lead to uninteresting diffs if you have 
  committed the deployment records to git (#770). A `version` field has been 
  added to deployment DCF files to facilitate future file format changes, if
  needed. Its value for this release is `1`.,

* `accounts()` returns a zero-row data frame if no accounts are registered.

* `accountInfo()` and `removeAccount()` no longer require `account` be 
  supplied (#666).

* `accountInfo()` and `servers()` redact sensitive information (secrets,
  private keys, and certificates) to make it hard to accidentally reveal
  such information in logs (#675).

* `addServer()` includes the port in the default server name, if present.

* `appDependencies()` includes implicit dependencies, and returns an additional 
  column giving the Repository (#670). Its documentation contains more 
  information about how dependency discovery works, and how you can control
  it, if needed.

* `applications()` now returns the application title, if available (#484),
  and processes multiple pages of results from a Connect server (#860).

* `connectApiUser()` now clearly requires an `apiKey` (#741).

* `deployApp()` output has been thoroughly reviewed and tweaked. As well as 
  general polish it now gives you more information about what it has discovered
  about the deployment, like the app name, account & server, and which files
  are included in the bundle (#669).

* `deployApp()` is more aggressive about saving deployment data, which should
  make it less likely that you need to repeat yourself after a failed 
  deployment. In particular, it now saves both before and after uploading the
  contents (#677) and it saves when you're updating content originally created
  by someone else (#270).

* `deployApp()` now gives an actionable error if you attempt to set
  visibility of an app deployed to posit.cloud (#838).

* `deployApp()` now uses a stricter policy for determining whether or not
  a locally installed package can be successfully installed on the deployment 
  server. This means that you're more likely to get a clean failure prior to 
  deployment (#659).

* `deployApp()` will now detect if you're attempting to publish to an app
  that has been deleted and will prompt you to create a new app (#226).

* `deployApp()` includes some new conveniences for large uploads including
  reporting the size of the bundle you're uploading and showing a progress bar 
  in interactive sessions  (#754).

* `deployApp()` now follows redirects, which should make it more robust to your
  server moving to a new url (#674).

* `deployApp()` uses simpler logic for determining whether it should create a
  new app or update an existing app. Now `appName`, `account`, and `server` are 
  used to find existing deployments. If none are found, it will create a new 
  deployment; if one is found, it'll be updated; if more than one are found, it 
  will prompt you to disambiguate (#666).

* `deployApp()` improves account resolution from `account` and `server` 
  arguments by giving specific recommendations on the values that you might use 
  in the case of ambiguity or lack of matches (#666). Additionally, you'll now 
  receive a clear error if you accidentally provide something other than a 
  string or `NULL` to these arguments.

* `deployApp()` now generates an interactive prompt to select `account`/`server`
  (if no previous deployments) or `appName`/`account`/`server` (if multiple 
  previous deployments) (#691). 

* `deployApp()` now advertises which startup scripts are run at the normal
  `logLevel`, and it evaluates each script in its own environment (#542).

* `deployApp()` now derives `appName` from `appDir` and `appPrimaryDoc`, 
  never using the title (#538). It now only simplifies the path if you are 
  publishing to shinyapps.io, since its restrictions on application names are 
  much tighter than those of Posit Connect.

* `deployApp()` will now warn if `appFiles` or `appManifestFiles` contain
  files that don't exist, rather than silently ignoring them (#706).

* `deployApp()` excludes temporary backup files (names starting or ending 
  with `~`) when automatically determining files to bundle (#111) as well as 
  directories that are likely to be Python virtual environments (#632). 
  Additionally, ignore rules are always now applied to all directories; 
  previously some (like `.Rproj.user` and `"manifest.json"`) were only 
  applied to the root directory. It correctly handles `.rscignore`  files 
  (i.e. as documented) (#568). 

* `deployApp(appSourceDoc)` has been deprecated; it did the same job as
  `recordDir`.

* `deployDoc()` includes a `.Rprofile` in the bundle, if one is found in the 
  same directory as the document.

* `lint()` should have fewer false positives for path problems:
  the relative path linter has been removed (#244) and the case-sensitive 
  linter now only checks strings containing a `/` (#611).

* New `listDeploymentFiles()`, which supsersedes `listBundleFiles()`.
  It now errors when if the bundle is either too large or contains too many 
  files, rather than silently truncating as before (#684).

* `serverInfo()` and `removeServer()` no longer require a `server` when 
  called interactively.

* `showMetrics()` once again returns a correctly named data frame (#528).

* Removed Rmd generation code (`writeRmdIndex()`) which had not worked, or
  been necessary, for quite some time (#106, #109).

* Locale detection has been improved on windows (#233).

* The `rsconnect.pre.deploy` and `rsconnect.post.deploy` hooks are now always
  called with the content directory, not sometimes the path to a specific file
  (#696).

* Functions that should only interact with shinyapps.io enforce the server
  type. Updated `addAuthorizedUser()`, `removeAuthorizedUser()`,
  `showUsers()`, `showInvited()`, `resendInvitation()`, `configureApp()`,
  `setProperty()`, `unsetProperty()`, `purgeApp()`, `restartApp()`,
  `terminateApp()`, `showUsage()`, and `showMetrics()` (#863, #864).

* When needed packages are not installed, and you're in an interactive
  environment, rsconnect will now prompt you to install them (#665).

* The confirmation prompt presented upon lint failures indicates "no" as its
  default. (#652)

# rsconnect 0.8.29

* Introduced support for publishing to Posit Cloud. This feature is currently
  in closed beta and requires access to an enabled account on Posit Cloud.
  See [Posit Cloud's Announcement](https://posit.cloud/learn/whats-new#publishing)
  for more information and to request access.

* Update company and product names for rebranding to Posit.

# rsconnect 0.8.28

* Shiny applications and Shiny documents no longer include an implicit
  dependency on [`ragg`](https://ragg.r-lib.org) when that package is present
  in the local environment. This reverts a change introduced in 0.8.27.
  
  Shiny applications should add an explicit dependency on `ragg` (usually with
  a `library("ragg")` statement) to see it used by `shiny::renderPlot` (via
  `shiny::plotPNG`).
  
  The documentation for `shiny::plotPNG` explains the use of `ragg`. (#598)

* Fix bug that prevented publishing or writing manifests for non-Quarto content
  when a Quarto path was provided to the `quarto` argument of `writeManifest()`,
  `deployApp()`, and related functions.

* Escape account names when performing a directory search to determine an
  appropriate server. (#620)

# rsconnect 0.8.27

* Quarto content will no longer silently deploy as R Markdown content when
  Quarto metadata is missing or cannot be gathered. Functions will error,
  requesting the path to a Quarto binary in the `quarto` argument. (#594)
* Fix typo for `.rscignore`. (#599)
* Quarto deployments specifying only an `appDir` and `quarto` binary but not an
  `appPrimaryDoc` work more consistently. A directory containing a `.qmd` file
  will deploy as Quarto content instead of failing, and a directory containing
  an `.Rmd` file will successfully deploy as Quarto content instead of falling
  back to R Markdown. (#601)
* If the `ragg` package is installed locally, it is now added as an implicit
  dependency to `shiny` apps since `shiny::renderPlot()` now uses it by default 
  (when available). This way, `shiny` apps won't have to add `library(ragg)` to 
  get consistent (higher-quality) PNG images when deployed. (#598)

# rsconnect 0.8.26

* Add ability to resend shinyapps.io application invitations (#543)
* Show expiration status in shinyapps.io for invitations (#543)
* Allow shinyapps.io deployments to be private at creation time (#403)
* Update the minimum `openssl` version to 2.0.0 to enable publishing for users
  on FIPS-compliant systems without the need for API keys. (#452)
* Added Quarto support to `writeManifest`, which requires passing the absolute
  path to a Quarto executable to its new `quarto` parameter
* Added `quarto` parameter to `deployApp` to enable deploying Quarto documents
  and websites by supplying the path to a Quarto executable
* Added support for deploying Quarto content that uses only the `jupyter` runtime
* Added support for selecting a target `image` in the bundle manifest
* The `showLogs` function takes a `server` parameter. (#57)
* Added the `rsconnect.tar` option, which can be used to specify the path to a
  `tar` implementation instead of R's internal implementation. The previous
  method, using the `RSCONNECT_TAR` environment variable, still works, but the
  new option will take precedence if both are set.

# rsconnect 0.8.25

* Use the `curl` option `-T` when uploading files to avoid out of memory
  errors with large files. (#544)
* The `rsconnect.max.bundle.size` and `rsconnect.max.bundle.files` options are
  enforced when processing an enumerated set of files. Previously, these
  limits were enforced only when bundling an entire content directory. (#542)
* Preserve file time stamps when copying files into the bundle staging
  directory, which then propagates into the created tar file. (#540)
* Configuration directories align with CRAN policy and use the location named
  by `tools::R_user_dir`. Configuration created by earlier versions of this
  package is automatically migrated to the new location. (#550)

# rsconnect 0.8.24

* Added support for publishing Quarto documents and websites
* Added support for `.rscignore` file to exclude files or directories from publishing (#368)
* Fixed issue causing missing value errors when publishing content containing filenames with extended characters (#514)
* Fixed issue preventing error tracebacks from displaying (#518)

# rsconnect 0.8.18

* Fixed issue causing configuration directory to be left behind after `R CMD CHECK`
* Fixed incorrect subdirectory nesting when storing configuration in `R_USER_CONFIG_DIR`
* Added linter for different-case Markdown links (#388)
* Use new Packrat release on CRAN, 0.6.0 (#501)
* Fix incorrect linter messages referring to `shiny.R` instead of `server.R` (#509)
* Warn, rather than err, when the repository URL for a package dependency
  cannot be validated. This allows deployment when using archived CRAN
  packages, or when using packages installed from source that are available on
  the server. (#508)
* Err when the app-mode cannot be inferred; seen with empty directories/file-sets (#512)
* Add `verbose` option to `writeManifest` utility (#468)

# rsconnect 0.8.17

* Fixed issue where setting `options(rsconnect.http.trace.json = TRUE)` could cause deployment errors with some HTTP transports (#490)
* Improve how large bundles (file size and count) are detected (#464)
* The `RSCONNECT_TAR` environment variable can be used to select the tar implementation used to create bundles (#446)
* Warn when files are owned by users or groups with long names, as this can cause the internal R tar implementation to produce invalid archives (#446)
* Add support for syncing the deployment metadata with the server (#396)
* Insist on ShinyApps accounts in `showUsers()` (#398)
* Improve the regex used for the browser and browseURL lints to include a word boundary (#400)
* Fixed bug where `connectApiUser()` did not set a user id (#407)
* New arguments to `deployApp` to force the generation of a Python environment file or a `requirements.txt` file (#409)
* Fail when no repository URL is available for a dependent package (#410)
* Fix error when an old version of a package is installed and a current version isn't available (#431, #436)
* Fix error where packages couldn't be found with nonstandard contrib URLs. (#451, #457)
* Improve detection of Shiny R Markdown files when `server.R` is present (#461)
* Fix failure to write manifest when package requires a newer R version than the active version (#467)
* Increase default HTTP timeout on non-Windows platforms (#476)
* Require `packrat` 0.5 or later (#434)
* Fix error when handling empty application / content lists (#417, #395)
* Calls to `writeManifest()` no longer reference `packrat` files in the generated `manifest.json`. The `packrat` entries were transient and only existed while computing dependencies. (#472)
* Fix `applications` when ShinyApps does not return `size` details (#496)
* GitLab is seen as a valid SCM source (#491)

# rsconnect 0.8.16

* Prevent attempts to deploy Connect applications without uploading (#145)
* Flag usage of `browser()` debugging calls when deploying (#196)
* Prevent accidental deployment of Plumber APIs to shinyapps.io (#204)
* Allow `appId` and other global deployment parameters to `deploySite` (#231)
* Fix error when running `deployments()` without any registered accounts (#261)
* Omit `renv` files from deployment bundle (#367)
* Fix failure to deploy in Packrat projects (#370)
* Fix issue deploying when a package exists in multiple repos (#372)
* Honor `RETICULATE_PYTHON` when writing manifests (#374)
* Add `on.failure` user hook to run a function when `deployApp()` fails (#375)
* Fix error when showing non-streaming logs (#377)
* Use internally computed MD5 sums when MD5 is disabled in FIPS mode (#378, #382)
* Make it clearer which log entries are emitted by RStudio Connect (#385)
* Add support for `requirements.txt` for Python, if it exists (#386)
* Restore compatibility with R < 3.5 (#394)
* Add support for authenticating with Connect via an API key rather than a token (#393)

# rsconnect 0.8.15

* Switch from **RCurl** to **curl** as the default HTTP backend (#325)
* Add `purgeApp()` function to purge previously deployed shinyapps.io applications (#352)
