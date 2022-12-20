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
