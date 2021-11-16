# NEWS

## 0.8.25

Released to CRAN on 2021-11-16

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

## 0.8.24

Released to CRAN on 2021-08-04

* Added support for publishing Quarto documents and websites
* Added support for `.rcsignore` file to exclude files or directories from publishing (#368)
* Fixed issue causing missing value errors when publishing content containing filenames with extended characters (#514)
* Fixed issue preventing error tracebacks from displaying (#518)

## 0.8.18

Released to CRAN on 2021-05-24

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

## 0.8.17

Released to CRAN on 2021-04-09

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

## 0.8.16

Released to CRAN on 2019-12-13

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

## 0.8.15

Released to CRAN on 2019-07-22

* Switch from **RCurl** to **curl** as the default HTTP backend (#325)
* Add `purgeApp()` function to purge previously deployed shinyapps.io applications (#352)
