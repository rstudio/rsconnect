# NEWS

## 0.8.17 (in development)

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
