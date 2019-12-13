# NEWS

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

