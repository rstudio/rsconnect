# NEWS

## 0.8.16 (in development)

* Omit `renv` files from deployment bundle (#367)
* Fix failure to deploy in Packrat projects (#370)
* Fix issue deploying when a package exists in multiple repos (#372)
* Honor `RETICULATE_PYTHON` when writing manifests (#374)
* Fix error when showing non-streaming logs (#377)
* Use internally computed MD5 sums when MD5 is disabled in FIPS mode (#378, #382)
* Make it clearer which log entries are emitted by RStudio Connect (#385)
* Add support for `requirements.txt` for Python, if it exists (#386)

## 0.8.15 

Released to CRAN on 2019-07-22

* Switch from **RCurl** to **curl** as the default HTTP backend (#325)
* Add `purgeApp()` function to purge previously deployed shinyapps.io applications (#352)

