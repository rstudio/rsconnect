## Summary

* Suppress more deployment output when requested.
* Reintroduce TensorFlow saved model support.
* Avoid refuse reported by the CRAN donttest check by skipping tests that use Quarto.
* Skip tests that use unavailable packages, identified by the CRAN noSuggests check.

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## revdepcheck results

We checked 24 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
