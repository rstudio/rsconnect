## Summary

Use httr2 as HTTP client. Support renv profiles. Remove several
long-deprecated functions, such as `addConnectServer()` and
`discoverServer()`, and other HTTP backends.

Note: The Rsconctdply package uses the removed `addConnectServer()` function.
This problem was reported to the project several weeks ago with no response.
We filed an issue (https://github.com/snchimata/Rsconctdply/issues/1) and
provided a fix (https://github.com/snchimata/Rsconctdply/pull/2).

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## revdepcheck results

We checked 24 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* Rsconctdply
  checking dependencies in R code ... WARNING

