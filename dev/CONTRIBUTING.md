# Contributing to rsconnect

rsconnect is an R package. It deploys content to Posit Connect, Posit
Connect Cloud, and shinyapps.io. This guide tells you how to work on the
package and which conventions to follow.

## Development commands

Run these commands from the package root.

``` r

devtools::load_all()          # load the package for interactive work
devtools::test()              # run all tests
devtools::test(filter = "deployApp")  # run tests/testthat/test-deployApp.R
devtools::document()          # update man/ and NAMESPACE from roxygen comments
devtools::check()             # run R CMD check
lintr::lint_package()         # run the linters that CI runs
```

Format the code with [air](https://posit-dev.github.io/air/) before you
commit. CI runs `air format .` on each pull request and suggests changes
if the code is not formatted:

``` sh
air format .
```

roxygen generates `man/` and `NAMESPACE`. Do not edit these files
manually. Change the roxygen comments and run `devtools::document()`. CI
runs roxygen and fails if `man/` or `NAMESPACE` is not up to date.

CI runs [lintr](https://lintr.r-lib.org/) and fails if it finds a
problem. The lintr configuration is in `.lintr.R`, and it needs lintr
3.2.0 or later. The configuration includes two custom linters. One
linter finds objects that a `test-*.R` file assigns at the top level.
The other linter finds assignments of an `if` block with braces. The
sections below explain these two rules.

## Client objects and S3 methods

Each server type has a client class: `connectClient`,
`connectCloudClient`, and `shinyAppsClient`. `clientForAccount()` in
`R/client.R` makes the correct client for an account.

If a function works with more than one client type, make it an S3
generic. Put the generic in `R/client-generics.R` and put each method in
the file for its client (`R/client-connect.R`,
`R/client-connectCloud.R`, or `R/client-shinyapps.R`). Mark each method
with `#' @export` and run `devtools::document()` so that NAMESPACE
registers it. Do not write branching logic on the server type or server
name (for example, `if (isShinyappsServer(...))`) to select the
behavior. Add a method instead.

All clients also have the class `rsconnectClient`. A method for
`rsconnectClient` is the default for all clients, and a method for one
client class overrides it. `findContentByName.rsconnectClient()` is an
example.

If a function works with only one client type, do not make a generic.
Write a standard function in the file for that client, and validate the
client at the start of the function with `checkConnectClient()`,
`checkConnectCloudClient()`, or `checkShinyappsClient()`:

``` r

connectCloudGetContent <- function(client, contentId) {
  checkConnectCloudClient(client)
  ...
}
```

## Code that the RStudio IDE calls

The RStudio IDE calls exported functions and also some internal
functions with `rsconnect:::`. The calls are in
[`SessionRSConnect.R`](https://github.com/rstudio/rstudio/blob/main/src/cpp/session/modules/SessionRSConnect.R)
in the RStudio repository. The internal functions are:

- `validateServerUrl()`, `getAppById()`, `getUserFromRawToken()`,
  `registerUserToken()`, and `showRstudioSourceMarkers()` in `R/ide.R`
- `isConnectServer()`, `isPositConnectCloudServer()`, and
  `connectCloudUrls()` in `R/servers.R`
- `clientForAccount()` in `R/client.R`
- `getAuthToken()` in `R/accounts.R`
- `hasLint()` in `R/lint.R`
- `cli_menu()` in `R/utils-cli.R`

The IDE also calls some fields on the client objects, for example
`client$getEnvVars()` on the Connect client and `client$getContent()` on
the Connect Cloud client. Do not rename or remove these functions or
fields, and do not change their arguments or return values. Before you
change an internal function, search `SessionRSConnect.R` for it, because
this list can become out of date. The tests in
`tests/testthat/test-ide.R` call these functions and fields in the same
way as the IDE.

## Errors and messages

Use [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
for errors, not [`stop()`](https://rdrr.io/r/base/stop.html). If a
helper function signals the error for its caller, give the helper a
`call = caller_env()` argument and pass `call` to `cli_abort()`.

Make sure that each message is correct for all callers. For example, an
error in shared code must not name Connect Cloud if the code also runs
for shinyapps.io. A message in code that
[`writeManifest()`](https://rstudio.github.io/rsconnect/dev/reference/writeManifest.md)
also uses must not talk about deployment.

## Tests

Tests use [testthat](https://testthat.r-lib.org/). New tests should go
into a `tests/testthat/test-*.R` file whose name matches the `.R` file
in which the function you are testing is defined. Put all test helpers
and shared test data in a `tests/testthat/helper*.R` file. Do not define
them in a `test-*.R` file. Each `test_that()` block must run alone after
`devtools::load_all()`, so do not make a test depend on objects that
other tests in the same file create.

Before you write a new helper, look for one in the `helper*.R` files.
Some helpers that tests use frequently:

- `local_temp_config()` uses a temporary config directory, so that tests
  do not change the real accounts and servers.
- `local_temp_app()` makes a temporary app directory from a list of
  files.
- `fake_client()` makes a client object of a given class.

Use [withr](https://withr.r-lib.org/) to make temporary changes in a
test, for example
[`withr::local_tempdir()`](https://withr.r-lib.org/reference/with_tempfile.html),
[`withr::local_envvar()`](https://withr.r-lib.org/reference/with_envvar.html),
and
[`withr::local_options()`](https://withr.r-lib.org/reference/with_options.html).
withr undoes the change when the test ends. Write files in a temporary
directory, not in the `tests/testthat` directory.

When you fix a bug, write a failing test first.

Make assertions specific. Do not only check that a call succeeds or
fails. Check the values that the code produces, for example the request
body or the URL that the code sends.

Integration tests are in `tests/integration` and
`tests/shinyapps-integration`. They need live servers and run in CI, not
with `devtools::test()`.

## Code style

Do not assign the result of a multi-line `if` block to a variable.
Assign the variable inside the block. It is usually best to initialize
the variable before the block and then overwrite it in the block, so
that the variable always has a value. You can also assign the variable
in each branch. If the logic is complex, move it into a helper function
that returns the value. A one-line `x <- if (a) b else c` without braces
is acceptable.

``` r

# Do not do this
plumberInfo <- if (appMode == "api") {
  inferPlumberInfo(appDir)
} else {
  NULL
}

# Do this
plumberInfo <- NULL
if (appMode == "api") {
  plumberInfo <- inferPlumberInfo(appDir)
}
```

A function must not use variables from the calling environment. Pass all
values as arguments.

Avoid `<<-` as much as possible. It is acceptable in test mocks that
record calls. Some package code also uses it. For example,
`withTokenRefreshRetry()` in the Connect Cloud client uses `<<-` to keep
a refreshed token in the `authInfo` of the client, so that later
requests use the new token. In new code, use a different design if you
can. Use `<<-` only if the alternative is much worse.

Look for an existing helper before you write new logic. If the same
logic occurs in more than one place, move it into a helper function.

## NEWS

Add an entry to `NEWS.md` for changes that users can see. Write the
entry for users. Describe the behavior that changed, not the internal
implementation. Put code in backticks. End the entry with the issue
number, for example `(#1292)`. If there is no issue, use the PR number.
