# Terminate an Application

Terminate and archive a currently deployed ShinyApps application.

Supported servers: ShinyApps servers

## Usage

``` r
terminateApp(appName, account = NULL, server = NULL, quiet = FALSE)
```

## Arguments

- appName:

  Name of application to terminate

- account:

  Account name. If a single account is registered on the system then
  this parameter can be omitted.

- server:

  Server name. Required only if you use the same account name on
  multiple servers (see
  [`servers()`](https://rstudio.github.io/rsconnect/reference/servers.md))

- quiet:

  Request that no status information be printed to the console during
  the termination.

## Note

This function only works for ShinyApps servers.

## See also

[`applications()`](https://rstudio.github.io/rsconnect/reference/applications.md),
[`deployApp()`](https://rstudio.github.io/rsconnect/reference/deployApp.md),
and
[`restartApp()`](https://rstudio.github.io/rsconnect/reference/restartApp.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# terminate an application
terminateApp("myapp")
} # }
```
