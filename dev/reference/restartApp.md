# Restart an Application

Restart an application currently running on a remote server.

Supported servers: ShinyApps servers

## Usage

``` r
restartApp(appName, account = NULL, server = NULL, quiet = FALSE)
```

## Arguments

- appName:

  Name of application to restart

- account:

  Account name. If a single account is registered on the system then
  this parameter can be omitted.

- server:

  Server name. Required only if you use the same account name on
  multiple servers (see
  [`servers()`](https://rstudio.github.io/rsconnect/dev/reference/servers.md))

- quiet:

  Request that no status information be printed to the console during
  the operation.

## Note

This function works only for ShinyApps servers.

## See also

[`applications()`](https://rstudio.github.io/rsconnect/dev/reference/applications.md),
[`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md),
and
[`terminateApp()`](https://rstudio.github.io/rsconnect/dev/reference/terminateApp.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# restart an application
restartApp("myapp")
} # }
```
