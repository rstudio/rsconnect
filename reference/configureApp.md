# Configure an Application

Configure an application running on a remote server.

Supported servers: ShinyApps servers

## Usage

``` r
configureApp(
  appName,
  appDir = getwd(),
  account = NULL,
  server = NULL,
  redeploy = TRUE,
  size = NULL,
  instances = NULL,
  logLevel = c("normal", "quiet", "verbose")
)
```

## Arguments

- appName:

  Name of application to configure

- appDir:

  Directory containing application. Defaults to current working
  directory.

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

  Use
  [`accounts()`](https://rstudio.github.io/rsconnect/reference/accounts.md)
  to see the full list of available options.

- redeploy:

  Re-deploy application after its been configured.

- size:

  Configure application instance size

- instances:

  Configure number of application instances

- logLevel:

  One of `"quiet"`, `"normal"` or `"verbose"`; indicates how much
  logging to the console is to be performed. At `"quiet"` reports no
  information; at `"verbose"`, a full diagnostic log is captured.

## Note

This function works only for ShinyApps servers.

## See also

[`applications()`](https://rstudio.github.io/rsconnect/reference/applications.md),
[`deployApp()`](https://rstudio.github.io/rsconnect/reference/deployApp.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# set instance size for an application
configureApp("myapp", size="xlarge")
} # }
```
