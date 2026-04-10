# Application Logs

These functions provide access to the logs for deployed ShinyApps
applications:

- `showLogs()` displays the logs.

- `getLogs()` returns the logged lines.

Supported servers: ShinyApps servers

## Usage

``` r
showLogs(
  appPath = getwd(),
  appFile = NULL,
  appName = NULL,
  account = NULL,
  server = NULL,
  entries = 50,
  streaming = FALSE
)

getLogs(
  appPath = getwd(),
  appFile = NULL,
  appName = NULL,
  account = NULL,
  server = NULL,
  entries = 50
)
```

## Arguments

- appPath:

  The path to the directory or file that was deployed.

- appFile:

  The path to the R source file that contains the application (for
  single file applications).

- appName:

  The name of the application to show logs for. May be omitted if only
  one application deployment was made from `appPath`.

- account:

  The account under which the application was deployed. May be omitted
  if only one account is registered on the system.

- server:

  Server name. Required only if you use the same account name on
  multiple servers.

- entries:

  The number of log entries to show. Defaults to 50 entries.

- streaming:

  Deprecated. Streaming logs is not currently supported as the
  ShinyApps.io backend no longer supports this feature. If `TRUE`, an
  error will be thrown. Defaults to `FALSE`.

## Value

`getLogs()` returns a data frame containing the logged lines.

## Note

These functions only work for applications deployed to ShinyApps.io.
