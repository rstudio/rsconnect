# Show Application Usage

Show application usage of a currently deployed application

Supported servers: ShinyApps servers

## Usage

``` r
showUsage(
  appDir = getwd(),
  appName = NULL,
  account = NULL,
  server = NULL,
  usageType = "hours",
  from = NULL,
  until = NULL,
  interval = NULL
)
```

## Arguments

- appDir:

  Directory containing application. Defaults to current working
  directory.

- appName:

  Name of application

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

  Use
  [`accounts()`](https://rstudio.github.io/rsconnect/reference/accounts.md)
  to see the full list of available options.

- usageType:

  Use metric to retreive (for example: "hours")

- from:

  Date range starting timestamp (Unix timestamp or relative time delta
  such as "2d" or "3w").

- until:

  Date range ending timestamp (Unix timestamp or relative time delta
  such as "2d" or "3w").

- interval:

  Summarization interval. Data points at intervals less then this will
  be grouped. (Relative time delta e.g. "120s" or "1h" or "30d").

## Note

This function only works for ShinyApps servers.
