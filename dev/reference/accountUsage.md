# Show Account Usage

Show account usage

Supported servers: ShinyApps servers

## Usage

``` r
accountUsage(
  account = NULL,
  server = NULL,
  usageType = "hours",
  from = NULL,
  until = NULL,
  interval = NULL
)
```

## Arguments

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

  Use
  [`accounts()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md)
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
  be grouped. (Number of seconds or relative time delta e.g. "1h").

## Note

This function only works for ShinyApps servers.
