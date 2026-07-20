# Show Application Metrics

Show application metrics of a currently deployed application.

Supported servers: ShinyApps servers

## Usage

``` r
showMetrics(
  metricSeries,
  metricNames,
  appDir = getwd(),
  appName = NULL,
  account = NULL,
  server = "shinyapps.io",
  from = NULL,
  until = NULL,
  interval = NULL
)
```

## Arguments

- metricSeries:

  Metric series to query. Refer to the [shinyapps.io
  documentation](https://docs.posit.co/shinyapps.io/metrics.html#ApplicationMetrics)
  for available series.

- metricNames:

  Metric names in the series to query. Refer to the [shinyapps.io
  documentation](https://docs.posit.co/shinyapps.io/metrics.html#ApplicationMetrics)
  for available metrics.

- appDir:

  A directory containing an application (e.g. a Shiny app or plumber
  API). Defaults to the current directory.

- appName:

  Application name, a string consisting of letters, numbers, `_` and
  `-`. The application name is used to identify applications on a
  server, so must be unique.

  If not specified, the name is automatically derived on the first
  deployment from `appDir` for applications and websites, and from
  `appPrimaryDoc` for documents. On subsequent deploys, the previously
  stored value is used.

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

  Use
  [`accounts()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md)
  to see the full list of available options.

- from:

  Date range starting timestamp (Unix timestamp or relative time delta
  such as "2d" or "3w").

- until:

  Date range ending timestamp (Unix timestamp or relative time delta
  such as "2d" or "3w").

- interval:

  Summarization interval. Data points at intervals less than this will
  be grouped. (Relative time delta e.g. "120s" or "1h" or "30d").
