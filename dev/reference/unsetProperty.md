# Unset Application property

Unset a property on currently deployed ShinyApps application (restoring
to its default value)

Supported servers: ShinyApps servers

## Usage

``` r
unsetProperty(
  propertyName,
  appPath = getwd(),
  appName = NULL,
  account = NULL,
  server = NULL,
  force = FALSE
)
```

## Arguments

- propertyName:

  Name of property

- appPath:

  Directory or file that was deployed. Defaults to current working
  directory.

- appName:

  Name of application

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

  Use
  [`accounts()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md)
  to see the full list of available options.

- force:

  Forcibly unset the property

## Note

This function only works for ShinyApps servers.

## Examples

``` r
if (FALSE) { # \dontrun{

# unset application package cache property to revert to default
unsetProperty("application.package.cache")

} # }
```
