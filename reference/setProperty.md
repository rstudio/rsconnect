# Set Application property

Set a property on currently deployed ShinyApps application.

Supported servers: ShinyApps servers

## Usage

``` r
setProperty(
  propertyName,
  propertyValue,
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

- propertyValue:

  Property value

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
  [`accounts()`](https://rstudio.github.io/rsconnect/reference/accounts.md)
  to see the full list of available options.

- force:

  Forcibly set the property

## Note

This function only works for ShinyApps servers.

## Examples

``` r
if (FALSE) { # \dontrun{

# set instance size for an application
setProperty("application.instances.count", 1)

# disable application package cache
setProperty("application.package.cache", FALSE)

} # }
```
