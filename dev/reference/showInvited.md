# List invited users for an application

List invited users for an application

Supported servers: ShinyApps servers

## Usage

``` r
showInvited(appDir = getwd(), appName = NULL, account = NULL, server = NULL)
```

## Arguments

- appDir:

  Directory containing application. Defaults to current working
  directory.

- appName:

  Name of application.

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

  Use
  [`accounts()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md)
  to see the full list of available options.

## Note

This function works only for ShinyApps servers.

## See also

[`addAuthorizedUser()`](https://rstudio.github.io/rsconnect/dev/reference/addAuthorizedUser.md)
and
[`showUsers()`](https://rstudio.github.io/rsconnect/dev/reference/showUsers.md)
