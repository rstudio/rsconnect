# Resend invitation for invited users of an application

Resend invitation for invited users of an application

Supported servers: ShinyApps servers

## Usage

``` r
resendInvitation(
  invite,
  regenerate = FALSE,
  appDir = getwd(),
  appName = NULL,
  account = NULL,
  server = NULL
)
```

## Arguments

- invite:

  The invitation to resend. Can be id or email address.

- regenerate:

  Regenerate the invite code. Can be helpful is the invitation has
  expired.

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

[`showInvited()`](https://rstudio.github.io/rsconnect/dev/reference/showInvited.md)
