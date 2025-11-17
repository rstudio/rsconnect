# Add authorized user to application

Add authorized user to application

Supported servers: ShinyApps servers

## Usage

``` r
addAuthorizedUser(
  email,
  appDir = getwd(),
  appName = NULL,
  account = NULL,
  server = NULL,
  sendEmail = NULL,
  emailMessage = NULL
)
```

## Arguments

- email:

  Email address of user to add.

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

- sendEmail:

  Send an email letting the user know the application has been shared
  with them.

- emailMessage:

  Optional character vector of length 1 containing a custom message to
  send in email invitation. Defaults to NULL, which will use default
  invitation message.

## Note

This function works only for ShinyApps servers.

## See also

[`removeAuthorizedUser()`](https://rstudio.github.io/rsconnect/dev/reference/removeAuthorizedUser.md)
and
[`showUsers()`](https://rstudio.github.io/rsconnect/dev/reference/showUsers.md)
