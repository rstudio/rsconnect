# Maintain environment variables across multiple applications

- `listAccountEnvVars()` lists the environment variables used by every
  application published to the specified account.

- `updateAccountEnvVars()` updates the specified environment variables
  with their current values for every app that uses them.

Secure environment variable are currently only supported by Posit
Connect so other server types will generate an error.

Supported servers: Posit Connect servers

## Usage

``` r
listAccountEnvVars(server = NULL, account = NULL)

updateAccountEnvVars(envVars, server = NULL, account = NULL)
```

## Arguments

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

  Use
  [`accounts()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md)
  to see the full list of available options.

- envVars:

  Names of environment variables to update. Their values will be
  automatically retrieved from the current process.

  If you specify multiple environment variables, any application that
  uses any of them will be updated with all of them.

## Value

`listAccountEnvVars()` returns a data frame with one row for each data
frame. It has variables `id`, `guid`, `name`, and `envVars`. `envVars`
is a list-column.
