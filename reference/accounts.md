# Account Management Functions

Functions to enumerate and remove accounts on the local system. Prior to
deploying applications you need to register your account on the local
system.

Supported servers: All servers

## Usage

``` r
accounts(server = NULL)

accountInfo(name = NULL, server = NULL)

removeAccount(name = NULL, server = NULL)
```

## Arguments

- server:

  Name of the server on which the account is registered (optional; see
  [`servers()`](https://rstudio.github.io/rsconnect/reference/servers.md))

- name:

  Name of account

## Value

`accounts` returns a data frame with the names of all accounts
registered on the system and the servers on which they reside.
`accountInfo` returns a list with account details.

## Details

You register an account using the
[`setAccountInfo()`](https://rstudio.github.io/rsconnect/reference/setAccountInfo.md)
function (for ShinyApps) or
[`connectUser()`](https://rstudio.github.io/rsconnect/reference/connectApiUser.md)
function (for other servers). You can subsequently remove the account
using the `removeAccount` function.

The `accounts` and `accountInfo` functions are provided for viewing
previously registered accounts.

## See also

Other Account functions:
[`connectApiUser()`](https://rstudio.github.io/rsconnect/reference/connectApiUser.md),
[`connectCloudUser()`](https://rstudio.github.io/rsconnect/reference/connectCloudUser.md),
[`setAccountInfo()`](https://rstudio.github.io/rsconnect/reference/setAccountInfo.md)
