# Register account on shinyapps.io

Configure a ShinyApps account for publishing from this system.

Supported servers: ShinyApps servers

## Usage

``` r
setAccountInfo(name, token, secret, server = "shinyapps.io")
```

## Arguments

- name:

  Name of account to save or remove

- token:

  User token for the account

- secret:

  User secret for the account

- server:

  Server to associate account with.

## See also

Other Account functions:
[`accounts()`](https://rstudio.github.io/rsconnect/reference/accounts.md),
[`connectApiUser()`](https://rstudio.github.io/rsconnect/reference/connectApiUser.md),
[`connectCloudUser()`](https://rstudio.github.io/rsconnect/reference/connectCloudUser.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# register an account
setAccountInfo("user", "token", "secret")

# remove the same account
removeAccount("user")
} # }
```
