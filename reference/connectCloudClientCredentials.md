# Register a Posit Connect Cloud account using OAuth client credentials

`connectCloudClientCredentials()` registers a Posit Connect Cloud
account using an OAuth 2.0 `client_credentials` grant provided by the
Posit Cloud auth service
(https://login.posit.cloud/identity/credentials). Use this function to
authenticate in non-interactive contexts.

Supported servers: Posit Connect Cloud servers

## Usage

``` r
connectCloudClientCredentials(
  clientId,
  clientSecret,
  accountName,
  name = NULL,
  quiet = FALSE
)
```

## Arguments

- clientId:

  The OAuth client ID issued for a Posit Connect Cloud service account.

- clientSecret:

  The OAuth client secret paired with `clientId`.

- accountName:

  The Posit Connect Cloud account name to publish to. The credentials
  must grant publish permission on this account.

- name:

  The local name to record the account under. Defaults to `accountName`.

- quiet:

  Whether or not to show messages while connecting the account.

## See also

Other Account functions:
[`accounts()`](https://rstudio.github.io/rsconnect/reference/accounts.md),
[`connectApiUser()`](https://rstudio.github.io/rsconnect/reference/connectApiUser.md),
[`connectCloudUser()`](https://rstudio.github.io/rsconnect/reference/connectCloudUser.md),
[`setAccountInfo()`](https://rstudio.github.io/rsconnect/reference/setAccountInfo.md)
