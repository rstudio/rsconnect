# Register account on Posit Connect

`connectUser()` and `connectApiUser()` connect your Posit Connect
account to the rsconnect package so that it can deploy and manage
applications on your behalf.

`connectUser()` is the easiest place to start because it allows you to
authenticate in-browser to your Posit Connect server. `connectApiUser()`
is appropriate for non-interactive settings; you'll need to
copy-and-paste the API key from your account settings.

Supported servers: Posit Connect servers

## Usage

``` r
connectApiUser(account = NULL, server = NULL, apiKey, quiet = FALSE)

connectUser(
  account = NULL,
  server = NULL,
  quiet = FALSE,
  launch.browser = getOption("rsconnect.launch.browser", interactive())
)
```

## Arguments

- account:

  A name for the account to connect.

- server:

  The server to connect to.

- apiKey:

  The API key used to authenticate the user

- quiet:

  Whether or not to show messages and prompts while connecting the
  account.

- launch.browser:

  If true, the system's default web browser will be launched
  automatically after the app is started. Defaults to `TRUE` in
  interactive sessions only. If a function is passed, it will be called
  after the app is started, with the app URL as a parameter.

## See also

Other Account functions:
[`accounts()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md),
[`connectCloudUser()`](https://rstudio.github.io/rsconnect/dev/reference/connectCloudUser.md),
[`setAccountInfo()`](https://rstudio.github.io/rsconnect/dev/reference/setAccountInfo.md)
