# Register account on Posit Connect Cloud

`connectCloudUser()` connects your Posit Connect Cloud account to the
rsconnect package so that it can deploy and manage applications on your
behalf. It will open a browser window to authenticate, then prompt you
to create an account or select an account to use if you have multiple.

Supported servers: Posit Connect Cloud servers

## Usage

``` r
connectCloudUser(launch.browser = TRUE)
```

## Arguments

- launch.browser:

  If true, the system's default web browser will be launched
  automatically after the app is started. Defaults to `TRUE` in
  interactive sessions only. If a function is passed, it will be called
  after the app is started, with the app URL as a parameter.

## See also

Other Account functions:
[`accounts()`](https://rstudio.github.io/rsconnect/reference/accounts.md),
[`connectApiUser()`](https://rstudio.github.io/rsconnect/reference/connectApiUser.md),
[`setAccountInfo()`](https://rstudio.github.io/rsconnect/reference/setAccountInfo.md)
