# List authorized users for an application

List authorized users for an application

Supported servers: ShinyApps, Posit Connect Cloud

## Usage

``` r
showUsers(
  appDir = getwd(),
  appName = NULL,
  contentId = NULL,
  account = NULL,
  server = NULL
)
```

## Arguments

- appDir:

  Directory containing application. Defaults to current working
  directory.

- appName:

  Name of application.

- contentId:

  On Posit Connect Cloud, the content ID to manage, taken from the
  content URL
  (`https://connect.posit.cloud/{account}/content/{contentId}`). When
  supplied, `appDir` and `appName` are ignored and no local deployment
  record is required. Not supported on shinyapps.io.

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

  Use
  [`accounts()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md)
  to see the full list of available options.

## Value

A data frame with one row per authorized user. Columns always present:
`id`, `email`, `account`. The `account` column is populated on
shinyapps.io only and is `NA` on Posit Connect Cloud. On Posit Connect
Cloud the data frame additionally includes `display_name` and `role`
(e.g. `"viewer"` or `"collaborator"`) from the API response.

## Note

This function works for ShinyApps and Posit Connect Cloud.

On Posit Connect Cloud, the content is resolved from the local
deployment record under `appDir`, which defaults to the working
directory. Pass `appDir` to point at the project directory that contains
the `rsconnect/` deployment record. `appName` selects among multiple
records in the same directory. Alternatively, pass `contentId` to target
the content directly, without a local deployment record.

## See also

[`addAuthorizedUser()`](https://rstudio.github.io/rsconnect/dev/reference/addAuthorizedUser.md)
and
[`showInvited()`](https://rstudio.github.io/rsconnect/dev/reference/showInvited.md)
