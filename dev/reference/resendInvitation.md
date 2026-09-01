# Resend invitation for invited users of an application

Resend invitation for invited users of an application

Supported servers: ShinyApps, Posit Connect Cloud

## Usage

``` r
resendInvitation(
  invite,
  regenerate = FALSE,
  appDir = getwd(),
  appName = NULL,
  contentId = NULL,
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

## Note

This function works for ShinyApps and Posit Connect Cloud. The
invitation can be selected by id or email address. On Posit Connect
Cloud, the `regenerate` argument has no effect.

On Posit Connect Cloud, the content is resolved from the local
deployment record under `appDir`, which defaults to the working
directory. Pass `appDir` to point at the project directory that contains
the `rsconnect/` deployment record. `appName` selects among multiple
records in the same directory. Alternatively, pass `contentId` to target
the content directly, without a local deployment record.

## See also

[`showInvited()`](https://rstudio.github.io/rsconnect/dev/reference/showInvited.md)
