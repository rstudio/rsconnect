# Migrate a deployment record to Posit Connect Cloud

Rewrites a local deployment record to point at an existing Posit Connect
Cloud content item so that subsequent
[`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
calls (including the RStudio IDE Publish button) route to Connect Cloud
instead of the original server. The target content must already exist in
Connect Cloud — this function updates only the local `.dcf` file on
disk.

## Usage

``` r
migrateToConnectCloud(
  appPath = ".",
  contentId,
  cloudAccount = NULL,
  appName = NULL,
  account = NULL,
  server = NULL
)
```

## Arguments

- appPath:

  Path to the content directory. Defaults to the current working
  directory.

- contentId:

  The Connect Cloud content ID. Found in the content URL:
  `https://connect.posit.cloud/{account}/content/{contentId}`.

- cloudAccount:

  Local name of the Connect Cloud account to write the new record under.
  When `NULL` and exactly one Connect Cloud account is registered, it is
  used automatically.

- appName, account, server:

  Filters to disambiguate the source deployment record when `appPath`
  has records for multiple deployments. When no source record exists,
  `appName` names the reconstructed record; if omitted, the name is
  derived from the content title.

## Value

The path to the new deployment record file, invisibly.

## Details

The source deployment record is optional. When `appPath` has no local
record (for example, the `rsconnect/` directory was never checked in or
has been lost), the Connect Cloud record is reconstructed from
`contentId` alone. In that case the record name is taken from `appName`,
or derived from the content's title when `appName` is not supplied.

Supported servers: all (source) -\> Posit Connect Cloud (target)
