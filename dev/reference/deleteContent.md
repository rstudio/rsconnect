# Delete content

Permanently delete content from Posit Connect Cloud. The deletion cannot
be undone.

Supported servers: Posit Connect Cloud

## Usage

``` r
deleteContent(
  appDir = getwd(),
  appName = NULL,
  contentId = NULL,
  account = NULL,
  server = NULL,
  force = FALSE
)
```

## Arguments

- appDir:

  Directory containing the content's deployment record. Defaults to the
  current working directory.

- appName:

  Name of the deployment record to use when `appDir` has more than one.

- contentId:

  The content ID to delete, taken from the content URL
  (`https://connect.posit.cloud/{account}/content/{contentId}`). When
  supplied, `appDir` and `appName` are ignored and no local deployment
  record is required.

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

- force:

  If `FALSE` (the default), ask for confirmation before deleting. Set to
  `TRUE` to delete without asking, which is required in non-interactive
  sessions.

## Note

This function only works for Posit Connect Cloud. When the content is
found through a local deployment record, that record is also removed.

## See also

[`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md),
[`applications()`](https://rstudio.github.io/rsconnect/dev/reference/applications.md),
and
[`forgetDeployment()`](https://rstudio.github.io/rsconnect/dev/reference/forgetDeployment.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# delete the content deployed from the current directory
deleteContent()

# delete content by id, without asking for confirmation
deleteContent(contentId = "0192f1a2-...", force = TRUE)
} # }
```
