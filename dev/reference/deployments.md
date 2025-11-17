# List Application Deployments

List deployment records for a given application.

Supported servers: All servers

## Usage

``` r
deployments(
  appPath = ".",
  nameFilter = NULL,
  accountFilter = NULL,
  serverFilter = NULL,
  excludeOrphaned = TRUE
)
```

## Arguments

- appPath:

  The path to the content that was deployed, either a directory or an
  individual document.

- nameFilter:

  Return only deployments matching the given name (optional)

- accountFilter:

  Return only deployments matching the given account (optional)

- serverFilter:

  Return only deployments matching the given server (optional)

- excludeOrphaned:

  If `TRUE` (the default), return only deployments made by a currently
  registered account. Deployments made from accounts that are no longer
  registered (via
  e.g.[`removeAccount()`](https://rstudio.github.io/rsconnect/dev/reference/accounts.md))
  will not be returned.

## Value

Returns a data frame with at least following columns:

|                  |                                             |
|------------------|---------------------------------------------|
| `name`           | Name of deployed application                |
| `account`        | Account owning deployed application         |
| `bundleId`       | Identifier of deployed application's bundle |
| `url`            | URL of deployed application                 |
| `deploymentFile` | Name of configuration file                  |

If additional metadata has been saved with the deployment record using
the `metadata` argument to
[`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md),
the frame will include additional columns.

## See also

[`applications()`](https://rstudio.github.io/rsconnect/dev/reference/applications.md)
to get a list of deployments from the server, and
[`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md)
to create a new deployment.

## Examples

``` r
if (FALSE) { # \dontrun{

# Return all deployments of the ~/r/myapp directory made with the 'abc'
# account
deployments("~/r/myapp", accountFilter="abc")
} # }
```
