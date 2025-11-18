# List Deployed Applications

List all applications currently deployed for a given account.

Supported servers: All servers

## Usage

``` r
applications(account = NULL, server = NULL)
```

## Arguments

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

  Use
  [`accounts()`](https://rstudio.github.io/rsconnect/reference/accounts.md)
  to see the full list of available options.

## Value

Returns a data frame with the following columns:

|              |                                                                                                                    |
|--------------|--------------------------------------------------------------------------------------------------------------------|
| `id`         | Application unique id                                                                                              |
| `name`       | Name of application                                                                                                |
| `title`      | Application title                                                                                                  |
| `url`        | URL where application can be accessed                                                                              |
| `status`     | Current status of application. Valid values are `pending`, `deploying`, `running`, `terminating`, and `terminated` |
| `size`       | Instance size (small, medium, large, etc.) (on ShinyApps.io)                                                       |
| `instances`  | Number of instances (on ShinyApps.io)                                                                              |
| `config_url` | URL where application can be configured                                                                            |

## Note

To register an account you call the
[`setAccountInfo()`](https://rstudio.github.io/rsconnect/reference/setAccountInfo.md)
function.

## See also

[`deployApp()`](https://rstudio.github.io/rsconnect/reference/deployApp.md),
[`terminateApp()`](https://rstudio.github.io/rsconnect/reference/terminateApp.md)

Other Deployment functions:
[`deployAPI()`](https://rstudio.github.io/rsconnect/reference/deployAPI.md),
[`deployApp()`](https://rstudio.github.io/rsconnect/reference/deployApp.md),
[`deployDoc()`](https://rstudio.github.io/rsconnect/reference/deployDoc.md),
[`deploySite()`](https://rstudio.github.io/rsconnect/reference/deploySite.md),
[`deployTFModel()`](https://rstudio.github.io/rsconnect/reference/deployTFModel.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# list all applications for the default account
applications()

# list all applications for a specific account
applications("myaccount")

# view the list of applications in the data viewer
View(applications())
} # }
```
