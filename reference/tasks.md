# List Tasks

List Tasks

Supported servers: ShinyApps servers

## Usage

``` r
tasks(account = NULL, server = NULL)
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

|                 |                     |
|-----------------|---------------------|
| `id`            | Task id             |
| `action`        | Task action         |
| `status`        | Current task status |
| `created_time`  | Task creation time  |
| `finished_time` | Task finished time  |

## Note

This function works only with shinyapps.io.

## See also

[`taskLog()`](https://rstudio.github.io/rsconnect/reference/taskLog.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# list tasks for the default account
tasks()

} # }
```
