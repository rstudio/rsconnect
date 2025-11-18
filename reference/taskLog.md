# Show task log

Writes the task log for the given task

Supported servers: ShinyApps servers

## Usage

``` r
taskLog(taskId, account = NULL, server = NULL, output = NULL)
```

## Arguments

- taskId:

  Task Id

- account, server:

  Uniquely identify a remote server with either your user `account`, the
  `server` name, or both. If neither are supplied, and there are
  multiple options, you'll be prompted to pick one.

  Use
  [`accounts()`](https://rstudio.github.io/rsconnect/reference/accounts.md)
  to see the full list of available options.

- output:

  Where to write output. Valid values are `NULL` or `stderr`

## Note

This function works only with shinyapps.io.

## See also

[`tasks()`](https://rstudio.github.io/rsconnect/reference/tasks.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# write task log to stdout
taskLog(12345)

# write task log to stderr
taskLog(12345, output="stderr")

} # }
```
