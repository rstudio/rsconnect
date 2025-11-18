# Generate Application Name

Generate a short name (identifier) for an application given an
application title.

## Usage

``` r
generateAppName(appTitle, appPath = NULL, account = NULL, unique = TRUE)
```

## Arguments

- appTitle:

  A descriptive title for the application.

- appPath:

  The path to the application's content, either a directory or an
  individual document. Optional.

- account:

  The account where the application will be deployed. Optional.

- unique:

  Whether to try to generate a unique name.

## Value

Returns a valid short name for the application.

## Details

This function modifies the title until it forms a suitable application
name. Suitable application names are 3 - 64 characters long and contain
only alphanumeric characters.

The function is intended to be used to find a name for a new
application. If `appPath` and `account` are both specified, then the
returned name will also be unique among locally known deployments of the
directory (note that it is not guaranteed to be unique on the server).
This behavior can be disabled by setting `unique = FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate a short name for a sample application
generateAppName("My Father's Country", "~/fathers-country", "myacct")
} # }
```
