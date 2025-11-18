# Deploy a Plumber API

Deploys an application consisting of plumber API routes. The given
directory must contain a script returning a `plumb` object or a plumber
API definition.

Supported servers: Posit Connect and ShinyApps servers

## Usage

``` r
deployAPI(api, ...)
```

## Arguments

- api:

  Path to the API project directory. Must contain either `entrypoint.R`
  or `plumber.R` (for plumber APIs) or `_server.yml` (for plumber2 APIs)

- ...:

  Additional arguments to
  [`deployApp()`](https://rstudio.github.io/rsconnect/reference/deployApp.md).

## Details

Deploy a plumber API definition by either supplying a directory
containing `plumber.R` (an API definition) or `entrypoint.R` that
returns a `plumb` object created by
[`plumber::plumb()`](https://www.rplumber.io/reference/plumb.html). See
the plumber documentation for more information. Alternatively, deploy a
plumber2 API by supplying a directory containing `_server.yml`.

## See also

Other Deployment functions:
[`applications()`](https://rstudio.github.io/rsconnect/reference/applications.md),
[`deployApp()`](https://rstudio.github.io/rsconnect/reference/deployApp.md),
[`deployDoc()`](https://rstudio.github.io/rsconnect/reference/deployDoc.md),
[`deploySite()`](https://rstudio.github.io/rsconnect/reference/deploySite.md),
[`deployTFModel()`](https://rstudio.github.io/rsconnect/reference/deployTFModel.md)
