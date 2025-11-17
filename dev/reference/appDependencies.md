# Detect application dependencies

`appDependencies()` recursively detects all R package dependencies for
an application by parsing all `.R` and `.Rmd` files and looking for
calls to [`library()`](https://rdrr.io/r/base/library.html),
[`require()`](https://rdrr.io/r/base/library.html),
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), `::`, and
so on. It then adds implicit dependencies (i.e. an `.Rmd` requires
Rmarkdown) and adds all recursive dependencies to create a complete
manifest of package packages need to be installed to run the app.

Supported servers: All servers

## Usage

``` r
appDependencies(
  appDir = getwd(),
  appFiles = NULL,
  appFileManifest = NULL,
  appMode = NULL
)
```

## Arguments

- appDir:

  A directory containing an application (e.g. a Shiny app or plumber
  API). Defaults to the current directory.

- appFiles, appFileManifest:

  Use `appFiles` to specify a character vector of files to bundle in the
  app or `appFileManifest` to provide a path to a file containing a list
  of such files. If neither are supplied, will bundle all files in
  `appDir`, apart from standard exclusions and files listed in a
  `.rscignore` file. See
  [`listDeploymentFiles()`](https://rstudio.github.io/rsconnect/dev/reference/listDeploymentFiles.md)
  for more details.

- appMode:

  Optional; the type of content being deployed. Provide this option when
  the inferred type of content is incorrect. This can happen, for
  example, when static HTML content includes a downloadable Shiny
  application `app.R`. Accepted values include `"shiny"`, `"api"`,
  `"rmd-static"`, `"rmd-shiny"`, `"quarto-static"`, `"quarto-shiny"`,
  and `"static"`. The Posit Connect API Reference contains a full set of
  available values. Not all servers support all types of content.

## Value

A data frame with one row for each dependency (direct, indirect, and
inferred), and 4 columns:

- `Package`: package name.

- `Version`: local version.

- `Source`: a short string describing the source of the package install,
  as described above.

- `Repository`: for CRAN and CRAN-like repositories, the URL to the
  repository. This will be ignored by the server if it has been
  configured with its own repository name -\> repository URL mapping.

## Dependency discovery

rsconnect use one of three mechanisms to find which packages your
application uses:

1.  If `renv.lock` is present, it will use the versions and sources
    defined in that file. If you're using the lockfile for some other
    purpose and don't want it to affect deployment, add `renv.lock` to
    `.rscignore`.

2.  Otherwise, rsconnect will call
    [`renv::snapshot()`](https://rstudio.github.io/renv/reference/snapshot.html)
    to find all packages used by your code. If you'd instead prefer to
    only use the packages declared in a `DESCRIPTION` file, run
    `renv::settings$snapshot.type("explicit")` to activate renv's
    "explicit" mode.

3.  Dependency resolution using renv is a new feature in rsconnect
    1.0.0, and while we have done our best to test it, it still might
    fail for your app. If this happens, please [file an
    issue](https://github.com/rstudio/rsconnect/issues) then set
    `options(rsconnect.packrat = TRUE)` to revert to the old dependency
    discovery mechanism.

## Remote installation

When deployed, the app must first install all of these packages, and
rsconnect ensures the versions used on the server will match the
versions you used locally. It knows how to install packages from the
following sources:

- CRAN and BioConductor (`Source: CRAN` or `Source: Bioconductor`). The
  remote server will ignore the specific CRAN or Bioconductor mirror
  that you use locally, always using the CRAN/BioC mirror that has been
  configured on the server.

- Other CRAN like and CRAN-like repositories. These packages will have a
  `Source` determined by the value of `getOptions("repos")`. For
  example, if you've set the following options:

      options(
         repos = c(
           CRAN = "https://cran.rstudio.com/",
           CORPORATE = "https://corporate-packages.development.company.com"
         )
      )

  Then packages installed from your corporate package repository will
  have source `CORPORATE`. Posit Connect [can be
  configured](https://docs.posit.co/connect/admin/appendix/configuration/#RPackageRepository)
  to override their repository url so that (e.g.) you can use different
  packages versions on staging and production servers.

- Packages installed from GitHub, GitLab, or BitBucket, have `Source`
  `github`, `gitlab`, and `bitbucket` respectively. When deployed, the
  bundle contains the additional metadata needed to precisely recreated
  the installed version.

It's not possible to recreate the packages that you have built and
installed from a directory on your local computer. This will have
`Source: NA` and will cause the deployment to error. To resolve this
issue, you'll need to install from one of the known sources described
above.

## Suggested packages

The `Suggests` field is not included when determining recursive
dependencies, so it's possible that not every package required to run
your application will be detected.

For example, ggplot2's `geom_hex()` requires the hexbin package to be
installed, but it is only suggested by ggplot2. So if your app uses
`geom_hex()` it will fail, reporting that the hexbin package is not
installed.

You can overcome this problem with (e.g.)
[`requireNamespace(hexbin)`](https://rdrr.io/r/base/ns-load.html). This
will tell rsconnect that your app needs the hexbin package, without
otherwise affecting your code.

## See also

[rsconnectPackages](https://rstudio.github.io/rsconnect/dev/reference/rsconnectPackages.md)(Using
Packages with rsconnect)

## Examples

``` r
if (FALSE) { # \dontrun{

# dependencies for the app in the current working dir
appDependencies()

# dependencies for an app in another directory
appDependencies("~/projects/shiny/app1")
} # }
```
