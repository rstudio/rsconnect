# Package Options

The rsconnect package supports several options that control the method
used for http communications, the printing of diagnostic information for
http requests, and the launching of an external browser after
deployment.

## Details

Supported global options include:

- `rsconnect.ca.bundle`:

  Path to a custom bundle of Certificate Authority root certificates to
  use when connecting to servers via SSL. This option can also be
  specied in the environment variable `RSCONNECT_CA_BUNDLE`. Leave
  undefined to use your system's default certificate store.

- `rsconnect.check.certificate`:

  Whether to check the SSL certificate when connecting to a remote host;
  defaults to `TRUE`. Setting to `FALSE` is insecure, but will allow you
  to connect to hosts using invalid certificates as a last resort.

- `rsconnect.http`:

  Http implementation used for connections to the back-end service:

  |            |                                                       |
  |------------|-------------------------------------------------------|
  | `libcurl`  | Secure https using the `curl` R package               |
  | `rcurl`    | Secure https using the `Rcurl` R package (deprecated) |
  | `curl`     | Secure https using the curl system utility            |
  | `internal` | Insecure http using raw sockets                       |

  If no option is specified then `libcurl` is used by default.

- `rsconnect.http.trace`:

  When `TRUE`, trace http calls (prints the method, path, and total
  milliseconds for each http request)

- `rsconnect.http.trace.json`:

  When `TRUE`, trace JSON content (shows JSON payloads sent to and
  received from the server))

- `rsconnect.http.verbose`:

  When `TRUE`, print verbose output for http connections (useful only
  for debugging SSL certificate or http connection problems)

- `rsconnect.tar`:

  By default, `rsconnect` uses R's internal `tar` implementation to
  compress content bundles. This may cause invalid bundles in some
  environments. In those cases, use this option to specify a path to an
  alternate `tar` executable. This option can also be specified in the
  environment variable `RSCONNECT_TAR`. Leave undefined to use the
  default `tar` implementation.

- `rsconnect.rcurl.options`:

  A named list of additional cURL options to use when using the RCurl
  HTTP implementation in R. Run
  [`RCurl::curlOptions()`](https://rdrr.io/pkg/RCurl/man/curlOptions.html)
  to see available options.

- `rsconnect.libcurl.options`:

  A named list of additional cURL options to use when using the curl
  HTTP implementation in R. Run
  [`curl::curl_options()`](https://jeroen.r-universe.dev/curl/reference/curl_options.html)
  to see available options.

- `rsconnect.error.trace`:

  When `TRUE`, print detailed stack traces for errors occurring during
  deployment.

- `rsconnect.launch.browser`:

  When `TRUE`, automatically launch a browser to view applications after
  they are deployed

- `rsconnect.locale.cache`:

  When `FALSE`, disable the detected locale cache (Windows only).

- `rsconnect.locale`:

  Override the detected locale.

- `rsconnect.max.bundle.size`:

  The maximum size, in bytes, for deployed content. If not set, defaults
  to 3 GB.

- `rsconnect.max.bundle.files`:

  The maximum number of files to deploy. If not set, defaults to 10,000.

- `rsconnect.force.update.apps`:

  When `TRUE`, bypasses the prompt to confirm whether you wish to update
  previously-deployed content

- `rsconnect.pre.deploy`:

  A function to run prior to deploying content; it receives as an
  argument the directory containing the content about to be deployed.

- `rsconnect.post.deploy`:

  A function to run after successfully deploying content; it receives as
  an argument the directory containing the content about to be deployed.

- `rsconnect.python.enabled`:

  When `TRUE`, use the python executable specified by the
  `RETICULATE_PYTHON` environment variable and add a `python` section to
  the deployment manifest. By default, python is enabled when deploying
  to Posit Connect and disabled when deploying to shinyapps.io.

When deploying content from the RStudio IDE, the rsconnect package's
deployment methods are executed in a vanilla R session that doesn't
execute startup scripts. This can make it challenging to ensure options
are set properly prior to push-button deployment, so the rsconnect
package has a parallel set of “startup” scripts it runs prior to
deploying. The follow are run in order, if they exist, prior to
deployment:

- `$R_HOME/etc/rsconnect.site`:

  Like `Rprofile.site`; for site-wide pre-flight and options.

- `~/.rsconnect_profile`:

  Like `.Rprofile`; for user-specific content.

- `$PROJECT/.rsconnect_profile`:

  Like `.Rprofile` for projects; `$PROJECT` here refers to the root
  directory of the content being deployed.

Note that, unlike `.Rprofile`, these files don't replace each other;
*all three* will be run if they exist.

## Examples

``` r
if (FALSE) { # \dontrun{

# use curl for http connections
options(rsconnect.http = "curl")

# trace http requests
options(rsconnect.http.trace = TRUE)

# print verbose output for http requests
options(rsconnect.http.verbose = TRUE)

# print JSON content
options(rsconnect.http.trace.json = TRUE)

# don't automatically launch a browser after deployment
options(rsconnect.launch.browser = FALSE)
} # }
```
