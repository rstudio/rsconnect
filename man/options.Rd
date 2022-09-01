\name{rsconnectOptions}
\alias{rsconnectOptions}

\title{Package Options}

\description{
The \pkg{rsconnect} package supports several options that control the method used for http communications, the printing of diagnostic information for http requests, and the launching of an external browser after deployment.
}

\details{
Supported global options include:
\describe{
   \item{\code{rsconnect.ca.bundle}}{Path to a custom bundle of Certificate Authority root certificates to use when connecting to servers via SSL. This option can also be specied in the environment variable \code{RSCONNECT_CA_BUNDLE}. Leave undefined to use your system's default certificate store.}
   \item{\code{rsconnect.check.certificate}}{Whether to check the SSL certificate when connecting to a remote host; defaults to \code{TRUE}. Setting to \code{FALSE} is insecure, but will allow you to connect to hosts using invalid certificates as a last resort.}
   \item{\code{rsconnect.http}}{Http implementation used for connections to the back-end service:
   \tabular{ll}{
     \code{libcurl} \tab Secure https using the \code{curl} R package\cr
     \code{rcurl} \tab Secure https using the \code{Rcurl} R package (deprecated)\cr
     \code{curl} \tab Secure https using the curl system utility \cr
     \code{internal} \tab Insecure http using raw sockets\cr
   }
   If no option is specified then \code{libcurl} is used by default.
   }
   \item{\code{rsconnect.http.trace}}{When \code{TRUE}, trace http calls (prints the method, path, and total milliseconds for each http request)}
   \item{\code{rsconnect.http.trace.json}}{When \code{TRUE}, trace JSON content (shows JSON payloads sent to and received from the server))}
   \item{\code{rsconnect.http.verbose}}{When \code{TRUE}, print verbose output for http connections (useful only for debugging SSL certificate or http connection problems)}
   \item{\code{rsconnect.tar}}{By default, \code{rsconnect} uses R's internal \code{tar} implementation to compress content bundles. This may cause invalid bundles in some environments. In those cases, use this option to specify a path to an alternate \code{tar} executable. This option can also be specified in the environment variable \code{RSCONNECT_TAR}. Leave undefined to use the default \code{tar} implementation.}
   \item{\code{rsconnect.rcurl.options}}{A named list of additional cURL options to use when using the RCurl HTTP implementation in R. Run \code{RCurl::curlOptions()} to see available options.}
   \item{\code{rsconnect.libcurl.options}}{A named list of additional cURL options to use when using the curl HTTP implementation in R. Run \code{curl::curl_options()} to see available options.}
   \item{\code{rsconnect.error.trace}}{{When \code{TRUE}, print detailed stack traces for errors occurring during deployment.}}
   \item{\code{rsconnect.launch.browser}}{When \code{TRUE}, automatically launch a browser to view applications after they are deployed}
   \item{\code{rsconnect.locale.cache}}{When \code{FALSE}, disable the detected locale cache (Windows only). }
   \item{\code{rsconnect.locale}}{Override the detected locale. }
   \item{\code{rsconnect.max.bundle.size}}{The maximum size, in bytes, for deployed content. If not set, defaults to 3 GB.}
   \item{\code{rsconnect.max.bundle.files}}{The maximum number of files to deploy. If not set, defaults to 10,000.}
   \item{\code{rsconnect.force.update.apps}}{When \code{TRUE}, bypasses the prompt to confirm whether you wish to update previously-deployed content}
   \item{\code{rsconnect.pre.deploy}}{A function to run prior to deploying content; it receives as an argument the path to the content that's about to be deployed.}
   \item{\code{rsconnect.post.deploy}}{A function to run after successfully deploying content; it receives as an argument the path to the content that was just deployed.}
   \item{\code{rsconnect.python.enabled}}{When \code{TRUE}, use the python executable specified by the \code{RETICULATE_PYTHON} environment variable and add a \code{python} section to the deployment manifest. By default, python is enabled when deploying to Posit Connect and disabled when deploying to shinyapps.io.}
}
When deploying content from the RStudio IDE, the rsconnect package's deployment methods are executed in a vanilla R session that doesn't execute startup scripts. This can make it challenging to ensure options are set properly prior to push-button deployment, so the rsconnect package has a parallel set of ``startup'' scripts it runs prior to deploying. The follow are run in order, if they exist, prior to deployment:
\describe{
    \item{\code{$R_HOME/etc/rsconnect.site}}{Like \code{Rprofile.site}; for site-wide pre-flight and options.}
    \item{\code{~/.rsconnect_profile}}{Like \code{.Rprofile}; for user-specific content.}
    \item{\code{$PROJECT/.rsconnect_profile}}{Like \code{.Rprofile} for projects; \code{$PROJECT} here refers to the root directory of the content being deployed.}
}
Note that, unlike \code{.Rprofile}, these files don't replace each other; \emph{all three} will be run if they exist.
}

\examples{
\dontrun{

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
}
}

