\name{rsconnectOptions}
\alias{rsconnectOptions}

\title{Package Options}

\description{
The \pkg{rsconnect} package supports several options that control the method used for http communications, the printing of diagnostic information for http requests, and the launching of an external browser after deployment.
}

\details{
Supported global options include:
\describe{
   \item{\code{rsconnect.http}}{Http implementation used for connections to the back-end service:
   \tabular{ll}{
     \code{rcurl} \tab Secure https using the \code{RCurl} package\cr
     \code{curl} \tab Secure https using the curl system utility \cr
     \code{internal} \tab Insecure http using raw sockets\cr
   }
   If no option is specified then \code{rcurl} is used by default.
   }
   \item{\code{rsconnect.http.trace}}{When \code{TRUE}, trace http calls (prints the method, path, and total milliseconds for each http request)}
   \item{\code{rsconnect.http.trace.json}}{When \code{TRUE}, trace JSON content (shows JSON payloads sent to and received from the server))}
   \item{\code{rsconnect.http.verbose}}{When \code{TRUE}, print verbose output for http connections (useful only for debugging SSL certificate or http connection problems)}
   \item{\code{rsconnect.launch.browser}}{When \code{TRUE}, automatically launch a browser to view applications after they are deployed}
   \item{\code{rsconnect.locale.cache}}{When \code{FALSE}, disable the detected locale cache (Windows only). }
   \item{\code{rsconnect.locale}}{Override the detected locale. }
}
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

