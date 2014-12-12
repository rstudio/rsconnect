\name{rsconnectProxies}
\alias{rsconnectProxies}

\title{HTTP Proxy Configuration}

\description{
If your system is behind an HTTP proxy then additional configuration may be required to connect to the ShinyApp service. The required configuration varies depending on what type of HTTP connection you are making to the server.

The default HTTP connection type is \code{rcurl} however addition connection types \code{curl} and \code{internal} are also supported. The HTTP connection type is configured using the \code{\link[rsconnect:rsconnectOptions]{rsconnect.http}} global option.
}

\section{HTTP Proxy Environment Variable}{

The most straightforward way to specify a proxy for \code{rcurl} and \code{curl} connections is to set the \env{http_proxy} environment variable. For example, you could add the following code to your \code{.Rprofile}:

\preformatted{
Sys.setenv(http_proxy = "http://proxy.example.com")
}

Proxy settings can include a host-name, port, and username/password if necessary. The following are all valid values for the \code{http_proxy} environment variable:

\preformatted{
http://proxy.example.com/
http://proxy.example.com:1080/
http://username:password@proxy.example.com:1080/
}

}

\section{Setting RCurl Proxy Options}{

The default HTTP connection type is \code{rcurl}. If you need more configurability than affored by the \code{http_proxy} environment variable you can specify RCurl proxy options explicity using \code{RCurlOptions}. For example, you could add the following code to your \code{.Rprofile}:

\preformatted{
options(RCurlOptions = list(proxy = "http://proxy.example.com")
}

You can set any underling curl option using this mechanism. To do this you translate curl options to lowercase and remove the \code{CURL_} prefix (for example, \code{CURLOPT_PROXYPORT} becomes \code{proxyport}).

A list of available curl options can be found here: \href{http://curl.haxx.se/libcurl/c/curl_easy_setopt.html}{curl http proxy options}.

}

\section{Using Internet Explorer Proxy Settings}{

If you are running on Windows and have difficulty configuring proxy settings for \code{rcurl} or \code{curl} connections, it's possible to re-use your Internet Explorer proxy settings for connections to the server. To do this you set the http connection type to \code{internal} as follows:

\preformatted{
options(rsconnect.http = "internal")
}

There are two things to keep in mind when using \code{internal} connection types:

\enumerate{
  \item The \code{internal} connection type uses an insecure (non-encrypted) http connection to the server. If you require an encrypted https connection it's recommended that you use an \code{rcurl} or \code{curl} connection.
  \item To take advantage of Internet Explorer proxy settings you must be using the \code{internet2} routines via the \code{setInternet2} function (this is the default within RStudio on Windows). You can check whether \code{internet2} is enabled by executing the following code: \code{ setInternet2(use = NA)}
}

}

