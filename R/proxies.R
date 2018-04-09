#' HTTP Proxy Configuration
#'
#' @description:
#'
#' If your system is behind an HTTP proxy then additional configuration may be required to connect to the ShinyApp service. The required configuration varies depending on what type of HTTP connection you are making to the server.
#'
#' The default HTTP connection type is `rcurl` however addition connection types `curl` and `internal` are also supported. The HTTP connection type is configured using the [rsconnectOptions](rsconnect.http()] global option.
#'
#' @section HTTP Proxy Environment Variable:
#'
#' The most straightforward way to specify a proxy for `rcurl` and `curl` connections is to set the \env{http_proxy} environment variable. For example, you could add the following code to your `.Rprofile`:
#'
#' \preformatted{
#' Sys.setenv(http_proxy = "http://proxy.example.com")
#' }
#'
#' Proxy settings can include a host-name, port, and username/password if necessary. The following are all valid values for the `http_proxy` environment variable:
#'
#' \preformatted{
#' http://proxy.example.com/
#' http://proxy.example.com:1080/
#' http://username:password@proxy.example.com:1080/
#' }
#'
#' @section Setting RCurl Proxy Options:
#'
#' The default HTTP connection type is `rcurl`. If you need more configurability than affored by the `http_proxy` environment variable you can specify RCurl proxy options explicity using `RCurlOptions`. For example, you could add the following code to your `.Rprofile`:
#'
#' preformatted{
#' options(RCurlOptions = list(proxy = "http://proxy.example.com")
#' }
#'
#' You can set any underling curl option using this mechanism. To do this you translate curl options to lowercase and remove the `CURL_` prefix (for example, `CURLOPT_PROXYPORT` becomes `proxyport`).
#'
#' @section Using Internet Explorer Proxy Settings:
#'
#' If you are running on Windows and have difficulty configuring proxy settings for `rcurl` or `curl` connections, it's possible to re-use your Internet Explorer proxy settings for connections to the server. To do this you set the http connection type to `internal` as follows:
#'
#' \preformatted{
#' options(rsconnect.http = "internal")
#' }
#'
#' The `internal` connection type uses an insecure (non-encrypted) http connection to the server. If you require an encrypted https connection it's recommended that you use an `rcurl` or `curl` connection.
#'
#'
#' @name rsconnectProxies
#' NULL
