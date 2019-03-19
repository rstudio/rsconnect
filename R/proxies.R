#' HTTP Proxy Configuration
#'
#' @description:
#'
#' If your system is behind an HTTP proxy then additional configuration may be
#' required to connect to the ShinyApp service. The required configuration
#' varies depending on what type of HTTP connection you are making to the
#' server.
#'
#' The default HTTP connection type is `libcurl` however addition connection types
#' `curl` and `internal` are also supported. The HTTP connection type is
#' configured using the [rsconnectOptions](rsconnect.http()] global option.
#'
#' @section HTTP Proxy Environment Variable:
#'
#' The most straightforward way to specify a proxy for `libcurl` and `curl`
#' connections is to set the \env{http_proxy} environment variable. For example,
#' you could add the following code to your `.rsconnect_profile`:
#'
#' \preformatted{
#' Sys.setenv(http_proxy = "http://proxy.example.com")
#' }
#'
#' Proxy settings can include a host-name, port, and username/password if necessary. The following
#' are all valid values for the `http_proxy` environment variable:
#'
#' \preformatted{
#' http://proxy.example.com/
#' http://proxy.example.com:1080/
#' http://username:password@proxy.example.com:1080/
#' }
#'
#' @section Setting Curl Proxy Options:
#'
#' The default HTTP connection type is `libcurl`. If you need more configurability
#' than afforded by the `http_proxy` environment variable you can specify 
#' proxy options explicity using `rsconnect.libcurl.options`. For example, you
#' could add the following code to your `.rsconnect_profile`:
#'
#' preformatted{
#' options(rsconnect.libcurl.options = list(proxy = "http://proxy.example.com")
#' }
#'
#' You can set any underlying curl option using this mechanism. Run
#' `curl::curl_options()` to see a list of options.
#'
#' @section Using Internet Explorer Proxy Settings:
#'
#' If you are running on Windows and have difficulty configuring proxy settings
#' for `libcurl` or `curl` connections, it's possible to re-use your Internet
#' Explorer proxy settings for connections to the server. To do this you set the
#' http connection type to `internal` as follows:
#'
#' \preformatted{
#' options(rsconnect.http = "internal")
#' }
#'
#' The `internal` connection type uses an insecure (non-encrypted) http
#' connection to the server. If you require an encrypted https connection it's
#' recommended that you use an `libcurl` or `curl` connection.
#'
#'
#' @name rsconnectProxies
#' NULL
