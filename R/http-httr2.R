# httr2-based HTTP backend for rsconnect
#
# These functions are called when getOption("rsconnect.httr2") is TRUE.
# They return httr2_response objects, letting httr2 handle cookies,
# redirects, and other HTTP concerns natively.

httr2Request <- function(
  service,
  authInfo,
  method,
  path,
  headers = list(),
  timeout = NULL,
  certificate = NULL,
  contentType = NULL,
  file = NULL
) {
  service$path <- path
  url <- buildHttpUrl(service)

  # Store user-specified cookies and append to headers (same as libcurl backend)
  headers <- appendCookieHeaders(service, headers)

  # No pipe, for compatibility with older R versions
  req <- httr2::request(url)
  req <- httr2::req_method(req, method)
  req <- httr2::req_headers(req, !!!headers)
  req <- httr2::req_user_agent(req, userAgent())

  # Apply more options

  # SSL certificate handling
  if (isFALSE(getOption("rsconnect.check.certificate", TRUE))) {
    req <- httr2::req_options(req, ssl_verifypeer = FALSE)
  } else if (!is.null(certificate)) {
    req <- httr2::req_options(req, cainfo = certificate)
  }
  # User-specified libcurl options
  user_opts <- getOption("rsconnect.libcurl.options")
  if (is.list(user_opts)) {
    req <- httr2::req_options(req, !!!user_opts)
  }

  # Timeout
  if (!is.null(timeout)) {
    req <- httr2::req_timeout(req, timeout)
  }

  # Verbose output
  if (httpVerbose()) {
    req <- httr2::req_verbose(req)
  }

  # Add file body with progress for large files
  if (!is.null(file)) {
    file_size <- file.info(file)$size
    req <- httr2::req_body_file(req, file, type = contentType)
    if (is_interactive() && file_size >= 10 * 1024^2) {
      req <- httr2::req_progress(req)
    }
  }

  # Don't error on HTTP failures - we handle that in handleResponse
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  httpTrace(method, path, httr2::resp_timing(resp)$total)

  # Store cookies from response (same as libcurl backend)
  cookieHeaders <- httr2::resp_headers(resp, "set-cookie")
  storeCookies(service, cookieHeaders)

  resp
}

# Convert httr2 response to the list format expected by handleResponse
httr2_response_to_list <- function(resp) {
  # Parse the final URL (after any redirects) to build the req structure
  final_url <- httr2::url_parse(resp$url)

  # Handle empty bodies (httr2::resp_body_string errors on empty body)
  content <- tryCatch(
    httr2::resp_body_string(resp),
    error = function(e) ""
  )

  list(
    req = list(
      protocol = final_url$scheme,
      host = final_url$hostname,
      port = final_url$port %||% "",
      path = final_url$path
    ),
    status = httr2::resp_status(resp),
    location = httr2::resp_header(resp, "location"),
    contentType = httr2::resp_content_type(resp) %||% "text/plain",
    content = content
  )
}
