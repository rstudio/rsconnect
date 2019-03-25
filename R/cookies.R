
# Environment in which cookies will be stored. Cookies are expected to survive
# the duration of the R session, but are not persisted outside of the R
# session.
.cookieStore <- new.env(parent=emptyenv())

# Returns the cookies associated with a particular host/port
# If no hostname is specified, returns all cookies
getCookies <- function(hostname, port=NULL){
  if (missing(hostname)){
    hosts <- ls(envir=.cookieStore)
    cookies <- lapply(hosts, function(h){
      getCookiesHostname(h)
    })
    do.call("rbind", cookies)
  } else {
    host <- getCookieHost(list(host=hostname, port=port))
    getCookiesHostname(host)
  }
}

# Get cookies for a particular hostname(:port)
getCookiesHostname <- function(host){
  if (!exists(host, .cookieStore)){
    NULL
  } else {
    cookies <- get(host, envir=.cookieStore)
    cookies$host <- host
    cookies
  }
}

# Clears the cookies associated with a particular hostname/port combination.
# If hostname and port are omitted, clears all the cookies
clearCookies <- function(hostname, port=NULL){
  if (missing(hostname)){
    rm(list=ls(envir=.cookieStore), envir=.cookieStore)
  } else {
    host <- getCookieHost(list(host=hostname, port=port))
    rm(list=host, envir=.cookieStore)
  }
}

# Parse out the raw headers provided and insert them into the cookieStore
# NOTE: Domain attribute is currently ignored
# @param requestURL the parsed URL as returned from `parseHttpUrl`
# @param cookieHeaders a list of characters strings representing the raw
#   Set-Cookie header value with the "Set-Cookie: " prefix omitted
storeCookies <- function(requestURL, cookieHeaders){
  cookies <- lapply(cookieHeaders, function(co){ parseCookie(requestURL, co) })

  # Filter out invalid cookies (which would return as NULL)
  cookies <- Filter(Negate(is.null), cookies)

  host <- getCookieHost(requestURL)

  hostCookies <- NULL
  if (!exists(host, .cookieStore)){
    # Create a new data frame for this host
    hostCookies <- data.frame(
      path=character(0),
      name=character(0),
      value=character(0),
      secure=logical(0),
      expires=character(0),
      stringsAsFactors = FALSE
    )
  } else {
    hostCookies <- get(host, envir=.cookieStore)
  }

  lapply(cookies, function(co){
    # Remove any duplicates
    # RFC says duplicate cookies are ones that have the same domain, name, and path
    hostCookies <<- hostCookies[!(co$name == hostCookies$name & co$path == hostCookies$path),]

    # append this new cookie on
    hostCookies <<- rbind(as.data.frame(co, stringsAsFactors=FALSE), hostCookies)
  })

  # Save this host's cookies into the cookies store.
  assign(host, hostCookies, envir=.cookieStore)
}

# Parse out an individual cookie
# @param requestURL the parsed URL as returned from `parseHttpUrl`
# @param cookieHeader the raw text contents of the Set-Cookie header with the
#   header name omitted.
parseCookie <- function(requestURL, cookieHeader){
  keyval <- regmatches(cookieHeader, regexec(
    # https://curl.haxx.se/rfc/cookie_spec.html
    # "characters excluding semi-colon, comma and white space"
    # white space is not excluded from values so we can capture `expires`
    "^([^;=, ]+)\\s*=\\s*([^;,]*)(;|$)", cookieHeader, ignore.case=TRUE))[[1]]
  if (length(keyval) == 0){
    # Invalid cookie format.
    warning("Unable to parse set-cookie header: ", cookieHeader)
    return(NULL)
  }
  key <- keyval[2]
  val <- keyval[3]

  # Path
  path <- regmatches(cookieHeader, regexec(
    "^.*\\sPath\\s*=\\s*([^;]+)(;|$).*$", cookieHeader, ignore.case=TRUE))[[1]]
  if (length(path) == 0){
    path <- "/"
  } else {
    path <- path[2]
  }
  if (!substring(requestURL$path, 1, nchar(path)) == path){
    # Per the RFC, the cookie's path must be a prefix of the request URL
    warning("Invalid path set for cookie on request for '", requestURL$path, "': ", cookieHeader)
    return(NULL)
  }

  # MaxAge
  maxage <- regmatches(cookieHeader, regexec(
    "^.*\\sMax-Age\\s*=\\s*(-?\\d+)(;|$).*$", cookieHeader, ignore.case=TRUE))[[1]]
  # If no maxage specified, then this is a session cookie, which means that
  # (since our cookies only survive for a single session anyways...) we should
  # keep this cookie around as long as we're alive.
  expires <- Sys.time() + 10^10
  if (length(maxage) > 0){
    # Compute time maxage seconds from now
    expires <- Sys.time() + as.numeric(maxage[2])
  }

  # Secure
  secure <- grepl(";\\s+Secure(;|$)", cookieHeader, ignore.case=TRUE)

  list(name=key,
       value=val,
       expires=expires,
       path=path,
       secure=secure)
}

# Appends a cookie header from the .cookieStore to the existing set of headers
# @param requestURL the parsed URL as returned from `parseHttpUrl`
# @param headers a named character vector containing the set of headers to be extended
appendCookieHeaders <- function(requestURL, headers){
  host <- getCookieHost(requestURL)

  if (!exists(host, .cookieStore)){
    # Nothing to do
    return(headers)
  }

  cookies <- get(host, envir=.cookieStore)

  # If any cookies are expired, remove them from the cookie store
  if (any(cookies$expires < as.integer(Sys.time()))){
    cookies <- cookies[cookies$expires >= as.integer(Sys.time()),]
    # Update the store, removing the expired cookies
    assign(host, cookies, envir=.cookieStore)
  }

  if (nrow(cookies) == 0){
    # Short-circuit, return unmodified headers.
    return(headers)
  }

  # Filter to only include cookies that match the path prefix
  cookies <- cookies[substring(requestURL$path, 1, nchar(cookies$path)) == cookies$path,]

  # If insecure channel, filter out secure cookies
  if(tolower(requestURL$protocol) != "https"){
    cookies <- cookies[!cookies$secure,]
  }

  # TODO: Technically per the RFC we're supposed to order these cookies by which
  # paths most specifically match the request.
  cookieHeader <- paste(apply(cookies, 1,
                              function(x){ paste0(x["name"], "=", x["value"]) }), collapse="; ")

  if (nrow(cookies) > 0){
    return(c(headers, cookie=cookieHeader))
  } else {
    # Return unmodified headers
    return(headers)
  }
}

getCookieHost <- function(requestURL){
  host <- requestURL$host
  port <- requestURL$port
  if (!is.null(port) && nchar(port) > 0){
    port <- sub("^:", "", port)
    # By my reading of the RFC, we technically only need to include the port #
    # in the index if the host is an IP address. But here we're including the
    # port number as a part of the host whether using a domain name or IP.
    # Erring on the side of not sending the cookies to the wrong services
    host <- paste(host, port, sep=":")
  }
  host
}

