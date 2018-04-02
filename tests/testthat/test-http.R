context("http")

test_that("URL parsing works", {
  p <- parseHttpUrl("http://yahoo.com")
  expect_equal(p$protocol, "http")
  expect_equal(p$host, "yahoo.com")
  expect_equal(p$port, "")
  expect_equal(p$path, "") #TODO: bug? Should default to /?

  p <- parseHttpUrl("https://rstudio.com/about")
  expect_equal(p$protocol, "https")
  expect_equal(p$host, "rstudio.com")
  expect_equal(p$port, "")
  expect_equal(p$path, "/about")

  p <- parseHttpUrl("http://127.0.0.1:3939/stuff/here/?who-knows")
  expect_equal(p$protocol, "http")
  expect_equal(p$host, "127.0.0.1")
  expect_equal(p$port, "3939")
  expect_equal(p$path, "/stuff/here/?who-knows") #TODO: bug?
})

test_that("Parsing cookies works", {
  parsedUrl <- parseHttpUrl("http://rstudio.com/test/stuff")
  # Note that the Expires field is ignored
  cookie <- parseCookie(parsedUrl, "mycookie=myvalue; Path=/; Expires=Sat, 24 Jun 2017 16:16:05 GMT; Max-Age=3600; HttpOnly")
  expect_equal(cookie$name, "mycookie")
  expect_equal(cookie$value, "myvalue")
  expect_true(cookie$expires > Sys.time() + 3595)
  expect_true(cookie$expires < Sys.time() + 3605)
  expect_equal(cookie$path, "/")
  expect_false(cookie$secure)

  # Max-Age with no semicolon, non-root path
  cookie <- parseCookie(parsedUrl, "mycookie2=myvalue2; Secure; Path=/test; max-AGE=3600")
  expect_equal(cookie$name, "mycookie2")
  expect_equal(cookie$value, "myvalue2")
  expect_true(cookie$expires > Sys.time() + 3595)
  expect_true(cookie$expires < Sys.time() + 3605)
  expect_equal(cookie$path, "/test")
  expect_true(cookie$secure)

  # Path with no semicolon, no max age
  cookie <- parseCookie(parsedUrl, "mycookie2=myvalue2; PATH=/test")
  expect_equal(cookie$name, "mycookie2")
  expect_equal(cookie$value, "myvalue2")
  expect_true(cookie$expires > (Sys.time() + 10^9))
  expect_equal(cookie$path, "/test")
  expect_false(cookie$secure)

  # No path, value with no semicolon
  cookie <- parseCookie(parsedUrl, "mycookie2=myvalue2")
  expect_equal(cookie$name, "mycookie2")
  expect_equal(cookie$value, "myvalue2")
  expect_true(cookie$expires > (Sys.time() + 10^9))
  expect_equal(cookie$path, "/")
  expect_false(cookie$secure)

  # Trailing secure
  cookie <- parseCookie(parsedUrl, "mycookie2=myvalue2; Secure")
  expect_equal(cookie$name, "mycookie2")
  expect_equal(cookie$value, "myvalue2")
  expect_true(cookie$expires > (Sys.time() + 10^9))
  expect_equal(cookie$path, "/")
  expect_true(cookie$secure)

  # Full cookie with spaces around =s
  cookie <- parseCookie(parsedUrl, "mycookie = myvalue; SEcure; Path = /; Expires = Sat, 24 Jun 2017 16:16:05 GMT; Max-Age = 3600; HttpOnly")
  expect_equal(cookie$name, "mycookie")
  expect_equal(cookie$value, "myvalue")
  expect_true(cookie$secure)
  expect_true(cookie$expires > Sys.time() + 3595)
  expect_true(cookie$expires < Sys.time() + 3605)
  expect_equal(cookie$path, "/")

  # Value-less cookie
  cookie <- parseCookie(parsedUrl, "mycookie=; Path = /")
  expect_equal(cookie$name, "mycookie")
  expect_equal(cookie$value, "")
  expect_equal(cookie$path, "/")

  # prove #229 is fixed
  cookie <- parseCookie(parsedUrl, "my_cookie-which/uses%20strange?characters=foo_-%20+?1234bar; Path = /")
  expect_equal(cookie$name, "my_cookie-which/uses%20strange?characters")
  expect_equal(cookie$value, "foo_-%20+?1234bar")
  expect_equal(cookie$path, "/")
})

test_that("Invalid cookies fail parsing", {
  # Invalid path, doesn't match request's path
  parsedUrl <- parseHttpUrl("http://rstudio.com/test/stuff")
  expect_warning({cookie <- parseCookie(parsedUrl, "mycookie=myvalue; Path=/something/else")},
                 "Invalid path set for cookie")
  expect_null(cookie)

  # Invalid key/val format
  expect_warning({cookie <- parseCookie(parsedUrl, "mycookie;")},
                 "Unable to parse set-cookie ")
  expect_null(cookie)
})

clearCookieStore <- function(){
  if (exists("fakedomain:123", .cookieStore)){
    rm("fakedomain:123", envir=.cookieStore)
  }
}

test_that("cookies can be stored", {
  clearCookieStore()

  parsedUrl <- parseHttpUrl("http://fakedomain:123/test/stuff")
  expect_warning({
    storeCookies(parsedUrl, c(
      "mycookie=myvalue; Path=/; Max-Age=3600; HttpOnly",
      "anotherCookie=what; Path=/test; Max-Age=100",
      "wrongpath=huh; Path=/uhoh; Max-Age=100",
      "secureCookie=123; Secure",
      "third=cookie; Path=/; Max-Age=500")
    )
  }, "Invalid path set for cookie")
  cookies <- get("fakedomain:123", envir=.cookieStore)
  expect_equal(nrow(cookies), 4)

  # Check the first cookie
  co <- cookies[cookies$name=="mycookie",]
  expect_equal(co$name, "mycookie")
  expect_equal(co$value, "myvalue")
  expect_true(co$expires > Sys.time() + 3595)
  expect_true(co$expires < Sys.time() + 3605)
  expect_equal(co$path, "/")
  expect_false(co$secure)

  # And the other
  co <- cookies[cookies$name=="anotherCookie",]
  expect_equal(co$name, "anotherCookie")
  expect_equal(co$value, "what")
  expect_true(co$expires > Sys.time() + 95)
  expect_true(co$expires < Sys.time() + 105)
  expect_equal(co$path, "/test")
  expect_false(co$secure)

  # Third
  co <- cookies[cookies$name=="third",]
  expect_equal(co$name, "third")
  expect_equal(co$value, "cookie")
  expect_true(co$expires > Sys.time() + 495)
  expect_true(co$expires < Sys.time() + 505)
  expect_equal(co$path, "/")
  expect_false(co$secure)

  # Fourth
  co <- cookies[cookies$name=="secureCookie",]
  expect_equal(co$name, "secureCookie")
  expect_equal(co$value, "123")
  expect_true(co$secure)
})

test_that("duplicate cookies overwrite one another", {
  clearCookieStore()

  parsedUrl <- parseHttpUrl("http://fakedomain:123/test/stuff")
  storeCookies(parsedUrl, "mycookie=myvalue; Path=/; Max-Age=3600")
  cookies <- get("fakedomain:123", envir=.cookieStore)
  expect_equal(nrow(cookies), 1)

  # Add another valid cookie, same domain, same name, different path
  storeCookies(parsedUrl, "mycookie=myvalue; Path=/test; Max-Age=3600")
  cookies <- get("fakedomain:123", envir=.cookieStore)
  expect_equal(nrow(cookies), 2)

  # Duplicate cookie should overwrite
  storeCookies(parsedUrl, "mycookie=myvalue; Path=/test; Max-Age=99")
  cookies <- get("fakedomain:123", envir=.cookieStore)
  expect_equal(nrow(cookies), 2)

  # Confirm that the stored cookie is the more recent one.
  co <- cookies[cookies$path=="/test",]
  expect_true(co$expires > Sys.time() + 94)
  expect_true(co$expires < Sys.time() + 104)
})

test_that("appending cookie headers works", {
  clearCookieStore()

  parsedUrl <- parseHttpUrl("http://fakedomain:123/test/stuff")

  # Nothing to append, no-op
  headers <- appendCookieHeaders(parsedUrl, c(header1=123, header2="abc"))
  expect_length(headers, 2)
  expect_equivalent(headers["header1"], "123")
  expect_equivalent(headers["header2"], "abc")

  # Store a cookie
  storeCookies(parsedUrl, "cookie1=value1; Path=/; Max-Age=3600")

  headers <- appendCookieHeaders(parsedUrl, c(header1=123, header2="abc"))
  expect_length(headers, 3)
  expect_equivalent(headers["header1"], "123")
  expect_equivalent(headers["header2"], "abc")
  expect_equivalent(headers["cookie"], "cookie1=value1")

  # Store a couple more cookies
  storeCookies(parsedUrl, "cookie2=value2; Path=/test; Max-Age=3600")
  # This one has the wrong path, will be filtered out
  storeCookies(parseHttpUrl("http://fakedomain:123/another"), "cookie3=value3; Path=/another; Max-Age=3600")

  headers <- appendCookieHeaders(parsedUrl, c(header1=123, header2="abc"))
  expect_length(headers, 3)
  expect_equivalent(headers["header1"], "123")
  expect_equivalent(headers["header2"], "abc")
  expect_equivalent(headers["cookie"], "cookie2=value2; cookie1=value1")

  # If you already have a cookie header, you end up with two
  headers <- appendCookieHeaders(parsedUrl, c(cookie="existing=value"))
  expect_length(headers, 2)
  expect_equal(headers[1], c(cookie="existing=value"))
  expect_equal(headers[2], c(cookie="cookie2=value2; cookie1=value1"))

  # Add a secure cookie
  storeCookies(parsedUrl, "securecookie=secureval; Path=/; Max-Age=3600; Secure")
  headers <- appendCookieHeaders(parsedUrl, c())
  expect_equivalent(headers["cookie"], "cookie2=value2; cookie1=value1")

  # But over a secure channel, you'd include the secure cookie
  headers <- appendCookieHeaders(parseHttpUrl("https://fakedomain:123/test/stuff"), c())
  expect_equivalent(headers["cookie"], "securecookie=secureval; cookie2=value2; cookie1=value1")
})

test_that("Expired cookies are removed", {
  clearCookieStore()

  parsedUrl <- parseHttpUrl("http://fakedomain:123/test/stuff")

  # Expired cookies are removed from the store and not included in the request
  storeCookies(parsedUrl, "expired=something; Max-Age=-1")

  cookies <- get("fakedomain:123", envir=.cookieStore)
  expect_equal(nrow(cookies), 1)

  # Now it will recognize that it's expired and remove it
  headers <- appendCookieHeaders(parsedUrl, NULL)
  expect_null(headers)

  cookies <- get("fakedomain:123", envir=.cookieStore)
  expect_equal(nrow(cookies), 0)

  # And with multiple cookies, it still removes only the expired one
  storeCookies(parsedUrl, "expired=something; Max-Age=-1")
  storeCookies(parsedUrl, "notexpired=something")

  cookies <- get("fakedomain:123", envir=.cookieStore)
  expect_equal(nrow(cookies), 2)

  # Now it will recognize that it's expired and remove it
  headers <- appendCookieHeaders(parsedUrl, c())
  expect_length(headers, 1)

  cookies <- get("fakedomain:123", envir=.cookieStore)
  expect_equal(nrow(cookies), 1)
})

test_that("getCookieHost works", {
  expect_equal(getCookieHost(parseHttpUrl("http://rstudio.com")), "rstudio.com")
  expect_equal(getCookieHost(parseHttpUrl("http://rstudio.com:3939")), "rstudio.com:3939")
  expect_equal(getCookieHost(parseHttpUrl("http://127.0.0.1")), "127.0.0.1")
  expect_equal(getCookieHost(parseHttpUrl("http://127.0.0.1:3939")), "127.0.0.1:3939")
})

test_that("getting and clearing cookies works", {
  clearCookieStore()

  all <- getCookies()
  expect_null(all)

  # Add a few cookies
  domain1 <- parseHttpUrl("http://domain1/test/stuff")
  storeCookies(domain1, "c1=v1")
  storeCookies(domain1, "c2=v2")

  domain2 <- parseHttpUrl("http://domain2:3939/test/stuff")
  storeCookies(domain2, "c3=v3")

  d1c <- getCookies("domain1")
  expect_equal(nrow(d1c), 2)
  expect_equal(d1c$host[1], "domain1")

  d2c <- getCookies("domain2")
  expect_null(d2c)
  d2c <- getCookies("domain2", 3939)
  expect_equal(nrow(d2c), 1)
  expect_equal(d2c$host[1], "domain2:3939")

  all <- getCookies()
  expect_equal(nrow(all), 3)

  # Delete cookies from one domain
  clearCookies("domain2", 3939)

  d2c <- getCookies("domain2", 3939)
  expect_null(d2c)

  all <- getCookies()
  expect_equal(nrow(all), 2)

  # Clear all cookies
  clearCookies()

  d1c <- getCookies("domain1")
  expect_null(d1c)

  all <- getCookies()
  expect_null(all)

})
