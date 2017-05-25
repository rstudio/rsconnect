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

  # Max-Age with no semicolon, non-root path
  cookie <- parseCookie(parsedUrl, "mycookie2=myvalue2; Path=/test; Max-Age=3600")
  expect_equal(cookie$name, "mycookie2")
  expect_equal(cookie$value, "myvalue2")
  expect_true(cookie$expires > Sys.time() + 3595)
  expect_true(cookie$expires < Sys.time() + 3605)
  expect_equal(cookie$path, "/test")

  # Path with no semicolon, no max age
  cookie <- parseCookie(parsedUrl, "mycookie2=myvalue2; Path=/test")
  expect_equal(cookie$name, "mycookie2")
  expect_equal(cookie$value, "myvalue2")
  expect_null(cookie$expires)
  expect_equal(cookie$path, "/test")

  # No path, value with no semicolon
  cookie <- parseCookie(parsedUrl, "mycookie2=myvalue2")
  expect_equal(cookie$name, "mycookie2")
  expect_equal(cookie$value, "myvalue2")
  expect_null(cookie$expires)
  expect_equal(cookie$path, "/")

  # Full cookie with spaces around =s
  cookie <- parseCookie(parsedUrl, "mycookie = myvalue; Path = /; Expires = Sat, 24 Jun 2017 16:16:05 GMT; Max-Age = 3600; HttpOnly")
  expect_equal(cookie$name, "mycookie")
  expect_equal(cookie$value, "myvalue")
  expect_true(cookie$expires > Sys.time() + 3595)
  expect_true(cookie$expires < Sys.time() + 3605)
  expect_equal(cookie$path, "/")

  # Value-less cookie
  cookie <- parseCookie(parsedUrl, "mycookie=; Path = /")
  expect_equal(cookie$name, "mycookie")
  expect_equal(cookie$value, "")
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
      "wrongpath=huh; Path=/uhoh; Max-Age=100")
    )
  }, "Invalid path set for cookie")
  cookies <- get("fakedomain:123", envir=.cookieStore)
  expect_equal(nrow(cookies), 2)

  # Check the first cookie
  co <- cookies[cookies$name=="mycookie",]
  expect_equal(co$name, "mycookie")
  expect_equal(co$value, "myvalue")
  expect_true(co$expires > Sys.time() + 3595)
  expect_true(co$expires < Sys.time() + 3605)
  expect_equal(co$path, "/")

  # And the other
  co <- cookies[cookies$name=="anotherCookie",]
  expect_equal(co$name, "anotherCookie")
  expect_equal(co$value, "what")
  expect_true(co$expires > Sys.time() + 95)
  expect_true(co$expires < Sys.time() + 105)
  expect_equal(co$path, "/test")
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
