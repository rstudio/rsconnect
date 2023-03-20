# Invalid cookies fail parsing

    Code
      cookie <- parseCookie("x=1; Path=/something/else", "/path")
    Condition
      Warning in `parseCookie()`:
      Invalid path set for cookie on request for '/path': x=1; Path=/something/else

---

    Code
      cookie <- parseCookie("mycookie;")
    Condition
      Warning in `parseCookie()`:
      Unable to parse set-cookie header: mycookie;

