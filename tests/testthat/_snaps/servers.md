# servers() redacts the certificate

    Code
      servers()
    Output
                name                         url          certificate
      1  cert_test_a     https://localhost:4567/ -----B... (redacted)
      2 shinyapps.io https://api.shinyapps.io/v1 Amazon... (redacted)
      3  posit.cloud https://api.shinyapps.io/v1 Amazon... (redacted)

# serverInfo() redacts the certificate

    Code
      str(serverInfo("posit.cloud"))
    Output
      List of 3
       $ name       : chr "posit.cloud"
       $ url        : chr "https://api.shinyapps.io/v1"
       $ certificate: Amazon... (redacted)
    Code
      str(serverInfo("shinyapps.io"))
    Output
      List of 3
       $ name       : chr "shinyapps.io"
       $ url        : chr "https://api.shinyapps.io/v1"
       $ certificate: Amazon... (redacted)

# serverInfo() errors if server not present

    Code
      serverInfo("foo")
    Condition
      Error in `serverInfo()`:
      ! Can't find server "foo".

# cloud server errors if not cloud server

    Code
      cloudServerInfo("foo")
    Condition
      Error in `cloudServerInfo()`:
      ! `name` must be one of "shinyapps.io", "posit.cloud", or "rstudio.cloud", not "foo".

# findServer() errors if no servers

    Code
      findServer()
    Condition
      Error in `findServer()`:
      ! No local servers have been registered

# findServer() errors/prompts of multiple servers present

    Code
      findServer()
    Condition
      Error in `findServer()`:
      ! Multiple servers found.
      i Use `server` to pick one of "myserver" and "yourserver".

---

    Code
      out <- findServer()
    Message
      Multiple servers found.
      Which one do you want to use?
      1: myserver
      2: yourserver
      Selection: 2

# findServer checks server name

    Code
      findServer(1)
    Condition
      Error:
      ! `server` must be a single string, not the number 1.
    Code
      findServer("foo")
    Condition
      Error in `findServer()`:
      ! Can't find `server` with name "foo".
      i Known servers are "shinyapps.io" and "posit.cloud".

