# servers() redacts the certificate

    Code
      servers()
    Output
                       name                                url          certificate
      1         cert_test_a            https://localhost:4567/ -----B... (redacted)
      2        shinyapps.io        https://api.shinyapps.io/v1 Amazon... (redacted)
      3 connect.posit.cloud https://api.connect.posit.cloud/v1 -----B... (redacted)

# serverInfo() redacts the certificate

    Code
      str(serverInfo("shinyapps.io"))
    Output
      List of 3
       $ name       : chr "shinyapps.io"
       $ url        : chr "https://api.shinyapps.io/v1"
       $ certificate: Amazon... (redacted)

---

    Code
      str(serverInfo("connect.posit.cloud"))
    Output
      List of 3
       $ name       : chr "connect.posit.cloud"
       $ url        : chr "https://api.connect.posit.cloud/v1"
       $ certificate: -----B... (redacted)

# serverInfo() errors if server not present

    Code
      serverInfo("foo")
    Condition
      Error in `findServer()`:
      ! Can't find `server` with name "foo".
      i Known servers are "shinyapps.io" and "connect.posit.cloud".

# addServer() errors if url not a connect server

    Code
      addServer(url)
    Condition
      Error in `addServer()`:
      ! `url` does not appear to be a Posit Connect server.

# addServer() and addServerCertificate() inform about their actions

    Code
      addServer("https://example.com", validate = FALSE)
    Message
      Server 'example.com' added successfully: https://example.com
    Code
      addServerCertificate("example.com", certificate = cert)
    Message
      Certificate added to server 'example.com'

# certificates can't be attached to plain http servers

    Code
      addServerCertificate("test", cert)
    Condition
      Error in `addServerCertificate()`:
      ! Certificates may only be attached to servers that use the HTTPS protocol.
      i Specify an HTTPS URL for the server, or omit the certificate.

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
      i Known servers are "shinyapps.io" and "connect.posit.cloud".

