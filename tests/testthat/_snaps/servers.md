# servers() redacts the certificate

    Code
      servers()
    Output
                name                         url          certificate
      1  cert_test_a     https://localhost:4567/ -----B... (redacted)
      2 shinyapps.io https://api.shinyapps.io/v1 Amazon... (redacted)
      3  posit.cloud https://api.shinyapps.io/v1 Amazon... (redacted)

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

