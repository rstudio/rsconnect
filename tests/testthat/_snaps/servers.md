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
       $ certificate: Amazon... (redacted)
       $ url        : chr "https://api.shinyapps.io/v1"
    Code
      str(serverInfo("shinyapps.io"))
    Output
      List of 3
       $ name       : chr "shinyapps.io"
       $ certificate: Amazon... (redacted)
       $ url        : chr "https://api.shinyapps.io/v1"

