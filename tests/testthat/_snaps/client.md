# client checks accept only their own client class

    Code
      checkShinyappsClient(connect)
    Condition
      Error:
      ! `client` must be a shinyapps.io client
    Code
      checkConnectCloudClient(shinyapps)
    Condition
      Error:
      ! `client` must be a Posit Connect Cloud client
    Code
      checkConnectClient(cloud)
    Condition
      Error:
      ! `client` must be a Posit Connect client

