# getAppById() fails where expected

    Code
      getAppById("123", "susan", "unknown", "unknown.com")
    Condition
      Error in `getAppById()`:
      ! Can't find server with url "unknown.com".
    Code
      getAppById("123", "robert", "unknown", "https://example.com")
    Condition
      Error in `getAppById()`:
      ! Can't find account "robert" on server "example.com".

# registerUserToken registers a user token

    Code
      accountInfo("the-account")
    Output
      $name
      [1] "the-account"
      
      $server
      [1] "the-server"
      
      $accountId
      [1] "42"
      
      $token
      [1] "the-token"
      
      $private_key
      [1] "the-pr... (redacted)"
      
      $username
      [1] "the-account"
      

