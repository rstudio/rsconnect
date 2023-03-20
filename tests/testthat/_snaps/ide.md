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

