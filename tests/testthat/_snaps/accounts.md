# secrets are hidden from casual inspection

    Code
      accountInfo("john")$private_key
    Output
      [1] "THISIS... (redacted)"
    Code
      accountInfo("susan")$secret
    Output
      [1] "THIS I... (redacted)"
    Code
      str(accountInfo("john"))
    Output
      List of 5
       $ username   : chr "john"
       $ accountId  : chr "userId"
       $ token      : chr "token"
       $ server     : chr "example.com"
       $ private_key: THISIS... (redacted)

