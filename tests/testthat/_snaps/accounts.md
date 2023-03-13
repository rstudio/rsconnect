# secrets are hidden from casual inspection

    Code
      accountInfo("1")$secret
    Output
      [1] "SECRET... (redacted)"
    Code
      accountInfo("2")$private_key
    Output
      [1] "SECRET... (redacted)"
    Code
      accountInfo("3")$apiKey
    Output
      [1] "SECRET... (redacted)"

