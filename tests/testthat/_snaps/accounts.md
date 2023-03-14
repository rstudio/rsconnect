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

# setAccountInfo() gives nice error on bad copy and paste

    Code
      setAccountInfo("name", "token", "<SECRET>")
    Condition
      Error in `setAccountInfo()`:
      ! You've copied and pasted the wrong thing.
      i Either click 'Show secret' or 'Copy to clipboard'.

