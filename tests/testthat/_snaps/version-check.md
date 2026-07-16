# update messages are informative but don't overclaim

    Code
      updateStartupMessage("1.10.0", "1.11.0")
    Output
      [1] "A newer version of rsconnect (1.11.0) is available from your repositories. You have 1.10.0."
    Code
      updateErrorHint("1.10.0", "1.11.0")
    Output
                                                                              i 
      "rsconnect 1.11.0 is available from your repositories (you have 1.10.0)." 

# deploy version message mentions latest version only when known

    Code
      deployVersionMessage("1.10.0")
    Output
      [1] "This is rsconnect 1.10.0"
    Code
      deployVersionMessage("1.10.0", "1.11.0")
    Output
      [1] "This is rsconnect 1.10.0 (1.11.0 is latest available)"

