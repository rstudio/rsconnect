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

