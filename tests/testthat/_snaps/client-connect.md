# leading timestamps are stripped

    Code
      stripConnectTimestamps(c(
        "2024/04/24 13:08:04.901698921 [rsc-session] Content GUID: 3bfbd98a-6d6d-41bd-a15f-cab52025742f",
        "2024/04/24 13:08:04.901734307 [rsc-session] Content ID: 43888",
        "2024/04/24 13:08:04.901742487 [rsc-session] Bundle ID: 94502",
        "2024/04/24 13:08:04.901747536 [rsc-session] Variant ID: 6465"))
    Output
      [1] "[rsc-session] Content GUID: 3bfbd98a-6d6d-41bd-a15f-cab52025742f"
      [2] "[rsc-session] Content ID: 43888"                                 
      [3] "[rsc-session] Bundle ID: 94502"                                  
      [4] "[rsc-session] Variant ID: 6465"                                  

# non-leading timestamps remain

    Code
      stripConnectTimestamps(c(
        "this message has a timestamp 2024/04/24 13:08:04.901698921 within a line"))
    Output
      [1] "this message has a timestamp 2024/04/24 13:08:04.901698921 within a line"

# messages without recognized timestamps are unmodified

    Code
      stripConnectTimestamps(c("this message has no timestamp",
        "2024/04/24 13:08 this message timestamp has a different format"))
    Output
      [1] "this message has no timestamp"                                 
      [2] "2024/04/24 13:08 this message timestamp has a different format"

# waitForTask

    Code
      invisible(client$waitForTask(101, quiet = FALSE))
    Output
      [rsc-session] Content GUID: 3bfbd98a-6d6d-41bd-a15f-cab52025742f
      [rsc-session] Content ID: 43888
      [rsc-session] Bundle ID: 94502
      [rsc-session] Variant ID: 6465

---

    Code
      invisible(client$waitForTask(42, quiet = TRUE))

