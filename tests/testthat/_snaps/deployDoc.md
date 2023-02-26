# deployDoc correctly reports bad path

    Code
      deployDoc("doesntexist.Rmd")
    Condition
      Error in `deployDoc()`:
      ! `doc`, "doesntexist.Rmd", does not exist.

