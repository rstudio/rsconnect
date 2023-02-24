# appPrimaryDoc must exist, if supplied

    Code
      deployApp(dir, appPrimaryDoc = c("foo.Rmd", "bar.Rmd"))
    Condition
      Error in `deployApp()`:
      ! `appPrimaryDoc` must be a single string, not a character vector.
    Code
      deployApp(dir, appPrimaryDoc = "foo.Rmd")
    Condition
      Error in `deployApp()`:
      ! `appPrimaryDoc` not found inside `appDir`

# recordDoc must exist, if supplied

    Code
      deployApp(dir, recordDir = "doesntexist")
    Condition
      Error in `deployApp()`:
      ! `recordDir`, "doesntexist", does not exist.

# appSourceDoc is deprecated & checks path

    Code
      deployApp(dir, appSourceDoc = "records")
    Condition
      Warning:
      The `appSourceDoc` argument of `deployApp()` is deprecated as of rsconnect 0.9.0.
      i Please use the `recordDir` argument instead.
      Error in `deployApp()`:
      ! `appSourceDoc`, "records", does not exist.

# deployHook executes function if set

    Code
      . <- runDeploymentHook("PATH", "rsconnect.pre.deploy", verbose = TRUE)
    Output
      Invoking `rsconnect.pre.deploy` hook

