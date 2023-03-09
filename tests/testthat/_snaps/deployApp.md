# appDir must be an existing directory

    Code
      deployApp(1)
    Condition
      Error in `deployApp()`:
      ! `appDir` must be a single string, not the number 1.
    Code
      deployApp("doesntexist")
    Condition
      Error in `deployApp()`:
      ! `appDir`, "doesntexist", does not exist.

# single document appDir is deprecated

    Code
      deployApp("foo.Rmd")
    Condition
      Warning:
      The `appDir` argument of `deployApp()` takes a directory, not a document, as of rsconnect 0.9.0.
      i Please use `deployDoc()` instead.
      Error in `deployDoc()`:
      ! `doc`, "foo.Rmd", does not exist.

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
      ! No content to deploy.
      x `appDir` is empty.

# startup scripts are logged by default

    Code
      runStartupScripts(".")
    Message
      i Running ./.rsconnect_profile

# deployHook executes function if set

    Code
      . <- runDeploymentHook("PATH", "rsconnect.pre.deploy", verbose = TRUE)
    Output
      Invoking `rsconnect.pre.deploy` hook

