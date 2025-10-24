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
      The `appDir` argument of `deployApp()` takes a directory, not a document, as of rsconnect 1.0.0.
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

# applicationDeleted() errors or prompts as needed

    Code
      applicationDeleted(client, target, app)
    Condition
      Error in `applicationDeleted()`:
      ! Failed to find existing content on server; it's probably been deleted.
      i Use `forgetDeployment()` to remove outdated record and try again.
      i Or use `applications()` to see other content deployed to the the server.

---

    Code
      . <- applicationDeleted(client, target, app)
    Message
      Failed to find existing content on server; it's probably been deleted.
      What do you want to do?
      1: Give up and try again later
      2: Delete existing deployment record & deploy this content as a new item
      Selection: 2

# deployApp() errors if envVars is given a named vector

    Code
      deployApp(local_temp_app(), envVars = c(FLAG = "true"))
    Condition
      Error in `deployApp()`:
      ! `envVars` must be a character vector containing only environment variable names.
      i Set environment variables with `Sys.setenv() or an `.Renviron` file.
      i Use `unname()` to remove the names from the vector passed to `envVars`.

