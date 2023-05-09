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

# needsVisibilityChange() errors for cloud

    Code
      needsVisibilityChange("posit.cloud", appVisibility = "public")
    Condition
      Error in `needsVisibilityChange()`:
      ! Can't change cloud app visiblity from `deployApp()`.
      i Please change on posit.cloud instead.

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
      ! Failed to find existing application on server; it's probably been deleted.
      i Use `forgetDeployment()` to remove outdated record and try again.
      i Or use `applications()` to see other applications you have on the server.

---

    Code
      . <- applicationDeleted(client, target, app)
    Message
      Failed to find existing application on server; it's probably been deleted.
      What do you want to do?
      1: Give up and try again later
      2: Delete existing deployment & create a new app
      Selection: 2

