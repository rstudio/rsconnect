# error when no deployments and no accounts

    Code
      findDeployment(app, appName = "placeholder")
    Condition
      Error in `accountInfo()`:
      ! No accounts registered.
      i To register an account, call `rsconnect::connectCloudUser()` (Posit Connect Cloud), `rsconnect::connectUser()` (Posit Connect), or `rsconnect::setAccountInfo()` (shinyapps.io).

# disambiguates multiple deployments

    Code
      findDeployment(app)
    Condition
      Error:
      ! This directory has been previously deployed in multiple places.
      Please use `appName`, `server` or `account` to disambiguate.
      Known applications:
      * test1 (server: example.com / username: ron): <https://example.com/ron/123>
      * test2 (server: example.com / username: ron): <https://example.com/ron/123>

---

    Code
      dep <- findDeployment(app)
    Message
      This directory has been previously deployed in multiple places.
      Which deployment do you want to use?
      1: test1 (server: example.com / username: ron): <https://example.com/ron/123>
      2: test2 (server: example.com / username: ron): <https://example.com/ron/123>
      Selection: 2

