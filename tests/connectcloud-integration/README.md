# Connect Cloud integration tests

Manual developer workflow for exercising `migrateDeployment()` end-to-end
against live shinyapps.io and Connect Cloud accounts (run interactively, not
by `testthat`).

Prerequisites: `devtools::install_local(".")` from a local rsconnect checkout.

1. Register a shinyapps.io account:

   ```r
   rsconnect::setAccountInfo(name = "MYNAME", token = "TOK", secret = "SEC")
   ```

2. Deploy the test app to shinyapps.io:

   ```r
   rsconnect::deployApp(
     appDir  = "tests/shinyapps-integration/example-shiny",
     appName = "migrate-demo",
     account = "MYNAME"
   )
   ```

   Creates `<appDir>/rsconnect/shinyapps.io/MYNAME/migrate-demo.dcf`.

3. Register a Connect Cloud account (browser):

   ```r
   rsconnect::connectCloudUser()
   ```

4. Deploy the same app to Connect Cloud to create the migration target:

   ```r
   rsconnect::deployApp(
     appDir  = "tests/shinyapps-integration/example-shiny",
     appName = "migrate-demo",
     server  = "connect.posit.cloud"
   )
   ```

   Note the content ID from the resulting DCF or the admin URL.

5. Simulate the migration scenario (reset to shinyapps.io as the active record):

   ```r
   rsconnect::forgetDeployment(
     "tests/shinyapps-integration/example-shiny",
     name    = "migrate-demo",
     account = "<cc_account>",
     server  = "connect.posit.cloud"
   )
   ```

6. Run the migration:

   ```r
   rsconnect::migrateDeployment(
     appPath   = "tests/shinyapps-integration/example-shiny",
     contentId = "<content ID from step 4>"
   )
   ```

7. Verify: `deployApp()` should now route to Connect Cloud.

   ```r
   rsconnect::deployments("tests/shinyapps-integration/example-shiny")
   ```

   Should show `server = "connect.posit.cloud"` only.

## Automated test

`test-migrate.R` runs one automated test against the live Connect Cloud API,
skipped unless `CONNECT_CLOUD_CONTENT_ID` is set. Required env vars are
documented in `setup.R`.
