# errors if no deployments

    Code
      findDeployment(app)
    Condition
      Error:
      ! Couldn't find any deployments matching supplied criteria.

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

