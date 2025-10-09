# Docs: https://posit-hosted.github.io/vivid-api

# Map rsconnect appMode to Connect Cloud contentType
cloudContentTypeFromAppMode <- function(appMode) {
  switch(
    appMode,
    "jupyter-notebook" = "jupyter",
    "python-bokeh" = "bokeh",
    "python-dash" = "dash",
    "python-shiny" = "shiny",
    "shiny" = "shiny",
    "python-streamlit" = "streamlit",
    "quarto" = "quarto",
    "quarto-static" = "quarto",
    "quarto-shiny" = "quarto",
    "rmd-static" = "rmarkdown",
    "rmd-shiny" = "rmarkdown",
    "static" = "static",
    stop("appMode '", appMode, "' is not supported by Connect Cloud")
  )
}

# Creates a client for interacting with the Connect Cloud API.
connectCloudClient <- function(service, authInfo) {
  # Generic retry wrapper. If a request fails with 401 Unauthorized, it will
  # exchange the refresh token for a new access token and retry the request
  # once.
  withTokenRefreshRetry <- function(request_fn, ...) {
    tryCatch(
      {
        request_fn(service, authInfo, ...)
      },
      rsconnect_http_401 = function(e) {
        # Exchange refresh token for new access token
        authClient <- cloudAuthClient()
        tokenResponse <- authClient$exchangeToken(list(
          grant_type = "refresh_token",
          refresh_token = authInfo$refreshToken
        ))

        # Save updated tokens
        registerAccount(
          authInfo$server,
          authInfo$name,
          authInfo$accountId,
          accessToken = tokenResponse$access_token,
          refreshToken = tokenResponse$refresh_token
        )

        # Retry the original request with refreshed token
        authInfo$accessToken <<- tokenResponse$access_token
        authInfo$refreshToken <<- tokenResponse$refresh_token
        request_fn(service, authInfo, ...)
      }
    )
  }

  list(
    service = function() {
      "connect.posit.cloud"
    },

    currentUser = function() {
      GET(service, authInfo, "/users/me")
    },

    withTokenRefreshRetry = withTokenRefreshRetry,

    listApplications = function(accountId, filters = list()) {
      # TODO: call the real API when available (api doesn't support filtering by name yet)
      return(list())
    },

    createContent = function(
      name,
      title,
      accountId,
      appMode,
      primaryFile
    ) {
      title <- if (nzchar(title)) title else name
      contentType <- cloudContentTypeFromAppMode(appMode)

      # Build revision object, conditionally including primary_file
      revision <- list(
        source_type = "bundle",
        content_type = contentType,
        app_mode = appMode,
        primary_file = primaryFile
      )

      json <- list(
        account_id = accountId,
        title = title,
        next_revision = revision
      )

      content <- withTokenRefreshRetry(
        POST_JSON,
        "/contents",
        json
      )
      list(
        id = content$id,
        application_id = content$id
      )
    },

    getContent = function(contentId) {
      path <- paste0("/contents/", contentId)
      withTokenRefreshRetry(GET, path)
    },

    updateContent = function(
      contentId,
      envVars,
      newBundle = FALSE,
      primaryFile
    ) {
      path <- paste0("/contents/", contentId)
      if (newBundle) {
        path <- paste0(path, "?new_bundle=true")
      }

      secrets <- unname(Map(
        function(name, value) {
          list(
            name = name,
            value = value
          )
        },
        envVars,
        Sys.getenv(envVars)
      ))

      json <- list(
        secrets = secrets,
        revision_overrides = list(
          primary_file = primaryFile
        )
      )

      withTokenRefreshRetry(PATCH_JSON, path, json)
    },

    uploadBundle = function(bundlePath, uploadUrl) {
      uploadService <- parseHttpUrl(uploadUrl)
      headers <- list()
      headers$`Content-Type` <- "application/gzip"

      http <- httpFunction()
      response <- http(
        uploadService$protocol,
        uploadService$host,
        uploadService$port,
        "POST",
        uploadService$path,
        headers,
        headers$`Content-Type`,
        bundlePath
      )

      response$status <= 299
    },

    publish = function(contentId) {
      path <- paste0("/contents/", contentId, "/publish")
      withTokenRefreshRetry(POST_JSON, path, list())
    },

    # Polls the revision until the publish process completes, returning whether
    # the publish request succeeded and the error message if it failed.
    awaitCompletion = function(revisionId) {
      repeat {
        path <- paste0("/revisions/", revisionId)
        response <- withTokenRefreshRetry(GET, path)

        if (!is.null(response$publish_result)) {
          if (response$publish_result == "failure") {
            return(list(
              success = FALSE,
              url = NULL,
              error = response$publish_error_details
            ))
          }

          contentUrl <- paste0(
            connectCloudUrls()$ui,
            "/",
            authInfo$username,
            "/content/",
            response$content_id
          )

          return(list(success = TRUE, url = contentUrl, error = NULL))
        }

        Sys.sleep(1)
      }
    },

    getAccounts = function(revisionId) {
      GET(service, authInfo, "/accounts?has_user_role=true")
    }
  )
}
