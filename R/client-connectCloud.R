# Docs: https://posit-hosted.github.io/vivid-api

# Generic retry wrapper that handles token refresh on 401 responses
withTokenRefreshRetry <- function(service, authInfo, request_fn, ...) {
  tryCatch(
    {
      request_fn(service, authInfo, ...)
    },
    rsconnect_http_401 = function(e) {
      tryCatch(
        {
          # Exchange refresh token for new access token
          authClient <- cloudAuthClient()
          tokenResponse <- authClient$exchangeToken(list(
            grant_type = "refresh_token",
            refresh_token = authInfo$refresh_token
          ))

          # Save updated tokens
          registerAccount(
            authInfo$server,
            authInfo$name,
            authInfo$accountId,
            accessToken = authInfo$access_token,
            refreshToken = authInfo$refresh_token
          )

          # Retry the original request with refreshed token
          authInfo$access_token <- tokenResponse$access_token
          authInfo$refresh_token <- tokenResponse$refresh_token
          request_fn(service, authInfo, ...)
        },
        error = function(refresh_error) {
          # If token refresh fails, re-throw the original 401 error
          stop(e)
        }
      )
    }
  )
}

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

connectCloudClient <- function(service, authInfo) {
  list(
    service = function() {
      "connect.posit.cloud"
    },

    createContent = function(
      name,
      title,
      accountId,
      appMode
    ) {
      title <- if (nzchar(title)) title else name
      contentType <- cloudContentTypeFromAppMode(appMode)
      json <- list(
        account_id = accountId,
        title = title,
        next_revision = list(
          source_type = "bundle",
          content_type = contentType,
          app_mode = appMode,
          primary_file = "app.R"
        )
      )

      content <- withTokenRefreshRetry(
        service,
        authInfo,
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
      withTokenRefreshRetry(service, authInfo, GET, path)
    },

    updateContent = function(contentId, envVars, newBundle = FALSE) {
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

      json <- list(secrets = secrets)
      withTokenRefreshRetry(service, authInfo, PATCH_JSON, path, json)
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
      withTokenRefreshRetry(service, authInfo, POST_JSON, path, list())
    },

    awaitCompletion = function(revisionId) {
      repeat {
        path <- paste0("/revisions/", revisionId)
        response <- withTokenRefreshRetry(service, authInfo, GET, path)

        if (!is.null(response$publish_result)) {
          if (response$publish_result == "failure") {
            return(list(
              success = FALSE,
              url = NULL,
              error = response$publish_error_details
            ))
          }
          return(list(success = TRUE, url = response$url, error = NULL))
        }

        Sys.sleep(1)
      }
    },

    getAccounts = function(revisionId) {
      GET(service, authInfo, "/accounts?has_user_role=true")
    }
  )
}
