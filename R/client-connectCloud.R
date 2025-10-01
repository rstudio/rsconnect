# Docs: https://build.posit.it/job/hostedapps/job/lucid-pipeline/job/main/API/

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

getCurrentProjectId <- function(service, authInfo) {
  currentApplicationId <- Sys.getenv("LUCID_APPLICATION_ID")
  if (currentApplicationId != "") {
    path <- paste0("/applications/", currentApplicationId)
    current_application <- GET(service, authInfo, path)
    return(current_application$content_id)
  }
}

connectCloudClient <- function(service, authInfo) {
  list(
    service = function() {
      "posit.cloud"
    },

    currentUser = function() {
      GET(service, authInfo, "/users/me")
    },

    listApplications = function(accountId, filters = list()) {
      # TODO: call the real API when available (api doesn't support filtering by name yet)
      return(list())
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

      content <- POST_JSON(service, authInfo, "/contents", json)
      list(
        id = content$id,
        application_id = content$id
      )
    },

    getContent = function(contentId) {
      path <- paste0("/contents/", contentId)
      GET(service, authInfo, path)
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
      PATCH_JSON(service, authInfo, path, json)
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
      POST_JSON(service, authInfo, path, list())
    },

    awaitCompletion = function(revisionId) {
      repeat {
        path <- paste0("/revisions/", revisionId)
        response <- GET(service, authInfo, path)

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
    }
  )
}
