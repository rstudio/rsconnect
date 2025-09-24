# Docs: https://build.posit.it/job/hostedapps/job/lucid-pipeline/job/main/API/

getCurrentProjectId <- function(service, authInfo) {
  currentApplicationId <- Sys.getenv("LUCID_APPLICATION_ID")
  if (currentApplicationId != "") {
    path <- paste0("/applications/", currentApplicationId)
    current_application <- GET(service, authInfo, path)
    return(current_application$content_id)
  }
}

connectCloudClient <- function(service, authInfo) {
  print("**** connectCloudClient")
  list(
    # status = function() {
    #   GET(service, authInfo, "/internal/status")
    # },

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

    createApplication = function(
      name,
      title,
      template,
      accountId,
      appMode,
      contentCategory = NULL,
      spaceId = NULL
    ) {
      json <- list(
        account_id = accountId,
        title = title,
      )
      content <- POST_JSON(service, authInfo, "/contents/", json)
      list(
        id = application$id,
        application_id = application$id,
        url = application$url
      )
    }
  )
}
