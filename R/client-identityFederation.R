# Attempt exchange an identity token sourced from Posit Workbench for an
# ephemeral Connect API key. Returns NULL if this exchange fails or an API key
# otherwise.
attemptIdentityFederation <- function(serverUrl) {
  cached <- getCachedApiKey(serverUrl)
  if (!is.null(cached)) {
    return(cached)
  }

  # Only attempt this in Workbench.
  if (
    Sys.getenv("POSIT_PRODUCT") != "WORKBENCH" &&
      !nzchar(Sys.getenv("RS_SERVER_ADDRESS"))
  ) {
    return(NULL)
  }

  token <- tryCatch(rstudioapi::getIdentityToken(), error = function(e) NULL)
  if (is.null(token)) {
    return(NULL)
  }

  # Call Connect's exchange endpoint.
  service <- parseHttpUrl(serverUrl)
  body <- paste0(
    "grant_type=",
    urlEncode("urn:ietf:params:oauth:grant-type:token-exchange"),
    "&subject_token_type=",
    urlEncode("urn:ietf:params:oauth:token-type:id_token"),
    "&subject_token=",
    urlEncode(token$token),
    "&requested_token_type=",
    urlEncode("urn:posit:connect:api-key")
  )
  tryCatch(
    {
      response <- POST(
        service,
        authInfo = list(),
        path = "/v1/oauth/integrations/credentials",
        contentType = "application/x-www-form-urlencoded",
        content = body
      )
      apiKey <- response$access_token
      if (!is.null(apiKey)) {
        cacheApiKey(serverUrl, apiKey, token$expiry)
      }
      apiKey
    },
    error = function(e) NULL
  )
}

cacheApiKey <- function(serverUrl, apiKey, expiry = NULL) {
  env_poke(apiKeyCache, serverUrl, list(apiKey = apiKey, expiry = expiry))
}

getCachedApiKey <- function(serverUrl) {
  cached <- env_get(apiKeyCache, serverUrl, default = NULL)
  if (is.null(cached)) {
    return(NULL)
  }

  # Evict expired API keys.
  if (!is.null(cached$expiry) && Sys.time() >= (cached$expiry - 60L)) {
    env_unbind(apiKeyCache, serverUrl)
    return(NULL)
  }

  cached$apiKey
}

# Session-level cache for ephemeral API keys.
apiKeyCache <- new.env(parent = emptyenv())
