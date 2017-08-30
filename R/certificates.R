
createCertificateFile <- function(certificate) {
  certificateFile <- NULL
  
  # start by checking for a cert file specified in an environment variable
  envFile <- Sys.getenv("RSCONNECT_CA_BUNDLE")
  if (nzchar(envFile) && file.exists(envFile)) {
    certificateFile <- envFile
  }
  
  # if no certificate contents specified, we're done
  if (is.null(certificate))
    return(certificateFile)
  
  # if we don't have a certificate file yet, try to find the system store
  if (is.null(certificateFile)) {
    certificateFile <- "" #TODO
  }
  
  # create a temporary file to house the certificates
  certficateStore <- tempfile(pattern = "cacerts", fileext = "pem")
  
  # copy the certificate file into the store
  file.copy(certificateFile, certificateStore)
  
  # append the server-specific certificate
  con <- file(certificateStore, open = "at")
  on.exit(close(con), add = TRUE)
  writeLines(con, certificate)
  
  return(certificateStore)
}

inferCertificateContents <- function(certificate) {
  # certificate can be specified as either a character vector or a filename;
  # infer which we're dealing with

  # tolerate NULL, which is a valid case representing no certificate
  if (is.null(certificate))
    return(NULL)

  # collapse to a single string if we got a vector of lines
  if (length(certificate) > 1)
    certificate <- paste(certificate, collapse = "\n")

  # looks like ASCII armored certificate data, return as-is
  if (identical(substr(certificate, 1, 27), "-----BEGIN CERTIFICATE-----"))
    return(certificate)

  # looks like a file; return its contents
  if (file.exists(certificate)) {
    return(paste(readLines(con = certificate, warn = FALSE), collapse = "\n"))
  }

  # doesn't look like something we can deal with
  stop("Invalid certificate '", substr(certificate, 1, 100),
    if(nchar(certificate) > 100) "..." else "", "'. Specify the certificate ",
    "as either an ASCII armored string, beginning with -----BEGIN ",
    "CERTIFICATE----, or a valid path to a file containing the certificate.")
}
