
createCertificateFile <- function(certificate) {
  certificateFile <- NULL

  # check the R option first, then fall back on the environment variable
  systemStore <- getOption("rsconnect.ca.bundle")
  if (is.null(systemStore) || nzchar(systemStore)) {
    systemStore <- Sys.getenv("RSCONNECT_CA_BUNDLE")
  }

  # start by checking for a cert file specified in an environment variable
  if (nzchar(systemStore) && file.exists(systemStore)) {
    certificateFile <- systemStore
  }

  # if no certificate contents specified, we're done
  if (is.null(certificate))
    return(certificateFile)

  # if we don't have a certificate file yet, try to find the system store
  if (is.null(certificateFile)) {
    stores <- c("/etc/ssl/certs/ca-certificates.crt",
                "/etc/pki/tls/certs/ca-bundle.crt",
                "/usr/share/ssl/certs/ca-bundle.crt",
                "/usr/local/share/certs/ca-root.crt",
                "/etc/ssl/cert.pem",
                "/var/lib/ca-certificates/ca-bundle.pem")
    for (store in stores) {
      if (file.exists(store)) {
        # if the bundle exists, stop here
        certificateFile <- store
        break
      }
    }

    # if we didn't find the system store, it's okay; the fact that we're here
    # means that we have a server-specific certificate so it's probably going
    # to be all right to use only that cert.
  }

  # create a temporary file to house the certificates
  certificateStore <- tempfile(pattern = "cacerts", fileext = "pem")

  # copy the certificate file into the store, if we found one
  if (!is.null(certificateFile)) {
    file.copy(certificateFile, certificateStore)
  }

  # append the server-specific certificate (with a couple of blank lines)
  con <- file(certificateStore, open = "at")
  on.exit(close(con), add = TRUE)
  writeLines(text = c("", "", certificate), con = con)

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
