# sanity check to make sure we're looking at an ASCII armored cert
validateCertificate <- function(certificate) {
  return(any(grepl("-----BEGIN CERTIFICATE-----", certificate, fixed = TRUE)))
}

createCertificateFile <- function(certificate) {
  certificateFile <- NULL

  # check the R option first, then fall back on the environment variable
  systemStore <- getOption("rsconnect.ca.bundle")
  if (is.null(systemStore) || !nzchar(systemStore)) {
    systemStore <- Sys.getenv("RSCONNECT_CA_BUNDLE")
  }

  # start by checking for a cert file specified in an environment variable
  if (!is.null(systemStore) && nzchar(systemStore)) {
    if (file.exists(systemStore)) {
      certificateFile <- path.expand(systemStore)
    } else {
      warning(
        "The certificate store '",
        systemStore,
        "' specified in the ",
        if (identical(systemStore, getOption("rsconnect.ca.bundle"))) {
          "rsconnect.ca.bundle option "
        } else {
          "RSCONNECT_CA_BUNDLE environment variable "
        },
        "does not exist. The system certificate store will be used instead."
      )
    }
  }

  # if no certificate contents specified, we're done
  if (is.null(certificate)) {
    return(certificateFile)
  }

  # if we don't have a certificate file yet, try to find the system store
  if (is.null(certificateFile)) {
    if (.Platform$OS.type == "unix") {
      # search known locations on Unix-like
      stores <- c(
        "/etc/ssl/certs/ca-certificates.crt",
        "/etc/pki/tls/certs/ca-bundle.crt",
        "/usr/share/ssl/certs/ca-bundle.crt",
        "/usr/local/share/certs/ca-root.crt",
        "/etc/ssl/cert.pem",
        "/var/lib/ca-certificates/ca-bundle.pem"
      )
    } else {
      # mirror behavior of curl on Windows, which looks in system folders,
      # the working directory, and %PATH%.
      stores <- c(
        file.path(getwd(), "curl-ca-bundle.crt"),
        "C:/Windows/System32/curl-ca-bundle.crt",
        "C:/Windows/curl-ca-bundle.crt",
        file.path(
          strsplit(Sys.getenv("PATH"), ";", fixed = TRUE),
          "curl-ca-bundle.crt"
        )
      )
    }

    # use our own baked-in bundle as a last resort
    stores <- c(
      stores,
      system.file(package = "rsconnect", "cert", "cacert.pem")
    )

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
  certificateStore <- tempfile(pattern = "cacerts", fileext = ".pem")
  dirCreate(dirname(certificateStore))
  file.create(certificateStore)

  # open temporary cert store
  con <- file(certificateStore, open = "at")
  defer(close(con))

  # copy the contents of the certificate file into the store, if we found one
  # (we don't do a straight file copy since we don't want to inherit or
  # correct permissions)
  if (!is.null(certificateFile)) {
    certLines <- readLines(certificateFile, warn = FALSE)
    writeLines(text = certLines, con = con)
  }

  # append the server-specific certificate (with a couple of blank lines)
  writeLines(text = c("", "", certificate), con = con)

  return(certificateStore)
}

inferCertificateContents <- function(certificate) {
  # certificate can be specified as either a character vector or a filename;
  # infer which we're dealing with

  # tolerate NULL, which is a valid case representing no certificate
  if (is.null(certificate) || identical(certificate, "")) {
    return(NULL)
  }

  # collapse to a single string if we got a vector of lines
  if (length(certificate) > 1) {
    certificate <- paste(certificate, collapse = "\n")
  }

  # looks like ASCII armored certificate data, return as-is
  if (validateCertificate(substr(certificate, 1, 27))) {
    return(certificate)
  }

  # looks like a file; return its contents
  if (file.exists(certificate)) {
    if (file.size(certificate) > 1048576) {
      stop(
        "The file '",
        certificate,
        "' is too large. Certificate files must ",
        "be less than 1MB."
      )
    }
    contents <- paste(
      readLines(con = certificate, warn = FALSE),
      collapse = "\n"
    )
    if (validateCertificate(contents)) {
      return(contents)
    } else {
      stop(
        "The file '",
        certificate,
        "' does not appear to be a certificate. ",
        "Certificate files should be in ASCII armored PEM format, with a ",
        "first line reading -----BEGIN CERTIFICATE-----."
      )
    }
  }

  # doesn't look like something we can deal with; guess error based on length
  if (nchar(certificate) < 200) {
    stop("The certificate file '", certificate, "' does not exist.")
  } else {
    stop(
      "The certificate '",
      substr(certificate, 1, 10),
      "...' is not ",
      "correctly formed. Specify the certificate as either an ASCII armored string, ",
      "beginning with -----BEGIN CERTIFICATE----, or a valid path to a file ",
      "containing the certificate."
    )
  }
}
