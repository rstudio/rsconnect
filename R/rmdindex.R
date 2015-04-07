
# Write an index.htm file for an Rmd deployment if necessary (returns the
# files written so they can be removed after deployment)
writeRmdIndex <- function(appName, appDir) {

  # files written
  files <- NULL

  # no index required for Shiny, or for directories with an index.Rmd
  if (!file.exists(file.path(appDir, "ui.R")) &&
        !file.exists(file.path(appDir, "server.R")) &&
        !file.exists(file.path(appDir, "index.htm")) &&
        !file.exists(file.path(appDir, "index.Rmd")))
  {
    # otherwise enumerate the Rmd files as the basis for the index
    appFiles <- list.files(path = appDir, pattern = "\\.(rmd|html)$",
                           recursive = FALSE, ignore.case = TRUE)

    if (length(appFiles) == 1)
      indexPage <- redirectWebPage(appFiles[1])
    else
      indexPage <- listingWebPage(appName, appFiles)

    indexFile <- file.path(appDir, "index.htm")
    files <- c(files, indexFile)
    writeLines(indexPage, indexFile, useBytes = TRUE)
  }

  files
}

redirectWebPage <- function(appFile) {
  meta <- paste('<meta http-equiv="refresh" content="0;',
                htmlEscape(appFile), '">', sep = "")
  webPage(meta, NULL)
}

listingWebPage <- function(appDir, appFiles) {

  head <- c("<style type='text/css'>",
            "body { padding-left: 20px; }",
            ".rmd { margin-bottom: 20px; }",
            ".rmdlink { font-size: 1.5em; text-decoration: none; }",
            ".rmdlink:hover { text-decoration: underline; }",
            "</style>")

  appDir <- htmlEscape(appDir)
  appFiles <- htmlEscape(appFiles)

  body <- paste("<h1>", basename(appDir), "</h1>", sep = "")
  body <- c(body, paste("<div class = 'rmd'>",
                        "<a href='", appFiles, "' class = 'rmdlink'>",
                        appFiles,
                        "</a></div>", sep = ""))

  webPage(head, body)
}

webPage <- function(head, body) {

  if (is.null(head))
    head <- c()

  if (is.null(body))
    body <- c()

  enc2utf8(c(
    '<!DOCTYPE HTML>',
    '<html>',
    '<head>',
    '<meta charset="UTF-8">',
    head,
    '</head>',
    '<body>',
    body,
    '</body>',
    '</html>')
  )
}

htmlEscape <- local({

  .htmlSpecials <- list(
    `&` = '&amp;',
    `<` = '&lt;',
    `>` = '&gt;'
  )
  .htmlSpecialsPattern <- paste(names(.htmlSpecials), collapse='|')
  .htmlSpecialsAttrib <- c(
    .htmlSpecials,
    `'` = '&#39;',
    `"` = '&quot;',
    `\r` = '&#13;',
    `\n` = '&#10;'
  )
  .htmlSpecialsPatternAttrib <- paste(names(.htmlSpecialsAttrib), collapse='|')

  function(text, attribute=FALSE) {
    pattern <- if(attribute)
      .htmlSpecialsPatternAttrib
    else
      .htmlSpecialsPattern

    # Short circuit in the common case that there's nothing to escape
    if (!any(grepl(pattern, text)))
      return(text)

    specials <- if(attribute)
      .htmlSpecialsAttrib
    else
      .htmlSpecials

    for (chr in names(specials)) {
      text <- gsub(chr, specials[[chr]], text, fixed=TRUE)
    }

    return(text)
  }
})

