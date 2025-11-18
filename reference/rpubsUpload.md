# Upload a file to RPubs

This function publishes a file to rpubs.com. If the upload succeeds a
list that includes an `id` and `continueUrl` is returned. A browser
should be opened to the `continueUrl` to complete publishing of the
document. If an error occurs then a diagnostic message is returned in
the `error` element of the list.

Supported servers: RPubs servers

## Usage

``` r
rpubsUpload(title, contentFile, originalDoc, id = NULL, properties = list())
```

## Arguments

- title:

  The title of the document.

- contentFile:

  The path to the content file to upload.

- originalDoc:

  The document that was rendered to produce the `contentFile`. May be
  `NULL` if the document is not known.

- id:

  If this upload is an update of an existing document then the id
  parameter should specify the document id to update. Note that the id
  is provided as an element of the list returned by successful calls to
  `rpubsUpload`.

- properties:

  A named list containing additional document properties (RPubs doesn't
  currently expect any additional properties, this parameter is reserved
  for future use).

## Value

A named list. If the upload was successful then the list contains a `id`
element that can be used to subsequently update the document as well as
a `continueUrl` element that provides a URL that a browser should be
opened to in order to complete publishing of the document. If the upload
fails then the list contains an `error` element which contains an
explanation of the error that occurred.

## Examples

``` r
if (FALSE) { # \dontrun{
# upload a document
result <- rpubsUpload("My document title", "Document.html")
if (!is.null(result$continueUrl))
   browseURL(result$continueUrl)
else
   stop(result$error)

# update the same document with a new title
updateResult <- rpubsUpload("My updated title", "Document.html",
                            id = result$id)
} # }
```
