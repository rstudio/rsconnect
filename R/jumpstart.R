#' Open a jumpstart example from RStudio Connect
#'
#' `pin_reactive_read()` and `pin_reactive_download()` wrap the results of
#' [pin_read()] and [pin_download()] into a Shiny reactive. This allows you to
#' use pinned data within your app, and have the results automatically
#' recompute when the pin is modified.
#'
#' @param name The name of the jumpstart example from an RStudio Connect
#'   instance.
#' @param connect_url The URL of your Connect instance
#' @param destination Download path
#' @param open_project Boolean that controls whether to open the project after
#'   download
#' @export
#' @examples
#' if (FALSE) {
#'   open_jumpstart(
#'     name = "stock-report",
#'     connect_url = "https://connect.example.com",
#'     destination = "/home/user/projects",
#'     open_project = TRUE
#'   )
#' }
open_jumpstart <- function(name, connect_url = Sys.getenv("CONNECT_URL", NA),
                           destination = "~", open_project = TRUE) {

  # Check the `connect_url` looks like a URL
  if (!grepl(pattern = "^http", x = connect_url)) {
    stop(
      "The `connect_url` parameter must be the full URL to your connect instance.\n",
      "  For example: \"https://connect.example.com\""
    )
  }


  # Assemble the download URL
  download_url <- paste0(
    connect_url, "/__api__/v1/experimental/examples/",
    name, "/zip"
  )


  # Assemble the destination path from the various fragments we collected
  zip_path <- normalizePath(file.path(
    path.expand(destination),
    paste0(
      name,
      ".zip"
    )
  ), mustWork = FALSE)


  # Check if the zip file already exists
  if (file.exists(zip_path)) {
    stop(
      "A file already exists at: ", zip_path, "\n",
      "  Please check and try again."
    )
  }


  # Download the file
  curl::curl_download(download_url,
    destfile = zip_path
  )


  # extrapolate some additional variables
  dest_dir_path <- dirname(zip_path) # The path the zip file will be extracted in
  proj_dir_path <- file.path(dest_dir_path, name) # The project path after unzipping
  # Rproj files do not always match the project name.

  # Check the path to unzip doesn't exist and unzip
  if (dir.exists(proj_dir_path)) {
    unlink(zip_path) # Removes the zip file in the event of a failure
    stop(
      "Can't extract zip file: target directory alraedy exists.",
      "  Please check the path, `", proj_dir_path, "` and try again"
    )
  }
  utils::unzip(zip_path, exdir = dest_dir_path)
  # Figure out the Rproj filename inside the project directory
  proj_name <- dir(
    path = proj_dir_path,
    pattern = ".*[.]Rproj$",
    recursive = FALSE
  )[1]
  proj_full_path <- file.path(
    dest_dir_path,
    name,
    proj_name
  ) # Full path to Rproj file

  unlink(zip_path)


  # Some debug information for development
  if (Sys.getenv("DEBUG", "default") == "TRUE") {
    cat("\nDebug Output:\n")
    cat("  Destination directory path: ", dest_dir_path, "\n")
    cat("  Project directory path:     ", proj_dir_path, "\n")
    cat("  Project name:               ", proj_name, "\n")
    cat("  Full project path:          ", proj_full_path, "\n")
  }


  if (open_project) {
    rstudioapi::openProject(proj_full_path, newSession = TRUE)
  }
}
