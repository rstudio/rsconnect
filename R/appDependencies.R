#' Detect Application Dependencies
#'
#' Recursively detect all package dependencies for an application. This function
#' parses all .R files in the application directory to determine what packages
#' the application depends on; and for each of those packages what other
#' packages they depend on.
#' @inheritParams deployApp
#' @param appDir Directory containing application. Defaults to current working
#'   directory.
#' @return Returns a data frame listing the package
#'   dependencies detected for the application: \tabular{ll}{ `package`
#'   \tab Name of package \cr `version` \tab Version of package\cr }
#' @details Dependencies are determined by parsing application source code and
#'   looking for calls to `library`, `require`, `::`, and
#'   `:::`.
#'
#'   Recursive dependencies are detected by examining the `Depends`,
#'   `Imports`, and `LinkingTo` fields of the packages immediately
#'   dependend on by the application.
#'
#' @note Since the `Suggests` field is not included when determining
#'   recursive dependencies of packages, it's possible that not every package
#'   required to run your application will be detected.
#'
#'   In this case, you can force a package to be included dependency by
#'   inserting call(s) to `require` within your source directory. This code
#'   need not actually execute, for example you could create a standalone file
#'   named `dependencies.R` with the following code: \cr \cr
#'   `require(xts)` \cr `require(colorspace)` \cr
#'
#'   This will force the `xts` and `colorspace` packages to be
#'   installed along with the rest of your application when it is deployed.
#' @examples
#' \dontrun{
#'
#' # dependencies for the app in the current working dir
#' appDependencies()
#'
#' # dependencies for an app in another directory
#' appDependencies("~/projects/shiny/app1")
#' }
#' @seealso [rsconnectPackages](Using Packages with rsconnect)
#' @export
appDependencies <- function(appDir = getwd(), appFiles = NULL) {
  # if the list of files wasn't specified, generate it
  if (is.null(appFiles)) {
    appFiles <- bundleFiles(appDir)
  }
  bundleDir <- bundleAppDir(appDir, appFiles)
  on.exit(unlink(bundleDir, recursive = TRUE), add = TRUE)
  deps <- snapshotRDependencies(bundleDir)
  data.frame(package = deps[, "Package"],
             version = deps[, "Version"],
             source = deps[, "Source"],
             row.names = c(1:length(deps[, "Package"])),
             stringsAsFactors = FALSE)
}
