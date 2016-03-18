#' Detect Application Dependencies
#'
#' Recursively detect all package dependencies for an application. This function
#' parses all .R files in the applicaition directory to determine what packages
#' the application depends on; and for each of those packages what other
#' packages they depend on.
#' @inheritParams deployApp
#' @param appDir Directory containing application. Defaults to current working
#'   directory.
#' @return Returns a data frame listing the package
#'   dependencies detected for the application: \tabular{ll}{ \code{package}
#'   \tab Name of package \cr \code{version} \tab Version of package\cr }
#' @details Dependencies are determined by parsing application source code and
#'   looking for calls to \code{library}, \code{require}, \code{::}, and
#'   \code{:::}.
#'
#'   Recursive dependencies are detected by examining the \code{Depends},
#'   \code{Imports}, and \code{LinkingTo} fields of the packages immediately
#'   dependend on by the application.
#'
#' @note Since the \code{Suggests} field is not included when determining
#'   recursive dependencies of packages, it's possible that not every package
#'   required to run your application will be detected.
#'
#'   In this case, you can force a package to be included dependency by
#'   inserting call(s) to \code{require} within your source directory. This code
#'   need not actually execute, for example you could create a standalone file
#'   named \code{dependencies.R} with the following code: \cr \cr
#'   \code{require(xts)} \cr \code{require(colorspace)} \cr
#'
#'   This will force the \code{xts} and \code{colorspace} packages to be
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
#' @seealso \link[rsconnect:rsconnectPackages]{Using Packages with rsconnect}
#' @export
appDependencies <- function(appDir = getwd(), appFiles=NULL) {
  # if the list of files wasn't specified, generate it
  if (is.null(appFiles)) {
    appFiles <- bundleFiles(appDir)
  }
  bundleDir <- bundleAppDir(appDir, appFiles)
  deps <- snapshotDependencies(bundleDir)
  data.frame(package = deps[,"Package"],
             version = deps[,"Version"],
             source = deps[,"Source"],
             row.names = c(1:length(deps[,"Package"])),
             stringsAsFactors=FALSE)
}

snapshotDependencies <- function(appDir, implicit_dependencies=c()) {

  # create a packrat "snapshot"
  addPackratSnapshot(appDir, implicit_dependencies)

  # TODO: should we care about lockfile version or packrat version?
  lockFilePath <- snapshotLockFile(appDir)
  df <- as.data.frame(read.dcf(lockFilePath), stringsAsFactors = FALSE)

  # get repos defined in the lockfile
  repos <- gsub("[\r\n]", " ", df[1, 'Repos'])
  repos <- strsplit(unlist(strsplit(repos, "\\s*,\\s*", perl = TRUE)), "=", fixed = TRUE)
  repos <- setNames(
    sapply(repos, "[[", 2),
    sapply(repos, "[[", 1)
  )

  # get Bioconductor repos if any
  biocRepos = repos[grep('BioC', names(repos), perl=TRUE, value=TRUE)]
  if (length(biocRepos) > 0) {
    biocPackages = available.packages(contriburl=contrib.url(biocRepos, type="source"))
  } else {
    biocPackages = c()
  }

  # get packages records defined in the lockfile
  records <- utils::tail(df, -1)

  # if the package is in a named CRAN-like repository capture it
  tmp <- sapply(seq.int(nrow(records)), function(i) {
    pkg <- records[i, "Package"]
    source <- records[i, "Source"]
    repository <- NA
    # capture Bioconcutor repository
    if (identical(source, "Bioconductor")) {
      if (pkg %in% biocPackages) {
        repository <- biocPackages[pkg, 'Repository']
      }
    } else {
      # capture CRAN-like repository
      repository <- if (source %in% names(repos)) repos[[source]] else NA
    }
    repository
  })
  records[, "Repository"] <- tmp
  return(records)
}

# get source packages from CRAN
availableCRANSourcePackages <- function() {
  available.packages("https://cran.rstudio.com/src/contrib", type = "source")
}
