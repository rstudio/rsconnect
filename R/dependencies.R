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
appDependencies <- function(appDir = getwd(), appFiles=NULL) {
  # if the list of files wasn't specified, generate it
  if (is.null(appFiles)) {
    appFiles <- bundleFiles(appDir)
  }
  bundleDir <- bundleAppDir(appDir, appFiles)
  on.exit(unlink(bundleDir, recursive = TRUE), add = TRUE)
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
  repo.packages <- available.packages(contriburl = contrib.url(repos), type = "source")
  named.repos <- name.all.repos(repos)
  repo.lookup <- data.frame(
    name = names(named.repos),
    url = as.character(named.repos),
    contrib.url = contrib.url(named.repos),
    stringsAsFactors = FALSE)

  # get packages records defined in the lockfile
  records <- utils::tail(df, -1)

  # if the package is in a named CRAN-like repository capture it
  tmp <- lapply(seq.int(nrow(records)), function(i) {
    pkg <- records[i, "Package"]
    source <- records[i, "Source"]
    repository <- NA
    # capture Bioconcutor repository
    if (identical(source, "Bioconductor")) {
      if (pkg %in% biocPackages) {
        repository <- biocPackages[pkg, 'Repository']
      }
    } else if (tolower(source) %in% c("github", "bitbucket", "source")) {
      # leave source+SCM packages alone.
    } else if (pkg %in% repo.packages) {
      # capture CRAN-like repository

      # Find this package in the set of available packages then use its
      # contrib.url to map back to the configured repositories.
      package.contrib <- repo.packages[pkg, 'Repository']
      package.repo <- repo.lookup[repo.lookup$contrib.url == package.contrib,]
      # If the incoming package comes from CRAN, keep the CRAN name in place
      # even if that means using a different name than the repos list.
      #
      # The "cran" source is a well-known location for shinyapps.io.
      #
      # shinyapps.io isn't going to use the manifest-provided CRAN URL,
      # but other consumers (Connect) will.
      if (tolower(source) != "cran") {
        source <- package.repo$name
      }
      repository <- package.repo$url
    } else {
      warning(sprintf("Unable to find repository URL for package %s", pkg),
              immediate. = TRUE)
    }
    data.frame(Source = source, Repository = repository)
  })
  records[, c("Source","Repository")] <- do.call("rbind", tmp)
  return(records)
}

# generate a random name prefixed with "repo_".
random.repo.name <- function() {
  paste("repo_", paste(sample(LETTERS, 8, replace = TRUE), collapse = ""), sep = "")
}

# Given a list of optionally named repository URLs, return a list of
# repository URLs where each element is named. Incoming names are preserved.
# Un-named repositories are given random names.
name.all.repos <- function(repos) {
  repo.names <- names(repos)
  if (is.null(repo.names)) {
    # names(X) return NULL when nothing is named. Build a same-sized vector of
    # empty-string names, which is the "no name here" placeholder value
    # produced when its input has a mix of named and un-named items.
    repo.names <- rep("", length(repos))
  }
  names(repos) <- sapply(repo.names, function(name) {
    if (name == "") {
      # Assumption: Random names are not repeated across a repo list.
      random.repo.name()
    } else {
      name
    }
  }, USE.NAMES = FALSE)
  repos
}

# get source packages from CRAN
availableCRANSourcePackages <- function() {
  available.packages("https://cran.rstudio.com/src/contrib", type = "source")
}
