
snapshotRDependencies <- function(appDir, implicit_dependencies=c(), verbose = FALSE) {

  # create a packrat "snapshot"

  addPackratSnapshot(appDir, implicit_dependencies, verbose = verbose)

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

  # read available.packages filters (allow user to override if necessary;
  # this is primarily to allow debugging)
  #
  # note that we explicitly exclude the 'R_version' filter as we want to ensure
  # that packages which require newer versions of R than the one currently
  # in use can still be marked as available on CRAN -- for example, currently
  # the package 'foreign' requires 'R (>= 4.0.0)' but older versions of R
  # can still successfully install older versions from the CRAN archive
  filters <- getOption(
    "available_packages_filters",
    default = c("duplicates")
  )

  # get Bioconductor repos if any
  biocRepos <- repos[grep('BioC', names(repos), perl = TRUE, value = TRUE)]
  biocPackages <- if (length(biocRepos) > 0) {
    available.packages(
      contriburl = contrib.url(biocRepos, type = "source"),
      type = "source",
      filters = filters
    )
  }

  # read available packages
  repo.packages <- available.packages(
    contriburl = contrib.url(repos, type = "source"),
    type = "source",
    filters = filters
  )

  named.repos <- name.all.repos(repos)
  repo.lookup <- data.frame(
    name = names(named.repos),
    url = as.character(named.repos),
    contrib.url = contrib.url(named.repos, type = "source"),
    stringsAsFactors = FALSE)

  # get packages records defined in the lockfile
  records <- utils::tail(df, -1)

  # if the package is in a named CRAN-like repository capture it
  tmp <- lapply(seq_len(nrow(records)), function(i) {

    pkg <- records[i, "Package"]
    source <- records[i, "Source"]
    repository <- NA
    # capture Bioconcutor repository
    if (identical(source, "Bioconductor")) {
      if (pkg %in% biocPackages) {
        repository <- biocPackages[pkg, 'Repository']
      }
    } else if (isSCMSource(source)) {
      # leave source+SCM packages alone.
    } else if (pkg %in% rownames(repo.packages)) {
      # capture CRAN-like repository

      # Find this package in the set of available packages then use its
      # contrib.url to map back to the configured repositories.
      package.contrib <- repo.packages[pkg, 'Repository']
      package.repo.index <- vapply(repo.lookup$contrib.url,
                                   function(url) grepl(url, package.contrib, fixed = TRUE), logical(1))
      package.repo <- repo.lookup[package.repo.index, ][1, ]
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
    }
    # validatePackageSource will emit a warning for packages with NA repository.
    data.frame(Source = source, Repository = repository)
  })
  records[, c("Source","Repository")] <- do.call("rbind", tmp)
  return(records)
}

# Return TRUE when the source indicates that a package was installed from
# source or comes from a source control system. This indicates that we will
# not have a repostory URL; location is recorded elsewhere.
isSCMSource <- function(source) {
  tolower(source) %in% c("github", "gitlab", "bitbucket", "source")
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
