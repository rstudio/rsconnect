
# detect all package dependencies for an application (recursively discovers
# dependencies for all .R files in the app directory)
appDependencies <- function(appDir) {
  
  # first get the packages referred to in source code
  pkgs <- character()
  sapply(list.files(appDir, pattern=glob2rx("*.R"), 
                    ignore.case=TRUE, recursive=TRUE),
         function(file) {
           pkgs <<- append(pkgs, fileDependencies(file.path(appDir, file)))
         })
  pkgs <- unique(pkgs)
  
  # then calculate recursive dependencies
  available <- availableCRANSourcePackages()
  which <-  c("Depends", "Imports", "LinkingTo", "Suggests")
  depsList <- tools::package_dependencies(pkgs, available, recursive=TRUE)
  
  # flatten the list
  deps <- unlist(depsList, recursive=TRUE, use.names=FALSE)
  pkgs <- unique(c(pkgs, deps))  
  pkgs
}

# detect all package dependencies for a source file (parses the file and then
# recursively examines all expressions in the file)
fileDependencies <- function(file) {
  
  # build a list of package dependencies to return
  pkgs <- character()
  
  # parse file and examine expressions
  exprs <- parse(file, n = -1L) 
  for (i in seq_along(exprs))
    pkgs <- append(pkgs, expressionDependencies(exprs[[i]]))
  
  # return packages
  unique(pkgs)
}

# detect the pacakge dependencies of an expression (adapted from 
# tools:::.check_packages_used)
expressionDependencies <- function(e) {
  
  # build a list of packages to return
  pkgs <- character()
  
  # examine calls
  if (is.call(e) || is.expression(e)) {
    
    # extract call
    call <- deparse(e[[1L]])[1L]
    
    # check for library or require and extract package argument
    if ((call %in% c("library", "require")) && (length(e) >= 2L)) {
      keep <- sapply(e, function(x) deparse(x)[1L] != "...")
      mc <- match.call(get(call, baseenv()), e[keep])
      if (!is.null(pkg <- mc$package)) {
        # ensure that types are rational
        if (!identical(mc$character.only, TRUE) || 
              identical(class(pkg), "character")) {
          pkg <- sub("^\"(.*)\"$", "\\1", deparse(pkg))
          pkgs <- append(pkgs, pkg)
        }
      }
    }
    
    # check for :: or :::
    else if (call %in% c("::", ":::")) {
      pkg <- deparse(e[[2L]])
      pkgs <- append(pkgs, pkg)
    }
    
    # check for uses of methods
    else if (call %in% c("setClass", "setMethod")) {
      pkgs <- append(pkgs, "methods")
    }
    
    # process subexpressions
    for (i in seq_along(e)) 
      pkgs <- append(pkgs, Recall(e[[i]]))
  }
  
  # return packages
  unique(pkgs)
}

# get source packages from CRAN
availableCRANSourcePackages <- function() {
  available.packages(contrib.url(getOption("repos")[["CRAN"]], "source"),
                     type = "source")
}
