#' Detect Application Dependencies
#' 
#' Recursively detect all package dependencies for an application. This function
#' parses all .R files in the applicaition directory to determine what packages 
#' the application depends on; and for each of those packages what other 
#' packages they depend on.
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
#' @seealso \link[shinyapps:shinyappsPackages]{Using Packages with ShinyApps}
#' @export
appDependencies <- function(appDir = getwd()) {
  deps <- dirDependencies(appDir)
  versions <- sapply(deps, function(dep)  {
    utils::packageDescription(dep, fields="Version")
  })
  data.frame(package = deps, 
             version = versions, 
             row.names = c(1:length(deps)),
             stringsAsFactors=FALSE)
}
 
# detect all package dependencies for a directory of files
dirDependencies <- function(dir) {
  
  # start with the shiny package (the dependency on shiny can be implicit
  # as the shiny package is automatically loaded prior to sourcing ui.R
  # and server.R)
  pkgs <- c("shiny")
  
  # now get the packages referred to in the source code; look in both .R
  # and .Rmd files
  sapply(list.files(dir, pattern="^.*[.][Rr]([Mm][Dd])?$", 
                    ignore.case=TRUE, recursive=TRUE),
         function(file) {
           # ignore files in the Packrat folder 
           if (!identical(substr(file, 1, 8), "packrat/"))
             pkgs <<- append(pkgs, fileDependencies(file.path(dir, file)))
         })
  pkgs <- unique(pkgs)
  
  # then calculate recursive dependencies
  installed <- installed.packages()
  which <-  c("Depends", "Imports", "LinkingTo")
  depsList <- tools::package_dependencies(pkgs, 
                                          installed, 
                                          which, 
                                          recursive=TRUE)
  
  # flatten the list
  deps <- unlist(depsList, recursive=TRUE, use.names=FALSE)
  unique(c(pkgs, deps))  
}

# detect all package dependencies for a source file (parses the file and then
# recursively examines all expressions in the file)
fileDependencies <- function(file) {
  
  # build a list of package dependencies to return
  pkgs <- character()
  
  ext <- tolower(tools::file_ext(file))
  if (identical(ext, "rmd")) {
    # if this is an R Markdown file, we'll need to use knitr to extract its code
    # chunks for parsing 
    if (require(knitr, quietly = TRUE)) {
      purled <- ""
      purled_con <- textConnection("purled", open = "w", local = TRUE)
      knitr::purl(file, purled_con, quiet = TRUE, documentation = 0,
                  encoding = checkEncoding(file))
      close(purled_con)
      input <- textConnection(purled, open = "r") 
      # rmarkdown is an implicit dependency (it's not referenced in the Rmd source)
      pkgs <- c(pkgs, "rmarkdown")
    } else {
      # no knitr, return an empty list
      warning("Could not determine dependencies for ", file, 
              " (requires knitr)")
      return(character())
    }    
  } else if (identical(ext, "r")) {
    # if this is an R script, we can parse its output directly
    input <- base::file(file, encoding = checkEncoding(file))
    on.exit(close(input), add = TRUE)
  } else {
    # if it's not an extension we know, emit a warning
    warning("Could not determine dependencies for ", file, 
            " (extension .", ext, " unknown)")
    return(character())
  }
  
  # parse file and examine expressions
  withCallingHandlers(
    exprs <- parse(input, n = -1L, encoding = checkEncoding(file)),
    error = function(e) message('\n\n* Failed to parse ', file, '\n')
  )
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
  available.packages("http://cran.rstudio.com/src/contrib", type = "source")
}
