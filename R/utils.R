
isStringParam <- function(param) {
  is.character(param) && (length(param) == 1)
}

stringParamErrorMessage <- function(param) {
  paste(param, "must be a single element character vector")
}
