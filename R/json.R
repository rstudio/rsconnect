
toJSON <- function(x, ...) {
  jsonlite::toJSON(x, dataframe="columns", null="null", na="null",
                   auto_unbox=TRUE, ...)
}
