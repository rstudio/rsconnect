#' Show Application Usage
#'
#' Show application usage of a currently deployed application
#' @param appName Name of application
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param account Account name. If a single account is registered on the
#'   system then this parameter can be omitted.
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers.
#' @param usageType Use metric to retreive (for example: "hours")
#' @param from Date range starting timestamp (Unix timestamp or relative time
#'   delta such as "2d" or "3w").
#' @param until Date range ending timestamp (Unix timestamp or relative time
#'   delta such as "2d" or "3w").
#' @param interval Summarization interval. Data points at intervals less then this
#'   will be grouped. (Number of seconds or relative time delta e.g. "1h").
#' @note This function only works for ShinyApps servers.
#' @export
showUsage <- function(appDir=getwd(), appName=NULL, account = NULL, server=NULL,
                      usageType="hours", from=NULL, until=NULL, interval=NULL) {

  # resolve account
  accountDetails <- accountInfo(resolveAccount(account, server), server)

  # intialize client
  api <- clientForAccount(accountDetails)

  # resolve application
  if (is.null(appName))
    appName = basename(appDir)
  application <- resolveApplication(accountDetails, appName)

  # get application usage
  data <- api$getAccountUsage(accountDetails$accountId,
                              usageType,
                              application$id,
                              from,
                              until,
                              interval)

  if (length(data$points) < 1) {
    stop("No data.", call.=FALSE)
  }

  # get data points
  points <- data$points[[1]]
  points <- lapply(points, function(X) {
    X[[1]] <- X[[1]]/1000 # convert from milliseconds to seconds
    X
  })

  # convert to data frame
  df <- data.frame(matrix(unlist(points), nrow=length(points), byrow=T), stringsAsFactors=FALSE)
  colnames(df) <- c("timestamp", usageType)
  return(df)
}

#' Show Application Metrics
#'
#' Show application metrics of a currently deployed application
#' @param metricSeries Metric series to query e.g. "container.cpu"
#' @param metricNames Metric names in the series to query e.g. c("cpu.user", "cpu.system")
#' @param appName Name of application
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param account Account name. If a single account is registered on the
#'   system then this parameter can be omitted.
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers.
#' @param from Date range starting timestamp (Unix timestamp or relative time
#'   delta such as "2d" or "3w").
#' @param until Date range ending timestamp (Unix timestamp or relative time
#'   delta such as "2d" or "3w").
#' @param interval Summarization interval. Data points at intervals less then this
#'   will be grouped. (Number of seconds or relative time delta e.g. "1h").
#' @note This function only works for ShinyApps servers.
#' @export
showMetrics <- function(metricSeries, metricNames, appDir=getwd(), appName=NULL, account = NULL, server=NULL,
                        from=NULL, until=NULL, interval=NULL) {

  # resolve account
  accountDetails <- accountInfo(resolveAccount(account, server), server)

  # intialize client
  api <- clientForAccount(accountDetails)

  # resolve application
  if (is.null(appName))
    appName = basename(appDir)
  application <- resolveApplication(accountDetails, appName)

  # get application usage
  data <- api$getApplicationMetrics(application$id,
                                    metricSeries,
                                    metricNames,
                                    from,
                                    until,
                                    interval)

  if (length(data$points) < 1) {
    stop("No data.", call.=FALSE)
  }

  # get data points
  points <- data$points
  points <- lapply(points, function(X) {
    X$time <- X$time/1000 # convert from milliseconds to seconds
    X
  })

  # convert to data frame
  df <- data.frame(matrix(unlist(points), nrow=length(points), byrow=T), stringsAsFactors=FALSE)
  colnames(df) <- c(metricNames, "timestamp")
  return(df)
}

#' Show Account Usage
#'
#' Show account usage
#' @param account Account name. If a single account is registered on the
#'   system then this parameter can be omitted.
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers.
#' @param usageType Use metric to retreive (for example: "hours")
#' @param from Date range starting timestamp (Unix timestamp or relative time
#'   delta such as "2d" or "3w").
#' @param until Date range ending timestamp (Unix timestamp or relative time
#'   delta such as "2d" or "3w").
#' @param interval Summarization interval. Data points at intervals less then this
#'   will be grouped. (Number of seconds or relative time delta e.g. "1h").
#' @note This function only works for ShinyApps servers.
#' @export
accountUsage <- function(account=NULL, server=NULL, usageType="hours",
                         from=NULL, until=NULL, interval=NULL) {
  # resolve account
  accountDetails <- accountInfo(resolveAccount(account, server), server)

  # intialize client
  api <- clientForAccount(accountDetails)

  # get application usage
  data <- api$getAccountUsage(accountDetails$accountId,
                              usageType,
                              NULL,
                              from,
                              until,
                              interval)
}
