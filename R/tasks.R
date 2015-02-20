#' List Tasks
#'
#' @param account Account name. If a single account is registered on the system
#'   then this parameter can be omitted.
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers (see \code{\link{servers}})
#' @return
#' Returns a data frame with the following columns:
#' \tabular{ll}{
#' \code{id} \tab Task id \cr
#' \code{action} \tab Task action\cr
#' \code{status} \tab Current task status\cr
#' \code{created_time} \tab Task creation time\cr
#' \code{finished_time} \tab Task finished time\cr
#' }
#' @examples
#' \dontrun{
#'
#' # list tasks for the default account
#' tasks()
#'
#' }
#' @seealso \code{\link{taskLog}}
#' @export
tasks <- function(account = NULL, server = NULL) {

  # resolve account and create connect client
  accountDetails <- accountInfo(resolveAccount(account, server), server)
  client <- clientForAccount(accountDetails)

  # list tasks
  tasks <- client$listTasks(accountDetails$accountId)

  # extract the subset of fields we're interested in
  res <- lapply(tasks, `[`, c('id', 'action', 'status', 'created_time'))

  # convert to data frame
  res <- do.call(rbind, res)

  as.data.frame(res, stringsAsFactors = FALSE)
}


#' Show task log
#'
#' Writes the task log for the given task
#' @param taskId Task Id
#' @param account Account name. If a single account is registered on the system
#'   then this parameter can be omitted.
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers (see \code{\link{servers}})
#' @param output Where to write output. Valid values are \code{NULL} or \code{stderr}
#' @examples
#' \dontrun{
#'
#' # write task log to stdout
#' taskLog(12345)
#'
#' # write task log to stderr
#' taskLog(12345, output="stderr")
#'
#' }
#' @seealso \code{\link{tasks}}
#' @export
taskLog <- function(taskId, account = NULL, server = NULL, output = NULL) {

  # resolve account and create connect client
  accountDetails <- accountInfo(resolveAccount(account, server), server)
  client <- clientForAccount(accountDetails)

  if (identical(output, "stderr")) {
    conn <- stderr()
  } else{
    conn <- ""
  }

  # show task log
  cat(client$getTaskLogs(taskId), file=conn)

  # get child tasks
  tasks <- client$listTasks(accountDetails$accountId,
                            filters=filterQuery("parent_id", taskId))

  # get child task logs
  for (task in tasks) {
    taskLog(task['id'], account = account, server = server, output = output)
  }

}
