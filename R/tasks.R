#' List Tasks
#' 
#' @param account Account name. If a single account is registered on the 
#' system then this parameter can be omitted.
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
tasks <- function(account = NULL) {
  
  # resolve account and create lucid client
  accountInfo <- accountInfo(resolveAccount(account))
  lucid <- lucidClient(accountInfo)

  # list tasks 
  tasks <- lucid$listTasks(accountInfo$accountId)
  
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
#' @param account Account name. If a single account is registered on the 
#' system then this parameter can be omitted.
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
taskLog <- function(taskId, account = NULL, output = NULL) {

  # resolve account and create lucid client
  accountInfo <- accountInfo(resolveAccount(account))
  lucid <- lucidClient(accountInfo)

  if (identical(output, "stderr")) {
    conn <- stderr()
  } else{
    conn <- ""
  }
  
  # show task log
  cat(lucid$getTaskLogs(taskId), file=conn)

  # get child tasks
  tasks <- lucid$listTasks(accountInfo$accountId, 
                           filters=filterQuery("parent_id", taskId))
  
  # get child task logs
  for (task in tasks) {
    taskLog(task['id'], account, output)
  }
  
}
