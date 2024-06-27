#' List Tasks
#'
#' @inheritParams deployApp
#' @return
#' Returns a data frame with the following columns:
#' \tabular{ll}{
#' `id` \tab Task id \cr
#' `action` \tab Task action\cr
#' `status` \tab Current task status\cr
#' `created_time` \tab Task creation time\cr
#' `finished_time` \tab Task finished time\cr
#' }
#' @examples
#' \dontrun{
#'
#' # list tasks for the default account
#' tasks()
#'
#' }
#' @seealso [taskLog()]
#' @note This function works only with shinyapps.io and posit.cloud.
#' @export
tasks <- function(account = NULL, server = NULL) {

  # resolve account and create connect client
  accountDetails <- accountInfo(account, server)
  checkCloudServer(accountDetails$server)

  client <- clientForAccount(accountDetails)

  # list tasks
  tasks <- client$listTasks(accountDetails$accountId)

  # extract the subset of fields we're interested in
  res <- lapply(tasks, `[`, c("id", "action", "status", "created_time"))

  # convert to data frame
  res <- do.call(rbind, res)

  as.data.frame(res, stringsAsFactors = FALSE)
}


#' Show task log
#'
#' Writes the task log for the given task
#' @param taskId Task Id
#' @inheritParams deployApp
#' @param output Where to write output. Valid values are `NULL` or `stderr`
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
#' @seealso [tasks()]
#' @note This function works only with shinyapps.io and posit.cloud.
#' @export
taskLog <- function(taskId, account = NULL, server = NULL, output = NULL) {

  accountDetails <- accountInfo(account, server)
  checkCloudServer(accountDetails$server)

  client <- clientForAccount(accountDetails)

  if (identical(output, "stderr")) {
    conn <- stderr()
  } else{
    conn <- ""
  }

  # show task log
  cat(client$getTaskLogs(taskId), file = conn)

  # get child tasks
  tasks <- client$listTasks(accountDetails$accountId,
                            filters = filterQuery("parent_id", taskId))

  # get child task logs
  for (task in tasks) {
    taskLog(task["id"], account = account, server = server, output = output)
  }

}
