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