# addToDeploymentHistory() adds needed new lines

    Code
      addToDeploymentHistory("path", list(x = 1))
      writeLines(readLines(deploymentHistoryPath()))
    Output
      x: 1
      appPath: path
    Code
      addToDeploymentHistory("path", list(x = 2))
      writeLines(readLines(deploymentHistoryPath()))
    Output
      x: 2
      appPath: path
      
      x: 1
      appPath: path

# addToDeploymentHistory() caps records at rsconnect.max.history.records

    Code
      for (i in 1:5) {
        addToDeploymentHistory("path", list(x = i))
      }
      writeLines(readLines(deploymentHistoryPath()))
    Output
      x: 5
      appPath: path
      
      x: 4
      appPath: path
      
      x: 3
      appPath: path

# addToDeploymentHistory() preserves only the new record when max_records = 1L

    Code
      addToDeploymentHistory("path", list(x = 3))
      writeLines(readLines(deploymentHistoryPath()))
    Output
      x: 3
      appPath: path

# addToDeploymentHistory() restarts when history exceeds rsconnect.max.history.bytes

    Code
      addToDeploymentHistory("path", list(x = 3))
      writeLines(readLines(deploymentHistoryPath()))
    Output
      x: 3
      appPath: path

