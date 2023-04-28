# addToDeploymentHistory() adds needed new lines

    Code
      addToDeploymentHistory("path", list(x = 1))
      writeLines(readLines(deploymentHistoryPath()))
    Output
      x: 1
      appPath: path
      version: 1
    Code
      addToDeploymentHistory("path", list(x = 2))
      writeLines(readLines(deploymentHistoryPath()))
    Output
      x: 2
      appPath: path
      version: 1
      
      x: 1
      appPath: path
      version: 1

