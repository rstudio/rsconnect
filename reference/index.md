# Package index

## Package options

- [`rsconnectOptions`](https://rstudio.github.io/rsconnect/reference/options.md)
  : Package Options

## Accounts

- [`accountUsage()`](https://rstudio.github.io/rsconnect/reference/accountUsage.md)
  : Show Account Usage
- [`accounts()`](https://rstudio.github.io/rsconnect/reference/accounts.md)
  [`accountInfo()`](https://rstudio.github.io/rsconnect/reference/accounts.md)
  [`removeAccount()`](https://rstudio.github.io/rsconnect/reference/accounts.md)
  : Account Management Functions
- [`listAccountEnvVars()`](https://rstudio.github.io/rsconnect/reference/listAccountEnvVars.md)
  [`updateAccountEnvVars()`](https://rstudio.github.io/rsconnect/reference/listAccountEnvVars.md)
  : Maintain environment variables across multiple applications
- [`setAccountInfo()`](https://rstudio.github.io/rsconnect/reference/setAccountInfo.md)
  : Register account on shinyapps.io

## Users

- [`addAuthorizedUser()`](https://rstudio.github.io/rsconnect/reference/addAuthorizedUser.md)
  : Add authorized user to application
- [`authorizedUsers()`](https://rstudio.github.io/rsconnect/reference/authorizedUsers.md)
  : (Deprecated) List authorized users for an application
- [`connectApiUser()`](https://rstudio.github.io/rsconnect/reference/connectApiUser.md)
  [`connectUser()`](https://rstudio.github.io/rsconnect/reference/connectApiUser.md)
  : Register account on Posit Connect
- [`connectCloudUser()`](https://rstudio.github.io/rsconnect/reference/connectCloudUser.md)
  : Register account on Posit Connect Cloud
- [`connectSPCSUser()`](https://rstudio.github.io/rsconnect/reference/connectSPCSUser.md)
  : Register account on Posit Connect in Snowpark Container Services
- [`removeAuthorizedUser()`](https://rstudio.github.io/rsconnect/reference/removeAuthorizedUser.md)
  : Remove authorized user from an application
- [`showUsers()`](https://rstudio.github.io/rsconnect/reference/showUsers.md)
  : List authorized users for an application

## Deployments

- [`writeManifest()`](https://rstudio.github.io/rsconnect/reference/writeManifest.md)
  :

  Create a `manifest.json`

- [`deployAPI()`](https://rstudio.github.io/rsconnect/reference/deployAPI.md)
  : Deploy a Plumber API

- [`deployApp()`](https://rstudio.github.io/rsconnect/reference/deployApp.md)
  : Deploy an Application

- [`deployDoc()`](https://rstudio.github.io/rsconnect/reference/deployDoc.md)
  : Deploy a single document

- [`deploySite()`](https://rstudio.github.io/rsconnect/reference/deploySite.md)
  : Deploy a website

- [`deployTFModel()`](https://rstudio.github.io/rsconnect/reference/deployTFModel.md)
  : Deploy a TensorFlow saved model

- [`deployments()`](https://rstudio.github.io/rsconnect/reference/deployments.md)
  : List Application Deployments

- [`forgetDeployment()`](https://rstudio.github.io/rsconnect/reference/forgetDeployment.md)
  : Forget Application Deployment

- [`listDeploymentFiles()`](https://rstudio.github.io/rsconnect/reference/listDeploymentFiles.md)
  : Gather files to be bundled with an app

## Servers

- [`addServer()`](https://rstudio.github.io/rsconnect/reference/addServer.md)
  [`removeServer()`](https://rstudio.github.io/rsconnect/reference/addServer.md)
  [`addServerCertificate()`](https://rstudio.github.io/rsconnect/reference/addServer.md)
  : Server management
- [`servers()`](https://rstudio.github.io/rsconnect/reference/servers.md)
  [`serverInfo()`](https://rstudio.github.io/rsconnect/reference/servers.md)
  : Server metadata

## Properties and Metrics

- [`setProperty()`](https://rstudio.github.io/rsconnect/reference/setProperty.md)
  : Set Application property
- [`showProperties()`](https://rstudio.github.io/rsconnect/reference/showProperties.md)
  : Show Application property
- [`unsetProperty()`](https://rstudio.github.io/rsconnect/reference/unsetProperty.md)
  : Unset Application property
- [`showInvited()`](https://rstudio.github.io/rsconnect/reference/showInvited.md)
  : List invited users for an application
- [`showLogs()`](https://rstudio.github.io/rsconnect/reference/showLogs.md)
  [`getLogs()`](https://rstudio.github.io/rsconnect/reference/showLogs.md)
  : Application Logs
- [`showMetrics()`](https://rstudio.github.io/rsconnect/reference/showMetrics.md)
  : Show Application Metrics
- [`showUsage()`](https://rstudio.github.io/rsconnect/reference/showUsage.md)
  : Show Application Usage
- [`showUsers()`](https://rstudio.github.io/rsconnect/reference/showUsers.md)
  : List authorized users for an application
- [`resendInvitation()`](https://rstudio.github.io/rsconnect/reference/resendInvitation.md)
  : Resend invitation for invited users of an application

## Applications

- [`appDependencies()`](https://rstudio.github.io/rsconnect/reference/appDependencies.md)
  : Detect application dependencies
- [`applications()`](https://rstudio.github.io/rsconnect/reference/applications.md)
  : List Deployed Applications
- [`configureApp()`](https://rstudio.github.io/rsconnect/reference/configureApp.md)
  : Configure an Application
- [`deployApp()`](https://rstudio.github.io/rsconnect/reference/deployApp.md)
  : Deploy an Application
- [`purgeApp()`](https://rstudio.github.io/rsconnect/reference/purgeApp.md)
  : Purge an Application
- [`restartApp()`](https://rstudio.github.io/rsconnect/reference/restartApp.md)
  : Restart an Application
- [`syncAppMetadata()`](https://rstudio.github.io/rsconnect/reference/syncAppMetadata.md)
  : Update deployment records
- [`terminateApp()`](https://rstudio.github.io/rsconnect/reference/terminateApp.md)
  : Terminate an Application

## Other functions

- [`addLinter()`](https://rstudio.github.io/rsconnect/reference/addLinter.md)
  : Add a Linter
- [`lint()`](https://rstudio.github.io/rsconnect/reference/lint.md) :
  Lint a Project
- [`linter()`](https://rstudio.github.io/rsconnect/reference/linter.md)
  : Create a Linter
- [`makeLinterMessage()`](https://rstudio.github.io/rsconnect/reference/makeLinterMessage.md)
  : Construct a Linter Message
- [`rpubsUpload()`](https://rstudio.github.io/rsconnect/reference/rpubsUpload.md)
  : Upload a file to RPubs
- [`taskLog()`](https://rstudio.github.io/rsconnect/reference/taskLog.md)
  : Show task log
- [`tasks()`](https://rstudio.github.io/rsconnect/reference/tasks.md) :
  List Tasks
