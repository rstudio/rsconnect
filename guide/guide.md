# Getting Started Guide
ShinyApps is a platform as a service (PaaS) for hosting Shiny applications.  This guide should help you get started 
with ShinyApps allowing you to create your online account, and deploy your first Shiny application to the cloud.


## Requirements

To get started with ShinyApps you will need:

- An R development environment
- The latest version of the [`devtools` package](https://github.com/hadley/devtools)
- The [`shinyapps` package](https://github.com/rstudio/shinyapps) from GitHub 
- A working shiny application on your machine
- Windows: RTools for building packages
- Mac OSX: XCode Command Line Tools for building packages

### Installingmthe devtools package

:warning: ShinyApps makes uses of the latest improvements to the `devtools` package, you **must** update `devtools` to 
version 1.4 or later.

Install `devtools` from CRAN:

    install.packages('devtools')

    (restart your R session)

### Installing the shinyapps package

The `shinyapps` package is used to deploy Shiny applications to the ShinyApps service. The package can only be installed 
from GitHub at this time.

Install the `shinyapps` package using `devtools`:

    devtools::install_github('rstudio/shinyapps')

After the `shinyapps` package has been installed, load it into your R session:

    library(shinyapps)

## Create A Project

You will probably want to create a new RStudio project for your application. For this guide, we will create a project
called "demo".

### Install Application dependencies 

The demo Shiny application we are going to deploy requires the `ggplot2` package as well as the `shiny`
package itself. You should ensure that any package that is required by your application is installed 
locally before you deploy your application:

    install.packages(c('ggplot2', 'shiny'))

### Shiny Demo Application

In this example, we have placed two Shiny source files: ui.R and server.R that you 
can cut and paste into your project:

**server.R**
```S
library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

  dataset <- reactive(function() {
    diamonds[sample(nrow(diamonds), input$sampleSize),]
  })

  output$plot <- reactivePlot(function() {

    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()

    if (input$color != 'None')
      p <- p + aes_string(color=input$color)

    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)

    if (input$jitter)
      p <- p + geom_jitter()
    if (input$smooth)
      p <- p + geom_smooth()

    print(p)

  }, height=700)

})
```

**ui.R**
```S
library(shiny)
library(ggplot2)

dataset <- diamonds

shinyUI(pageWithSidebar(

  headerPanel("Diamonds Explorer"),

  sidebarPanel(

    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                value=min(1000, nrow(dataset)), step=500, round=0),

    selectInput('x', 'X', names(dataset)),
    selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
    selectInput('color', 'Color', c('None', names(dataset))),

    checkboxInput('jitter', 'Jitter'),
    checkboxInput('smooth', 'Smooth'),

    selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
    selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
  ),

  mainPanel(
    plotOutput('plot')
  )
))
```

![Project](images/project.png)

### Test your application

You can test that your application works by running shiny.

    library(shiny)
    runApp()

## Configuring ShinyApps

To use the `shinyapps` package, you will need to signup for a ShinyApps account. When you create your account, a
token and secret will be generated automatically for you. This token and secret are used in place of your username
and password when deploying and managing applications.

### Sign In to ShinyApps.io

Go to [my.shinyapps.io](https://my.shinyapps.io) and click "Sign In". You will be prompted to signin using 
your Google Account.
The first time you signin, you will be prompted to setup your account. The account name will be used
as the domain name for all your applications. Account names must be between 4 and 63 characters and
can only contain letters, numbers and dashes (-). Additionally, account names may not begin with a
number or a dash or end with a dash (see [RFC 952](http://tools.ietf.org/html/rfc952)). Some account names may
be reserved.

### Configuring your account

Once your account is set up in ShinyApps, you will want to configure the `shinyapps` package to
use your account. You will need to retrieve your token from the ShinyApps dashboard. Tokens are
listed under the `Tokens` page found in the menu on the top right of the ShinyApps dashboard.

![Tokens](images/tokens.png)

You can configure the `shinyapps` package to use your account in two ways:

#### Method 1

Click the copy button on the token page and paste the result into your R session. This will paste the full 
command to configure your account using the appropriate parameters for the `shinyapps::setAccountInfo` function.

#### Method 2

Run the 'setAccountInfo' function from the `shinyapps` package passing in the token and secret from the
Profile / Tokens page.

    shinyapps::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")

## Packages

### Package Dependencies

When you deploy your application, the `shinyapps` package will attempt to detect the packages that
are used by your application. This list of packages and their dependencies are sent along with your
code to the ShinyApps service so the packages can be built and installed into the R library for
your application. The first time you deploy your application, it may take some time to build these
packages depending on how many of them there are. However, you will not have to wait for these
packages to build during subsequent deployments (unless you upgrade or downgraded a package).

For more information on application package dependencies, see the documentation for the `shinyapps`
package.

<!--- Any more specific links? (Or is it not written yet?) This seems important. -->

### Package Sources

Currently, the ShinyApps.io service only supports deploying packages that are from CRAN, and GitHub.

BioConductor and R-Forge packages will be supported soon.

### Important Note on GitHub Packages

:warning: Only packages that are installed from GitHub with the `devtools::install_github` using the
**latest** version of `devtools` are supported. This means that packages that you have previously
installed from GitHub using `devtools`, **must** be reinstalled before you can deploy your
application.

__If you are getting an error such as "PackageSourceError" when you attempt to deploy, this is
likely the reason. Remember, any GitHub package, and its dependencies will need to be reinstalled
using the latest version of `devtools`.__

## Deploying Applications

To deploy your application, use the `deployApp` command from the `shinyapps` packages.

    deployApp()

![Deploy](images/deploy.png)

Once the deploy finishes, your browser should automatically open up to your newly deployed application.

Congratulations! you've deployed your first application. :-)

Feel free to make changes to your code, and run `deployApp` again. You will note the second time you
deploy, it will take much less time, as the packages have already been built.

### Application Instances

When an application is deployed, it runs on its own virtualized server called an instance. Each
instance runs an identical copy of the code and packages that were deployed (called the image).

During an application deployment, a new image is created with the updated code and packages, and
one or more instances are started with the new image. The old instances are shutdown and destroyed.
This has a few implications that should be taken into consideration when writing and deploying
Shiny applications on the ShinyApps platform:

1) __Data written by a Shiny application to the local filesystem of an instance will not be persisted
across application deployments.__ Additionally, the distributed nature of the ShinyApps platform
means that instances may be shutdown and re-created at any time for maintenance or to recover from
server failures.

2) It is possible to have more then one instance of an application. This means that __multiple instances
of an application do not share a local filesystem__. A file written to one instance will not be available
to the other.

In addition to providing an isolated environment for each application, instances are also limited in
the amount of system resources they are allowed to consume. This means that instances are only
allowed to use a given amount of memory depending on their type. The table below outlines the various
instance types, and how much memory is allowed. __By default, all applications are deployed on
'small' instances, and are allowed to use 256 MB of memory.__ The instance type used by an application
can be changed using the `configureApp` function from the `shinyapps` package. See the section below on 
_Configuring Shiny Applications_.


<table>
  <tr>
    <th>
    Instance Type
    </th>
    <th>
    Memory
    </th>
  </tr>
  <tr>
    <td>
    small (default)
    </td>
    <td>
    256 MB
    </td>
  </tr>
  <tr>
    <td>
    medium
    </td>
    <td>
    512 MB
    </td>
  </tr>
  <tr>
    <td>
    large
    </td>
    <td>
    1024 MB
    </td>
  </tr>
  <tr>
    <td>
    xlarge
    </td>
    <td>
    2048 MB
    </td>
  </tr>
  <tr>
    <td>
    xxlarge
    </td>
    <td>
    4096 MB
    </td>
  </tr>
</table>

Note: Instance types and limits are not finalized, and may be changed in the future.


### Configuring Shiny Applications

Both the instance type, and the number of instances of an application are configurable using the 
`configureApp` function from the `shinyapps` package.

To change the instance type of your application from the default of small to medium run:

    shinyapps::configureApp(APPNAME, size="medium")

This will redeploy your application using the medium instance type.

## Application Authentication

With ShinyApps it is possible to configure authentication for your application. Once authentication has
been enabled, when a user visits your application, he or she will be prompted to enter a username 
and password. Only user's who have logged in with a valid username and password will be able view or use your
application. Authentication is automatically enabled when the first authorized user is added.

To get started, you will first need prepare your system to be able to build packages. Ensure you have
installed the following requirements for your system before continuing:

- Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/) for building packages
- Mac OSX: XCode Command Line Tools for building packages
- Linux: GCC

### Install the latest version of shinyapps package

For authentication support, you will need `shinyapps` >= 0.3.

Install the `shinyapps` package using `devtools`:

```S
devtools::install_github('rstudio/shinyapps')
```

### Install the latest version of scrypt package

You will also need the scrypt package used for encrypting passwords.

Install the `scrypt` package using `devtools`:

```S
devtools::install_github('rstudio/rscrypt')
```

You should restart your R session at this point, if you already had the `shinyapps`
package installed.

### Adding Authorized Users

The `shinyapps` package exposes a number of functions for managing an application's authorized users.
To get started, ensure the `shinyapps` package is loaded:

```S
library(shinyapps)
```

As with other `shinyapps` functions, you should change your working directory to where your 
application code is:

```S
setwd("/path/to/my/shiny/app")
```

To add an authorized user, use the `shinyapps::addAuthorizedUser` function. You will
be prompted to enter a password for the user. Please remember that for security, 
passwords are stored using an scrypt hash. Once stored, passwords can not be retrieved (but they 
can always be reset).

:exclamation: Passwords must be at least 4 characters in length, and may not contain: "\t", "\n", "$" or ":".

```S
addAuthorizedUser("andy")
```

After adding or removing a user, you will need deploy your application using the
`shinyapps::deployApp` function.

```S
deployApp()
```

That's it. You will now be prompted for a username and password, when your application is visited.

Note: If a user forgets his or her password, you can reset it using the
`shinyapps::addAuthorizedUser` function. You will be prompted to confirm you want to reset the
user's password.

### Removing Authorized Users

To remove an authorized user, use the `shinyapps::removeAuthorizedUser` function.

```S
removeAuthorizedUser("andy")
```

After adding or removing a user, you will need deploy your application using the
`shinyapps::deployApp` function.
