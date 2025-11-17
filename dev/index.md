# rsconnect

rsconnect makes it easy to publish your Shiny apps, RMarkdown and Quarto
documents, and Plumber APIs^(\*) to [Posit
Connect](https://posit.co/products/enterprise/connect/), [Posit Connect
Cloud](https://connect.posit.cloud/), and
[shinyapps.io](https://www.shinyapps.io/) from R.

(If you’re looking for the Python equivalent, try
[rsconnect-python](https://pypi.org/project/rsconnect-python/).)

## Installation

You can install the released version of rsconnect from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rsconnect")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rstudio/rsconnect")
```

## Setup

To use rsconnect, you first need to teach it about the server you want
to publish to. If you use the RStudio IDE, the easiest way to get set up
is to use the publishing dialog, which you can find by clicking the
“Tools” menu, then selecting “Global options”, then clicking
“Publishing”. Click “Connect” to add new servers.

You can also connect from any R session by running a little code:

- For Posit Conect Cloud, call
  [`connectCloudUser()`](https://rstudio.github.io/rsconnect/dev/reference/connectCloudUser.md)
  to authenticate through the browser.

- For Posit Connect, first use
  [`addServer()`](https://rstudio.github.io/rsconnect/dev/reference/addServer.md)
  to register your server with rsconnect, then call either
  [`connectUser()`](https://rstudio.github.io/rsconnect/dev/reference/connectApiUser.md)
  or
  [`connectApiUser()`](https://rstudio.github.io/rsconnect/dev/reference/connectApiUser.md).
  [`connectUser()`](https://rstudio.github.io/rsconnect/dev/reference/connectApiUser.md)
  is a bit simpler if you’re in an interactive session;
  [`connectApiUser()`](https://rstudio.github.io/rsconnect/dev/reference/connectApiUser.md)
  works anywhere but requires a you to copy and paste an API key from
  your user profile.

- For shinyapps.io, go to your [tokens
  page](https://www.shinyapps.io/admin/#/tokens) and click “Add Token”,
  then follow the instructions to copy and paste the appropriate call to
  [`setAccountInfo()`](https://rstudio.github.io/rsconnect/dev/reference/setAccountInfo.md).
  Learn more in the [Getting Started
  Guide](https://shiny.rstudio.com/articles/shinyapps.html).

Now that you’re setup you can use
[`deployApp()`](https://rstudio.github.io/rsconnect/dev/reference/deployApp.md),
[`deployDoc()`](https://rstudio.github.io/rsconnect/dev/reference/deployDoc.md),
and friends to publish your apps, documentations, APIs and more.
