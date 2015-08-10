
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # read a file on disk
    otherFile <- read.table("~/.rsconnect-tests/local-file.txt")
    anotherFile <- readLines('../../foo.bar')
    serverFile <- "\\\\server\\path\\to\\file"
    validWeblink <- "//www.google.com/"

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # don't warn on this line
    text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

    ## read a csv file
    file <- read.csv("college.txt") ## bad
    file <- read.csv("College.txt") ## okay

    ## don't warn about absolute paths that could be URL query paths
    file <- paste("/applcations")

  })

})
