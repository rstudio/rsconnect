library(shiny)
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    # open Google. no reason, just do it
    browseURL("https://www.google.com/")
    dist <- rnorm(input$obs)
    hist(dist)
    # stop to examine state
    browser()
  })
  output$obs <- renderText({
    paste(input$obs, "\n", input$obs)
  })
})
