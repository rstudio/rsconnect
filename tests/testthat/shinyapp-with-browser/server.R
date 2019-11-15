library(shiny)
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    dist <- rnorm(input$obs)
    hist(dist)
    # stop to examine state
    browser()
  })
  output$obs <- renderText({paste(input$obs, "\n", input$obs)})
})
