library(shiny)

shinyServer(function(input, output) {
  
  output$session <- renderPrint({
    sessionInfo()
  })

})
