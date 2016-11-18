library(shiny)
library(babynames)
ui <- fluidPage(
  p("Just a Test"),
  textOutput("test")
)

server <- function(input, output, session){
  output$test <- renderText({
    b <- head(babynames, 1)
    b <- as.character(b)
  })
}

shinyApp(ui=ui, server=server)