library(shiny)

shinyUI(fluidPage(
  titlePanel("SessionInfo"),
  verbatimTextOutput("session")
))
