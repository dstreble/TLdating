require(shiny)

shinyUI(navbarPage(
  id = "shinyDRAC",
  title = "ShinyDRAC",
  
  tabPanel("Sample information",
           uiOutput(outputId = "infoPage")),
  tabPanel("Input",
           uiOutput(outputId = "inPage")),
  tabPanel("Output",
           uiOutput(outputId = "outPage")),
  tabPanel("help",
           uiOutput(outputId = "helpPage"))
))