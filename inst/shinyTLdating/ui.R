
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(navbarPage(
  title = "shinyTLdating",
  
  tabPanel("Sample information",
           uiOutput(outputId = "infoPage")),
  tabPanel("Equivalent dose",
           uiOutput(outputId = "dePage")),
  tabPanel("a-value",
           uiOutput(outputId = "aPage")),
  tabPanel("Annual dose rate",
           uiOutput(outputId = "drPage")),
  tabPanel("Age",
           uiOutput(outputId = "agePage")),
  tabPanel("help",
           uiOutput(outputId = "helpPage"))
))

