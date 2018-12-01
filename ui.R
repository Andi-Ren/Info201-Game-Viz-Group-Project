#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("shinythemes")
# install.packages("plotly")
library(shiny)
library(shinythemes)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(navbarPage(
  title = "IGDB",
  theme = shinytheme("slate"),
  tabPanel(
    "Overview",
    mainPanel(
      tags$h1("Overview of the project")
    )
  ),
  tabPanel(
    "Graph1",
    mainPanel(
      tags$h1("Overview of the project")
    )
  ),
  tabPanel(
    "Graph2",
    mainPanel(
      tags$h1("Overview of the project")
    )
  ),
  tabPanel(
    "Graph3",
    mainPanel(
      tags$h1("Overview of the project")
    )
  ),
  tabPanel(
    "Insight",
    mainPanel(
    )
  )
))