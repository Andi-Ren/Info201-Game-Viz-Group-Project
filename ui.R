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
shinyUI(tagList(
  shinythemes::themeSelector(),
  navbarPage(
    # theme = "cerulean",  # <--- Uncomment this for using a theme
    "Game-Viz-Group",
    tabPanel("Graph 1",
             sidebarPanel(
               textInput("txt", "Select Game:", "Game1")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Tab 1"),
                 tabPanel("Tab 2"),
                 tabPanel("Tab 3")
               )
             )
    ),
    tabPanel("Graph 2",

              mainPanel(
                plotlyOutput("lineplot")
              )),
    tabPanel("Game Hotness Gauge",
              fluidPage(
                sidebarPanel(
                  uiOutput("select_game")
                ),
              ),
              mainPanel(
                plotlyOutput("gauge_plot")
              )
            )
  )
))