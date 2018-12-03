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
    tabPanel(
      "About",
      titlePanel("What is this app and what is it for."),
      "This Shiny web app is for visualizing information about video games
        for video game players who likes to analyze and critique.
        we work with data retrived from Internet Games Database(IGDB), 
        a community-driven site that gathers and shares game-related information. The data is collected by IGDB.com, 
        and we gain access through its API."
    ),
    tabPanel(
      "Graph 1",
      titlePanel("Genre distribution of video games over the years"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("select_year"),
          uiOutput("filter_genre")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Pie Chart", plotlyOutput("genre_pie_chart"), textOutput("test_genre")),
            tabPanel("Tab 2"),
            tabPanel("Tab 3")
          )
        )
      )
    ),
    tabPanel("Graph 2",
              sidebarPanel(
                uiOutput("select_genre"),
                uiOutput("select_theme"),
                uiOutput("select_franchise"),
                uiOutput("select_year_wayne"),
                uiOutput("base_game")
              ),
              mainPanel(
                plotlyOutput("lineplot")
              )),
    tabPanel("Game Hotness Gauge",
              fluidPage(
                sidebarPanel(
                  uiOutput("select_game")
                )
              ),
              mainPanel(
                plotlyOutput("gauge_plot")
              )
            )
  )
))