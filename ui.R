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
  #shinythemes::themeSelector(),
  navbarPage(
    #"superhero",
    theme = shinytheme("superhero"),  # <--- Uncomment this for using a theme
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
      "Genre Distribution",
      titlePanel("Genre distribution of video games over the years"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("select_year"),
          actionButton("reset", "Clear All Game Types", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          uiOutput("filter_genre")
        ),
        mainPanel(
            h1("Information Obtained from the Pie Chart"),
            br(),
            textOutput("least_genre"),
            br(),
            textOutput("most_genre"),
            br(), 
            hr(),
            plotlyOutput("genre_pie_chart")
        )
      )
    ),
    tabPanel("Game Rating Summary",
             titlePanel("Rating Summary of Games Across Time, Genre, Theme, and Franchise"),
              sidebarPanel(
                uiOutput("select_element"),
                uiOutput("select_year_wayne"),
                uiOutput("base_game")
              ),
              mainPanel(
                plotlyOutput("lineplot"),
                br(),
                htmlOutput("lineplot_text")
              )),
    tabPanel("Game Hotness Gauge",
              sidebarPanel(
                  uiOutput("select_game"),
                  uiOutput("select_measure")
              ),
              mainPanel(
                plotlyOutput("gauge_plot")
              )
            )
  )
))