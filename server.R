#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)

load("data/5000games.Rda")
load("data/companylist.Rdat")
load("data/genrelist.Rdat")
load("data/themedata.Rdat")
load("data/collectiondata.Rdat")
game_datas_all <- game_datas_all %>% 
  mutate(first_release_date = as.Date(game_datas_all$first_release_date))
collection_datas <- collection_datas %>% arrange(name)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$select_genre <- renderUI({
    selectInput("genre",
                label = "Genre",
                choices = genre_datas$name)
  })
  output$select_theme <- renderUI({
    selectInput("theme",
                label = "Theme",
                choices = theme_datas$name)
  })
  output$select_franchise <- renderUI({
    selectInput("franchise",
                label = "Franchise",
                choices = collection_datas$name,
                selectize = FALSE,
                multiple = TRUE,
                size = 20)
  })
  output$select_year <- renderUI({
    sliderInput("year",
                label = "Year",
                min = 1971,
                max = 2018,
                value = c(1971, 2018))
  })
  output$base_game <- renderUI({
    checkboxInput("base", label = "Base games only", value = TRUE)
  })
  output$lineplot <- renderPlotly({
    selected_genre <- genre_datas %>% filter(name == input$genre[1])
    gamelist <- unlist(selected_genre$games, use.names = FALSE)
    start_year <- as.Date(paste0(input$year[1], "-01-01"))
    end_year <- as.Date(paste0(input$year[2], "-12-31"))
    game_data <- game_datas_all %>%
      filter(start_year <= first_release_date & end_year >= first_release_date) %>%
      filter(id %in% gamelist)
    if (!input$base[1]) {
      game_data <- game_data %>% filter(is.na(version_parent))
    }
    game_data <- game_data %>% select(name, first_release_date, total_rating) %>%
      arrange(first_release_date)
    plot <- plot_ly(game_data, x = ~first_release_date, y = ~total_rating,
                   type = 'scatter', mode = 'lines+markers', text = ~name)
  })
  
})
