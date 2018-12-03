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
generate_unique_release_year <- function() {
  filtered_data <- game_datas_all %>% select(id, name, first_release_date, genres) 
  unique_year <- substr(filtered_data$first_release_date, 1, 4)
  unique_year <- unique(unique_year)
  unique_year <- unique_year[!is.na(unique_year)]
}

generate_genre_occurrence <- function() {
  length(which(grepel(12, game_datas_all$genres)))
}

#generate_pie_chart <- function(genre_data, title) {
#  
#}

#generate_filtered_genre <- function() {
#  result <- game_datas_all %>% select(id, name, first_release_date, genres) %>% 
#    filter(substr(first_release_date, 1, 4) == input$year_selection)
#}

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
  reactive_data <- reactive({
    game_data <- game_datas_all
    if (!is.null(input$franchise) & !is.null(input$genre) & !is.null(input$theme)) {
      if (input$theme[1] != "All") {
        selected_theme <- theme_datas %>% filter(name == input$theme[1])
        gamelist_theme <- unlist(selected_theme$games, use.names = FALSE)
        game_data <- game_data %>% filter(id %in% gamelist_theme)
      }
      if (input$franchise[1] != "All") {
        selected_franchise <- collection_datas %>% filter(name == input$franchise[1])
        gamelist_franchise <- unlist(selected_franchise$games, use.names = FALSE)
        game_data <- game_data %>% filter(id %in% gamelist_franchise)
      }
      if (input$genre[1] != "All") {
        selected_genre <- genre_datas %>% filter(name == input$genre[1])
        gamelist_genre <- unlist(selected_genre$games, use.names = FALSE)
        game_data <- game_data %>% filter(id %in% gamelist_genre)
      }
    }
    game_data
  })
  output$select_genre <- renderUI({
    #game_data <- reactive_data()
    #genre_list <- unlist(game_data$genre)
    #genre_list <- unique(genre_list)
    #genre_data <- genre_datas %>% filter(id %in% genre_list)
    selectInput("genre",
                label = "Genre",
                choices = c("Select all" = "All", genre_datas$name),
                selected = "All"
                )
  })
  output$select_theme <- renderUI({
    #game_data <- reactive_data()
    #theme_list <- unlist(game_data$theme)
    #theme_list <- unique(theme_list)
    #theme_data <- theme_datas %>% filter(id %in% theme_list)
    selectInput("theme",
                label = "Theme",
                choices = c("Select all" = "All", theme_datas$name),
                selected = "All"
                )
  })
  output$select_franchise <- renderUI({
    #game_data <- reactive_data()
    #franchise_list <- unlist(game_data$franchise)
    #franchise_list <- unique(franchise_list)
    #collection_data <- collection_datas %>% filter(id %in% franchise_list)
    selectInput("franchise",
                label = "Franchise",
                choices = c("Select all" = "All", collection_datas$name),
                selectize = FALSE,
                size = 20,
                selected = "All"
                )
  })
  output$select_year_wayne <- renderUI({
    sliderInput("year",
                label = "Year",
                min = 1971,
                max = 2018,
                value = c(1971, 2018))
  })
  output$base_game <- renderUI({
    checkboxInput("base", label = "Base games only", value = FALSE)
  })
  output$lineplot <- renderPlotly({
    game_data <- reactive_data()
    start_year <- as.Date(paste0(input$year[1], "-01-01"))
    end_year <- as.Date(paste0(input$year[2], "-12-31"))
    game_data <- game_data %>%
      filter(start_year <= first_release_date & end_year >= first_release_date)
    if (input$base[1]) {
      game_data <- game_data %>% filter(is.na(game))
    }
    game_data <- game_data %>% select(name, first_release_date, total_rating) %>%
      arrange(first_release_date)
    if (nrow(game_data) >= 0) {
      x_style <- list(title = "Launch Date")
      y_style <- list(title = "Rating", range = c(10, 100))
      plot <- plot_ly(game_data, x = ~first_release_date, y = ~total_rating,
                      type = 'scatter', mode = 'lines+markers', text = ~name) %>%
              layout(xaxis = x_style, yaxis = y_style)
    }
  })
  output$select_year <- renderUI({
    return(selectInput("year_selection", "Select Release Year", choices=generate_unique_release_year()))
  })
  output$filter_genre <- renderUI({
    selected_year_data <- game_datas_all %>% select(id, name, first_release_date, genres) %>% 
          filter(substr(first_release_date, 1, 4) == input$year_selection)
    unique_genre_number <- unique(unlist(selected_year_data$genres))
    id <- unique_genre_number
    unique_genre_dataframe <- data.frame(id, stringsAsFactors = FALSE)
    small_genre_data <- select(genre_datas, id, name)
    joined_dataframe <- inner_join(small_genre_data, unique_genre_dataframe, by="id")
    return(checkboxGroupInput("genre_types", "Game Type(s)", choices=joined_dataframe$name, selected=joined_dataframe$name))
  })
  output$genre_pie_chart <- renderText({
    year_data <- game_datas_all %>% select(id, name, first_release_date, genres) %>% 
      filter(substr(first_release_date, 1, 4) == input$year_selection)
    genres_vector <- unlist(year_data$genres)
    filtered_genres <- genre_datas %>% filter(name %in% input$genre_types) #might show problem
    genre_vector <- filtered_genres$id
   # genres_occurrence <- length(which(grepel(12, game_datas_all$genres)))
    filtered_genres <- input$genre_types
    selected_genres_id <- c()
    for (i in 1:length(filtered_genres)) {
      genre_id <- genre_datas %>% filter(name == filtered_genres[i])
      genre_id <- genre_id$id
      append(selected_genres_id, genre_id)
    }
    selected_genres_id
  })
  
})
