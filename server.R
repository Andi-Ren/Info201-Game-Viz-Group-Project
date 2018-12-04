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
library(flexdashboard)
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
  output$select_genre <- renderUI({
    selectInput("genre",
                label = "Genre",
                choices = c("All", genre_datas$name))
  })
  output$select_theme <- renderUI({
    selectInput("theme",
                label = "Theme",
                choices = c("All", theme_datas$name))
  })
  output$select_franchise <- renderUI({
    selectInput("franchise",
                label = "Franchise",
                choices = c("All", collection_datas$name),
                selectize = FALSE,
                size = 20)
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
    game_data <- game_datas_all
    if (input$genre[1] != "All") {
      selected_genre <- genre_datas %>% filter(name == input$genre[1])
      gamelist <- unlist(selected_genre$games, use.names = FALSE)
      game_data <- game_data %>% filter(id %in% gamelist)
    }
    if (input$theme[1] != "All") {
      selected_theme <- theme_datas %>% filter(name == input$theme[1])
      gamelist <- unlist(selected_theme$games, use.names = FALSE)
      game_data <- game_data %>% filter(id %in% gamelist)
    }
    if (input$franchise[1] != "All") {
      selected_franchise <- collection_datas %>% filter(name == input$franchise[1])
      gamelist <- unlist(selected_franchise$games, use.names = FALSE)
      game_data <- game_data %>% filter(id %in% gamelist)
    }
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
      plot <- plot_ly(game_data, x = ~first_release_date, y = ~total_rating,
                      type = 'scatter', mode = 'lines+markers', text = ~name)
    }
  })
  
  output$select_game <- renderUI({
    game <- game_datas_all
    game <- arrange(game, -popularity)
    selectInput("Games",
                label = "Games",
                selectize = FALSE,
                size = 20,
                choices = game$name)
  })
  
  output$gauge_plot <- renderPlotly({ 
    max <- max(game_datas_all$popularity)
    game <- filter(game_datas_all, name == input$Games[1])
    pop <- game$popularity
    section <- max / 5
    rad <- (1 - (pop / max)) * pi
    
    m <- list(
      l = 50,
      r = 50,
      b = 100,
      t = 100,
      pad = 4
    )
    
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, 10, 10, 10, 10, 10, 10),
      labels = c(" ", "0", as.character(section), as.character(section * 2), as.character(section * 3), as.character(section * 4), as.character(section * 5)),
      rotation = 108,
      direction = "clockwise",
      hole = 0.6,
      textinfo = "label",
      textposition = "outside",
      hoverinfo = "none",
      domain = list(x = c(0, 1), y = c(0, 1)),
      marker = list(colors = c('rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)','rgb(255, 255, 255)')),
      showlegend = FALSE,
      width = 1000, 
      height = 500
    )
    
    base_plot <- add_trace(
      base_plot,
      type = "pie",
      values = c(50, 10, 10, 10, 10, 10),
      labels = c(" ", "Dead", "Fading", "Alive", "Hot", "Red Hot!"),
      rotation = 90,
      direction = "clockwise",
      hole = 0.5,
      textinfo = "label",
      textposition = "inside",
      hoverinfo = "name",
      domain = list(x = c(0, 1), y = c(0, 1)),
      marker = list(colors = c('rgb(255, 255, 255)', 'rgb(232,226,202)', 'rgb(244,220,66)', 'rgb(244,166,66)', 'rgb(244,100,66)', 'rgb(244,66,66)')),
      showlegend= FALSE
    )
    
    a <- list(
      showticklabels = FALSE,
      autotick = FALSE,
      showgrid = FALSE,
      zeroline = FALSE)
    
    b <- list(
      xref = 'paper',
      yref = 'paper',
      x = 0.5,
      y = 0.4,
      showarrow = F,
      text = pop)
    
    base_chart <- layout(
      base_plot,
      shapes = list(
        list(
          type = 'path',
          path = paste('M 0.5 0.5 L', as.character(0.15 * cos(rad) + 0.5), as.character(0.3 * sin(rad) + 0.5), 'L 0.5 0.5 Z'),
          xref = 'paper',
          yref = 'paper',
          fillcolor = 'rgba(44, 160, 101, 0.5)'
        )
      ),
      xaxis = a,
      yaxis = a,
      annotations = b
    ) 
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
    genre_vector
  })
  
  output$gauge = renderGauge({
    gauge(input$value, 
          min = 0, 
          max = 1, 
          sectors = gaugeSectors(success = c(0.5, 1), 
                                 warning = c(0.3, 0.5),
                                 danger = c(0, 0.3)))
  })
  
})
