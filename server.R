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
load("data/5000games.Rda")
load("data/genrelist.Rdat")
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
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
