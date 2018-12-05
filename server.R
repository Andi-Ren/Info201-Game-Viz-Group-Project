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
library(ggplot2)
generate_unique_release_year <- function() {
  filtered_data <- game_datas_all %>% select(id, name, first_release_date, genres) 
  unique_year <- substr(filtered_data$first_release_date, 1, 4)
  unique_year <- unique(unique_year)
  unique_year <- unique_year[!is.na(unique_year)]
}

generate_genre_occurrence <- function(genre_id) {
  length(which(grepl(genre_id, game_datas_all$genres)))
}

generate_pie_chart <- function(genre_name, genre_id, year) {
    result <- plot_ly(labels = genre_name, values = genre_id, 
                      textposition = 'inside',
                      textinfo = 'percent'
                    ) %>%
    add_pie(hole = 0.6) %>%
    layout(title = paste("Genre distribution of video games in the year of", year),  
           font = list(
             color = '#fff'),
           showlegend = F,
           paper_bgcolor='transparent',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}

generate_gauge_chart <- function(value, max, name, tabs, Measure) {
  section <- max / 5
  rad <- (1 - (value / max)) * pi
  
  base_plot <- plot_ly(
    type = "pie",
    values = c(40, 10, 10, 10, 10, 10, 10),
    labels = c(" ", "0", as.character(section), as.character(round(section * 2)), as.character(round(section * 3)), as.character(round(section * 4)), as.character(round(section * 5))),
    rotation = 108,
    direction = "clockwise",
    hole = 0.6,
    textinfo = "label",
    textposition = "outside",
    hoverinfo = "none",
    domain = list(x = c(0, 1), y = c(0, 1)),
    marker = list(colors = c('transparent', 'transparent', 'transparent', 'transparent', 'transparent', 'transparent','transparent')),
    showlegend = FALSE,
    width = 1400, 
    height = 700
  )
  
  base_plot <- add_trace(
    base_plot,
    type = "pie",
    values = c(50, 10, 10, 10, 10, 10),
    labels = tabs,
    rotation = 90,
    direction = "clockwise",
    hole = 0.5,
    textinfo = "label",
    textposition = "inside",
    hoverinfo = "name",
    domain = list(x = c(0, 1), y = c(0, 1)),
    marker = list(colors = c('transparent', 'rgb(232,226,202)', 'rgb(244,220,66)', 'rgb(244,166,66)', 'rgb(244,100,66)', 'rgb(244,66,66)')),
    showlegend= FALSE
  )
  
  a <- list(
    showticklabels = FALSE,
    autotick = FALSE,
    showgrid = FALSE,
    zeroline = FALSE)
   
  
  base_chart <- layout(
    base_plot,
    font = list(
      color = '#fff'),
    shapes = list(
      list(
        type = 'path',
        path = paste('M 0.475 0.5 L', as.character(0.15 * cos(rad) + 0.5), as.character(0.3 * sin(rad) + 0.5), 'L 0.525 0.5 Z'),
        xref = 'paper',
        yref = 'paper',
        fillcolor = 'yellow'
      )
    ),
    xaxis = a,
    yaxis = a,
    paper_bgcolor='transparent',
    annotations = list(xref = 'paper', 
                       yref = 'paper', 
                       x = 0.5, 
                       y = 0.4, 
                       showarrow = F, 
                       text = paste("The", Measure, "for", name, "is", value)))
}

load("data/5000games.Rda")
load("data/companylist.Rdat")
load("data/genrelist.Rdat")
load("data/themedata.Rdat")
load("data/collectiondata.Rdat")
game_datas_all <- game_datas_all %>% 
  mutate(first_release_date = as.Date(game_datas_all$first_release_date))
game_datas <- game_datas_all %>% select(id, name, collection,
                                        popularity, total_rating, total_rating_count,
                                        developers, themes, genres, first_release_date,
                                        hypes, cover.url, game)
collection_datas <- collection_datas %>% arrange(name)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  reactive_data <- reactive({
    game_data <- game_datas_all
    if (!is.null(input$franchise) & !is.null(input$genre) & !is.null(input$theme)) {
      if (input$theme[1] != "all") {
        selected_theme <- theme_datas %>% filter(name == input$theme[1])
        gamelist_theme <- unlist(selected_theme$games, use.names = FALSE)
        game_data <- game_data %>% filter(id %in% gamelist_theme)
      }
      if (input$franchise[1] != "all") {
        selected_franchise <- collection_datas %>% filter(name == input$franchise[1])
        gamelist_franchise <- unlist(selected_franchise$games, use.names = FALSE)
        game_data <- game_data %>% filter(id %in% gamelist_franchise)
      }
      if (input$genre[1] != "all") {
        selected_genre <- genre_datas %>% filter(name == input$genre[1])
        gamelist_genre <- unlist(selected_genre$games, use.names = FALSE)
        game_data <- game_data %>% filter(id %in% gamelist_genre)
      }
    }
    game_data
  })
  
  output$select_genre <- renderUI({
    selectInput("genre",
                label = "Genre",
                choices = c("Select all" = "All", genre_datas$name),
                selected = "All"
                )
  })
  
  output$select_theme <- renderUI({
    selectInput("theme",
                label = "Theme",
                choices = c("Select all" = "All", theme_datas$name),
                selected = "All"
                )
  })
  
  output$select_franchise <- renderUI({
    selectInput("franchise",
                label = "Franchise",
                choices = c("Select all" = "All", collection_datas$name),
                selectize = FALSE,
                size = 20,
                selected = "All"
                )
    start_year <- as.Date(paste0(input$year[1], "-01-01"))
    end_year <- as.Date(paste0(input$year[2], "-12-31"))
    game_data <- game_data %>%
      filter(start_year <= first_release_date & end_year >= first_release_date)
    if (input$base[1]) {
      game_data <- game_data %>% filter(is.na(game))
    }
    game_data <- game_data %>% select(name, first_release_date, total_rating) %>%
      arrange(first_release_date)
  })
  output$select_element <- renderUI({
    output = tagList()
    output[[1]] = selectInput("genre",
                    label = h4("Genre"),
                    choices = c("Select all" = "all", genre_datas$name),
                    selected = "all"
                  )
    output[[2]] = selectInput("theme",
                    label = h4("Theme"),
                    choices = c("Select all" = "all", theme_datas$name),
                    selected = "all"
                  )
    output[[3]] = selectInput("franchise",
                    label = h4("Franchise"),
                    choices = c("Select all" = "all", collection_datas$name),
                    selectize = FALSE,
                    size = 20,
                    selected = "all"
                  )
    output
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
    if (nrow(game_data) >= 0) {
      x_style <- list(title = "Launch Date")
      y_style <- list(title = "Rating", range = c(10, 100))
      plot <- plot_ly(game_data, x = ~first_release_date, y = ~total_rating,
                      type = 'scatter', mode = 'lines+markers', text = ~name) %>%
              layout(xaxis = x_style, yaxis = y_style)
    }
  })
  output$lineplot_text <- renderText({
    game_data <- reactive_data()
    if (nrow(game_data) > 0) {
      sum_text <- paste0("<p>According to our database, from ", input$year[1], "-01 to ",
                         input$year[2], "-12, there are <strong>",
                         nrow(game_data), "</strong> games in ", input$genre[1], " genre, ",
                         input$theme, " theme, ", " and ", input$franchise, " franchise. ")
      if (mean(game_data$total_rating) >= 90) {
        score_text <- "\"<strong>Universal Acclaim</strong>\""
      } else if (mean(game_data$total_rating) >= 75) {
        score_text <- "\"<strong>Generally Favorable</strong>\""
      } else if (mean(game_data$total_rating) >= 50) {
        score_text <- "\"<strong>Mixed or Average</strong>\""
      } else if (mean(game_data$total_rating) >= 20) {
        score_text <- "\"<strong>Generally Unfavorablev\""
      } else {
        score_text <- "\"<strong>Universal Dislike\""
      }
      if (nrow(game_data) <= 1) {
        sum_text <- paste0(sum_text, "</p><p>",
                           "According to the criteria of Metacritic, the score of this game would qualify as ",
                           score_text, ". </p>")
      } else if (nrow(game_data) <= 2) {
        sum_text <- paste0(sum_text, "</p><p>", " The average score of these games are ",
                           round(mean(game_data$total_rating), 2), ". </p><p>",
                           "According to the criteria of Metacritic, the average score of this group would qualify as ",
                           score_text, ". </p>")
      } else {
        sum_text <- paste0(sum_text, "</p><p>The highest rated game here is <strong>",
                           game_data[which.max(game_data$total_rating), ]$name,
                           "</strong> with a rating of <strong>", round(max(game_data$total_rating), 2),
                           "</strong>, and the lowest rated game is <strong>", 
                           game_data[which.min(game_data$total_rating), ]$name,
                           "</strong> with a rating of <strong>", round(min(game_data$total_rating), 2), "</strong>. </p><p>",
                           " The <strong>average rating</strong> of these games are <strong>",
                           round(mean(game_data$total_rating), 2),
                           "</strong> with a standard deviation of <strong>",
                           round(sd(game_data$total_rating), 2), "</strong>. </p><p>",
                           "According to the criteria of <a href=\"https://www.metacritic.com/about-metascores\">Metacritic</a>,",
                           "the average score of this group would qualify as ",
                           score_text, ". </p>")
      }
    } else {
      sum_text <- "There is 0 game in your selected genre, theme, and franchise."
    }
    sum_text
  })
  output$recommandation <- renderText({
    
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
  output$select_measure <- renderUI({
    game <- game_datas_all
    game <- arrange(game, -popularity)
    selectInput("Measurement",
                label = "Measurement",
                selectize = FALSE,
                size = 1,
                choices = c("Public Attention", "Overall Ratings"))
  })
  output$gauge_plot <- renderPlotly({ 
    game <- arrange(filter(game_datas_all, name == input$Games[1]), -popularity)[1,]
    name <- game$name
    measure <- input$Measurement[1]
    if(measure == "Public Attention") {
      value <- game$popularity
      max <- max(game_datas_all$popularity)
      tabs <- c(' ', 1 ,2 ,3 ,4 ,5)
    } else if (measure == "Ratings") {
      value <- game$total_rating
      max <- max(game_datas_all$total_rating)
      tabs <- c(' ', 2, 3, 4, 5, 6)
    } else if (measure == "Player Ratings") {
      value <- game$total_rating
      max <- max(game_datas_all$total_rating)
      tabs <- c(' ', 2, 3, 4, 5, 6)
    }
    generate_gauge_chart(value, max, name, tabs, measure)
    #generate_gauge_chart(value2, max2)
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
  
  output$genre_pie_chart <- renderPlotly({
    year_data <- game_datas_all %>% select(id, name, first_release_date, genres) %>% 
      filter(substr(first_release_date, 1, 4) == input$year_selection)
    genres_vector <- unlist(year_data$genres)
    filtered_genres <- genre_datas %>% filter(name %in% input$genre_types) #might show problem
    genre_vector <- filtered_genres$id
    genre_occurrences <- sapply(genre_vector, generate_genre_occurrence)
    generate_pie_chart(input$genre_types, genre_occurrences, input$year_selection)
  })
})
