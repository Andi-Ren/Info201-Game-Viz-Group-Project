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
load("data/franchisedata.Rdat")
game_datas_all <- game_datas_all %>% 
  mutate(first_release_date = as.Date(game_datas_all$first_release_date))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$selectfran <- renderUI({
    selectInput("genre",
                label = "Genre",
                choices = genre_datas$name)
  })
  output$lineplot <- renderPlotly({
    selected_genre <- genre_datas %>% filter(name == input$genre[1])
    gamelist <- unlist(selected_genre$games, use.names = FALSE)
    game_data <- game_datas_all %>% filter(id %in% gamelist) %>%
      select(name, first_release_date, total_rating) %>% arrange(first_release_date)
    plot <- plot_ly(game_data, x = ~first_release_date, y = ~total_rating,
                   type = 'scatter', mode = 'lines+markers', text = ~name)
  })
  
  output$select_game <- renderUI({
    selectInput("genre",
                label = "Genre",
                choices = genre_datas$name)
  })
  
  output$gauge_plot <- renderPlotly({ 
    max <- max(game_datas_all$popularity)
    game <- filter(game_datas_all, name == input$value)
    pop <- game$popularity
    section <- max / 5
    rad <- (1 - (pop / max)) * pi
    
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
      showlegend = FALSE
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
      hoverinfo = "game name",
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
          path = paste('M 0.5 0.5 L', as.character(0.3 * cos(rad) + 0.5), as.character(0.3 * sin(rad) + 0.5), 'L 0.5 0.5 Z'),
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
  
})
