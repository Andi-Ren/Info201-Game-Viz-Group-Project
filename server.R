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
    selectInput("fran",
                label = "Franchise",
                choices = franchises_datas$name)
  })
  output$lineplot <- renderPlot({
    selected_fran <- franchises_datas %>% filter(name == "The Witcher")
    gamelist <- unlist(selected_fran$games, use.names = FALSE)
    game_datas_all <- game_datas_all %>% filter(id %in% gamelist) %>%
      select(name, first_release_date, total_rating) %>% arrange(first_release_date)
    plot<- plot_ly(game_datas_all, x = ~first_release_date, y = ~total_rating,
                   type = 'scatter', mode = 'lines', text = ~name)
  })
  
})
