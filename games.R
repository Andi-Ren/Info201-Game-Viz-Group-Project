library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

source("keys.R")

# order by total ratings count in descending order
game_url_game <- paste0("https://api-endpoint.igdb.com",
                          "/games/?order=total_rating_count:desc&fields=*&limit=50&scroll=1")
game_res_all <- GET(game_url_game, add_headers("user-key" = game_key, "Accept" = "application/json"))
game_datas_all <- flatten(fromJSON(rawToChar(content(game_res_all, "raw"))))

View(game_datas_all)

# continuation of the last query
for(i in 1:99) {
  game_nextpage_url <- paste0("https://api-endpoint.igdb.com",
                              game_res_all[["headers"]][["x-next-page"]])
  game_nextpage_content <- GET(game_nextpage_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
  game_nextpage <- flatten(fromJSON(rawToChar(content(game_nextpage_content, "raw"))))
  
  # Join the datas
  
  game_datas_all <- bind_rows(game_datas_all, game_nextpage)  
}

write.csv(game_datas_all, file = "5000_games_data.csv", row.names = F)
# You can repeatedly call game_nextpage and join datas for a larger dataframe
# You can also order by popularity, rating, etc.
# game_res_all[["headers"]][["x-count"]] gives you total number of games that imdb has, so we can call a for loop depends on it
# But ideally we should call query less, since I think that there is a monthly limit of 3000 queries.

# To do: make shinyapp server.R and ui.R
