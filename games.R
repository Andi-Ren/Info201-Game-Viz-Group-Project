library(httr)
library(jsonlite)
library(dplyr)

source("~/Documents/info201_R/igdb_sample/igdb_key.R")
# order by total ratings count in descending order
game_url <- paste0("https://api-endpoint.igdb.com",
                          "/games/?order=total_rating_count:desc&fields=*&limit=50&scroll=1")
game_res <- GET(game_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
game_datas_all <- flatten(fromJSON(rawToChar(content(game_res, "raw"))))

# continuation of the last query
for(i in 1:99) {
  game_nextpage_url <- paste0("https://api-endpoint.igdb.com",
                              game_res[["headers"]][["x-next-page"]])
  game_nextpage_content <- GET(game_nextpage_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
  game_nextpage <- flatten(fromJSON(rawToChar(content(game_nextpage_content, "raw"))))
  
  # Join the datas
  
  game_datas_all <- bind_rows(game_datas_all, game_nextpage)  
}

# You can repeatedly call game_nextpage and join datas for a larger dataframe
# You can also order by popularity, rating, etc.
# game_res_all[["headers"]][["x-count"]] gives you total number of games that imdb has, so we can call a for loop depends on it
# But ideally we should call query less, since I think that there is a monthly limit of 3000 queries.

# To do: make shinyapp server.R and ui.R

load("data/1000games.Rda")
load("data/5000games.Rda")

# Covert the first
library(anytime)
game_datas_all <- mutate(game_datas_all, first_release_date = anydate(first_release_date / 1000))

company_url <- paste0("https://api-endpoint.igdb.com",
                      "/companies/?fields=*&limit=50&scroll=1")
company_res <- GET(company_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
company_datas <- flatten(fromJSON(rawToChar(content(company_res, "raw"))))

genre_url <- paste0("https://api-endpoint.igdb.com",
                      "/genres/?fields=*&limit=50&scroll=1")
genre_res <- GET(genre_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
genre_datas <- flatten(fromJSON(rawToChar(content(genre_res, "raw"))))

franchises_url <- paste0("https://api-endpoint.igdb.com",
                    "/franchises/?fields=*&limit=50&scroll=1")
franchises_res <- GET(franchises_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
franchises_datas <- flatten(fromJSON(rawToChar(content(franchises_res, "raw"))))

unique_dev <- unique(game_datas_all$developers)
unique_dev <- unlist(unique_dev)
unique_dev <- unique(unique_dev)

unique_dev_string <- paste0(as.character(unique_dev[1:50]), collapse=",")
unique_dev <- unique_dev[-(1:50)]

company_url <- paste0("https://api-endpoint.igdb.com",
                      "/companies/", unique_dev_string,
                      "?fields=*")
company_res <- GET(company_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
company_datas <- flatten(fromJSON(rawToChar(content(company_res, "raw"))))

unique_dev_string <- paste0(as.character(unique_dev[1:1628]), collapse=",")
unique_dev <- unique_dev[-(1:200)]

company_url <- paste0("https://api-endpoint.igdb.com",
                      "/companies/", unique_dev_string,
                      "?fields=*")
company_res <- GET(company_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
company_datas2 <- flatten(fromJSON(rawToChar(content(company_res, "raw"))))
company_datas <- bind_rows(company_datas, company_datas2)

company_datas <- mutate(company_datas, start_date = anydate(start_date / 1000))

save(company_datas, file = "data/companylist.Rdat")
save(genre_datas, file = "data/genrelist.Rdat")

load("data/companylist.Rdat")
load("data/genrelist.Rdat")

