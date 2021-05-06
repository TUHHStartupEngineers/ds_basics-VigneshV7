# Challenge 1 ----

## 1.0 LIBRARIES 

library(tidyverse) 
library(rvest)     
library(xopen)     
library(jsonlite)  
library(glue)      
library(stringi)   
library(httr)
library(knitr)

clientID <- "xxxxxxxxxx"
secret   <- "xxxxxxxxxx"

response <- POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

mytoken <- content(response)$access_token

HeaderValue <- paste0("Bearer ", mytoken)

get_album_tracks <- function(artistID = "4vC2GtOXDzAfthZ4gnFowC") {
  URI <- paste0('https://api.spotify.com/v1/artists/', artistID, '/albums')
  response2 <- GET(url = URI, config = add_headers(Authorization = HeaderValue))
  
  # convert to tibble
  albums <- content(response2)
  
  albums_items <- albums$items
  
  albums_items_data <- as.data.frame(do.call(cbind, albums_items))
  
  albums_items_tbl <- as_tibble(nms = names(albums_items_data), t(albums_items_data)) %>% 
    select(-c(1:8, 11)) %>% 
    set_names(c("Album Name", "Release Date", "Number of Tracks", "Type", "URI"))
}

albums_items_tbl <- get_album_tracks(artistID = "4vC2GtOXDzAfthZ4gnFowC")

kable(albums_items_tbl %>% head(n = 10), "rst",
      align = "ccccc", 
      caption = "Album information of the Desired Artist")

# Challenge 2 ----

scrape_bike_data <- function(url) {
  
  html_gravel_bikes <- read_html(url)
  
  bike_name <- html_gravel_bikes %>% 
    html_nodes(css = ".catalog-category-model__title") %>% 
    html_text() %>% 
    str_replace_all("[\r\n]", "") %>% 
    enframe(name = "Sl No.", value = "Name")
  
  bike_price <- html_gravel_bikes %>% 
    html_nodes(css = ".catalog-category-model__price-current-value") %>% 
    html_text() %>% 
    str_replace_all("[\r\n]", "") %>% 
    enframe(name = "Sl No.", value = "Price")
  
  bike_info_combined <- left_join(bike_name, bike_price) %>% 
    select(-("Sl No."))
}

url_home <- "https://www.rosebikes.de/fahrr%C3%A4der/gravel/backroad-al"
bike_info_output <- scrape_bike_data(url_home)

kable(bike_info_output, "rst", align = "cc", 
      caption = "Prices of Backroad Al models of Gravel bikes from Rosebikes")