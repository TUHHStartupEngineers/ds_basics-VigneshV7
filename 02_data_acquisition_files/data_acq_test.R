library(RSQLite)
library(dplyr)

con <- RSQLite::dbConnect(drv = SQLite(),
                          dbname = "00_data/02_chinook/Chinook_Sqlite.sqlite")

dbListTables(con)
tbl(con, "Album")

dbDisconnect(con)
con

# Star Wars API
library(httr)
library(glue)
resp <- GET("https://swapi.dev/api/people/1/")

# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws error if request did not succeed
}

resp <- sw_api("/people/1")
resp