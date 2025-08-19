# get free agency data

# load libraries ----
library(tidyverse)
library(baseballr)
library(rvest)
library(jsonlite)

# get data ----
years <- c(2004:2025)

transactions <- map_df(years, function(x) {mlb_people_free_agents(x)})

players <- chadwick_player_lu()

players |> 
  filter(mlb_played_last == 2025) |> 
  select(name_last, name_first, key_mlbam) |> 
  mutate(name = str_c(name_first, name_last, sep = " ")) |> 
  select(name, id = key_mlbam)


