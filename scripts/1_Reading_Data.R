## Final Project
## Reading in and compiling dataset

# load libraries ----
library(tidyverse)

mlb_schedule(season = 2025) |> 
  filter(game_type == "R") |> 
  slice_min(date) |> 
  select(date)

# set dates ----
days <- c(as.character(seq(as.Date("2025-03-18"), as.Date("2025-09-28"), by = "days")))

# load data ----
pitchers <- list()

for (i in days) {
  pitcher_data <- statcast_search_pitchers(start_date = i, end_date = i) |> 
    filter(game_type == "R")
  
  if (nrow(pitcher_data) != 0) {
    pitchers[[as.character(i)]] <- pitcher_data
  }
}

savant_data <- bind_rows(pitchers)

dir.create("scripts/data")

write_csv(savant_data, "scripts/data/savant_data_25.csv")
