# get free agency data

# load libraries ----
library(tidyverse)
library(baseballr)
library(rvest)
library(jsonlite)

# get data ----
years <- c(2004:2025)

transactions <- map_df(years, function(x) {mlb_people_free_agents(x)})

predictions |> 
  group_by(pitcher_name, pitch_type) |> 
  summarize(pitch_grade = mean(pitch_grade, na.rm = TRUE),
            n = n()) |> 
  filter(n >= 500, pitch_type == "FF") |> 
  arrange(desc(pitch_grade))
