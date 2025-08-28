# get free agency data

# load libraries ----
library(tidyverse)
library(baseballr)
library(rvest)
library(httr)
library(jsonlite)

# get data ----
years <- c(2004:2025)

transactions <- map_df(years, function(x) {mlb_people_free_agents(x)})

# pull spotrac data ----
url <- "https://www.spotrac.com/mlb/new-york-yankees/yearly/_/sort/cap_total2/view/roster"

response <- GET(url)

html <- read_html(content(response, "text", encoding = "UTF-8"))

tables <- html_table(html, fill = TRUE)

# explore ----
predictions |> 
  group_by(pitcher_name, pitch_type) |> 
  summarize(pitch_grade = mean(pitch_grade, na.rm = TRUE),
            n = n()) |> 
  filter(n >= 500, pitch_type == "FF") |> 
  arrange(desc(pitch_grade))
