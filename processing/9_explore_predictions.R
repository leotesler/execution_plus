# explore predictions

# load libraries ----
library(tidyverse)
library(here)
library(DT)
library(baseballr)
library(kableExtra)
library(httr)
library(rvest)
library(jsonlite)
library(googlesheets4)

# load predictions ----
predictions <- read_rds(here("predictions/mlb_2026.rds")) |>
  filter(pitch_type != "") |> 
  mutate(pitch_grade = (pitch_grade/mean(pitch_grade, na.rm = TRUE)*100))

predictions |> 
  slice_max(game_date, n = 1) |> 
  select(game_date)

predictions |> 
  group_by(pitch_type) |> 
  summarize(mean_grade = median(pitch_grade, na.rm = TRUE))

predictions |> 
  filter(pitcher_name == "Winquest, Cade") |> 
  print(width = Inf)

# summary table
predictions |> 
  group_by(pitcher_name, pitch_type) |> 
  summarize(team = pitcher_team[which.max(game_date)],
            throws = p_throws[which.max(game_date)],
            mean_pitch_grade = mean(pitch_grade, na.rm = TRUE),
            avg_velo = mean(release_speed, na.rm = TRUE),
            avg_spin = mean(release_spin_rate, na.rm = TRUE),
            n = n()) |> 
  filter(n >= 10) |> 
  arrange(desc(mean_pitch_grade))

# raw data table
url <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=pit&lg=all&qual=0&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0&pageitems=300000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response <- GET(url)
page <- content(response, as = "text", encoding = "UTF-8")
data <- fromJSON(page)
fg_data <- as.tibble(data$data) |> 
  janitor::clean_names() |> 
  select(x_mlbamid, player_name, tbf)

player_ep_mlb <- predictions |> 
  group_by(id, pitcher_name) |> 
  summarize(execution_plus = mean(pitch_grade, na.rm = TRUE)) |> 
  select(!pitcher_name) |> 
  left_join(fg_data, by = join_by(id == x_mlbamid))

save(player_ep_mlb, file = "processing/player_ep_mlb_25.rda")

sheet_write(player_ep, 
            ss = "1dIH3BHPOGVjCiww6ZSpyLh1TxTNSL7h7pYnd1CYJCiA",
            sheet = "2025 Execution Plus MLB")
