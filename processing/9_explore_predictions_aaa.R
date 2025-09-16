# explore predictions

# load libraries ----
library(tidyverse)
library(here)
library(DT)

# load predictions ----
predictions <- read_csv(here("predictions/aaa_2024.csv")) |> 
  mutate(pitch_grade = (pitch_grade/mean(pitch_grade, na.rm = TRUE)*100))

predictions |> 
  slice_max(game_date, n = 1) |> 
  select(game_date)

# summary table
predictions |> 
  group_by(pitcher_name, pitch_type) |> 
  summarize(mean_pitch_grade = mean(pitch_grade, na.rm = TRUE),
            avg_velo = mean(release_speed, na.rm = TRUE),
            avg_spin = mean(release_spin_rate, na.rm = TRUE),
            n = n()) |> 
  arrange(desc(mean_pitch_grade)) |> 
  filter(n >= 500)

# raw data table
predictions |> 
  select(pitcher_name, pitcher_team, pitch_type,
         game_date, batter, balls, strikes, pitch_grade) |> 
  arrange(desc(pitch_grade)) |> 
  filter(strikes == "0",
         pitch_grade >= 199.5)
