# explore predictions

# load libraries ----
library(tidyverse)
library(here)
library(DT)
library(baseballr)
library(kableExtra)

# load predictions ----
predictions <- read_csv(here("predictions/mlb_2025.csv")) |> 
  mutate(pitch_grade = (pitch_grade/mean(pitch_grade, na.rm = TRUE)*100))

predictions |> 
  slice_max(game_date, n = 1) |> 
  select(game_date)

predictions |> 
  group_by(pitch_type) |> 
  summarize(mean_grade = median(pitch_grade, na.rm = TRUE))

# summary table
predictions |> 
  group_by(pitcher_name, game_date) |> 
  summarize(team = pitcher_team[which.max(game_date)],
            throws = p_throws[which.max(game_date)],
            mean_pitch_grade = mean(pitch_grade, na.rm = TRUE),
            avg_velo = mean(release_speed, na.rm = TRUE),
            avg_spin = mean(release_spin_rate, na.rm = TRUE),
            n = n()) |> 
  filter(n >= 80) |> 
  arrange(desc(mean_pitch_grade))

# raw data table
predictions |> 
  select(pitcher_name, pitcher_team, pitch_type,
         game_date, batter, balls, strikes, pitch_grade) |> 
  arrange(desc(pitch_grade)) |> 
  filter(strikes == "0",
         pitch_grade >= 199.5)

# qualified pitchers cumulative exeuction+
qual_pitchers <- fg_pitcher_leaders(pos = "all", qual = "0", startseason = "2024", endseason = "2024") |> 
  select(xMLBAMID, GS, IP, Throws) |> 
  filter(GS == 0, IP >= 50, Throws == "L")

predictions |> 
  group_by(pitcher_name, pitcher) |> 
  summarize(mean_pitch_grade = mean(pitch_grade, na.rm = TRUE),
            avg_velo = mean(release_speed, na.rm = TRUE),
            avg_spin = mean(release_spin_rate, na.rm = TRUE),
            n = n()) |> 
  left_join(qual_pitchers, by = join_by(pitcher == xMLBAMID)) |> 
  filter(!is.na(IP)) |> 
  arrange(desc(mean_pitch_grade))
