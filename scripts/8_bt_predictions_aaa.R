# boosted tree model predictions

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(stacks)

# load model and data ----
load("results/bt_fit_final.rda")
compiled_data <- read_csv("data/aaa_24/aaa_24.csv")
pitcher_ids <- read_csv(here("data/aaa_24/pitcher_id.csv")) |> 
  mutate(pitcher_id = player_id,
         pitcher_name = player_name) |> 
  select(pitcher_id, pitcher_name)

# clean new data ----
compiled_data <- compiled_data |> 
  mutate(across(where(is.character), as.factor),
         balls = factor(balls),
         strikes = factor(strikes),
         delta_run_exp = -delta_run_exp)

mean_run_exp <- compiled_data |> 
  group_by(pitch_type) |> 
  summarize(mean_run_exp = mean(delta_run_exp, na.rm = TRUE),
            n = n()) |> 
  arrange(mean_run_exp)

compiled_data <- compiled_data |> 
  left_join(mean_run_exp, by = join_by(pitch_type == pitch_type)) |> 
  mutate(run_exp_above_avg = delta_run_exp - mean_run_exp)

# generate predictions ----
predictions <- compiled_data |> 
  bind_cols(predict(bt_fit_final, compiled_data)) |>
  mutate(predicted_reaa = .pred) |> 
  select(!.pred)

predictions <- predictions |> 
  left_join(pitcher_ids, by = join_by(pitcher...8 == pitcher_id)) |>
  mutate(pitcher_team = if_else(inning_topbot == "Bot", away_team, home_team),
         opponent = if_else(inning_topbot == "Top", away_team, home_team)) |> 
  select(id = pitcher...8, pitcher_name, pitcher_team, pitch_type, 
         release_speed, release_spin_rate, description, bb_type, events, 
         batter_name = player_name, opponent, game_date, 
         batter, stand, p_throws, predicted_reaa, balls, strikes, 
         inning_topbot, inning)

predictions <- predictions |> 
  mutate(pitch_grade = percent_rank(predicted_reaa)*100) |> 
  mutate(pitcher_name = if_else(is.na(pitcher_name), batter_name, pitcher_name)) |> 
  select(!batter_name)

predictions |> 
  skimr::skim()

# save predictions ----
dir.create("predictions")

write_csv(predictions, file = "predictions/aaa_2024.csv")
