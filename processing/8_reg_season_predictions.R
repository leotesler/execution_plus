# regular season predictions

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(stacks)
library(baseballr)
library(rvest)

# load model and data ----
load(here("results/bt_fit_final.rda"))
prior_preds <- readRDS("predictions/mlb_2025.rds")

days <- c(as.character(seq(as.Date("2025-03-18"), as.Date("2025-03-19"), by = "days")),
          as.character(seq(as.Date("2025-03-27"), as.Date(today() - 1), by = "days")))

days <- as.character(seq(max(prior_preds$game_date) + 1, Sys.Date() - 1, by = "day"))

pitchers <- list()

for (i in days) {
  pitcher_data <- statcast_search_pitchers(start_date = i, end_date = i) |> 
    filter(game_type == "R")
  
  if (nrow(pitcher_data) != 0) {
  pitchers[[as.character(i)]] <- pitcher_data
  }
}

compiled_data <- bind_rows(pitchers)

pitcher_ids <- compiled_data |> 
  mutate(pitcher_id = pitcher,
         pitcher_name = player_name) |> 
  select(pitcher_id, pitcher_name) |> 
  distinct(pitcher_id, pitcher_name)

hitters <- list()

for (i in days) {
  hitter_data <- statcast_search_batters(start_date = i, end_date = i) |> 
    filter(game_type == "R")
  
  if (nrow(hitter_data) != 0) {
  hitters[[as.character(i)]] <- hitter_data
  }
}

hitter_ids <- bind_rows(hitters) |> 
  mutate(hitter_id = batter,
         hitter_name = player_name) |> 
  select(hitter_id, hitter_name) |> 
  distinct(hitter_id, hitter_name)

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
  left_join(pitcher_ids, by = join_by(pitcher == pitcher_id)) |>
  mutate(pitcher_team = if_else(inning_topbot == "Bot", away_team, home_team),
         opponent = if_else(inning_topbot == "Top", away_team, home_team),
         id = pitcher,
         batter_name = player_name) |> 
  select(!pitcher) |> 
  select(!player_name) |> 
  left_join(hitter_ids, by = join_by(batter == hitter_id))

predictions <- predictions |> 
  mutate(pitch_grade = percent_rank(predicted_reaa)*100) |> 
  mutate(pitcher_name = if_else(is.na(pitcher_name), batter_name, pitcher_name)) |> 
  select(!batter_name) |> 
  mutate(batter_name = hitter_name) |> 
  select(!hitter_name) |> 
  mutate(balls = as.numeric(balls),
         strikes = as.numeric(strikes))

predictions <- bind_rows(predictions, prior_preds) |> 
  mutate(pitch_grade = (pitch_grade/mean(pitch_grade, na.rm = TRUE)*100),
         pitch_type = factor(pitch_type))

# process data for summaries ----
swing_code <- c("bunt_foul_tip", "foul", "foul_bunt", "foul_tip",
                "hit_into_play", "missed_bunt", "swinging_strike", "swinging_strike_blocked")
whiff_code <- c("swinging_strike", "swinging_strike_blocked", "foul_tip")

predictions <- predictions |> 
  mutate(swing = description %in% swing_code,
         whiff = description %in% whiff_code,
         in_zone = zone < 10,
         out_zone = zone > 10,
         chase = !in_zone & swing,
         pfx_z = pfx_z * 12,
         pfx_x = pfx_x * 12)

df_statcast_grouped <- predictions |>
  filter(!is.na(pitch_type)) |>
  group_by(pitch_type) |>
  summarize(pitch = n(),
            release_speed = mean(release_speed, na.rm = TRUE),
            pfx_z = mean(pfx_z, na.rm = TRUE),
            pfx_x = mean(pfx_x, na.rm = TRUE),
            release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
            release_pos_x = mean(release_pos_x, na.rm = TRUE),
            release_pos_z = mean(release_pos_z, na.rm = TRUE),
            release_extension = mean(release_extension, na.rm = TRUE),
            delta_run_exp = mean(delta_run_exp, na.rm = TRUE),
            swing = sum(swing, na.rm = TRUE),
            whiff = sum(whiff, na.rm = TRUE),
            in_zone = sum(in_zone, na.rm = TRUE),
            out_zone = sum(out_zone, na.rm = TRUE),
            chase = sum(chase, na.rm = TRUE),
            xwoba = mean(estimated_woba_using_speedangle, na.rm = TRUE)) |> 
  mutate(pitch_usage = pitch/sum(pitch),
         whiff_rate = whiff/sum(pitch),
         in_zone_rate = in_zone/sum(pitch),
         chase_rate = chase/sum(pitch),
         delta_run_exp_per_100 = (delta_run_exp*100)/sum(pitch))

summary_row <- predictions |> 
  summarize(pitch = n(),
            release_speed = mean(release_speed, na.rm = TRUE),
            pfx_z = mean(pfx_z, na.rm = TRUE),
            pfx_x = mean(pfx_x, na.rm = TRUE),
            release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
            release_pos_x = mean(release_pos_x, na.rm = TRUE),
            release_pos_z = mean(release_pos_z, na.rm = TRUE),
            release_extension = mean(release_extension, na.rm = TRUE),
            delta_run_exp = mean(delta_run_exp, na.rm = TRUE),
            swing = sum(swing, na.rm = TRUE),
            whiff = sum(whiff, na.rm = TRUE),
            in_zone = sum(in_zone, na.rm = TRUE),
            out_zone = sum(out_zone, na.rm = TRUE),
            chase = sum(chase, na.rm = TRUE),
            xwoba = mean(estimated_woba_using_speedangle, na.rm = TRUE)) |> 
  mutate(pitch_usage = pitch/sum(pitch),
         whiff_rate = whiff/sum(pitch),
         in_zone_rate = in_zone/sum(pitch),
         chase_rate = chase/sum(pitch),
         delta_run_exp_per_100 = (delta_run_exp*100)/sum(pitch)) |> 
  mutate(pitch_type = "All",
         .before = 1)

df_statcast_grouped <- bind_rows(df_statcast_grouped, summary_row)

# save predictions ----
dir.create("predictions")
dir.create("ExecutionPlusApp/predictions")

saveRDS(predictions, file = "predictions/mlb_2025.rds")
saveRDS(df_statcast_grouped, "ExecutionPlusApp/predictions/df_statcast_grouped.rds")

predictions |> 
  group_by(id) |> 
  group_walk(~ {
    saveRDS(.x, paste0("ExecutionPlusApp/predictions/", .y$id, ".rds"))
  })
