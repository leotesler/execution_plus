## Calculating Outcome Grade

# load libraries ----
library(tidyverse)
library(tidymodels)
library(naniar)

# load data ----
savant_data <- read_csv("data/mlb_23/savant_data.csv") |> 
  mutate(delta_run_exp = -delta_run_exp,
         across(where(is.character), as.factor))

# inspect data ----
savant_data |> 
  ggplot(aes(x = delta_run_exp)) +
  geom_histogram()

mean_run_exp <- savant_data |> 
  group_by(pitch_type) |> 
  summarize(mean_run_exp = mean(delta_run_exp, na.rm = TRUE),
            n = n()) |> 
  arrange(mean_run_exp)

range(savant_data$delta_run_exp, na.rm = TRUE)

# calculate outcome grade ----
pitch_data <- savant_data |> 
  left_join(mean_run_exp, by = join_by(pitch_type == pitch_type))

pitch_data <- pitch_data |> 
  mutate(run_exp_above_avg = delta_run_exp - mean_run_exp)

pitch_data <- pitch_data |> 
  mutate(pitch_type = factor(pitch_type)) |> 
  select(release_spin_rate, spin_axis, 
         of_fielding_alignment, if_fielding_alignment, 
         release_extension, effective_speed, 
         release_speed, zone, vz0, vx0, vy0,
         sz_top, sz_bot, release_pos_z,
         release_pos_x, release_pos_y,
         plate_z, plate_x, pfx_x, pfx_z,
         az, ay, ax, pitch_type, pitch_name,
         type, strikes, stand, p_throws, post_home_score,
         post_away_score, player_name, pitcher...8,
         pitcher...60, pitch_number, outs_when_up,
         inning_topbot, inning, home_team, home_score,
         game_year, game_type, game_date, game_pk,
         description, delta_home_win_exp, batter,
         bat_score, des, balls, away_team, home_score,
         away_score, at_bat_number, run_exp_above_avg) |>
  mutate(balls = factor(balls),
         strikes = factor(strikes),
         stand = factor(stand),
         p_throws = factor(p_throws)) |> 
  filter(!is.na(run_exp_above_avg))

gg_miss_var(pitch_data)

write_rds(pitch_data, "data/mlb_23/pitch_data.rds")
