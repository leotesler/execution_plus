# Generate Predictions for AAA players

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(baseballr)
library(httr)
library(rvest)
library(jsonlite)

# load model and data ----
load(here("results/bt_fit_final.rda"))

# handle common conflicts ----
tidymodels_prefer()

# get AAA games ----
dates <- mlb_schedule(season = 2025, level_ids = "11") |> 
  group_by(date) |> 
  summarize() |> 
  pull(date)

# statcast minor league search function ----
statcast_search_minors <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(), player_type = "pitcher", level = "AAA") {
  url <- paste0(
    "https://baseballsavant.mlb.com/statcast-search-minors/csv?",
    "hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&hfStadium=&hfBBL=&hfNewZones=",
    "&hfPull=&hfC=&hfSea=2025%7C&hfSit=&player_type=pitcher&hfOuts=",
    "&home_road=&pitcher_throws=&batter_stands=&hfSA=&hfEventOuts=",
    "&hfEventRuns=&game_date_gt=", start_date, "&game_date_lt=",
    end_date, "&hfMo=&hfTeam=&hfOpponent=&hfRO=&position=&hfInn=&hfBBT=",
    "&hfFlag=is%5C.%5C.tracked%7C&hfLevel=", level, "%7C&metric_1=&hfTeamAffiliate=",
    "&hfOpponentAffiliate=&group_by=name&min_pitches=0&min_results=0&min_pas=0&sort_col=",
    "pitches&player_event_sort=api_p_release_speed&sort_order=desc&chk_is..tracked=on",
    "&type=details&all=true&minors=true"
  )
  
  payload <- tryCatch(
    baseballr:::csv_from_url(url),
    error = function(e) {
      message("Error fetching CSV. URL likely returned HTML.")
      return(NULL)
    }
  )
  
  if (is.null(payload)) return(NULL)
  
  n_cols <- ncol(payload)
  
  if (n_cols == 118) {
    names(payload) <- c("pitch_type", "game_date", "release_speed", 
                        "release_pos_x", "release_pos_z", "player_name", 
                        "batter", "pitcher", "events", "description", "spin_dir", 
                        "spin_rate_deprecated", "break_angle_deprecated", 
                        "break_length_deprecated", "zone", "des", "game_type", 
                        "stand", "p_throws", "home_team", "away_team", "type", 
                        "hit_location", "bb_type", "balls", "strikes", "game_year", 
                        "pfx_x", "pfx_z", "plate_x", "plate_z", "on_3b", 
                        "on_2b", "on_1b", "outs_when_up", "inning", "inning_topbot", 
                        "hc_x", "hc_y", "tfs_deprecated", "tfs_zulu_deprecated", 
                        "umpire", "sv_id", "vx0", "vy0", "vz0", "ax", "ay", 
                        "az", "sz_top", "sz_bot", "hit_distance_sc", "launch_speed", 
                        "launch_angle", "effective_speed", "release_spin_rate", 
                        "release_extension", "game_pk", "fielder_2", "fielder_3", 
                        "fielder_4", "fielder_5", "fielder_6", "fielder_7", 
                        "fielder_8", "fielder_9", "release_pos_y", "estimated_ba_using_speedangle", 
                        "estimated_woba_using_speedangle", "woba_value", 
                        "woba_denom", "babip_value", "iso_value", "launch_speed_angle", 
                        "at_bat_number", "pitch_number", "pitch_name", "home_score", 
                        "away_score", "bat_score", "fld_score", "post_away_score", 
                        "post_home_score", "post_bat_score", "post_fld_score", 
                        "if_fielding_alignment", "of_fielding_alignment", 
                        "spin_axis", "delta_home_win_exp", "delta_run_exp", 
                        "bat_speed", "swing_length", "estimated_slg_using_speedangle", 
                        "delta_pitcher_run_exp", "hyper_speed", "home_score_diff", 
                        "bat_score_diff", "home_win_exp", "bat_win_exp", 
                        "age_pit_legacy", "age_bat_legacy", "age_pit", "age_bat", 
                        "n_thruorder_pitcher", "n_priorpa_thisgame_player_at_bat", 
                        "pitcher_days_since_prev_game", "batter_days_since_prev_game", 
                        "pitcher_days_until_next_game", "batter_days_until_next_game", 
                        "api_break_z_with_gravity", "api_break_x_arm", "api_break_x_batter_in", 
                        "arm_angle", "attack_angle", "attack_direction", 
                        "swing_path_tilt", "intercept_ball_minus_batter_pos_x_inches", 
                        "intercept_ball_minus_batter_pos_y_inches")
    
    payload <- baseballr:::process_statcast_payload(payload)
    payload <- baseballr:::make_baseballr_data(
      payload, "MiLB Statcast data", Sys.time()
    )
    return(payload)
  }
  
  if (n_cols == 76) {
    
    minor_names <- c("pitch_type", "game_date", "release_speed", 
                     "release_pos_x", "release_pos_z", "player_name", 
                     "batter", "pitcher", "events", "description", "spin_dir", 
                     "spin_rate_deprecated", "break_angle_deprecated", 
                     "break_length_deprecated", "zone", "des", "game_type", 
                     "stand", "p_throws", "home_team", "away_team", "type", 
                     "hit_location", "bb_type", "balls", "strikes", "game_year", 
                     "pfx_x", "pfx_z", "plate_x", "plate_z", "on_3b", 
                     "on_2b", "on_1b", "outs_when_up", "inning", "inning_topbot", 
                     "hc_x", "hc_y", "tfs_deprecated", "tfs_zulu_deprecated", 
                     "umpire", "sv_id", "vx0", "vy0", "vz0", "ax", "ay", 
                     "az", "sz_top", "sz_bot", "hit_distance_sc", "launch_speed", 
                     "launch_angle", "effective_speed", "release_spin_rate", 
                     "release_extension", "game_pk", "fielder_2", "fielder_3", 
                     "fielder_4", "fielder_5", "fielder_6", "fielder_7", 
                     "fielder_8", "fielder_9", "release_pos_y", "estimated_ba_using_speedangle", 
                     "estimated_woba_using_speedangle", "woba_value", 
                     "woba_denom", "babip_value", "iso_value", "launch_speed_angle", 
                     "at_bat_number", "pitch_number", "pitch_name", "home_score", 
                     "away_score", "bat_score", "fld_score", "post_away_score", 
                     "post_home_score", "post_bat_score", "post_fld_score", 
                     "if_fielding_alignment", "of_fielding_alignment", 
                     "spin_axis", "delta_home_win_exp", "delta_run_exp", 
                     "bat_speed", "swing_length", "estimated_slg_using_speedangle", 
                     "delta_pitcher_run_exp", "hyper_speed", "home_score_diff", 
                     "bat_score_diff", "home_win_exp", "bat_win_exp", 
                     "age_pit_legacy", "age_bat_legacy", "age_pit", "age_bat", 
                     "n_thruorder_pitcher", "n_priorpa_thisgame_player_at_bat", 
                     "pitcher_days_since_prev_game", "batter_days_since_prev_game", 
                     "pitcher_days_until_next_game", "batter_days_until_next_game", 
                     "api_break_z_with_gravity", "api_break_x_arm", "api_break_x_batter_in", 
                     "arm_angle", "attack_angle", "attack_direction", 
                     "swing_path_tilt", "intercept_ball_minus_batter_pos_x_inches", 
                     "intercept_ball_minus_batter_pos_y_inches")
    
    if (length(minor_names) == n_cols) {
      names(payload) <- minor_names
    } else {
      warning("Unexpected number of columns for MiLB payload; leaving names as-is.")
    }
    
    return(payload)
  }
  
  if (n_cols <=5) {
    warning("HTML response detected (rate-limited or invalid query).")
    return(payload)
  }
  
  warning(paste0("Unknown schema: returned ", n_cols, " columns."))
  return(payload)
}

# iterate through statcast data ----
aaa_data_list <- list()

for (i in dates) {
  data <- statcast_search_minors(start_date = i, end_date = i) |> 
    mutate(across(where(is.logical), as.character),
           pitcher_days_since_prev_game = as.numeric(pitcher_days_since_prev_game),
           batter = as.character(batter),
           pitcher = as.character(pitcher),
           hit_location = as.numeric(hit_location),
           balls = as.numeric(balls),
           strikes = as.numeric(strikes),
           game_year = as.numeric(game_year),
           outs_when_up = as.numeric(outs_when_up),
           across(starts_with("fielder"), as.character),
           across(starts_with("estimated"), as.numeric),
           across(starts_with("woba"), as.numeric),
           across(starts_with("babip"), as.numeric),
           across(starts_with("iso"), as.numeric),
           launch_speed_angle = as.numeric(launch_speed_angle),
           across(ends_with("exp"), as.numeric),
           hyper_speed = as.numeric(hyper_speed),
           across(ends_with("diff"), as.numeric),
           across(starts_with("age"), as.numeric),
           across(starts_with("n_"), as.numeric),
           batter_days_since_prev_game = as.numeric(batter_days_since_prev_game),
           across(ends_with("next_game"), as.numeric),
           across(starts_with("api_"), as.numeric))
  
  aaa_data_list[[as.character(i)]] <- data
}

aaa_data <- bind_rows(aaa_data_list)

# clean data for predictions ----
aaa_data <- aaa_data |> 
  mutate(balls = factor(balls),
         strikes = factor(strikes))

# generate predictions ----
swing_code <- c("bunt_foul_tip", "foul", "foul_bunt", "foul_tip",
                "hit_into_play", "missed_bunt", "swinging_strike", "swinging_strike_blocked")
whiff_code <- c("swinging_strike", "swinging_strike_blocked", "foul_tip")

aaa_pred <- aaa_data |> 
  bind_cols(predict(bt_fit_final, aaa_data)) |> 
  mutate(pitcher_team = if_else(inning_topbot == "Bot", away_team, home_team),
         opponent = if_else(inning_topbot == "Top", away_team, home_team),
         pitch_grade = percent_rank(.pred)*100,
         balls = as.numeric(balls),
         strikes = as.numeric(strikes),
         swing = description %in% swing_code,
         whiff = description %in% whiff_code,
         in_zone = zone < 10,
         out_zone = zone > 10,
         chase = !in_zone & swing,
         pitch_grade = percent_rank(pitch_grade)*100,
         pitch_grade = (pitch_grade/mean(pitch_grade, na.rm = TRUE))*100)

# save data ----
save(aaa_pred, file = "predictions/aaa_2025.rds")

aaa_pred |> 
  group_by(pitcher) |> 
  group_walk(~ {
    out_path <- here::here("ExecutionPlusApp", "predictions", paste0(.y$pitcher, "_aaa.rds"))
    saveRDS(.x, file = out_path)
    message("Saved ", out_path, " with size: ", file.info(out_path)$size)
  })
