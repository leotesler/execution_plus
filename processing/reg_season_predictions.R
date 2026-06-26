# regular season predictions

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(baseballr)
library(rvest)
library(httr)
library(jsonlite)
library(googledrive)
library(xgboost)
library(googlesheets4)

# set Fangraphs cookie and Google auth ----
fg_cookie <- Sys.getenv("FG_COOKIE")

options(gargle_oauth_cache = "~/.config/gargle")
gs4_auth(email = "ltesler194@gmail.com", cache = "~/.config/gargle")

# check date ----
end_date <- as.Date("2026-09-29")

if (Sys.Date() > end_date) {
  quit(save = "no")
}

# fix baseballr functions ----
statcast_search <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(), 
                             playerid = NULL, player_type = "batter", ...) {
  if (start_date <= "2015-03-01") {
    message("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
  }
  if (start_date < "2008-03-25") {
    stop("The data are limited to the 2008 MLB season and after.")
    return(NULL)
  }
  if (start_date == Sys.Date()) {
    message("The data are collected daily at 3 a.m. Some of today's games may not be included.")
  }
  if (start_date > as.Date(end_date)) {
    stop("The start date is later than the end date.")
    return(NULL)
  }
  playerid_var <- ifelse(player_type == "pitcher", "pitchers_lookup%5B%5D", 
                         "batters_lookup%5B%5D")
  vars <- tibble::tribble(~var, ~value, "all", "true", "hfPT", 
                          "", "hfAB", "", "hfBBT", "", "hfPR", "", "hfZ", "", 
                          "stadium", "", "hfBBL", "", "hfNewZones", "", "hfGT", 
                          "R%7CPO%7CS%7C&hfC", "hfSea", paste0(lubridate::year(start_date), 
                                                               "%7C"), "hfSit", "", "hfOuts", "", "opponent", "", 
                          "pitcher_throws", "", "batter_stands", "", "hfSA", "", 
                          "player_type", player_type, "hfInfield", "", "team", 
                          "", "position", "", "hfOutfield", "", "hfRO", "", "home_road", 
                          "", playerid_var, ifelse(is.null(playerid), "", as.character(playerid)), 
                          "game_date_gt", as.character(start_date), "game_date_lt", 
                          as.character(end_date), "hfFlag", "", "hfPull", "", 
                          "metric_1", "", "hfInn", "", "min_pitches", "0", "min_results", 
                          "0", "group_by", "name", "sort_col", "pitches", "player_event_sort", 
                          "h_launch_speed", "sort_order", "desc", "min_abs", "0", 
                          "type", "details") %>% dplyr::mutate(pairs = paste0(.data$var, 
                                                                              "=", .data$value))
  if (is.null(playerid)) {
    vars <- vars %>% dplyr::filter(!grepl("lookup", .data$var))
  }
  url_vars <- paste0(vars$pairs, collapse = "&")
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?", 
                url_vars)
  tryCatch({
    suppressMessages(suppressWarnings(payload <- baseballr:::csv_from_url(url, 
                                                              encoding = "UTF-8")))
  }, error = function(cond) {
    message(cond)
    stop("No payload acquired")
  }, warning = function(cond) {
    message(cond)
  })
  if (nrow(payload) > 1) {
    names(payload) <- c(
      "pitch_type", "game_date", "release_speed", 
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
      "bat_speed", "swing_length", "miss_distance", "estimated_slg_using_speedangle", 
      "delta_pitcher_run_exp", "hyper_speed", "home_score_diff", 
      "bat_score_diff", "home_win_exp", "bat_win_exp", 
      "age_pit_legacy", "age_bat_legacy", "age_pit", "age_bat", 
      "n_thruorder_pitcher", "n_priorpa_thisgame_player_at_bat", 
      "pitcher_days_since_prev_game", "batter_days_since_prev_game", 
      "pitcher_days_until_next_game", "batter_days_until_next_game", 
      "api_break_z_with_gravity", "api_break_x_arm", "api_break_x_batter_in", 
      "arm_angle", "attack_angle", "attack_direction", 
      "swing_path_tilt", "intercept_ball_minus_batter_pos_x_inches", 
      "intercept_ball_minus_batter_pos_y_inches"
    )
    payload <- process_statcast_payload(payload) %>% baseballr:::make_baseballr_data("MLB Baseball Savant Statcast Search data from baseballsavant.mlb.com", 
                                                                         Sys.time())
    return(payload)
  }
  else {
    warning("No valid data found")
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
                        "bat_speed", "swing_length", "miss_distance", "estimated_slg_using_speedangle", 
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
    payload <- payload %>% baseballr:::make_baseballr_data("MLB Baseball Savant Statcast Search data from baseballsavant.mlb.com", 
                                               Sys.time())
    return(payload)
  }
}

statcast_search_pitchers <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(), 
                                     playerid = NULL, ...) {
  statcast_search(start_date = start_date, end_date = end_date, 
                  playerid = playerid, player_type = "pitcher", ...)
}

statcast_search_batters <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(), 
                                     playerid = NULL, ...) {
  statcast_search(start_date = start_date, end_date = end_date, 
                  playerid = playerid, player_type = "batter", ...)
}

# load model and data ----
bt_fit_final <- readRDS(here("scripts/results/bt_fit_final.rds"))
bt_fit_final$fit$fit$fit <- xgb.load("scripts/results/bt_booster.xgb")

prior_preds <- readRDS("predictions/mlb_2026.rds")

days <- c(as.character(seq(as.Date("2026-03-25"), as.Date(today() - 1), by = "days")))

days <- as.character(seq(max(prior_preds$game_date) + 1, Sys.Date() - 1, by = "days"))

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
compiled_data_new <- compiled_data |> 
  mutate(across(where(is.character), as.factor),
         balls = factor(balls),
         strikes = factor(strikes),
         delta_run_exp = -delta_run_exp)

mean_run_exp <- compiled_data_new |> 
  group_by(pitch_type) |> 
  summarize(mean_run_exp = mean(delta_run_exp, na.rm = TRUE),
            n = n()) |> 
  arrange(mean_run_exp)

compiled_data_expanded <- compiled_data_new |> 
  left_join(mean_run_exp, by = join_by(pitch_type == pitch_type)) |> 
  mutate(run_exp_above_avg = delta_run_exp - mean_run_exp)

# generate predictions ----
predictions <- compiled_data_expanded |> 
  bind_cols(predict(bt_fit_final, compiled_data_expanded)) |>
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
  mutate(pitcher_name = if_else(is.na(pitcher_name), batter_name, pitcher_name)) |> 
  select(!batter_name) |> 
  mutate(batter_name = hitter_name) |> 
  select(!hitter_name) |> 
  mutate(balls = as.numeric(balls),
         strikes = as.numeric(strikes),
         pfx_z = pfx_z * 12,
         pfx_x = pfx_x * 12)

predictions <- bind_rows(
  predictions,
  prior_preds
) |> 
  filter(pitch_type != "") |> 
  mutate(percentile_rank = percent_rank(predicted_reaa)*100,
         pitch_grade = (percentile_rank/mean(percentile_rank, na.rm = TRUE)*100),
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
         chase = !in_zone & swing)

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

saveRDS(predictions, file = "predictions/mlb_2026.rds")
saveRDS(df_statcast_grouped, "ExecutionPlusApp/predictions/df_statcast_grouped.rds")

predictions |> 
  group_by(id) |> 
  group_walk(~ {
    saveRDS(.x, paste0("ExecutionPlusApp/predictions/", .y$id, ".rds"))
  })

# write to google sheet ----
url_sheet <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=pit&lg=all&qual=0&season=2026&season1=2026&startdate=2026-03-01&enddate=2026-11-01&month=0&hand=&team=0&pageitems=300000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
response_sheet <- GET(
  url_sheet,
  add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
    `Cookie` = fg_cookie,
    `Referer` = "https://www.fangraphs.com/leaders/major-league"
  )
)
page_sheet <- content(response_sheet, as = "text", encoding = "UTF-8")
data_sheet <- fromJSON(page_sheet)
fg_data <- as.tibble(data_sheet$data) |> 
  janitor::clean_names() |> 
  select(x_mlbamid, player_name, tbf)

player_ep_mlb <- predictions |> 
  group_by(id, pitcher_name) |> 
  summarize(execution_plus = mean(pitch_grade, na.rm = TRUE)) |> 
  select(!pitcher_name) |> 
  left_join(fg_data, by = join_by(id == x_mlbamid))

sheet_write(player_ep_mlb, 
            ss = "1bJvouRVq8Z_p5ZLcNJ--mkrvSmVm9ulDahVvGRFnOtE",
            sheet = "Player Execution Plus")

2

# write Fangraphs data to sheets ----
url_bat <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=0&season=2026&season1=2026&startdate=2026-03-01&enddate=2026-11-01&month=0&hand=&team=0%2Cto&pageitems=3000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
url_pit <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=pit&lg=all&qual=0&season=2026&season1=2026&startdate=2026-03-01&enddate=2026-11-01&month=0&hand=&team=0%2Cto&pageitems=3000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
url_fld <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=fld&lg=all&qual=0&season=2026&season1=2026&startdate=2026-03-01&enddate=2026-11-01&month=0&hand=&team=0%2Cto&pageitems=3000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"

response_bat <- GET(
  url_bat,
  add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
    `Cookie` = fg_cookie,
    `Referer` = "https://www.fangraphs.com/leaders/major-league"
  )
)
response_pit <- GET(
  url_pit,
  add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
    `Cookie` = fg_cookie,
    `Referer` = "https://www.fangraphs.com/leaders/major-league"
  )
)
response_fld <- GET(
  url_fld,
  add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
    `Cookie` = fg_cookie,
    `Referer` = "https://www.fangraphs.com/leaders/major-league"
  )
)

raw_bat <- content(response_bat, as = "text", encoding = "UTF-8")
raw_pit <- content(response_pit, as = "text", encoding = "UTF-8")
raw_fld <- content(response_fld, as = "text", encoding = "UTF-8")

json_bat <- fromJSON(raw_bat)
json_pit <- fromJSON(raw_pit)
json_fld <- fromJSON(raw_fld)

data_bat <- tibble(json_bat$data)
data_pit <- tibble(json_pit$data)
data_fld <- tibble(json_fld$data)


range_write(
  ss = "1bJvouRVq8Z_p5ZLcNJ--mkrvSmVm9ulDahVvGRFnOtE",
  data = data_bat,
  sheet = "Fangraphs Batting Data",
  range = "A1",
  col_names = TRUE,
  reformat = FALSE
)
range_write(
  ss = "1bJvouRVq8Z_p5ZLcNJ--mkrvSmVm9ulDahVvGRFnOtE",
  data = data_pit,
  sheet = "Fangraphs Pitching Data",
  range = "A1",
  col_names = TRUE,
  reformat = FALSE
)
range_write(
  ss = "1bJvouRVq8Z_p5ZLcNJ--mkrvSmVm9ulDahVvGRFnOtE",
  data = data_fld,
  sheet = "Fangraphs Fielding Data",
  range = "A1",
  col_names = TRUE,
  reformat = FALSE
)

# re-deploy app ----
#rsconnect::deployApp(appDir = "ExecutionPlusApp")
