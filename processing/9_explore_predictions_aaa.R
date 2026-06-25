# explore predictions

# load libraries ----
library(tidyverse)
library(here)
library(DT)
library(baseballr)
library(httr)
library(rvest)
library(jsonlite)
library(googlesheets4)

# load predictions ----
load(here("predictions/aaa_2025.rds"))

predictions <- aaa_pred |> 
  mutate(pitch_grade = (pitch_grade/mean(pitch_grade, na.rm = TRUE))*100)

predictions |> 
  slice_max(game_date, n = 1) |> 
  select(game_date)

predictions |> 
  ggplot(aes(x = pitch_grade)) +
  geom_histogram(bins = 100)

# summary table
predictions |> 
  group_by(player_name, pitch_type) |> 
  summarize(mean_pitch_grade = mean(pitch_grade, na.rm = TRUE),
            avg_velo = mean(release_speed, na.rm = TRUE),
            avg_spin = mean(release_spin_rate, na.rm = TRUE),
            n = n()) |> 
  arrange(desc(mean_pitch_grade)) |> 
  filter(n >= 300)

# for google sheets export ----
url <- "http://statsapi.mlb.com/api/v1/stats?stats=season&sportId=11&season=2025&group=pitching&playerPool=All&limit=20000&offset=0"
raw <- fromJSON(url, simplifyVector = FALSE)
splits <- raw$stats[[1]]$splits

parse_split <- function(x) {
  tibble(
    season = x$season,
    player_id = x$player$id,
    player_name = x$player$fullName,
    team_id = x$team$id,
    team_name = x$team$name,
    league_id = x$league$id,
    league_name = x$league$name,
    sport_id = x$sport$id,
    sport_name = x$sport$abbreviation,
    position = x$position$name,
    rank = x$rank,
    
    !!!as_tibble(x$stat)
  )
}

aaa_bf <- map_dfr(splits, parse_split) |> 
  janitor::clean_names() |>
  mutate(player_id = as.character(player_id)) |> 
  select(player_id, tbf = batters_faced)

player_ep_aaa <- predictions |> 
  group_by(pitcher, player_name) |> 
  summarize(execution_plus = mean(pitch_grade, na.rm = TRUE)) |> 
  separate(player_name, into = c("last", "first"), sep = ",") |> 
  mutate(first = trimws(first),
         player_name = str_c(first, last, sep = " ")) |> 
  select(id = pitcher, execution_plus, player_name) |> 
  left_join(aaa_bf, by = join_by(id == player_id))

save(player_ep_aaa, file = "processing/player_ep_aaa_25.rda")

sheet_write(player_ep,
            ss = "1dIH3BHPOGVjCiww6ZSpyLh1TxTNSL7h7pYnd1CYJCiA",
            sheet = "2025 Exeuction Plus AAA")
  
