# stuff+ comparison

# load libraries ----
library(tidyverse)
library(here)
library(baseballr)
library(DT)

# load predictions and ids
predictions <- read_csv(here("predictions/mlb_2024.csv")) |> 
  mutate(pitch_grade = (pitch_grade/mean(pitch_grade, na.rm = TRUE)*100))

load(here("data/ids.rda")) 

# get stuff+ data ----
stuff_plus <- fg_pitcher_leaders(startseason = 2024, endseason = 2024) |> 
  janitor::clean_names() |> 
  select(season, x_mlbamid, starts_with("sp_s"))

stuff_plus <- stuff_plus |> 
  pivot_longer(cols = c(sp_s_ch:sp_s_sl, sp_s_kc:sp_s_fo), names_to = "pitch_type") |> 
  mutate(pitch_type = factor(pitch_type),
         pitch_type = fct_recode(pitch_type, 
                                 "CH" = "sp_s_ch",
                                 "FF" = "sp_s_ff",
                                 "SI" = "sp_s_si",
                                 "SL" = "sp_s_sl",
                                 "KC" = "sp_s_kc",
                                 "CU" = "sp_s_cu",
                                 "FC" = "sp_s_fc",
                                 "FS" = "sp_s_fs",
                                 "FO" = "sp_s_fo")) |> 
  select(season, x_mlbamid, pitch_type, stuff_plus_pitch = value, stuff_plus_overall = sp_stuff)

# join data ----
comparison <- predictions |> 
  group_by(pitcher_name, id, pitch_type) |> 
  summarize(mean_pitch_grade = mean(pitch_grade, na.rm = TRUE),
            avg_velo = mean(release_speed, na.rm = TRUE),
            avg_spin = mean(release_spin_rate, na.rm = TRUE),
            n = n()) |> 
  arrange(desc(mean_pitch_grade)) |> 
  left_join(stuff_plus, by = join_by(id == x_mlbamid, pitch_type)) |> 
  select(pitcher_name, pitch_type, execution_plus = mean_pitch_grade, stuff_plus_pitch, stuff_plus_overall, n, avg_velo, avg_spin)

# explore data ----
comparison |> 
  filter(!is.na(stuff_plus_pitch), n >= 100) |> 
  ggplot(aes(x = execution_plus, y = stuff_plus_pitch)) +
  geom_point()

comparison |> 
  filter(!is.na(stuff_plus_pitch), n >= 100) |> 
  arrange(desc(stuff_plus_pitch))
