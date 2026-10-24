# combine MLB and AAA Predictions

# load libraries ----
library(tidyverse)
library(googlesheets4)

# load predictions ----
load("processing/player_ep_mlb_26.rda")
load("processing/player_ep_aaa_26.rda")

# join datasets ----
player_ep <- player_ep_mlb |> 
  mutate(id = as.character(id)) |> 
  full_join(player_ep_aaa, by = join_by(id, player_name),
            suffix = c("_mlb", "_aaa")) |> 
  mutate(across(everything(), ~replace_na(.x, 0)),
         tbf = tbf_mlb+tbf_aaa,
         execution_plus = (execution_plus_mlb*tbf_mlb + execution_plus_aaa*tbf_aaa)/tbf) |> 
  select(id, execution_plus, player_name, tbf)

# write to google sheet ----
sheet_write(player_ep,
            ss = "1bcz_P5I4jzxcCUXjLkc77EJeXp_jH0vqfGKPvJDU8q0",
            sheet = "2026 Execution Plus")
