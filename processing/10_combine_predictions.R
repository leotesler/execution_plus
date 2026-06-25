# combine MLB and AAA Predictions

# load libraries ----
library(tidyverse)
library(googlesheets4)

# load predictions ----
load("processing/player_ep_mlb_25.rda")
load("processing/player_ep_aaa_25.rda")

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
            ss = "1dIH3BHPOGVjCiww6ZSpyLh1TxTNSL7h7pYnd1CYJCiA",
            sheet = "2025 Execution Plus")
