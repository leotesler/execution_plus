## Final Project
## Reading in and compiling dataset

# load libraries ----
library(tidyverse)

# load data
savant_data <- bind_rows(read_csv("data/mlb_23/ari_pitches_2023.csv"),
                         read_csv("data/mlb_23/atl_pitches_2023.csv"),
                         read_csv("data/mlb_23/bal_pitches_2023.csv"),
                         read_csv("data/mlb_23/bos_pitches_2023.csv"),
                         read_csv("data/mlb_23/chc_pitches_2023.csv"),
                         read_csv("data/mlb_23/chw_pitches_2023.csv"),
                         read_csv("data/mlb_23/cin_pitches_2023.csv"),
                         read_csv("data/mlb_23/cle_pitches_2023.csv"),
                         read_csv("data/mlb_23/col_pitches_2023.csv"),
                         read_csv("data/mlb_23/det_pitches_2023.csv"),
                         read_csv("data/mlb_23/excess_pitches_2023.csv"),
                         read_csv("data/mlb_23/hou_pitches_2023.csv"),
                         read_csv("data/mlb_23/kcr_pitches_2023.csv"),
                         read_csv("data/mlb_23/laa_pitches_2023.csv"),
                         read_csv("data/mlb_23/lad_pitches_2023.csv"),
                         read_csv("data/mlb_23/mia_pitches_2023.csv"),
                         read_csv("data/mlb_23/mil_pitches_2023.csv"),
                         read_csv("data/mlb_23/min_pitches_2023.csv"),
                         read_csv("data/mlb_23/nym_pitches_2023.csv"),
                         read_csv("data/mlb_23/nyy_pitches_2023.csv"),
                         read_csv("data/mlb_23/oak_pitches_2023.csv"),
                         read_csv("data/mlb_23/phi_pitches_2023.csv"),
                         read_csv("data/mlb_23/pit_pitches_2023.csv"),
                         read_csv("data/mlb_23/sdp_pitches_2023.csv"),
                         read_csv("data/mlb_23/sea_pitches_2023.csv"),
                         read_csv("data/mlb_23/sfg_pitches_2023.csv"),
                         read_csv("data/mlb_23/stl_pitches_2023.csv"),
                         read_csv("data/mlb_23/tbr_pitches_2023.csv"),
                         read_csv("data/mlb_23/tex_pitches_2023.csv"),
                         read_csv("data/mlb_23/tor_pitches_2023.csv"),
                         read_csv("data/mlb_23/wsh_pitches_2023.csv"))

write_csv(savant_data, "data/mlb_23/savant_data.csv")
