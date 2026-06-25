# read data for predictions

# load libraries ----
library(tidyverse)
library(here)
library(baseballr)

# load existing data ----
load(here("data/mlb_24/compiled_data.rda"))

# load new data ----
new_data <- read_csv(here("data/mlb_24/savant_data.csv"))

# compile data ----
compiled_data <- compiled_data |> 
  bind_rows(new_data)

# save data ----
save(compiled_data, file = here("data/mlb_24/compiled_data.rda"))

# FOR AAA DATA
existing_data <- read_csv(here("data/aaa_24/aaa_24.csv"))
new_data <- read_csv(here("data/aaa_24/savant_data.csv"))

aaa_24 <- existing_data |> 
  bind_rows(new_data)

write_csv(aaa_24, file = here("data/aaa_24/aaa_24.csv"))
