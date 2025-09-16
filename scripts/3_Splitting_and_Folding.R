## Calculating Outcome Grade

# load libraries ----
library(tidyverse)
library(tidymodels)

# load data ----
pitch_data <- read_rds("data/mlb_23/pitch_data.rds")

# prefer tidymodels ----
tidymodels_prefer()

# sample data ----
set.seed(27)
pitch_sample_split <- pitch_data |> 
  initial_split(prop = 0.075, strata = run_exp_above_avg)

pitch_sample <- training(pitch_sample_split)

# split data
set.seed(99)
pitch_split <- pitch_sample |> 
  initial_split(prop = 0.8, strata = run_exp_above_avg)

pitch_train <- training(pitch_split)
pitch_test <- testing(pitch_split)

# Fold data
set.seed(99)
pitch_folds <- vfold_cv(pitch_train, v = 5, repeats = 3, strata = run_exp_above_avg)

# save results
dir.create("samples")

save(pitch_train, file = "samples/pitch_train.rda")
save(pitch_test, file = "samples/pitch_test.rda")
save(pitch_folds, file = "samples/pitch_folds.rda")
