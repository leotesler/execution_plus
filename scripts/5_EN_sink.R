## elastic net model - basic

# load libraries ----
library(tidyverse)
library(tidymodels)
library(doMC)
library(here)
library(stacks)

# load data ----
load(here("samples/pitch_folds.rda"))
load(here("recipes/main_sink_rec.rda"))

# prefer tidymodels ----
tidymodels_prefer()

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# model specification ----
en_spec <- linear_reg(penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# building workflows ----
en_wflow <- workflow() |> 
  add_model(en_spec) |> 
  add_recipe(main_sink_rec)

# tuning parameters ----
en_params <- extract_parameter_set_dials(en_spec)

en_grid <- grid_regular(en_params, levels = 5)

# fitting workflow ----
en_tuned_sink <- tune_grid(en_wflow,
                      pitch_folds,
                      grid = en_grid,
                      control = control_stack_grid())

# save results ----
save(en_tuned_sink, file = here("results/en_tuned_sink.rda"))
