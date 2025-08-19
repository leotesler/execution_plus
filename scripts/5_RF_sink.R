# Random forest - basic

# load libraries ----
library(tidyverse)
library(tidymodels)
library(doMC)
library(here)
library(stacks)

# load data ----
load(here("samples/pitch_folds.rda"))
load(here("recipes/tree_sink_rec.rda"))

# prefer tidymodels ----
tidymodels_prefer()

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# model specification ----
rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |> 
  set_engine("ranger") |> 
  set_mode("regression")

# define workflow ----
rf_wflow <- workflow() |> 
  add_model(rf_spec) |> 
  add_recipe(tree_sink_rec)

# tuning parameters ----
rf_params <- extract_parameter_set_dials(rf_spec) |> 
  update(mtry = mtry(range = c(1, 44)))

rf_grid <- grid_latin_hypercube(rf_params, size = 30)

# fitting workflow ----
set.seed(99)
rf_tuned_sink <- tune_grid(rf_wflow,
                           pitch_folds,
                           grid = rf_grid,
                           control = control_stack_grid())

# save results ----
save(rf_tuned_sink, file = here("results/rf_tuned_sink.rda"))
