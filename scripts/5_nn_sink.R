# neural network model - basic

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(stacks)

# load data ----
load(here("samples/pitch_folds.rda"))
load(here("recipes/tree_sink_rec.rda"))

# handle common conflicts ----
set.seed(99)
tidymodels_prefer()

# parallel processing ----
registerDoMC(cores = parallel::detectCores())

# model specificiation ----
nn_spec <- mlp(penalty = tune(), hidden_units = tune()) |> 
  set_engine("nnet") |> 
  set_mode("regression")

# define workflow ----
nn_wflow <- workflow() |> 
  add_model(nn_spec) |> 
  add_recipe(tree_sink_rec)

# tuning parameters ----
nn_params <- extract_parameter_set_dials(nn_spec)

nn_grid <- grid_regular(nn_params, levels = 5)

# fitting workflows ----
nn_tuned_basic <- tune_grid(nn_wflow,
                            pitch_folds,
                            grid = nn_grid,
                            control = control_stack_grid())

# save results ----
save(nn_tuned_basic, file = here("results/nn_tuned_basic.rda"))
