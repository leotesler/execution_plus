# boosted tree model - basic

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
registerDoMC(cores = detectCores())

# model specification ----
bt_spec <- boost_tree(mtry = tune(), min_n = tune(), learn_rate = tune(), trees = 1000) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

# define workflow ----
bt_wflow <- workflow() |> 
  add_model(bt_spec) |> 
  add_recipe(tree_sink_rec)

# tuning parameters ----
bt_params <- extract_parameter_set_dials(bt_spec) |> 
  update(mtry = mtry(range = c(1, 44)))

bt_grid <- grid_latin_hypercube(bt_params, size = 30)

# fitting workflows ----
bt_tuned_basic <- tune_grid(bt_wflow,
                            pitch_folds,
                            grid = bt_grid,
                            control = control_stack_grid())

# save results ----
save(bt_tuned_basic, file = here("results/bt_tuned_basic.rda"))
