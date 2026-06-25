# KNN model - basic

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(stacks)
library(doMC)

# load data ----
load(here("samples/pitch_folds.rda"))
load(here("recipes/tree_sink_rec.rda"))

# handle common conflicts ----
set.seed(99)
tidymodels_prefer()

# parallel processing ----
registerDoMC(cores = parallel::detectCores())

# model specification ----
knn_spec <- nearest_neighbor(neighbors = tune()) |> 
  set_engine("kknn") |> 
  set_mode("regression")

# define workflow ----
knn_wflow <- workflow() |> 
  add_model(knn_spec) |> 
  add_recipe(tree_sink_rec)

# tuning parameters ----
knn_params <- extract_parameter_set_dials(knn_spec)

knn_grid <- grid_regular(knn_params, levels = 10)

# fitting workflows ----
knn_tuned_basic <- tune_grid(knn_wflow,
                             pitch_folds,
                             grid = knn_grid,
                             control = control_stack_grid())

# save results ----
save(knn_tuned_basic, file = here("results/knn_tuned_basic.rda"))
