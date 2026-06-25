# boosted tree model - complex

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(stacks)

# load data ----
load(here("scripts/samples/pitch_folds.rda"))
load(here("scripts/recipes/tree_feat_rec.rda"))

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
  add_recipe(tree_feat_rec)

# tuning parameters ----
bt_params <- extract_parameter_set_dials(bt_spec) |> 
  update(mtry = mtry(range = c(1, 39)))

bt_grid <- grid_latin_hypercube(bt_params, size = 30)

# fitting workflows ----
progress_env <- new.env()
progress_env$counter <- 0
total_models <- nrow(pitch_folds)*nrow(bt_grid)

progress_update <- function() {
  progress_env$counter <- progress_env$counter + 1
  cat(sprintf("Completed %d of %d models.\n",
              progress_env$counter, total_models))
  flush.console()
}

bt_tuned_complex <- tune_grid(bt_wflow,
                              pitch_folds,
                              grid = bt_grid,
                              control = control_grid(
                                save_workflow = TRUE,
                                extract = function(...) {
                                  progress_update()
                                  list()
                                }
                              ))

# save results ----
save(bt_tuned_complex, file = here("scripts/results/bt_tuned_complex.rda"))
