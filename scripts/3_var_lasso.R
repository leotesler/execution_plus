# Lasso variable selection

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

# load data ----
load(here("samples/pitch_train.rda"))

# handle common conflicts ----
tidymodels_prefer()

# parallel processing ----
num_cores <- detectCores()
registerDoMC(cores = num_cores)

# lasso folds ----
set.seed(22)
lasso_folds <- pitch_train |> 
  vfold_cv(v = 5, repeats = 1, strata = run_exp_above_avg)

# basic recipe ----
recipe_lasso <- recipe(run_exp_above_avg ~ ., pitch_train) |> 
  step_rm(pitch_name, player_name, pitcher...8, pitcher...60, inning_topbot, game_date, batter, des, game_type) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_predictors())

prep(recipe_lasso) |> 
  bake(new_data = NULL) |> 
  skimr::skim()

# model specification ----
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# define workflow ----
lasso_wflow <- workflow() |> 
  add_model(lasso_spec) |> 
  add_recipe(recipe_lasso)

# tuning parameters ----
lasso_params <- extract_parameter_set_dials(lasso_spec)

lasso_grid <- grid_regular(lasso_params, levels = 5)

# fitting models ----
lasso_tuned <- tune_grid(lasso_wflow,
                         lasso_folds,
                         grid = lasso_grid,
                         control = control_grid(save_workflow = TRUE))

# selecting optimal workflow ----
optimal_wflow <- lasso_tuned |> 
  extract_workflow() |> 
  finalize_workflow(select_best(lasso_tuned, metric = "rmse"))

# fitting best workflow ----
var_select_lasso <- fit(optimal_wflow, pitch_train)

# saving results ----
dir.create("results")

save(var_select_lasso, file = here("results/var_select_lasso.rda"))
