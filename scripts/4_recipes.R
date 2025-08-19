## Recipes

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(naniar)

# load data ----
load(here("samples/pitch_train.rda"))
load(here("results/var_select_lasso.rda"))

# prefer tidymodels ----
tidymodels_prefer()

# variable selection ----
selected_lasso <- var_select_lasso |> 
  tidy() |> 
  filter(estimate != 0) |> 
  pull(term)

numeric_vars <- pitch_train |> 
  select(where(is.numeric)) |> 
  colnames()

factor_vars <- pitch_train |> 
  select(where(is.factor)) |> 
  colnames()

imp_numerics <- selected_lasso[selected_lasso %in% numeric_vars]

num_true <- map(factor_vars,
                ~startsWith(selected_lasso, prefix = .x) |> 
                  sum())

names(num_true) <- factor_vars

imp_factors <- enframe(unlist(num_true)) |> 
  filter(value != 0) |> 
  pull(name)

var_keep <- c(imp_numerics, imp_factors)

pitch_train_lasso <- pitch_train |> 
  select(all_of(var_keep), run_exp_above_avg)

# missingness check ----
gg_miss_var(pitch_train_lasso)

# recipes for parametric models ----
main_sink_rec <- recipe(run_exp_above_avg ~ ., pitch_train) |> 
  step_rm(pitch_name, player_name, pitcher...8, pitcher...60, inning_topbot, game_date, batter, des, game_type) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_nzv(all_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_predictors())

main_feat_rec <- recipe(run_exp_above_avg ~ ., pitch_train_lasso) |>
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_other(all_nominal_predictors(), threshold = 0.05) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_nzv(all_predictors()) |> 
  step_interact(~all_predictors():all_predictors()) |> 
  step_impute_median(all_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_corr(all_predictors(), threshold = 0.9) |> 
  step_normalize(all_predictors())

# recipes for non-parametric models ----
tree_sink_rec <- recipe(run_exp_above_avg ~ ., pitch_train) |> 
  step_rm(pitch_name, player_name, pitcher...8, pitcher...60, inning_topbot, game_date, batter, des, game_type) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_predictors())

tree_feat_rec <- recipe(run_exp_above_avg ~ ., pitch_train_lasso) |>
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |>  
  step_other(all_nominal_predictors(), threshold = 0.05) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_nzv(all_nominal_predictors()) |> 
  step_impute_median(all_numeric_predictors()) |>
  step_zv(all_predictors()) |> 
  step_corr(all_predictors(), threshold = 0.9) |> 
  step_normalize(all_predictors())

ex <- prep(tree_feat_rec) |> 
  bake(new_data = NULL)

# save results ----
dir.create("recipes")

save(main_sink_rec, file = here("recipes/main_sink_rec.rda"))
save(main_feat_rec, file = here("recipes/main_feat_rec.rda"))
save(tree_sink_rec, file = here("recipes/tree_sink_rec.rda"))
save(tree_feat_rec, file = here("recipes/tree_feat_rec.rda"))
  
