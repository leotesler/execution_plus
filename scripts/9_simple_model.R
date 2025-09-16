# explore predictions

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)


# load predictions ----
predictions <- read_csv(here("predictions/mlb_2024.csv")) |> 
  mutate(pitch_grade = (pitch_grade/mean(pitch_grade, na.rm = TRUE)*100))

# splits and folds ----
set.seed(45)
lm_split <- initial_split(predictions, prop = 0.8, strata = pitch_grade)

lm_train <- training(lm_split)
lm_test <- testing(lm_split)

lm_folds <- vfold_cv(lm_train, v = 5, repeats = 3, strata = pitch_grade)

# recipe ----
lm_rec <- recipe(pitch_grade ~ pitch_type + release_speed + release_spin_rate + pfx_x + pfx_z + stand + p_throws,
                 lm_train) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_nzv(all_predictors()) |> 
  step_interact(~all_predictors():all_predictors()) |> 
  step_corr(all_predictors(), threshold = 0.9) |> 
  step_nzv(all_predictors()) |> 
  step_zv(all_predictors())

# resamples fit ----
lm_spec <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

lm_wflow <- workflow() |> 
  add_model(lm_spec) |> 
  add_recipe(lm_rec)

lm_fit_resamples <- fit_resamples(lm_wflow,
                                  lm_folds,
                                  control = control_resamples(save_workflow = TRUE))

# fit to training set ----
lm_wflow_final <- lm_fit_resamples |> 
  extract_workflow() |> 
  finalize_workflow(select_best(lm_fit_resamples, metric = "rmse"))

lm_fit_final <- fit(lm_wflow_final, lm_train)

save(lm_fit_final, file = here("results/simple_final.rda"))

load("results/simple_final.rda")

lm_fit_final |> 
  tidy()

lm_test |> 
  bind_cols(predict(lm_fit_final, lm_test)) |> 
  rmse(pitch_grade, .pred)

max(lm_test$pitch_grade) - min(lm_test$pitch_grade)
