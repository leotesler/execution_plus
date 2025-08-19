# ensemble model - complex

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(stacks)

# load models ----
load("results/lm_fit_feat.rda")
load("results/en_tuned_feat.rda")
load("results/bt_tuned_complex.rda")
load("results/rf_tuned_feat.rda")
load("results/knn_tuned_complex.rda")
load("results/nn_tuned_complex.rda")
load("results/mars_tuned_complex.rda")

# load testing data ----
load("samples/pitch_test.rda")

# handle common conflicts ----
set.seed(99)
tidymodels_prefer()

# parallel processing ----
registerDoMC(cores = detectCores())

# data stack ----
pitch_stack <- stacks() |> 
  add_candidates(lm_fit_feat) |> 
  add_candidates(en_tuned_feat) |> 
  add_candidates(knn_tuned_complex) |> 
  add_candidates(rf_tuned_feat) |> 
  add_candidates(bt_tuned_complex) |> 
  add_candidates(nn_tuned_complex) |> 
  add_candidates(mars_tuned_complex)

# blend predictions ----
blend_penalty <- c(10^(-6:-1), 0, 0.5, 1, 1.5, 2)

pitch_blend_complex <- pitch_stack |> 
  blend_predictions(penalty = blend_penalty)

dir.create("results/ensemble_info")

save(pitch_blend_complex, file = "results/ensemble_info/pitch_blend_complex.rda")

# explore blend ----
autoplot(pitch_blend_complex)

autoplot(pitch_blend_complex, type = "members")

autoplot(pitch_blend_complex, type = "weights")

# fit model ----
ensemble_model_complex <- pitch_blend_complex |> 
  fit_members()

# save model ----
save(ensemble_model_complex, file = here("results/ensemble_model_complex.rda"))

# get member predictions ----
member_pred_complex <- pitch_test |> 
  select(run_exp_above_avg) |> 
  bind_cols(predict(ensemble_model_complex, pitch_test, members = TRUE))

# save member predictions ----
save(member_pred_complex, file = "results/ensemble_info/member_pred_complex.rda")
