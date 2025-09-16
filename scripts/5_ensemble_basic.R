# ensemble model - basic

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(stacks)

# load models ----
load("results/lm_fit_sink.rda")
load("results/en_tuned_sink.rda")
load("results/bt_tuned_basic.rda")
load("results/rf_tuned_sink.rda")
load("results/knn_tuned_basic.rda")
load("results/nn_tuned_basic.rda")
load("results/mars_tuned_basic.rda")

# load testing data ----
load("samples/pitch_test.rda")

# handle common conflicts ----
set.seed(99)
tidymodels_prefer()

# parallel processing ----
registerDoMC(cores = detectCores())

# data stack ----
pitch_stack <- stacks() |> 
  add_candidates(lm_fit_sink) |> 
  add_candidates(en_tuned_sink) |> 
  add_candidates(knn_tuned_basic) |> 
  add_candidates(rf_tuned_sink) |> 
  add_candidates(bt_tuned_basic) |> 
  add_candidates(nn_tuned_basic) |> 
  add_candidates(mars_tuned_basic)

# blend predictions ----
blend_penalty <- c(10^(-6:-1), 0.5, 1, 1.5, 2)

pitch_blend_basic <- pitch_stack |> 
  blend_predictions(penalty = blend_penalty)

dir.create("results/ensemble_info")

save(pitch_blend_basic, file = "results/ensemble_info/pitch_blend_basic.rda")

# explore blend ----
autoplot(pitch_blend_basic)

autoplot(pitch_blend_basic, type = "members")

autoplot(pitch_blend_basic, type = "weights")

# fit model ----
ensemble_model_basic <- pitch_blend_basic |> 
  fit_members()

# save model ----
save(ensemble_model_basic, file = here("results/ensemble_model_basic.rda"))

# get member predictions ----
member_pred_basic <- pitch_test |> 
  select(run_exp_above_avg) |> 
  bind_cols(predict(ensemble_model_basic, pitch_test, members = TRUE))

# save member predictions ----
save(member_pred_basic, file = "results/ensemble_info/member_pred_basic.rda")
