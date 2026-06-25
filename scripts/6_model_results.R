## Tuning Results

# load libraries ----
library(tidyverse)
library(tidymodels)
library(doMC)
library(DT)

# load data ----
load("results/null_fit.rda")
load("results/lm_fit_feat.rda")
load("results/lm_fit_sink.rda")
load("results/en_tuned_feat.rda")
load("results/en_tuned_sink.rda")
load("results/bt_tuned_basic.rda")
load("results/bt_tuned_complex.rda")
load("results/rf_tuned_sink.rda")
load("results/rf_tuned_feat.rda")
load("results/knn_tuned_basic.rda")
load("results/knn_tuned_complex.rda")
load("results/nn_tuned_basic.rda")
load("results/nn_tuned_complex.rda")
load("results/mars_tuned_basic.rda")
load("results/mars_tuned_complex.rda")

load("samples/pitch_train.rda")

# prefer tidymodels ----
tidymodels_prefer()

# displaying results
null_tbl <- null_fit |> 
  show_best(metric = "rmse") |> 
  slice_min(mean) |> 
  select(mean, n, std_err) |> 
  mutate(model = "null",
         recipe = "basic",
         .before = 1)

lm_sink_tbl <- lm_fit_sink |> 
  show_best(metric = "rmse") |> 
  slice_min(mean) |> 
  select(mean, n, std_err) |> 
  mutate(model = "OLM",
         recipe = "basic",
         .before = 1)

lm_feat_tbl <- lm_fit_feat |> 
  show_best(metric = "rmse") |> 
  slice_min(mean) |> 
  select(mean, n, std_err) |> 
  mutate(model = "OLM",
         recipe = "complex",
         .before = 1)

en_sink_tbl <- en_tuned_sink |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "EN",
         recipe = "basic",
         .before = 1)

en_feat_tbl <- en_tuned_feat |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "EN",
         recipe = "complex",
         .before = 1)

bt_sink_tbl <- bt_tuned_basic |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "boosted tree",
         recipe = "basic",
         .before = 1)

bt_feat_tbl <- bt_tuned_complex |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "boosted tree",
         recipe = "complex",
         .before = 1)

rf_sink_tbl <- rf_tuned_sink |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "random forest",
         recipe = "basic",
         .before = 1)

rf_feat_tbl <- rf_tuned_feat |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "random forest",
         recipe = "complex",
         .before = 1)

knn_sink_tbl <- knn_tuned_basic |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "knn",
         recipe = "basic",
         .before = 1)

knn_feat_tbl <- knn_tuned_complex |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "knn",
         recipe = "complex",
         .before = 1)

nn_sink_tbl <- nn_tuned_basic |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "nn",
         recipe = "basic",
         .before = 1)

nn_feat_tbl <- nn_tuned_complex |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "nn",
         recipe = "complex",
         .before = 1)

mars_sink_tbl <- mars_tuned_basic |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "mars",
         recipe = "basic",
         .before = 1)

mars_feat_tbl <- mars_tuned_complex |> 
  show_best(metric = "rmse") |> 
  slice_head(n = 1) |> 
  select(mean, n, std_err) |> 
  mutate(model = "mars",
         recipe = "complex",
         .before = 1)

bind_rows(null_tbl,
          lm_sink_tbl, lm_feat_tbl,
          en_sink_tbl, en_feat_tbl,
          bt_sink_tbl, bt_feat_tbl,
          rf_sink_tbl, rf_feat_tbl,
          knn_sink_tbl, knn_feat_tbl,
          nn_sink_tbl, nn_feat_tbl,
          mars_sink_tbl, mars_feat_tbl) |> 
  arrange(mean) |> 
  mutate(RMSE = mean,
         .after = 1) |>
  select(model, recipe, RMSE, std_err) |> 
  datatable() |> 
  formatRound(columns = 3:4, digits = 2)

max(pitch_train$run_exp_above_avg) - min(pitch_train$run_exp_above_avg)
