# final OLM model

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(knitr)

# load model and data ----
load("results/bt_tuned_complex.rda")
load("samples/pitch_train.rda")
load("samples/pitch_test.rda")

set.seed(99)

select_best(bt_tuned_complex, metric = "rmse")

# fit bt model ----
bt_wflow_final <- bt_tuned_complex |> 
  extract_workflow() |> 
  finalize_workflow(select_best(bt_tuned_complex, metric = "rmse"))

bt_fit_final <- fit(bt_wflow_final, pitch_train)

# assess model ----
metrics <- metric_set(rmse, rsq, mae, mape)

pitch_test |> 
  bind_cols(predict(bt_fit_final, pitch_test)) |> 
  metrics(run_exp_above_avg, .pred)

# save results ----
save(bt_fit_final, file = "results/bt_fit_final.rda")

# view predictions ----
example <- pitch_test |> 
  bind_cols(predict(bt_fit_final, pitch_test)) |> 
  arrange(.pred)

example |> 
  ggplot(aes(x = .pred)) +
  geom_histogram()
