# final OLM model

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(knitr)

# load model and data ----
load("results/lm_fit_sink.rda")
load("samples/pitch_train.rda")
load("samples/pitch_test.rda")
load("recipes/main_sink_rec.rda")

# fit olm model ----
lm_wflow_final <- lm_fit_sink |> 
  extract_workflow() |> 
  finalize_workflow(select_best(lm_fit_sink, metric = "rmse"))

lm_fit_final <- fit(lm_wflow_final, pitch_train)

# assess model ----
pitch_test |> 
  bind_cols(predict(lm_fit_final, pitch_test)) |> 
  rmse(run_exp_above_avg, .pred)

# save results ----
save(lm_fit_final, file = "results/lm_fit_final.rda")

# view predictions ----
example <- pitch_test |> 
  bind_cols(predict(lm_fit_final, pitch_test)) |> 
  arrange(.pred)

example |> 
  ggplot(aes(x = .pred)) +
  geom_histogram()
