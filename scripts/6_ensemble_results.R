# ensemble model results

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(DT)
library(stacks)

# load models and testing set ----
load(here("results/ensemble_model_basic.rda"))
load(here("results/ensemble_model_complex.rda"))
load(here("samples/pitch_test.rda"))

metrics <- metric_set(rmse, rsq, mae, mape)

# compile results ----
basic_tbl <- pitch_test |> 
  bind_cols(predict(ensemble_model_basic, pitch_test)) |> 
  metrics(run_exp_above_avg, .pred) |> 
  pivot_wider(values_from = .estimate, names_from = .metric) |> 
  mutate(model = "ensemble",
         recipe = "basic") |> 
  select(model, recipe, rmse, rsq, mae, mape)

complex_tbl <- pitch_test |> 
  bind_cols(predict(ensemble_model_complex, pitch_test)) |> 
  metrics(run_exp_above_avg, .pred) |> 
  pivot_wider(values_from = .estimate, names_from = .metric) |> 
mutate(model = "ensemble",
       recipe = "complex") |> 
  select(model, recipe, rmse, rsq, mae, mape)

bind_rows(basic_tbl, complex_tbl) |> 
  arrange(rmse) |> 
  datatable() |> 
  formatRound(columns = 3:6, digits = 2)

example <- pitch_test |> 
  bind_cols(predict(ensemble_model_complex, pitch_test)) |> 
  filter(.pred <= 0 | .pred >= 100)

ggplot(example, aes(x = .pred)) +
  geom_histogram() +
  facet_wrap(~pitch_type)

example |> 
  arrange(.pred) |> 
  select(player_name, pitch_type, description, .pred)

example |> 
  ggplot(aes(x = description)) +
  geom_bar()
