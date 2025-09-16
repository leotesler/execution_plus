# Null model

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

# load data ----
load(here("samples/pitch_folds.rda"))
load(here("recipes/main_sink_rec.rda"))

# handle common conflicts ----
tidymodels_prefer()

# parallel processing ----
num_cores <- detectCores()
registerDoMC(cores = num_cores)

# model specification ----
null_spec <- null_model() |> 
  set_engine("parsnip") |> 
  set_mode("regression")

# define workflow ----
null_wflow <- workflow() |> 
  add_model(null_spec) |> 
  add_recipe(main_sink_rec)

# fit to resamples ----
null_fit <- fit_resamples(null_wflow,
                          pitch_folds,
                          control = control_resamples(save_workflow = TRUE))

# save results ----
save(null_fit, file = here("results/null_fit.rda"))
