## linear model - complex

# load libraries ----
library(tidyverse)
library(tidymodels)
library(doMC)
library(here)
library(stacks)

# load data ----
load(here("samples/pitch_folds.rda"))
load(here("recipes/main_feat_rec.rda"))

# prefer tidymodels ----
tidymodels_prefer()

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# model specification ----
lm_spec <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

# building workflows ----
lm_wflow <- workflow() |> 
  add_model(lm_spec) |> 
  add_recipe(main_feat_rec)

# feat fit ----
lm_fit_feat <- fit_resamples(lm_wflow,
                             pitch_folds,
                             control = control_stack_resamples())

# saving results ----
save(lm_fit_feat, file = here("results/lm_fit_feat.rda"))
