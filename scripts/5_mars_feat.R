# mars model - complex

# load libraries ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(stacks)

# load data ----
load(here("samples/pitch_folds.rda"))
load(here("recipes/tree_feat_rec.rda"))

# handle common conflicts ----
set.seed(99)
tidymodels_prefer()

# parallel processing ----
registerDoMC(cores = parallel::detectCores())

# model specification ----
mars_spec <- mars(num_terms = tune(), prod_degree = tune()) |> 
  set_engine("earth") |> 
  set_mode("regression")

# define workflow ----
mars_wflow <- workflow() |> 
  add_model(mars_spec) |> 
  add_recipe(tree_feat_rec)

# tuning parameters ----
mars_params <- extract_parameter_set_dials(mars_spec) |> 
  update(num_terms = num_terms(range = c(1, 39)))

mars_grid <- grid_regular(mars_params, levels = c(15, 2))

# fitting workflows ----
mars_tuned_complex <- tune_grid(mars_wflow,
                                pitch_folds,
                                grid = mars_grid,
                                control = control_stack_grid())

# save results ----
save(mars_tuned_complex, file = here("results/mars_tuned_complex.rda"))
