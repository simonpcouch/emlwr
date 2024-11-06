library(tidyverse)
library(tidymodels)
library(bench)
library(qs)
library(bonsai)
library(finetune)

benchmarks <- qread("data/models/app/benchmarks.rds")
benchmarks <- as_tibble(benchmarks)

# for individual model fits <- 
#   bm$extracts_timings[[1]] %>% unnest(.extracts) %>% filter(stage == "model")

# data preparation -------------------------------------------------------------
bm <- 
  benchmarks %>%
  rowwise() %>%
  mutate(
    time_to_tune = pluck(bench_mark, "median")
  ) %>% 
  select(-metrics, -extracts_timings, -bench_mark, -preprocessor, -os, -r_version) %>%
  mutate(time_to_tune_float = as.numeric(time_to_tune)) %>%
  mutate(n_rows = as.numeric(n_rows), n_workers = as.numeric(n_workers)) %>%
  separate(model, into = c("model_type", "engine"), sep = "_classification_|_regression_") %>%
  mutate(model = paste0(model_type, " (", engine, ")")) %>%
  mutate(across(where(is.character), as.factor)) %>%
  # TODO: let folks choose one or both of these and discard mismatching
  # engines (possible "greying them out" in the dropdown)
  select(-model_type, -engine)

bm

# fit a model to generate the predictions --------------------------------------
bm_fit <-
  tune_race_anova(
    recipe(time_to_tune_float ~ ., bm %>% select(-time_to_tune)) %>% 
      step_zv(all_predictors()),
    boost_tree(learn_rate = tune(), trees = tune()) %>%
      set_engine(engine = "lightgbm") %>%
      set_mode(mode = "regression"),
    resamples = bootstraps(bm %>% select(-time_to_tune)),
    control = control_race(save_workflow = TRUE)
  ) %>%
  fit_best(metric = "rmse")

qsave(bm, file = "data/models/app/bm.rds")

saveRDS(bm_fit, "data/models/app/bm_fit.rds")
lgb.save(extract_fit_engine(bm_fit), "data/models/app/bm_fit_engine.rds")
