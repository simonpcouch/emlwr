library(tidyverse)
library(bench)
library(qs)

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
  select(-model_type, -engine) %>%
  relocate(model, .before = everything()) %>%
  relocate(strategy, .after = everything()) %>%
  rename(task = dataset)

bm

qsave(bm, file = "data/models/app/bm.rds")
