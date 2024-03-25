library(tidymodels)
library(future)
library(readmission)
library(finetune)
library(bonsai)

basic_fit <- 
  select_best(bm_basic$result[[1]], metric = "roc_auc") %>%
  finalize_workflow(workflow(readmitted ~ ., bt), parameters = .) %>%
  last_fit(split = readmission_split)

save(basic_fit, file = "data/01-intro/basic_fit.Rda")