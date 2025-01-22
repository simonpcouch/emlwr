# Some common setup code to source at the start of each chapter.
library(tidymodels)
library(tidyverse)
library(future)
library(bench)
library(qs)

library(knitr)

# Overwrite the (wonderful) knitr print method for bench_mark objects
# to include even less data than the original.
data_cols <- c("n_itr", "n_gc", "total_time", "result", "memory", "time", "gc")

knit_print.bench_mark <- function(x, ..., options) {
  x <- x[!colnames(x) %in% c(data_cols, "min", "itr/sec", "gc/sec")]
  
  print(structure(x, class = class(x)[-1]))
}

print.bench_mark <- function(x, ...) {
  knit_print.bench_mark(x, ..., options = NULL)
}

trim_bench_mark <- function(bench_mark) {
  bench_mark$memory <- NULL
  bench_mark$result <- NULL
  
  bench_mark
}

# ggplot styling ---------------------------------------------------------------
options(
  ggplot2.discrete.colour = c("#42725c", "#cd6f3d", "#1a162d", "#a8ab71", "#858585", "#d8b365"),
  ggplot2.discrete.fill = c("#42725c", "#cd6f3d", "#1a162d", "#a8ab71", "#858585", "#d8b365")
)

# functions to simulate data ---------------------------------------------------
# these functions are light wrappers around their modeldata friends that
# introduce factor variables based on cutting some numeric inputs into bins 
# with the goal of surfacing performance implications of factor predictors.
#
# bin numeric variable into groups, with cutpoints 
# randomly selected from the distribution of the input
bin_roughly <- function(x) {
  n_levels <- sample(1:4, 1)
  cutpoints <- sort(sample(x, n_levels))
  
  x <- rowSums(vapply(cutpoints, `>`, logical(length(x)),  x))
  
  factor(x, labels = paste0("level_", 1:(n_levels+1)))
}

simulate_regression <- function(n_rows) {
  modeldata::sim_regression(n_rows) %>%
    select(-c(predictor_16:predictor_20)) %>%
    mutate(across(contains("_1"), bin_roughly))
}

simulate_classification <- function(n_rows, n_levels) {
  modeldata::sim_classification(n_rows, num_linear = 12) %>%
    mutate(across(contains("_1"), bin_roughly))
}
