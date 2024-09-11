# Some common setup code to source at the start of each chapter.
library(tidymodels)
library(tidyverse)
library(future)
library(bench)

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
