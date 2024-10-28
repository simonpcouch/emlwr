# A script to replicate the timings for the Distributed Computing subsection
# on a cloud service.
# install.packages(c("future", "tidymodels", "xgboost", "bench"))
source("includes/common.R")

time_resample_bt <- function(n_rows, plan) {
  set.seed(1)
  d <- simulate_regression(n_rows)

  suppressWarnings(
    plan(plan, workers = 4) 
  )
  
  bench::mark(
    resample =
      fit_resamples(
        boost_tree("regression"),
        outcome ~ .,

        vfold_cv(d, v = 10)
      )
  )
}

t_cloud <-
  bench::press(
    time_resample_bt(n_rows, plan),
    n_rows = 10^(2:6),
    plan = c("sequential", "multisession")
  )

t_cloud$memory <- NULL
t_cloud$result <- NULL

t_cloud$memory <- NULL
t_cloud$result <- NULL

t_cloud$memory <- NULL
t_cloud$result <- NULL

qsave(t_cloud, file = "t_cloud.rds")
