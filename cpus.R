# scrapes the table at https://www.cpubenchmark.net/cpu_list.php
library(tidyverse)

cpus_raw <- read_html("https://www.cpubenchmark.net/cpu_list.php")

cpus <- cpus_raw %>%
  html_element(xpath = '//*[(@id = "cputable")]') %>%
  html_table() %>%
  select(1:2) %>%
  set_names(c("name", "mark")) %>%
  mutate(
    mark = gsub(",", "", mark),
    mark = as.numeric(mark)
  )

cpus

qs::qsave(cpus, "data/models/app/cpus.rds")
