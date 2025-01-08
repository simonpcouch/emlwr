# setup ------------------------------------------------------------------------
library(tidyverse)
library(workflows)
library(lightgbm)

library(bench)
library(qs)
library(here)

library(shiny)
library(shinydashboard)
library(fresh)

options(
  ggplot2.discrete.colour = c(
    # the usual four used in the book
    "#1a162d", "#42725c", "#cd6f3d",  "#a8ab71",
    # several more for when many models are selected
    "#8b4b65", "#557088", "#d9b594", "#6b705c", "#956b4b", "#2d4041"
  )
)

bm <- qread(here("data/models/app/bm.rds"))

# from https://www.cpubenchmark.net/cpu_list.php
cpus <- qread(here("data/models/app/cpus.rds"))

# constants  -------------------------------------------------------------------
n_rows <- round(10^seq(from = 2, to = 6, by = .5))

# intel i7-13700
reference_mark <- 37002

# TODO: make this estimate adjust for `tune_race_anova`
footer_context <- paste0(collapse = "", c(
  "Timings estimate the time to evaluate an initial set of 10 models across 10 ",
  "resamples, resulting in 100 model fits on 9/10th of rows, 100 sets of ",
  "predictions on 1/10th of rows, and metric calculations on each set of predictions."
))

# ui ---------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Time To Tune"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Timings", tabName = "timings", icon = icon("clock"))
    ),
    collapsed = TRUE
  ),
  
  dashboardBody(
    includeCSS(here("data/models/app/emlwr.css")),
    use_theme(
      create_theme(
        adminlte_color(light_blue = "#42725c")
      )
    ),
    tabItems(
      tabItem(tabName = "timings",
              fluidRow(
                box(
                  title = "Modeling Engine", 
                  status = "primary",
                  width = 12,
                  selectInput("model", "Model:",
                              choices = unique(bm$model),
                              multiple = TRUE,
                              selected = c(
                                "linear_reg (glmnet)", 
                                "boost_tree (xgboost)",
                                "boost_tree (lightgbm)"
                              ))
                )
              ),
              fluidRow(
                box(
                  title = "Time To Tune",
                  width = 7,
                  footer = footer_context,
                  plotOutput("plot", height = "400px")
                ),
                box(
                  title = "Additional Parameters", width = 5, status = "primary",
                  selectInput("task", "Task:",
                              choices = unique(bm$task),
                              selected = "regression"),

                  # TODO: somehow need to only enable selection numbers of 
                  # workers that we've actually run (or run all in 1:10 eep)
                  sliderInput("n_workers", "Number of Workers:",
                              value = 1,
                              min = 1,
                              max = 10,
                              step = 1,
                              animate = TRUE),
                  
                  selectInput("tuning_fn", "Tuning Function:",
                              choices = unique(bm$tuning_fn)),
                  selectInput("cpu", "CPU:", choices = NULL),
                  markdown("Timings scaled according to [CPU benchmarks](https://www.cpubenchmark.net/cpu_list.php).")
                )
              )
      )
    )
  )
)

# server -----------------------------------------------------------------------
server <- function(input, output, session) {
  updateSelectizeInput(
    session,
    'cpu',
    choices = cpus$name,
    server = TRUE,
    selected = "Intel Core i7-13700"
  )
  
  output$plot <- renderPlot({
    new_data <- bm[
      bm$model %in% input$model &
      bm$task == input$task &
      bm$n_workers == input$n_workers &
      bm$tuning_fn == input$tuning_fn,
    ]
    
    if (!identical(input$cpu, "")) {
      new_data$time_to_tune_float <-
        new_data$time_to_tune_float *
        (reference_mark/ cpus$mark[cpus$name == input$cpu])
      new_data$time_to_tune <- as_bench_time(new_data$time_to_tune_float)
    }
    
    ggplot(new_data, aes(x = n_rows, y = time_to_tune, col = model, group = model)) +
      # TODO: change these to lines once we have data
      geom_point() +
      geom_line() +
      scale_x_log10() +
      labs(x = "Number of Rows", y = "Time to Tune (seconds)") +
      theme(
        text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)
      )
  })
}

shinyApp(ui, server)
