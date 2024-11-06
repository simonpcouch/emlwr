
library(tidyverse)

library(bench)
library(qs)
library(here)

library(shiny)
library(shinydashboard)

options(
  ggplot2.discrete.colour = c(
    # the usual four used in the book
    "#1a162d", "#42725c", "#cd6f3d",  "#a8ab71",
    # several more for when many models are selected
    "#8b4b65", "#557088", "#d9b594", "#6b705c", "#956b4b", "#2d4041"
  )
)

bm <- qread(here("data/models/app/bm.rds"))
bm_fit <- readRDS(here("data/models/app/bm_fit.rds"))
bm_fit$fit$fit$fit <- lgb.load(here("data/models/app/bm_fit_engine.rds"))

ui <- dashboardPage(
  dashboardHeader(title = "Predict Time To Tune"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction", tabName = "prediction", icon = icon("clock"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "prediction",
              # Top row for model selection
              fluidRow(
                box(
                  title = "Modeling Engine", 
                  status = "primary",
                  width = 12,
                  selectInput("model", "Model:",
                              choices = unique(bm$model),
                              multiple = TRUE,
                              selected = unique(bm$model)[1])
                )
              ),
              fluidRow(
                box(
                  title = "Predicted Time To Tune",
                  width = 7,
                  plotOutput("plot", height = "400px")
                ),
                box(
                  title = "Additional Parameters", width = 5, status = "primary",
                  selectInput("dataset", "Dataset:",
                              choices = unique(bm$dataset)),
                  
                  selectInput("strategy", "Strategy:",
                              choices = unique(bm$strategy)),
                  
                  sliderInput("n_workers", "Number of Workers:",
                              value = 1,
                              min = 1,
                              max = 10,
                              step = 1,
                              animate = TRUE),
                  
                  selectInput("tuning_fn", "Tuning Function:",
                              choices = unique(bm$tuning_fn))
                )
              )
      )
    )
  )
)

n_rows <- round(10^seq(from = 2, to = 6, by = .5))

server <- function(input, output) {
  output$plot <- renderPlot({
    new_data <- data.frame(
      model = input$model,
      dataset = input$dataset,
      n_rows = 0L,
      strategy = "stand-in",
      n_workers = input$n_workers,
      tuning_fn = input$tuning_fn
    )
    
    new_data <- dplyr::mutate(
      new_data,
      strategy = dplyr::case_when(n_workers > 1 ~ "multisession", .default = "sequential")
    )
    new_data <- purrr::map(n_rows, ~mutate(new_data, n_rows = .x)) %>% list_rbind()
    
    predictions <- predict(bm_fit, new_data = new_data)
    
    new_data <- new_data %>%
      mutate(.pred = as_bench_time(predictions$.pred))
    
    ggplot(new_data, aes(x = n_rows, y = .pred, col = model)) +
      geom_line() +
      scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
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
