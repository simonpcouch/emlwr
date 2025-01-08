library(ggplot2)
library(bench)
library(qs)
library(shiny)
library(bslib)
library(shinylive)

options(
  ggplot2.discrete.colour = c(
    "#1a162d", "#42725c", "#cd6f3d", "#a8ab71",
    "#8b4b65", "#557088", "#d9b594", "#6b705c", "#956b4b", "#2d4041"
  )
)

n_rows <- round(10^seq(from = 2, to = 6, by = .5))
reference_mark <- 37002
footer_context <- paste0(collapse = "", c(
  "Timings estimate the time to evaluate an initial set of 10 models across 10 ",
  "resamples, resulting in 100 model fits on 9/10th of rows, 100 sets of ",
  "predictions on 1/10th of rows, and metric calculations on each set of predictions."
))

load(url("https://raw.githubusercontent.com/simonpcouch/emlwr/main/data/models/app/bm.rda"))
load(url("https://raw.githubusercontent.com/simonpcouch/emlwr/main/data/models/app/cpus.rda"))

ui <- page_fillable(
  theme = bs_theme(
    bg = "#ffffff",
    fg = "#333333",
    primary = "#42725c"
  ),
  title = "Time To Tune",
  layout_column_wrap(
    width = 1/1,
    heights_equal = "row",
    selectInput(
      "model", "Model:",
      choices = unique(bm$model),
      multiple = TRUE,
      selected = c(
        "linear_reg (glmnet)", 
        "boost_tree (xgboost)",
        "boost_tree (lightgbm)"
      )
    )
  ),
  layout_sidebar(
    sidebar = sidebar(
      selectInput(
        "task", "Task:",
        choices = unique(bm$task),
        selected = "regression"
      ),
      sliderInput(
        "n_workers", "Number of Workers:",
        value = 1,
        min = 1,
        max = 10,
        step = 1,
        animate = TRUE
      ),
      selectInput(
        "tuning_fn", "Tuning Function:",
        choices = unique(bm$tuning_fn)
      ),
      selectInput(
        "cpu", "CPU:", 
        choices = NULL
      ),
      markdown("Timings scaled according to [CPU benchmarks](https://www.cpubenchmark.net/cpu_list.php).")
    ),
    card(
      full_screen = TRUE,
      card_header("Time To Tune"),
      plotOutput("plot", height = "400px"),
      footer = footer_context
    )
  )
)

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
        (reference_mark / cpus$mark[cpus$name == input$cpu])
      new_data$time_to_tune <- as_bench_time(new_data$time_to_tune_float)
    }
    
    ggplot(new_data, aes(x = n_rows, y = time_to_tune, col = model, group = model)) +
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

app <- shinyApp(ui, server)
