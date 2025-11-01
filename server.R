

library(shiny)
library(tidyverse)

server <- function(input, output, session) {
  
  # ---- Load Data Here ----
  train <- read.csv("train.csv")
  test  <- read.csv("test.csv")
  
  num_vars <- names(train)[sapply(train, is.numeric)]
  cat_vars <- names(train)[sapply(train, is.factor) | sapply(train, is.character)]
  
  # ---- Dynamic categorical selectors ----
  output$cat1_values <- renderUI({
    req(input$cat1)
    selectInput("cat1_val", paste("Select", input$cat1, "value:"),
                choices = c("All", sort(unique(train[[input$cat1]]))))
  })
  
  output$cat2_values <- renderUI({
    req(input$cat2)
    selectInput("cat2_val", paste("Select", input$cat2, "value:"),
                choices = c("All", sort(unique(train[[input$cat2]]))))
  })
  
  # ---- Reactive filtered data ----
  filtered_data <- reactiveVal(train)
  
  observeEvent(input$apply_filters, {
    data <- train
    
    if (!is.null(input$cat1_val) && input$cat1_val != "All") {
      data <- data %>% filter(.data[[input$cat1]] == input$cat1_val)
    }
    if (!is.null(input$cat2_val) && input$cat2_val != "All") {
      data <- data %>% filter(.data[[input$cat2]] == input$cat2_val)
    }
    
    if (!is.null(input$num1_range)) {
      data <- data %>% filter(between(.data[[input$num1]], input$num1_range[1], input$num1_range[2]))
    }
    if (!is.null(input$num2_range)) {
      data <- data %>% filter(between(.data[[input$num2]], input$num2_range[1], input$num2_range[2]))
    }
    
    filtered_data(data)
  })
  
  # ---- Numeric summaries ----
  summary_df 
  
  observeEvent(input$summary_btn, {
    df <- filtered_data()
    req(input$summary_var)
    res <- df %>%
      summarise(
        Mean = mean(.data[[input$summary_var]], na.rm = TRUE),
        Median = median(.data[[input$summary_var]], na.rm = TRUE),
        SD = sd(.data[[input$summary_var]], na.rm = TRUE),
        IQR = IQR(.data[[input$summary_var]], na.rm = TRUE)
      )
    summary_df(res)
  })
  
  output$summary_table <- renderTable({
    req(summary_df())
    summary_df()
  })
  
}
