# server.R — Server logic for Seoul Bike App
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  
  #  UI for category levels
  output$cat1_opts <- renderUI({
    category_levels <- levels(bikeData[[input$cat1]])
    selectInput("cat1vals", "Levels:", choices = category_levels, selected = category_levels, multiple = TRUE)
  })
  output$cat2_opts <- renderUI({
    category_levels <- levels(bikeData[[input$cat2]])
    selectInput("cat2vals", "Levels:", choices = category_levels, selected = category_levels, multiple = TRUE)
  })
  
  # UI for numeric ranges 
  output$num1_range <- renderUI({
    numeric_range <- range(as.numeric(bikeData[[input$num1]]), na.rm = TRUE)
    sliderInput("num1vals", "Range:", min = floor(numeric_range[1]), max = ceiling(numeric_range[2]), value = numeric_range)
  })
  output$num2_range <- renderUI({
    numeric_range <- range(as.numeric(bikeData[[input$num2]]), na.rm = TRUE)
    sliderInput("num2vals", "Range:", min = floor(numeric_range[1]), max = ceiling(numeric_range[2]), value = numeric_range)
  })
  
  
  #  Reactive data 
  filtered_data_values <- reactiveValues(data = bikeData)
  observeEvent(input$goBtn, {
    filtered_dataset <- bikeData
    if (!is.null(input$cat1vals)) filtered_dataset <- filtered_dataset %>% filter(.data[[input$cat1]] %in% input$cat1vals)
    if (!is.null(input$cat2vals)) filtered_dataset <- filtered_dataset %>% filter(.data[[input$cat2]] %in% input$cat2vals)
    if (!is.null(input$num1vals)) filtered_dataset <- filtered_dataset %>% filter(between(.data[[input$num1]], input$num1vals[1], input$num1vals[2]))
    if (!is.null(input$num2vals)) filtered_dataset <- filtered_dataset %>% filter(between(.data[[input$num2]], input$num2vals[1], input$num2vals[2]))
    filtered_data_values$data <- filtered_dataset
  })
  current_filtered_data <- reactive(filtered_data_values$data)
  
  # Filtered Data Summary
  output$dataSummary <- renderText({
    filtered_data <- current_filtered_data()
    paste0(
      "Filtered ", nrow(filtered_data), " rows and ", ncol(filtered_data), " columns after applying filters.\n",
      "Filters used → ",
      input$cat1, ": ", paste(input$cat1vals, collapse = ", "), "; ",
      input$cat2, ": ", paste(input$cat2vals, collapse = ", "), "; ",
      input$num1, ": ", paste(input$num1vals, collapse = "–"), "; ",
      input$num2, ": ", paste(input$num2vals, collapse = "–")
    )
  })
  
  #  Data Tab 
  output$dataTable <- DT::renderDataTable({
    filtered_data <- current_filtered_data()
    validate(need(nrow(filtered_data) > 0, "No rows left"))
    filtered_data
  })
  output$downloadBtn <- downloadHandler(
    filename = function() paste0("bike_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(current_filtered_data(), file, row.names = FALSE)
  )
  
  # Tables
  output$tab1_out <- renderTable({
    filtered_data <- current_filtered_data()
    filtered_data %>% count(.data[[input$tab1_var]])
  })
  output$tab2_out <- renderTable({
    filtered_data <- current_filtered_data()
    filtered_data %>%
      count(.data[[input$tab2_var1]], .data[[input$tab2_var2]]) %>%
      tidyr::pivot_wider(
        names_from = !! rlang::sym(input$tab2_var2),
        values_from = n,
        values_fill = 0
      )
  })
  
  #  Summaries 
  output$sum_out <- renderTable({
    filtered_data <- current_filtered_data()
    filtered_data %>%
      group_by(.data[[input$sum_grp]]) %>%
      summarise(
        n = n(),
        avg = mean(.data[[input$sum_num]], na.rm = TRUE),
        med = median(.data[[input$sum_num]], na.rm = TRUE),
        sdv = sd(.data[[input$sum_num]], na.rm = TRUE),
        mn = min(.data[[input$sum_num]], na.rm = TRUE),
        mx = max(.data[[input$sum_num]], na.rm = TRUE)
      )
  })
  
  # Plots 
  output$plt_out <- renderPlot({
    filtered_data <- current_filtered_data()
    validate(need(nrow(filtered_data) > 0, "No rows to plot."))
    
    if (input$pltType == "Bar (Seasons and Holiday)") {
      ggplot(filtered_data, aes(x = Seasons, fill = Holiday)) +
        geom_bar() +
        labs(x = "Season", y = "Count", title = "Counts by Season and Holiday") +
        theme_minimal()
      
    } else if (input$pltType == "Boxplot (Rentals and Seasons)") {
      ggplot(filtered_data, aes(x = Seasons, y = `Rented Bike Count`, fill = Seasons)) +
        geom_boxplot() +
        labs(x = "Season", y = "Rented Bike Count", title = "Rentals by Season") +
        theme_minimal()
      
    } else if (input$pltType == "Scatter (Temp and Rentals)") {
      temperature_column <- if ("Temperature(C)" %in% names(filtered_data)) "Temperature(C)" else "Temperature(°C)"
      ggplot(filtered_data, aes(x = .data[[temperature_column]], y = `Rented Bike Count`, color = `Functioning Day`)) +
        geom_point(alpha = 0.5) +
        facet_wrap(~Seasons) +
        labs(x = temperature_column, y = "Rented Bike Count", title = "Temperature vs Rentals") +
        theme_minimal()
      
    } else if (input$pltType == "Line (Daily Rentals by Season)") {
      daily_summary <- filtered_data %>%
        filter(!is.na(Date)) %>%
        group_by(Date, Seasons) %>%
        summarise(rentals = sum(`Rented Bike Count`, na.rm = TRUE), .groups = "drop")
      ggplot(daily_summary, aes(x = Date, y = rentals, color = Seasons)) +
        geom_line() +
        facet_wrap(~Seasons, scales = if (input$facetFree) "free_y" else "fixed") +
        labs(x = "Date", y = "Total Rentals", title = "Daily Rentals by Season") +
        theme_minimal()
    }
  })
}
