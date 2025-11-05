# app.R — ST558 Project2 Shiny App 
options(stringsAsFactors = FALSE)

# Load the required packages
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)  

# read and laod data file
file_path <- "/Users/saurabhgupta/projects/github/ST558_Project2_ShinyApp/SeoulBikeData.csv"

text_lines <- readLines(file_path, encoding = "UTF-8", warn = FALSE)
text_lines <- iconv(text_lines, from = "", to = "UTF-8", sub = "")
bikeData <- read.csv(textConnection(text_lines), check.names = FALSE)
closeAllConnections()

# change data types
bikeData$Date <- as.Date(bikeData$Date, "%d/%m/%Y")
bikeData$Hour <- as.integer(bikeData$Hour)
bikeData$Holiday <- as.factor(bikeData$Holiday)
bikeData$`Functioning Day` <- as.factor(bikeData$`Functioning Day`)
bikeData$Seasons <- factor(bikeData$Seasons, levels = c("Winter","Spring","Summer","Autumn"))
bikeData$Month <- format(bikeData$Date, "%b")

# find columns
cat_vars_all <- c("Seasons", "Holiday", "Functioning Day")
num_vars_all <- c("Rented Bike Count","Temperature(°C)","Temperature(C)","Humidity(%)",
                  "Wind speed (m/s)","Visibility (10m)","Dew point temperature(C)",
                  "Solar Radiation (MJ/m2)","Rainfall(mm)","Snowfall (cm)","Hour")
cat_vars <- intersect(cat_vars_all, names(bikeData))
num_vars <- intersect(num_vars_all, names(bikeData))

# ui part
ui <- fluidPage(
  titlePanel("Seoul Bike App"),
  sidebarLayout(
    sidebarPanel(
      helpText("Use these filters to subset the data, then click Apply Filters."),
      selectInput("cat1", "First category:", choices = cat_vars, selected = cat_vars[1]),
      uiOutput("cat1_opts"),
      selectInput("cat2", "Second category:", choices = setdiff(cat_vars, cat_vars[1])),
      uiOutput("cat2_opts"),
      selectInput("num1", "First numeric variable:", choices = num_vars),
      uiOutput("num1_range"),
      selectInput("num2", "Second numeric variable:", choices = setdiff(num_vars, num_vars[1])),
      uiOutput("num2_range"),
      actionButton("goBtn", "Apply Filters")
    ),
    mainPanel(
      tabsetPanel(
        #  About Tab 
        tabPanel("About",
                 h4("About this App"),
                 p("This app allows users to explore the Seoul Bike Sharing dataset interactively."),
                 p("Data Source:",
                   tags$a(href="https://archive.ics.uci.edu/ml/datasets/Seoul+Bike+Sharing+Demand",
                          "UCI Machine Learning Repository"), "."),
                 p("Use the sidebar to subset the data by categories or numeric ranges. 
             The Data tab displays and allows download of the filtered dataset. 
             The Explore tab provides summary tables and plots."),
                 img(src = "app-cover.jpg", width = "100%"),
                 br(),
                 hr(),
                 h4("Filtered Data Summary"),
                 textOutput("dataSummary")
        ),
        
        #  Data Tab 
        tabPanel("Data",
                 downloadButton("downloadBtn", "Download CSV"),
                 br(), br(),
                 DT::dataTableOutput("dataTable")
        ),
        
        #  Explore Tab 
        tabPanel("Explore",
                 radioButtons("showMode", "Choose display type:",
                              choices = c("Tables", "Summaries", "Plots")),
                 conditionalPanel(
                   "input.showMode == 'Tables'",
                   selectInput("tab1_var", "One way table variable:", choices = cat_vars),
                   selectInput("tab2_var1", "Two way table variable 1:", choices = cat_vars),
                   selectInput("tab2_var2", "Two way table variable 2:", choices = cat_vars),
                   tableOutput("tab1_out"),
                   tableOutput("tab2_out")
                 ),
                 conditionalPanel(
                   "input.showMode == 'Summaries'",
                   selectInput("sum_num", "Numeric variable:", choices = num_vars),
                   selectInput("sum_grp", "Group by (categorical):", choices = cat_vars),
                   tableOutput("sum_out")
                 ),
                 conditionalPanel(
                   "input.showMode == 'Plots'",
                   selectInput("pltType", "Plot type:",
                               choices = c("Bar (Seasons and Holiday)",
                                           "Boxplot (Rentals and Seasons)",
                                           "Scatter (Temp and Rentals)",
                                           "Line (Daily Rentals by Season)")),
                   checkboxInput("facetFree", "Free y-axis", TRUE),
                   plotOutput("plt_out")
                 )
        )
      )
    )
  )
)

# server part
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

# Run the Shiny app
shinyApp(ui = ui, server = server)
