# app.R — ST558 Project2 Shiny App 
options(stringsAsFactors = FALSE)

# Load the required packages
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)  

# read and laod data file
path <- "/Users/saurabhgupta/projects/github/ST558_Project2_ShinyApp/SeoulBikeData.csv"

txt <- readLines(path, encoding = "UTF-8", warn = FALSE)
txt <- iconv(txt, from = "", to = "UTF-8", sub = "")
bikeData <- read.csv(textConnection(txt), check.names = FALSE)
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
                 img(src = "app-cover.jpg", width = "100%")
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
    lv <- levels(bikeData[[input$cat1]])
    selectInput("cat1vals", "Levels:", choices = lv, selected = lv, multiple = TRUE)
  })
  output$cat2_opts <- renderUI({
    lv <- levels(bikeData[[input$cat2]])
    selectInput("cat2vals", "Levels:", choices = lv, selected = lv, multiple = TRUE)
  })
  
  # UI for numeric ranges 
  output$num1_range <- renderUI({
    rng <- range(as.numeric(bikeData[[input$num1]]), na.rm = TRUE)
    sliderInput("num1vals", "Range:", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
  })
  output$num2_range <- renderUI({
    rng <- range(as.numeric(bikeData[[input$num2]]), na.rm = TRUE)
    sliderInput("num2vals", "Range:", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
  })
  
  #  Reactive data 
  rv <- reactiveValues(data = bikeData)
  observeEvent(input$goBtn, {
    d <- bikeData
    if (!is.null(input$cat1vals)) d <- d %>% filter(.data[[input$cat1]] %in% input$cat1vals)
    if (!is.null(input$cat2vals)) d <- d %>% filter(.data[[input$cat2]] %in% input$cat2vals)
    if (!is.null(input$num1vals)) d <- d %>% filter(between(.data[[input$num1]], input$num1vals[1], input$num1vals[2]))
    if (!is.null(input$num2vals)) d <- d %>% filter(between(.data[[input$num2]], input$num2vals[1], input$num2vals[2]))
    rv$data <- d
  })
  curData <- reactive(rv$data)
  
  #  Data Tab 
  output$dataTable <- DT::renderDataTable({
    d <- curData()
    validate(need(nrow(d) > 0, "No rows left"))
    d
  })
  output$downloadBtn <- downloadHandler(
    filename = function() paste0("bike_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(curData(), file, row.names = FALSE)
  )
  
# Tables
  output$tab1_out <- renderTable({
    d <- curData()
    d %>% count(.data[[input$tab1_var]])
  })
  output$tab2_out <- renderTable({
    d <- curData()
    d %>%
      count(.data[[input$tab2_var1]], .data[[input$tab2_var2]]) %>%
      tidyr::pivot_wider(
        names_from = !! rlang::sym(input$tab2_var2),
        values_from = n,
        values_fill = 0
      )
  })
  
  #  Summaries 
  output$sum_out <- renderTable({
    d <- curData()
    d %>%
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
    d <- curData()
    validate(need(nrow(d) > 0, "No rows to plot."))
    
    if (input$pltType == "Bar (Seasons and Holiday)") {
      ggplot(d, aes(x = Seasons, fill = Holiday)) +
        geom_bar() +
        labs(x = "Season", y = "Count", title = "Counts by Season and Holiday") +
        theme_minimal()
      
    } else if (input$pltType == "Boxplot (Rentals and Seasons)") {
      ggplot(d, aes(x = Seasons, y = `Rented Bike Count`, fill = Seasons)) +
        geom_boxplot() +
        labs(x = "Season", y = "Rented Bike Count", title = "Rentals by Season") +
        theme_minimal()
      
    } else if (input$pltType == "Scatter (Temp and Rentals)") {
      temp_col <- if ("Temperature(C)" %in% names(d)) "Temperature(C)" else "Temperature(°C)"
      ggplot(d, aes(x = .data[[temp_col]], y = `Rented Bike Count`, color = `Functioning Day`)) +
        geom_point(alpha = 0.5) +
        facet_wrap(~Seasons) +
        labs(x = temp_col, y = "Rented Bike Count", title = "Temperature vs Rentals") +
        theme_minimal()
      
    } else if (input$pltType == "Line (Daily Rentals by Season)") {
      daily <- d %>%
        filter(!is.na(Date)) %>%
        group_by(Date, Seasons) %>%
        summarise(rentals = sum(`Rented Bike Count`, na.rm = TRUE), .groups = "drop")
      ggplot(daily, aes(x = Date, y = rentals, color = Seasons)) +
        geom_line() +
        facet_wrap(~Seasons, scales = if (input$facetFree) "free_y" else "fixed") +
        labs(x = "Date", y = "Total Rentals", title = "Daily Rentals by Season") +
        theme_minimal()
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
