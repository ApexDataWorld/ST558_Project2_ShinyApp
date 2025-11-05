# app.R — ST558 Project2 Shiny App 
options(stringsAsFactors = FALSE)

# ---- Libraries ----
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# read data file
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
cat_vars <- c("Seasons", "Holiday", "Functioning Day")
num_vars <- c("Rented Bike Count", "Temperature(C)", "Humidity(%)",
              "Wind speed (m/s)", "Visibility (10m)", "Dew point temperature(C)",
              "Solar Radiation (MJ/m2)", "Rainfall(mm)", "Snowfall (cm)", "Hour")


# ui part
ui <- fluidPage(
  titlePanel("Seoul Bike App"),
  sidebarLayout(
    sidebarPanel(
      helpText("Pick stuff and click Apply"),
      selectInput("cat1", "First category:", choices = cat_vars, selected = cat_vars[1]),
      uiOutput("cat1_opts"),
      selectInput("cat2", "Second category:", choices = setdiff(cat_vars, cat_vars[1])),
      uiOutput("cat2_opts"),
      selectInput("num1", "First number:", choices = num_vars),
      uiOutput("num1_range"),
      selectInput("num2", "Second number:", choices = setdiff(num_vars, num_vars[1])),
      uiOutput("num2_range"),
      actionButton("goBtn", "Apply Filters")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 h4("About this app"),
                 p("This app lets you look at the Seoul bike data and make plots."),
                 img(src = "seoul-bikes.jpg", width = "50%")),
        tabPanel("Data",
                 downloadButton("downloadBtn", "Download CSV"),
                 br(), tableOutput("dataTable")),
        tabPanel("Explore",
                 radioButtons("showMode", "Choose:", 
                              choices = c("Tables", "Summaries", "Plots")),
                 conditionalPanel(
                   "input.showMode == 'Tables'",
                   selectInput("tab1_var", "One variable:", choices = cat_vars),
                   selectInput("tab2_var1", "Two-way 1:", choices = cat_vars),
                   selectInput("tab2_var2", "Two-way 2:", choices = cat_vars),
                   tableOutput("tab1_out"),
                   tableOutput("tab2_out")
                 ),
                 conditionalPanel(
                   "input.showMode == 'Summaries'",
                   selectInput("sum_num", "Numeric var:", choices = num_vars),
                   selectInput("sum_grp", "Group by:", choices = cat_vars),
                   tableOutput("sum_out")
                 ),
                 conditionalPanel(
                   "input.showMode == 'Plots'",
                   selectInput("pltType", "Plot type:",
                               choices = c("Bar", "Violin", "Scatter", "Heatmap", "Line", "Boxplot")),
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
  
  # pick levels for factors
  output$cat1_opts <- renderUI({
    lv <- levels(bikeData[[input$cat1]])
    selectInput("cat1vals", "Levels:", choices = lv, selected = lv, multiple = TRUE)
  })
  
  output$cat2_opts <- renderUI({
    lv <- levels(bikeData[[input$cat2]])
    selectInput("cat2vals", "Levels:", choices = lv, selected = lv, multiple = TRUE)
  })
  
  # range sliders
  output$num1_range <- renderUI({
    rng <- range(bikeData[[input$num1]], na.rm = TRUE)
    sliderInput("num1vals", "Range:", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
  })
  output$num2_range <- renderUI({
    rng <- range(bikeData[[input$num2]], na.rm = TRUE)
    sliderInput("num2vals", "Range:", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
  })
  
  # filtered data
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
  
  # table and download
  output$dataTable <- renderTable({
    d <- curData()
    validate(need(nrow(d) > 0, "No rows left"))
    d
  })
  
  output$downloadBtn <- downloadHandler(
    filename = function() paste0("bike_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(curData(), file, row.names = FALSE)
  )
  
  # tables
  output$tab1_out <- renderTable({
    d <- curData()
    d %>% count(.data[[input$tab1_var]])
  })
  
  output$tab2_out <- renderTable({
    d <- curData()
    d %>%
      count(.data[[input$tab2_var1]], .data[[input$tab2_var2]]) %>%
      tidyr::pivot_wider(names_from = !! rlang::sym(input$tab2_var2),
                         values_from = n, values_fill = 0)
  })
  
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
  
  # temperature helper
  getTemp <- function(nms) {
    tcols <- c("Temperature(°C)", "Temperature(C)", "Temperature (°C)", "Temperature (C)")
    hit <- intersect(tcols, nms)
    if (length(hit)) hit[1] else "Temperature(°C)"
  }
  
  # plots
  

}

# ---- Run App ----
shinyApp(ui, server)
