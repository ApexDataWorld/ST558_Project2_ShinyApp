# ui.R — User Interface for Seoul Bike App
library(shiny)
library(DT)

# ---- UI Definition ----
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
