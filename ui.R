library(shiny)

ui <- fluidPage(

  titlePanel("Data Exploration Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Subset the Data"),
      
      selectInput("cat1", "Choose Categorical Variable 1:", choices = NULL),
      uiOutput("cat1_values"),
      
      selectInput("cat2", "Choose Categorical Variable 2:", choices = NULL),
      uiOutput("cat2_values"),
      
      selectInput("num1", "Choose Numeric Variable 1:", choices = NULL),
      uiOutput("num1_slider"),
      
      selectInput("num2", "Choose Numeric Variable 2:", choices = NULL),
      uiOutput("num2_slider"),
      
      actionButton("apply_filters", "Apply Filters", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel("About",
                 h3("About This App"),
        ),
        
        tabPanel("Data Download",
                 h4("Filtered Data"),
        ),
        
        tabPanel("Data Exploration",
                 h4("Explore Numeric and Categorical Summaries"),
        )
      )
    )
  )
)
